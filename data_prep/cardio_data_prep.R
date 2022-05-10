# Data preparation for cardiovascular tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## Lookups ---- from deaths_data_preparation.R 
## for Hospital Discharges / deaths Cardiac
###############################################.
dep_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/deprivation_geography.rds") %>%
  rename(datazone_2011 = datazone) %>%
  select(datazone_2011, year, sc_quin) %>%
  filter(year>2014)

dep_lookup21 <- dep_lookup %>%  filter(year == 2019) %>% mutate(year = 2021)

dep_lookup <- rbind(dep_lookup, dep_lookup21)

###############################################.
## Cath labs - cardiac procedures ----
###############################################.
create_cathlab <- function() {
  
gj_cath_age <- read_excel(paste0(data_folder, "cath_labs/GJNH_CathLabData_ForPHS_III.xlsx"),
                          sheet = "Age") %>% clean_names() %>% 
  mutate(type = "age",
         age_band = recode(age_band, "Gt60" = "60 and over",
                           "Lt60" = "Under 60")) %>% 
  rename(category = age_band)

gj_cath_sex <- read_excel(paste0(data_folder, "cath_labs/GJNH_CathLabData_ForPHS_III.xlsx"),
                          sheet = "Sex") %>% clean_names() %>% 
  mutate(type = "sex") %>%  rename(category = gender)

gj_cath_all <- read_excel(paste0(data_folder, "cath_labs/GJNH_CathLabData_ForPHS_III.xlsx"),
                          sheet = "No strata") %>% clean_names() %>% 
  mutate(type = "sex", category = "All")

# Merging together and formating
gj_cath <- rbind(gj_cath_age, gj_cath_sex, gj_cath_all) %>% 
  mutate(week_ending = as.Date(date_to),
         lab = "Golden Jubilee National Hospital") %>% 
  select(-proc_year, -wk, -date_from, -date_to) %>% 
  rename(count = num_procs, groups = group)

###############################################.
# Lothian/RIE cath labs 
loth_age <- read_csv(paste0(data_folder, "cath_labs/Lothian_age.csv")) %>% 
  clean_names() %>% 
  mutate(type = "age",
         age_band = recode(age_band, "gt60" = "60 and over",
                           "lt60" = "Under 60")) %>% 
  rename(category = age_band)

loth_all <- read_csv(paste0(data_folder, "cath_labs/Lothian_no_strata.csv")) %>% 
  clean_names() %>% mutate(type = "sex", category = "All")

loth_sex <- read_csv(paste0(data_folder, "cath_labs/Lothian_sex.csv")) %>% 
  clean_names() %>% 
  mutate(type = "sex",
         gender = recode(gender, "M" = "Male",
                         "F" = "Female")) %>% 
  rename(category = gender)

# Merging together and formating
loth_cath <- rbind(loth_age, loth_all, loth_sex) %>% 
  mutate(week_ending = as.Date(paste(proc_year, proc_week, 7, sep="-"), "%Y-%W-%u"),
         lab = "Royal Infirmary of Edinburgh") %>% 
  select(-proc_year) %>% rename(count = num)

###############################################.
# All labs 
cath_labs <- rbind(loth_cath, gj_cath) %>% 
  mutate(groups = recode(groups, "angio" = "Angiography", "Angio" = "Angiography", 
                         "devices" = "Devices", "Device" = "Devices", 
                         "pci" = "Percutaneous coronary intervention",
                         "PCI" = "Percutaneous coronary intervention"))

# Creating value for total
all_cath <- cath_labs %>% 
  group_by(category, type, week_ending, proc_week, groups) %>%
  # Not using mean to avoid issues with missing data for some weeks
  summarise(count = sum(count, na.rm = T)) %>% 
  mutate(lab = "All") %>% ungroup %>% 
  # TEMPORARY - RIE NOT COMPLETE AFTER first week of May
  filter(week_ending<as.Date("2020-05-10"))

cath_labs <- rbind(cath_labs, all_cath)

# Creating average admissions of pre-covid data (2018-2019) by day of the year
cath_labs_hist <- cath_labs %>% filter(year(week_ending) %in% c("2018", "2019")) %>%
  group_by(category, type, proc_week, groups, lab) %>%
  # Not using mean to avoid issues with missing data for some weeks
  summarise(count_average = round((sum(count, na.rm = T))/2, 1)) %>% ungroup()

cath_labs_2020 <- left_join(cath_labs %>% filter(year(week_ending) %in% c("2020")),
                            cath_labs_hist,
                            by = c("category", "type", "groups", "proc_week", "lab")) %>%
  # Creating %variation from precovid to covid period
  mutate(count_average = ifelse(is.na(count_average), 0, count_average),
         variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  ifelse(is.infinite(variation), 8000, variation)) %>% 
  select(-proc_week) %>% 
  # Supressing small numbers
  mutate(count = case_when(count<5 ~ NA_real_, TRUE ~ count),
         variation = case_when((count<5 | is.na(count)) ~ NA_real_, TRUE ~ variation))

saveRDS(cath_labs_2020, "shiny_app/data/cath_lab_data.rds")
saveRDS(cath_labs_2020, paste0(data_folder,"final_app_files/cath_lab_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

}

###############################################.
## A&E Cardio ----
###############################################.
create_aecardio <- function(filedate, last_week) {
  # Reads in A&E cardio ICD 10 codes for modal, only required to run in case code list changes
  ae_cardio_codes <- read_xlsx(paste0(data_folder, "A&E_Cardio/A&E-CardioConditionCodes.xlsx"))
  saveRDS(ae_cardio_codes, "shiny_app/data/ae_cardio_codes.rds")
  saveRDS(ae_cardio_codes, paste0(data_folder,"final_app_files/ae_cardio_codes_", 
                                  format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
  print("File ae_cardio_codes.rds produced and saved")
  
  # Read in data, clean names + some simple mutations
  ae_cardio <- read_xlsx(paste0(ae_folder, filedate, "-CardioVascular-AttendancesDuringCovid-19.xlsx")) %>% 
    clean_names() %>% 
    rename(diag_cat = diagnosis_catagory,
           dep = prompt_dataset_deprivation_scot_quintile) %>% 
    mutate(week_ending = as.Date(week_ending),
           age_band = ifelse(is.na(age_band), "Missing", age_band)) %>%
    create_depgroups()
  
  # Reshaping of A&E cardio data to make it compliant with Shiny app format
  ae_cardio_all <- ae_cardio %>% 
    group_by(diag_cat, week_ending) %>% 
    summarise(count = sum(number_of_attendances),
              type = "all",
              category = "All",
              area_name = "Scotland",
              area_type = "Scotland") %>% 
    ungroup() %>% 
    select(week_ending, area_name, area_type, type, category, count)
  
  ae_cardio_dep <- ae_cardio %>% 
    group_by(diag_cat, week_ending, dep) %>% 
    summarise(count = sum(number_of_attendances),
              type = "dep",
              area_name = "Scotland",
              area_type = "Scotland") %>% 
    ungroup() %>% 
    rename(category = dep) %>% 
    select(week_ending, area_name, area_type, type, category, count)
  
  ae_cardio_age <- ae_cardio %>% 
    group_by(diag_cat, week_ending, age_band) %>% 
    summarise(count = sum(number_of_attendances),
              type = "age",
              area_name = "Scotland",
              area_type = "Scotland") %>% 
    ungroup() %>% 
    rename(category = age_band) %>% 
    select(week_ending, area_name, area_type, type, category, count)
  
  ae_cardio <- rbind(ae_cardio_all, ae_cardio_dep, ae_cardio_age)
  
  # Remove temporary object from environment to reduce session size
  rm(ae_cardio_all, ae_cardio_age, ae_cardio_dep)
  
  prepare_final_data(ae_cardio, "ae_cardio", last_week = last_week)
  
  print("File ae_cardio.rds produced and saved")
  
}

###############################################.
## OOH Cardiac  ----
###############################################.
create_oohcardio <- function(filedate, last_week) {
  ooh_data_cardiac <- read_csv(paste0(data_folder, "GP_OOH_Cardio/", filedate, "-Weekly Cardio Diagnosis OOH extract.csv")) %>% 
    janitor::clean_names() %>% 
    filter(age > 14) %>%  # Filter age > 14
    mutate(week_ending = as.Date(gp_ooh_sc_end_date), # Formatting dates
           week_ending = ceiling_date(week_ending, "week", change_on_boundary = F)) %>% 
    rename(hb = reporting_health_board_name_as_at_date_of_episode,
           dep = patient_prompt_dataset_deprivation_scot_quintile, 
           sex = gender_description,
           number_of_cases = gp_ooh_number_of_cases) %>% 
    create_agegroups() %>% #age bands
    create_depgroups() # deprivation groups format
  
  # remove diagnosis field as just showing total cardiac
  ooh_data_cardiac %<>%
    group_by(week_ending, hb, age_grp, sex, dep) %>%
    summarise(count = sum(number_of_cases, na.rm = T)) %>% ungroup
  
  ooh_data_cardiac %<>% 
    mutate( sex = recode(sex, "MALE" = "Male", "FEMALE" = "Female", "NOT SPEC" = NA_character_, "NOT KNOWN" = NA_character_),
            scot = "Scotland") %>% 
    proper() # convert HB names to correct format
  
  ooh_data_cardiac %<>% 
    gather(area_type, area_name, c(area_name, scot)) %>% ungroup() %>% 
    mutate(area_type = recode(area_type, "area_name" = "Health board", 
                              "scot" = "Scotland")) %>%
    rename(age = age_grp) %>% 
    # Aggregating to make it faster to work with
    group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
    summarise(count = sum(count, na.rm = T))  %>% ungroup() 
  
  # Creating totals for groups
  ooh_cd_all <- ooh_data_cardiac %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
  ooh_cd_sex <- ooh_data_cardiac %>% agg_cut(grouper="sex") %>% rename(category = sex)
  ooh_cd_dep <- ooh_data_cardiac %>% agg_cut(grouper="dep") %>% rename(category = dep)
  ooh_cd_age <- ooh_data_cardiac %>% agg_cut(grouper="age") %>% rename(category = age)
  
  ooh_cardiac <- rbind(ooh_cd_all, ooh_cd_sex, ooh_cd_dep, ooh_cd_age)
  
  ooh_cardiac %<>% # Filter graphs that look odd due to small numbers
    filter(!area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles")) %>% 
    filter(!area_name %in% c("NHS Borders", "NHS Dumfries & Galloway", 
                             "NHS Lanarkshire") | type !="age") %>% 
    filter(!area_name %in% c("NHS Borders", "NHS Dumfries & Galloway", 
                             "NHS Fife", "NHS Highland") | type !="dep")
  
  # Formatting file for shiny app
  prepare_final_data_cardiac(dataset = ooh_cardiac, filename = "ooh_cardiac", last_week = last_week)

  print("File ooh_cardiac.rds produced and saved")
  print("#############################################")
  print("Remember to change final_app_files script")
  file.edit("data_prep/final_app_files.R")
}

###############################################.
## SAS Cardiac ----
###############################################.
create_sascardio <- function(filedate, last_week) {
  sas_data_cardiac <- read_csv(paste0(data_folder,"SAS_Cardio/", filedate, "-Weekly Cardio Diagnosis SAS extract.csv")) %>%
    janitor::clean_names() %>% 
    filter(age > 14) %>% # Filter age > 14
    mutate(week_ending = as.Date(sas_call_start_date), # Formatting dates
           week_ending = ceiling_date(week_ending, "week", change_on_boundary = F)) %>% 
    rename(hb = reporting_health_board_name_current,
           sex = gender_description,
           dep = patient_prompt_dataset_deprivation_scot_quintile,
           number_of_cases = number_of_incidents) %>% 
    create_agegroups() %>% # Age Bands
    create_depgroups()   #deprivation groups
  
  # remove diagnosis field as just showing total cardiac
  sas_data_cardiac %<>%
    group_by(week_ending, hb, age_grp, sex, dep) %>%
    summarise(count = sum(number_of_cases, na.rm = T)) %>% 
    ungroup() %>%
    mutate(sex = recode(sex, "MALE" = "Male", "FEMALE" = "Female", "0" = NA_character_, "NOT KNOWN" = NA_character_),
           scot = "Scotland") %>% 
    proper() # convert HB names to correct format
  
  sas_data_cardiac %<>% 
    gather(area_type, area_name, c(area_name, scot)) %>% ungroup() %>% 
    rename(age = age_grp) %>% 
    mutate(area_type = recode(area_type, "area_name" = "Health board", "scot" = "Scotland")) %>% 
    # Aggregating to make it faster to work with
    group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
    summarise(count = sum(count, na.rm = T))  %>% ungroup() 
  
  # Creating totals for groups
  sas_cd_all <- sas_data_cardiac %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
  sas_cd_sex <- sas_data_cardiac %>% agg_cut(grouper="sex") %>% rename(category = sex)
  sas_cd_dep <- sas_data_cardiac %>% agg_cut(grouper="dep") %>% rename(category = dep)
  sas_cd_age <- sas_data_cardiac %>% agg_cut(grouper="age") %>% rename(category = age)
  
  sas_cardiac <- rbind(sas_cd_all, sas_cd_sex, sas_cd_dep, sas_cd_age)
  
  # Filter graphs that look odd due to small numbers
  sas_cardiac %<>% filter(!area_name %in% c("NHS Orkney", "NHS Shetland")) %>% 
    filter(!area_name %in% c("NHS Western Isles") | type !="dep")%>%
    filter(!area_name %in% c("NHS Western Isles") | type !="age")
  
  # Formatting file for shiny app
  prepare_final_data_cardiac(dataset = sas_cardiac, filename = "sas_cardiac", last_week = last_week)
  
  print("File sas_cardiac.rds produced and saved")

}

###############################################.
## Prescribing - Cardiovascular Drugs ----
###############################################.
create_cardiodrugs <- function(filedate, last_week) {
  cardio_drugs <- read_xlsx(paste0(data_folder, "prescribing_cardio/", filedate, "-covid emessage AMS only.xlsx")) %>% 
    select(1:5) %>% 
    clean_names() %>% 
    filter(condition %in% c("Antihypertensive, anti-anginal, anti-arrhythmic and heart failure drugs",
                            "Antiplatelet drugs",
                            "Oral anticoagulants",
                            "Lipid-lowering drugs")) %>% 
    mutate(week_ending = as.Date(week_ending),
           area_type = case_when(substr(area_code,1,3) == "S37" ~ "HSC partnership",
                                 substr(area_code,1,3) == "S08" ~ "Health board",
                                 substr(area_code,1,3) == "S00" ~ "Scotland"),
           area_name = case_when(area_type == "Health board" ~ stringr::str_to_title(area_name),
                                 area_type == "Scotland" ~ stringr::str_to_title(area_name),
                                 TRUE ~ area_name),
           area_name = case_when(area_type == "Health board" ~ gsub("Nhs", "NHS", area_name),
                                 TRUE ~ area_name),
           type = "condition") %>% 
    rename(category = condition,
           count = items) %>% 
    select(week_ending, area_name, area_type, type, category, count)
  
  cardio_drugs_all <- cardio_drugs %>% 
    group_by(week_ending, area_name, area_type, type) %>% 
    summarise(count = sum(count),
              category = "All") %>% 
    ungroup() %>% 
    select(week_ending, area_name, area_type, type, category, count)
  
  cardio_drugs <- rbind(cardio_drugs, cardio_drugs_all)
  
  prepare_final_data(cardio_drugs, "cardio_drugs", last_week = last_week)
  
  print("File cardio_drugs.rds produced and saved")
  
}

#########################################################################.
# Cardio admissions SMR01 ----
#########################################################################.

create_cardioadmissions <- function(last_week) {

# Speed up aggregations of different data cuts (A&E,NHS24,OOH)
# Adapted for cardiac discharges data
agg_cut_cardiac_dis <- function(dataset, grouper) {
  dataset %>%
    group_by_at(c("month_ending","diagnosis","type_admission","area_name", "area_type", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup() %>% 
    mutate(type = grouper) 
}  

# SMR start and end dates
smr_start_date <- format(ymd(20180101), "%d %B %Y")
smr_end_date <- format(ymd(last_week), "%d %B %Y")

SMRA_connect <- dbConnect(odbc(), 
                          dsn="SMRA",
                          uid=.rs.askForPassword("SMRA Username:"),
                          pwd=.rs.askForPassword("SMRA Password:"))
query_smr01 <- 
  glue("select LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, HBTREAT_CURRENTDATE, 
              MAIN_CONDITION, AGE_IN_YEARS age, SEX, ADMISSION_TYPE, DR_POSTCODE pc7, DATAZONE_2011
  from SMR01_PI 
  WHERE ADMISSION_DATE between '{smr_start_date}' AND '{smr_end_date}'
  ORDER BY LINK_NO, ADMISSION_DATE, DISCHARGE_DATE")

smr01_pi_data <- as_tibble(dbGetQuery(SMRA_connect, query_smr01)) %>%
  clean_names()
dbDisconnect(SMRA_connect)

# Condition strings
smr01_pi_data$mc3 <- substr(smr01_pi_data$main_condition, 0,3)

# Create blank diagnosis field to avoid ifelse NA issue
smr01_pi_data <- mutate(smr01_pi_data, diagnosis = "")

# CHD
smr01_ami <- smr01_pi_data %>% filter(mc3 %in% c("I21","I22")) %>%
  mutate(diagnosis = "Heart Attack")

smr01_hf <- smr01_pi_data %>% filter(mc3 %in% c("I50")) %>%
  mutate(diagnosis = "Heart Failure")

# Stroke
# Includes Subarachnoid Haemorrhage
smr01_str <- smr01_pi_data %>% filter(mc3 %in% c("I60","I61","I63","I64")) %>%
  mutate(diagnosis = "Stroke")

#browser()

# Filter to just include heart attack, stroke and heart failure
smr01_pi_data <- bind_rows(smr01_ami, smr01_hf, smr01_str)

# Note was issue with ceiling date creating wrong dates
smr01_pi_data <- smr01_pi_data %>%
  mutate(month_ending = quarter(as.Date(admission_date), type = "date_last", fiscal_start = 1)) %>%
  mutate(month_ending = floor_date(as.Date(month_ending), "month")) %>%
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         year = year(month_ending),
         hbname = match_area(hbtreat_currentdate)) #HB name
                                            
#Merging with deprivation and geography lookup
smr01_pi_data <- left_join(smr01_pi_data, dep_lookup) %>% rename(dep = sc_quin)

smr01_pi_data %<>% create_agegroups() %>%  # Age Bands
  create_depgroups() %>%  #deprivation groups
  select(-age) %>% 
  rename(age = age_grp)

# Selecting only emergency admissions
smr01_pi_data %<>% filter(between(admission_type, 20, 22) |
                                           between(admission_type, 30, 36) |
                                           between(admission_type, 38, 39))

smr01_pi_data <- rename(smr01_pi_data, type_admission = admission_type)

smr01_pi_data %<>%
  count(year, month_ending, hbname, diagnosis, type_admission, 
        sex, dep, age, name = "count") %>% 
  mutate(scot = "Scotland")

smr01_piv <- smr01_pi_data %>%
  mutate(scot = "Scotland") %>%
  pivot_longer(cols = c(hbname, scot), names_to="area_type", values_to="area_name") %>% 
    mutate (area_type = recode(area_type, "hbname" = "Health board", 
                   "scot" = "Scotland")) %>%
  mutate(area_name = case_when(area_type=="Health board" ~ (paste0("NHS ",gsub(" and ", " & ", area_name))), 
                               TRUE~area_name))  %>%
  group_by(month_ending, sex, dep, age, diagnosis, type_admission, area_name, area_type) %>% 
  summarise(count = sum(count))

# Create aggregations for each split
dis_all <- smr01_piv %>% agg_cut_cardiac_dis(grouper=NULL) %>% mutate(type = "sex", category = "All")
dis_sex <- smr01_piv %>% agg_cut_cardiac_dis(grouper="sex") %>% rename(category = sex)
dis_dep <- smr01_piv %>% agg_cut_cardiac_dis(grouper="dep") %>% rename(category = dep)
dis_age <- smr01_piv %>% agg_cut_cardiac_dis(grouper="age") %>% rename(category = age)

cardio_data_dis <- rbind(dis_all, dis_age, dis_sex, dis_dep) #%>% 

cardio_data_dis %<>% # Filter graphs that look odd due to small numbers
  filter(!area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles") | !type %in% c("age","dep")) %>% 
  filter(!area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles") | !category %in% c("Female","Male"))

prepare_final_data_m_cardiac(dataset = cardio_data_dis, filename = "cardio_admissions", 
                             last_month = last_week, extra_vars = "diagnosis", aver = 3)

final_cardio_SMR01 <- final_data %>% 
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
         variation =  ifelse(is.infinite(variation), NA_integer_, variation))

saveRDS(final_cardio_SMR01, paste0("shiny_app/data/cardio_admissions.rds"))
saveRDS(final_cardio_SMR01, paste0(data_folder,"final_app_files/cardio_admissions_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_cardio_SMR01, paste0(open_data, "cardio_admissions_data.rds"))

}

###############################################.
## Deaths Data Cardiac ----
## Provisional calendar year will be finalised summer 2022
## https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/births-deaths-and-other-vital-events-quarterly-figures/
###############################################.

create_cardiodeaths <- function(last_week) {

#last_week <- "2021-09-30" #just for testing not using function
last_month <- ymd(last_week)

# Speed up aggregations of different data cuts (A&E,NHS24,OOH)
agg_cut_cardiac <- function(dataset, grouper) {
  dataset %>%
    group_by_at(c("month_ending","diagnosis","area_name", "area_type", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup() %>% 
    mutate(type = grouper) 
}

SMRA_connect <- dbConnect(odbc(), 
                          dsn="SMRA",
                          uid=.rs.askForPassword("SMRA Username:"),
                          pwd=.rs.askForPassword("SMRA Password:"))

Query_Deaths <- 
  glue("select DATE_OF_REGISTRATION, AGE, SEX, UNDERLYING_CAUSE_OF_DEATH, 
              HBRES_CURRENTDATE hb, POSTCODE pc7, DATAZONE_2011 
       from ANALYSIS.GRO_DEATHS_C 
       WHERE DATE_OF_REGISTRATION >= '29 December 2014' 
            AND regexp_like(UNDERLYING_CAUSE_OF_DEATH, 'I2[12]|I50|I6[0134]') ")

# Extract data from database using SQL query above
cardio_data_deaths <- as_tibble(dbGetQuery(SMRA_connect, Query_Deaths)) %>%
  clean_names()

# Close connection
dbDisconnect(SMRA_connect)

cardio_data_deaths <- cardio_data_deaths %>%
  mutate(month_ending = quarter(as.Date(date_of_registration), type = "date_last", fiscal_start = 1)) %>%
  mutate(month_ending = floor_date(as.Date(month_ending), "month")) %>%
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         age = case_when(between(age, 0,74) ~ "Under 75", T ~ "75 and over"),
         year = year(month_ending)) #to allow merging

cardio_data_deaths$mc3 <- substr(cardio_data_deaths$underlying_cause_of_death, 0,3)

# Create blank diagnosis field to avoid ifelse NA issue
cardio_data_deaths <- mutate(cardio_data_deaths, diagnosis = "")

# CHD
deaths_ami <- cardio_data_deaths %>% filter(mc3 %in% c("I21","I22")) %>%
  mutate(diagnosis = "Heart Attack")

deaths_hf <- cardio_data_deaths %>% filter(mc3 %in% c("I50")) %>%
  mutate(diagnosis = "Heart Failure")

# Stroke
deaths_str <- cardio_data_deaths %>% filter(mc3 %in% c("I60","I61","I63","I64")) %>%
  mutate(diagnosis = "Stroke")

remove(cardio_data_deaths)

# Filter to just include heart attack, stroke and heart failure
cardio_data_deaths <- bind_rows(deaths_ami, deaths_hf, deaths_str)

#Merging with deprivation and geography lookup
cardio_data_deaths <- left_join(cardio_data_deaths, dep_lookup) 

#Pivoting so one row per area
cardio_data_deaths %<>% 
  mutate(scot = "Scotland") %>% 
  pivot_longer(cols = c(hb, scot)) %>% 
  #filtering out NA duplicates (which are counted in Scotland totals, but not elsewhere)
  filter(!is.na(value)) %>% 
  # More formatting
  mutate(area_name = case_when(value == "Scotland" ~ "Scotland", 
                               T ~ match_area(value)),
         dep = recode(sc_quin, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         area_type = recode(name, "hb" = "Health board", 
                            "scot" = "Scotland")) %>% 
  mutate(area_name = case_when(area_type=="Health board" ~ (paste0("NHS ",gsub(" and ", " & ", area_name))), 
                               TRUE~area_name)) %>% 
  # Aggregating to make it faster to work with
  count(month_ending, sex, dep, age, diagnosis, area_name, area_type, name = "count") 

# Create aggregations for each split
deaths_all <- cardio_data_deaths %>% agg_cut_cardiac(grouper=NULL) %>% mutate(type = "sex", category = "All")
deaths_sex <- cardio_data_deaths %>% agg_cut_cardiac(grouper="sex") %>% rename(category = sex)
deaths_dep <- cardio_data_deaths %>% agg_cut_cardiac(grouper="dep") %>% rename(category = dep)
deaths_age <- cardio_data_deaths %>% agg_cut_cardiac(grouper="age") %>% rename(category = age)

cardio_data_deaths <- rbind(deaths_all, deaths_age, deaths_sex, deaths_dep) %>% 
  #filter(!(area_type != "Scotland" & type == "dep")) %>% #SIMD only at Scotland level - only req for weekly data
  mutate(area_id = paste(area_type, "-", area_name)) # this helps with the next step

cardio_data_deaths %<>%
  mutate(area_id = paste(area_type, "-", area_name)) %>% 
  filter(area_id %in% unique(cardio_data_deaths$area_id)) %>% 
  select(-area_id) %>% 
  filter(area_name != "NHS Unknown residency")

cardio_data_deaths %<>% # Filter graphs that look odd due to small numbers
  filter(area_name %in% c("Scotland") | !type %in% c("age","dep")) %>%
  filter(area_name %in% c("Scotland") | !category %in% c("Female","Male"))
#  filter(!area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles") | diagnosis !="Heart Failure") %>%

# Running final functions
prepare_final_data_m_cardiac(dataset = cardio_data_deaths, filename = "cardio_deaths", 
                             last_month, extra_vars = "diagnosis", aver = 5)

# Dealing with variation to replicate previous output. 
# This might not be needed in future if we set a standard way of dealing with this.
final_cardio_deaths <- final_data %>% 
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
         variation =  ifelse(is.infinite(variation), NA_integer_, variation)) 

saveRDS(final_cardio_deaths, paste0("shiny_app/data/cardio_deaths.rds"))
saveRDS(final_cardio_deaths, paste0(data_folder,"final_app_files/cardio_deaths_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_cardio_deaths, paste0(open_data, "cardio_deaths_data.rds"))

}


