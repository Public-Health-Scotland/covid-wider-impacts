# Data preparation for cardiovascular tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

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

###############################################.
## Hospital Discharges Cardiac ----
###############################################.

### 1 - Load packages ----
library(odbc)       # for accessing SMRA
library(glue)       # For SQL date parameters

filedate <- "2021-11-29"

# SMR Start Date
smr_start_date <- ymd(20180101)

# SMR End Date
smr_end_date <- ymd(20210331)

###############################################.
## Lookups ---- from deaths_data_preparation.R
###############################################.
# Bringing  LA and datazone info.
postcode_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2021_2.rds') %>% 
  setNames(tolower(names(.))) %>%   #variables to lower case
  select(pc7, datazone2011, hscp2019, hscp2019name)

# SIMD quintile to datazone lookup
dep_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/deprivation_geography.rds") %>%
  rename(datazone2011 = datazone) %>%
  select(datazone2011, year, sc_quin) %>%
  filter(year>2014)

dep_lookup21 <- dep_lookup %>%  filter(year == 2019) %>% mutate(year = 2021)

dep_lookup <- rbind(dep_lookup, dep_lookup21)

geo_lookup <- left_join(dep_lookup, postcode_lookup)

#########################################################################
# SMR01_PI DATA April 1997 Onwards
#########################################################################

SMRA_connect <- dbConnect(odbc(), 
                          dsn="SMRA",
                          uid=.rs.askForPassword("SMRA Username:"),
                          pwd=.rs.askForPassword("SMRA Password:"),
                          port = "1527",
                          host = "nssstats01.csa.scot.nhs.uk",
                          SVC = "SMRA.nss.scot.nhs.uk")

query_smr01 <- 
  glue("select LINK_NO, CIS_MARKER, DISCHARGE_DATE, HBTREAT_CURRENTDATE, MAIN_CONDITION, AGE_IN_YEARS,
  SEX, ADMISSION_TYPE, INPATIENT_DAYCASE_IDENTIFIER, POSTCODE, DR_POSTCODE
  from SMR01_PI WHERE DISCHARGE_DATE >= TO_DATE({shQuote(smr_start_date, type = 'sh')}, 'yyyy-mm-dd') AND 
  DISCHARGE_DATE <= TO_DATE({shQuote(smr_end_date, type = 'sh')}, 'yyyy-mm-dd')
  ORDER BY LINK_NO, ADMISSION_DATE, DISCHARGE_DATE")

smr01_pi_data <- as_tibble(dbGetQuery(SMRA_connect, query_smr01)) %>%
  clean_names()
dbDisconnect(SMRA_connect)

# Condition strings
smr01_pi_data$mc3 <- substr(smr01_pi_data$main_condition, 0,3)
smr01_pi_data$mc1 <- substr(smr01_pi_data$main_condition, 0,1)
smr01_pi_data$mc2 <- as.numeric(substr(smr01_pi_data$main_condition, 2,3))

# IHD and CVD diagnosis
smr01_pi_data <- mutate(smr01_pi_data, diagnosis = "")
smr01_pi_data$diagnosis <- ifelse(smr01_pi_data$mc3 %in% c("I20","I21","I22","I23","I24","I25"), 
                               "Coronary Heart Disease", smr01_pi_data$diagnosis)
smr01_pi_data$diagnosis <- ifelse(smr01_pi_data$mc1 == "I" & smr01_pi_data$mc2 %in% 60:69 | (smr01_pi_data$mc3 == "G45"),
                               "Cerebrovascular Disease", smr01_pi_data$diagnosis)

#tabyl(smr01_pi_data$diagnosis, sort = TRUE)

smr01_pi_data <- smr01_pi_data %>%
  filter(smr01_pi_data$diagnosis %in% c("Coronary Heart Disease","Cerebrovascular Disease"))

smr01_pi_data <- smr01_pi_data %>%
mutate(month_ending = floor_date(as.Date(discharge_date), "month")) %>% 
    mutate(sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         year = year(month_ending)) #to allow merging

smr01_pi_data <- smr01_pi_data %>% rename(pc7 = dr_postcode)
smr01_pi_data <- smr01_pi_data %>% rename(age = age_in_years)

smr01_pi_data <- smr01_pi_data %>% mutate(hbname = match_area(hbtreat_currentdate))
                                            
#Merging with deprivation and geography lookup
smr01_pi_data <- left_join(smr01_pi_data, geo_lookup) %>% select(-datazone2011) 
smr01_pi_data <- smr01_pi_data %>% rename(dep = sc_quin)

smr01_pi_data <- smr01_pi_data %>% create_agegroups() # Age Bands
smr01_pi_data <- smr01_pi_data %>% create_depgroups() #deprivation groups
smr01_pi_data <- smr01_pi_data %>% select(-age)
smr01_pi_data <- smr01_pi_data %>% rename(age = age_grp)

# Admission type
smr01_pi_data = smr01_pi_data %>%
  mutate(type_admission = case_when(
    between(admission_type, 20, 22) ~ "Emergency",
    between(admission_type, 30, 36) ~ "Emergency",
    between(admission_type, 38, 39) ~ "Emergency",
    between(admission_type, 10, 12) ~ "Elective",
    between(admission_type, 19, 19) ~ "Elective",
    between(admission_type, 18, 18) ~ "Transfer",
    between(admission_type, 40, 40) ~ "Other",
    between(admission_type, 42, 42) ~ "Other",
    between(admission_type, 48, 48) ~ "Other"))

smr01_pi_data <- smr01_pi_data %>%
group_by(year, month_ending, hbname, hscp2019name, diagnosis, type_admission, sex, dep, age) %>% 
  summarise(count = n())

smr01_pi_data <- smr01_pi_data %>% mutate(scot = "Scotland")

smr01_piv <- smr01_pi_data %>%
  mutate(scot = "Scotland") %>%
  pivot_longer(cols = c(hbname, hscp2019name, scot), names_to="area_type", values_to="area_name") %>% 
    mutate (area_type = recode(area_type, "hbname" = "Health board", 
                   "hscp2019name" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  mutate(all="All") %>%
  group_by(year, month_ending, all, sex, dep, age, diagnosis, type_admission, area_type, area_name) %>% 
  summarise(count = sum(count))
  
smr01_piv2 <- smr01_piv %>%
  pivot_longer(cols = c(all, sex, dep, age), names_to="type", values_to="category") %>% 
  group_by(month_ending, diagnosis, type_admission, area_type, area_name, type, category) %>% 
  filter(!(area_type != "Scotland" & area_type != "Health board" & type == "dep")) %>% #SIMD only at HB/Scotland level
  summarise(count= sum(count))

# Using function for monthly data from injuries_data_preparation script
prepare_final_data_m_cardiac(dataset = smr01_piv2, filename = "cardio_discharges", 
                             last_month = "2021-05-01", extra_vars = "diagnosis", extra_vars2 = "type_admission", aver = 3)

final_cardio_SMR01 <- final_data %>% 
  rename (week_ending=month_ending) %>%
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation =  case_when(count_average == 0 & count == 0 ~ 0, T ~ variation),
         variation =  ifelse(is.infinite(variation), NA_integer_, variation))

saveRDS(final_cardio_SMR01, paste0("shiny_app/data/cardio_discharges.rds"))
saveRDS(final_cardio_SMR01, paste0(data_folder,"final_app_files/cardio_discharges_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(final_cardio_SMR01, paste0(open_data, "cardio_discharges_data.rds"))


###############################################.
## Deaths Data Cardiac ----
###############################################.

last_month <- "2021-09-25"

# Speed up aggregations of different data cuts (A&E,NHS24,OOH)
agg_cut_cardiac <- function(dataset, grouper) {
  dataset %>%
    group_by_at(c("month_ending","diagnosis","area_name", "area_type", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup() %>% 
    mutate(type = grouper) 
}

library(odbc)
# SMR login details
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

cardio_data_deaths <- as_tibble(dbGetQuery(channel, statement=
   "SELECT date_of_registration, age, sex, UNDERLYING_CAUSE_OF_DEATH, 
    HBRES_CURRENTDATE hb, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_C
    WHERE date_of_registration >= '29 December 2014'
  UNION ALL
    SELECT date_of_registration, age, sex, UNDERLYING_CAUSE_OF_DEATH, 
        HB9 hb, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_WEEKLY_C")) %>%
  setNames(tolower(names(.))) %>% 
  # Formatting variables - using month keeping variable name week_ending to use functions
  mutate(month_ending = ceiling_date(as.Date(date_of_registration), "month")) %>% 
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         age = case_when(between(age, 0,64) ~ "Under 65", T ~ "65 and over"),
         year = year(month_ending)) #to allow merging

#cardio_data_deaths <- cardio_data_deaths %>%
#  filter(date_of_registration <= as.Date(last_week))

cardio_data_deaths$mc3 <- substr(cardio_data_deaths$underlying_cause_of_death, 0,3)
cardio_data_deaths$mc1 <- substr(cardio_data_deaths$underlying_cause_of_death, 0,1)
cardio_data_deaths$mc2 <- as.numeric(substr(cardio_data_deaths$underlying_cause_of_death, 2,3))

# IHD and CVD diagnosis
cardio_data_deaths <- mutate(cardio_data_deaths, diagnosis = "")
cardio_data_deaths$diagnosis <- ifelse(cardio_data_deaths$mc3 %in% c("I20","I21","I22","I23","I24","I25"), 
                                  "Coronary Heart Disease", cardio_data_deaths$diagnosis)
cardio_data_deaths$diagnosis <- ifelse(cardio_data_deaths$mc1 == "I" & cardio_data_deaths$mc2 %in% 60:69 | (cardio_data_deaths$mc3 == "G45"),
                                  "Cerebrovascular Disease", cardio_data_deaths$diagnosis)

#tabyl(smr01_pi_data$diagnosis, sort = TRUE)

cardio_data_deaths <- cardio_data_deaths %>%
  filter(cardio_data_deaths$diagnosis %in% c("Coronary Heart Disease","Cerebrovascular Disease"))

#Merging with deprivation and geography lookup
cardio_data_deaths <- left_join(cardio_data_deaths, geo_lookup) %>% select(-datazone2011) 

#Pivoting so one row per area
cardio_data_deaths %<>% 
  mutate(scot = "Scotland") %>% 
  pivot_longer(cols = c(hb, hscp2019, scot)) %>% 
  #filtering out NA duplicates (which are counted in Scotland totals, but not elsewhere)
  filter(!is.na(value)) %>% 
  # More formatting
  mutate(area_name = case_when(value == "Scotland" ~ "Scotland", 
                               T ~ match_area(value)),
         dep = recode(sc_quin, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         area_type = recode(name, "hb" = "Health board", 
                            "hscp2019" = "HSC partnership", "scot" = "Scotland")) %>% 
  mutate(area_name = case_when(area_type=="Health board" ~ (paste0("NHS ",gsub(" and ", " & ", area_name))), 
                               TRUE~area_name)) %>% 
  # Aggregating to make it faster to work with
  group_by(month_ending, sex, dep, age, diagnosis, area_name, area_type) %>% 
  summarise(count = n())  %>% ungroup()

# Create aggregations for each split
deaths_all <- cardio_data_deaths %>% agg_cut_cardiac(grouper=NULL) %>% mutate(type = "sex", category = "All")
deaths_sex <- cardio_data_deaths %>% agg_cut_cardiac(grouper="sex") %>% rename(category = sex)
deaths_dep <- cardio_data_deaths %>% agg_cut_cardiac(grouper="dep") %>% rename(category = dep)
deaths_age <- cardio_data_deaths %>% agg_cut_cardiac(grouper="age") %>% rename(category = age)

cardio_data_deaths <- rbind(deaths_all, deaths_age, deaths_sex, deaths_dep) %>% 
  filter(!(area_type != "Scotland" & type == "dep")) %>% #SIMD only at Scotland level
  mutate(area_id = paste(area_type, "-", area_name)) # this helps with the next step

# This step is to make sure we have rows for all weeks for all areas/category
# even those with zeroes. It's a bit convoluted but it works
cardio_data_deaths %<>%
  #pivot_wider(id_cols = c(area_type, diagnosis, category, type, month_ending), 
  #            names_from = area_name, values_from = count, values_fill = list(count = 0)) %>% 
  #pivot_longer(c(`Aberdeen City`:`Western Isles`), values_to = "count", names_to = "area_name") %>% 
  # This is to get rid of combinations that don't exist (e.g. Scotland - Fife)
  mutate(area_id = paste(area_type, "-", area_name)) %>% 
  filter(area_id %in% unique(cardio_data_deaths$area_id)) %>% 
  select(-area_id) %>% 
  filter(area_name != "NHS Unknown residency")

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

#final_cardio_deaths <<- final_cardio_deaths

print("deaths_data.rds file prepared and saved, including open data")


##END

check <- readRDS("/PHI_conf/HeartDiseaseStroke/Topics/covid-wider-update/colin/covid-wider-impact-pra/shiny_app/data/cardio_smr01_discharges.rds")
check <- readRDS("/PHI_conf/HeartDiseaseStroke/Topics/covid-wider-update/colin/covid-wider-impact-pra/shiny_app/data/cardio_deaths.rds")

tabyl(smr01_pi_data$month_ending, sort = TRUE)
tabyl(cardio_discharges$type_admission, sort = TRUE)
tabyl(cardio_discharges$diagnosis, sort = TRUE)



