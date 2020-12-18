# Data preparation for app

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("functions_packages_data_prep.R")

###############################################.
## RAPID data ----
###############################################.
# Prepared by Unscheduled care team
rap_adm <- readRDS(paste0(data_folder, "rapid/Admissions_by_category_30-Nov.rds")) %>% 
  janitor::clean_names() %>% 
  # taking out aggregated values, not clear right now
  filter(!(substr(hosp,3,5) == "All" | (substr(hscp_name,3,5) == "All")) &
           date_adm > as.Date("2017-12-31")) 

# Bringing HB names
rap_adm <- left_join(rap_adm, hb_lookup, by = c("hb" = "hb_cypher")) %>% 
  select(-hb) %>% rename(hb = area_name) %>% select(-area_type)

# Bringing spec names
spec_lookup <- read_csv("data/spec_groups_dashboard.csv")

rap_adm <- left_join(rap_adm, spec_lookup, by = c("spec" = "spec_code")) %>% 
  select(-spec) %>% rename(spec = dash_groups)

# For modal in app
spec_lookup <- spec_lookup %>% filter(!(dash_groups %in% c("Dental", "Other"))) %>% 
  arrange(dash_groups, spec_name) %>% 
  mutate(dash_groups = case_when(
    spec_name == "Paediatric Dentistry" ~ "Paediatrics (medical)",
    spec_name == "Paediatrics" ~ "Paediatrics (medical)",
    spec_name == "Paediatric Surgery" ~ "Paediatrics (surgical)",
    TRUE ~ dash_groups
  )) %>% 
  select("Specialty name" = spec_name, "Specialty group" = dash_groups)

saveRDS(spec_lookup, "shiny_app/data/spec_lookup.rds")
saveRDS(spec_lookup, paste0(data_folder,"final_app_files/spec_lookup_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# Formatting groups
rap_adm %<>% 
  rename(dep = simd_quintile, age = age_group) %>%
  mutate(sex = recode(sex, "male" = "Male", "female" = "Female")) %>% 
  mutate(age = recode_factor(age, "Under_5" = "Under 5", "5_thru_14" = "5 - 14", 
                             "15_thru_44" = "15 - 44", "45_thru_64" = "45 - 64",
                             "65_thru_74" = "65 - 74", "75_thru_84" = "75 - 84",
                             "85+" = "85 and over")) %>% 
  create_depgroups()  %>% 
  mutate(admission_type = recode(admission_type, "elective" = "Planned", "emergency" = "Emergency")) %>% 
  mutate(spec = case_when(
    spec_name == "Paediatric Dentistry" ~ "Paediatrics (medical)",
    spec_name == "Paediatrics" ~ "Paediatrics (medical)",
    spec_name == "Paediatric Surgery" ~ "Paediatrics (surgical)",
    TRUE ~ spec
  ))

# Aggregating to weekly data
rap_adm %<>% 
  mutate(week_ending = ceiling_date(date_adm, "week", change_on_boundary = F)) %>% #end of week
  group_by(hscp_name, hb, admission_type, dep, age, sex, week_ending, spec) %>% 
  summarise(count = sum(count, na.rm = T))

# Aggregating for each geo level
rap_adm %<>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, c(hb, hscp_name, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "hb" = "Health board", 
                            "hscp_name" = "HSC partnership", "scot" = "Scotland")) 

# Aggregating to obtain totals for each split type and then putting all back together
# Totals for overalls for all pop including totals by specialty too
rap_adm_all <- agg_rapid(NULL, split = "sex", specialty = T) %>% mutate(category = "All") 
rap_adm_sex <- agg_rapid(c("sex"), split = "sex") %>% rename(category = sex) # Totals for overalls for all sexes
rap_adm_age <- agg_rapid(c("age"), split = "age") %>% rename(category = age) # Totals for overalls for all age groups
# Totals for overalls for deprivation quintiles
rap_adm_depr <- agg_rapid(c("dep"), split = "dep") %>% rename(category = dep) 

rap_adm <- rbind(rap_adm_all, rap_adm_depr, rap_adm_sex, rap_adm_age) 

# Producing data for combined medical specialty
spec_med <- rap_adm %>% 
  filter(spec %in% c("Cancer", "Medical (excl. Cardiology & Cancer)", "Cardiology")) %>% 
  mutate(spec = "Medical (incl. Cardiology & Cancer)") %>% 
  group_by(week_ending, area_name, area_type, type, 
           admission_type, spec, category) %>% 
  summarise(count = sum(count, na.rm = T)) %>% ungroup

# Producing data for combined Paediatrics specialty
paed_com <- rap_adm %>% 
  filter(spec %in% c("Paediatrics (medical)", "Paediatrics (surgical)")) %>% 
  mutate(spec = "Paediatrics (medical & surgical)") %>% 
  group_by(week_ending, area_name, area_type, type, 
           admission_type, spec, category) %>% 
  summarise(count = sum(count, na.rm = T)) %>% ungroup

rap_adm <- rbind(rap_adm, spec_med, paed_com) %>% 
  # Excluding specialties groups with very few cases and of not much interest
  filter(!(spec %in% c("Dental", "Other"))) 

prepare_final_data(rap_adm, "rapid", last_week = "2020-11-22", 
                   extra_vars = c("admission_type", "spec"))

###############################################.
## OOH data ----
###############################################.
# Saving big files as RDS to avoid unzipping 
# ooh <- read_excel(paste0(data_folder, "GP_OOH/WIDER IMPACT PC OOH Data Update_2018to26042020.xlsx")) 
# saveRDS(ooh, paste0(data_folder,"GP_OOH/OOH DATA 2018to26042020.rds"))
# file.remove(paste0(data_folder,"GP_OOH/WIDER IMPACT PC OOH Data Update_2018to26042020.xlsx"))

# Read in historic OOH file
ooh <- readRDS(paste0(data_folder, "GP_OOH/OOH DATA 2018to26042020.rds")) %>%
  janitor::clean_names() %>%
  rename(hb=treatment_nhs_board_name, hscp=hscp_of_residence_name_current,
         dep=prompt_dataset_deprivation_scot_quintile,sex=gender,
         count=number_of_cases) %>%
  mutate(age = recode_factor(age_band, "0-4" = "Under 5", 
                             "70-74"  = "65 - 74", "90+" = "85 and over", "10-14" = "5 - 14", 
                             "15-19" = "15 - 44", "20-24" = "15 - 44", "25-29" = "15 - 44", 
                             "30-34" = "15 - 44", "35-39" = "15 - 44", "40-44" = "15 - 44", 
                             "45-49" = "45 - 64", "50-54" = "45 - 64", "55-59" = "45 - 64", "5-9" = "5 - 14", 
                             "60-64" = "45 - 64", "65-69"  = "65 - 74", "75-79"= "75 - 84", "80-84"= "75 - 84", 
                             "85-89" = "85 and over"),
         sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         dep = recode(dep, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         scot = "Scotland") %>%
  proper() #convert HB names to correct format

# Aggregate up to get figures for each area type.
ooh %<>% gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>%
  filter(between(week_ending, as.Date("2018-01-01"), as.Date("2020-04-26")))

#new data extract from week ending 03 may 2020 up to week ending 31 may 2020
ooh_may_onwards <- read_excel(paste0(data_folder, "GP_OOH/WIDER IMPACT PC OOH Data_56_5474569671804807881.xlsx")) %>% 
  janitor::clean_names() %>%
  rename(count=number_of_cases, hscp=hscp_of_residence_name_current, age_group=age_band,
         hb=treatment_nhs_board_name, sex=gender, dep=prompt_dataset_deprivation_scot_quintile) %>%
  mutate(age_group = recode(age_group, "44079" = "5-9", "41913" = "10-14"),
         age = recode_factor(age_group, "0-4" = "Under 5", "5-9" = "5 - 14",  "10-14" = "5 - 14",  
                             "15-19" = "15 - 44", "20-24" = "15 - 44", "25-29" = "15 - 44", 
                             "30-34" = "15 - 44", "35-39" = "15 - 44", "40-44" = "15 - 44", 
                             "45-49" = "45 - 64", "50-54" = "45 - 64", "55-59" = "45 - 64", 
                             "60-64" = "45 - 64", "65-69" = "65 - 74", "70-74" = "65 - 74",
                             "75-79" = "75 - 84", "80-84" = "75 - 84", "85-89" = "85 and over",
                             "90+" = "85 and over"),
         sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         dep = recode(dep, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         scot = "Scotland") %>% 
  proper() # convert HB names to correct format

ooh_may_onwards <- ooh_may_onwards %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup()

#bind old and new ooh data
ooh <- rbind(ooh_may_onwards, ooh)

# Creating totals for groups
ooh_all <- ooh %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
ooh_sex <- ooh %>% agg_cut(grouper="sex") %>% rename(category = sex)
ooh_dep <- ooh %>% agg_cut(grouper="dep") %>% rename(category = dep)
ooh_age <- ooh %>% agg_cut(grouper="age") %>% rename(category = age)

ooh <- rbind(ooh_all, ooh_sex, ooh_dep, ooh_age)

# Formatting file for shiny app
prepare_final_data(dataset = ooh, filename = "ooh", last_week = "2020-11-22")

###############################################.
## A&E data ----
###############################################.
# Read A&E data both at HSCP and HB level
ae_data <- rbind(read_csv(unz(paste0(ae_folder,"2020-11-26-HSCP-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"),
                              "HSCP.csv")) %>% 
                   janitor::clean_names() %>% 
                   rename(area=hscp_of_residence_code_as_at_arrival_date),
                 read_csv(unz(paste0(ae_folder,"2020-11-26-NHSBoard-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"), 
                              "NHS Boards.csv")) %>% 
                   janitor::clean_names() %>% 
                   rename(area=treatment_nhs_board_9_digit_code_as_at_date_of_episode))

# Format data
ae_data %<>% 
  rename(dep=prompt_dataset_deprivation_scot_quintile, age=pat_age,
         sex=pat_gender_code, count=number_of_attendances) %>% 
  mutate(area_name = match_area(area), #use PHS methods package to add area names
         area_type= case_when(substr(area,1,3) == "S37" ~ "HSC partnership",
                              substr(area,1,3) == "S08" ~ "Health board")) %>%
  create_agegroups() %>%
  create_sexgroups() %>%
  create_depgroups() %>%
  group_by(week_ending, area_name, area_type, age_grp, sex, dep, area) %>%
  summarise(count=sum(count)) %>%
  ungroup() 

# Generate scotland level dataset
ae_scot <- ae_data %>% filter( substr(area ,1,3) == "S08") %>% 
  group_by(week_ending, age_grp, sex, dep) %>%
  summarise(count=sum(count)) %>%
  mutate(area_name="Scotland",
         area_type="Scotland") %>% ungroup()

ae_data <- rbind(ae_data %>% select(-area), ae_scot) %>% 
  rename(age=age_grp) %>%  mutate(week_ending=as.Date(week_ending,format="%d/%m/%Y")) %>% 
  mutate(area_name = case_when(area_type=="Health board" ~ (paste0("NHS ",gsub(" and ", " & ", area_name))), 
                               TRUE~area_name)) 
##Reshape dataset for shiny app
#Use aggregation function to aggregate data files into format
ae_all <- ae_data %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
ae_sex <- agg_cut(dataset=ae_data, grouper="sex") %>% rename(category=sex)
ae_dep <- agg_cut(dataset=ae_data, grouper="dep") %>% rename(category=dep)
ae_age <- agg_cut(dataset=ae_data, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
ae_data <- rbind(ae_all, ae_sex, ae_dep, ae_age) 

prepare_final_data(ae_data, "ae", last_week = "2020-11-22")

###############################################.
## NHS24 data ----
###############################################.

# #Read in new nhs24 data as txt file, save as RDS and remove txt file version from directory.
# #Each week this section of code can be uncommented run for the latest weeks data then recommented after txt file deleted
     # nhs24 <- (read_tsv(paste0(data_folder,"NHS24/NHS24_Extract 08062020 to 27092020.txt")))
     # saveRDS(nhs24, paste0(data_folder,"NHS24/NHS24 Extract 17082020 to 23082020.rds"))
     # file.remove(paste0(data_folder,"NHS24/NHS24 Extract 17082020 to 23082020.txt"))

nhs24 <-  rbind(readRDS(paste0(data_folder, "NHS24/NHS24 01Jan2018 to 07Jun2020.rds")),
                read_tsv(paste0(data_folder, "NHS24/NHS24_report_08062020to29112020.txt"))) %>%
  janitor::clean_names() %>% 
  rename(hb = patient_nhs_board_description_current,
         hscp = nhs_24_patient_hscp_name_current,
         sex = gender_description,
         dep = nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
         covid_flag = nhs_24_covid_19_flag,
         week_ending = nhs_24_call_rcvd_date,
         count = number_of_nhs_24_records) %>% 
  # Formatting dates
  mutate(week_ending = as.Date(week_ending, format="%d-%b-%y"),
         week_ending = ceiling_date(week_ending, "week", change_on_boundary = F))

# Joining with latest data and formatting
nhs24 %<>%
  mutate(sex = str_to_title(sex)) %>% 
  proper() %>% #convert HB names to correct format
  create_agegroups () %>%
  create_depgroups () 

# Aggregate to weekly data
nhs24 %<>% 
  group_by(hscp, sex, dep, age_grp, week_ending, area_name) %>% 
  summarise(count = sum(count, na.rm = T)) %>% ungroup()

# Aggregate up to get figures for each area type.
nhs24 %<>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age_grp, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>% rename(age = age_grp)

# Use aggregation function to aggregate data files for use in shiny app
nhs24_allsex <- nhs24 %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
nhs24_sex <- agg_cut(dataset= nhs24, grouper="sex") %>% rename(category=sex)
nhs24_dep <- agg_cut(dataset= nhs24, grouper="dep") %>% rename(category=dep)
nhs24_age <- agg_cut(dataset= nhs24, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
nhs24 <- rbind(nhs24_allsex, nhs24_sex, nhs24_dep, nhs24_age)

# Formatting file for shiny app
prepare_final_data(dataset = nhs24, filename = "nhs24", last_week = "2020-11-29")

###############################################.
## SAS data ----
###############################################.
# Code to transform extract to rds and delete giant txt file
# sas <-(read_tsv(paste0(data_folder,"SAS/COVID_WIDER_IMPACT_SAS_01012018to10052020.txt")))
# saveRDS(sas, paste0(data_folder,"SAS/COVID_WIDER_IMPACT_SAS_01012018to10052020.rds"))
# file.remove(paste0(data_folder,"SAS/COVID_WIDER_IMPACT_SAS_01012018to10052020.txt"))

sas <- readRDS(paste0(data_folder,"SAS/COVID_WIDER_IMPACT_SAS_01012018to10052020.rds")) %>%
  janitor::clean_names() %>%
  rename(hb=reporting_health_board_name_current, hscp=patient_hscp_name_current,
         dep=patient_prompt_dataset_deprivation_scot_quintile,
         count=number_of_incidents,gender=gender_description) %>%
  select(-sas_call_start_calendar_week) %>%
  # Formatting dates and sex
  mutate(week_ending = as.Date(week_ending, format="%d-%b-%Y"),
         sex=case_when(is.na(gender)~"Missing", gender=="" ~"Missing", gender=="MALE" ~ "Male", gender=="FEMALE" ~"Female", 
                       gender %in% c(0, 9 ) ~ "Missing", TRUE ~ as.character(gender))) %>% 
  proper() %>% #convert HB names to correct format
  create_agegroups () %>%
  create_depgroups () %>%
  filter(between(week_ending, as.Date("2018-01-07"), as.Date("2020-05-10")))  #filter complete weeks (Mon-Sun)

# Aggregate up to get figures for each area type.
sas %<>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age_grp, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>% rename(age = age_grp)

#NEW WEEKLY DATA UPDATE
sas_new <- read_tsv(paste0(data_folder,"SAS/2020-11-30-COVID WIDER IMPACT SAS_Prompt report.txt")) %>% 
  janitor::clean_names() %>%
  rename(hb=reporting_health_board_name_current, hscp=patient_hscp_name_current,
         dep=patient_prompt_dataset_deprivation_scot_quintile,
         count=number_of_incidents,gender=gender_description) %>%
  select(-sas_call_start_calendar_week) %>%
  # Formatting dates and sex
  mutate(week_ending = as.Date(week_ending, format="%d-%b-%Y"),
         sex=case_when(is.na(gender)~"Missing", gender=="" ~"Missing", gender=="MALE" ~ "Male", gender=="FEMALE" ~"Female", 
                       gender %in% c(0, 9 ) ~ "Missing", TRUE ~ as.character(gender))) %>% 
  proper() %>% #convert HB names to correct format
  create_agegroups () %>%
  create_depgroups () %>% 
  # filter needed as because start/end dates assignations some cases could
  # be double counted for that end week
  filter(week_ending > as.Date("2020-05-10"))  

# Aggregate up to get figures for each area type.
sas_new %<>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age_grp, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>% rename(age = age_grp)

#bind old and new SAS data
sas <- rbind(sas_new, sas)

# Use aggregation function to aggregate data files for use in shiny app
sas_allsex <- sas %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
sas_sex <- agg_cut(dataset= sas, grouper="sex") %>% rename(category=sex)
sas_dep <- agg_cut(dataset= sas, grouper="dep") %>% rename(category=dep)
sas_age <- agg_cut(dataset= sas, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
sas <- rbind(sas_allsex, sas_sex, sas_dep, sas_age)

# Formatting file for shiny app
prepare_final_data(dataset = sas, filename = "sas", last_week = "2020-11-22")

###############################################.
## Cath labs - cardiac procedures ----
###############################################.
# Data for cardiovascular app
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

###############################################.
## A&E Cardio ----
###############################################.

# Reads in A&E cardio ICD 10 codes for modal, only required to run in case code list changes
ae_cardio_codes <- read_xlsx(paste0(data_folder, "A&E_Cardio/A&E-CardioConditionCodes.xlsx"))
saveRDS(ae_cardio_codes, "shiny_app/data/ae_cardio_codes.rds")
saveRDS(ae_cardio_codes, paste0(data_folder,"final_app_files/ae_cardio_codes_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
rm(ae_cardio_codes)

# Read in data, clean names + some simple mutations
ae_cardio <- read_xlsx(paste0(ae_folder, "2020-11-26-CardioVascular-AttendancesDuringCovid-19.xlsx")) %>% 
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

prepare_final_data(ae_cardio, "ae_cardio", last_week = "2020-11-22")

###############################################.
## OOH Cardiac  ----
###############################################.

ooh_data_cardiac <- read_csv(paste0(data_folder, "GP_OOH_Cardio/Weekly Diagnosis OOH CSV.csv")) %>% 
  janitor::clean_names() %>% 
  filter(age > 14) # Filter age > 14

# Change file into correct format prior to getting final specification
ooh_data_cardiac <- ooh_data_cardiac %>% 
  rename(hb = reporting_health_board_name_as_at_date_of_episode,
         dep = patient_prompt_dataset_deprivation_scot_quintile, 
         sex = gender_description,
         number_of_cases = gp_ooh_number_of_cases)

ooh_data_cardiac = ooh_data_cardiac %>%
  create_agegroups() %>% #age bands
  create_depgroups() # deprivation groups format

# remove diagnosis field as just showing total cardiac
ooh_data_cardiac <- ooh_data_cardiac %>%
  group_by(week_ending, hb, age_grp, sex, dep) %>%
  summarise(count = sum(number_of_cases, na.rm = T)) %>% ungroup

ooh_data_cardiac <- ooh_data_cardiac %>% 
  mutate( sex = recode(sex, "MALE" = "Male", "FEMALE" = "Female", "NOT SPEC" = NA_character_, "NOT KNOWN" = NA_character_),
         week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         scot = "Scotland") %>% 
  proper() # convert HB names to correct format

ooh_data_cardiac <- ooh_data_cardiac %>% 
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

# Filter graphs that look odd due to small numbers
ooh_cardiac <- ooh_cardiac %>% filter(!area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles"))
ooh_cardiac <- ooh_cardiac %>% filter(!area_name %in% c("NHS Borders", "NHS Dumfries & Galloway", 
                                                        "NHS Lanarkshire") | type !="age")
ooh_cardiac <- ooh_cardiac %>% filter(!area_name %in% c("NHS Borders", "NHS Dumfries & Galloway", 
                                                        "NHS Fife", "NHS Highland") | type !="dep")

# Formatting file for shiny app
prepare_final_data_cardiac(dataset = ooh_cardiac, filename = "ooh_cardiac", last_week = "2020-11-08")

###############################################.
## SAS Cardiac ----
###############################################.

sas_data_cardiac <- read_csv(paste0(data_folder,"SAS_Cardio/Weekly Diagnosis SAS CSV.csv")) %>%
  janitor::clean_names() %>% 
  filter(age > 14) # Filter age > 14

# Change file into correct format prior to getting final specification
sas_data_cardiac <-  sas_data_cardiac %>%
  rename(hb = reporting_health_board_name_current,
         sex = gender_description,
         dep = patient_prompt_dataset_deprivation_scot_quintile,
         number_of_cases = number_of_incidents) %>% 
  create_agegroups() %>% # Age Bands
  create_depgroups() %>% 
  mutate(week_ending = dmy(week_ending))
 
# remove diagnosis field as just showing total cardiac
sas_data_cardiac <- sas_data_cardiac %>%
  group_by(week_ending, hb, age_grp, sex, dep) %>%
  summarise(count = sum(number_of_cases, na.rm = T)) %>% 
  ungroup()

sas_data_cardiac <- sas_data_cardiac %>%
  mutate(sex = recode(sex, "MALE" = "Male", "FEMALE" = "Female", "0" = NA_character_, "NOT KNOWN" = NA_character_),
         week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         scot = "Scotland") %>% 
  proper() # convert HB names to correct format

sas_data_cardiac <- sas_data_cardiac %>% 
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
sas_cardiac <- sas_cardiac %>% filter(!area_name %in% c("NHS Orkney", "NHS Shetland"))
sas_cardiac <- sas_cardiac %>% filter(!area_name %in% c("NHS Western Isles") | type !="dep")
sas_cardiac <- sas_cardiac %>% filter(!area_name %in% c("NHS Western Isles") | type !="age")

# Formatting file for shiny app
prepare_final_data_cardiac(dataset = sas_cardiac, filename = "sas_cardiac", last_week = "2020-11-22")

###############################################.
## Prescribing - Cardiovascular Drugs ----
###############################################.
cardio_drugs <- read_xlsx(paste0(data_folder, "prescribing_cardio/2020-11-26-covid emessage AMS only.xlsx")) %>% 
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

# Remove temporary object from environment to reduce session size
rm(cardio_drugs_all)

prepare_final_data(cardio_drugs, "cardio_drugs", last_week = "2020-11-22")

###############################################.
## 6-in-1 data ----
###############################################.

#field with date all immunisation data files prepared
imms_date <- "20201207"

six_alldose <- read_csv(paste0(data_folder,"immunisations/6in1/20201207/six_in_one_dashboard_",imms_date,".csv"), 
                col_types =list(eligible_start=col_date(format="%m/%d/%Y"),
                                time_period_eligible=col_factor())) %>%
janitor::clean_names()

six_alldose <- left_join(six_alldose, hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(eligible_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, exclude, immunisation, eligible_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no)

saveRDS(six_alldose, "shiny_app/data/six_alldose.rds")
saveRDS(six_alldose, paste0(data_folder,"final_app_files/six_alldose_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## immunisations data table dataset prep ----
## immunisation team supply a single csv file that is split into two rds files (one for each immunisation, mmr and 6-in-1)

imms_datatable <- format_immchild_table(paste0("immunisations/dashboardtable_",imms_date))
                           
six_datatable <- imms_datatable %>%
  filter(str_detect(immunisation,"six-in-one")) %>%
  select(-uptake_13m_num:-uptake_3y8m_percent) #remove uptake columns that related to mmr 

saveRDS(six_datatable, paste0("shiny_app/data/","sixinone_datatable.rds"))
saveRDS(six_datatable, paste0(data_folder,"final_app_files/sixinone_datatable_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

mmr_datatable <- imms_datatable %>%
  filter(str_detect(immunisation,"mmr")) %>%
  select(-uptake_12weeks_num:-uptake_32weeks_percent) #remove uptake columns that related to mmr 
saveRDS(mmr_datatable, paste0("shiny_app/data/","mmr_datatable.rds"))
saveRDS(mmr_datatable, paste0(data_folder,"final_app_files/mmr_datatable_", 
                              format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# Grampian data
mmr_dose2_datatable_grampian <- format_immchild_table(paste0("immunisations/mmr/20201207/dashboardtable_grampian_",imms_date)) 
saveRDS(mmr_dose2_datatable_grampian, paste0("shiny_app/data/","mmr_dose2_datatable_grampian.rds"))
saveRDS(mmr_dose2_datatable_grampian, paste0(data_folder,"final_app_files/mmr_dose2_datatable_grampian_", 
                              format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
###############################################.
## 6-in-1 simd data ---- 
six_dose1_simdtable <- format_immsimd_data(paste0("immunisations/6in1/20201207/six-in-one dose 1_simd_",imms_date))
saveRDS(six_dose1_simdtable, paste0("shiny_app/data/","six_dose1_simdtable.rds"))
saveRDS(six_dose1_simdtable, paste0(data_folder,"final_app_files/six_dose1_simdtable_", 
                                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

six_dose2_simdtable <- format_immsimd_data(paste0("immunisations/6in1/20201207/six-in-one dose 2_simd_",imms_date))
saveRDS(six_dose2_simdtable, paste0("shiny_app/data/","six_dose2_simdtable.rds"))
saveRDS(six_dose2_simdtable, paste0(data_folder,"final_app_files/six_dose2_simdtable_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

six_dose3_simdtable <- format_immsimd_data(paste0("immunisations/6in1/20201207/six-in-one dose 3_simd_",imms_date))
saveRDS(six_dose3_simdtable, paste0("shiny_app/data/","six_dose3_simdtable.rds"))
saveRDS(six_dose3_simdtable, paste0(data_folder,"final_app_files/six_dose3_simdtable_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
# Immunisation definitions ----
# apply both for MRR and 6 in one
age_defs_imm_mmr <- read_excel(paste0(data_folder, "immunisations/age definitions.xlsx"),
                               sheet = "mmr_dash") %>% 
  mutate(defined = case_when(is.na(defined) ~ "", T ~ paste0(defined)))

age_defs_imm_mmr <- age_defs_imm_mmr %>% flextable() %>% 
  set_header_labels(defined = "Defined in weeks as:",
                    "...1" = "") %>% 
  merge_at(i =1, j = 1:2, part ="body") %>% 
  merge_at(i =6, j = 1:2, part ="body") %>% 
  merge_at(i =11, j = 1:3, part ="body") %>% 
  align(j = 1) %>% 
  bold(i =1) %>% bold(i =6) %>% bold(i = 11)

age_defs_imm_mmr # checking

saveRDS(age_defs_imm_mmr, "shiny_app/data/age_elig_mmr.rds")
saveRDS(age_defs_imm_mmr, paste0(data_folder,"final_app_files/age_defs_imm_mmr_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# 6 in one age eligibility
age_defs_imm_6inone <- read_excel(paste0(data_folder, "immunisations/age definitions.xlsx"),
                                  sheet = "6inone_dash") 

age_defs_imm_6inone <- age_defs_imm_6inone %>% flextable() %>% 
  set_header_labels("...1" = "") %>% 
  merge_at(i =1, j = 1:2, part ="body") %>% 
  merge_at(i =6, j = 1:2, part ="body") %>% 
  merge_at(i =11, j = 1:2, part ="body") %>% 
  align(j =1) %>% 
  bold(i =1) %>% bold(i =6) %>% bold(i = 11) 
age_defs_imm_6inone #checking

saveRDS(age_defs_imm_6inone, "shiny_app/data/age_elig_6inone.rds")
saveRDS(age_defs_imm_6inone, paste0(data_folder,"final_app_files/age_defs_imm_6inone_", 
                                 format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# month eligibility table
month_defs_imm <- read_excel(paste0(data_folder, "immunisations/month eligible definitions.xlsx"),
                             sheet = "for_dash") %>% 
  mutate("Month eligible" = format(as.Date(`Month eligible`), "%b-%Y")) %>% 
  flextable() %>%
  add_header_row(values = c("Month eligible", "Defined as children reaching relevant age in period:", "", "Number of weeks")) %>% 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_v(j = 1:2, part = "header") %>% 
  merge_v(j = 4, part = "header") %>%
  theme_vanilla

month_defs_imm #checking everything looks ok

saveRDS(month_defs_imm, "shiny_app/data/month_eligibility_immun.rds")
saveRDS(month_defs_imm, paste0(data_folder,"final_app_files/month_eligibility_immun_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## MMR data ----
###############################################.
#field with date all immunisation data files prepared
imms_date <- "20201207"

# mmr dose 1 & 2 - scurve data
mmr_alldose <- read_csv(paste0(data_folder,"immunisations/mmr/20201207/mmr_dashboard_",imms_date,".csv"),
                      col_types =list(eligible_start=col_date(format="%m/%d/%Y"),
                                      time_period_eligible=col_factor())) %>%
  janitor::clean_names()

mmr_alldose <- left_join(mmr_alldose, hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(eligible_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  #rename(week_12_start=week_16_start) %>%
  select (extract_date, exclude, immunisation, eligible_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no)

saveRDS(mmr_alldose, paste0("shiny_app/data/","mmr_alldose.rds"))
saveRDS(mmr_alldose, paste0(data_folder,"final_app_files/mmr_alldose_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## MMR simd data ----
###############################################.
mmr_dose1_simdtable <- format_immsimd_data(paste0("immunisations/mmr/20201207/mmr dose 1_simd_",imms_date))
saveRDS(mmr_dose1_simdtable, paste0("shiny_app/data/","mmr_dose1_simdtable.rds"))
saveRDS(mmr_dose1_simdtable, paste0(data_folder,"final_app_files/mmr_dose1_simdtable_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

mmr_dose2_simdtable <- format_immsimd_data(paste0("immunisations/mmr/20201207/mmr dose 2_simd_",imms_date))
saveRDS(mmr_dose2_simdtable, paste0("shiny_app/data/","mmr_dose2_simdtable.rds"))
saveRDS(mmr_dose2_simdtable, paste0(data_folder,"final_app_files/mmr_dose2_simdtable_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: first visit ----
###############################################.
## First visit - scurve data
first <- read_csv(paste0(data_folder,"child_health/firstvisit_dashboard20201207.csv"), 
                col_types =list(week_2_start=col_date(format="%m/%d/%Y"),
                                time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
first$time_period_eligible <- factor(first$time_period_eligible, 
                                     levels=unique(first$time_period_eligible[order(first$week_2_start, decreasing = T)]), 
                                     ordered=TRUE)

first %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_2_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_2_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) 

saveRDS(first, paste0("shiny_app/data/","first_visit.rds"))
saveRDS(first, paste0(data_folder,"final_app_files/first_visit_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# First visit - summary table data
first_datatable <- format_immchild_table("child_health/firstvisit_dashboardtab_20201207") 

saveRDS(first_datatable, paste0("shiny_app/data/","first_visit_datatable.rds"))
saveRDS(first_datatable, paste0(data_folder,"final_app_files/first_visit_datatable_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: 6-8 weeks  ----
###############################################.

## 6 to 8 weeks visit - scurve data
sixtoeight <- read_csv(paste0(data_folder,"child_health/sixtoeight_dashboard20201207.csv"), 
                  col_types =list(week_6_start=col_date(format="%m/%d/%Y"),
                                  time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
sixtoeight$time_period_eligible <- factor(sixtoeight$time_period_eligible, 
                                          levels=unique(sixtoeight$time_period_eligible[order(sixtoeight$week_6_start, decreasing = T)]), 
                                          ordered=TRUE)

sixtoeight %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_6_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_6_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv<168) 


saveRDS(sixtoeight, paste0("shiny_app/data/","six_to_eight.rds"))
saveRDS(sixtoeight, paste0(data_folder,"final_app_files/six_to_eight_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# 6-8 weeks visit - summary table data
sixtoeight_datatable <- format_immchild_table("child_health/sixtoeight_dashboardtab_20201207") 

saveRDS(sixtoeight_datatable, paste0("shiny_app/data/","six_to_eight_datatable.rds"))
saveRDS(sixtoeight_datatable, paste0(data_folder,"final_app_files/six_to_eight_datatable_", 
                           format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: 13-15 month ----
###############################################.

## 13 to 15 month visit - scurve data
thirteen <- read_csv(paste0(data_folder,"child_health/thirteen_dashboard20201207.csv"), 
                       col_types =list(week_57_start=col_date(format="%m/%d/%Y"),
                                       time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
thirteen$time_period_eligible <- factor(thirteen$time_period_eligible, 
                                        levels=unique(thirteen$time_period_eligible[order(thirteen$week_57_start, decreasing = T)]), 
                                        ordered=TRUE)

thirteen %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_57_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_57_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv>=371 & interv<=518) 

saveRDS(thirteen, paste0("shiny_app/data/","thirteen.rds"))
saveRDS(thirteen, paste0(data_folder,"final_app_files/thirteen_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# 13 to 15 month visit - summary table data
thirteen_datatable <- format_immchild_table("child_health/thirteen_dashboardtab_20201207") 

saveRDS(thirteen_datatable, paste0("shiny_app/data/","thirteen_datatable.rds"))
saveRDS(thirteen_datatable, paste0(data_folder,"final_app_files/thirteen_datatable_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: 27-30 month ----
###############################################.

## 27 to 30 month visit - scurve data
twentyseven <- read_csv(paste0(data_folder,"child_health/twentyseven_dashboard20201207.csv"), 
                     col_types =list(week_117_start=col_date(format="%m/%d/%Y"),
                                     time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
twentyseven$time_period_eligible <- factor(twentyseven$time_period_eligible, 
                                           levels=unique(twentyseven$time_period_eligible[order(twentyseven$week_117_start, decreasing = T)]), 
                                           ordered=TRUE)

twentyseven %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_117_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_117_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv>=791 & interv<=945) 

saveRDS(twentyseven, paste0("shiny_app/data/","twentyseven.rds"))
saveRDS(twentyseven, paste0(data_folder,"final_app_files/twentyseven_", 
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


# 27 to 30 month visit - summary table data
# Data for data download should include complete months and all weeks
twentyseven_datatable <- format_immchild_table("child_health/twentyseven_dashboardtab_20201207") 

saveRDS(twentyseven_datatable, paste0("shiny_app/data/","twentyseven_datatable.rds"))
saveRDS(twentyseven_datatable, paste0(data_folder,"final_app_files/twentyseven_datatable_", 
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: 4-5 year ----
###############################################.

## 4 to 5 year visit - scurve data
fourtofive <- read_csv(paste0(data_folder,"child_health/fourtofive_dashboard20201207.csv"), 
                        col_types =list(week_209_start=col_date(format="%m/%d/%Y"),
                                        time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
fourtofive$time_period_eligible <- factor(fourtofive$time_period_eligible, 
                                          levels=unique(fourtofive$time_period_eligible[order(fourtofive$week_209_start, decreasing = T)]), 
                                          ordered=TRUE)

fourtofive %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_209_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_209_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv>=1428 & interv<=1582) 

saveRDS(fourtofive, paste0("shiny_app/data/","fourtofive.rds"))
saveRDS(fourtofive, paste0(data_folder,"final_app_files/fourtofive_", 
                                     format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
# 4 to 5 year review - summary table data
# Data for data download should include complete months and all weeks
fourtofive_datatable <- format_immchild_table("child_health/fourtofive_dashboardtab_20201207")

saveRDS(fourtofive_datatable, paste0("shiny_app/data/","fourtofive_datatable.rds"))
saveRDS(fourtofive_datatable, paste0(data_folder,"final_app_files/fourtofive_datatable_", 
                                      format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Perinatal mortality ----
###############################################.
# P CHART PERINATAL DATA
p_perinatal <- bind_rows(read_excel(paste0(data_folder,"perinatal/Pchart - SB NND PNND EXTPERI_decupd.xlsx"),
                          sheet = "Stillbirth", skip = 2) %>% mutate(type = "stillbirths"),
                     read_excel(paste0(data_folder,"perinatal/Pchart - SB NND PNND EXTPERI_decupd.xlsx"),
                                sheet = "NND", skip = 2) %>% mutate(type = "nnd"),
                     read_excel(paste0(data_folder,"perinatal/Pchart - SB NND PNND EXTPERI_decupd.xlsx"),
                                sheet = "Extended perinatal", skip = 2) %>% mutate(type = "extperi"),
                     read_excel(paste0(data_folder,"perinatal/Pchart - SB NND PNND EXTPERI_decupd.xlsx"),
                                sheet = "PNND", skip = 2) %>% mutate(type = "pnnd")) %>% 
  janitor::clean_names() %>%
  select(month_of_year=sample_2, number_of_deaths_in_month=observation, sample_size, rate, centreline, stdev = binomial_st_dev_16, 
         upper_cl_3_std_dev:type)

u_perinatal <- read_excel(paste0(data_folder,"perinatal/Uchart - INFANT DEATHS_decupd.xlsx"),
           sheet = "Uchart", skip = 2) %>% mutate(type = "infantdeaths") %>% 
  janitor::clean_names() %>%
  select(month_of_year=sample,  number_of_deaths_in_month=observation, sample_size=ao_o_size, rate, centreline, stdev = poisson_st_dev_16, 
         upper_cl_3_std_dev:type)

# Mergin both datasets together 
perinatal <- rbind(p_perinatal, u_perinatal) %>% 
  mutate(area_name="Scotland", #creating geo variables
         area_type="Scotland",
         month_of_year = gsub(" ", "0", month_of_year), #formatting date
         month_of_year = as.Date(paste0(month_of_year,"1"), format="%Y%m%d")) 

# Creating rules for spc charts
perinatal %<>% 
  arrange(type, area_name, month_of_year) %>% 
  mutate(upper_sigma1 = rate + stdev,
         lower_sigma1 = rate + stdev) %>% 
  group_by(type, area_name) %>% 
  # for rules: outliers when over or under 3 sigma limit
  mutate(outlier = case_when(rate>upper_cl_3_std_dev | rate< lower_cl_3_std_dev ~ T, T ~ F),
         # Shift: run of 8or more consecutive data points above or below the centreline
         # First id when this run is happening and then iding all points part of it
         shift_i = case_when((rate > centreline & lag(rate, 1) > centreline 
                              & lag(rate, 2) > centreline & lag(rate, 3) > centreline 
                              & lag(rate, 4) > centreline & lag(rate, 5) > centreline
                              & lag(rate, 6) > centreline & lag(rate, 7) > centreline) |
                               (rate < centreline & lag(rate, 1) < centreline 
                                & lag(rate, 2) < centreline & lag(rate, 3) < centreline 
                                & lag(rate, 4) < centreline & lag(rate, 5) < centreline
                                & lag(rate, 6) < centreline & lag(rate, 7) < centreline) ~ T , T ~ F),
         shift = case_when(shift_i == T | lead(shift_i, 1) == T | lead(shift_i, 2) == T
                           | lead(shift_i, 3) == T | lead(shift_i, 4) == T
                           | lead(shift_i, 5) == T | lead(shift_i, 6) == T
                           | lead(shift_i, 7) == T ~ T, T ~ F),
         # Trend: A run of 6 or more consecutive data points
         trend_i = case_when((rate > lag(rate ,1) & lag(rate, 1) > lag(rate, 2) 
                              & lag(rate, 2) > lag(rate, 3)  & lag(rate, 3) > lag(rate, 4) 
                              & lag(rate, 4) > lag(rate, 5) ) |
                               (rate < lag(rate ,1) & lag(rate, 1) < lag(rate, 2) 
                                & lag(rate, 2) < lag(rate, 3)  & lag(rate, 3) < lag(rate, 4) 
                                & lag(rate, 4) < lag(rate, 5) ) 
                             ~ T , T ~ F),
         trend = case_when(trend_i == T | lead(trend_i, 1) == T | lead(trend_i, 2) == T
                           | lead(trend_i, 3) == T | lead(trend_i, 4) == T
                           | lead(trend_i, 5) == T  ~ T, T ~ F),
         #Outer One –Third: Two out of three consecutive data points which sit close to one of the control limits(within 2 and 3 sigma)
         outer_i = case_when((rate > upper_wl_2_std_dev & rate < upper_cl_3_std_dev) & 
                               ((lag(rate,1) > upper_wl_2_std_dev & lag(rate,1) < upper_cl_3_std_dev) | 
                                  (lag(rate,2) > upper_wl_2_std_dev & lag(rate,2) < upper_cl_3_std_dev)) ~ T, T ~ F),
         outer = case_when(outer_i == T | lead(outer_i, 1) == T | lead(outer_i, 2) == T ~ T, T ~ F),
         # Inner One -Third: 15 or more consecutive data points that lie close to the centreline(within 1 sigma).
         inner_i = case_when(rate < upper_sigma1 & rate > lower_sigma1 &
                               lag(rate, 1) < upper_sigma1 & lag(rate, 1) > lower_sigma1 &
                               lag(rate, 2) < upper_sigma1 & lag(rate, 2) > lower_sigma1 &
                               lag(rate, 3) < upper_sigma1 & lag(rate, 3) > lower_sigma1 &
                               lag(rate, 4) < upper_sigma1 & lag(rate, 4) > lower_sigma1 &
                               lag(rate, 5) < upper_sigma1 & lag(rate, 5) > lower_sigma1 &
                               lag(rate, 6) < upper_sigma1 & lag(rate, 6) > lower_sigma1 &
                               lag(rate, 7) < upper_sigma1 & lag(rate, 7) > lower_sigma1 &
                               lag(rate, 8) < upper_sigma1 & lag(rate, 8) > lower_sigma1 &
                               lag(rate, 9) < upper_sigma1 & lag(rate, 9) > lower_sigma1 &
                               lag(rate, 10) < upper_sigma1 & lag(rate, 10) > lower_sigma1 &
                               lag(rate, 11) < upper_sigma1 & lag(rate, 11) > lower_sigma1 &
                               lag(rate, 12) < upper_sigma1 & lag(rate, 12) > lower_sigma1 &
                               lag(rate, 13) < upper_sigma1 & lag(rate, 13) > lower_sigma1 &
                               lag(rate, 14) < upper_sigma1 & lag(rate, 14) > lower_sigma1 ~ T, T ~F),
         inner = case_when(inner_i == T | lead(inner_i, 1) == T | lead(inner_i, 2) == T
                           | lead(inner_i, 3) == T | lead(inner_i, 4) == T
                           | lead(inner_i, 5) == T | lead(inner_i, 6) == T
                           | lead(inner_i, 7) == T | lead(inner_i, 8) == T
                           | lead(inner_i, 9) == T | lead(inner_i, 10) == T
                           | lead(inner_i, 11) == T | lead(inner_i, 12) == T
                           | lead(inner_i, 13) == T | lead(inner_i, 14) == T ~T, T ~ F)) %>%
  ungroup %>% 
  select(-shift_i, -trend_i, -outer_i, -inner_i) 

saveRDS(perinatal, "shiny_app/data/perinatal.rds")
saveRDS(perinatal, paste0(data_folder,"final_app_files/perinatal_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Pregnancy (antenatal booking) ----
###############################################.

#field with date all antenatal booking data files prepared
antenatal_booking_date <- "12112020_cut"

# Excel workbook containing number of women booking for antenatal care - weekly file (Scotland and NHS board except small islands)
ante_booking_no <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyNosBooked_Charts_",antenatal_booking_date,".xlsx"),
                              sheet = "Data for Dashboard Charts") %>%
  janitor::clean_names() %>%
  rename(centreline_no=centreline, dottedline_no=dottedline, booked_no=booked) %>%
  mutate(week_book_starting=as.Date(week_book_starting,format="%d-%b-%y")) 

# Excel workbook containing avergage gestation of women booking for antenatal care  - weekly file (Scotland and NHS board except small islands)
ante_booking_gest <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyAveGestation_Charts_",antenatal_booking_date,".xlsx"),
                              sheet = "Data for Dashboard Charts") %>%
  janitor::clean_names() %>%
  rename(centreline_g=centreline, dottedline_g=dottedline, booked_g=booked) %>%
  mutate(week_book_starting=as.Date(week_book_starting,format="%d-%b-%y")) 

# join two (numbers and average gestation) booking sheets to form single file for shiny app
ante_booking <- left_join(ante_booking_no, ante_booking_gest, by = c("week_book_starting","area"))

# Match area names from lookup & format for shinyapp
ante_booking <- left_join(ante_booking, hb_lookup, by = c("area" = "hb_cypher")) %>%
  mutate(type=case_when(area_type=="Health board" ~ "Health board",
                        area=="Scotland" ~ "Scotland",
                        (substr(area,1,4)=="SIMD") ~ "dep", TRUE ~ "age"),
         area_name=case_when(type=="Scotland" ~ "Scotland",
                             type=="age" ~ "Scotland",
                             type=="dep" ~ "Scotland",
                             TRUE ~ area_name),
         area_type=case_when(type=="Health board" ~ "Health board", TRUE ~ area_name), 
         category=case_when(type=="Scotland" ~ "All",
                            type=="Health board" ~ "All",
                            type=="age" ~ area,
                            type=="dep" ~ area, T ~"other"),
         category=case_when(area=="SIMD 1" ~ "1 - most deprived",
                            area=="SIMD 2" ~ "2",
                            area=="SIMD 3" ~ "3",
                            area=="SIMD 4" ~ "4",
                            area=="SIMD 5" ~ "5 - least deprived",
                            type=="age" ~ category, T ~area_name),
         category=case_when(category=="40 plus" ~ "40 and over",TRUE ~ category)) %>%
  select(-area)

#add control chart flags for charting
ante_booking <- ante_booking %>%
  group_by(area_name, area_type, type, category) %>% 
  mutate(# Shift: run of 6 or more consecutive data points above or below the centreline
    # First id when this run is happening and then finding all points part of it
    # SHIFT NUMBER OF WOMEN BOOKING
    shift_i_booked_no = case_when((booked_no > dottedline_no & lag(booked_no, 1) > dottedline_no 
                                   & lag(booked_no, 2) > dottedline_no & lag(booked_no, 3) > dottedline_no 
                                   & lag(booked_no, 4) > dottedline_no & lag(booked_no, 5) > dottedline_no) |
                                    (booked_no < dottedline_no & lag(booked_no, 1) < dottedline_no 
                                     & lag(booked_no, 2) < dottedline_no & lag(booked_no, 3) < dottedline_no 
                                     & lag(booked_no, 4) < dottedline_no & lag(booked_no, 5) < dottedline_no) ~ T , T ~ F),
    shift_booked_no = case_when(shift_i_booked_no == T | lead(shift_i_booked_no, 1) == T | lead(shift_i_booked_no, 2) == T
                                | lead(shift_i_booked_no, 3) == T | lead(shift_i_booked_no, 4) == T
                                | lead(shift_i_booked_no, 5) == T  ~ T, T ~ F),
    # SHIFT FOR AVERAGE GESTATION
    shift_i_booked_gest = case_when((ave_gest > dottedline_g & lag(ave_gest, 1) > dottedline_g 
                                     & lag(ave_gest, 2) > dottedline_g & lag(ave_gest, 3) > dottedline_g 
                                     & lag(ave_gest, 4) > dottedline_g & lag(ave_gest, 5) > dottedline_g) |
                                      (ave_gest < dottedline_g & lag(ave_gest, 1) < dottedline_g 
                                       & lag(ave_gest, 2) < dottedline_g & lag(ave_gest, 3) < dottedline_g 
                                       & lag(ave_gest, 4) < dottedline_g & lag(ave_gest, 5) < dottedline_g) ~ T , T ~ F),
    shift_booked_gest = case_when(shift_i_booked_gest == T | lead(shift_i_booked_gest, 1) == T | lead(shift_i_booked_gest, 2) == T
                                  | lead(shift_i_booked_gest, 3) == T | lead(shift_i_booked_gest, 4) == T
                                  | lead(shift_i_booked_gest, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points - NUMBERS OF WOMEN BOOKING
    trend_i_booked_no = case_when((booked_no > lag(booked_no ,1) & lag(booked_no, 1) > lag(booked_no, 2) 
                                   & lag(booked_no, 2) > lag(booked_no, 3)  & lag(booked_no, 3) > lag(booked_no, 4)) |
                                    (booked_no < lag(booked_no ,1) & lag(booked_no, 1) < lag(booked_no, 2) 
                                     & lag(booked_no, 2) < lag(booked_no, 3)  & lag(booked_no, 3) < lag(booked_no, 4) )  
                                  ~ T , T ~ F),
    trend_booked_no = case_when(trend_i_booked_no == T | lead(trend_i_booked_no, 1) == T | lead(trend_i_booked_no, 2) == T
                                | lead(trend_i_booked_no, 3) == T | lead(trend_i_booked_no, 4) == T
                                ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points - AVERAGE GESTATION
    trend_i_booked_gest = case_when((ave_gest > lag(ave_gest ,1) & lag(ave_gest, 1) > lag(ave_gest, 2) 
                                   & lag(ave_gest, 2) > lag(ave_gest, 3)  & lag(ave_gest, 3) > lag(ave_gest, 4)) |
                                    (ave_gest < lag(ave_gest ,1) & lag(ave_gest, 1) < lag(ave_gest, 2) 
                                     & lag(ave_gest, 2) < lag(ave_gest, 3)  & lag(ave_gest, 3) < lag(ave_gest, 4) )  
                                  ~ T , T ~ F),
    trend_booked_gest = case_when(trend_i_booked_gest == T | lead(trend_i_booked_gest, 1) == T | lead(trend_i_booked_gest, 2) == T
                                | lead(trend_i_booked_gest, 3) == T | lead(trend_i_booked_gest, 4) == T
                                ~ T, T ~ F)) %>%
  select(-shift_i_booked_no, -trend_i_booked_no,-shift_i_booked_gest, -trend_i_booked_gest) %>%
  ungroup()

saveRDS(ante_booking, "shiny_app/data/ante_booking.rds")
saveRDS(ante_booking, paste0(data_folder,"final_app_files/ante_booking_", 
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## ANTENATAL DATA DOWNLOAD FILE FOR SHINY APP
## Data download to include weekly Scotland data for age/deprivation breakdown PLUS monthly booking data for all NHS boards (even the small island boards)

## Monthly booking numbers and average gestation at booking data 
gest_booking_download <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyAveGestation_Charts_",antenatal_booking_date,".xlsx"),
                                    sheet = "Monthly Data for Download") %>%
  janitor::clean_names()

# Match area names from lookup & format for shinyapp
gest_booking_download <- left_join(gest_booking_download, hb_lookup, by = c("area" = "hb_cypher")) %>%
  mutate(area_name=case_when(area=="Scotland" ~ "Scotland", T~ area_name),
         area_type=case_when(area=="Scotland" ~ "Scotland", T~ area_type),
         time_period="monthly") %>%
  select(-area) %>%
  rename(booking_month=month_booking, number_of_bookings=booked, average_gestation_at_booking=ave_gest) %>%
  arrange(area_type, booking_month)

# Weekly scotland level booking numbers and gestation
ante_booking_download1 <- ante_booking %>%
  mutate(time_period="weekly") %>%
  rename(booking_week_beginning=week_book_starting, number_of_bookings=booked_g, average_gestation_at_booking=ave_gest)

# Add weekly and month files into one file
ante_booking_download <- bind_rows(ante_booking_download1, gest_booking_download) %>%
  rename(chart_type=type,chart_category=category,
         number_of_women_booking=number_of_bookings,
         centreline_number=centreline_no,
         dottedline_number=dottedline_no,
         number_of_women_booking_gest_under_10wks=g_u10wks,
         number_of_women_booking_gest_10to12wks=g_10to12wks,
         number_of_women_booking_gest_over_12wks=g_13pluswks,
         centreline_gestation=centreline_g,
         dottedline_gestation=dottedline_g) %>%
  select(time_period, booking_week_beginning, booking_month, area_name, area_type, chart_type, chart_category,
         number_of_women_booking, centreline_number, dottedline_number,
         number_of_women_booking_gest_under_10wks,number_of_women_booking_gest_10to12wks,number_of_women_booking_gest_over_12wks,
         average_gestation_at_booking, centreline_gestation, dottedline_gestation)

saveRDS(ante_booking_download, "shiny_app/data/ante_booking_download.rds")
saveRDS(ante_booking_download, paste0(data_folder,"final_app_files/ante_booking_download_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Pregnancy (terminations) ----
###############################################.
#field with date all antenatal booking data files prepared
top_date <- "2020-11-05"

## Termination data for run chart (scotland and nhs board) - monthly
top_runchart <- readRDS(paste0(data_folder, "pregnancy/terminations/",top_date,
                               "/WI_TERMINATIONS_RUNCHARTS_",top_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date,
         centreline_no = av_pre_pan_terminations,
         dottedline_no = ext_av_count,
         centreline_g = pre_pan_av_gest,
         dottedline_g = ext_av_gest) %>%
  mutate(terminations=as.numeric(terminations),
         month=as.Date(month),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type=case_when(type=="Health board" ~ "Health board", TRUE ~ area_name), 
         category=case_when(type=="Scotland" ~ "All",
                            type=="Health board" ~ "All"))

## Termination data for scotland only by age and dep
top_scot <- readRDS(paste0(data_folder, "pregnancy/terminations/",top_date,
                           "/WI_TERMINATIONS_SCOTLAND_CHARTS_",top_date,".rds")) %>%  
  janitor::clean_names() %>%
  ungroup() %>% # for some reason dataset appears to be grouped which prevents formatting 
  rename(area_name=hbres, month=date, category=variable) %>%
  mutate(month=as.Date(month),
         type=case_when(chart=="AGEGRP" ~ "age",chart=="SIMD" ~ "dep",TRUE ~ "other"),
         area_type="Scotland",
         category=as.character(case_when(category=="40+" ~ "40 and over", 
                                         category=="under 20" ~ "Under 20", 
                                         TRUE ~ as.character(category))))
         
## Combine area based and age/dep terminations data, format and add shifts/trends
top <- bind_rows(top_runchart, top_scot) %>%
  select(-chart) %>%
  #dotted line used to assess shifts or trends therefore need to fill cells which are set to NA 
  mutate(dottedline_no= case_when(is.na(dottedline_no)~centreline_no,TRUE ~ dottedline_no),
         dottedline_g= case_when(is.na(dottedline_g)~centreline_g,TRUE ~ dottedline_g)) %>% #recode age group as required
  #sort data to ensure trends/shifts compare correct data points
  group_by(area_name, area_type, type) %>%
  mutate(# Shift: run of 6 or more consecutive data points above or below the centreline
    # First id when this run is happening and then finding all points part of it
    # SHIFT NUMBER OF terminations
    shift_i_top_no = case_when((terminations > dottedline_no & lag(terminations, 1) > dottedline_no 
                                & lag(terminations, 2) > dottedline_no & lag(terminations, 3) > dottedline_no 
                                & lag(terminations, 4) > dottedline_no & lag(terminations, 5) > dottedline_no) |
                                 (terminations < dottedline_no & lag(terminations, 1) < dottedline_no 
                                  & lag(terminations, 2) < dottedline_no & lag(terminations, 3) < dottedline_no 
                                  & lag(terminations, 4) < dottedline_no & lag(terminations, 5) < dottedline_no) ~ T , T ~ F),
    shift_top_no = case_when(shift_i_top_no == T | lead(shift_i_top_no, 1) == T | lead(shift_i_top_no, 2) == T
                             | lead(shift_i_top_no, 3) == T | lead(shift_i_top_no, 4) == T
                             | lead(shift_i_top_no, 5) == T  ~ T, T ~ F),
    # SHIFT FOR AVERAGE GESTATION
    shift_i_top_gest = case_when((av_gest > dottedline_g & lag(av_gest, 1) > dottedline_g 
                                     & lag(av_gest, 2) > dottedline_g & lag(av_gest, 3) > dottedline_g 
                                     & lag(av_gest, 4) > dottedline_g & lag(av_gest, 5) > dottedline_g) |
                                      (av_gest < dottedline_g & lag(av_gest, 1) < dottedline_g 
                                       & lag(av_gest, 2) < dottedline_g & lag(av_gest, 3) < dottedline_g 
                                       & lag(av_gest, 4) < dottedline_g & lag(av_gest, 5) < dottedline_g) ~ T , T ~ F),
    shift_top_gest = case_when(shift_i_top_gest == T | lead(shift_i_top_gest, 1) == T | lead(shift_i_top_gest, 2) == T
                                  | lead(shift_i_top_gest, 3) == T | lead(shift_i_top_gest, 4) == T
                                  | lead(shift_i_top_gest, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points - NUMBERS OF TOP
    trend_i_top_no = case_when((terminations > lag(terminations ,1) & lag(terminations, 1) > lag(terminations, 2) 
                                   & lag(terminations, 2) > lag(terminations, 3)  & lag(terminations, 3) > lag(terminations, 4)) |
                                    (terminations < lag(terminations ,1) & lag(terminations, 1) < lag(terminations, 2) 
                                     & lag(terminations, 2) < lag(terminations, 3)  & lag(terminations, 3) < lag(terminations, 4) )  
                                  ~ T , T ~ F),
    trend_top_no = case_when(trend_i_top_no == T | lead(trend_i_top_no, 1) == T | lead(trend_i_top_no, 2) == T
                                | lead(trend_i_top_no, 3) == T | lead(trend_i_top_no, 4) == T
                                ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points - AVERAGE GESTATION TOP
    trend_i_top_gest = case_when((av_gest > lag(av_gest ,1) & lag(av_gest, 1) > lag(av_gest, 2) 
                                     & lag(av_gest, 2) > lag(av_gest, 3)  & lag(av_gest, 3) > lag(av_gest, 4)) |
                                      (av_gest < lag(av_gest ,1) & lag(av_gest, 1) < lag(av_gest, 2) 
                                       & lag(av_gest, 2) < lag(av_gest, 3)  & lag(av_gest, 3) < lag(av_gest, 4) )  
                                    ~ T , T ~ F),
    trend_top_gest = case_when(trend_i_top_gest == T | lead(trend_i_top_gest, 1) == T | lead(trend_i_top_gest, 2) == T
                                  | lead(trend_i_top_gest, 3) == T | lead(trend_i_top_gest, 4) == T
                                  ~ T, T ~ F)) %>%
  select(-shift_i_top_no, -trend_i_top_no,-shift_i_top_gest, -trend_i_top_gest) %>%
  ungroup()

saveRDS(top, "shiny_app/data/top.rds")
saveRDS(top, paste0(data_folder,"final_app_files/top_", 
                                     format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## TERMINATIONS DATA DOWNLOAD FILE FOR SHINY APP
## Data download to include monthly Scotland data for age/deprivation breakdown PLUS monthly data for NHS boards (excluding the small island boards)

top_download_board <- read_csv(paste0(data_folder, "pregnancy/terminations/",top_date,
                                      "/WI_TERMINATIONS_DOWNLOAD_",top_date,".csv"))%>%  
  janitor::clean_names() %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"),
         termination_month=format(date,"%b %Y")) %>%
  rename(area_name=hbres, 
         number_of_terminations=terminations,
         centreline_number=av_pre_pan_terminations,
         dottedline_number=ext_av_count,
         number_of_terminations_gest_under_10wks=x9_weeks,
         number_of_terminations_gest_10to12wks=x10_12_weeks,
         number_of_terminations_gest_over_12wks=x13_weeks,
         average_gestation_at_termination = av_gest,
         centreline_gestation = pre_pan_av_gest,
         dottedline_gestation = ext_av_gest) %>%
  mutate(average_gestation_at_termination =format(average_gestation_at_termination,digits = 1, nsmall = 1),
         centreline_gestation =format(centreline_gestation,digits = 1, nsmall = 1),
         dottedline_gestation =format(dottedline_gestation,digits = 1, nsmall = 1),
         area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         chart_category=case_when(area_type=="Scotland" ~ "All",
                                  area_type=="Health board" ~ "All"),
         chart_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                              area_name=="Scotland" ~ "Scotland", TRUE ~ "Other")) %>%
  select(termination_month, area_name, area_type, chart_type, chart_category, 
         number_of_terminations, centreline_number, dottedline_number,
         number_of_terminations_gest_under_10wks,
         number_of_terminations_gest_10to12wks,
         number_of_terminations_gest_over_12wks,
         average_gestation_at_termination, centreline_gestation, dottedline_gestation,date) %>%
  arrange(area_name, area_type,chart_type, date) %>% 
  select(-date)

top_download_scot <- top_scot %>%
  mutate(month=as.Date(month,format="%Y-%m-%d"),
         termination_month=format(month,"%b %Y"),
         av_gest =format(av_gest,digits = 1, nsmall = 1)) %>%
  rename(number_of_terminations=terminations,
         average_gestation_at_termination = av_gest,
         chart_category=category,
         chart_type=type) %>%
  select(-chart, -month)

top_download <- bind_rows(top_download_board, top_download_scot)

saveRDS(top_download, "shiny_app/data/top_download.rds")
saveRDS(top_download, paste0(data_folder,"final_app_files/top_download_", 
                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Pregnancy (mode of delivery) ----
###############################################.
#field with date data files prepared
mod_folder <- "20201126"
mod_date <- "2020-11-26"

##mode of delivery data supplied in 4 files: runchart data, line charts for scotland (age and dep split), line charts for NHS board and data download

## 1-RUNCHART DATA
## mod data for run chart (scotland and nhs board) - monthly
mod_runchart <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",mod_folder,"/WI_DELIVERIES_RUNCHART_mode_",mod_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name = hbres, month = date) %>%
  mutate(month = as.Date(month),
         type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland"),
         area_type = type, 
         category = "All") %>%
# the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
# ext_ columns are extended median which are blank before projection time period
mutate(ext_csection_all = case_when(is.na(ext_csection_all) ~ median_csection_all,
                                   TRUE ~ ext_csection_all),
       ext_csection_elec = case_when(is.na(ext_csection_elec) ~ median_csection_elec,
                                    TRUE ~ ext_csection_elec),
       ext_csection_emer = case_when(is.na(ext_csection_emer) ~ median_csection_emer,
                                    TRUE ~ ext_csection_emer)) %>% 
  group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
  #call function to add flags for runchart shifts and trends
  #shift: name for new field where shift is flagged
  #trend: name for new field where trend is flagged
  #value: which column in dataset contains value being evaluated
  #median: which column in dataset contains the median against which value is tested
  runchart_flags(shift="csection_all_shift", trend="csection_all_trend", 
                 value=perc_csection_all, median=ext_csection_all) %>%
  runchart_flags(shift="csection_emer_shift", trend="csection_emer_trend", 
                 value=perc_csection_emer, median=ext_csection_emer) %>%
  runchart_flags(shift="csection_elec_shift", trend="csection_elec_trend", 
                 value=perc_csection_elec, median=ext_csection_elec) %>%
  ungroup()

saveRDS(mod_runchart, "shiny_app/data/mod_runchart_data.rds")
saveRDS(mod_runchart, paste0(data_folder,"final_app_files/mod_runchart_data_", 
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 2- LINECHART DATA mode of delivery for Scotland only by age and dep
mod_scot <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",mod_folder,"/WI_DELIVERIES_SCOT_CHARTS_mode_",mod_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date, category=variable) %>%
  mutate(month=as.Date(month),
         area_type="Scotland",
         type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
         category=as.character(category))

saveRDS(mod_scot, "shiny_app/data/mod_scot_data.rds")
saveRDS(mod_scot, paste0(data_folder,"final_app_files/mod_scot_data_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 3- LINECHART DATA mode of delivery for Scotland & NHS board
mod_linechart <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",mod_folder,"/WI_DELIVERIES_LINECHART_mode_",mod_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date) %>%
  mutate(month=as.Date(month, format="%Y-%m-%d "),
         #month=format(month,"%b %Y"),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type = type, 
         category="All") %>%
  group_by(area_name, month) %>% 
  mutate(tot_births=sum(births/2), # divide by two because total births already a row in the dataset
         percent_births=(births/tot_births)*100) %>% 
  ungroup()

mod_linechart <- mod_linechart %>%
  mutate(mode = recode(mode, "Spontaneous" = "Spontaneous vaginal", "Assisted" = "Assisted vaginal", "Caesarean - Emergency" = "Emergency caesarean",
                       "Caesarean - Elective" = "Elective caesarean"))

saveRDS(mod_linechart, "shiny_app/data/mod_linechart_data.rds") 
saveRDS(mod_linechart, paste0(data_folder,"final_app_files/mod_linechart_data_", 
                         format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


## 4- Mode of delivery DATA DOWNLOAD FILE FOR SHINY APP
mod_download <- read_csv(paste0(data_folder, "pregnancy/mode_of_delivery/",mod_folder,"/WI_DELIVERIES_DOWNLOAD_mode_",mod_date,".csv"))%>%  
  janitor::clean_names() %>%
  mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
         month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
  rename(area_name=nhs_board_of_residence,
         centreline_csection_all = median_csection_all,
         centreline_csection_emer = median_csection_emer,
         centreline_csection_elec = median_csection_elec,
         dottedline_csection_all = ext_csection_all,
         dottedline_csection_emer = ext_csection_emer,
         dottedline_csection_elec = ext_csection_elec) %>% 
  mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland"),
         chart_category="All",
         chart_type= area_type)

saveRDS(mod_download, "shiny_app/data/mod_download_data.rds")  
saveRDS(mod_download, paste0(data_folder,"final_app_files/mod_download_data_", 
                         format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Pregnancy (inductions) ----
###############################################.
induct_folder <- "20201126"
induct_date <- "2020-11-26"

## 1-RUNCHART DATA
## mod data for run chart (scotland and nhs board) - monthly
induct_runchart <- readRDS(paste0(data_folder, "pregnancy/inductions/",induct_folder,"/WI_DELIVERIES_RUNCHART_induced_",induct_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name = hbres, month = date) %>%
  mutate(month = as.Date(month),
         type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland"),
         area_type = type, 
         category = "All") %>%
  # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
  # ext_ columns are extended median which are blank before projection time period
  mutate(ext_ind_37_42 = case_when(is.na(ext_ind_37_42) ~ median_ind_37_42,
                                      TRUE ~ ext_ind_37_42)) %>%
  group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
  #call function to add flags for runchart shifts and trends
  #shift: name for new field where shift is flagged
  #trend: name for new field where trend is flagged
  #value: which column in dataset contains value being evaluated
  #median: which column in dataset contains the median against which value is tested
  runchart_flags(shift="induction_shift", trend="induction_trend", 
                 value=perc_ind_37_42, median=ext_ind_37_42) %>%
  ungroup()

saveRDS(induct_runchart, "shiny_app/data/induct_runchart_data.rds")
saveRDS(induct_runchart, paste0(data_folder,"final_app_files/induct_runchart_data_", 
                         format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 2- LINECHART DATA inductions for Scotland only by age and dep
induct_scot <- readRDS(paste0(data_folder, "pregnancy/inductions/",induct_folder,"/WI_DELIVERIES_SCOT_CHARTS_induced_",induct_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date, category=variable) %>%
  mutate(month=as.Date(month),
         area_type="Scotland",
         type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
         category=as.character(category))

saveRDS(induct_scot, "shiny_app/data/induct_scot_data.rds")
saveRDS(induct_scot, paste0(data_folder,"final_app_files/induct_scot_data_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 3- LINECHART DATA inductions for Scotland & NHS board
induct_linechart <- readRDS(paste0(data_folder, "pregnancy/inductions/",induct_folder,"/WI_DELIVERIES_LINECHART_induced_",induct_date,".rds")) %>%  
  janitor::clean_names() %>%
  mutate(tot_births_37_42=births_37_42) %>%
  #reshape data file for ease of creation of line chart with percentages
  pivot_longer(cols = ind_37_42:births_37_42, names_to = "ind",values_to = "births") %>%
  rename(area_name=hbres, month=date) %>%
  mutate(month=as.Date(month, format="%Y-%m-%d "),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type = type, 
         category="All",
         percent_births=((births/tot_births_37_42)*100),
         #NOTE the gestation categories are not mutually exclusive - <37 contains <32
         ind=case_when(ind=="ind_37_42" ~ "Births that followed induction",
                       ind=="births_37_42" ~ "All births",
                       TRUE~as.character(ind))) 

saveRDS(induct_linechart, "shiny_app/data/induct_linechart_data.rds") 
saveRDS(induct_linechart, paste0(data_folder,"final_app_files/induct_linechart_data_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 4- Mode of delivery DATA DOWNLOAD FILE FOR SHINY APP
induct_download <- read_csv(paste0(data_folder, "pregnancy/inductions/",induct_folder,"/WI_DELIVERIES_DOWNLOAD_induced_",induct_date,".csv"))%>%  
  janitor::clean_names() %>%
  mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
         month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
  rename(area_name=nhs_board_of_residence,
         centreline_induced_37_42 = median_induced_37_42,
         dottedline_induced_37_42 = ext_induced_37_42) %>% 
  mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland"),
         chart_category="All",
         chart_type= area_type) 

saveRDS(induct_download, "shiny_app/data/induct_download_data.rds")  
saveRDS(induct_download, paste0(data_folder,"final_app_files/induct_download_data_", 
                                 format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Pregnancy (gestation at delivery) ----
###############################################.

gestation_folder <- "20201126"
gestation_date <- "2020-11-26"

## 1-RUNCHART DATA
gestation_runchart <- readRDS(paste0(data_folder,"pregnancy/gestation_at_delivery/",gestation_folder,"/WI_DELIVERIES_RUNCHART_gestation_",gestation_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name = hbres, month = date) %>%
  mutate(month = as.Date(month),
         type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland"),
         area_type = type, 
         category = "All") %>%
  # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
  # ext_ columns are extended median which are blank before projection time period
  mutate(ext_under32 = case_when(is.na(ext_under32) ~ median_under32,
                                   TRUE ~ ext_under32),
         ext_under37 = case_when(is.na(ext_under37) ~ median_under37,
                                 TRUE ~ ext_under37),
         ext_32_36 = case_when(is.na(ext_32_36) ~ median_32_36,
                                 TRUE ~ ext_32_36),
         ext_42plus = case_when(is.na(ext_42plus) ~ median_42plus,
                                 TRUE ~ ext_42plus)) %>%
  group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
  #call function to add flags for runchart shifts and trends
  #shift: name for new field where shift is flagged
  #trend: name for new field where trend is flagged
  #value: which column in dataset contains value being evaluated
  #median: which column in dataset contains the median against which value is tested
  runchart_flags(shift="gest_under32_shift", trend="gest_under32_trend", 
                 value=perc_under32, median=ext_under32) %>%
  runchart_flags(shift="gest_under37_shift", trend="gest_under37_trend", 
                 value=perc_under37, median=ext_under37) %>%
  runchart_flags(shift="gest_32_36_shift", trend="gest_32_36_trend", 
                 value=perc_32_36, median=ext_32_36) %>%
  runchart_flags(shift="gest_42plus_shift", trend="gest_42plus_trend", 
                 value=perc_42plus, median=ext_42plus) %>%
  ungroup()

saveRDS(gestation_runchart, "shiny_app/data/gestation_runchart_data.rds")
saveRDS(gestation_runchart, paste0(data_folder,"final_app_files/gestation_runchart_data_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 2- LINECHART DATA inductions for Scotland only by age and dep
gestation_scot <- readRDS(paste0(data_folder, "pregnancy/gestation_at_delivery/",gestation_folder,"/WI_DELIVERIES_SCOT_CHARTS_gestation_",gestation_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date, category=variable) %>%
  mutate(month=as.Date(month),
         area_type="Scotland",
         type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
         category=as.character(category))

saveRDS(gestation_scot, "shiny_app/data/gestation_scot_data.rds")
saveRDS(gestation_scot, paste0(data_folder,"final_app_files/gestation_scot_data_", 
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 3- LINECHART DATA gestation for Scotland & NHS board
gestation_linechart <- readRDS(paste0(data_folder, "pregnancy/gestation_at_delivery/",gestation_folder,"/WI_DELIVERIES_LINECHART_gestation_",gestation_date,".rds")) %>%  
  janitor::clean_names() %>%
  mutate(tot_births_18_44=births_18_44) %>%
  #reshape data file for ease of creation of line chart with percentages
  pivot_longer(cols = births_under32:births_18_44, names_to = "gest",values_to = "births") %>%
  rename(area_name=hbres, month=date) %>%
  mutate(month=as.Date(month, format="%Y-%m-%d "),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type = type, 
         category="All",
         percent_births=format(((births/tot_births_18_44)*100),digits=1, nsmall=1),
         #NOTE the gestation categories are not mutually exclusive - <37 contains <32
         gest=case_when(gest=="births_under32" ~ "Under 32 weeks",
                        gest=="births_under37" ~ "Under 37 weeks",
                        gest=="births_32_36" ~ "32 to 36 weeks",
                        gest=="births_37_41" ~ "37 to 41 weeks",
                        gest=="births_18_44" ~ "All gestations (18-44 weeks)",
                        gest=="births_42plus" ~ "42 weeks plus",
                        TRUE~as.character(gest))) 

saveRDS(gestation_linechart, "shiny_app/data/gestation_linechart_data.rds")  
saveRDS(gestation_linechart, paste0(data_folder,"final_app_files/gestation_linechart_data_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 4- DATA DOWNLOAD FILE FOR SHINY APP
gestation_download <- read_csv(paste0(data_folder, "pregnancy/gestation_at_delivery/",gestation_folder,"/WI_DELIVERIES_DOWNLOAD_gestation_",gestation_date,".csv"))%>%  
  janitor::clean_names() %>%
  mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
         month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
  rename(area_name=nhs_board_of_residence,
         centreline_under32 = median_under32,
         centreline_32_36 = median_32_36,
         centreline_under37 = median_under37,
         centreline_42plus = median_42plus,
         dottedline_under32 = ext_under32,
         dottedline_32_36 = ext_32_36,
         dottedline_under37 = ext_under37,
         dottedline_42plus = ext_42plus) %>% 
  mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland"),
         chart_category="All",
         chart_type= area_type) 

saveRDS(gestation_download, "shiny_app/data/gestation_download_data.rds") 
saveRDS(gestation_download, paste0(data_folder,"final_app_files/gestation_download_data_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


###############################################.
## Child development ----
###############################################.
# Do we need any sort of supression - look at island values.
child_dev <- rbind(read_excel(paste0(data_folder, "child_development/7thDecDashboard - 13-15m.xlsx")) %>% 
                     mutate(review = "13-15 month"),
                   read_excel(paste0(data_folder, "child_development/7thDecDashboard - 27-30m.xlsx")) %>% 
                     mutate(review = "27-30 month")) %>% 
  clean_names() %>% 
  rename(area_name = geography) %>% 
  mutate(area_type = case_when(area_name == "Scotland" ~ "Scotland",
                               stringr::str_sub(area_name, start = -4) == "HSCP" ~ "HSCP",
                               T ~ "Health board"),
         area_name = case_when(area_type=="Health board" ~ paste0("NHS ", area_name),  
                               TRUE ~ area_name),
         month_review = as.Date(month_review)) %>% 
  filter((year(month_review) %in% c("2019", "2020"))) 

child_dev %<>% # Dealing with NAs, which are 0s
  mutate_at(c("pc_1_plus", "concerns_1_plus"), ~replace_na(., 0)) %>% 
  #Glasgow is incomplete before May19, converting to NA
  mutate(no_reviews = case_when(area_name == "NHS Greater Glasgow & Clyde" & 
                                  review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ no_reviews),
         no_meaningful_reviews = case_when(area_name == "NHS Greater Glasgow & Clyde" & 
                                             review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ no_meaningful_reviews),
         concerns_1_plus = case_when(area_name == "NHS Greater Glasgow & Clyde" & 
                                       review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ concerns_1_plus),
         pc_1_plus = case_when(area_name == "NHS Greater Glasgow & Clyde" & 
                                 review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ pc_1_plus),
         pc_meaningful = case_when(area_name == "NHS Greater Glasgow & Clyde" & review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ pc_meaningful))



# Calculating centre lines and adding them to child_dev
child_dev_centreline_hb <- child_dev %>% 
  filter(month_review< as.Date("2020-03-01") & month_review>= as.Date("2019-01-01")) %>% 
  filter(!(area_name %in% c("Scotland", "NHS Greater Glasgow & Clyde") & review == "13-15 month")) %>% 
  select(area_name, review, pc_1_plus) %>% group_by(area_name, review) %>% 
  mutate(pc_1_plus_centreline = median(pc_1_plus)) %>% ungroup() %>% 
  select(-pc_1_plus) %>% unique

child_dev_centreline_scot <- child_dev %>% 
  filter(month_review< as.Date("2020-03-01") & month_review>= as.Date("2019-05-01")) %>% 
  filter(area_name %in% c("Scotland", "NHS Greater Glasgow & Clyde") & review == "13-15 month") %>% 
  select(area_name, review, pc_1_plus) %>% group_by(area_name, review) %>% 
  mutate(pc_1_plus_centreline = median(pc_1_plus)) %>% ungroup() %>% 
  select(-pc_1_plus) %>% unique

child_dev_centreline <- rbind(child_dev_centreline_hb, child_dev_centreline_scot)

child_dev %<>% left_join(child_dev_centreline) 

child_dev %<>% 
  group_by(area_name, area_type, review) %>% 
  mutate(# Shift: run of 6 or more consecutive data points above or below the pc_1_plus_centreline
         # First id when this run is happening and then iding all points part of it
         shift_i = case_when((pc_1_plus > pc_1_plus_centreline & lag(pc_1_plus, 1) > pc_1_plus_centreline 
                              & lag(pc_1_plus, 2) > pc_1_plus_centreline & lag(pc_1_plus, 3) > pc_1_plus_centreline 
                              & lag(pc_1_plus, 4) > pc_1_plus_centreline & lag(pc_1_plus, 5) > pc_1_plus_centreline) |
                               (pc_1_plus < pc_1_plus_centreline & lag(pc_1_plus, 1) < pc_1_plus_centreline 
                                & lag(pc_1_plus, 2) < pc_1_plus_centreline & lag(pc_1_plus, 3) < pc_1_plus_centreline 
                                & lag(pc_1_plus, 4) < pc_1_plus_centreline & lag(pc_1_plus, 5) < pc_1_plus_centreline) ~ T , T ~ F),
         shift = case_when(shift_i == T | lead(shift_i, 1) == T | lead(shift_i, 2) == T
                           | lead(shift_i, 3) == T | lead(shift_i, 4) == T
                           | lead(shift_i, 5) == T  ~ T, T ~ F),
         # Trend: A run of 5 or more consecutive data points
         trend_i = case_when((pc_1_plus > lag(pc_1_plus ,1) & lag(pc_1_plus, 1) > lag(pc_1_plus, 2) 
                              & lag(pc_1_plus, 2) > lag(pc_1_plus, 3)  & lag(pc_1_plus, 3) > lag(pc_1_plus, 4)) |
                               (pc_1_plus < lag(pc_1_plus ,1) & lag(pc_1_plus, 1) < lag(pc_1_plus, 2) 
                                & lag(pc_1_plus, 2) < lag(pc_1_plus, 3)  & lag(pc_1_plus, 3) < lag(pc_1_plus, 4) )  
                             ~ T , T ~ F),
         trend = case_when(trend_i == T | lead(trend_i, 1) == T | lead(trend_i, 2) == T
                           | lead(trend_i, 3) == T | lead(trend_i, 4) == T
                             ~ T, T ~ F)) %>% 
  select(-shift_i, -trend_i) %>% ungroup()

remove(child_dev_centreline, child_dev_centreline_scot, child_dev_centreline_hb)

saveRDS(child_dev, "shiny_app/data/child_dev.rds")
saveRDS(child_dev, paste0(data_folder,"final_app_files/child_dev_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Breastfeeding ----
###############################################.
breastfeeding <- bind_rows(read_xlsx(paste0(data_folder, "/breastfeeding/7thDecDashboard - firstvisit.xlsx")) %>% 
                         mutate(review = "First visit"),
                       read_xlsx(paste0(data_folder, "/breastfeeding/7thDecDashboard - 6-8 week.xlsx")) %>% 
                         mutate(review = "6-8 week")) %>% 
  clean_names() %>% 
  rename(area_name = geography) %>% 
  mutate(area_type = case_when(area_name == "Scotland" ~ "Scotland",
                               stringr::str_sub(area_name, start = -4) == "HSCP" ~ "HSCP",
                               T ~ "Health board"),
         area_name = case_when(area_type=="Health board" ~ paste0("NHS ", area_name),  
                               TRUE ~ area_name),
         month_review = as.Date(month_review)) %>% 
  filter((year(month_review) %in% c("2019", "2020")))

# Calculating centre lines and adding them to breastfeeding
breastfeeding_centreline <- breastfeeding %>% 
  filter(month_review< as.Date("2020-03-01") & month_review>= as.Date("2019-01-01")) %>% 
  select(area_name, review, pc_valid, pc_excl, pc_overall, pc_ever) %>% group_by(area_name, review) %>% 
  mutate(pc_valid_centreline = median(pc_valid),
         pc_excl_centreline = median(pc_excl),
         pc_overall_centreline = median(pc_overall),
         pc_ever_centreline = median(pc_ever)) %>% ungroup() %>% 
  select(-c(pc_valid, pc_excl, pc_overall, pc_ever)) %>% unique

breastfeeding <- breastfeeding %>% left_join(breastfeeding_centreline)

breastfeeding %<>% 
  group_by(area_name, area_type, review) %>% 
  mutate(# Shift: run of 6 or more consecutive data points above or below the pc_1_plus_centreline
    # First id when this run is happening and then iding all points part of it
    shift_i_excl = case_when((pc_excl > pc_excl_centreline & lag(pc_excl, 1) > pc_excl_centreline 
                         & lag(pc_excl, 2) > pc_excl_centreline & lag(pc_excl, 3) > pc_excl_centreline 
                         & lag(pc_excl, 4) > pc_excl_centreline & lag(pc_excl, 5) > pc_excl_centreline) |
                          (pc_excl < pc_excl_centreline & lag(pc_excl, 1) < pc_excl_centreline 
                           & lag(pc_excl, 2) < pc_excl_centreline & lag(pc_excl, 3) < pc_excl_centreline 
                           & lag(pc_excl, 4) < pc_excl_centreline & lag(pc_excl, 5) < pc_excl_centreline) ~ T , T ~ F),
    shift_excl = case_when(shift_i_excl == T | lead(shift_i_excl, 1) == T | lead(shift_i_excl, 2) == T
                      | lead(shift_i_excl, 3) == T | lead(shift_i_excl, 4) == T
                      | lead(shift_i_excl, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points
    trend_i_excl = case_when((pc_excl > lag(pc_excl ,1) & lag(pc_excl, 1) > lag(pc_excl, 2) 
                         & lag(pc_excl, 2) > lag(pc_excl, 3)  & lag(pc_excl, 3) > lag(pc_excl, 4)) |
                          (pc_excl < lag(pc_excl ,1) & lag(pc_excl, 1) < lag(pc_excl, 2) 
                           & lag(pc_excl, 2) < lag(pc_excl, 3)  & lag(pc_excl, 3) < lag(pc_excl, 4) )  
                        ~ T , T ~ F),
    trend_excl = case_when(trend_i_excl == T | lead(trend_i_excl, 1) == T | lead(trend_i_excl, 2) == T
                      | lead(trend_i_excl, 3) == T | lead(trend_i_excl, 4) == T
                      ~ T, T ~ F)) %>% 
  mutate(# Shift: run of 6 or more consecutive data points above or below the pc_overall_centreline
    # First id when this run is happening and then iding all points part of it
    shift_i_over = case_when((pc_overall > pc_overall_centreline & lag(pc_overall, 1) > pc_overall_centreline 
                              & lag(pc_overall, 2) > pc_overall_centreline & lag(pc_overall, 3) > pc_overall_centreline 
                              & lag(pc_overall, 4) > pc_overall_centreline & lag(pc_overall, 5) > pc_overall_centreline) |
                               (pc_overall < pc_overall_centreline & lag(pc_overall, 1) < pc_overall_centreline 
                                & lag(pc_overall, 2) < pc_overall_centreline & lag(pc_overall, 3) < pc_overall_centreline 
                                & lag(pc_overall, 4) < pc_overall_centreline & lag(pc_overall, 5) < pc_overall_centreline) ~ T , T ~ F),
    shift_over = case_when(shift_i_over == T | lead(shift_i_over, 1) == T | lead(shift_i_over, 2) == T
                           | lead(shift_i_over, 3) == T | lead(shift_i_over, 4) == T
                           | lead(shift_i_over, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points
    trend_i_over = case_when((pc_overall > lag(pc_overall ,1) & lag(pc_overall, 1) > lag(pc_overall, 2) 
                              & lag(pc_overall, 2) > lag(pc_overall, 3)  & lag(pc_overall, 3) > lag(pc_overall, 4)) |
                               (pc_overall < lag(pc_overall ,1) & lag(pc_overall, 1) < lag(pc_overall, 2) 
                                & lag(pc_overall, 2) < lag(pc_overall, 3)  & lag(pc_overall, 3) < lag(pc_overall, 4) )  
                             ~ T , T ~ F),
    trend_over = case_when(trend_i_over == T | lead(trend_i_over, 1) == T | lead(trend_i_over, 2) == T
                           | lead(trend_i_over, 3) == T | lead(trend_i_over, 4) == T
                           ~ T, T ~ F)) %>% 
  mutate(# Shift: run of 6 or more consecutive data points above or below the pc_ever_centreline
    # First id when this run is happening and then iding all points part of it
    shift_i_ever = case_when((pc_ever > pc_ever_centreline & lag(pc_ever, 1) > pc_ever_centreline 
                              & lag(pc_ever, 2) > pc_ever_centreline & lag(pc_ever, 3) > pc_ever_centreline 
                              & lag(pc_ever, 4) > pc_ever_centreline & lag(pc_ever, 5) > pc_ever_centreline) |
                               (pc_ever < pc_ever_centreline & lag(pc_ever, 1) < pc_ever_centreline 
                                & lag(pc_ever, 2) < pc_ever_centreline & lag(pc_ever, 3) < pc_ever_centreline 
                                & lag(pc_ever, 4) < pc_ever_centreline & lag(pc_ever, 5) < pc_ever_centreline) ~ T , T ~ F),
    shift_ever = case_when(shift_i_ever == T | lead(shift_i_ever, 1) == T | lead(shift_i_ever, 2) == T
                           | lead(shift_i_ever, 3) == T | lead(shift_i_ever, 4) == T
                           | lead(shift_i_ever, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points
    trend_i_ever = case_when((pc_ever > lag(pc_ever ,1) & lag(pc_ever, 1) > lag(pc_ever, 2) 
                              & lag(pc_ever, 2) > lag(pc_ever, 3)  & lag(pc_ever, 3) > lag(pc_ever, 4)) |
                               (pc_ever < lag(pc_ever ,1) & lag(pc_ever, 1) < lag(pc_ever, 2) 
                                & lag(pc_ever, 2) < lag(pc_ever, 3)  & lag(pc_ever, 3) < lag(pc_ever, 4) )  
                             ~ T , T ~ F),
    trend_ever = case_when(trend_i_ever == T | lead(trend_i_ever, 1) == T | lead(trend_i_ever, 2) == T
                           | lead(trend_i_ever, 3) == T | lead(trend_i_ever, 4) == T
                           ~ T, T ~ F)) %>% 
  select(-shift_i_ever, -trend_i_ever, -shift_i_excl, -trend_i_excl, -shift_i_over, -trend_i_over) %>% 
  ungroup

remove(breastfeeding_centreline)

saveRDS(breastfeeding, "shiny_app/data/breastfeeding.rds")
saveRDS(breastfeeding, paste0(data_folder,"final_app_files/breastfeeding_", 
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Prescribing - Mental health ----
###############################################.

### historic file for MH drugs ##
mentalhealth_drugs_historic <- read_xlsx(paste0(data_folder, "prescribing_mh/Weekly new incident emessage - Multi-condition Jan 18-Jun 20.xlsx")) %>% 
  select(1:5) %>% 
  clean_names() %>% 
  filter(condition %in% c("Anxiolytic",
                          "Hypnotic",
                          "SSRI SNRI")) %>% 
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
         count = incident_cases_week_01) %>% 
  select(week_ending, area_name, area_type, type, category, count)

mentalhealth_drugs_hist_all <- mentalhealth_drugs_historic %>% 
  group_by(week_ending, area_name, area_type, type) %>% 
  summarise(count = sum(count),
            category = "All") %>% 
  ungroup() %>% 
  select(week_ending, area_name, area_type, type, category, count)

mentalhealth_drugs_historic <- rbind(mentalhealth_drugs_historic, mentalhealth_drugs_hist_all)

### Newer MH drugs data ##
mentalhealth_drugs <- read_xlsx(paste0(data_folder, "prescribing_mh/2020-11-26-Weekly new incident emessage - Multi-condition.xlsx")) %>% 
  select(1:5) %>% 
  clean_names() %>% 
  filter(condition %in% c("Anxiolytic",
                          "Hypnotic",
                          "SSRI SNRI")) %>% 
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
         count = incident_cases_week_01) %>% 
  select(week_ending, area_name, area_type, type, category, count) %>%
  filter(week_ending >= as.Date("2020-07-05"))

mentalhealth_drugs_all <- mentalhealth_drugs %>% 
  group_by(week_ending, area_name, area_type, type) %>% 
  summarise(count = sum(count),
            category = "All") %>% 
  ungroup() %>% 
  select(week_ending, area_name, area_type, type, category, count)

mentalhealth_drugs <- rbind(mentalhealth_drugs, mentalhealth_drugs_all)

mentalhealth_drugs <- rbind(mentalhealth_drugs, mentalhealth_drugs_historic)

prepare_final_data(mentalhealth_drugs, "mentalhealth_drugs", last_week = "2020-11-22")

###############################################.
## A&E - mental health ----
###############################################.
# mh_aye_hist <- read_csv(paste0(data_folder, "A&E_mh/A&E_Extract_-_Mental_Health_Wider_impacts.csv"))
# saveRDS(mh_aye_hist, paste0(data_folder, "A&E_mh/A&E_mh_2018to310502020.rds"))
mh_aye <- rbind(readRDS(paste0(data_folder, "A&E_mh/A&E_mh_2018to310502020.rds")) %>% 
                  filter(as.Date(`Arrival Date`) < as.Date("2020-06-01")) %>%
                  mutate(`Arrival Date`=as.Date(`Arrival Date`,format="%Y/%m/%d")),
                read_csv(paste0(data_folder, "A&E_mh/A&E_Extract_-_Mental_Health_Wider_impacts 01062020to22112020.csv"))) %>%
  clean_names() 

# List of terms used to identify mh cases
mh_aye_freetext <- toupper(paste0("overdose|'OD|O/D|drug od|drugs od|drug overdose|", 
                                  "self harm|self-harm|selfharm|depress|psych|", 
                                  "mental health|mentalhealth|mental-health|",
                                  "mental illness|mentalillness|mental-illness|",
                                  "suicid|suicdal|eating disorder|eatingdisorder|eating-disorder|",
                                  " MHAT| CAHMS|behavioural disorder|mental disorder|",
                                  "personality disorder|personalitydisorder|personality-disorder|", 
                                  "anxiety|bipolar|schizophren|schizoaffective|",
                                  "anorexi|bulimi|adhd|dissociative| dsh|",
                                  "adjustment disorder|emotional disorder|bereavement|",
                                  "self-poison|selfpoison|self poison|",
                                  "delusional|hallucination|alcohol withdrawal|withdrawal from alcohol|",
                                  "drug withdrawal|drugs withdrawal|withdrawal drug|",
                                  "manic episode|panic|recreational drug use|intoxication"))

# List of terms used to identify false positives
mh_text_notincluded <- paste0("ACCIDENTAL OVERDOSE|ACCIDENTAL POISONING|",
                              "ACCIDENTAL CHILD POISONING|TYMPANIC")

mh_aye %<>%
  mutate(diagnosis_1_text = toupper(diagnosis_1_text),
         diagnosis_2_text = toupper(diagnosis_2_text),
         diagnosis_3_text = toupper(diagnosis_3_text),
         presenting_complaint_text = toupper(presenting_complaint_text)) %>% 
  # Creating variable for those case identified through codes and no free text
  mutate(def_yes = case_when((substr(intent_of_injury_code,1,2) == "02" | #intentional self-harm
                                diagnosis_1_code %in% c("16") | #16 is psychiatry
                                diagnosis_2_code %in% c("16") |
                                diagnosis_3_code %in% c("16") |
                                # Including all Fs apart from dementia, delirium and learning disabilities
                                # Including R44-R46: hallucinations, emotional states
                                # Includinx X60-x84: intentional self-harm
                                substr(disease_1_code, 1, 2) %in% c("F1", "F2", "F3", "F4", "F5", "F6", "F8", "F9", "X6", "X7") |
                                substr(disease_2_code, 1, 2) %in% c("F1", "F2", "F3", "F4", "F5", "F6", "F8", "F9", "X6", "X7") |
                                substr(disease_3_code, 1, 2) %in% c("F1", "F2", "F3", "F4", "F5", "F6", "F8", "F9", "X6", "X7") |
                                substr(disease_1_code, 1, 3) %in% c("R44", "R45", "R46", "X80", "X81", "X82", "X83", "X84",
                                                                    "F06", "F07", "F08", "F09") |
                                substr(disease_2_code, 1, 3) %in% c("R44", "R45", "R46", "X80", "X81", "X82", "X83", "X84",
                                                                    "F06", "F07", "F08", "F09") |
                                substr(disease_3_code, 1, 3) %in% c("R44", "R45", "R46", "X80", "X81", "X82", "X83", "X84",
                                                                    "F06", "F07", "F08", "F09") |
                                # sequalae and personal historyof self-harm and psych traum
                                substr(disease_1_code, 1, 4) %in% c("Y871", "Z914", "Z915", "Z004", "Z046") | 
                                substr(disease_2_code, 1, 4) %in% c("Y871", "Z914", "Z915", "Z004", "Z046") |
                                substr(disease_3_code, 1, 4) %in% c("Y871", "Z914", "Z915", "Z004", "Z046") ) ~ 1, T ~0)) 

# Filtering based on conditions above and free text search terms
mh_aye %<>% 
  filter(def_yes == 1 |
           grepl(mh_aye_freetext, diagnosis_1_text) |
           grepl(mh_aye_freetext, diagnosis_2_text) |
           grepl(mh_aye_freetext, diagnosis_3_text) |
           grepl(mh_aye_freetext, presenting_complaint_text) )

# Excluding false positives
mh_aye %<>% 
  filter(!(def_yes == 0 & (
    grepl(mh_text_notincluded, presenting_complaint_text)|
      grepl(mh_text_notincluded, diagnosis_1_text)|
      grepl(mh_text_notincluded, diagnosis_2_text)|
      grepl(mh_text_notincluded, diagnosis_3_text))))

mh_aye %<>% #excluding very young kids as mostly false positives
  filter(pat_age>4) 

#Now another round excluding accidental poisonings, etc
mh_aye %<>% 
  # Formatting dataset
  rename(dep=prompt_dataset_deprivation_scot_quintile, age=pat_age,
         sex=pat_gender_description, count=number_of_attendances,
         hb=treatment_nhs_board_description_as_at_date_of_episode) %>%
  proper() %>% #fixing formatting of names
  mutate(area_type = "Health board",
         week_ending = ceiling_date(as.Date(arrival_date), "week", change_on_boundary = F),
         age_grp = as.character(case_when(between(age, 0, 17) ~ "5 - 17",
                                          between(age, 18, 44) ~ "18 - 44", 
                                          between(age, 45, 64) ~ "45 - 64", 
                                          between(age, 65, 200) ~ "65 and over", 
                                          T ~ "Missing"))) %>%
  create_depgroups() %>%
  group_by(week_ending, area_name, area_type,  age_grp, sex, dep) %>%
  summarise(count=sum(count, na.rm = T)) %>% #aggregating
  ungroup() 

# Generate scotland level dataset
mh_aye_scot <- mh_aye %>%
  group_by(week_ending, age_grp, sex, dep) %>%
  summarise(count=sum(count, na.rm = T)) %>%
  mutate(area_name="Scotland", area_type="Scotland") %>% ungroup()

# Joining together
mh_aye <- rbind(mh_aye, mh_aye_scot) %>% 
  rename(age=age_grp) %>%  mutate(week_ending=as.Date(week_ending,format="%d/%m/%Y")) 

#Use aggregation function to aggregate data files into format
mh_aye_all <- mh_aye %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
mh_aye_sex <- agg_cut(dataset=mh_aye, grouper="sex") %>% rename(category=sex)
mh_aye_dep <- agg_cut(dataset=mh_aye, grouper="dep") %>% rename(category=dep)
mh_aye_age <- agg_cut(dataset=mh_aye, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
mh_aye <- rbind(mh_aye_all, mh_aye_sex, mh_aye_dep, mh_aye_age) 

# Filtering out cuts with very small counts for HBS
mh_aye %<>% 
  filter(!(area_name %in% c("NHS Western Isles", "NHS Orkney", "NHS Shetland"))) %>% 
  filter(area_name == "Scotland" | category == "All")

prepare_final_data(mh_aye, "mh_A&E", last_week = "2020-11-22")

###############################################.
## OOH - mental health ----
###############################################.

mh_ooh <- read_tsv(paste0(data_folder, "GP_OOH_mh/GP_OOH_MH_WIDER_IMPACT_30nov.txt")) %>%
  janitor::clean_names() %>%
  rename(hb=patient_nhs_board_description_current, 
         dep=patient_prompt_dataset_deprivation_scot_quintile,sex=gender_description,
         count=gp_ooh_number_of_cases, age_group=age_band, week_ending=gp_ooh_sc_start_date) %>%
  mutate(week_ending = as.Date(week_ending, format= "%d/%m/%Y"), 
         week_ending = ceiling_date(week_ending, "week", change_on_boundary = F),
         # Query excludes under 5s
         age = recode_factor(age_group, "0-12" = "5 - 17", "13 to 17" = "5 - 17",  
                             "18 to 24" = "18 - 44", "25 to 34" = "18 - 44", "35 to 44" = "18 - 44", "45 to 54" = "45 - 64", 
                             "55 to 64" = "45 - 64", "65 to 74" = "65 and over", "75 to 84" = "65 and over",
                             "85plus" = "65 and over"),
         sex = recode(sex, "MALE" = "Male", "FEMALE" = "Female", "0" = NA_character_, "9" = NA_character_),
         dep = recode(dep, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         #week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         scot = "Scotland") %>%
  proper() #convert HB names to correct format

# Aggregate up to get figures for each area type.
mh_ooh %<>% gather(area_type, area_name, c(area_name, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                             "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>%
  filter(between(week_ending, as.Date("2018-01-01"), as.Date("2020-11-22")))

mh_ooh_all <- mh_ooh %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
mh_ooh_sex <- mh_ooh %>% agg_cut(grouper="sex") %>% rename(category = sex)
mh_ooh_dep <- mh_ooh %>% agg_cut(grouper="dep") %>% rename(category = dep)
mh_ooh_age <- mh_ooh %>% agg_cut(grouper="age") %>% rename(category = age)

mh_ooh <- rbind(mh_ooh_all, mh_ooh_sex, mh_ooh_dep, mh_ooh_age)

mh_ooh %<>% 
  filter(!(area_name %in% c("NHS Western Isles", "NHS Orkney", "NHS Shetland"))) %>% 
  filter(area_name == "Scotland" | category == "All")

prepare_final_data(mh_ooh, "mh_ooh", last_week = "2020-11-22")

##END
