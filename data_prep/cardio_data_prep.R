# Data preparation for cardiovascular tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

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
ae_cardio <- read_xlsx(paste0(ae_folder, "2021-01-08-CardioVascular-AttendancesDuringCovid-19.xlsx")) %>% 
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

prepare_final_data(ae_cardio, "ae_cardio", last_week = "2021-01-03")

###############################################.
## OOH Cardiac  ----
###############################################.

ooh_data_cardiac <- read_csv(paste0(data_folder, "GP_OOH_Cardio/2021-01-11-Weekly Cardio Diagnosis OOH extract.csv")) %>% 
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
prepare_final_data_cardiac(dataset = ooh_cardiac, filename = "ooh_cardiac", last_week = "2021-01-03")

###############################################.
## SAS Cardiac ----
###############################################.

sas_data_cardiac <- read_csv(paste0(data_folder,"SAS_Cardio/2021-01-11-Weekly Cardio Diagnosis SAS extract.csv")) %>%
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
prepare_final_data_cardiac(dataset = sas_cardiac, filename = "sas_cardiac", last_week = "2021-01-03")

###############################################.
## Prescribing - Cardiovascular Drugs ----
###############################################.
cardio_drugs <- read_xlsx(paste0(data_folder, "prescribing_cardio/2021-01-07-covid emessage AMS only.xlsx")) %>% 
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

prepare_final_data(cardio_drugs, "cardio_drugs", last_week = "2021-01-03")

##END