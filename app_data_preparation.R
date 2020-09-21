# Data preparation for app

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("functions_packages_data_prep.R")

###############################################.
## RAPID data ----
###############################################.
# Prepared by Unscheduled care team
rap_adm <- readRDS(paste0(data_folder, "rapid/Admissions_by_category_21-Sep.rds")) %>% 
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

prepare_final_data(rap_adm, "rapid", last_week = "2020-09-13", 
                   extra_vars = c("admission_type", "spec"))

###############################################.
## OOH data ----
###############################################.
# Saving big files as RDS to avoid unzipping 
# ooh <- read_csv(unzip(paste0(data_folder, "GP_OOH/OOH DATA 2018 - 22032020.zip"),"OOH DATA 2018 - 22032020.csv")) 
# saveRDS(ooh, paste0(data_folder,"GP_OOH/OOH DATA 2018 - 22032020.rds"))
# file.remove(paste0(data_folder,"GP_OOH/OOH DATA 2018 - 22032020.zip"))
# ooh_new <- read_csv(unzip(paste0(data_folder, "GP_OOH/COVID DASHBOARD EXTRACT_2203to0505.zip"),"COVID DASHBOARD EXTRACT_2203to0505.csv"))
# saveRDS(ooh_new, paste0(data_folder,"GP_OOH/COVID DASHBOARD EXTRACT_2203to0505.rds"))
# file.remove(paste0(data_folder, "GP_OOH/COVID DASHBOARD EXTRACT_2203to0505.zip"))

# Read in historic OOH file
ooh <- readRDS(paste0(data_folder, "GP_OOH/OOH DATA 2018 - 22032020.rds")) %>%
  janitor::clean_names() %>%
  rename(hb=treatment_nhs_board_name, hscp=hscp_of_residence_name,
         dep=prompt_dataset_deprivation_scot_quintile,sex=gender,
         count=number_of_consultations) %>%
  mutate(age = recode_factor(age_group, "0-4" = "Under 5", "5-14" = "5 - 14",  
                                   "15-24" = "15 - 44", "25-44" = "15 - 44", "45-64" = "45 - 64",
                                   "65-74" = "65 - 74", "75-84" = "75 - 84",
                                   "85 plus" = "85 and over"),
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
  filter(between(week_ending, as.Date("2018-01-01"), as.Date("2020-03-22")))

## Additional new OOH data
ooh_new <- readRDS(paste0(data_folder, "GP_OOH/COVID DASHBOARD EXTRACT_2203to0505.rds")) %>%
  janitor::clean_names() %>%
  rename(date=sc_start_date, hb=treatment_nhs_board_name, hscp=hscp_of_residence_name_current,
         dep=prompt_dataset_deprivation_scot_quintile,sex=gender, age_group=age_band,
         count=number_of_cases) %>%
  mutate(age = recode_factor(age_group, "0-4" = "Under 5", "5-9" = "5 - 14",  "10-14" = "5 - 14",  
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
         date = as.Date(date, "%d/%m/%y")) %>% #formatting date
  mutate(week_ending = ceiling_date(date, "week", change_on_boundary = F)) %>% #end of week
  proper() #convert HB names to correct format

# Aggregate up to get figures for each area type.
ooh_new %<>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating by week to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>% 
  filter(between(week_ending, as.Date("2020-03-23"), as.Date("2020-04-26")))  #filter complete weeks (Mon-Sun)

#new data extract from week ending 03 may 2020 up to week ending 31 may 2020
ooh_may_onwards <- read_excel(paste0(data_folder, "GP_OOH/WIDER IMPACT PC OOH Data_53_7564963323438636106.xlsx")) %>% 
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

ooh_may_onwards %<>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup()

#bind old and new ooh data
ooh <- rbind(ooh_may_onwards, ooh_new, ooh)

# Creating totals for groups
ooh_all <- ooh %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
ooh_sex <- ooh %>% agg_cut(grouper="sex") %>% rename(category = sex)
ooh_dep <- ooh %>% agg_cut(grouper="dep") %>% rename(category = dep)
ooh_age <- ooh %>% agg_cut(grouper="age") %>% rename(category = age)

ooh <- rbind(ooh_all, ooh_sex, ooh_dep, ooh_age)

# Formatting file for shiny app
prepare_final_data(dataset = ooh, filename = "ooh", last_week = "2020-09-06")

###############################################.
## A&E data ----
###############################################.
# Read A&E data both at HSCP and HB level
ae_data <- rbind(read_csv(unz(paste0(ae_folder,"HSCP-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"),
                              "HSCP.csv")) %>% 
                   janitor::clean_names() %>% 
                   rename(area=hscp_of_residence_code_as_at_arrival_date),
                 read_csv(unz(paste0(ae_folder,"NHSBoard-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"), 
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

prepare_final_data(ae_data, "ae", last_week = "2020-09-13")

###############################################.
## NHS24 data ----
###############################################.

# #Read in new nhs24 data as txt file, save as RDS and remove txt file version from directory.
# #Each week this section of code can be uncommented run for the latest weeks data then recommented after txt file deleted
    # nhs24 <- (read_tsv(paste0(data_folder,"NHS24/NHS24 Extract 17082020 to 23082020.txt")))
    # saveRDS(nhs24, paste0(data_folder,"NHS24/NHS24 Extract 17082020 to 23082020.rds"))
    # file.remove(paste0(data_folder,"NHS24/NHS24 Extract 17082020 to 23082020.txt"))

nhs24 <-  rbind(readRDS(paste0(data_folder, "NHS24/NHS24 01Jan2018 to 07Jun2020.rds")),
                read_tsv(paste0(data_folder, "NHS24/NHS24_report_08062020to20092020.txt"))) %>%
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
prepare_final_data(dataset = nhs24, filename = "nhs24", last_week = "2020-09-20")

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
sas_new <- read_tsv(paste0(data_folder,"SAS/COVID_WIDER_IMPACT_SAS_11052020to06092020.txt")) %>% 
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
prepare_final_data(dataset = sas, filename = "sas", last_week = "2020-09-06")

###############################################.
## Deaths ----
###############################################.
deaths <- readRDS(paste0(data_folder, "deaths/deaths_data.rds"))
saveRDS(deaths, "shiny_app/data/deaths_data.rds")
saveRDS(deaths, paste0(open_data, "deaths_data.rds"))

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

###############################################.
## A&E Cardio ----
###############################################.

# Reads in A&E cardio ICD 10 codes for modal, only required to run in case code list changes
ae_cardio_codes <- read_xlsx(paste0(data_folder, "A&E_Cardio/A&E-CardioConditionCodes.xlsx"))
saveRDS(ae_cardio_codes, "shiny_app/data/ae_cardio_codes.rds")
rm(ae_cardio_codes)

# Read in data, clean names + some simple mutations
ae_cardio <- read_xlsx(paste0(ae_folder, "CardioVascular-AttendancesDuringCovid-19.xlsx")) %>% 
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

prepare_final_data(ae_cardio, "ae_cardio", last_week = "2020-09-13")

###############################################.
## Prescribing - Cardiovascular Drugs ----
###############################################.
cardio_drugs <- read_xlsx(paste0(data_folder, "prescribing_cardio/covid emessage AMS only 20200917.xlsx")) %>% 
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

prepare_final_data(cardio_drugs, "cardio_drugs", last_week = "2020-09-13")

###############################################.
## 6-in-1 s-curve data ----
###############################################.
six_alldose <- read_csv(paste0(data_folder,"immunisations/6in1/six_in_one_dashboard_20200824.csv"), 
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

saveRDS(six_alldose, "shiny_app/data/six_alldose_data.rds")

###############################################.
## immunisations data table dataset prep ----
## immunisation team supply a single csv file that is split into two rds files (one for each immunisation)

imms_datatable <- format_immchild_table("immunisations/dashboardtable_20200824")
                           
six_datatable <- imms_datatable %>%
  filter(str_detect(immunisation,"six-in-one")) %>%
  select(-uptake_13m_num:-uptake_3y8m_percent) #remove uptake columns that related to mmr 
saveRDS(six_datatable, paste0("shiny_app/data/","sixinone_datatable.rds"))

mmr_datatable <- imms_datatable %>%
  filter(str_detect(immunisation,"mmr")) %>%
  select(-uptake_12weeks_num:-uptake_32weeks_percent) #remove uptake columns that related to mmr 
saveRDS(mmr_datatable, paste0("shiny_app/data/","mmr_datatable.rds"))

# Grampian data
mmr_dose2_datatable_grampian <- format_immchild_table("immunisations/mmr/dashboardtable_grampian_20200824") 
saveRDS(mmr_dose2_datatable_grampian, paste0("shiny_app/data/","mmr_dose2_datatable_grampian.rds"))

###############################################.
## 6-in-1 simd data ---- 
six_dose1_simdtable <- format_immsimd_data("immunisations/6in1/six-in-one dose 1_simd_20200824")
saveRDS(six_dose1_simdtable, paste0("shiny_app/data/","six_dose1_simdtable.rds"))

six_dose2_simdtable <- format_immsimd_data("immunisations/6in1/six-in-one dose 2_simd_20200824")
saveRDS(six_dose2_simdtable, paste0("shiny_app/data/","six_dose2_simdtable.rds"))

six_dose3_simdtable <- format_immsimd_data("immunisations/6in1/six-in-one dose 3_simd_20200824")
saveRDS(six_dose3_simdtable, paste0("shiny_app/data/","six_dose3_simdtable.rds"))

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

###############################################.
## MMR s-curve data ----
###############################################.
# mmr dose 1 & 2 - scurve data
mmr_alldose <- read_csv(paste0(data_folder,"immunisations/mmr/mmr_dashboard_20200824.csv"),
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

saveRDS(mmr_alldose, paste0("shiny_app/data/","mmr_alldose_data.rds"))

###############################################.
## MMR data table ----
###############################################.
# 
# # MMR at dose 1  - summary table data
# mmr_dose1_datatable <- rbind(format_immchild_table("immunisations/mmr/mmr_dose1_dashboardtab_20200727"),
#                              format_immchild_table("immunisations/mmr/mmr_dose1_islandboarddownload_20200727"))
# saveRDS(mmr_dose1_datatable, paste0("shiny_app/data/","mmr_dose1_datatable.rds"))
# 
# # MMR at dose 2  - summary table data
# mmr_dose2_datatable <- rbind(format_immchild_table("immunisations/mmr/mmr_dose2_dashboardtab_20200727"),
#                              format_immchild_table("immunisations/mmr/mmr_dose2_islandboarddownload_20200727"))
# saveRDS(mmr_dose2_datatable, paste0("shiny_app/data/","mmr_dose2_datatable.rds"))
# 
# # Grampian data
# mmr_dose2_datatable_grampian <- format_immchild_table("immunisations/mmr/mmr_dose2_dashboardtab_grampian_20200727") 
# saveRDS(mmr_dose2_datatable_grampian, paste0("shiny_app/data/","mmr_dose2_datatable_grampian.rds"))

###############################################.
# ## MMR hscp data ----
# mmr_1_hscp <- format_immhscp_table("immunisations/mmr/mmr_dose1_dashboardtab-hscp_20200727")
# saveRDS(mmr_1_hscp, paste0("shiny_app/data/","mmr_dose1_hscp.rds"))
# 
# mmr_2_hscp <- format_immhscp_table("immunisations/mmr/mmr_dose2_dashboardtab-hscp_20200727")
# saveRDS(mmr_2_hscp, paste0("shiny_app/data/","mmr_dose2_hscp.rds"))
# 
# mmr_2_hscp_grampian <- format_immhscp_table("immunisations/mmr/mmr_dose2_dashboardtab_grampian_hscp_20200727")
# saveRDS(mmr_2_hscp_grampian, paste0("shiny_app/data/","mmr_dose2_hscp_grampian.rds"))

###############################################.
## MMR simd data ----
###############################################.
mmr_dose1_simdtable <- format_immsimd_data("immunisations/mmr/mmr dose 1_simd_20200824")
saveRDS(mmr_dose1_simdtable, paste0("shiny_app/data/","mmr_dose1_simdtable.rds"))

mmr_dose2_simdtable <- format_immsimd_data("immunisations/mmr/mmr dose 2_simd_20200824")
saveRDS(mmr_dose2_simdtable, paste0("shiny_app/data/","mmr_dose2_simdtable.rds"))

###############################################.
## Child health review: first visit ----
###############################################.
## First visit - scurve data
first <- read_csv(paste0(data_folder,"child_health/firstvisit_dashboard20200824.csv"), 
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

saveRDS(first, paste0("shiny_app/data/","first_visit_data.rds"))

# First visit - summary table data
first_datatable_download <- format_immchild_table("child_health/firstvisit_dashboardtab_20200824") 

saveRDS(first_datatable_download, paste0("shiny_app/data/","first_visit_datatable_download.rds"))

first_datatable <- first_datatable_download %>% 
  filter(exclude == 0)

saveRDS(first_datatable, paste0("shiny_app/data/","first_visit_datatable.rds"))

###############################################.
## Child health review: 6-8 weeks  ----
###############################################.

## 6 to 8 weeks visit - scurve data
sixtoeight <- read_csv(paste0(data_folder,"child_health/sixtoeight_dashboard20200824.csv"), 
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

saveRDS(sixtoeight, paste0("shiny_app/data/","six_to_eight_data.rds"))

# 6-8 weeks visit - summary table data
sixtoeight_datatable_download <- format_immchild_table("child_health/sixtoeight_dashboardtab_20200824") 

saveRDS(sixtoeight_datatable_download, paste0("shiny_app/data/","six_to_eight_datatable_download.rds"))

sixtoeight_datatable <- sixtoeight_datatable_download %>% 
  filter(exclude == 0)

saveRDS(sixtoeight_datatable, paste0("shiny_app/data/","six_to_eight_datatable.rds"))


###############################################.
## Child health review: 13-15 month ----
###############################################.

## 13 to 15 month visit - scurve data
thirteen <- read_csv(paste0(data_folder,"child_health/thirteen_dashboard20200824.csv"), 
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

saveRDS(thirteen, paste0("shiny_app/data/","thirteen_data.rds"))

# 13 to 15 month visit - summary table data
thirteen_datatable_download <- format_immchild_table("child_health/thirteen_dashboardtab_20200824") 

saveRDS(thirteen_datatable_download, paste0("shiny_app/data/","thirteen_datatable_download.rds"))

thirteen_datatable <- thirteen_datatable_download %>% 
  filter(exclude == 0)

saveRDS(thirteen_datatable, paste0("shiny_app/data/","thirteen_datatable.rds"))

###############################################.
## Child health review: 27-30 month ----
###############################################.

## 27 to 30 month visit - scurve data
twentyseven <- read_csv(paste0(data_folder,"child_health/twentyseven_dashboard20200824.csv"), 
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

saveRDS(twentyseven, paste0("shiny_app/data/","twentyseven_data.rds"))

# 27 to 30 month visit - summary table data
# Data for data download should include complete months and all weeks
twentyseven_datatable_download <- format_immchild_table("child_health/twentyseven_dashboardtab_20200824") 

saveRDS(twentyseven_datatable_download, paste0("shiny_app/data/","twentyseven_datatable_download.rds"))

# Data for flextable should include complete months and weeks for incomplete months only
twentyseven_datatable <- twentyseven_datatable_download %>% 
  filter(exclude == 0)

saveRDS(twentyseven_datatable, paste0("shiny_app/data/","twentyseven_datatable.rds"))

###############################################.
## Child health review: 4-5 year ----
###############################################.

## 4 to 5 year visit - scurve data
fourtofive <- read_csv(paste0(data_folder,"child_health/fourtofive_dashboard20200824.csv"), 
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

saveRDS(fourtofive, paste0("shiny_app/data/","fourtofive_data.rds"))

# 4 to 5 year review - summary table data
# Data for data download should include complete months and all weeks
fourtofive_datatable_download <- format_immchild_table("child_health/fourtofive_dashboardtab_20200824") %>% 
  filter(area_name != "NHS Dumfries & Galloway") %>%  
  filter(area_name != "NHS Highland")

saveRDS(fourtofive_datatable_download, paste0("shiny_app/data/","fourtofive_datatable_download.rds"))

# Data for flextable should include complete months and weeks for incomplete months only
fourtofive_datatable <- fourtofive_datatable_download %>% 
  filter(exclude == 0)

saveRDS(fourtofive_datatable, paste0("shiny_app/data/","fourtofive_datatable.rds"))

###############################################.
## Perinatal mortality ----
###############################################.
# P CHART PERINATAL DATA
p_perinatal <- bind_rows(read_excel(paste0(data_folder,"perinatal/Pchart - SB NND PNND EXTPERI_augupdate.xlsx"),
                          sheet = "Stillbirth", skip = 2) %>% mutate(type = "stillbirths"),
                     read_excel(paste0(data_folder,"perinatal/Pchart - SB NND PNND EXTPERI_augupdate.xlsx"),
                                sheet = "NND", skip = 2) %>% mutate(type = "nnd"),
                     read_excel(paste0(data_folder,"perinatal/Pchart - SB NND PNND EXTPERI_augupdate.xlsx"),
                                sheet = "Extended perinatal", skip = 2) %>% mutate(type = "extperi"),
                     read_excel(paste0(data_folder,"perinatal/Pchart - SB NND PNND EXTPERI_augupdate.xlsx"),
                                sheet = "PNND", skip = 2) %>% mutate(type = "pnnd")) %>% 
  janitor::clean_names() %>%
  select(month_of_year=sample_2, number_of_deaths_in_month=observation, sample_size, rate, centreline, stdev = binomial_st_dev_16, 
         upper_cl_3_std_dev:type)

u_perinatal <- read_excel(paste0(data_folder,"perinatal/Uchart - INFANT DEATHS_augupdate.xlsx"),
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

saveRDS(perinatal, paste0("shiny_app/data/","perinatal_data.rds"))

##END

