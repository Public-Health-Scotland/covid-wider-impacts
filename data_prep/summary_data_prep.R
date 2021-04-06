# Data preparation for summary trends tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## RAPID data ----
###############################################.
# Prepared by Unscheduled care team
rap_adm <- readRDS(paste0(data_folder, "rapid/Admissions_by_category_06-Apr.rds")) %>% 
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

saveRDS(spec_lookup, "shiny_app/data/spec_lookup_rapid.rds")
saveRDS(spec_lookup, paste0(data_folder,"final_app_files/spec_lookup_rapid", 
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

prepare_final_data(rap_adm, "rapid", last_week = "2021-03-28", 
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
ooh_may_onwards <- read_excel(paste0(data_folder, "GP_OOH/WIDER IMPACT PC OOH Data_51_2032005111570143530.xlsx")) %>% 
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
prepare_final_data(dataset = ooh, filename = "ooh", last_week = "2021-03-28")

###############################################.
## A&E data ----
###############################################.
# Read A&E data both at HSCP and HB level
ae_data <- rbind(read_csv(unz(paste0(ae_folder,"2021-04-01-HSCP-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"),
                              "HSCP.csv")) %>% 
                   janitor::clean_names() %>% 
                   rename(area=hscp_of_residence_code_as_at_arrival_date),
                 read_csv(unz(paste0(ae_folder,"2021-04-01-NHSBoard-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"), 
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

prepare_final_data(ae_data, "ae", last_week = "2021-03-28")

###############################################.
## NHS24 data ----
###############################################.

# #Read in new nhs24 data as txt file, save as RDS and remove txt file version from directory.
# #Each week this section of code can be uncommented run for the latest weeks data then recommented after txt file deleted
# nhs24 <- (read_tsv(paste0(data_folder,"NHS24/NHS24_Extract 08062020 to 27092020.txt")))
# saveRDS(nhs24, paste0(data_folder,"NHS24/NHS24 Extract 17082020 to 23082020.rds"))
# file.remove(paste0(data_folder,"NHS24/NHS24 Extract 17082020 to 23082020.txt"))

nhs24 <-  rbind(readRDS(paste0(data_folder, "NHS24/NHS24 01Jan2018 to 07Jun2020.rds")) %>% clean_names() %>% 
                  mutate(week_ending = as.Date(nhs_24_call_rcvd_date, format="%d-%b-%y")),
                read_tsv(paste0(data_folder, "NHS24/2021-04-05-NHS24 report v6 covid Extract PC (For Wider Impact work).txt")) %>% 
                  clean_names() %>% 
                  mutate(week_ending = as.Date(nhs_24_call_rcvd_date, format="%d-%b-%Y"))) %>%
  rename(hb = patient_nhs_board_description_current,
         hscp = nhs_24_patient_hscp_name_current,
         sex = gender_description,
         dep = nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
         covid_flag = nhs_24_covid_19_flag,
         count = number_of_nhs_24_records) %>% 
  # Formatting dates
  mutate(week_ending = ceiling_date(week_ending, "week", change_on_boundary = F))

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
prepare_final_data(dataset = nhs24, filename = "nhs24", last_week = "2021-03-28")

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
sas_new <- read_tsv(paste0(data_folder,"SAS/2021-04-05-COVID WIDER IMPACT SAS_Prompt report.txt")) %>% 
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
prepare_final_data(dataset = sas, filename = "sas", last_week = "2021-03-28")


### END ###.