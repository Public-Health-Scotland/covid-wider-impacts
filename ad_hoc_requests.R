# Code for ad hoc requests related to the data included in the dashboard

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("functions_packages_data_prep.R")

###############################################.
## Request for splits for age groups by deprivation ----
###############################################.
# They are interested in under 5, 5-15 and 15 to 44 by deprivation category
# for the measures: admissions, A&E, nhs24, ooh and sas.
# Adapted from the app_data_preparation script
###############################################.
# Functions
# Function to format data in the right format for the request
prepare_csv <- function(dataset, filename, last_week, extra_vars = NULL) {
  
  # Creating week number to be able to compare pre-covid to covid period
  dataset <- dataset %>% mutate(week_no = isoweek(week_ending),
                                # Fixing HSCP names
                                area_name = gsub(" and ", " & ", area_name))
  
  
  # Creating average admissions of pre-covid data (2018-2019) by day of the year
  historic_data <- dataset %>% filter(year(week_ending) %in% c("2018", "2019")) %>% 
    group_by_at(c("dep", "age", "area_name", "area_type", "week_no", extra_vars)) %>% 
    # Not using mean to avoid issues with missing data for some weeks
    summarise(count_average = round((sum(count, na.rm = T))/2, 1)) 
  
  # Joining with 2020 data
  # Filtering weeks with incomplete week too!! Temporary
  data_2020 <- left_join(dataset %>% filter(year(week_ending) %in% c("2020")), 
                         historic_data, 
                         by = c("dep", "age", "area_name", "area_type", "week_no", extra_vars)) %>% 
    # Filtering cases without information on age, sex, area or deprivation (still counted in all)
    filter(!(is.na(area_name) | 
               area_name %in% c("", "ENGLAND/WALES/NORTHERN IRELAND", "UNKNOWN HSCP - SCOTLAND",
                                "ENGland/Wales/Northern Ireland", "NANA"))) %>% 
    # Creating %variation from precovid to covid period 
    mutate(count_average = ifelse(is.na(count_average), 0, count_average),
           variation = round(-1 * ((count_average - count)/count_average * 100), 1),
           # Dealing with infinite values from historic average = 0
           variation =  ifelse(is.infinite(variation), 8000, variation)) %>% 
    select(-week_no) 
  
  data_2020 <- data_2020 %>% 
    filter(week_ending <= as.Date(last_week))
  
  final_data <<- data_2020
  
  write_csv(data_2020, paste0("/conf/PHSCOVID19_Analysis/Publication outputs/mairi_watson_request/", filename, "_data.csv"))
}

# Speed up aggregations of different data cuts (A&E,NHS24,OOH)
agg_cut_hoc <- function(dataset, grouper) {
  dataset %>%
    group_by_at(c("week_ending","area_name", "area_type", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup()
  }


###############################################.
# RAPID admissions 
rap_adm <- readRDS(paste0(data_folder, "rapid/Admissions_by_category_27-Jul.rds")) %>% 
  janitor::clean_names() %>% 
  # taking out aggregated values, not clear right now
  filter(!(substr(hosp,3,5) == "All" | (substr(hscp_name,3,5) == "All")) &
           date_adm > as.Date("2017-12-31")) 

# Bringing HB names
rap_adm <- left_join(rap_adm, hb_lookup, by = c("hb" = "hb_cypher")) %>% 
  select(-hb) %>% rename(hb = area_name) %>% select(-area_type)

# Formatting groups
rap_adm %<>% 
  filter(age_group %in% c("Under_5", "5_thru_14", "15_thru_44" )) %>% 
  rename(dep = simd_quintile, age = age_group) %>%
  mutate(age = recode_factor(age, "Under_5" = "Under 5", "5_thru_14" = "5 - 14", 
                             "15_thru_44" = "15 - 44")) %>% 
  create_depgroups()  %>% 
  mutate(admission_type = recode(admission_type, "elective" = "Planned", "emergency" = "Emergency")) 

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
rap_adm_agedep <- agg_rapid(c("age", "dep"), split = "agedep") %>%  # Totals for overalls for all age groups
  filter(dep != "Missing") %>% select(-spec, -type)

prepare_csv(rap_adm_agedep, "rapid_agedep", last_week = "2020-07-19", extra_vars = c("admission_type"))

###############################################.
# OOH
# Read in historic OOH file
ooh <- readRDS(paste0(data_folder, "GP_OOH/OOH DATA 2018 - 22032020.rds")) %>%
  janitor::clean_names() %>%
  rename(hb=treatment_nhs_board_name, hscp=hscp_of_residence_name,
         dep=prompt_dataset_deprivation_scot_quintile,sex=gender,
         count=number_of_consultations) %>%
  filter(age_group %in% c("0-4", "5-14", "15-24", "25-44")) %>% 
  mutate(age = recode_factor(age_group, "0-4" = "Under 5", "5-14" = "5 - 14",  
                             "15-24" = "15 - 44", "25-44" = "15 - 44"),
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
                      .default = "Other"),
         dep = recode(dep, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         date = as.Date(date, "%d/%m/%y")) %>% #formatting date
  filter(age != "Other") %>% 
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
ooh_may_onwards <- read_excel(paste0(data_folder, "GP_OOH/WIDER IMPACT PC OOH Data_54_8981685717109972450.xlsx")) %>% 
  janitor::clean_names() %>%
  rename(count=number_of_cases, hscp=hscp_of_residence_name_current, age_group=age_band,
         hb=treatment_nhs_board_name, sex=gender, dep=prompt_dataset_deprivation_scot_quintile) %>%
  mutate(age_group = recode(age_group, "44079" = "5-9", "41913" = "10-14"),
         age = recode_factor(age_group, "0-4" = "Under 5", "5-9" = "5 - 14",  "10-14" = "5 - 14",  
                             "15-19" = "15 - 44", "20-24" = "15 - 44", "25-29" = "15 - 44", 
                             "30-34" = "15 - 44", "35-39" = "15 - 44", "40-44" = "15 - 44", 
                             .default = "Other"),
         dep = recode(dep, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - least deprived"),
         week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         scot = "Scotland") %>% 
  filter(age != "Other") %>% 
  proper() # convert HB names to correct format

ooh_may_onwards <- ooh_may_onwards %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup()

#bind old and new ooh data
ooh <- rbind(ooh_may_onwards, ooh_new, ooh)

# Creating totals for groups
ooh_agedep <- ooh %>% agg_cut_hoc(grouper=c("age", "dep")) %>% 
  filter(!(is.na(dep)))

# Formatting file for shiny app
prepare_csv(dataset = ooh_agedep, filename = "ooh_agedep", last_week = "2020-07-26")

###############################################.
# A&E
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
#Use aggregation function to aggregate data files into format
ae_agedep <- agg_cut_hoc(dataset=ae_data, grouper=c("age", "dep")) %>% 
  filter(age %in% c("15 - 44", "5 - 14", "Under 5") & dep != "Missing")

prepare_csv(ae_agedep, "ae_agedep", last_week = "2020-07-19")

###############################################.
## NHS24 data 

# #Read in new nhs24 data as txt file, save as RDS and remove txt file version from directory.
# #Each week this section of code can be uncommented run for the latest weeks data then recommented after txt file deleted
# nhs24 <- (read_tsv(paste0(data_folder,"NHS24/NHS24 Extract 20072020 to 26072020.txt")))
# saveRDS(nhs24, paste0(data_folder,"NHS24/NHS24 Extract 20072020 to 26072020.rds"))
# file.remove(paste0(data_folder,"NHS24/NHS24 Extract 20072020 to 26072020.txt"))

nhs24 <-  rbind(readRDS(paste0(data_folder, "NHS24/NHS24 01Jan2018 to 07Jun2020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 08062020 to 14062020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 15062020 to 21062020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 22062020 to 28062020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 29062020 to 05072020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 06072020 to 12072020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 13072020 to 19072020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 20072020 to 26072020.rds"))) %>%
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
nhs24_agedep <- agg_cut_hoc(dataset= nhs24, grouper=c("age", "dep")) %>% 
  filter(age %in% c("15 - 44", "5 - 14", "Under 5") & dep != "Missing")

# Formatting file for shiny app
prepare_csv(dataset = nhs24_agedep, filename = "nhs24_agedep", last_week = "2020-07-26")

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
sas_new <-read_tsv(paste0(data_folder,"SAS/COVID_WIDER_IMPACT_SAS_11052020to19072020.txt")) %>% 
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
  create_depgroups ()

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
sas_agedep <- agg_cut_hoc(dataset= sas, grouper=c("age", "dep"))  %>% 
  filter(dep != "Missing" & !(is.na(area_name)) & 
           age %in% c("15 - 44", "5 - 14", "Under 5"))

# Formatting file for shiny app
prepare_csv(dataset = sas_agedep, filename = "sas_agedep", last_week = "2020-07-19")

##END
