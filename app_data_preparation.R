# Data preparation for app

###############################################.
## Packages ----
###############################################.
library(dplyr) #manipulating data
library(janitor) #cleaning names
library(lubridate)#dates
library(zoo) #dates
library(readr) #reading/writing files
library(stringr) #manipulating string
library(phsmethods) #matching codes with names
library(tidyr) # for wide to long formatting
library(readxl) # reading excel

data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/"

###############################################.
## Functions ----
###############################################.
# This function aggregates data for each different cut requires
agg_rapid <- function(grouper = NULL, split, specialty = F) {
  
  agg_helper <- function(more_vars, type_chosen = split) {
    rap_adm %>%
      group_by_at(c("week_ending","area_name", "area_type", more_vars)) %>%
      summarise(count = sum(count)) %>% ungroup() %>%
      mutate(type = type_chosen)
  }
  
  # Aggregating to obtain totals for each split type and then putting all back together.
  adm_type <- agg_helper(c(grouper, "admission_type")) %>% 
    mutate(spec = "All") 
  
  all <- agg_helper(grouper) %>% 
    mutate(admission_type = "All", spec = "All") 

  if (specialty == T) {
    spec_all <- agg_helper(c(grouper, "spec")) %>% 
      mutate(admission_type = "All") 
    
    spec_adm <- agg_helper(c(grouper, "spec", "admission_type")) 

    rbind(all, adm_type, spec_all, spec_adm)
  } else {
    rbind(all, adm_type) 
  }
}


# Speed up aggregations of different data cuts (A&E,NHS24,OOH)
agg_cut <- function(dataset, grouper) {
  dataset %>%
    group_by_at(c("week_ending","area_name", "area_type", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup() %>% 
    mutate(type = grouper) 
}


# Create age groups
create_agegroups <- function(dataset) {
  dataset %>% mutate(age_grp1 = as.character(case_when(between(age, 0, 4) ~ "Under 5",
                                                       between(age, 5, 14) ~ "5 - 14",
                                                       between(age, 15, 44) ~ "15 - 44", 
                                                       between(age, 45, 64) ~ "45 - 64", 
                                                       between(age, 65, 74) ~ "65 - 74", 
                                                       between(age, 75, 84) ~ "75 - 84",
                                                       between(age, 85, 200) ~ "85 and over")),
                     age_grp=case_when(is.na(age) ~"Missing", TRUE~age_grp1))
}

# Format sex groups
create_sexgroups <- function(dataset) {
  dataset %>% mutate(sex=case_when(is.na(sex)~"Missing", sex==1 ~ "Male", sex==2 ~"Female", 
                                   sex %in% c(0, 9 ) ~ "Missing", TRUE~as.character(sex)))
}

# Format deprivation groups
create_depgroups <- function(dataset) {
  dataset %>% mutate(dep=case_when(is.na(dep)~"Missing", dep==1 ~ "1 - most deprived",
                                   dep==5 ~"5 - least deprived", TRUE~as.character(dep)))
}

# Convert HB names to correct format
proper <- function(dataset) {
  dataset %>% 
    mutate(hb1= str_to_title(hb),
           area_name=paste0(toupper(substr(hb1, 1, 3)),substring(hb1, 4))) %>%
    select(-hb1, -hb)
}

# Function to format data in the right format for the Shiny app
prepare_final_data <- function(dataset, filename, last_week, extra_vars = NULL) {
  
  # Creating week number to be able to compare pre-covid to covid period
  dataset <- dataset %>% mutate(week_no = isoweek(week_ending),
                                # Fixing HSCP names
                                area_name = gsub(" and ", " & ", area_name))
    
  
  # Creating average admissions of pre-covid data (2018-2019) by day of the year
  historic_data <- dataset %>% filter(year(week_ending) %in% c("2018", "2019")) %>% 
    group_by_at(c("category", "type", "area_name", "area_type", "week_no", extra_vars)) %>% 
    # Not using mean to avoid issues with missing data for some weeks
    summarise(count_average = round((sum(count, na.rm = T))/2, 1)) 
  
  # Joining with 2020 data
  # Filtering weeks with incomplete week too!! Temporary
  data_2020 <- left_join(dataset %>% filter(year(week_ending) %in% c("2020")), 
                         historic_data, 
                         by = c("category", "type", "area_name", "area_type", "week_no", extra_vars)) %>% 
    # Filtering cases without information on age, sex, area or deprivation (still counted in all)
    filter(!(is.na(category) | category %in% c("Missing", "missing", "Not Known") |
               is.na(area_name) | 
               area_name %in% c("", "ENGLAND/WALES/NORTHERN IRELAND", "UNKNOWN HSCP - SCOTLAND",
                                "ENGland/Wales/Northern Ireland", "NANA"))) %>% 
    # Creating %variation from precovid to covid period 
    mutate(count_average = ifelse(is.na(count_average), 0, count_average),
           variation = round(-1 * ((count_average - count)/count_average * 100), 1),
           # Dealing with infinite values from historic average = 0
           variation =  ifelse(is.infinite(variation), 8000, variation)) %>% 
    select(-week_no) 
  
  # Supressing numbers under 5
  data_2020 <- data_2020 %>% filter(count>=5) %>% 
    filter(week_ending <= as.Date(last_week))
  
  final_data <<- data_2020
  
  saveRDS(data_2020, paste0("shiny_app/data/", filename,"_data.rds"))
  saveRDS(data_2020, paste0("/conf/PHSCOVID19_Analysis/Publication outputs/open_data/", filename,"_data.rds"))
}

###############################################.
## Reading RAPID data ----
###############################################.
# Prepared by Unscheduled care team
rap_adm <- readRDS(paste0(data_folder, "rapid/Admissions_by_category_22-Jun.rds")) %>% 
  janitor::clean_names() %>% 
  # taking out aggregated values, not clear right now
  filter(!(substr(hosp,3,5) == "All" | (substr(hscp_name,3,5) == "All")) &
           date_adm > as.Date("2017-12-31")) 

# Bringing HB names
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher)

rap_adm <- left_join(rap_adm, hb_lookup, by = c("hb" = "hb_cypher")) %>% 
  select(-hb) %>% rename(hb = description)

# Bringing spec names
spec_lookup <- read_csv("data/spec_groups_dashboard.csv")

rap_adm <- left_join(rap_adm, spec_lookup, by = c("spec" = "spec_code")) %>% 
  select(-spec) %>% rename(spec = dash_groups)

# For modal in app
spec_lookup <- spec_lookup %>% filter(!(dash_groups %in% c("Dental", "Other"))) %>% 
  arrange(dash_groups, spec_name) %>% 
  select("Specialty name" = spec_name, "Specialty group" = dash_groups)

saveRDS(spec_lookup, "shiny_app/data/spec_lookup.rds")

# Formatting groups
rap_adm <- rap_adm %>% 
  rename(dep = simd_quintile, age = age_group) %>%
  mutate(sex = recode(sex, "male" = "Male", "female" = "Female")) %>% 
  mutate(age = recode_factor(age, "Under_5" = "Under 5", "5_thru_14" = "5 - 14", 
                                  "15_thru_44" = "15 - 44", "45_thru_64" = "45 - 64",
                                  "65_thru_74" = "65 - 74", "75_thru_84" = "75 - 84",
                                  "85+" = "85 and over")) %>% 
  create_depgroups()  %>% 
  mutate(admission_type = recode(admission_type, "elective" = "Planned", "emergency" = "Emergency"))

# Aggregating to weekly data
rap_adm <- rap_adm %>% 
  mutate(week_ending = ceiling_date(date_adm, "week", change_on_boundary = F)) %>% #end of week
  group_by(hscp_name, hb, admission_type, dep, age, sex, week_ending, spec) %>% 
  summarise(count = sum(count, na.rm = T))

# Aggregating for each geo level
rap_adm <- rap_adm %>% mutate(scot = "Scotland") %>% 
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

rap_adm <- rbind(rap_adm, spec_med) %>% 
  # Excluding specialties groups with very few cases and of not much interest
  filter(!(spec %in% c("Dental", "Other"))) 

prepare_final_data(rap_adm, "rapid", last_week = "2020-06-14", 
                   extra_vars = c("admission_type", "spec"))

###############################################.
## Preparing OOH data ----
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
ooh <- ooh %>% gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
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
ooh_new <- ooh_new %>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating by week to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>% 
  filter(between(week_ending, as.Date("2020-03-23"), as.Date("2020-04-26")))  #filter complete weeks (Mon-Sun)

#new data extract from week ending 03 may 2020 up to week ending 31 may 2020
ooh_may_onwards <- read_csv(paste0(data_folder, "GP_OOH/new_22062020.csv")) %>% 
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
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>% 
  filter(between(week_ending, as.Date("2020-05-03"), as.Date("2020-06-21"))) #filter complete weeks (Mon-Sun)

#bind old and new ooh data
ooh <- rbind(ooh_may_onwards, ooh_new, ooh)

# Creating totals for groups
ooh_all <- ooh %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
ooh_sex <- ooh %>% agg_cut(grouper="sex") %>% rename(category = sex)
ooh_dep <- ooh %>% agg_cut(grouper="dep") %>% rename(category = dep)
ooh_age <- ooh %>% agg_cut(grouper="age") %>% rename(category = age)

ooh <- rbind(ooh_all, ooh_sex, ooh_dep, ooh_age)

# Formatting file for shiny app
prepare_final_data(dataset = ooh, filename = "ooh", last_week = "2020-06-21")

###############################################.
## Preparing A&E data ----
###############################################.
#short cut to a&e folder areas
ae_zip_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/A&E/2020-06-18-Extracts/"

# Read A&E data both at HSCP and HB level
ae_data <- rbind(read_csv(unz(paste0(ae_zip_folder,"HSCP-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"),
                              "HSCP.csv")) %>% 
                   janitor::clean_names() %>% 
                   rename(area=hscp_of_residence_code_as_at_arrival_date),
                 read_csv(unz(paste0(ae_zip_folder,"NHSBoard-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"), 
                              "NHS Boards.csv")) %>% 
                   janitor::clean_names() %>% 
                   rename(area=treatment_nhs_board_9_digit_code_as_at_date_of_episode))

# Format data
ae_data <- ae_data %>% 
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

prepare_final_data(ae_data, "ae", last_week = "2020-06-14")

###############################################.
## Preparing NHS24 data ----
###############################################.

# #Read in new nhs24 data as txt file, save as RDS and remove txt file version from directory.
# #Each week this section of code can be uncommented run for the latest weeks data then recommented after txt file deleted
# nhs24 <- (read_tsv(paste0(data_folder,"NHS24/NHS24 Extract 15062020 to 21062020.txt")))
# saveRDS(nhs24, paste0(data_folder,"NHS24/NHS24 Extract 15062020 to 21062020.rds"))
# file.remove(paste0(data_folder,"NHS24/NHS24 Extract 15062020 to 21062020.txt"))

nhs24 <-  rbind(readRDS(paste0(data_folder, "NHS24/NHS24 01Jan2018 to 07Jun2020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 08062020 to 14062020.rds")),
                readRDS(paste0(data_folder, "NHS24/NHS24 Extract 15062020 to 21062020.rds"))) %>%
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
nhs24 <- nhs24 %>%
  mutate(sex = str_to_title(sex)) %>% 
  proper() %>% #convert HB names to correct format
  create_agegroups () %>%
  create_depgroups () 

# Aggregate to weekly data
nhs24 <- nhs24 %>% 
  group_by(hscp, sex, dep, age_grp, week_ending, area_name) %>% 
  summarise(count = sum(count, na.rm = T)) %>% ungroup()

# Aggregate up to get figures for each area type.
nhs24 <- nhs24 %>% mutate(scot = "Scotland") %>% 
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
prepare_final_data(dataset = nhs24, filename = "nhs24", last_week = "2020-06-21")

###############################################.
## Reading SAS data ----
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
sas <- sas %>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age_grp, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>% rename(age = age_grp)

#NEW WEEKLY DATA UPDATE
sas_new <-rbind(read_tsv(paste0(data_folder,"SAS/COVID WIDER IMPACT SAS_11052020to17052020.txt")),
                read_tsv(paste0(data_folder,"SAS/COVID WIDER IMPACT SAS_18052020to25052020.txt")),
                read_tsv(paste0(data_folder,"SAS/COVID WIDER IMPACT SAS_25052020to31052020.txt")),
                read_tsv(paste0(data_folder,"SAS/COVID WIDER IMPACT SAS_01062020to07062020.txt")),
                read_tsv(paste0(data_folder,"SAS/COVID WIDER IMPACT SAS_08062020to14062020.txt"))) %>%
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
sas_new <- sas_new %>% mutate(scot = "Scotland") %>% 
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
sas<- rbind(sas_allsex, sas_sex, sas_dep, sas_age)

# Formatting file for shiny app
prepare_final_data(dataset = sas, filename = "sas", last_week = "2020-06-14")

###############################################.
## Cath labs ----
###############################################.
# Data for cardiovascular app
gj_cath_age <- read_excel(paste0(data_folder, "cath_labs/GJNH_CathLabProcCountsByWeekNo_ForPHS.xlsx"),
                      sheet = "Age") %>% clean_names() %>% 
  mutate(type = "age",
         age_band = recode(age_band, "Gt60" = "60 and over",
                           "Lt60" = "Under 60")) %>% 
  rename(category = age_band)

gj_cath_sex <- read_excel(paste0(data_folder, "cath_labs/GJNH_CathLabProcCountsByWeekNo_ForPHS.xlsx"),
                          sheet = "Sex") %>% clean_names() %>% 
  mutate(type = "sex") %>%  rename(category = gender)

gj_cath_all <- read_excel(paste0(data_folder, "cath_labs/GJNH_CathLabProcCountsByWeekNo_ForPHS.xlsx"),
                          sheet = "No Strata") %>% clean_names() %>% 
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
ae_cardio_codes <- read_xlsx("/conf/PHSCOVID19_Analysis/shiny_input_files/A&E_Cardio/A&E-CardioConditionCodes.xlsx")
saveRDS(ae_cardio_codes, "shiny_app/data/ae_cardio_codes.rds")
rm(ae_cardio_codes)

# Set A&E cardio folder
ae_cardio_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/A&E_Cardio/"

# Read in data, clean names + some simple mutations
ae_cardio <- read_xlsx(paste0(ae_cardio_folder, "2020-06-18-CardioVascular-AttendancesDuringCovid-19.xlsx")) %>% 
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

prepare_final_data(ae_cardio, "ae_cardio", last_week = "2020-06-14")

###############################################.
## Prescribing - Cardiovascular Drugs ----
###############################################.
cardio_drugs <- read_xlsx("/conf/PHSCOVID19_Analysis/shiny_input_files/prescribing data/covid emessage AMS only 20200618.xlsx") %>% 
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

prepare_final_data(cardio_drugs, "cardio_drugs", last_week = "2020-06-14")

###############################################.
## Prepare 6-in-1 scurve data ----
###############################################.

six_alldose <- read_csv(paste0(data_folder,"immunisations/6in1/six_in_one_dashboard20200622.csv"), 
                col_types =list(eligible_start=col_date(format="%m/%d/%Y"),
                                time_period_eligible=col_factor())) %>%
janitor::clean_names()

# Bringing HB names immunisation data contain HB cypher not area name
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher) %>%
  rename(area_name=description) %>%
  mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
         area_type="Health board")

six_alldose <- left_join(six_alldose, hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(eligible_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, exclude, immunisation, eligible_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no)

final_data <<- six_alldose

saveRDS(six_alldose, paste0("shiny_app/data/","six_alldose_data.rds"))

###############################################.
## Prepare 6-in-1 summary table data----
###############################################.

# 6-in-1 at 8 weeks - summary table data
six_datatable <- read_csv(paste0(data_folder,"immunisations/6in1/six in one_1_dashboardtab_20200622.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(six_datatable, paste0("shiny_app/data/","sixinone_datatable.rds"))

# 6-in-1 at dose 2 (usually 12 weeks) - summary table data
six_dose2_datatable <- read_csv(paste0(data_folder,"immunisations/6in1/six in one_2_dashboardtab_20200622.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(six_dose2_datatable, paste0("shiny_app/data/","sixinone_dose2_datatable.rds"))

# 6-in-1 at dose 3 (usually 16 weeks) - summary table data
six_dose3_datatable <- read_csv(paste0(data_folder,"immunisations/6in1/six in one_3_dashboardtab_20200622.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(six_dose3_datatable, paste0("shiny_app/data/","sixinone_dose3_datatable.rds"))


###############################################.
## Prepare MMR data ----
###############################################.

# mmr dose 1 & 2 - scurve data
mmr_alldose <- read_csv(paste0(data_folder,"immunisations/mmr/mmr_dashboard20200622.csv"),
                      col_types =list(eligible_start=col_date(format="%m/%d/%Y"),
                                      time_period_eligible=col_factor())) %>%
  janitor::clean_names()

# Bringing HB names immunisation data contain HB cypher not area name
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher) %>%
  rename(area_name=description) %>%
  mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
         area_type="Health board")

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

# MMR at dose 1  - summary table data
mmr_dose1_datatable <- read_csv(paste0(data_folder,"immunisations/mmr/mmr_dose1_dashboardtab_20200622.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(mmr_dose1_datatable, paste0("shiny_app/data/","mmr_dose1_datatable.rds"))

# MMR at dose 2  - summary table data
mmr_dose2_datatable <- read_csv(paste0(data_folder,"immunisations/mmr/mmr_dose2_dashboardtab_20200622.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(mmr_dose2_datatable, paste0("shiny_app/data/","mmr_dose2_datatable.rds"))

###############################################.
## Prepare Child Health data ----
###############################################.
## First visit - scurve data
first <- read_csv(paste0(data_folder,"child_health/firstvisit_dashboard20200601.csv"), 
                col_types =list(week_2_start=col_date(format="%m/%d/%Y"),
                                time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
first$time_period_eligible <- factor(first$time_period_eligible, 
                                     levels=unique(first$time_period_eligible[order(first$week_2_start, decreasing = T)]), 
                                     ordered=TRUE)

# Bringing HB names immunisation data contain HB cypher not area name
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher) %>%
  rename(area_name=description) %>%
  mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
         area_type="Health board")

first %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_2_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_2_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv<168)

saveRDS(first, paste0("shiny_app/data/","first_visit_data.rds"))

# First visit - summary table data
first_datatable <- read_csv(paste0(data_folder,"child_health/firstvisit_dashboardtab_20200601.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(first_datatable, paste0("shiny_app/data/","first_visit_datatable.rds"))


## 6 to 8 weeks visit - scurve data
sixtoeight <- read_csv(paste0(data_folder,"child_health/sixtoeight_dashboard20200601.csv"), 
                  col_types =list(week_8_start=col_date(format="%m/%d/%Y"),
                                  time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
sixtoeight$time_period_eligible <- factor(sixtoeight$time_period_eligible, 
                                     levels=unique(sixtoeight$time_period_eligible[order(sixtoeight$week_8_start, decreasing = T)]), 
                                     ordered=TRUE)

# Bringing HB names immunisation data contain HB cypher not area name
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher) %>%
  rename(area_name=description) %>%
  mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
         area_type="Health board")

sixtoeight %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_8_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_8_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv<168)

saveRDS(sixtoeight, paste0("shiny_app/data/","six_to_eight_data.rds"))

# 6-8 weeks visit - summary table data
sixtoeight_datatable <- read_csv(paste0(data_folder,"child_health/sixtoeight_dashboardtab_20200601.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(sixtoeight_datatable, paste0("shiny_app/data/","six_to_eight_datatable.rds"))

###############################################.
## Prepare perinatal data ----
###############################################.

perinatal_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/perinatal/"

# P CHART PERINATAL DATA
p_perinatal <- read_csv(paste0(perinatal_folder,"all_p_data.csv")) %>%
  janitor::clean_names() %>%
  rename(date=sample, binomial_stdev_proportion=binomial_st_dev, binomial_stdev_rate=binomial_st_dev_1)

p_perinatal <- p_perinatal %>%
  mutate(area_name="Scotland",
         area_type="Scotland")

p_perinatal <- p_perinatal %>%
  mutate(date = gsub(" ", "0", date),
         date = as.Date(paste0(date,"1"), format="%Y%m%d")) 

saveRDS(p_perinatal, paste0("shiny_app/data/","p_perinatal_data.rds"))

# # perinatal mortality - summary table data - MAY ADD TO TAB
p_perinatal_datatable <- read_csv(paste0(perinatal_folder,"all_p_data.csv")) %>%
  janitor::clean_names() %>%
  rename(date=sample, number=observation, totalbirths=sample_size) %>%
  mutate(area_name="Scotland",
         area_type="Scotland") %>%
  select(date, number, totalbirths, proportion, rate, type)


saveRDS(p_perinatal_datatable, paste0("shiny_app/data/","p_perinatal_datatable.rds"))

# U CHART PERINATAL DATA

u_perinatal <- read_csv(paste0(perinatal_folder,"all_u_data.csv")) %>%
  janitor::clean_names() %>%
  rename(date=sample)

u_perinatal <- u_perinatal %>%
  mutate(area_name="Scotland",
         area_type="Scotland")

u_perinatal <- u_perinatal %>%
  mutate(date = gsub(" ", "0", date),
         date = as.Date(paste0(date,"1"), format="%Y%m%d")) 

saveRDS(u_perinatal, paste0("shiny_app/data/","u_perinatal_data.rds"))

# # perinatal mortality - summary table data - MAY ADD TO TAB
u_perinatal_datatable <- read_csv(paste0(perinatal_folder,"all_u_data.csv")) %>%
  janitor::clean_names() %>%
  rename(date=sample, stillbirths=observation, totalbirths=sample_size) %>%
  mutate(area_name="Scotland",
         area_type="Scotland") %>%
  select(date, stillbirths, totalbirths, proportion, rate, type)

saveRDS(u_perinatal_datatable, paste0("shiny_app/data/","u_perinatal_datatable.rds"))

# MAY NEED LATER FOR HB DATA
# Bringing HB names 
# hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
#   janitor::clean_names() %>% select(description, hb_cypher) %>%
#   rename(area_name=description) %>%
#   mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
#          area_type="Health board")


##END

