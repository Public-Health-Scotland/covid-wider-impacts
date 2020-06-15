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
rap_adm <- readRDS(paste0(data_folder, "rapid/Admissions_by_category_15-Jun.rds")) %>% 
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

prepare_final_data(rap_adm, "rapid", last_week = "2020-06-07", 
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
ooh_may_onwards <- read.csv(paste0(data_folder, "GP_OOH/new_15062020.csv")) %>% 
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
  filter(between(week_ending, as.Date("2020-05-03"), as.Date("2020-06-14"))) #filter complete weeks (Mon-Sun)

#bind old and new ooh data
ooh <- rbind(ooh_may_onwards, ooh_new, ooh)

# Creating totals for groups
ooh_all <- ooh %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
ooh_sex <- ooh %>% agg_cut(grouper="sex") %>% rename(category = sex)
ooh_dep <- ooh %>% agg_cut(grouper="dep") %>% rename(category = dep)
ooh_age <- ooh %>% agg_cut(grouper="age") %>% rename(category = age)

ooh <- rbind(ooh_all, ooh_sex, ooh_dep, ooh_age)

# Formatting file for shiny app
prepare_final_data(dataset = ooh, filename = "ooh", last_week = "2020-06-14")

###############################################.
## Preparing A&E data ----
###############################################.
#short cut to a&e folder areas
ae_zip_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/A&E/2020-06-11-Extracts/"

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

prepare_final_data(ae_data, "ae", last_week = "2020-06-07")

###############################################.
## Preparing NHS24 data ----
###############################################.

nhs24_zip_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/NHS24/3. Vicky Elliott - NHS24/Zipped/"

## Reading in NHS24 data
nhs24 <- rbind(read_csv(unz(paste0(nhs24_zip_folder, "0. NHS24 Extract 1 Jan 18 - 30 Jun 18.zip"), 
                            "0. NHS24 Extract 1 Jan 18 - 30 Jun 18.csv")),
               read_csv(unz(paste0(nhs24_zip_folder, "0a. NHS24 Extract 1 Jul 18 - 31 Dec 18.zip"), 
                            "0a. NHS24 Extract 1 Jul 18 - 31 Dec 18.csv")),
               read_csv(unz(paste0(nhs24_zip_folder, "1. NHS24 Extract 1 Jan 19 - 30 Jun 19.zip"), 
                            "1. NHS24 Extract 1 Jan 19 - 30 Jun 19.csv")),
               read_csv(unz(paste0(nhs24_zip_folder, "2. NHS24 Extract 1 Jul 19 - 31 Dec 19.zip"), 
                            "2. NHS24 Extract 1 Jul 19 - 31 Dec 19.csv")),
               read_csv(unz(paste0(nhs24_zip_folder,"NHS24 Extract 1 Jan 20 - 19 Apr 20.zip"), 
                            "Report 2.csv")),
               read_csv(unz(paste0(nhs24_zip_folder,"NHS24 Extract 20 Apr 20 - 10 May 20.zip"), 
                          "Report 2.csv")),
               read_csv(unz(paste0(nhs24_zip_folder,"NHS24 covid Extract 11 May 20 - 17 May 20.zip"), 
                            "Report 2.csv")),
               read_csv(unz(paste0(nhs24_zip_folder,"NHS24 Extract 18052020to24052020.zip"), 
                            "Report 2.csv")),
               read_csv(unz(paste0(nhs24_zip_folder,"NHS24 Extract 250520to31052020.zip"), 
                            "Report 2.csv")),
               read_csv(unz(paste0(nhs24_zip_folder,"NHS24 Extract 1 Jun 20 - 7 Jun 20.zip"), 
                            "Report 2.csv")),
               read_csv(unz(paste0(nhs24_zip_folder,"NHS24 Extract 8 Jun 20 - 14 Jun 20.zip"), 
                            "Report 2.csv"))) %>%
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
prepare_final_data(dataset = nhs24, filename = "nhs24", last_week = "2020-06-07")

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
                read_tsv(paste0(data_folder,"SAS/COVID WIDER IMPACT SAS_01062020to07062020.txt"))) %>%
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
prepare_final_data(dataset = sas, filename = "sas", last_week = "2020-06-07")

###############################################.
## Cath labs ----
###############################################.
# Data for cardiovascular app
cath_lab <- bind_rows(read_excel(paste0(data_folder, "cath_labs/CoronaryProcsByWkNo_Urgency.xls"), 
                       sheet = "JK_q001_CoronaryProcsByWkNo+Urg"),
                      read_excel(paste0(data_folder, "cath_labs/CoronaryProcsByWkNo_Urgency.xls"), 
                       sheet = "JK_q002_CoronaryProcsByWkNo+Urg")) %>% 
  clean_names %>% 
  select(week_ending = to_date, week_no, "All" = total, "Planned" = total_elective, "Emergency" = total_emergency, 
         "Urgent" = urgent) %>% 
  pivot_longer(c(All:Urgent), names_to = "admission_type", values_to = "count")

cath_2019 <- cath_lab %>% filter(year(week_ending) %in% c("2019")) %>% 
  rename(count_average = count) %>%  select(-week_ending)

cath_2020 <- full_join(cath_lab %>% filter(year(week_ending) %in% c("2020")), 
                       cath_2019, 
                       by = c("week_no", "admission_type")) %>% 
  select(-week_no) %>% 
  # Create variation
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

saveRDS(cath_2020, paste0("shiny_app/data/cath_lab_data.rds"))

# Data: GJNH Coronary Angios/PCI 
angio_lab <- read_excel(paste0(data_folder, "cath_labs/MonthlyTrendsCorAngioNumbersAgeSex.xls"), 
                               sheet = "Sheet2") %>% clean_names 
  

saveRDS(angio_lab, paste0("shiny_app/data/angio_lab_data.rds"))

###############################################.
## Prepare 6-in-1 dose 1 ----
###############################################.
#immunisation_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/immunisations/6in1/"

# 6-in-1 at 8 weeks - scurve data
six <- read_csv(paste0(data_folder,"immunisations/6in1/six in one_1_dashboard20200525.csv"), 
                col_types =list(week_8_start=col_date(format="%m/%d/%Y"),
                                time_period_eligible=col_factor())) %>%
  janitor::clean_names()

# Bringing HB names immunisation data contain HB cypher not area name
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher) %>%
  rename(area_name=description) %>%
  mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
         area_type="Health board")

six <- left_join(six, hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_8_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, immunisation, week_8_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no)

final_data <<- six

saveRDS(six, paste0("shiny_app/data/","sixinone_data.rds"))

# 6-in-1 at 8 weeks - summary table data
six_datatable <- read_csv(paste0(data_folder,"immunisations/6in1/six in one_1_dashboardtab_20200525.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(six_datatable, paste0("shiny_app/data/","sixinone_datatable.rds"))

###############################################.
## Prepare 6-in-1 dose 2 ----
###############################################.

#immunisation_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/immunisations/6in1/"

# 6-in-1 at dose 2 (usually 12 weeks) - scurve data
six_dose2 <- read_csv(paste0(data_folder,"immunisations/6in1/six_in_one_2_dashboard20200525.csv"), 
                col_types =list(week_12_start=col_date(format="%m/%d/%Y"),
                                time_period_eligible=col_factor())) %>%
  janitor::clean_names()

# Bringing HB names immunisation data contain HB cypher not area name
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher) %>%
  rename(area_name=description) %>%
  mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
         area_type="Health board")

six_dose2 <- left_join(six_dose2, hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_12_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, immunisation, week_12_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no)

saveRDS(six_dose2, paste0("shiny_app/data/","sixinone_dose2_data.rds"))

# 6-in-1 at dose 2 (usually 12 weeks) - summary table data
six_dose2_datatable <- read_csv(paste0(data_folder,"immunisations/6in1/six in one_2_dashboardtab_20200525.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(six_dose2_datatable, paste0("shiny_app/data/","sixinone_dose2_datatable.rds"))


###############################################.
## Prepare 6-in-1 dose 3 ----
###############################################.

#immunisation_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/immunisations/6in1/"

# 6-in-1 at dose 3 (usually 16 weeks) - scurve data
six_dose3 <- read_csv(paste0(data_folder,"immunisations/6in1/six_in_one_3_dashboard20200525.csv"), 
                      col_types =list(week_16_start=col_date(format="%m/%d/%Y"),
                                      time_period_eligible=col_factor())) %>%
  janitor::clean_names()

# Bringing HB names immunisation data contain HB cypher not area name
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher) %>%
  rename(area_name=description) %>%
  mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
         area_type="Health board")

six_dose3 <- left_join(six_dose3, hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_16_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  #rename(week_12_start=week_16_start) %>%
  select (extract_date, immunisation, week_16_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no)

saveRDS(six_dose3, paste0("shiny_app/data/","sixinone_dose3_data.rds"))

# 6-in-1 at dose 3 (usually 16 weeks) - summary table data
six_dose3_datatable <- read_csv(paste0(data_folder,"immunisations/6in1/six in one_3_dashboardtab_20200525.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(six_dose3_datatable, paste0("shiny_app/data/","sixinone_dose3_datatable.rds"))

###############################################.
## Prepare Child Health data ----
###############################################.
child_health_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/child_health/"

# First visit - scurve data
first <- read_csv(paste0(child_health_folder,"firstvisit_dashboard20200601.csv"), 
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
first_datatable <- read_csv(paste0(child_health_folder,"firstvisit_dashboardtab_20200601.csv")) %>%
  janitor::clean_names() %>%
  rename(area_name=geography_name) %>%
  select (-geography) %>%
  mutate(time_period_eligible=as.factor(time_period_eligible))

saveRDS(first_datatable, paste0("shiny_app/data/","first_visit_datatable.rds"))

##END

