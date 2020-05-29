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

data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/" # folder for files

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
}

###############################################.
## Reading RAPID data ----
###############################################.
# Prepared by Unscheduled care team
rap_adm <- readRDS("/conf/PHSCOVID19_Analysis/shiny_input_files/rapid/Admissions_by_category_18-May.rds") %>% 
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

prepare_final_data(rap_adm, "rapid", last_week = "2020-05-10", 
                   extra_vars = c("admission_type", "spec"))

###############################################.
## Preparing OOH data ----
###############################################.
# Read in historic OOH file
ooh <- read_csv(unzip("/conf/PHSCOVID19_Analysis/shiny_input_files/GP_OOH/OOH DATA 2018 - 22032020.zip","OOH DATA 2018 - 22032020.csv")) %>%
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
ooh_new <- read_csv(unzip("/conf/PHSCOVID19_Analysis/shiny_input_files/GP_OOH/COVID DASHBOARD EXTRACT_2203to0505.zip","COVID DASHBOARD EXTRACT_2203to0505.csv")) %>%
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

#new data extract from week ending 03 may 2020 up to week ending 17 may 2020
new_ooh_03_17may2020 <- read_csv("/conf/PHSCOVID19_Analysis/shiny_input_files/GP_OOH/new_18052020.csv") %>% 
  janitor::clean_names() %>%
  rename(count=number_of_cases, hscp=hscp_of_residence_name_current, age_group=age_band,
         hb=treatment_nhs_board_name, sex=gender, dep=prompt_dataset_deprivation_scot_quintile) %>%
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
         week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         scot = "Scotland") %>% 
  proper() # convert HB names to correct format

new_ooh_03_17may2020 <- new_ooh_03_17may2020 %>% 
  gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup() %>% 
  filter(between(week_ending, as.Date("2020-05-03"), as.Date("2020-05-17"))) #filter complete weeks (Mon-Sun)


#bind old and new ooh data
ooh <- rbind(new_ooh_03_17may2020, ooh_new, ooh)

# Creating totals for groups
ooh_all <- ooh %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
ooh_sex <- ooh %>% agg_cut(grouper="sex") %>% rename(category = sex)
ooh_dep <- ooh %>% agg_cut(grouper="dep") %>% rename(category = dep)
ooh_age <- ooh %>% agg_cut(grouper="age") %>% rename(category = age)

ooh <- rbind(ooh_all, ooh_sex, ooh_dep, ooh_age)

# Formatting file for shiny app
prepare_final_data(dataset = ooh, filename = "ooh", last_week = "2020-05-17")

###############################################.
## Preparing A&E data ----
###############################################.
#short cut to a&e folder areas
ae_zip_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/A&E/2020-05-14-Extracts/"

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

prepare_final_data(ae_data, "ae", last_week = "2020-05-10")

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
prepare_final_data(dataset = nhs24, filename = "nhs24", last_week = "2020-05-17")


###############################################.
## Reading SAS data ----
###############################################.

sas_zip_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/SAS/"

sas <- rbind(read_tsv(unzip(paste0(sas_zip_folder,"COVID WIDER IMPACT SAS_01012018to03052020.zip"),
                            "COVID WIDER IMPACT SAS_01012018to03052020.txt")),
             read_tsv(paste0(sas_zip_folder, "COVID WIDER IMPACT SAS_04052020to10052020.txt"))) %>%
  
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

# Use aggregation function to aggregate data files for use in shiny app
sas_allsex <- sas %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
sas_sex <- agg_cut(dataset= sas, grouper="sex") %>% rename(category=sex)
sas_dep <- agg_cut(dataset= sas, grouper="dep") %>% rename(category=dep)
sas_age <- agg_cut(dataset= sas, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
sas<- rbind(sas_allsex, sas_sex, sas_dep, sas_age)

# Formatting file for shiny app
prepare_final_data(dataset = sas, filename = "sas", last_week = "2020-05-10")

###############################################.
## Cath labs ----
###############################################.
# Data for cardiovascular app
cath_lab <- bind_rows(read_excel(paste0(data_folder, "cath_labs/CoronaryProcsByWkNo_Urgency.xls"), 
                       sheet = "JK_q001_CoronaryProcsByWkNo+Urg"),
                      read_excel(paste0(data_folder, "cath_labs/CoronaryProcsByWkNo_Urgency.xls"), 
                       sheet = "JK_q002_CoronaryProcsByWkNo+Urg")) %>% 
  clean_names %>% 
  mutate(tot_emerg_urg = Reduce("+", c(.[9], .[11]))) %>% 
  select(week_ending = to_date, week_no, "All" = total, "Planned" = total_elective, "Emergency" = tot_emerg_urg) %>% 
  pivot_longer(c(All:Emergency), names_to = "admission_type", values_to = "count")

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
                               sheet = "Sheet2") %>% clean_names %>% 
  mutate(month_date = as.Date(paste0(year, "-", month, "-", "01")),
         percent_female = round(percent_female*100, 1),
         percent_70 = round(percent_70*100, 1))
  

saveRDS(angio_lab, paste0("shiny_app/data/angio_lab_data.rds"))

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
ae_cardio <- read_xlsx(paste0(ae_cardio_folder, "2020-05-28-CardioVascular-AttendancesDuringCovid-19.xlsx")) %>% 
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

prepare_final_data(ae_cardio, "ae_cardio", last_week = "2020-05-24")

##END