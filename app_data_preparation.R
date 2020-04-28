# Data preparation for app

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(janitor)
library(lubridate)
library(zoo)
library(readr)
library(stringr)
library(phsmethods)
#library(readxl)


###############################################.
## Functions ----
###############################################.
# This function aggregates data for each different cut requires
agg_rapid <- function(extra_vars = NULL, split, specialty = F) {
  
  agg_helper <- function(more_vars, type_chosen = split) {
    rap_adm %>% 
      group_by_at(c("date_adm", more_vars)) %>% 
      summarise(count = sum(count)) %>% ungroup() %>% 
      mutate(type = type_chosen) 
  }
  
  # Aggregating to obtain totals for each split type and then putting all back together.
  adm_type_scot <- agg_helper(c(extra_vars, "admission_type")) %>% 
    mutate(area_name = "Scotland", spec = "All") 
  
  all_scot <- agg_helper(extra_vars) %>% 
    mutate(admission_type = "All", area_name = "Scotland", spec = "All") 

  adm_type_hb <- agg_helper(c(extra_vars, "admission_type", "hb")) %>% 
    mutate(spec = "All") %>% rename(area_name = hb)

  all_hb <- agg_helper(c(extra_vars, "hb")) %>% 
    mutate(admission_type = "All", spec = "All") %>% rename(area_name = hb)
  
  adm_type_hscp <- agg_helper(c(extra_vars, "admission_type", "hscp_name")) %>% 
    mutate(spec = "All") %>% rename(area_name = hscp_name)
  
  all_hscp <- agg_helper(c(extra_vars, "hscp_name")) %>% 
    mutate(admission_type = "All", spec = "All") %>% rename(area_name = hscp_name)


  if (specialty == T) {
    spec_scot <- agg_helper(c(extra_vars, "spec")) %>% 
      mutate(area_name = "Scotland", admission_type = "All") 

    spec_hscp <- agg_helper(c(extra_vars, "spec", "hscp_name")) %>% 
      mutate(admission_type = "All") %>% rename(area_name = hscp_name)

    spec_hb <- agg_helper(c(extra_vars, "spec", "hb")) %>% 
      mutate(admission_type = "All") %>% rename(area_name = hb)
    
    rbind(all_scot, spec_scot, adm_type_scot, all_hb, spec_hb, adm_type_hb,
          all_hscp, spec_hscp, adm_type_hscp)
  } else {
    rbind(all_scot, adm_type_scot, all_hb, adm_type_hb, all_hscp, adm_type_hscp)
  }
}


# Speed up aggregations of different data cuts (A&E,NHS24,OOH)
agg_cut <- function(dataset, grouper) {
  dataset %>%
    group_by_at(c("week_ending","area_name", "area_type", "date", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup() %>% 
    mutate(type = grouper) 
}


# Create age groups
create_agegroups <- function(dataset) {
  dataset %>% mutate(age_grp1 = as.character(case_when(between(age, 0, 4) ~ " Under 5",
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
  dataset %>% mutate(sex=case_when(is.na(sex)~"Missing", sex==1 ~ "Male", sex==2 ~"Female", TRUE~as.character(sex)))
}

# Format deprivation groups
create_depgroups <- function(dataset) {
  dataset %>% mutate(dep=case_when(is.na(dep)~"Missing", dep==1 ~ "1 - most deprived", dep==5 ~"5 - Least deprived", TRUE~as.character(dep)))
}

# Convert HB names to correct format
proper <- function(dataset) {
  dataset %>% 
    mutate(hb1= str_to_title(hb),
           area_name=paste0(toupper(substr(hb1, 1, 3)),substring(hb1, 4))) %>%
    select(-hb1, -hb)
}


###############################################.
## Reading RAPID data ----
###############################################.
# Prepare by Unscheduled care team
rap_adm <- readRDS("/conf/PHSCOVID19_Analysis/Admissions_by_category_24_Apr.rds") %>% 
  janitor::clean_names() %>% 
  # taking out aggregated values, not clear right now
  filter(!(substr(hosp,3,5) == "All" | (substr(hscp_name,3,5) == "All"))) 

# Bringing HB names
hb_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Health_Board_Identifiers.rds") %>% 
  janitor::clean_names() %>% select(description, hb_cypher)

rap_adm <- left_join(rap_adm, hb_lookup, by = c("hb" = "hb_cypher")) %>% 
  select(-hb) %>% rename(hb = description)

# Bringing spec names
spec_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/Specialty_Groupings.rds") %>% 
  janitor::clean_names() %>% select(grouping, speccode)

rap_adm <- left_join(rap_adm, spec_lookup, by = c("spec" = "speccode")) %>% 
  select(-spec) %>% rename(spec = grouping)

# Aggregating to obtain totals for each split type and then putting all back together
# Totals for overalls for all pop including totals by specialty too
rap_adm_all <- agg_rapid(NULL, split = "sex", specialty = T) %>% 
  mutate(category = "All") 

# Totals for overalls for all sexes
rap_adm_sex <- agg_rapid(c("sex"), split = "sex") %>% 
  mutate( category = recode(sex, "male" = "Male", "female" = "Female")) %>% 
  select(-sex)

# Totals for overalls for all age groups
rap_adm_age <- agg_rapid(c("age_group"), split = "age") %>% 
  filter(age_group != "missing") %>% 
  mutate(category = recode_factor(age_group, "Under_5" = "Under 5", "5_thru_14" = "5 - 14", 
                                  "15_thru_44" = "15 - 44", "45_thru_64" = "45 - 64",
                                  "65_thru_74" = "65 -74", "75_thru_84" = "75 -84",
                                  "85+" = "85 and over")) %>% 
  select(-age_group)

# Totals for overalls for deprivation quintiles
rap_adm_depr <- agg_rapid(c("simd_quintile"), split = "dep") %>% 
  rename(category = simd_quintile) %>% 
  mutate(category = as.character(category),
         category = recode(category, "1" = "1 - most deprived", "5" = "5 - Least deprived"))
  
rap_adm <- rbind(rap_adm_all, rap_adm_depr, rap_adm_sex, rap_adm_age) %>% 
  # Filtering cases without information on age, sex or deprivation (still counted in all)
  filter(!(is.na(category) | area_name == "" |
             area_name %in% c("ENGLAND/WALES/NORTHERN IRELAND", "UNKNOWN HSCP - SCOTLAND"))) %>% 
  rename(date = date_adm) %>% 
  # Creating area type variable
  mutate(area_type = case_when(substr(area_name, 1,3) == "NHS" ~ "Health board",
                               area_name == "Scotland" ~ "Scotland",
                               TRUE ~ "HSC partnership"),
         admission_type = recode(admission_type, "elective" = "Planned", "emergency" = "Emergency"))

# Preparing data by week for publication
rap_adm <- rap_adm %>% 
  mutate(week_ending = ceiling_date(date, "week")) %>% #end of week
  group_by(category, type, admission_type, spec, area_name, area_type, week_ending) %>% 
  summarise(count = sum(count)) %>% 
  ungroup()

saveRDS(rap_adm, "data/rapid_data_pub.rds")

rap_adm <- rap_adm %>%
  # Creating week number to be able to compare pre-covid to covid period
  mutate(week_no = isoweek(week_ending))

# Creating average admissions of pre-covid data by day of the year
rap_adm_old <- rap_adm %>% filter(week_ending<as.Date("2020-01-01")) %>% 
  group_by(category, type, admission_type, spec, area_name, week_no) %>% 
  summarise(count_average = round(mean(count, na.rm = T), 1)) 

# Joining with 2020 data
rap_adm_2020 <- left_join(rap_adm %>% filter(between(week_ending, as.Date("2020-01-01"), as.Date("2020-04-20"))), 
                          rap_adm_old, 
                          by = c("category", "type", "admission_type", "spec", "area_name", "week_no")) %>% 
  rename(date = week_ending) %>% 
  # Creating %variation from precovid to covid period
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

# Temporary for testing purposes: supressing numbers under 5
rap_adm_2020$count <- ifelse(rap_adm_2020$count<5,0,rap_adm_2020$count)

saveRDS(rap_adm_2020, "shiny_app/data/rapid_data.rds")

###############################################.
## Preparing OOH data ----
###############################################.
ooh <- read_csv("/conf/PHSCOVID19_Analysis/OOH_shiny_app/OOH Weekly Demand_Scot+HBs.csv") %>% 
  janitor::clean_names() %>% 
  # Recoding variables to match other datasets
  mutate(age_group = recode_factor(age_group, "0-4" = "Under 5", "5-14" = "5 - 14",  
                                   "15-24" = "15 - 44", "25-44" = "15 - 44", "45-64" = "45 - 64",
                                    "65-74" = "65 -74", "75-84" = "75 -84",
                                    "85 plus" = "85 and over"),
         sex = recode(gender, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         dep = recode(prompt_dataset_deprivation_scot_quintile, 
                      "1" = "1 - most deprived", "2" = "2",  "3" = "3", 
                      "4" = "4", "5" = "5 - Least deprived"),
         week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         date = week_ending) %>% 
  # Selecting consultations for app
  select(date, week_ending, sex, dep, age = age_group, count = number_of_consultations, 
         area_name = treatment_nhs_board_name) %>% 
  #renaming Health boards to match standard used in rest of datasets
  mutate(area_name = paste0(toupper(substr(area_name, 1, 3)), substring(str_to_title(area_name), 4)),
         area_type = "Health board") %>% 
  # Aggregating to make it faster to work with
  group_by(date, week_ending, sex, dep, age, area_type, area_name) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup()

# Creating Scotland totals and joining with hb data
ooh_scot <- ooh %>% group_by(date, week_ending, sex, dep, age) %>% 
  summarise(count = sum(count, na.rm = T)) %>% 
  mutate(area_name = "Scotland", area_type = "Scotland") %>% ungroup()

ooh <- rbind(ooh, ooh_scot)

# Creating totals for groups
ooh_all <- ooh %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
ooh_sex <- ooh %>% agg_cut(grouper="sex") %>% rename(category = sex)
ooh_dep <- ooh %>% agg_cut(grouper="dep") %>% rename(category = dep)
ooh_age <- ooh %>% agg_cut(grouper="age") %>% rename(category = age)
 
ooh <- rbind(ooh_all, ooh_sex, ooh_dep, ooh_age) %>% 
  # Creating week number to be able to compare pre-covid to covid period
  mutate(week_no = isoweek(week_ending))

# Creating average admissions of pre-covid data by day of the year
ooh_old <- ooh %>% filter(week_ending<as.Date("2020-01-01")) %>% 
  group_by(category, type, area_name, week_no) %>% 
  summarise(count_average = round(mean(count, na.rm = T), 1)) 

# Joining with 2020 data
ooh_2020 <- left_join(ooh %>% filter(between(week_ending, as.Date("2020-01-01"), as.Date("2020-04-20"))), 
                          ooh_old, 
                          by = c("category", "type", "area_name", "week_no")) %>% 
  # filtering empty cases
  filter(!(is.na(category))) %>% 
  # Creating % variation from pre_covid to covid
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

# Temporary for testing purposes: supressing numbers under 5
ooh_2020$count <- ifelse(ooh_2020$count<5,0,ooh_2020$count)

saveRDS(ooh_2020, "shiny_app/data/ooh_data.rds")




###############################################.
## Preparing A&E data ----
###############################################.

#short cut to a&e folder area
ae_zip_folder <- "/conf/PHSCOVID19_Analysis/UCD/A&E/2020-04-24-Extracts/"

# Read a&e data file (HBT level)
ae_board_data<- read.csv(unz(paste0(ae_zip_folder,"NHSBoard-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"), "NHS Boards.csv"),
                         header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>% 
  janitor::clean_names() %>%  as.data.frame() %>%
  rename(area=treatment_nhs_board_9_digit_code_as_at_date_of_episode,dep=prompt_dataset_deprivation_scot_quintile,age=pat_age,
         sex=pat_gender_code,count=number_of_attendances)

# Format data
ae_board_data <- ae_board_data %>%
  mutate(area_name = match_area(area), #use PHS methods package to add area names
         area_type="Health board") %>%
  create_agegroups() %>%
  create_sexgroups() %>%
  create_depgroups() %>%
  group_by(week_ending, area_name, area_type, age_grp, sex, dep) %>%
  summarise(count=sum(count)) %>%
  ungroup()

# Generate scotland level dataset
ae_scot_data <- ae_board_data %>%
  group_by(week_ending, age_grp, sex, dep) %>%
  summarise(count=sum(count)) %>%
  mutate(area_name="Scotland",
         area_type="Scotland") %>%
  ungroup()

# Read a&e data (HSCP of residence)
ae_hscp_data<- read.csv(unz(paste0(ae_zip_folder,"HSCP-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"), "HSCP.csv"), 
                             header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>% as.data.frame() %>%
  rename(area=hscp_of_residence_code_as_at_arrival_date,dep=prompt_dataset_deprivation_scot_quintile,age=pat_age,
         sex=pat_gender_code,count=number_of_attendances)

ae_hscp_data <- ae_hscp_data %>%
  mutate(area_name = match_area(area),
         area_type="HSC partnership") %>%
  create_agegroups() %>%
  create_sexgroups() %>%
  create_depgroups() %>%
  group_by(week_ending, area_name, area_type, age_grp, sex, dep) %>%
  summarise(count=sum(count)) %>%
  ungroup()

# Add scotland, hscp and nhs board data files
ae_all_geos <- rbind(ae_board_data, ae_hscp_data, ae_scot_data)

# Tidy files not needed
rm(ae_board_data, ae_hscp_data, ae_scot_data, ae_zip_folder)

#Add an 'all' sex category to data files so possible to present male, female and all.
ae_all_sex <- ae_all_geos %>%
  group_by(week_ending, area_name, area_type, age_grp, dep) %>%
  summarise(count = sum(count)) %>% ungroup() %>% 
  mutate(sex = "All") %>%
  ungroup()

# Add the all sex age group to other a&e data
ae_all <- rbind(ae_all_geos, ae_all_sex) %>%
  rename(age=age_grp) %>%  mutate(date=as.Date(week_ending,format="%d/%m/%Y"))

##Reshape dataset for shiny app
#Use aggregation function to aggregate data files into format

ae_sex <- agg_cut(dataset=ae_all, grouper="sex") %>% rename(category=sex)
ae_dep <- agg_cut(dataset=ae_all, grouper="dep") %>% rename(category=dep)
ae_age <- agg_cut(dataset=ae_all, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
ae_final_data <- rbind(ae_sex, ae_dep, ae_age)

# Tidy files not needed
rm(ae_sex, ae_dep, ae_age, ae_all_sex, ae_all_geos, ae_all)

# Derive a week number from week_ending column to ease presentation of data
ae_final_data <- ae_final_data %>%
  mutate(week_no=isoweek(date),year=year(date))

# Calculate average number of attendances by week and category
ae_average <- ae_final_data %>%
  subset(year %in% c("2018","2019")) %>%
  group_by(area_name,area_type,category,type,week_no) %>%
  summarise(count_average=mean(count)) %>%
  ungroup()

# Filter for latest year - 2020
ae_latest_year <- ae_final_data %>%
  subset(year=="2020")

#join latest year of data with averages from previous years
ae_shiny <- left_join(ae_average,ae_latest_year,by = c("area_name", "area_type", "category","type","week_no"))

# Temporary for testing purposes: supressing numbers under 5
ae_shiny$count <- ifelse(ae_shiny$count<5,0,ae_shiny$count)

# Remove weeks that haven't happened yet & reformat NHS board names to include prefix/&
ae_shiny <- ae_shiny %>%
  filter(category != "Missing") %>% #taking out empty counts
  subset(week_no<17) %>%
  mutate(area_name1 = case_when(area_type=="NHS board" ~ (paste0("NHS ",gsub(" and ", " & ", area_name))), TRUE~area_name)) %>%
  select(-area_name) %>%
  rename(area_name=area_name1) %>% 
  # Creating % variation from pre_covid to covid
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

#save output for shiny app
saveRDS(ae_shiny, "shiny_app/data/ae_data.rds")

# Tidy
rm(ae_average, ae_latest_year, ae_final_data)

###############################################.
## Preparing NHS24 data ----
###############################################.

nhs24_zip_folder <- "/conf/PHSCOVID19_Analysis/UCD/NHS 24 SAS GPOOH reporting/06 Publications/3. Vicky Elliott - NHS24/Zipped/"

## Reading in NHS24 data
nhs24_jantojun18 <- read.csv(unz(paste0(nhs24_zip_folder, "0. NHS24 Extract 1 Jan 18 - 30 Jun 18.zip"), "0. NHS24 Extract 1 Jan 18 - 30 Jun 18.csv"), 
                             header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>% as.data.frame() 

nhs24_jultodec18 <- read.csv(unz(paste0(nhs24_zip_folder, "0a. NHS24 Extract 1 Jul 18 - 31 Dec 18.zip"), "0a. NHS24 Extract 1 Jul 18 - 31 Dec 18.csv"), 
                             header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>% as.data.frame() 

nhs24_jantojun19 <- read.csv(unz(paste0(nhs24_zip_folder, "1. NHS24 Extract 1 Jan 19 - 30 Jun 19.zip"), "1. NHS24 Extract 1 Jan 19 - 30 Jun 19.csv"), 
                             header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>% as.data.frame() 

nhs24_jultodec19 <- read.csv(unz(paste0(nhs24_zip_folder, "2. NHS24 Extract 1 Jul 19 - 31 Dec 19.zip"), "2. NHS24 Extract 1 Jul 19 - 31 Dec 19.csv"), 
                             header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>% as.data.frame() 

nhs24_jantoapr20 <- read.csv(unz(paste0(nhs24_zip_folder,"3. NHS24 Extract 1 Jan 20 - 24 Apr 20.zip"), "3. NHS24 Extract 1 Jan 20 - 24 Apr 20.csv"), 
                             header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>% as.data.frame() 

# Add data files
nhs24 <-  rbind(nhs24_jantojun18,nhs24_jultodec18,nhs24_jantojun19,nhs24_jultodec19,nhs24_jantoapr20) %>% 
  rename(hb=patient_nhs_board_description_current,
         hscp=nhs_24_patient_hscp_name_current,
         sex=gender_description,
         dep=nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
         covid_flag=nhs_24_covid_19_flag,
         date=nhs_24_call_rcvd_date,
         count=number_of_nhs_24_records)
# Tidy
rm(nhs24_jantojun18,nhs24_jultodec18,nhs24_jantojun19,nhs24_jultodec19,nhs24_jantoapr20, nhs24_zip_folder)

nhs24 <- nhs24 %>%
  proper() %>% #convert HB names to correct format
  mutate(sex=str_to_title(sex),
         date2=as.Date(date,format="%d-%b-%y"),
         week_ending = ceiling_date(date2, "week")) %>% #end of week) 
  create_agegroups () %>%
  create_depgroups ()

#Create Scotland file
scot_nhs24 <- nhs24 %>%
  group_by(week_ending, sex, dep, age_grp) %>%
  summarise(count=sum(count)) %>%
  mutate(area_name="Scotland",
         area_type="Scotland") %>%
  ungroup()

#Create NHS Board file
board_nhs24 <- nhs24 %>%
  group_by(week_ending, area_name, sex, dep, age_grp) %>%
  summarise(count=sum(count)) %>%
  mutate(area_type="Health board") %>%
  ungroup()

#Create hscp file
hscp_nhs24 <- nhs24 %>%
  group_by(week_ending, hscp, sex, dep, age_grp) %>%
  summarise(count=sum(count)) %>%
  mutate(area_type="HSC partnership") %>%
  rename(area_name=hscp) %>%
  ungroup()

#add scotland, hscp and nhs board data files
nhs24_all_geos <- rbind(hscp_nhs24,board_nhs24,scot_nhs24)

#create all (sex group)
nhs24_allsex <- nhs24_all_geos %>%
  group_by(week_ending, area_name, area_type, age_grp, dep) %>%
  summarise(count = sum(count)) %>% ungroup() %>% 
  mutate(sex = "All") %>%
  ungroup()

# Add the all sex age group to other a&e data
nhs24_all <- rbind(nhs24_all_geos, nhs24_allsex) %>%
  rename(age=age_grp) %>%
  mutate(date=as.Date(week_ending,format="%d-%m-%Y"))

# Use aggregation function to aggregate data files for use in shiny app
nhs24_sex <- agg_cut(dataset= nhs24_all, grouper="sex") %>% rename(category=sex)
nhs24_dep <- agg_cut(dataset= nhs24_all, grouper="dep") %>% rename(category=dep)
nhs24_age <- agg_cut(dataset= nhs24_all, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
nhs24_final_data <- rbind(nhs24_sex, nhs24_dep, nhs24_age)

# Tidy
rm(nhs24_age,nhs24_dep,nhs24_sex,nhs24_all_geos, board_nhs24,scot_nhs24,hscp_nhs24, nhs24_allsex, nhs24_all)

# Derive a week number from week_ending column to ease presentation of data
nhs24_final_data <- nhs24_final_data %>%
  mutate(week_no=isoweek(date), year=year(date))

# Find average number of attendances by week and category
nhs24_average <- nhs24_final_data %>%
  subset(year %in% c("2018","2019")) %>%
  group_by(area_name,area_type,category,type,week_no) %>%
  summarise(count_average=mean(count)) %>% ungroup()

# Filter for latest year - 2020 
nhs24_latest_year <- nhs24_final_data %>% subset(year=="2020")

# Join latest year of data with averages from previous years
nhs24_shiny <- left_join(nhs24_average,nhs24_latest_year,by = c("area_name", "area_type", "category","type","week_no"))

# Temporary for testing purposes: supressing numbers under 5
nhs24_shiny$count <- ifelse(nhs24_shiny$count<5,0,nhs24_shiny$count)

# Remove weeks that haven't happened yet & reformat NHS board names to include prefix/&
nhs24_shiny <- nhs24_shiny %>%
  filter(!(category %in% c("Missing", "Not Known"))) %>% #taking out empty counts
  subset(week_no<17) %>%
  # Creating % variation from pre_covid to covid
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

#save output for shiny app
saveRDS(nhs24_shiny, "shiny_app/data/nhs24_data.rds")

# Tidy
rm(nhs24_average, nhs24_latest_year, nhs24_final_data)




##END