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

###############################################.
## Functions ----
###############################################.
# This function aggregates data for each different cut requires
agg_rapid <- function(grouper = NULL, split, specialty = F) {
  
  agg_helper <- function(more_vars, type_chosen = split) {
    rap_adm %>%
      group_by_at(c("week_ending","area_name", "area_type", "date", more_vars)) %>%
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
    group_by_at(c("week_ending","area_name", "area_type", "date", grouper)) %>%
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
                                   dep==5 ~"5 - Least deprived", TRUE~as.character(dep)))
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
  mutate(week_ending = ceiling_date(date_adm, "week")) %>% #end of week
  group_by(hscp_name, hb, admission_type, dep, age, sex, week_ending, spec) %>% 
  summarise(count = sum(count))

# Aggregating for each geo level
rap_adm <- rap_adm %>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, c(hb, hscp_name, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "hb" = "Health board", 
                            "hscp_name" = "HSC partnership", "scot" = "Scotland")) %>%  
  # Empty areas out
  filter(!(area_name %in% c("", "ENGLAND/WALES/NORTHERN IRELAND", "UNKNOWN HSCP - SCOTLAND"))) %>% 
  mutate(date = week_ending)

# Aggregating to obtain totals for each split type and then putting all back together
# Totals for overalls for all pop including totals by specialty too
rap_adm_all <- agg_rapid(NULL, split = "sex", specialty = T) %>% mutate(category = "All") 
rap_adm_sex <- agg_rapid(c("sex"), split = "sex") %>% rename(category = sex)# Totals for overalls for all sexes
rap_adm_age <- agg_rapid(c("age"), split = "age") %>% rename(category = age) %>% # Totals for overalls for all age groups
  filter(category != "missing") 
# Totals for overalls for deprivation quintiles
rap_adm_depr <- agg_rapid(c("dep"), split = "dep") %>% rename(category = dep) %>% 
  filter(category != "Missing") 
  
rap_adm <- rbind(rap_adm_all, rap_adm_depr, rap_adm_sex, rap_adm_age) %>% 
  # Filtering cases without information on age, sex or deprivation (still counted in all)
  filter(!(is.na(category) ))

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
  # Creating %variation from precovid to covid period
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

# Temporary for testing purposes: supressing numbers under 5
rap_adm_2020$count <- ifelse(rap_adm_2020$count<5,0,rap_adm_2020$count)

saveRDS(rap_adm_2020, "shiny_app/data/rapid_data.rds")

###############################################.
## Preparing OOH data ----
###############################################.
# Read in historic OOH file
ooh <- read_csv(unzip("/conf/PHSCOVID19_Analysis/OOH_shiny_app/OOH DATA 2018 - 22032020.zip","OOH DATA 2018 - 22032020.csv")) %>%
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
                      "4" = "4", "5" = "5 - Least deprived"),
         week_ending = as.Date(week_ending, "%d/%m/%Y"), #formatting date
         date = week_ending,
         scot = "Scotland") %>%
  proper() #convert HB names to correct format

# Aggregate up to get figures for each area type.
ooh <- ooh %>% gather(area_type, area_name, c(area_name, hscp, scot)) %>% ungroup() %>% 
  mutate(area_type = recode(area_type, "area_name" = "Health board", 
                            "hscp" = "HSC partnership", "scot" = "Scotland")) %>% 
  # Aggregating to make it faster to work with
  group_by(date, week_ending, sex, dep, age, area_name, area_type) %>% 
  summarise(count = sum(count, na.rm = T))  %>% ungroup()

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
# Filtering weeks with incomplete week too!! Temporary
ooh_2020 <- left_join(ooh %>% filter(between(week_ending, as.Date("2020-01-01"), as.Date("2020-03-22"))), 
                      ooh_old, 
                      by = c("category", "type", "area_name", "week_no")) %>% 
  # filtering empty cases
  filter(!(is.na(category) | is.na(area_name) | area_name %in% c("UNKNOWN HSCP - SCOTLAND" ))) %>% 
  # Creating % variation from pre_covid to covid
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

# Temporary for testing purposes: supressing numbers under 5
ooh_2020$count <- ifelse(ooh_2020$count<5,0,ooh_2020$count)

saveRDS(ooh_2020, "shiny_app/data/ooh_data.rds")
ooh_2020 <- readRDS("shiny_app/data/ooh_data.rds")

###############################################.
## Preparing A&E data ----
###############################################.
#short cut to a&e folder area
ae_zip_folder <- "/conf/PHSCOVID19_Analysis/UCD/A&E/2020-04-24-Extracts/"

# Read A&E data both at HSCP and HB level
ae_data <- rbind(read_csv(unz(paste0(ae_zip_folder,"HSCP-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"), "HSCP.csv")) %>% 
                   janitor::clean_names() %>% 
                   rename(area=hscp_of_residence_code_as_at_arrival_date),
                 read_csv(unz(paste0(ae_zip_folder,"NHSBoard-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip"), "NHS Boards.csv")) %>% 
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
  rename(age=age_grp) %>%  mutate(date=as.Date(week_ending,format="%d/%m/%Y"))

##Reshape dataset for shiny app
#Use aggregation function to aggregate data files into format
ae_all <- ae_data %>% agg_cut(grouper=NULL) %>% mutate(type = "sex", category = "All")
ae_sex <- agg_cut(dataset=ae_data, grouper="sex") %>% rename(category=sex)
ae_dep <- agg_cut(dataset=ae_data, grouper="dep") %>% rename(category=dep)
ae_age <- agg_cut(dataset=ae_data, grouper="age") %>% rename(category=age)

# Add final aggregation files to one master file
ae_data <- rbind(ae_all, ae_sex, ae_dep, ae_age) %>% 
# Derive a week number from week_ending column to ease presentation of data
  mutate(week_no=isoweek(date),year=year(date)) 

# Calculate average number of attendances by week and category
ae_average <- ae_data %>%
  subset(year %in% c("2018","2019")) %>%
  group_by(area_name,area_type,category,type,week_no) %>%
  summarise(count_average=round(mean(count, na.rm = T),1)) %>% ungroup()

# Filter for latest year - 2020
ae_latest_year <- ae_data %>%
  subset(year=="2020")

#join latest year of data with averages from previous years
ae_shiny <- left_join(ae_average,ae_latest_year,
                      by = c("area_name", "area_type", "category","type","week_no"))

# Temporary for testing purposes: supressing numbers under 5
ae_shiny$count <- ifelse(ae_shiny$count<5,0,ae_shiny$count)

# Remove weeks that haven't happened yet & reformat NHS board names to include prefix/&
ae_shiny <- ae_shiny %>%
  filter(category != "Missing") %>% #taking out empty counts
  filter(!(is.na(area_name) | area_name %in% c("UNKNOWN HSCP - SCOTLAND", "ENGLAND/WALES/NORTHERN IRELAND"))) %>% 
  
  subset(week_no<17) %>%
  mutate(area_name = case_when(area_type=="Health board" ~ (paste0("NHS ",gsub(" and ", " & ", area_name))), 
                                TRUE~area_name)) %>%
  # Creating % variation from pre_covid to covid
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

#save output for shiny app
saveRDS(ae_shiny, "shiny_app/data/ae_data.rds")

# Tidy
rm(ae_average, ae_latest_year, ae_data)

###############################################.
## Preparing NHS24 data ----
###############################################.

nhs24_zip_folder <- "/conf/PHSCOVID19_Analysis/UCD/NHS 24 SAS GPOOH reporting/06 Publications/3. Vicky Elliott - NHS24/Zipped/"

## Reading in NHS24 data
nhs24 <- rbind(read_csv(unz(paste0(nhs24_zip_folder, "0. NHS24 Extract 1 Jan 18 - 30 Jun 18.zip"), 
                            "0. NHS24 Extract 1 Jan 18 - 30 Jun 18.csv")),
               read_csv(unz(paste0(nhs24_zip_folder, "0a. NHS24 Extract 1 Jul 18 - 31 Dec 18.zip"), 
                            "0a. NHS24 Extract 1 Jul 18 - 31 Dec 18.csv")),
               read_csv(unz(paste0(nhs24_zip_folder, "1. NHS24 Extract 1 Jan 19 - 30 Jun 19.zip"), 
                            "1. NHS24 Extract 1 Jan 19 - 30 Jun 19.csv")),
               read_csv(unz(paste0(nhs24_zip_folder, "2. NHS24 Extract 1 Jul 19 - 31 Dec 19.zip"), 
                            "2. NHS24 Extract 1 Jul 19 - 31 Dec 19.csv")),
               read_csv(unz(paste0(nhs24_zip_folder,"3. NHS24 Extract 1 Jan 20 - 24 Apr 20.zip"), 
                            "3. NHS24 Extract 1 Jan 20 - 24 Apr 20.csv"))) %>%
  janitor::clean_names() %>% 
  rename(hb=patient_nhs_board_description_current,
         hscp=nhs_24_patient_hscp_name_current,
         sex=gender_description,
         dep=nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
         covid_flag=nhs_24_covid_19_flag,
         date=nhs_24_call_rcvd_date,
         count=number_of_nhs_24_records)

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
  summarise(count_average=round(mean(count, na.rm = T),1)) %>% ungroup()

# Filter for latest year - 2020 
nhs24_latest_year <- nhs24_final_data %>% subset(year=="2020")

# Join latest year of data with averages from previous years
nhs24_shiny <- left_join(nhs24_average,nhs24_latest_year,by = c("area_name", "area_type", "category","type","week_no"))

# Temporary for testing purposes: supressing numbers under 5
nhs24_shiny$count <- ifelse(nhs24_shiny$count<5,0,nhs24_shiny$count)

# Remove weeks that haven't happened yet & reformat NHS board names to include prefix/&
nhs24_shiny <- nhs24_shiny %>%
  filter(!(category %in% c("Missing", "Not Known"))) %>% #taking out empty counts
  filter(!(is.na(area_name) | area_name %in% c("UNKNOWN HSCP - SCOTLAND", "ENGLAND/WALES/NORTHERN IRELAND"))) %>% 
  subset(week_no<17) %>%
  # Creating % variation from pre_covid to covid
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

#save output for shiny app
saveRDS(nhs24_shiny, "shiny_app/data/nhs24_data.rds")

# Tidy
rm(nhs24_average, nhs24_latest_year, nhs24_final_data)


##END