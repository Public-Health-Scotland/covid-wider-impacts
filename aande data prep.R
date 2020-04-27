# A&E Attendance data preparation for wider impact covid app

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(janitor)
library(phsmethods)
library(lubridate)

###############################################.
## Functions ----
###############################################.

#format age groups
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

#format sex groups
create_sexgroups <- function(dataset) {
  dataset %>% mutate(sex=case_when(is.na(sex)~"Missing", sex==1 ~ "Male", sex==2 ~"Female", TRUE~as.character(sex)))
}

#format deprivation groups
create_depgroups <- function(dataset) {
  dataset %>% mutate(dep=case_when(is.na(dep)~"Missing", dep==1 ~ "1 - most deprived", dep==5 ~"5 - Least deprived", TRUE~as.character(dep)))
}

# Speed up aggregations of different data cuts
aggregation_help <- function(grouper) {
  ae_all %>%
    group_by_at(c("week_ending","area_name", "area_type", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup() %>% 
    mutate(type = grouper) 
}


###############################################.
## Reading in A&E data ----
###############################################.

# prepare a&e data (HBT level)

ae_board_data<- read.csv(unz("/conf/PHSCOVID19_Analysis/UCD/A&E/2020-04-24-Extracts/NHSBoard-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip", "NHS Boards.csv"), header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>% 
  as.data.frame() %>%
  rename(area=treatment_nhs_board_9_digit_code_as_at_date_of_episode,dep=prompt_dataset_deprivation_scot_quintile,age=pat_age,
         sex=pat_gender_code,count=number_of_attendances)

#format data
ae_board_data <- ae_board_data %>%
  mutate(area_name = match_area(area),
         area_type="Health board") %>%
  create_agegroups() %>%
  create_sexgroups() %>%
  create_depgroups() %>%
  group_by(week_ending, area_name, area_type, age_grp, sex, dep) %>%
  summarise(count=sum(count)) %>%
  ungroup()

#generate scotland level dataset
ae_scot_data <- ae_board_data %>%
  group_by(week_ending, age_grp, sex, dep) %>%
  summarise(count=sum(count)) %>%
  mutate(area_name="Scotland",
         area_type="Scotland") %>%
  ungroup()

# prepare a&e data (HSCP of residence)
ae_hscp_data<- read.csv(unz("/conf/PHSCOVID19_Analysis/UCD/A&E/2020-04-24-Extracts/HSCP-ED-Attendances-SIMD-AgeBand-COVID-19-Publication.zip", "HSCP.csv"), header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>% 
  as.data.frame() %>%
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

#add scotland, hscp and nhs board data files
ae_all_geos <- rbind(ae_board_data, ae_hscp_data, ae_scot_data)

#Add an 'all' sex category to data files so possible to present male, female and all.
ae_all_sex <- ae_all_geos %>%
  group_by(week_ending, area_name, area_type, age_grp, dep) %>%
  summarise(count = sum(count)) %>% ungroup() %>% 
  mutate(sex = "All") %>%
  ungroup()

# Add the all sex age group to other a&e data
ae_all <- rbind(ae_all_geos, ae_all_sex) %>%
  rename(age=age_grp)

##Reshape dataset for shiny app
#Use aggregation function to aggregate data files into format

ae_sex <- aggregation_help(grouper="sex") %>%
  rename(category=sex)
  
ae_dep <- aggregation_help(grouper="dep") %>%
  rename(category=dep)

ae_age <- aggregation_help(grouper="age") %>%
  rename(category=age)

# Add final aggregation files to one master file
ae_final_data <- rbind(ae_sex, ae_dep, ae_age)

#derive a week number from week_ending column to ease presentation of data
ae_final_data <- ae_final_data %>%
  mutate(date=as.Date(week_ending,format="%d/%m/%Y"),
         week_no=isoweek(date),
         year=year(date))

#find average number of attendances by week and category
ae_average <- ae_final_data %>%
  subset(year %in% c("2018","2019")) %>%
  group_by(area_name,area_type,category,type,week_no) %>%
  summarise(count_average=mean(count)) %>%
  ungroup()

#filter for latest year - 2020  (maybe need to reextract data )
ae_latest_year <- ae_final_data %>%
  subset(year=="2020")

#join latest year of data with averages from previous years
ae_shiny <- left_join(ae_average,ae_latest_year,by = c("area_name", "area_type", "category","type","week_no"))

# Temporary for testing purposes: supressing numbers under 5
ae_shiny$count <- ifelse(ae_shiny$count<5,0,ae_shiny$count)

# Remove weeks that haven't happened yet & reformat NHS board names to include prefix/&
ae_shiny <- ae_shiny %>%
  subset(week_no<17) %>%
  mutate(area_name1 = case_when(area_type=="NHS board" ~ (paste0("NHS ",gsub(" and ", " & ", area_name))), TRUE~area_name)) %>%
  select(-area_name) %>%
  rename(area_name=area_name1) %>% 
  # Creating % variation from pre_covid to covid
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

#save output for shiny app
saveRDS(ae_shiny, "shiny_app/data/ae_data.rds")

ae_shiny <- readRDS("shiny_app/data/ae_data.rds")

