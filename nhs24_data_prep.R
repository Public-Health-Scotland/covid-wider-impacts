# NHS24 data preparation for wider impact covid app

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(janitor)
library(lubridate)
library(readxl)
library(stringr)


###############################################.
## Functions ----
###############################################.

#Convert HB names to correct format
proper <- function(dataset)
  dataset %>% 
  mutate(hb1= str_to_title(hb),
         area_name=paste0(toupper(substr(hb1, 1, 3)),substring(hb1, 4))) %>%
  select(-hb1, -hb)


#format age groups
create_agegroups <- function(dataset) {
  dataset %>% mutate(age_grp1 = as.character(case_when(between(age, 0, 4) ~ " Under 5",
                                                       between(age, 5, 14) ~ "5 - 14",
                                                       between(age, 15, 44) ~ "15 - 44", 
                                                       between(age, 45, 64) ~ "45 - 64", 
                                                       between(age, 65, 74) ~ "65 - 74", 
                                                       between(age, 75, 84) ~ "75 - 84",
                                                       between(age, 85, 200) ~ "85 and over")),
                     age_grp=case_when(is.na(age) ~"Missing", TRUE~age_grp1)) %>%
    select(-age_grp1)
}


#format deprivation groups
create_depgroups <- function(dataset) {
  dataset %>% mutate(dep=case_when(is.na(dep)~"Missing", dep==1 ~ "1 - most deprived", dep==5 ~"5 - Least deprived", TRUE~as.character(dep)))
}

# Speed up aggregations of different data cuts
aggregation_help <- function(grouper) {
  nhs24_all %>%
    group_by_at(c("week_ending","area_name", "area_type", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup() %>% 
    mutate(type = grouper) 
}

###############################################.
## Reading in A&E data ----
###############################################.

# prepare a&e data (HBT level)

nhs24_jantojun18 <- read.csv(unz("/conf/PHSCOVID19_Analysis/UCD/NHS 24 SAS GPOOH reporting/06 Publications/3. Vicky Elliott - NHS24/Zipped/0. NHS24 Extract 1 Jan 18 - 30 Jun 18.zip", "0. NHS24 Extract 1 Jan 18 - 30 Jun 18.csv"), header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
janitor::clean_names() %>%
as.data.frame() %>%
rename(hb=patient_nhs_board_description_current,
       hscp=nhs_24_patient_hscp_name_current,
       sex=gender_description,
       dep=nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
       covid_flag=nhs_24_covid_19_flag,
       date=nhs_24_call_rcvd_date,
       count=number_of_nhs_24_records)
  
nhs24_jultodec18 <- read.csv(unz("/conf/PHSCOVID19_Analysis/UCD/NHS 24 SAS GPOOH reporting/06 Publications/3. Vicky Elliott - NHS24/Zipped/0a. NHS24 Extract 1 Jul 18 - 31 Dec 18.zip", "0a. NHS24 Extract 1 Jul 18 - 31 Dec 18.csv"), header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>%
  as.data.frame() %>%
  rename(hb=patient_nhs_board_description_current,
         hscp=nhs_24_patient_hscp_name_current,
         sex=gender_description,
         dep=nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
         covid_flag=nhs_24_covid_19_flag,
         date=nhs_24_call_rcvd_date,
         count=number_of_nhs_24_records)

nhs24_jantojun19 <- read.csv(unz("/conf/PHSCOVID19_Analysis/UCD/NHS 24 SAS GPOOH reporting/06 Publications/3. Vicky Elliott - NHS24/Zipped/1. NHS24 Extract 1 Jan 19 - 30 Jun 19.zip", "1. NHS24 Extract 1 Jan 19 - 30 Jun 19.csv"), header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>%
  as.data.frame() %>%
  rename(hb=patient_nhs_board_description_current,
         hscp=nhs_24_patient_hscp_name_current,
         sex=gender_description,
         dep=nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
         covid_flag=nhs_24_covid_19_flag,
         date=nhs_24_call_rcvd_date,
         count=number_of_nhs_24_records)

nhs24_jultodec19 <- read.csv(unz("/conf/PHSCOVID19_Analysis/UCD/NHS 24 SAS GPOOH reporting/06 Publications/3. Vicky Elliott - NHS24/Zipped/2. NHS24 Extract 1 Jul 19 - 31 Dec 19.zip", "2. NHS24 Extract 1 Jul 19 - 31 Dec 19.csv"), header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>%
  as.data.frame() %>%
  rename(hb=patient_nhs_board_description_current,
         hscp=nhs_24_patient_hscp_name_current,
         sex=gender_description,
         dep=nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
         covid_flag=nhs_24_covid_19_flag,
         date=nhs_24_call_rcvd_date,
         count=number_of_nhs_24_records)

nhs24_jantoapr20 <- read.csv(unz("/conf/PHSCOVID19_Analysis/UCD/NHS 24 SAS GPOOH reporting/06 Publications/3. Vicky Elliott - NHS24/Zipped/3. NHS24 Extract 1 Jan 20 - 24 Apr 20.zip", "3. NHS24 Extract 1 Jan 20 - 24 Apr 20.csv"), header = TRUE, na.strings = "", stringsAsFactors = FALSE, sep = ",") %>%
  janitor::clean_names() %>%
  as.data.frame() %>%
  rename(hb=patient_nhs_board_description_current,
         hscp=nhs_24_patient_hscp_name_current,
         sex=gender_description,
         dep=nhs_24_patient_prompt_dataset_deprivation_scot_quintile,
         covid_flag=nhs_24_covid_19_flag,
         date=nhs_24_call_rcvd_date,
         count=number_of_nhs_24_records)

# Add data files
nhs24 <-  rbind(nhs24_jantojun18,nhs24_jultodec18,nhs24_jantojun19,nhs24_jultodec19,nhs24_jantoapr20)

# Tidy
rm(nhs24_jantojun18,nhs24_jultodec18,nhs24_jantojun19,nhs24_jultodec19,nhs24_jantoapr20)


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
  mutate(area_type="Health board") %>%
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
  rename(age=age_grp)


##Reshape dataset for shiny app
#Use aggregation function to aggregate data files into format

nhs24_sex <- aggregation_help(grouper="sex") %>%
  rename(category=sex)

nhs24_dep <- aggregation_help(grouper="dep") %>%
  rename(category=dep)

nhs24_age <- aggregation_help(grouper="age") %>%
  rename(category=age)

# Add final aggregation files to one master file
nhs24_final_data <- rbind(nhs24_sex, nhs24_dep, nhs24_age)

#tidy
rm(nhs24_age,nhs24_dep,nhs24_sex,nhs24_all_geos, board_nhs24,scot_nhs24,hscp_nhs24, nhs24_allsex, nhs24_all)


#derive a week number from week_ending column to ease presentation of data
nhs24_final_data <- nhs24_final_data %>%
  mutate(date=as.Date(week_ending,format="%d-%m-%Y"),
         week_no=isoweek(date),
         year=year(date))

#find average number of attendances by week and category
nhs24_average <- nhs24_final_data %>%
  subset(year %in% c("2018","2019")) %>%
  group_by(area_name,area_type,category,type,week_no) %>%
  summarise(count_average=mean(count)) %>%
  ungroup()

#filter for latest year - 2020  (maybe need to reextract data )
nhs24_latest_year <- nhs24_final_data %>%
  subset(year=="2020")

#join latest year of data with averages from previous years
nhs24_shiny <- left_join(nhs24_average,nhs24_latest_year,by = c("area_name", "area_type", "category","type","week_no"))

# Temporary for testing purposes: supressing numbers under 5
nhs24_shiny$count <- ifelse(nhs24_shiny$count<5,0,nhs24_shiny$count)

# Remove weeks that haven't happened yet & reformat NHS board names to include prefix/&
nhs24_shiny <- nhs24_shiny %>%
  filter(category != "Missing") %>% #taking out empty counts
  subset(week_no<18) %>%
  # Creating % variation from pre_covid to covid
  mutate(variation = round(-1 * ((count_average - count)/count_average * 100), 1))

#save output for shiny app
saveRDS(nhs24_shiny, "shiny_app/data/nhs24_data.rds")

nhs24_shiny <- readRDS("shiny_app/data/nhs24_data.rds")

