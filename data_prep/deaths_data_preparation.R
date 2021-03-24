##### Weekly all-cause deaths data preparation for Wider Impacts app
##### Liz Richardson 
##### elizabeth.richardson1@nhs.net 
##### 10 September 2020

###############################################.
## Packages ----
###############################################.

library(tidyverse) # all kinds of stuff 
library(lubridate) # for working with dates
library(scales)
library(ggplot2)

###############################################.
## File pathways ----
###############################################.

## remove any existing objects from global environment ----------------------
rm(list = ls())

## set pathways ----------------------

#Source of the data: access granted via Chris Robertson chrisrobertson@nhs.net.  Contact Chris if data access needed. 
datafile <- "F:/PHI/SHPN-PA/05 RIG/Surveillance/Mortality/03 Data/NRS_coded_deaths/NRS_Data.rds"

#Paths to the secure folder that the PHO team use to access sensitive data. 
#The syntax stores a copy of the deaths data here for easier access by the team, and stores the processed deaths data here too. 
datafolder <- "Q:/Team-NRSData/NRS data/Projects/Weekly COVID deaths/data/"
working <- "Q:/Team-NRSData/NRS data/Projects/Weekly COVID deaths/wider_impacts_dashboard/working_data/"
lookups <- "Q:/Team-NRSData/NRS data/Projects/Weekly COVID deaths/wider_impacts_dashboard/data/"

#Path to PHSCOVID19_Analysis where the processed deaths data needs to be stored for inclusion in the wider impacts shiny app (access granted via Victoria Elliott victoria.elliott@nhs.net)
#The processed data can be emailed to Vicky or jaime.villacampa@nhs.net if the user of this script doesn't have access to this folder.
shiny_files <- "B:/shiny_input_files/deaths/"

###############################################.
## Reading in data and lookups ----
###############################################.

### load deaths data ----------------------
#df_received <- readRDS(paste0(datafolder,"received_data/NRS_Data.rds")) %>% 
#  as_tibble()
df_received <- readRDS(datafile) %>% 
  as_tibble()
#save the raw NRS data into the datafolder for others in PHO team to use
setwd(paste0(datafolder, "received_data"))
file.rename("NRS_Data.rds", paste0("NRS_Data (replaced on ", format(Sys.Date(), "%d-%b-%Y"), ").rds"))
write_rds(df_received, "NRS_Data.rds")

names(df_received) <- tolower(names(df_received)) # decapitalise column names

### load postcode-simd look up ---------------
pc_lookup <- readRDS(paste0(lookups, "pc_lookup.rds"))

### load weekly lookup file ----------------------
weeks_lookup <- readRDS(paste0(lookups, "weeks_lookup.rds"))

### load geography lookup file ----------------------
geo_lookup <- readRDS(paste0(lookups, "geo_lookup.rds"))

###############################################.
## Data prep ----
###############################################.

df_received <- df_received %>% 
  ## recode variables
  mutate(age_grp2 = case_when(age<=64 ~ "Under 65", age>=65 ~ "65 and over", TRUE ~ "")) %>%   ## create age groups
  mutate(sex_grp = if_else(sex == "M", "Male", "Female")) %>%   ## create sex groups
  ## add registration week and year
  mutate(reg_week = isoweek(ymd(df_received$reg.date))) %>%   ## use ISO8601 week numbering standard as per NRS
  mutate(reg_year = isoyear(ymd(df_received$reg.date))) %>%   ## change year to ISO8601 week numbering standard too
  ## add geog and SIMD codes 
  left_join(pc_lookup, by = "postcode") 

#### check for missings:
table(df_received$age_grp2, useNA = c("always"))
table(df_received$sex_grp, useNA = c("always"))
table(is.na(df_received$postcode)) # any missing postcodes can't be joined to a geography, but will still count in the Scotland data

#### set max_week equal to latest week of 2020 ---------
max_week <- as.integer(df_received %>%
                         filter(reg_year==2020) %>%
                         summarise(max(reg_week))) # assign 2020 max week value to filter 2015-19 

### check that last week is a complete week ---------------
days_in_max_week <- as.integer(
  df_received %>% 
    filter(reg_year==2020 & reg_week==max_week) %>%
    group_by(reg.date) %>%
    summarise() %>%
    ungroup() %>%
    summarise(count = n()))

#complete weeks have 6 or 7 days of data in them
#subtract one from max_week if the last week isn't complete:
max_week <- ifelse(days_in_max_week<6, max_week-1, max_week)

#### create weekly aggregated dataframe ---------------
df_weekly <- df_received %>% 
  filter(reg_year >= 2015) %>% ##create dataframe only with relevant years (2015-2020)
  filter(reg_week <= max_week) %>%  ## remove weeks > max week
  select(year=reg_year, reg_week, DZ2011, HB2019, HSCP2019, simd2020_sc_quintile, sex_grp, age_grp2) %>%
  mutate(group=case_when(year==2020 ~ "2020",
                         year<2020 ~ "2015to2019"),       
         allcause = 1) %>%
  select(-DZ2011, -year) %>%
  group_by(reg_week, group, HB2019, HSCP2019, simd2020_sc_quintile, sex_grp, age_grp2) %>%
  summarise_all(sum, na.rm =T) %>% ungroup()

# Checks:
#sum(df_weekly$allcause) == nrow(df_temp_all) # check number of deaths equals rows in unaggregated data
#df_weekly %>% group_by(group) %>% summarise(total_n = sum(allcause, na.rm  = TRUE)) %>% ungroup() ## check number of deaths per group
#sapply(df_weekly, function(x) sum(is.na(x))) ## check number of missing cases per column

## save raw data file
#write_rds(df_weekly, paste0(working,"covid_widerimpacts_raw.rds"))
#df_weekly <-readRDS(paste0(working,"covid_widerimpacts_raw.rds"))

###############################################.
## Prepare aggregated weekly data ----
###############################################.

#### aggregate by Scotland ---------------
scot <- df_weekly %>% 
  group_by(group, reg_week) %>%
  select(-age_grp2, -sex_grp, -HSCP2019, -simd2020_sc_quintile, -HB2019) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  mutate(area_code = "S00000001",
         category = "All")

#### aggregate by Scotland and sex ---------------
scot_sex <- df_weekly %>% 
  group_by(group, reg_week, sex_grp) %>%
  select(-age_grp2, -HSCP2019, -simd2020_sc_quintile, -HB2019) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(category = sex_grp) %>%
  mutate(area_code = "S00000001")

#### aggregate by Scotland and age group ---------------
scot_age <- df_weekly %>% 
  group_by(group, reg_week, age_grp2) %>%
  select(-sex_grp, -HSCP2019, -simd2020_sc_quintile, -HB2019) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(category = age_grp2) %>%
  mutate(area_code = "S00000001")

#### aggregate by Scotland and SIMD ---------------
scot_simd <- df_weekly %>% 
  group_by(group, reg_week, simd2020_sc_quintile) %>%
  select(-sex_grp, -HSCP2019, -HB2019, -age_grp2) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(category = simd2020_sc_quintile) %>%
  mutate(area_code = "S00000001",
         category = case_when(category==1 ~ "1 - most deprived",
                              category==2 ~ "2",
                              category==3 ~ "3",
                              category==4 ~ "4",
                              category==5 ~ "5 - least deprived"))

#### aggregate by HB ---------------
hb <- df_weekly %>% 
  group_by(group, reg_week, HB2019) %>%
  select(-age_grp2, -sex_grp, -HSCP2019, -simd2020_sc_quintile) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(area_code = HB2019) %>%
  mutate(category = "All")

#### aggregate by HB and sex ---------------
hb_sex <- df_weekly %>% 
  group_by(group, reg_week, HB2019, sex_grp) %>%
  select(-age_grp2, -HSCP2019, -simd2020_sc_quintile) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(area_code = HB2019,
         category = sex_grp) 

#### aggregate by HB and age group ---------------
hb_age <- df_weekly %>% 
  group_by(group, reg_week, HB2019, age_grp2) %>%
  select(-sex_grp, -HSCP2019, -simd2020_sc_quintile) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(area_code = HB2019,
         category = age_grp2)

#### aggregate by HB and SIMD --------------- #not needed as we're not presenting subnational SIMD data now
# hb_simd <- df_weekly %>%
#   group_by(group, reg_week, HB2019, simd2020_sc_quintile) %>%
#   select(-sex_grp, -HSCP2019, -age_grp2) %>%
#   summarise_all(sum, na.rm =T) %>% ungroup() %>%
#   rename(area_code = HB2019,
#          category = simd2020_sc_quintile) %>%
#   mutate(category = case_when(category==1 ~ "1 - most deprived",
#                               category==2 ~ "2",
#                               category==3 ~ "3",
#                               category==4 ~ "4",
#                               category==5 ~ "5 - least deprived"))

#### aggregate by HSCP ---------------
hscp <- df_weekly %>% 
  group_by(group, reg_week, HSCP2019) %>%
  select(-age_grp2, -sex_grp, -HB2019, -simd2020_sc_quintile) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(area_code = HSCP2019) %>%
  mutate(category = "All")

#### aggregate by HSCP and sex ---------------
hscp_sex <- df_weekly %>% 
  group_by(group, reg_week, HSCP2019, sex_grp) %>%
  select(-age_grp2, -HB2019, -simd2020_sc_quintile) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(area_code = HSCP2019,
         category = sex_grp) 

#### aggregate by HSCP and age group ---------------
hscp_age <- df_weekly %>% 
  group_by(group, reg_week, HSCP2019, age_grp2) %>%
  select(-sex_grp, -HB2019, -simd2020_sc_quintile) %>%
  summarise_all(sum, na.rm =T) %>% ungroup() %>%
  rename(area_code = HSCP2019,
         category = age_grp2) 

#### aggregate by HSCP and SIMD --------------- #not needed as we're not presenting subnational SIMD data now
# hscp_simd <- df_weekly %>%
#   group_by(group, reg_week, HSCP2019, simd2020_sc_quintile) %>%
#   select(-sex_grp, -HB2019, -age_grp2) %>%
#   summarise_all(sum, na.rm =T) %>% ungroup() %>%
#   rename(area_code = HSCP2019,
#          category = simd2020_sc_quintile) %>%
#   mutate(category = case_when(category==1 ~ "1 - most deprived",
#                               category==2 ~ "2",
#                               category==3 ~ "3",
#                               category==4 ~ "4",
#                               category==5 ~ "5 - least deprived"))

#combine the rows:
combined <- rbind(hb, hb_age, hb_sex,  
                  hscp, hscp_age, hscp_sex, 
                  scot, scot_age, scot_sex, scot_simd)

#make blank data holder for all combos of the categories (deaths count == 0)
allcategories <- unique(combined$category)
allweeks <- c(1:max_week)
allareas <- unique(combined$area_code)
allgroups <- unique(combined$group)
blank <- data.frame(expand.grid(group=allgroups, category=allcategories, area_code=allareas, reg_week=allweeks))
blank <- blank %>%
  filter(!is.na(category)) %>%
  filter(!is.na(area_code)) %>%
  mutate(blank=0) 
#Remove rows for HB/HSCP and SIMD:
blank <- blank %>%
  mutate(drop = case_when(area_code != "S00000001" & category %in% c("1 - most deprived", "2", "3", "4", "5 - least deprived") ~ 1, TRUE ~ 0)) %>%
  filter(drop!=1)

# #Remove rows for the missing SIMD quintiles: (not relevant now we're not presenting subnational SIMD data)
# #Orkney and Shetland (S37000022 S37000026 S08000025 S08000026) are missing SIMD1 and SIMD5, 
# #and WIsles (S37000031 S08000028) is missing SIMD1, SIMD4 and SIMD5)
# blank <- blank %>%
#   mutate(drop = case_when(area_code %in% c("S37000031", "S08000028") & category %in% c("1 - most deprived", "4", "5 - least deprived") ~ 1,
#                           area_code %in% c("S37000022", "S37000026", "S08000025", "S08000026") & category %in% c("1 - most deprived", "5 - least deprived") ~ 1,
#                           TRUE ~ 0)) %>%
#   filter(drop!=1)

#merge the deaths data into the blank holder, so all combinations of categories will be represented:
combined <- blank %>%
  merge(y=combined, by=c("group", "reg_week", "category", "area_code"), all.x=TRUE) %>% 
  mutate(allcause = case_when(is.na(allcause) ~ 0,
                              TRUE ~ allcause))

# Finally prepare the data for the wider impacts dashboard
combined_wide <- combined %>%
  select(-starts_with("covid"), -blank) %>%
  pivot_wider(names_from = group, values_from = allcause, names_prefix = "y") %>%
  rename(count = y2020) %>%
  mutate(count_average = round(y2015to2019/5, 1)) %>%
  merge(y=weeks_lookup, by="reg_week", all.x=TRUE) %>% #get week_ending date
  merge(y=geo_lookup, by.x="area_code", by.y="code", all.x=TRUE) %>% #get area names
  mutate(week_ending = as.Date(end, "%d/%m/%Y"),
         area_name = as.character(areaname),
         area_type = as.character(areatype),
         category = as.character(category),
         type = case_when(category %in% c("All", "Male", "Female") ~ "sex",
                          category %in% c("Under 65", "65 and over") ~ "age",
                          category %in% c("1 - most deprived", "2", "3", "4", "5 - least deprived") ~ "dep")) %>%
  select(week_ending, area_name, area_type, count, type, category, count_average) %>%
  mutate(variation = round((count - count_average) * 100 / count_average, 1)) %>%
  mutate(variation = case_when(count==0 & count_average==0 ~ 0,
                               variation==Inf ~ as.double(NA),
                               TRUE ~ variation)) %>%
  arrange(category, week_ending, area_type, area_name, type)

## save final data file
setwd(shiny_files)
file.rename("deaths_data.rds", paste0("deaths_data (replaced on ", format(Sys.Date(), "%d-%b-%Y"), ").rds"))
write_rds(combined_wide, "deaths_data.rds")
write_rds(combined_wide, paste0(working, "deaths_data.rds"))
write_rds(combined_wide, "Q:/Team-NRSData/NRS data/Projects/Weekly COVID deaths/wider_impacts_dashboard/shiny_app/data/deaths_data.rds")
########################################
# QA checks against NRS published data:
########################################

#check HB data corresponds with NRS: sum for whole 2020
# check <- combined_wide %>%
#   filter(area_type!="HSC partnership") %>%
#   filter(category=="All") %>%
#   group_by(area_name) %>%
#   summarise(deaths2020 = sum(count),
#             deaths2015to2016average = sum(count_average*5)/5)

#count those that didn't match (By week 20 we're missing 114 deaths for HB/HSCP level. These are deaths of non-Scottish residents, according to NRS)
# check2 <- df_weekly %>%
#   filter(is.na(HB2019)) %>%
#   group_by(group) %>%
#   summarise(allcause = sum(allcause))



###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

library(odbc)
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

###############################################.
## Cancer deaths ----
###############################################.
data_deaths <- tbl_df(dbGetQuery(channel, statement=
"SELECT date_of_registration, age, sex, UNDERLYING_CAUSE_OF_DEATH diag, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_C
    WHERE date_of_registration >= '29 December 2014'
  UNION ALL
    SELECT date_of_registration, age, sex, UNDERLYING_CAUSE_OF_DEATH diag, POSTCODE pc7
    FROM ANALYSIS.GRO_DEATHS_WEEKLY_C")) %>%
  setNames(tolower(names(.))) %>% 
  mutate(week_ending = ceiling_date(as.Date(date_of_registration), "week", change_on_boundary = F)) %>% 
  filter(substr(diag,1,1) == "C") %>% 
  mutate(sex = recode(sex, "1" = "Male", "2" = "Female", "0" = NA_character_, "9" = NA_character_),
         age = case_when(between(age, 0,64) ~ "Under 65", T ~ "65 and over"))

#simd, hscp, hb missing need to bring
#function prepare final data probably won't work without tweaks

### END