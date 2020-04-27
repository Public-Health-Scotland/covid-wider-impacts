# Data preparation for app

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(janitor)
library(lubridate)
library(zoo)
library(readr)

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

# Speed up aggregations of different data cuts for OOH dataset
agg_ooh <- function(grouper) {
  ooh %>%
    group_by_at(c("week_ending","area_name", "area_type", "date", grouper)) %>%
    summarise(count = sum(count)) %>% ungroup() %>% 
    mutate(type = grouper) 
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
spec_lookup <- readRDS("/conf/linkage/output/lookups/Unicode/National Reference Files/specialt.rds") %>% 
  janitor::clean_names() %>% select(description, speccode)

rap_adm <- left_join(rap_adm, spec_lookup, by = c("spec" = "speccode")) %>% 
  select(-spec) %>% rename(spec = description)

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
  mutate(category = recode(age_group, "5_thru_14" = "5 - 14", "15_thru_44" = "15 - 44", "45_thru_64" = "45 - 64",
                           "65_thru_74" = "65 -74", "75_thru_84" = "75 -84",
                           "85+" = "85 and over", "Under_5" = "Under 5")) %>% 
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
  mutate(age_group = recode(age_group, "5-14" = "5 - 14", "15-24" = "15 - 44", 
                           "25-44" = "15 - 44", "45-64" = "45 - 64",
                           "65-74" = "65 -74", "75-84" = "75 -84",
                           "85 plus" = "85 and over", "0-4" = "Under 5"),
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
  mutate(area_name = recode(area_name, "NHS AYRSHIRE & ARRAN" = "NHS Ayrshire & Arran", 
                            "NHS BORDERS" = "NHS Borders", "NHS DUMFRIES & GALLOWAY" = "NHS Dumfries & Galloway", 
                            "NHS FIFE" = "NHS Fife", "NHS FORTH VALLEY" = "NHS Forth Valley", 
                            "NHS GRAMPIAN" = "NHS Grampian", "NHS GREATER GLASGOW & CLYDE" = "NHS Greater Glasgow & Clyde", 
                            "NHS HIGHLAND" = "NHS Highland", "NHS LANARKSHIRE" = "NHS Lanarkshire", 
                            "NHS LOTHIAN" = "NHS Lothian", "NHS ORKNEY" = "NHS Orkney", 
                            "NHS SHETLAND" = "NHS Shetland", "NHS TAYSIDE" ="NHS Tayside", 
                            "NHS WESTERN ISLES" = "NHS Western Isles"),
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
ooh_all <- agg_ooh(grouper=NULL) %>% mutate(type = "sex", category = "All")
ooh_sex <- agg_ooh(grouper="sex") %>% rename(category = sex)
ooh_dep <- agg_ooh(grouper="dep") %>% rename(category = dep)
ooh_age <- agg_ooh(grouper="age") %>% rename(category = age)
 
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


##END