# Data preparation for app

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(janitor)

###############################################.
## Reading RAPID data ----
###############################################.
# Prepare by Unscheduled care team
rap_adm <- readRDS("/conf/PHSCOVID19_Analysis/Admissions_by_SIMD_age_sex.rds") %>% 
  janitor::clean_names() %>% 
  as.data.frame()

# Aggregating to obtain totals for each split type and then putting all back together.
rap_adm_all <- rap_adm %>% group_by(date_adm) %>% 
  summarise(count = sum(count)) %>% 
  mutate(type = "sex", category = "All")

rap_adm_depr <- rap_adm %>% group_by(date_adm, simd_quintile) %>% 
  summarise(count = sum(count)) %>% rename(category = simd_quintile) %>% 
  mutate(type = "depr", category = as.character(category),
         category = recode(category, "1" = "1 - most deprived", "5" = "5 - Least deprived"))

rap_adm_sex <- rap_adm %>% group_by(date_adm, sex) %>% 
  summarise(count = sum(count)) %>% rename(category = sex) %>% 
  mutate(type = "sex", 
         category = recode(category, "male" = "Male", "female" = "Female")) 

rap_adm_age <- rap_adm %>% group_by(date_adm, age_group) %>% 
  summarise(count = sum(count)) %>% rename(category = age_group) %>% 
  mutate(type = "age")
  
rap_adm <- bind_rows(rap_adm_all, rap_adm_depr, rap_adm_sex, rap_adm_age) %>% 
  # Filtering cases without information on age, sex or deprivation (still counted in all)
  filter(!(is.na(category))) %>% 
  rename(date = date_adm)

saveRDS(rap_adm, "shiny_app/data/rapid_data.rds")

##END