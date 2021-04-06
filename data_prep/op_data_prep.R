
## Data Release - COVID Wider Impacts Dashboard            ##
## Original Author - Róisín Farrell                        ##
## Original Date - November 2020                           ##
## Latest Update Author - Roisin Farrell                   ##
## Latest Update Date - November 2020                      ##
##                                                         ##
## Type - Extraction/preparation                           ##
## Written/run on - R Studio SERVER                        ##
## R version - 3.6.1                                       ##
## Description - analysis of outpatients for COVID wider   ##
##                  impacts dashboard                      ##
##                                                         ##
## Approximate run time: 4 minutes                        ##

start <- Sys.time()

### 0 - Load required packages
library(dplyr)      # for data wrangling
library(magrittr)   # for %<>% operator
library(stringr)    # for manipulation of strings
library(tidyr)      # for manipulating data in the "tidyway"
library(lubridate)  # for dates
library(readr)
library(fst)
library(janitor)
library(purrr)

### read in disclosure script
source("outpatient_functions.R")

### 1 - Read in data ----
outpats_full = read_fst(paste0(SCT_folder, "Outpatients_basefile.fst"))

##create specialty lookup, including which specialties are in each group ----
##this is used in the dashboard
spec_lookup = outpats_full %>%
  group_by(Description, Grouping) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  select(-count) %>%
  filter(Grouping != "Dental",
         Grouping != "Other") %>%
  arrange(Grouping)

saveRDS(spec_lookup, paste0(WID_folder,"final_app_files/spec_lookup_op_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(spec_lookup, paste0(data_folder, "spec_lookup_op.rds"))

# Aggregating to weekly data ----
outpats_agg = outpats_full %>% 
  filter(year >= 2018) %>%
  mutate(count = 1) %>%
  mutate(week_ending = ceiling_date(clinic_date, "week", change_on_boundary = F)) %>% #end of week
  group_by(HSCPName, hbtreat_name, hbres_new_name, appt_type, attendance_status, simd, 
           age_grp, sex_name, mode_contact_new, week_ending, Grouping) %>% 
  summarise(count = sum(count, na.rm = T)) %>% 
  ungroup() %>%
  rename(spec = Grouping) %>%
  ##remove DNAs ***TEMPORARY***
  filter(attendance_status == "Attended")

### recode locations ----
outpats_agg %<>% mutate(HSCPName = case_when(is.na(HSCPName) ~ "Other",
                                                 TRUE ~ HSCPName),
                        hbtreat_name = case_when(is.na(hbtreat_name) ~ "Other",
                                                 TRUE ~ hbtreat_name),
                        hbres_new_name = case_when(is.na(hbres_new_name) ~ "Other",
                                                   TRUE ~ hbres_new_name),
                        hbtreat_name = case_when(hbtreat_name == "NHS Louisa Jordan (Covid-19)" ~ 
                                                   "NHS Louisa Jordan",
                                                 TRUE ~ hbtreat_name),
                        hbres_new_name = case_when(substr(hbres_new_name, 1, 3) != "NHS" ~
                                                     "Other",
                                                   TRUE ~ hbres_new_name),
                        hbtreat_name = case_when(substr(hbtreat_name, 1, 4) != "NHS "
                                                     & hbtreat_name != "National Waiting Times Centre" ~
                                                     "Other",
                                                   TRUE ~ hbtreat_name),
                        HSCPName = case_when(hbres_new_name == "Other" ~
                                                     "Other",
                                                   TRUE ~ HSCPName))

# Aggregating for each geo level ----
outpats_agg %<>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, 
         c(hbtreat_name, hbres_new_name, HSCPName, scot)) %>% 
  ungroup() %>% 
  mutate(area_type = recode(area_type, 
                            "hbtreat_name" = "Health board of treatment", 
                            "hbres_new_name" = "Health board of residence",
                            "HSCPName" = "HSC partnership of residence", 
                            "scot" = "Scotland"))

### function that creates totals for each split
agg_op <- function(grouper = NULL, split, specialty = F) {
  
  agg_helper <- function(more_vars, type_chosen = split) {
    outpats_agg %>%
      group_by_at(c("week_ending", "area_name", "area_type", more_vars)) %>%
      summarise(count = sum(count)) %>% ungroup() %>%
      mutate(type = type_chosen)
  }
  
  # Aggregating to obtain totals for each split type and then putting all back together.
  appt_type <- agg_helper(c(grouper, "appt_type")) %>% 
    mutate(spec = "All")
  
  all <- agg_helper(grouper) %>% 
    mutate(appt_type = "All", spec = "All") 
  
  if (specialty == T) {
    spec_all <- agg_helper(c(grouper, "spec")) %>% 
      mutate(appt_type = "All") 
    
    spec_adm <- agg_helper(c(grouper, "spec", "appt_type")) 
    
    rbind(all, appt_type, spec_all, spec_adm)
  } else {
    rbind(all, appt_type) 
  }
}

# Aggregating to obtain totals for each split type and then putting all back together
op_adm_all <- agg_op(NULL, split = "sex", specialty = T) %>% 
  mutate(category = "All") # Totals for all pop and specialty

op_adm_sex <- agg_op(c("sex_name"), split = "sex") %>% 
  rename(category = sex_name) %>%
  filter(category %in% c("Male", "Female")) # Totals for all sexes

op_adm_age <- agg_op(c("age_grp"), split = "age") %>% 
  rename(category = age_grp) # Totals for all age groups

op_adm_depr <- agg_op(c("simd"), split = "dep")  %>% 
  mutate(simd=case_when(is.na(simd)~"Missing", 
                        simd==1 ~ "1 - most deprived",
                        simd==5 ~"5 - least deprived", 
                        TRUE~as.character(simd))) %>% 
  rename(category = simd) %>%
  filter(!is.na(category),
         category != "Missing") # Totals for all SIMD quintiles

op_adm_moc <- agg_op(c("mode_contact_new"), split = "moc") %>% 
  rename(category = mode_contact_new) %>%
  filter(!is.na(category)) # Totals for all modes of clinical interaction

### apply disclosure flag to cases ----
op_adm = disc_flag_adm() %>%
  ## remove specialties with low numbers
  filter(!(spec %in% c("Dental", "Other")))

### creating new object for final data ----
dataset = op_adm
  
  # Creating week number to be able to compare pre-covid to covid period ----
  dataset <- dataset %>% 
    mutate(week_no = isoweek(week_ending),
           # Fixing area names
           area_name = gsub(" and ", " & ", area_name)) 
  ## apply disclosure flag to cases ----
  dataset <- disc_flag_all()
  
  # Apply disclosure control ----
  dataset = disclosure()
  
  # Creating average appts of pre-covid data (2018-2019) by week of the year ----
  historic_data <- dataset %>% filter(year(week_ending) %in% c("2018", "2019")) %>% 
    group_by(category, type, area_name, area_type, week_no, appt_type, spec) %>%
    summarise(count_average = round((sum(count, na.rm = T))/2, 1)) 
  
  # Joining with 2020 data
  data_2020 <- left_join(dataset %>% 
                           filter(year(week_ending) %in% c("2020")), 
                         historic_data) %>%
    # Creating %variation from precovid to covid period 
    mutate(count_average = ifelse(is.na(count_average), 0, count_average),
           variation = round(-1 * ((count_average - count)/count_average * 100), 1),
           # Dealing with infinite values from historic average = 0
           variation = case_when(is.na(count) ~ 0,
                                 is.na(count_average) ~ 0,
                                 is.nan(variation) ~ 0,
                                 is.infinite(variation) ~ 0,
                                 TRUE ~ variation)) %>%
    ## renaming for ease in dashboard 
    rename("admission_type" = "appt_type") %>% 
    ## data goes to week ending October 4th but October data haven't been published
    filter(week_ending <= dmy("27-09-2020")) %>%
    # changing values where there is no activity in 2018/19 to 100% variation
    #   to avoid weird trends
    mutate(variation = case_when(count_average == 0 & variation == 0 &
                                       count != 0 ~ 100,
                                 TRUE ~ variation))
  
  saveRDS(data_2020, "shiny_app/data/outpats.rds")
  saveRDS(data_2020, paste0(WID_folder,"outpats.rds"))
  saveRDS(data_2020, paste0(WID_folder,"final_app_files/outpats_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  saveRDS(data_2020, paste0(WID_folder, "outpats.rds"))
  
  ### Creating area type lookup for dashboard ----
  area_type_op <- dataset %>%
    group_by(area_name, area_type) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    select(-count) %>%
    arrange(area_type)
  
  saveRDS(area_type_op, paste0(WID_folder, "final_app_files/area_type_op_",
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  saveRDS(area_type_op, paste0(data_folder, "area_type_op.rds"))
  
  # time taken.
  end <- Sys.time()
  end - start
  
  ### END OF SCRIPT ###
  
  
  
