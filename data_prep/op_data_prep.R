
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
## Approximate run time: <2 minutes                        ##

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

# Aggregating to weekly data
outpats_agg = outpats_full %>% 
  filter(year >= 2018) %>%
  mutate(count = 1) %>%
  mutate(week_ending = ceiling_date(clinic_date, "week", change_on_boundary = F)) %>% #end of week
  group_by(hscp2019name, hbtreat_name, hbres_new_name, appt_type, attendance_status, simd, 
           age_grp, sex_name, mode_contact_new, week_ending, Grouping) %>% 
  summarise(count = sum(count, na.rm = T)) %>% 
  ungroup() %>%
  rename(spec = Grouping) %>%
  filter(attendance_status == "Attended")

outpats_agg %<>% mutate(hscp2019name = case_when(is.na(hscp2019name) ~ "Other",
                                                 TRUE ~ hscp2019name),
                        hbtreat_name = case_when(is.na(hbtreat_name) ~ "Other",
                                                 TRUE ~ hbtreat_name),
                        hbres_new_name = case_when(is.na(hbres_new_name) ~ "Other",
                                                   TRUE ~ hbres_new_name),
                        hbtreat_name = case_when(hbtreat_name == "NHS Louisa Jordan (Covid-19)" ~ 
                                                   "NHS Louisa Jordan",
                                                 TRUE ~ hbtreat_name),
                        hbres_new_name = case_when(substr(hbres_new_name, 1, 3) != "NHS" ~
                                                     "Other",
                                                   TRUE ~ hbres_new_name))

# Aggregating for each geo level
outpats_agg %<>% mutate(scot = "Scotland") %>% 
  gather(area_type, area_name, 
         c(hbtreat_name, hbres_new_name, hscp2019name, scot)) %>% 
  ungroup() %>% 
  mutate(area_type = recode(area_type, 
                            "hbtreat_name" = "Health board of treatment", 
                            "hbres_new_name" = "Health board of residence",
                            "hscp2019name" = "HSC partnership", 
                            "scot" = "Scotland"))

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
# Totals for overalls for all pop including totals by specialty too
op_adm_all <- agg_op(NULL, split = "sex", specialty = T) %>% 
  mutate(category = "All")
op_adm_sex <- agg_op(c("sex_name"), split = "sex") %>% 
  rename(category = sex_name) %>% # Totals for overalls for all sexes
  filter(category %in% c("Male", "Female"))
op_adm_age <- agg_op(c("age_grp"), split = "age") %>% 
  rename(category = age_grp) # Totals for overalls for all age groups
# Totals for overalls for deprivation quintiles
op_adm_depr <- agg_op(c("simd"), split = "dep")  %>% 
  mutate(simd=case_when(is.na(simd)~"Missing", 
                        simd==1 ~ "1 - most deprived",
                        simd==5 ~"5 - least deprived", 
                        TRUE~as.character(simd))) %>% 
  rename(category = simd) %>%
  filter(!is.na(category),
         category != "Missing")
# Totals for overalls for mode of contact
op_adm_moc <- agg_op(c("mode_contact_new"), split = "moc") %>% 
  rename(category = mode_contact_new) %>%
  filter(!is.na(category))

### apply disclosure flag to cases
op_adm = disc_flag_adm() %>%
  filter(!(spec %in% c("Dental", "Other")))

dataset = op_adm

# Function to format data in the right format for the Shiny app
# prepare_final_data <- function(dataset, filename, last_week, extra_vars = NULL) {
  
  # Creating week number to be able to compare pre-covid to covid period
  dataset <- dataset %>% 
    mutate(week_no = isoweek(week_ending),
           # Fixing HSCP names
           area_name = gsub(" and ", " & ", area_name)) 
  ## apply disclosure flag to cases 
  dataset <- disc_flag_all()
  
  # Apply disclosure control
  dataset = disclosure()
  
  # Creating average appts of pre-covid data (2018-2019) by day of the year
  historic_data <- dataset %>% filter(year(week_ending) %in% c("2018", "2019")) %>% 
    group_by(category, type, area_name, area_type, week_no, appt_type, spec) %>%
    # Not using mean to avoid issues with missing data for some weeks
    summarise(count_average = round((sum(count, na.rm = T))/2, 1)) 
  
  # Joining with 2020 data
  # Filtering weeks with incomplete week too!! Temporary
  data_2020 <- left_join(dataset %>% 
                           filter(year(week_ending) %in% c("2020")), 
                         historic_data) %>%
    # filter(count != 0 & count_average != 0) %>%
    # Creating %variation from precovid to covid period 
    mutate(count_average = ifelse(is.na(count_average), 0, count_average),
           variation = round(-1 * ((count_average - count)/count_average * 100), 1),
           # Dealing with infinite values from historic average = 0
           variation = case_when(is.na(count) ~ 0,
                                 is.na(count_average) ~ 0,
                                 is.nan(variation) ~ 0,
                                 is.infinite(variation) ~ 0,
                                 TRUE ~ variation)) %>%
    rename("admission_type" = "appt_type") %>% 
    filter(week_ending <= dmy("27-09-2020")) %>%
    mutate(variation_new = case_when(count_average == 0 & variation == 0 ~ 100,
                                 TRUE ~ variation))
  
  final_data <<- data_2020
  saveRDS(data_2020, "shiny_app/data/outpats.rds")
  saveRDS(data_2020, paste0(WID_folder,"outpats.rds"))
  saveRDS(data_2020, paste0(WID_folder,"final_app_files/outpats_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  saveRDS(data_2020, paste0(WID_folder, "outpats.rds"))
# }
  
  area_type_op <- dataset %>%
    group_by(area_name, area_type) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    select(-count) %>%
    arrange(area_type)
  
  saveRDS(area_type_op, paste0(WID_folder, "final_app_files/area_type_op_",
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  saveRDS(area_type_op, paste0(data_folder, "area_type_op.rds"))
  
# prepare_final_data(op_adm, "outpats", last_week = "2020-09-27",
#                    extra_vars = c("admission_type", "spec"))
  
