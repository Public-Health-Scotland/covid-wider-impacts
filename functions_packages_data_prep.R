# Contains all the common stuff used for the app data prep

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
library(flextable)
library(magrittr)
library(haven)

###############################################.
## Filepaths ----
###############################################.

# Filepath changes depending on Desktop/Server
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/"
  ae_folder <- "/conf/PHSCOVID19_Analysis/UCD/A&E/2020-10-29-Extracts/" #short cut to a&e folder areas
  cl_out <- "/conf/linkage/output/lookups/Unicode/"
  open_data <- "/conf/PHSCOVID19_Analysis/Publication outputs/open_data/"
} else {
  data_folder <- "//Isdsf00d03/PHSCOVID19_Analysis/shiny_input_files/"
  ae_folder <- "//Isdsf00d03/PHSCOVID19_Analysis/UCD/A&E/2020-10-29-Extracts/" #short cut to a&e folder areas
  cl_out <- "//Isdsf00d03/cl-out/lookups/Unicode/"
  open_data <- "//Isdsf00d03/PHSCOVID19_Analysis/Publication outputs/open_data/"
  
}

###############################################.
## Lookups ----
###############################################.
#Used for RAPID, Immunisations and child health reviews as they use cyphers instead
hb_lookup <- read_spss(paste0(cl_out, "National Reference Files/Health_Board_Identifiers.sav")) %>% 
  janitor::clean_names() %>% select(description, hb_cypher) %>%
  rename(area_name=description) %>%
  mutate(hb_cypher=as.character(hb_cypher), area_name= as.character(area_name),
         area_type="Health board")

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
  
  saveRDS(data_2020, paste0("shiny_app/data/", filename,".rds"))
  saveRDS(data_2020, paste0(data_folder,"final_app_files/", filename, "_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  saveRDS(data_2020, paste0(open_data, filename,"_data.rds"))
}


# Function to format cardiac data in the right format for the Shiny app
prepare_final_data_cardiac <- function(dataset, filename, last_week, extra_vars = NULL) {
  
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
           variation =  ifelse(is.infinite(variation), 0, variation)) %>% 
    select(-week_no) 
  
  # Disclosure control
  # Does not work setting < 5 counts to zero makes graphs look jaggy
  #data_2020 <- data_2020 %>% 
  #  mutate(count = if_else(count < 5, 0, count),
  #         count_average = if_else(count_average < 5, 0, count_average),
  #         variation = if_else(count < 5, 0, variation))
  
  # Filter week
  #data_2020 <- data_2020 %>%
  #  filter(week_ending <= as.Date(last_week))
  
  # Supressing numbers under 5
  data_2020 <- data_2020 %>% filter(count>=10) %>% 
    filter(week_ending <= as.Date(last_week)) 
  
  final_data <<- data_2020
  
  saveRDS(data_2020, paste0("shiny_app/data/", filename,".rds"))
  saveRDS(data_2020, paste0(data_folder,"final_app_files/", filename, "_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  saveRDS(data_2020, paste0(open_data, filename,"_data.rds"))
  
  #saveRDS(data_2020, paste0("shiny_app/data/", filename,"_data.rds"))
  #saveRDS(data_2020, paste0("/conf/PHSCOVID19_Analysis/Publication outputs/open_data/", filename,"_data.rds"))
}

#Function to format the immunisations and child health review tables
format_immchild_table <- function(filename) {
  read_csv(paste0(data_folder, filename, ".csv")) %>%
    janitor::clean_names() %>%
    rename(area_name=geography_name) %>%
    select (-geography) %>%
    arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
    mutate(time_period_eligible=as.factor(time_period_eligible))
  
}

##END
