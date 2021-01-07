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
library(readxl)

###############################################.
## Filepaths ----
###############################################.

# Filepath changes depending on Desktop/Server
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/"
  ae_folder <- "/conf/PHSCOVID19_Analysis/UCD/A&E/" #short cut to a&e folder areas
  cl_out <- "/conf/linkage/output/lookups/Unicode/"
  open_data <- "/conf/PHSCOVID19_Analysis/Publication outputs/open_data/"
} else {
  data_folder <- "//Isdsf00d03/PHSCOVID19_Analysis/shiny_input_files/"
  ae_folder <- "//Isdsf00d03/PHSCOVID19_Analysis/UCD/A&E/" #short cut to a&e folder areas
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
  
  # Create average for week 53 using average for week 52
  week_53 <- historic_data %>% filter(week_no == 52) %>% 
    tidylog::mutate(week_no = 53)
  
  # Add rows to historic data
  historic_data %<>% rbind(week_53)
  
  # Joining with 2020 and 2021 data
  # Filtering weeks with incomplete week too!! Temporary
  data_2020 <- left_join(dataset %>% filter(year(week_ending) %in% c("2020", "2021")), 
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
  # Setting < 5 counts to zero
  data_2020 <- data_2020 %>% 
    mutate(count = if_else(count < 5, 0, count),
           count_average = if_else(count_average < 5, 0, count_average),
           variation = if_else(count < 5, 0, variation))
  
  # Filter week
  data_2020 <- data_2020 %>%
    filter(week_ending <= as.Date(last_week))
  
  # Supressing numbers under 5
  #data_2020 <- data_2020 %>% filter(count>=5) %>% 
  #  filter(week_ending <= as.Date(last_week)) 
  
  final_data <<- data_2020
  
  saveRDS(data_2020, paste0("shiny_app/data/", filename,".rds"))
  saveRDS(data_2020, paste0(data_folder,"final_app_files/", filename, "_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  saveRDS(data_2020, paste0(open_data, filename,"_data.rds"))
  
  #saveRDS(data_2020, paste0("shiny_app/data/", filename,"_data.rds"))
  #saveRDS(data_2020, paste0("/conf/PHSCOVID19_Analysis/Publication outputs/open_data/", filename,"_data.rds"))
}

#Function to format the immunisations and child health review tables
format_immchild_table <- function(filename, save_as, save_file = T) {
  imm_ch_dt <- read_csv(paste0(data_folder, filename, ".csv")) %>%
    janitor::clean_names() %>%
    rename(area_name=geography_name) %>%
    select (-geography) %>%
    arrange (as.Date(eligible_date_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
    mutate(time_period_eligible=as.factor(time_period_eligible))
  
  if (save_file == T) {
  
  imm_ch_dt <<- imm_ch_dt
  
  saveRDS(imm_ch_dt, paste0("shiny_app/data/", save_as, "_datatable.rds"))
  saveRDS(imm_ch_dt, paste0(data_folder,"final_app_files/", save_as, "_datatable_", 
                                  format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  }
  
}

# Function for reading in immunisation SIMD data - could be improved once exactly what information is to be displayed is agreed
format_immsimd_data <- function(filename) {
  data_simd <-  read_csv(paste0(data_folder, filename, ".csv")) %>%
    janitor::clean_names() %>%
    mutate(eligible_start = case_when((str_length(eligible_start)<10) ~ paste0("0", eligible_start), 
                                      TRUE ~ eligible_start)) %>%
    arrange (cohort,as.Date(eligible_start, format="%m/%d/%Y")) %>% #ensure cohorts sort correctly in shiny flextable
    mutate(time_period_eligible = as.factor(case_when(cohort == "monthly" ~ paste0(toupper(substr(time_period_eligible, 1, 3)),
                                                          " 20",substring(time_period_eligible,5,6)), 
                                                      TRUE ~ time_period_eligible))) %>%
    rename(area_name = geography, simdq = simd2020v2_sc_quintile) %>%
    mutate(simdq=case_when(simdq == 6 ~"Scotland", simdq == 1 ~ "1 - most deprived",
                           simdq == 5 ~ "5 - least deprived", TRUE ~ as.character(simdq))) %>%
    # filtering out data where simd missing as small numbers lead to massive percentage differences.
    # filtering out Scotland totals as not plotted
    filter(!(simdq %in% c("0", "Scotland"))) %>% 
    droplevels()
  
  # Creating levels for factor in chronological order
  data_simd$time_period_eligible <- factor(data_simd$time_period_eligible, 
                                          levels=unique(data_simd$time_period_eligible[order(data_simd$eligible_start, decreasing = T)]), 
                                          ordered=TRUE)
  return(data_simd)
  saveRDS(data_simd, paste0("shiny_app/data/", save_as, "_simdtable.rds"))
  saveRDS(data_simd, paste0(data_folder,"final_app_files/", save_as, "_simdtable_", 
                                      format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
    }

#function to format child health data
format_childhealth <- function(filename, week_var, week_var2, save_as, 
                               intmin = 0, intmax = 10000) {
  
  ch_data <- read_csv(paste0(data_folder,"child_health/", filename, ".csv"), 
                      col_types =list(time_period_eligible=col_character())) %>%
    janitor::clean_names() %>% 
    mutate_at(week_var, ~as.Date(., format="%m/%d/%Y"))
  
  # Creating levels for factor in chronological order
  ch_data$time_period_eligible <- factor(ch_data$time_period_eligible,
                                         levels=unique(ch_data$time_period_eligible[order(ch_data[[week_var]], decreasing = T)]),
                                         ordered=TRUE)
  
  ch_data %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
    mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
           area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
           weeks=interv/7,
           week_no= isoweek({{week_var2}}),
           cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
    arrange(cohort) %>%
    select (extract_date, review, {{week_var2}}, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
    filter(between(interv, intmin, intmax))
  
  function_output <<- ch_data
  
  saveRDS(ch_data, paste0("shiny_app/data/", save_as, ".rds"))
  saveRDS(ch_data, paste0(data_folder,"final_app_files/", save_as, "_",
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
  
}

# Function to flag shifts and trends on run chart data
# Parameters:
# shift: name for new field where shift is flagged
# trend: name for new field where trend is flagged
# value: which column in dataset contains value being evaluated
# median: which column in dataset contains the median against which value is tested

runchart_flags <- function(dataset, shift, trend, value, median) {

  dataset <- dataset %>%
    mutate(shift_i = case_when(({{value}} > {{median}} & lag({{value}}, 1) > {{median}} & 
                                  lag({{value}}, 2) > {{median}} & lag({{value}}, 3) > {{median}} & 
                                  lag({{value}}, 4) > {{median}} & lag({{value}}, 5) > {{median}}) 
                               | ({{value}} < {{median}} & lag({{value}}, 1) < {{median}} & 
                                    lag({{value}}, 2) < {{median}} & lag({{value}}, 3) < {{median}} & 
                                    lag({{value}}, 4) < {{median}} & lag({{value}}, 5) < {{median}}) ~ T , T ~ F),
           shift = case_when(shift_i == T | lead(shift_i, 1) == T | lead(shift_i, 2) == T
                             | lead(shift_i, 3) == T | lead(shift_i, 4) == T
                             | lead(shift_i, 5) == T  ~ T, T ~ F),
           trend_i = case_when(({{value}} > lag({{value}} ,1) & lag({{value}}, 1) > lag({{value}}, 2)
                                & lag({{value}}, 2) > lag({{value}}, 3)  & lag({{value}}, 3) > lag({{value}}, 4)) |
                                 ({{value}} < lag({{value}} ,1) & lag({{value}}, 1) < lag({{value}}, 2)
                                  & lag({{value}}, 2) < lag({{value}}, 3)  & lag({{value}}, 3) < lag({{value}}, 4) )
                               ~ T , T ~ F),
           trend = case_when(trend_i == T | lead(trend_i, 1) == T | lead(trend_i, 2) == T
                             | lead(trend_i, 3) == T | lead(trend_i, 4) == T
                             ~ T, T ~ F)) %>%
    rename({{shift}}:=shift,{{trend}}:=trend) %>%
    select(-shift_i, -trend_i)

  }

##END
