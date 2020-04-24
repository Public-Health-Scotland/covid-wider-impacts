# Data preparation for app

###############################################.
## Packages ----
###############################################.
library(dplyr)
library(janitor)

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

###############################################.
## Reading RAPID data ----
###############################################.
# Prepare by Unscheduled care team
rap_adm <- readRDS("/conf/PHSCOVID19_Analysis/Admissions_by_category.rds") %>% 
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
  mutate(category = recode(age_group, "15_thru_44" = "15 - 44", "45_thru_64" = "45 - 64",
                           "65_thru_74" = "65 -74", "75_thru_84" = "75 -84",
                           "85+" = "Over 85", "Under_5" = "Under 5")) %>% 
  select(-age_group)

# Totals for overalls for deprivation quintiles
rap_adm_depr <- agg_rapid(c("simd_quintile"), split = "depr") %>% 
  rename(category = simd_quintile) %>% 
  mutate(category = as.character(category),
         category = recode(category, "1" = "1 - most deprived", "5" = "5 - Least deprived"))
  
rap_adm <- rbind(rap_adm_all, rap_adm_depr, rap_adm_sex, rap_adm_age) %>% 
  # Filtering cases without information on age, sex or deprivation (still counted in all)
  filter(!(is.na(category) | 
             area_name %in% c("ENGLAND/WALES/NORTHERN IRELAND", "UNKNOWN HSCP - SCOTLAND"))) %>% 
  rename(date = date_adm) %>% 
  # Creating area type variable
  mutate(area_type = case_when(substr(area_name, 1,3) == "NHS" ~ "Health board",
                               area_name == "Scotland" ~ "Scotland",
                               TRUE ~ "Council area"),
         admission_type = recode(admission_type, "elective" = "Planned", "emergency" = "Emergency"))

saveRDS(rap_adm, "shiny_app/data/rapid_data.rds")

##END