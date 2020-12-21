# Data preparation for child health review data

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## Child health reviews ----
###############################################.
#First visit data and table
format_childhealth(filename = "firstvisit_dashboard20201207", week_var = "week_2_start",
                   week_var2 = week_2_start, save_as = "first_visit")

format_immchild_table("child_health/firstvisit_dashboardtab_20201207", "first_visit") 

# 6-8 weeks visit data and table
format_childhealth(filename = "sixtoeight_dashboard20201207", week_var = "week_6_start",
                   week_var2 = week_6_start, save_as = "six_to_eight", intmax = 168)

format_immchild_table("child_health/sixtoeight_dashboardtab_20201207", "six_to_eight") 

# 13-15 month visit data and table
format_childhealth(filename = "thirteen_dashboard20201207", week_var = "week_57_start",
                   week_var2 = week_57_start, save_as = "thirteen", intmin = 370, intmax = 519)

format_immchild_table("child_health/thirteen_dashboardtab_20201207", "thirteen") 

## 27 to 30 month visit data and table
format_childhealth(filename = "twentyseven_dashboard20201207", week_var = "week_117_start",
                   week_var2 = week_117_start, save_as = "twentyseven", intmin = 790, intmax = 946)

format_immchild_table("child_health/twentyseven_dashboardtab_20201207", "twentyseven") 

## 4 to 5 year visit data and table
format_childhealth(filename = "fourtofive_dashboard20201207", week_var = "week_209_start",
                   week_var2 = week_209_start, save_as = "fourtofive", intmin = 1427, intmax = 1583)

format_immchild_table("child_health/fourtofive_dashboardtab_20201207", "fourtofive") 

###############################################.
## Child development ----
###############################################.
# Do we need any sort of supression - look at island values.
child_dev <- rbind(read_excel(paste0(data_folder, "child_development/7thDecDashboard - 13-15m.xlsx")) %>% 
                     mutate(review = "13-15 month"),
                   read_excel(paste0(data_folder, "child_development/7thDecDashboard - 27-30m.xlsx")) %>% 
                     mutate(review = "27-30 month")) %>% 
  clean_names() %>% 
  rename(area_name = geography) %>% 
  mutate(area_type = case_when(area_name == "Scotland" ~ "Scotland",
                               stringr::str_sub(area_name, start = -4) == "HSCP" ~ "HSCP",
                               T ~ "Health board"),
         area_name = case_when(area_type=="Health board" ~ paste0("NHS ", area_name),  
                               TRUE ~ area_name),
         month_review = as.Date(month_review)) %>% 
  filter((year(month_review) %in% c("2019", "2020"))) 

child_dev %<>% # Dealing with NAs, which are 0s
  mutate_at(c("pc_1_plus", "concerns_1_plus"), ~replace_na(., 0)) %>% 
  #Glasgow is incomplete before May19, converting to NA
  mutate(no_reviews = case_when(area_name == "NHS Greater Glasgow & Clyde" & 
                                  review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ no_reviews),
         no_meaningful_reviews = case_when(area_name == "NHS Greater Glasgow & Clyde" & 
                                             review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ no_meaningful_reviews),
         concerns_1_plus = case_when(area_name == "NHS Greater Glasgow & Clyde" & 
                                       review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ concerns_1_plus),
         pc_1_plus = case_when(area_name == "NHS Greater Glasgow & Clyde" & 
                                 review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ pc_1_plus),
         pc_meaningful = case_when(area_name == "NHS Greater Glasgow & Clyde" & review == "13-15 month" &
                                  month_review< as.Date("2019-05-01") ~ NA_real_, T ~ pc_meaningful))

# Calculating centre lines and adding them to child_dev
child_dev_centreline_hb <- child_dev %>% 
  filter(month_review< as.Date("2020-03-01") & month_review>= as.Date("2019-01-01")) %>% 
  filter(!(area_name %in% c("Scotland", "NHS Greater Glasgow & Clyde") & review == "13-15 month")) %>% 
  select(area_name, review, pc_1_plus) %>% group_by(area_name, review) %>% 
  mutate(pc_1_plus_centreline = median(pc_1_plus)) %>% ungroup() %>% 
  select(-pc_1_plus) %>% unique

child_dev_centreline_scot <- child_dev %>% 
  filter(month_review< as.Date("2020-03-01") & month_review>= as.Date("2019-05-01")) %>% 
  filter(area_name %in% c("Scotland", "NHS Greater Glasgow & Clyde") & review == "13-15 month") %>% 
  select(area_name, review, pc_1_plus) %>% group_by(area_name, review) %>% 
  mutate(pc_1_plus_centreline = median(pc_1_plus)) %>% ungroup() %>% 
  select(-pc_1_plus) %>% unique

child_dev_centreline <- rbind(child_dev_centreline_hb, child_dev_centreline_scot)

child_dev %<>% left_join(child_dev_centreline) 

child_dev %<>% 
  group_by(area_name, area_type, review) %>% 
  runchart_flags(shift = "shift", trend = "trend", #shifts and trends for runcharts
                 value = pc_1_plus, median = pc_1_plus_centreline) %>% ungroup()

remove(child_dev_centreline, child_dev_centreline_scot, child_dev_centreline_hb)

saveRDS(child_dev, "shiny_app/data/child_dev.rds")
saveRDS(child_dev, paste0(data_folder,"final_app_files/child_dev_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Breastfeeding ----
###############################################.
breastfeeding <- bind_rows(read_xlsx(paste0(data_folder, "/breastfeeding/7thDecDashboard - firstvisit.xlsx")) %>% 
                         mutate(review = "First visit"),
                       read_xlsx(paste0(data_folder, "/breastfeeding/7thDecDashboard - 6-8 week.xlsx")) %>% 
                         mutate(review = "6-8 week")) %>% 
  clean_names() %>% 
  rename(area_name = geography) %>% 
  mutate(area_type = case_when(area_name == "Scotland" ~ "Scotland",
                               stringr::str_sub(area_name, start = -4) == "HSCP" ~ "HSCP",
                               T ~ "Health board"),
         area_name = case_when(area_type=="Health board" ~ paste0("NHS ", area_name),  
                               TRUE ~ area_name),
         month_review = as.Date(month_review)) %>% 
  filter((year(month_review) %in% c("2019", "2020")))

# Calculating centre lines and adding them to breastfeeding
breastfeeding_centreline <- breastfeeding %>% 
  filter(month_review< as.Date("2020-03-01") & month_review>= as.Date("2019-01-01")) %>% 
  select(area_name, review, pc_valid, pc_excl, pc_overall, pc_ever) %>% group_by(area_name, review) %>% 
  mutate(pc_valid_centreline = median(pc_valid),
         pc_excl_centreline = median(pc_excl),
         pc_overall_centreline = median(pc_overall),
         pc_ever_centreline = median(pc_ever)) %>% ungroup() %>% 
  select(-c(pc_valid, pc_excl, pc_overall, pc_ever)) %>% unique

breastfeeding <- breastfeeding %>% left_join(breastfeeding_centreline)

breastfeeding %<>% 
  group_by(area_name, area_type, review) %>% 
  runchart_flags(shift = "shift_excl", trend = "trend_excl", #shifts and trends for runcharts
                 value = pc_excl, median = pc_excl_centreline) %>% 
  runchart_flags(shift = "shift_over", trend = "trend_over", #shifts and trends for runcharts
                 value = pc_overall, median = pc_overall_centreline) %>% 
  runchart_flags(shift = "shift_ever", trend = "trend_ever", #shifts and trends for runcharts
                 value = pc_ever, median = pc_ever_centreline) %>% 
  ungroup

remove(breastfeeding_centreline)

saveRDS(breastfeeding, "shiny_app/data/breastfeeding.rds")
saveRDS(breastfeeding, paste0(data_folder,"final_app_files/breastfeeding_", 
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


##END
