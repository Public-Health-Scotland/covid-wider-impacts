# Data preparation for child health review data

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## Child health review: first visit ----
###############################################.
## First visit - scurve data
first <- read_csv(paste0(data_folder,"child_health/firstvisit_dashboard20201207.csv"), 
                col_types =list(week_2_start=col_date(format="%m/%d/%Y"),
                                time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
first$time_period_eligible <- factor(first$time_period_eligible, 
                                     levels=unique(first$time_period_eligible[order(first$week_2_start, decreasing = T)]), 
                                     ordered=TRUE)

first %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_2_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_2_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) 

saveRDS(first, paste0("shiny_app/data/","first_visit.rds"))
saveRDS(first, paste0(data_folder,"final_app_files/first_visit_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# First visit - summary table data
first_datatable <- format_immchild_table("child_health/firstvisit_dashboardtab_20201207") 

saveRDS(first_datatable, paste0("shiny_app/data/","first_visit_datatable.rds"))
saveRDS(first_datatable, paste0(data_folder,"final_app_files/first_visit_datatable_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: 6-8 weeks  ----
###############################################.

## 6 to 8 weeks visit - scurve data
sixtoeight <- read_csv(paste0(data_folder,"child_health/sixtoeight_dashboard20201207.csv"), 
                  col_types =list(week_6_start=col_date(format="%m/%d/%Y"),
                                  time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
sixtoeight$time_period_eligible <- factor(sixtoeight$time_period_eligible, 
                                          levels=unique(sixtoeight$time_period_eligible[order(sixtoeight$week_6_start, decreasing = T)]), 
                                          ordered=TRUE)

sixtoeight %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_6_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_6_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv<168) 


saveRDS(sixtoeight, paste0("shiny_app/data/","six_to_eight.rds"))
saveRDS(sixtoeight, paste0(data_folder,"final_app_files/six_to_eight_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# 6-8 weeks visit - summary table data
sixtoeight_datatable <- format_immchild_table("child_health/sixtoeight_dashboardtab_20201207") 

saveRDS(sixtoeight_datatable, paste0("shiny_app/data/","six_to_eight_datatable.rds"))
saveRDS(sixtoeight_datatable, paste0(data_folder,"final_app_files/six_to_eight_datatable_", 
                           format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: 13-15 month ----
###############################################.

## 13 to 15 month visit - scurve data
thirteen <- read_csv(paste0(data_folder,"child_health/thirteen_dashboard20201207.csv"), 
                       col_types =list(week_57_start=col_date(format="%m/%d/%Y"),
                                       time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
thirteen$time_period_eligible <- factor(thirteen$time_period_eligible, 
                                        levels=unique(thirteen$time_period_eligible[order(thirteen$week_57_start, decreasing = T)]), 
                                        ordered=TRUE)

thirteen %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_57_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_57_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv>=371 & interv<=518) 

saveRDS(thirteen, paste0("shiny_app/data/","thirteen.rds"))
saveRDS(thirteen, paste0(data_folder,"final_app_files/thirteen_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# 13 to 15 month visit - summary table data
thirteen_datatable <- format_immchild_table("child_health/thirteen_dashboardtab_20201207") 

saveRDS(thirteen_datatable, paste0("shiny_app/data/","thirteen_datatable.rds"))
saveRDS(thirteen_datatable, paste0(data_folder,"final_app_files/thirteen_datatable_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: 27-30 month ----
###############################################.

## 27 to 30 month visit - scurve data
twentyseven <- read_csv(paste0(data_folder,"child_health/twentyseven_dashboard20201207.csv"), 
                     col_types =list(week_117_start=col_date(format="%m/%d/%Y"),
                                     time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
twentyseven$time_period_eligible <- factor(twentyseven$time_period_eligible, 
                                           levels=unique(twentyseven$time_period_eligible[order(twentyseven$week_117_start, decreasing = T)]), 
                                           ordered=TRUE)

twentyseven %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_117_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_117_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv>=791 & interv<=945) 

saveRDS(twentyseven, paste0("shiny_app/data/","twentyseven.rds"))
saveRDS(twentyseven, paste0(data_folder,"final_app_files/twentyseven_", 
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


# 27 to 30 month visit - summary table data
# Data for data download should include complete months and all weeks
twentyseven_datatable <- format_immchild_table("child_health/twentyseven_dashboardtab_20201207") 

saveRDS(twentyseven_datatable, paste0("shiny_app/data/","twentyseven_datatable.rds"))
saveRDS(twentyseven_datatable, paste0(data_folder,"final_app_files/twentyseven_datatable_", 
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Child health review: 4-5 year ----
###############################################.

## 4 to 5 year visit - scurve data
fourtofive <- read_csv(paste0(data_folder,"child_health/fourtofive_dashboard20201207.csv"), 
                        col_types =list(week_209_start=col_date(format="%m/%d/%Y"),
                                        time_period_eligible=col_character())) %>%
  janitor::clean_names() 

# Creating levels for factor in chronological order
fourtofive$time_period_eligible <- factor(fourtofive$time_period_eligible, 
                                          levels=unique(fourtofive$time_period_eligible[order(fourtofive$week_209_start, decreasing = T)]), 
                                          ordered=TRUE)

fourtofive %<>% left_join(hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(week_209_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, review, week_209_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no) %>% 
  filter(interv>=1428 & interv<=1582) 

saveRDS(fourtofive, paste0("shiny_app/data/","fourtofive.rds"))
saveRDS(fourtofive, paste0(data_folder,"final_app_files/fourtofive_", 
                                     format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
# 4 to 5 year review - summary table data
# Data for data download should include complete months and all weeks
fourtofive_datatable <- format_immchild_table("child_health/fourtofive_dashboardtab_20201207")

saveRDS(fourtofive_datatable, paste0("shiny_app/data/","fourtofive_datatable.rds"))
saveRDS(fourtofive_datatable, paste0(data_folder,"final_app_files/fourtofive_datatable_", 
                                      format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

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
  mutate(# Shift: run of 6 or more consecutive data points above or below the pc_1_plus_centreline
         # First id when this run is happening and then iding all points part of it
         shift_i = case_when((pc_1_plus > pc_1_plus_centreline & lag(pc_1_plus, 1) > pc_1_plus_centreline 
                              & lag(pc_1_plus, 2) > pc_1_plus_centreline & lag(pc_1_plus, 3) > pc_1_plus_centreline 
                              & lag(pc_1_plus, 4) > pc_1_plus_centreline & lag(pc_1_plus, 5) > pc_1_plus_centreline) |
                               (pc_1_plus < pc_1_plus_centreline & lag(pc_1_plus, 1) < pc_1_plus_centreline 
                                & lag(pc_1_plus, 2) < pc_1_plus_centreline & lag(pc_1_plus, 3) < pc_1_plus_centreline 
                                & lag(pc_1_plus, 4) < pc_1_plus_centreline & lag(pc_1_plus, 5) < pc_1_plus_centreline) ~ T , T ~ F),
         shift = case_when(shift_i == T | lead(shift_i, 1) == T | lead(shift_i, 2) == T
                           | lead(shift_i, 3) == T | lead(shift_i, 4) == T
                           | lead(shift_i, 5) == T  ~ T, T ~ F),
         # Trend: A run of 5 or more consecutive data points
         trend_i = case_when((pc_1_plus > lag(pc_1_plus ,1) & lag(pc_1_plus, 1) > lag(pc_1_plus, 2) 
                              & lag(pc_1_plus, 2) > lag(pc_1_plus, 3)  & lag(pc_1_plus, 3) > lag(pc_1_plus, 4)) |
                               (pc_1_plus < lag(pc_1_plus ,1) & lag(pc_1_plus, 1) < lag(pc_1_plus, 2) 
                                & lag(pc_1_plus, 2) < lag(pc_1_plus, 3)  & lag(pc_1_plus, 3) < lag(pc_1_plus, 4) )  
                             ~ T , T ~ F),
         trend = case_when(trend_i == T | lead(trend_i, 1) == T | lead(trend_i, 2) == T
                           | lead(trend_i, 3) == T | lead(trend_i, 4) == T
                             ~ T, T ~ F)) %>% 
  select(-shift_i, -trend_i) %>% ungroup()

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
  mutate(# Shift: run of 6 or more consecutive data points above or below the pc_1_plus_centreline
    # First id when this run is happening and then iding all points part of it
    shift_i_excl = case_when((pc_excl > pc_excl_centreline & lag(pc_excl, 1) > pc_excl_centreline 
                         & lag(pc_excl, 2) > pc_excl_centreline & lag(pc_excl, 3) > pc_excl_centreline 
                         & lag(pc_excl, 4) > pc_excl_centreline & lag(pc_excl, 5) > pc_excl_centreline) |
                          (pc_excl < pc_excl_centreline & lag(pc_excl, 1) < pc_excl_centreline 
                           & lag(pc_excl, 2) < pc_excl_centreline & lag(pc_excl, 3) < pc_excl_centreline 
                           & lag(pc_excl, 4) < pc_excl_centreline & lag(pc_excl, 5) < pc_excl_centreline) ~ T , T ~ F),
    shift_excl = case_when(shift_i_excl == T | lead(shift_i_excl, 1) == T | lead(shift_i_excl, 2) == T
                      | lead(shift_i_excl, 3) == T | lead(shift_i_excl, 4) == T
                      | lead(shift_i_excl, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points
    trend_i_excl = case_when((pc_excl > lag(pc_excl ,1) & lag(pc_excl, 1) > lag(pc_excl, 2) 
                         & lag(pc_excl, 2) > lag(pc_excl, 3)  & lag(pc_excl, 3) > lag(pc_excl, 4)) |
                          (pc_excl < lag(pc_excl ,1) & lag(pc_excl, 1) < lag(pc_excl, 2) 
                           & lag(pc_excl, 2) < lag(pc_excl, 3)  & lag(pc_excl, 3) < lag(pc_excl, 4) )  
                        ~ T , T ~ F),
    trend_excl = case_when(trend_i_excl == T | lead(trend_i_excl, 1) == T | lead(trend_i_excl, 2) == T
                      | lead(trend_i_excl, 3) == T | lead(trend_i_excl, 4) == T
                      ~ T, T ~ F)) %>% 
  mutate(# Shift: run of 6 or more consecutive data points above or below the pc_overall_centreline
    # First id when this run is happening and then iding all points part of it
    shift_i_over = case_when((pc_overall > pc_overall_centreline & lag(pc_overall, 1) > pc_overall_centreline 
                              & lag(pc_overall, 2) > pc_overall_centreline & lag(pc_overall, 3) > pc_overall_centreline 
                              & lag(pc_overall, 4) > pc_overall_centreline & lag(pc_overall, 5) > pc_overall_centreline) |
                               (pc_overall < pc_overall_centreline & lag(pc_overall, 1) < pc_overall_centreline 
                                & lag(pc_overall, 2) < pc_overall_centreline & lag(pc_overall, 3) < pc_overall_centreline 
                                & lag(pc_overall, 4) < pc_overall_centreline & lag(pc_overall, 5) < pc_overall_centreline) ~ T , T ~ F),
    shift_over = case_when(shift_i_over == T | lead(shift_i_over, 1) == T | lead(shift_i_over, 2) == T
                           | lead(shift_i_over, 3) == T | lead(shift_i_over, 4) == T
                           | lead(shift_i_over, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points
    trend_i_over = case_when((pc_overall > lag(pc_overall ,1) & lag(pc_overall, 1) > lag(pc_overall, 2) 
                              & lag(pc_overall, 2) > lag(pc_overall, 3)  & lag(pc_overall, 3) > lag(pc_overall, 4)) |
                               (pc_overall < lag(pc_overall ,1) & lag(pc_overall, 1) < lag(pc_overall, 2) 
                                & lag(pc_overall, 2) < lag(pc_overall, 3)  & lag(pc_overall, 3) < lag(pc_overall, 4) )  
                             ~ T , T ~ F),
    trend_over = case_when(trend_i_over == T | lead(trend_i_over, 1) == T | lead(trend_i_over, 2) == T
                           | lead(trend_i_over, 3) == T | lead(trend_i_over, 4) == T
                           ~ T, T ~ F)) %>% 
  mutate(# Shift: run of 6 or more consecutive data points above or below the pc_ever_centreline
    # First id when this run is happening and then iding all points part of it
    shift_i_ever = case_when((pc_ever > pc_ever_centreline & lag(pc_ever, 1) > pc_ever_centreline 
                              & lag(pc_ever, 2) > pc_ever_centreline & lag(pc_ever, 3) > pc_ever_centreline 
                              & lag(pc_ever, 4) > pc_ever_centreline & lag(pc_ever, 5) > pc_ever_centreline) |
                               (pc_ever < pc_ever_centreline & lag(pc_ever, 1) < pc_ever_centreline 
                                & lag(pc_ever, 2) < pc_ever_centreline & lag(pc_ever, 3) < pc_ever_centreline 
                                & lag(pc_ever, 4) < pc_ever_centreline & lag(pc_ever, 5) < pc_ever_centreline) ~ T , T ~ F),
    shift_ever = case_when(shift_i_ever == T | lead(shift_i_ever, 1) == T | lead(shift_i_ever, 2) == T
                           | lead(shift_i_ever, 3) == T | lead(shift_i_ever, 4) == T
                           | lead(shift_i_ever, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points
    trend_i_ever = case_when((pc_ever > lag(pc_ever ,1) & lag(pc_ever, 1) > lag(pc_ever, 2) 
                              & lag(pc_ever, 2) > lag(pc_ever, 3)  & lag(pc_ever, 3) > lag(pc_ever, 4)) |
                               (pc_ever < lag(pc_ever ,1) & lag(pc_ever, 1) < lag(pc_ever, 2) 
                                & lag(pc_ever, 2) < lag(pc_ever, 3)  & lag(pc_ever, 3) < lag(pc_ever, 4) )  
                             ~ T , T ~ F),
    trend_ever = case_when(trend_i_ever == T | lead(trend_i_ever, 1) == T | lead(trend_i_ever, 2) == T
                           | lead(trend_i_ever, 3) == T | lead(trend_i_ever, 4) == T
                           ~ T, T ~ F)) %>% 
  select(-shift_i_ever, -trend_i_ever, -shift_i_excl, -trend_i_excl, -shift_i_over, -trend_i_over) %>% 
  ungroup

remove(breastfeeding_centreline)

saveRDS(breastfeeding, "shiny_app/data/breastfeeding.rds")
saveRDS(breastfeeding, paste0(data_folder,"final_app_files/breastfeeding_", 
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


##END
