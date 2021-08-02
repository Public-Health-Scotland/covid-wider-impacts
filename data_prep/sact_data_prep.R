##########################################
#
# SACT Data prep - WORKING COPY for
# PHS COVID-Wider-Impact dashboard
# (original version saved)
#
# M.Turner - Cancer Team
# 
##########################################

##########################################
# Housekeeping
##########################################

require(tidyverse)||install.packages("tidyverse")
require(janitor)||install.packages("janitor")
require(lubridate)||install.packages("lubridate")

##########################################
# Date Input (Monday after data refresh)

year <- "2021"
month <- "08"
day <- "02"

release_date <- paste(year, month, day, sep = "-")
path_monthly_data <- paste0("////PHI_conf//CancerGroup1//Topics//CancerStatistics//Projects//20210205-SACT-dashboard//R Shiny//SACT Dashboard Data//SummaryMonthlyPatients-", release_date, ".csv")
path_weekly_data <- paste0("////PHI_conf//CancerGroup1//Topics//CancerStatistics//Projects//20210205-SACT-dashboard//R Shiny//SACT Dashboard Data//WeeklySACTActivityDashboard-", release_date, ".csv")

rm(year, month, day, release_date)

##########################################


########################################################################

# 1. MONTHLY DATA

# seperate 2020 and 2021 data, rename vars
sact20 <- read_csv(path_monthly_data) %>%
  clean_names() %>%
  rename(site = tumour_group, area = treatment_hb_name,
         year = appointment_year, month = appointment_month,
         region = cancer_network, All = all, Intravenous = iv,
         Oral = oral, Subcutaneous = sc, Intrathecal = it, Other = other) %>%
  filter(year != 2021) %>%
  replace(is.na(.), "Unknown") %>% 
  select(-year)

sact21 <- read_csv(path_monthly_data) %>%
  clean_names() %>%
  rename(site = tumour_group, area = treatment_hb_name,
         year = appointment_year, month = appointment_month,
         region = cancer_network, All = all, Intravenous = iv,
         Oral = oral, Subcutaneous = sc, Intrathecal = it, Other = other) %>%
  filter(year == 2021) %>%
  replace(is.na(.), "Unknown") %>% 
  mutate(month = month + 12) %>% 
  select(-year)

sact <- rbind(sact20, sact21)

sact$area <-  recode(sact$area, "NHS AYRSHIRE AND ARRAN" = "NHS Ayrshire & Arran",
                     "NHS BORDERS" = "NHS Borders",
                     "NHS DUMFRIES & GALLOWAY" = "NHS Dumfries & Galloway",
                     "NHS FIFE" = "NHS Fife",
                     "NHS FORTH VALLEY" = "NHS Forth Valley",
                     "NHS GRAMPIAN" = "NHS Grampian",
                     "NHS GREATER GLASGOW & CLYDE" = "NHS Greater Glasgow & Clyde",
                     "NHS HIGHLAND" = "NHS Highland",
                     "NHS LANARKSHIRE" = "NHS Lanarkshire",
                     "NHS LOTHIAN" = "NHS Lothian",
                     "NHS ORKNEY" = "NHS Orkney",
                     "NHS SHETLAND" = "NHS Shetland",
                     "NHS TAYSIDE" = "NHS Tayside",
                     "NHS WESTERN ISLES" = "NHS Western Isles")

sact$site <-  recode(sact$site, "BONE SARCOMA" = "Bone Sarcoma",
                     "BREAST" = "Breast",
                     "CNS" = "Central Nervous System",
                     "CUP" = "Cancer of Unknown Origin",
                     "EXCLUDE" = "Other",
                     "GERM CELL" = "Germ Cell",
                     "GYNAECOLOGY" = "Gynaecology",
                     "HAEMATOLOGY" = "Haematology",
                     "HEAD AND NECK" = "Head & Neck",
                     "LOWER GI" = "Lower GI",
                     "LUNG AND CHEST" = "Lung & Chest",
                     "NEUROENDOCRINE" = "Neuroendocrine",
                     "SKIN" = "Skin",
                     "SOFT TISSUE SARCOMA" = "Soft Tissue Sarcoma",
                     "UPPER GI" = "Upper GI",
                     "UROLOGICAL" = "Urological",
                     "Unknown" = "Unknown")

# convert to long dataset format
sact_long <- sact %>% 
  group_by(month, region, area, site) %>% 
  pivot_longer(All:Intrathecal, names_to = "treatment", values_to = "count") %>% 
  ungroup

sact_long <- sact_long %>%
  mutate(region = if_else((area %in% c("NCA", "SCAN", "WOSCAN")),"Scotland",region)) %>%
  mutate(month_name = as.factor(month))

levels(sact_long$month_name) <- c("Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20",
                                  "Sep-20", "Oct-20", "Nov-20", "Dec-20", "Jan-21", "Feb-21", "Mar-21",
                                  "Apr-21", "May-21", "Jun-21")

sact <- sact_long %>% 
  select(-month) %>% 
  rename(month = month_name) 


rm(sact_long)

saveRDS(sact, paste0("/conf/PHSCOVID19_Analysis/shiny_input_files/final_app_files/", "sact_data_", 
                     format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

saveRDS(sact, "shiny_app/data/sact_data.rds")

################################################################


# 2. WEEKLY DATA 

# read in 2021 data and recode week number values
sact_weekly_new <- read_csv(path_weekly_data) %>%  
  clean_names() %>% 
  select(iso_appointment_year:other_regimen_level, it_appointment_level:week_beginning) %>% 
  filter(iso_appointment_year == "2021" & treatment_hb_name != "Unmapped") %>% 
  mutate(appointment_week = appointment_week + 53) %>% 
  select(-iso_appointment_year)

sact_weekly <- read_csv(path_weekly_data) %>%  
  clean_names() %>% 
  select(iso_appointment_year:other_regimen_level, it_appointment_level:week_beginning) %>% 
  filter(iso_appointment_year != "2021" & treatment_hb_name != "Unmapped") %>% 
  select(-iso_appointment_year)

# rename vars and recode region for networks
sact_weekly_new <- rbind(sact_weekly, sact_weekly_new) %>% 
  rename(site = tumour_group, area = treatment_hb_name,
         week = appointment_week,
         region = cancer_network) %>%
  mutate(region = if_else((area %in% c("NCA", "SCAN", "WOSCAN")),"Scotland",region)) %>% 
  replace(is.na(.), "Unknown")

sact_weekly_new$area <-  recode(sact_weekly_new$area, "NHS AYRSHIRE AND ARRAN" = "NHS Ayrshire & Arran",
                                "NHS BORDERS" = "NHS Borders",
                                "NHS DUMFRIES & GALLOWAY" = "NHS Dumfries & Galloway",
                                "NHS FIFE" = "NHS Fife",
                                "NHS FORTH VALLEY" = "NHS Forth Valley",
                                "NHS GRAMPIAN" = "NHS Grampian",
                                "NHS GREATER GLASGOW & CLYDE" = "NHS Greater Glasgow & Clyde",
                                "NHS HIGHLAND" = "NHS Highland",
                                "NHS LANARKSHIRE" = "NHS Lanarkshire",
                                "NHS LOTHIAN" = "NHS Lothian",
                                "NHS ORKNEY" = "NHS Orkney",
                                "NHS SHETLAND" = "NHS Shetland",
                                "NHS TAYSIDE" = "NHS Tayside",
                                "NHS WESTERN ISLES" = "NHS Western Isles")

sact_weekly_new$site <-  recode(sact_weekly_new$site, "BONE SARCOMA" = "Bone Sarcoma",
                                "BREAST" = "Breast",
                                "CNS" = "Central Nervous System",
                                "CUP" = "Cancer of Unknown Origin",
                                "EXCLUDE" = "Other",
                                "GERM CELL" = "Germ Cell",
                                "GYNAECOLOGY" = "Gynaecology",
                                "HAEMATOLOGY" = "Haematology",
                                "HEAD AND NECK" = "Head & Neck",
                                "LOWER GI" = "Lower GI",
                                "LUNG AND CHEST" = "Lung & Chest",
                                "NEUROENDOCRINE" = "Neuroendocrine",
                                "SKIN" = "Skin",
                                "SOFT TISSUE SARCOMA" = "Soft Tissue Sarcoma",
                                "UPPER GI" = "Upper GI",
                                "UROLOGICAL" = "Urological",
                                "Unknown" = "Unknown")


# divide dataset into Regimen and Appointment level 
sact_weekly_reg <- sact_weekly_new %>% 
  select(week:site, all_appointments, it_regimen_level:other_regimen_level) %>%
  rename(All = all_appointments, Intrathecal = it_regimen_level, Intravenous = iv_regimen_level, 
         Oral = oral_regimen_level, Subcutaneous = sc_regimen_level, Other = other_regimen_level) %>%
  mutate(appt_reg = "Regimen level") 


sact_weekly_appt <- sact_weekly_new %>% 
  select(week:site, all_appointments, it_appointment_level:other_appointment_level) %>%
  rename(All = all_appointments, Intrathecal = it_appointment_level, Intravenous = iv_appointment_level, Oral = oral_appointment_level,
         Subcutaneous = sc_appointment_level, Other = other_appointment_level) %>% 
  mutate(appt_reg = "Appointment level") 


# create long format dataset and populate null value weeks with 0
sact_weekly <- rbind(sact_weekly_reg, sact_weekly_appt)  %>%  
  group_by(week, region, area, site) %>%
  pivot_longer(All:Other, names_to = "treatment", values_to = "count") %>%
  ungroup() %>%  
  select(-region) %>% 
  complete(week, nesting(area, site, treatment, appt_reg), fill = list(count = 0)) %>% 
  complete(area, nesting(week, site, treatment, appt_reg), fill = list(count = 0)) %>% 
  mutate(region = case_when(area == "NHS Grampian" ~ "NCA",
                            area == "NHS Highland" ~ "NCA",
                            area == "NHS Orkney" ~ "NCA",
                            area == "NHS Shetland" ~ "NCA",
                            area == "NHS Tayside" ~ "NCA",
                            area == "NHS Western Isles" ~ "NCA",
                            area == "NHS Borders" ~ "SCAN",
                            area == "NHS Dumfries & Galloway" ~ "SCAN",
                            area == "NHS Fife" ~ "SCAN",
                            area == "NHS Lothian" ~ "SCAN",
                            area == "NHS Ayrshire & Arran" ~ "WOSCAN",
                            area == "NHS Forth Valley" ~ "WOSCAN",
                            area == "NHS Greater Glasgow & Clyde" ~ "WOSCAN",
                            area == "NHS Lanarkshire" ~ "WOSCAN",
                            TRUE ~ "Scotland"))

# calculate reference week from mean count of weeks 4-9
sact_weekly_ave <- sact_weekly %>%
  filter(week >= 4 & week <= 9) %>%
  group_by(region, area, site, treatment, appt_reg) %>%
  mutate(five_wk_ave = mean(count)) %>%
  ungroup() %>%
  select(-week) %>%
  select(-count)

sact_weekly <- left_join(sact_weekly, sact_weekly_ave) %>%
  distinct() %>%
  mutate(week_on_week_diff = (count-five_wk_ave)) %>%
  replace(is.na(.), 0) %>%
  mutate(week_on_refweek_perc = case_when(five_wk_ave != 0 ~ (week_on_week_diff/five_wk_ave)*100,
                                          TRUE ~ week_on_week_diff*100)) %>%
  select(-c(week_on_week_diff, five_wk_ave))


# add week beginning variable
sact_weekly <- sact_weekly %>%
  mutate(week_beginning = dmy("30/12/2019") + days(7*(week-1))) 


rm(sact_weekly_new, sact_weekly_appt, sact_weekly_reg, sact_weekly_ave)


saveRDS(sact_weekly, paste0("/conf/PHSCOVID19_Analysis/shiny_input_files/final_app_files/", "sact_weekly_data_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

saveRDS(sact_weekly, "shiny_app/data/sact_weekly_data.rds")