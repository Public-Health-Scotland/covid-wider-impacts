##########################################
#
# DCE Data prep for
# PHS COVID-Wider-Impact dashboard
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
# Import and Tidy Data
##########################################

# TESTING NEW DATA
# dce <- read_csv("data/DCE Staging Data DashFormat 2019-2020 081021.csv") %>%  

##########################################

# NEW SECTION TO INCLUDE REFORMATTING OF BOXI DATA

input_folder <- paste0("////PHI_conf//CancerGroup1//Topics//DetectCancerEarly//Investigations",
                       "//20201109-DCE-Covid-Monthly-Analysis//Data//")


dce_BI_extract <- read_csv(paste0(input_folder,"DCE Staging Data - Dashboard Data 8-10-21.csv"), col_names = T) %>%  
  clean_names() %>% 
  replace(is.na(.), 0) %>% 
  arrange(year, month)

dce_counts <- dce_BI_extract %>% 
  select(-c(stage_1_percent, stage_2_percent, stage_3_percent, stage_4_percent, stage_9_percent, total)) %>% 
  pivot_longer(cols = c(stage_1:stage_9),
               names_to = "stage",
               values_to = "number") 

dce_percent <- dce_BI_extract %>% 
  select(-c(stage_1, stage_2, stage_3, stage_4, stage_9, total)) %>%
  rename(stage_1 = stage_1_percent,
         stage_2 = stage_2_percent,
         stage_3 = stage_3_percent,
         stage_4 = stage_4_percent,
         stage_9 = stage_9_percent) %>% 
  pivot_longer(cols = c(stage_1:stage_9),
               names_to = "stage",
               values_to = "percent") 

dce_dash_data <- left_join(dce_counts, dce_percent) %>% 
  mutate(stage = case_when(stage == "stage_1" ~ 1,
                           stage == "stage_2" ~ 2,
                           stage == "stage_3" ~ 3,
                           stage == "stage_4" ~ 4,
                           stage == "stage_9" ~ 9)) 

#####################################################################

dce <- dce_dash_data %>% 
  clean_names() %>%
  mutate (stage = as.character(stage)) %>% 
  mutate (stage = case_when(stage == "9" ~ "NK",
                            TRUE ~ stage))

dce$stage <- factor(dce$stage, levels = c("1", "2", "3", "4", "NK"))


dce <- dce %>% 
  filter(year != "2018" & area != "Scotland"
         & area != "NCA" & area != "SCAN" & area != "WOSCAN") %>%
  rename(count = number) %>% 
  mutate (count = as.numeric(sub("-", "", count))) %>%
  mutate (site = as.factor(site)) %>%
  mutate(region = case_when(area %in% c("NHS GRAMPIAN", "NHS HIGHLAND", "NHS ORKNEY",
                                        "NHS SHETLAND", "NHS TAYSIDE", "NHS WESTERN ISLES") ~ "NCA",
                            area %in% c("NHS BORDERS", "NHS DUMFRIES & GALLOWAY", 
                                        "NHS FIFE", "NHS LOTHIAN") ~ "SCAN",
                            area %in% c("NHS AYRSHIRE & ARRAN", "NHS FORTH VALLEY", 
                                        "NHS GREATER GLASGOW & CLYDE", "NHS LANARKSHIRE") ~ "WOSCAN")) %>% 
  select(-percent) %>% 
  replace(is.na(.), 0)


dce$area <-  recode(dce$area, "NHS AYRSHIRE & ARRAN" = "NHS Ayrshire & Arran",
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

#
##########################################
# Add totals by network and for Scotland
##########################################

# Networks

dce_networks <- dce %>%
  mutate(area = case_when(area == "NHS Grampian" ~ "NCA",
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
                          area == "NHS Lanarkshire" ~ "WOSCAN")) %>%
  count(area, site, stage, year, month, wt = count) %>%
  rename(count = "n") %>% 
  mutate(region = area) 

# Scotland

dce_scotland <- dce %>%
  mutate(area = "Scotland") %>%
  count(area, site, stage, year, month, wt = count) %>%
  rename(count = "n") %>% 
  mutate(region = "")

dce_scotland <- bind_rows(dce_scotland, dce_networks) %>% 
  mutate(region = "Scotland")

# Combine all values

dce_all_areas <- bind_rows(dce, dce_networks, dce_scotland) 

rm(dce, dce_networks, dce_scotland)



dce_data <-  dce_all_areas %>% 
  count(region, area, site, stage, year, month, wt = count) %>% 
  spread(year, n) %>%
  rename(count19 = "2019", count20 = "2020") 

dce_data_19 <- dce_data %>%
  count(region, area, site, month, wt = count19) %>% 
  rename(total19 = "n")  


dce_data_20 <- dce_data %>%
  count(region, area, site, month, wt = count20) %>% 
  rename(total20 = "n")  


dce_data_total <- full_join(dce_data_19, dce_data_20)  
rm(dce_data_19, dce_data_20)


dce_data_total <- left_join(dce_data, dce_data_total) %>%
  mutate(ratio = count20/count19,
         percent19 = format(round(((count19/total19)*100), 2)),
         percent20 = format(round(((count20/total20)*100), 2)),
         difference = case_when(count19 > 0 ~ ((count20 - count19)/count19)*100,
                                count19 == 0 & count20 > 0 ~ (count20*100), TRUE ~ 0)) %>% 
  mutate(month_name = as.factor(month)) 


levels(dce_data_total$month_name) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                                       "Sep", "Oct", "Nov", "Dec")

dce_data_total <- dce_data_total %>%
  select(-month) %>% 
  rename(month = month_name)

rm(dce_data, dce_BI_extract, dce_counts, dce_dash_data, dce_percent)

saveRDS(dce_data_total, "shiny_app/data/dce_data.rds")
# saveRDS(dce_all_areas, "shiny_app/data/dce_data_areas.rds")

saveRDS(dce_data_total, paste0("/conf/PHSCOVID19_Analysis/shiny_input_files/final_app_files/", "dce_data", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))



