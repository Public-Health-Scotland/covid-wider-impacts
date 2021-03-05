# Data preparation for births and babies tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## Mode of delivery ----
###############################################.
#field with date data files prepared
mod_folder <- "20210215"
mod_date <- "2021-02-15"

##mode of delivery data supplied in 4 files: runchart data, line charts for scotland (age and dep split), line charts for NHS board and data download

## 1-RUNCHART DATA
## mod data for run chart (scotland and nhs board) - monthly
mod_runchart <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",mod_folder,"/WI_DELIVERIES_RUNCHART_mode_",mod_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name = hbres, month = date) %>%
  mutate(month = as.Date(month),
         type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland"),
         area_type = type, 
         category = "All") %>%
  # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
  # ext_ columns are extended median which are blank before projection time period
  mutate(ext_csection_all = case_when(is.na(ext_csection_all) ~ median_csection_all,
                                      TRUE ~ ext_csection_all),
         ext_csection_elec = case_when(is.na(ext_csection_elec) ~ median_csection_elec,
                                       TRUE ~ ext_csection_elec),
         ext_csection_emer = case_when(is.na(ext_csection_emer) ~ median_csection_emer,
                                       TRUE ~ ext_csection_emer)) %>% 
  group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
  #call function to add flags for runchart shifts and trends
  #shift: name for new field where shift is flagged
  #trend: name for new field where trend is flagged
  #value: which column in dataset contains value being evaluated
  #median: which column in dataset contains the median against which value is tested
  runchart_flags(shift="csection_all_shift", trend="csection_all_trend", 
                 value=perc_csection_all, median=ext_csection_all) %>%
  runchart_flags(shift="csection_emer_shift", trend="csection_emer_trend", 
                 value=perc_csection_emer, median=ext_csection_emer) %>%
  runchart_flags(shift="csection_elec_shift", trend="csection_elec_trend", 
                 value=perc_csection_elec, median=ext_csection_elec) %>%
  ungroup()

saveRDS(mod_runchart, "shiny_app/data/mod_runchart_data.rds")
saveRDS(mod_runchart, paste0(data_folder,"final_app_files/mod_runchart_data_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 2- LINECHART DATA mode of delivery for Scotland only by age and dep
mod_scot <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",mod_folder,"/WI_DELIVERIES_SCOT_CHARTS_mode_",mod_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date, category=variable) %>%
  mutate(month=as.Date(month),
         area_type="Scotland",
         type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
         category=as.character(category))

saveRDS(mod_scot, "shiny_app/data/mod_scot_data.rds")
saveRDS(mod_scot, paste0(data_folder,"final_app_files/mod_scot_data_", 
                         format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 3- LINECHART DATA mode of delivery for Scotland & NHS board
mod_linechart <- readRDS(paste0(data_folder, "pregnancy/mode_of_delivery/",mod_folder,"/WI_DELIVERIES_LINECHART_mode_",mod_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date) %>%
  mutate(month=as.Date(month, format="%Y-%m-%d "),
         #month=format(month,"%b %Y"),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type = type, 
         category="All") %>%
  group_by(area_name, month) %>% 
  mutate(tot_births=sum(births/2), # divide by two because total births already a row in the dataset
         percent_births=(births/tot_births)*100) %>% 
  ungroup()

mod_linechart <- mod_linechart %>%
  mutate(mode = recode(mode, "Spontaneous" = "Spontaneous vaginal", "Assisted" = "Assisted vaginal", "Caesarean - Emergency" = "Emergency caesarean",
                       "Caesarean - Elective" = "Elective caesarean"))

saveRDS(mod_linechart, "shiny_app/data/mod_linechart_data.rds") 
saveRDS(mod_linechart, paste0(data_folder,"final_app_files/mod_linechart_data_", 
                              format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


## 4- Mode of delivery DATA DOWNLOAD FILE FOR SHINY APP
mod_download <- read_csv(paste0(data_folder, "pregnancy/mode_of_delivery/",mod_folder,"/WI_DELIVERIES_DOWNLOAD_mode_",mod_date,".csv"))%>%  
  janitor::clean_names() %>%
  mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
         month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
  rename(area_name=nhs_board_of_residence,
         centreline_csection_all = median_csection_all,
         centreline_csection_emer = median_csection_emer,
         centreline_csection_elec = median_csection_elec,
         dottedline_csection_all = ext_csection_all,
         dottedline_csection_emer = ext_csection_emer,
         dottedline_csection_elec = ext_csection_elec) %>% 
  mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland"),
         chart_category="All",
         chart_type= area_type)

saveRDS(mod_download, "shiny_app/data/mod_download_data.rds")  
saveRDS(mod_download, paste0(data_folder,"final_app_files/mod_download_data_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Inductions ----
###############################################.
induct_folder <- "20210215"
induct_date <- "2021-02-15"

## 1-RUNCHART DATA
## mod data for run chart (scotland and nhs board) - monthly
induct_runchart <- readRDS(paste0(data_folder, "pregnancy/inductions/",induct_folder,"/WI_DELIVERIES_RUNCHART_induced_",induct_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name = hbres, month = date) %>%
  mutate(month = as.Date(month),
         type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland"),
         area_type = type, 
         category = "All") %>%
  # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
  # ext_ columns are extended median which are blank before projection time period
  mutate(ext_ind_37_42 = case_when(is.na(ext_ind_37_42) ~ median_ind_37_42,
                                   TRUE ~ ext_ind_37_42)) %>%
  group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
  #call function to add flags for runchart shifts and trends
  #shift: name for new field where shift is flagged
  #trend: name for new field where trend is flagged
  #value: which column in dataset contains value being evaluated
  #median: which column in dataset contains the median against which value is tested
  runchart_flags(shift="induction_shift", trend="induction_trend", 
                 value=perc_ind_37_42, median=ext_ind_37_42) %>%
  ungroup()

saveRDS(induct_runchart, "shiny_app/data/induct_runchart_data.rds")
saveRDS(induct_runchart, paste0(data_folder,"final_app_files/induct_runchart_data_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 2- LINECHART DATA inductions for Scotland only by age and dep
induct_scot <- readRDS(paste0(data_folder, "pregnancy/inductions/",induct_folder,"/WI_DELIVERIES_SCOT_CHARTS_induced_",induct_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date, category=variable) %>%
  mutate(month=as.Date(month),
         area_type="Scotland",
         type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
         category=as.character(category))

saveRDS(induct_scot, "shiny_app/data/induct_scot_data.rds")
saveRDS(induct_scot, paste0(data_folder,"final_app_files/induct_scot_data_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 3- LINECHART DATA inductions for Scotland & NHS board
induct_linechart <- readRDS(paste0(data_folder, "pregnancy/inductions/",induct_folder,"/WI_DELIVERIES_LINECHART_induced_",induct_date,".rds")) %>%  
  janitor::clean_names() %>%
  mutate(tot_births_37_42=births_37_42) %>%
  #reshape data file for ease of creation of line chart with percentages
  pivot_longer(cols = ind_37_42:births_37_42, names_to = "ind",values_to = "births") %>%
  rename(area_name=hbres, month=date) %>%
  mutate(month=as.Date(month, format="%Y-%m-%d "),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type = type, 
         category="All",
         percent_births=((births/tot_births_37_42)*100),
         #NOTE the gestation categories are not mutually exclusive - <37 contains <32
         ind=case_when(ind=="ind_37_42" ~ "Births that followed induction",
                       ind=="births_37_42" ~ "All births",
                       TRUE~as.character(ind))) 

saveRDS(induct_linechart, "shiny_app/data/induct_linechart_data.rds") 
saveRDS(induct_linechart, paste0(data_folder,"final_app_files/induct_linechart_data_", 
                                 format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 4- Mode of delivery DATA DOWNLOAD FILE FOR SHINY APP
induct_download <- read_csv(paste0(data_folder, "pregnancy/inductions/",induct_folder,"/WI_DELIVERIES_DOWNLOAD_induced_",induct_date,".csv"))%>%  
  janitor::clean_names() %>%
  mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
         month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
  rename(area_name=nhs_board_of_residence,
         centreline_induced_37_42 = median_induced_37_42,
         dottedline_induced_37_42 = ext_induced_37_42) %>% 
  mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland"),
         chart_category="All",
         chart_type= area_type) 

saveRDS(induct_download, "shiny_app/data/induct_download_data.rds")  
saveRDS(induct_download, paste0(data_folder,"final_app_files/induct_download_data_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Gestation at delivery ----
###############################################.

gestation_folder <- "20210215"
gestation_date <- "2021-02-15"

## 1-RUNCHART DATA
gestation_runchart <- readRDS(paste0(data_folder,"pregnancy/gestation_at_delivery/",gestation_folder,"/WI_DELIVERIES_RUNCHART_gestation_",gestation_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name = hbres, month = date) %>%
  mutate(month = as.Date(month),
         type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland"),
         area_type = type, 
         category = "All") %>%
  # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
  # ext_ columns are extended median which are blank before projection time period
  mutate(ext_under32 = case_when(is.na(ext_under32) ~ median_under32,
                                 TRUE ~ ext_under32),
         ext_under37 = case_when(is.na(ext_under37) ~ median_under37,
                                 TRUE ~ ext_under37),
         ext_32_36 = case_when(is.na(ext_32_36) ~ median_32_36,
                               TRUE ~ ext_32_36),
         ext_42plus = case_when(is.na(ext_42plus) ~ median_42plus,
                                TRUE ~ ext_42plus)) %>%
  group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
  #call function to add flags for runchart shifts and trends
  #shift: name for new field where shift is flagged
  #trend: name for new field where trend is flagged
  #value: which column in dataset contains value being evaluated
  #median: which column in dataset contains the median against which value is tested
  runchart_flags(shift="gest_under32_shift", trend="gest_under32_trend", 
                 value=perc_under32, median=ext_under32) %>%
  runchart_flags(shift="gest_under37_shift", trend="gest_under37_trend", 
                 value=perc_under37, median=ext_under37) %>%
  runchart_flags(shift="gest_32_36_shift", trend="gest_32_36_trend", 
                 value=perc_32_36, median=ext_32_36) %>%
  runchart_flags(shift="gest_42plus_shift", trend="gest_42plus_trend", 
                 value=perc_42plus, median=ext_42plus) %>%
  ungroup()

saveRDS(gestation_runchart, "shiny_app/data/gestation_runchart_data.rds")
saveRDS(gestation_runchart, paste0(data_folder,"final_app_files/gestation_runchart_data_", 
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 2- LINECHART DATA inductions for Scotland only by age and dep
gestation_scot <- readRDS(paste0(data_folder, "pregnancy/gestation_at_delivery/",gestation_folder,"/WI_DELIVERIES_SCOT_CHARTS_gestation_",gestation_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date, category=variable) %>%
  mutate(month=as.Date(month),
         area_type="Scotland",
         type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD" ~ "dep"),
         category=as.character(category))

saveRDS(gestation_scot, "shiny_app/data/gestation_scot_data.rds")
saveRDS(gestation_scot, paste0(data_folder,"final_app_files/gestation_scot_data_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 3- LINECHART DATA gestation for Scotland & NHS board
gestation_linechart <- readRDS(paste0(data_folder, "pregnancy/gestation_at_delivery/",gestation_folder,"/WI_DELIVERIES_LINECHART_gestation_",gestation_date,".rds")) %>%  
  janitor::clean_names() %>%
  mutate(tot_births_18_44=births_18_44) %>%
  #reshape data file for ease of creation of line chart with percentages
  pivot_longer(cols = births_under32:births_18_44, names_to = "gest",values_to = "births") %>%
  rename(area_name=hbres, month=date) %>%
  mutate(month=as.Date(month, format="%Y-%m-%d "),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type = type, 
         category="All",
         percent_births=format(((births/tot_births_18_44)*100),digits=1, nsmall=1),
         #NOTE the gestation categories are not mutually exclusive - <37 contains <32
         gest=case_when(gest=="births_under32" ~ "Under 32 weeks",
                        gest=="births_under37" ~ "Under 37 weeks",
                        gest=="births_32_36" ~ "32 to 36 weeks",
                        gest=="births_37_41" ~ "37 to 41 weeks",
                        gest=="births_18_44" ~ "All gestations (18-44 weeks)",
                        gest=="births_42plus" ~ "42 weeks plus",
                        TRUE~as.character(gest))) 

saveRDS(gestation_linechart, "shiny_app/data/gestation_linechart_data.rds")  
saveRDS(gestation_linechart, paste0(data_folder,"final_app_files/gestation_linechart_data_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 4- DATA DOWNLOAD FILE FOR SHINY APP
gestation_download <- read_csv(paste0(data_folder, "pregnancy/gestation_at_delivery/",gestation_folder,"/WI_DELIVERIES_DOWNLOAD_gestation_",gestation_date,".csv"))%>%  
  janitor::clean_names() %>%
  mutate(month_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
         month_of_discharge=format(month_of_discharge,"%b %Y")) %>%
  rename(area_name=nhs_board_of_residence,
         centreline_under32 = median_under32,
         centreline_32_36 = median_32_36,
         centreline_under37 = median_under37,
         centreline_42plus = median_42plus,
         dottedline_under32 = ext_under32,
         dottedline_32_36 = ext_32_36,
         dottedline_under37 = ext_under37,
         dottedline_42plus = ext_42plus) %>% 
  mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland"),
         chart_category="All",
         chart_type= area_type) 

saveRDS(gestation_download, "shiny_app/data/gestation_download_data.rds") 
saveRDS(gestation_download, paste0(data_folder,"final_app_files/gestation_download_data_", 
                                   format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Perinatal mortality ----
###############################################.
# P CHART PERINATAL DATA
p_perinatal <- bind_rows(read_excel(paste0(data_folder,"perinatal/Pchart - SB NND EXTPERI_marupdate.xlsx"),
                                    sheet = "Stillbirth", skip = 2) %>% mutate(type = "stillbirths"),
                         read_excel(paste0(data_folder,"perinatal/Pchart - SB NND EXTPERI_marupdate.xlsx"),
                                    sheet = "NND", skip = 2) %>% mutate(type = "nnd"),
                         read_excel(paste0(data_folder,"perinatal/Pchart - SB NND EXTPERI_marupdate.xlsx"),
                                    sheet = "Extended perinatal", skip = 2) %>% mutate(type = "extperi")) %>%
  janitor::clean_names() %>%
  select(month_of_year=sample_2, number_of_deaths_in_month=observation, sample_size, rate, centreline, stdev = binomial_st_dev_16, 
         upper_cl_3_std_dev:type)

u_perinatal <- bind_rows(read_excel(paste0(data_folder,"perinatal/Uchart - PNND INFANT DEATHS_marupdate.xlsx"),
                                    sheet = "ID", skip = 2) %>% mutate(type = "infantdeaths"),
                         read_excel(paste0(data_folder,"perinatal/Uchart - PNND INFANT DEATHS_marupdate.xlsx"),
                                    sheet = "PNND", skip = 2) %>% mutate(type = "pnnd")) %>%  
  janitor::clean_names() %>%
  select(month_of_year=sample,  number_of_deaths_in_month=observation, sample_size=ao_o_size, rate, centreline, stdev = poisson_st_dev_16, 
         upper_cl_3_std_dev:type)

# Mergin both datasets together 
perinatal <- rbind(p_perinatal, u_perinatal) %>% 
  mutate(area_name="Scotland", #creating geo variables
         area_type="Scotland",
         month_of_year = gsub(" ", "0", month_of_year), #formatting date
         month_of_year = as.Date(paste0(month_of_year,"1"), format="%Y%m%d")) 

# Creating rules for spc charts
perinatal %<>% 
  arrange(type, area_name, month_of_year) %>% 
  mutate(upper_sigma1 = rate + stdev,
         lower_sigma1 = rate + stdev) %>% 
  group_by(type, area_name) %>% 
  # for rules: outliers when over or under 3 sigma limit
  mutate(outlier = case_when(rate>upper_cl_3_std_dev | rate< lower_cl_3_std_dev ~ T, T ~ F),
         # Shift: run of 8or more consecutive data points above or below the centreline
         # First id when this run is happening and then iding all points part of it
         shift_i = case_when((rate > centreline & lag(rate, 1) > centreline 
                              & lag(rate, 2) > centreline & lag(rate, 3) > centreline 
                              & lag(rate, 4) > centreline & lag(rate, 5) > centreline
                              & lag(rate, 6) > centreline & lag(rate, 7) > centreline) |
                               (rate < centreline & lag(rate, 1) < centreline 
                                & lag(rate, 2) < centreline & lag(rate, 3) < centreline 
                                & lag(rate, 4) < centreline & lag(rate, 5) < centreline
                                & lag(rate, 6) < centreline & lag(rate, 7) < centreline) ~ T , T ~ F),
         shift = case_when(shift_i == T | lead(shift_i, 1) == T | lead(shift_i, 2) == T
                           | lead(shift_i, 3) == T | lead(shift_i, 4) == T
                           | lead(shift_i, 5) == T | lead(shift_i, 6) == T
                           | lead(shift_i, 7) == T ~ T, T ~ F),
         # Trend: A run of 6 or more consecutive data points
         trend_i = case_when((rate > lag(rate ,1) & lag(rate, 1) > lag(rate, 2) 
                              & lag(rate, 2) > lag(rate, 3)  & lag(rate, 3) > lag(rate, 4) 
                              & lag(rate, 4) > lag(rate, 5) ) |
                               (rate < lag(rate ,1) & lag(rate, 1) < lag(rate, 2) 
                                & lag(rate, 2) < lag(rate, 3)  & lag(rate, 3) < lag(rate, 4) 
                                & lag(rate, 4) < lag(rate, 5) ) 
                             ~ T , T ~ F),
         trend = case_when(trend_i == T | lead(trend_i, 1) == T | lead(trend_i, 2) == T
                           | lead(trend_i, 3) == T | lead(trend_i, 4) == T
                           | lead(trend_i, 5) == T  ~ T, T ~ F),
         #Outer One –Third: Two out of three consecutive data points which sit close to one of the control limits(within 2 and 3 sigma)
         outer_i = case_when((rate > upper_wl_2_std_dev & rate < upper_cl_3_std_dev) & 
                               ((lag(rate,1) > upper_wl_2_std_dev & lag(rate,1) < upper_cl_3_std_dev) | 
                                  (lag(rate,2) > upper_wl_2_std_dev & lag(rate,2) < upper_cl_3_std_dev)) ~ T, T ~ F),
         outer = case_when(outer_i == T | lead(outer_i, 1) == T | lead(outer_i, 2) == T ~ T, T ~ F),
         # Inner One -Third: 15 or more consecutive data points that lie close to the centreline(within 1 sigma).
         inner_i = case_when(rate < upper_sigma1 & rate > lower_sigma1 &
                               lag(rate, 1) < upper_sigma1 & lag(rate, 1) > lower_sigma1 &
                               lag(rate, 2) < upper_sigma1 & lag(rate, 2) > lower_sigma1 &
                               lag(rate, 3) < upper_sigma1 & lag(rate, 3) > lower_sigma1 &
                               lag(rate, 4) < upper_sigma1 & lag(rate, 4) > lower_sigma1 &
                               lag(rate, 5) < upper_sigma1 & lag(rate, 5) > lower_sigma1 &
                               lag(rate, 6) < upper_sigma1 & lag(rate, 6) > lower_sigma1 &
                               lag(rate, 7) < upper_sigma1 & lag(rate, 7) > lower_sigma1 &
                               lag(rate, 8) < upper_sigma1 & lag(rate, 8) > lower_sigma1 &
                               lag(rate, 9) < upper_sigma1 & lag(rate, 9) > lower_sigma1 &
                               lag(rate, 10) < upper_sigma1 & lag(rate, 10) > lower_sigma1 &
                               lag(rate, 11) < upper_sigma1 & lag(rate, 11) > lower_sigma1 &
                               lag(rate, 12) < upper_sigma1 & lag(rate, 12) > lower_sigma1 &
                               lag(rate, 13) < upper_sigma1 & lag(rate, 13) > lower_sigma1 &
                               lag(rate, 14) < upper_sigma1 & lag(rate, 14) > lower_sigma1 ~ T, T ~F),
         inner = case_when(inner_i == T | lead(inner_i, 1) == T | lead(inner_i, 2) == T
                           | lead(inner_i, 3) == T | lead(inner_i, 4) == T
                           | lead(inner_i, 5) == T | lead(inner_i, 6) == T
                           | lead(inner_i, 7) == T | lead(inner_i, 8) == T
                           | lead(inner_i, 9) == T | lead(inner_i, 10) == T
                           | lead(inner_i, 11) == T | lead(inner_i, 12) == T
                           | lead(inner_i, 13) == T | lead(inner_i, 14) == T ~T, T ~ F)) %>%
  ungroup %>% 
  select(-shift_i, -trend_i, -outer_i, -inner_i) 

saveRDS(perinatal, "shiny_app/data/perinatal.rds")
saveRDS(perinatal, paste0(data_folder,"final_app_files/perinatal_", 
                          format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# saving perinatal open data files
perinatal %<>% 
  select(area_name, month_of_year, type, number_of_deaths_in_month, rate, 
         relevant_births = sample_size) %>% 
  mutate(type = recode_factor(type, "extperi" = "Extended perinatal deaths", "infantdeaths" = "Infant deaths", "nnd" = "Neonatal deaths", 
                              "pnnd" = "Post-neonatal deaths", "stillbirths" = "Stillbirths")) 

saveRDS(perinatal, paste0(open_data, "perinatal_data.rds"))

###############################################.
## Apgar ----
###############################################.
apgar_folder <- "20210218"
apgar_date <- "2021_02_18"

## 1-RUNCHART DATA
## apgar data for run chart (scotland and nhs board) - monthly
apgar_runchart <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_DELIVERIES_RUNCHART_Apgar5_",apgar_date,".rds")) %>%  
  rename(area_name = HBRES, quarter = DATE) %>%  
  janitor::clean_names() %>%
  mutate(quarter = as.Date(quarter),
         type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland"),
         area_type = type, 
         category = "All") %>%
  # the median column is used to assess shifts or trends - dataset contains NA cells which need to filled
  # ext_ columns (don't exist in data file) are extended median which are blank before projection time period
  group_by(area_name) %>% 
  mutate(ext_apgar5_37plus = max(median_apgar5_37plus, na.rm = T)) %>% 
  ungroup() %>% 
  # mutate(ext_ind_37_42 = case_when(is.na(ext_ind_37_42) ~ median_ind_37_42,
  #                                  TRUE ~ ext_ind_37_42)) %>%
  group_by(area_name, area_type, type) %>%   #sort data to ensure trends/shifts compare correct data points
  #call function to add flags for runchart shifts and trends
  #shift: name for new field where shift is flagged
  #trend: name for new field where trend is flagged
  #value: which column in dataset contains value being evaluated
  #median: which column in dataset contains the median against which value is tested
  runchart_flags(shift="apgar_shift", trend="apgar_trend", 
                 value=perc_low_apgar5_37plus, median=ext_apgar5_37plus) %>%
  ungroup()

saveRDS(apgar_runchart, "shiny_app/data/apgar_runchart_data.rds")
saveRDS(apgar_runchart, paste0(data_folder,"final_app_files/apgar_runchart_data_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 2- LINECHART DATA apgar for Scotland only by age and dep
apgar_scot <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_DELIVERIES_SCOT_CHARTS_Apgar5_",apgar_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, quarter=date, category=variable) %>%
  mutate(quarter=as.Date(quarter),
         area_type = case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                          area_name=="Scotland" ~ "Scotland"),
         type=case_when(subgroup=="AGEGRP" ~ "age",subgroup=="SIMD5" ~ "dep"),
         category=as.character(category),
         category = case_when(category == "-under 20" ~ "Under 20",
                              category == "40+" ~ "40 and over",
                              category == "1" ~ "1 - most deprived",
                              category == "5" ~ "5 - least deprived",
                                     TRUE ~ as.character(category)))

saveRDS(apgar_scot, "shiny_app/data/apgar_scot_data.rds")
saveRDS(apgar_scot, paste0(data_folder,"final_app_files/apgar_scot_data_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 3- LINECHART DATA apgar for Scotland & NHS board
apgar_linechart <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_DELIVERIES_LINECHART_Apgar5_",apgar_date,".rds")) %>%  
  rename(area_name=HBRES, quarter=DATE) %>%
  janitor::clean_names() %>%
  mutate(tot_apgar5_37plus=apgar5_37plus) %>%
  #reshape data file for ease of creation of line chart with percentages
  pivot_longer(cols = low_apgar5_37plus:apgar5_37plus, names_to = "ind",values_to = "apgar5") %>%
  mutate(quarter=as.Date(quarter, format="%Y-%m-%d "),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type = type, 
         category="All",
         percent_apgar=((apgar5/tot_apgar5_37plus)*100),
         ind=case_when(ind=="low_apgar5_37plus" ~ "Babies with a low Apgar 5",
                       ind=="apgar5_37plus" ~ "Babies with an Apgar 5",
                       TRUE~as.character(ind))) 

saveRDS(apgar_linechart, "shiny_app/data/apgar_linechart_data.rds") 
saveRDS(apgar_linechart, paste0(data_folder,"final_app_files/apgar_linechart_data_", 
                                 format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## 4- Apgar DATA DOWNLOAD FILE FOR SHINY APP
apgar_download <- readRDS(paste0(data_folder, "births_babies/apgar/",apgar_folder,"/WI_Apgar5_DOWNLOAD_",apgar_date,".rds"))%>%  
  janitor::clean_names() %>%
  mutate(quarter_of_discharge=as.Date(month_of_discharge,format="%Y-%m-%d"),
         quarter_of_discharge=format(quarter_of_discharge,"%b %Y")
         ) %>%
  rename(area_name=nhs_board_of_residence,
         centreline_apgar5_37plus = median_apgar5_37plus,
         dottedline_apgar5_37plus = ext_median_apgar5_37plus) %>% 
  mutate(area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland"),
         chart_category="All",
         chart_type= area_type) 

saveRDS(apgar_download, "shiny_app/data/apgar_download_data.rds")  
saveRDS(apgar_download, paste0(data_folder,"final_app_files/apgar_download_data_", 
                                format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

##END
