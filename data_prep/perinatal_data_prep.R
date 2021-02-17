# Data preparation for stillbirths and infant deaths

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

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

##END