# Data preparation for pregnancy tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## Pregnancy (antenatal booking) ----
###############################################.

#field with date all antenatal booking data files prepared
antenatal_booking_date <- "13012021"

# Excel workbook containing number of women booking for antenatal care - weekly file (Scotland and NHS board except small islands)
ante_booking_no <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyNosBooked_Charts_",antenatal_booking_date,".xlsx"),
                              sheet = "Data for Dashboard Charts") %>%
  janitor::clean_names() %>%
  rename(centreline_no=centreline, dottedline_no=dottedline, booked_no=booked) %>%
  mutate(week_book_starting=as.Date(week_book_starting,format="%d-%b-%y")) %>% 
  filter(week_book_starting < "2021-01-11")

# Excel workbook containing avergage gestation of women booking for antenatal care  - weekly file (Scotland and NHS board except small islands)
ante_booking_gest <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyAveGestation_Charts_",antenatal_booking_date,".xlsx"),
                                sheet = "Data for Dashboard Charts") %>%
  janitor::clean_names() %>%
  rename(centreline_g=centreline, dottedline_g=dottedline, booked_g=booked) %>%
  mutate(week_book_starting=as.Date(week_book_starting,format="%d-%b-%y")) %>% 
  filter(week_book_starting < "2021-01-11")

# join two (numbers and average gestation) booking sheets to form single file for shiny app
ante_booking <- left_join(ante_booking_no, ante_booking_gest, by = c("week_book_starting","area"))

# Match area names from lookup & format for shinyapp
ante_booking <- left_join(ante_booking, hb_lookup, by = c("area" = "hb_cypher")) %>%
  mutate(type=case_when(area_type=="Health board" ~ "Health board",
                        area=="Scotland" ~ "Scotland",
                        (substr(area,1,4)=="SIMD") ~ "dep", TRUE ~ "age"),
         area_name=case_when(type=="Scotland" ~ "Scotland",
                             type=="age" ~ "Scotland",
                             type=="dep" ~ "Scotland",
                             TRUE ~ area_name),
         area_type=case_when(type=="Health board" ~ "Health board", TRUE ~ area_name), 
         category=case_when(type=="Scotland" ~ "All",
                            type=="Health board" ~ "All",
                            type=="age" ~ area,
                            type=="dep" ~ area, T ~"other"),
         category=case_when(area=="SIMD 1" ~ "1 - most deprived",
                            area=="SIMD 2" ~ "2",
                            area=="SIMD 3" ~ "3",
                            area=="SIMD 4" ~ "4",
                            area=="SIMD 5" ~ "5 - least deprived",
                            type=="age" ~ category, T ~area_name),
         category=case_when(category=="40 plus" ~ "40 and over",TRUE ~ category)) %>%
  select(-area)

#add control chart flags for charting
ante_booking <- ante_booking %>%
  group_by(area_name, area_type, type, category) %>% 
  mutate(# Shift: run of 6 or more consecutive data points above or below the centreline
    # First id when this run is happening and then finding all points part of it
    # SHIFT NUMBER OF WOMEN BOOKING
    shift_i_booked_no = case_when((booked_no > dottedline_no & lag(booked_no, 1) > dottedline_no 
                                   & lag(booked_no, 2) > dottedline_no & lag(booked_no, 3) > dottedline_no 
                                   & lag(booked_no, 4) > dottedline_no & lag(booked_no, 5) > dottedline_no) |
                                    (booked_no < dottedline_no & lag(booked_no, 1) < dottedline_no 
                                     & lag(booked_no, 2) < dottedline_no & lag(booked_no, 3) < dottedline_no 
                                     & lag(booked_no, 4) < dottedline_no & lag(booked_no, 5) < dottedline_no) ~ T , T ~ F),
    shift_booked_no = case_when(shift_i_booked_no == T | lead(shift_i_booked_no, 1) == T | lead(shift_i_booked_no, 2) == T
                                | lead(shift_i_booked_no, 3) == T | lead(shift_i_booked_no, 4) == T
                                | lead(shift_i_booked_no, 5) == T  ~ T, T ~ F),
    # SHIFT FOR AVERAGE GESTATION
    shift_i_booked_gest = case_when((ave_gest > dottedline_g & lag(ave_gest, 1) > dottedline_g 
                                     & lag(ave_gest, 2) > dottedline_g & lag(ave_gest, 3) > dottedline_g 
                                     & lag(ave_gest, 4) > dottedline_g & lag(ave_gest, 5) > dottedline_g) |
                                      (ave_gest < dottedline_g & lag(ave_gest, 1) < dottedline_g 
                                       & lag(ave_gest, 2) < dottedline_g & lag(ave_gest, 3) < dottedline_g 
                                       & lag(ave_gest, 4) < dottedline_g & lag(ave_gest, 5) < dottedline_g) ~ T , T ~ F),
    shift_booked_gest = case_when(shift_i_booked_gest == T | lead(shift_i_booked_gest, 1) == T | lead(shift_i_booked_gest, 2) == T
                                  | lead(shift_i_booked_gest, 3) == T | lead(shift_i_booked_gest, 4) == T
                                  | lead(shift_i_booked_gest, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points - NUMBERS OF WOMEN BOOKING
    trend_i_booked_no = case_when((booked_no > lag(booked_no ,1) & lag(booked_no, 1) > lag(booked_no, 2) 
                                   & lag(booked_no, 2) > lag(booked_no, 3)  & lag(booked_no, 3) > lag(booked_no, 4)) |
                                    (booked_no < lag(booked_no ,1) & lag(booked_no, 1) < lag(booked_no, 2) 
                                     & lag(booked_no, 2) < lag(booked_no, 3)  & lag(booked_no, 3) < lag(booked_no, 4) )  
                                  ~ T , T ~ F),
    trend_booked_no = case_when(trend_i_booked_no == T | lead(trend_i_booked_no, 1) == T | lead(trend_i_booked_no, 2) == T
                                | lead(trend_i_booked_no, 3) == T | lead(trend_i_booked_no, 4) == T
                                ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points - AVERAGE GESTATION
    trend_i_booked_gest = case_when((ave_gest > lag(ave_gest ,1) & lag(ave_gest, 1) > lag(ave_gest, 2) 
                                     & lag(ave_gest, 2) > lag(ave_gest, 3)  & lag(ave_gest, 3) > lag(ave_gest, 4)) |
                                      (ave_gest < lag(ave_gest ,1) & lag(ave_gest, 1) < lag(ave_gest, 2) 
                                       & lag(ave_gest, 2) < lag(ave_gest, 3)  & lag(ave_gest, 3) < lag(ave_gest, 4) )  
                                    ~ T , T ~ F),
    trend_booked_gest = case_when(trend_i_booked_gest == T | lead(trend_i_booked_gest, 1) == T | lead(trend_i_booked_gest, 2) == T
                                  | lead(trend_i_booked_gest, 3) == T | lead(trend_i_booked_gest, 4) == T
                                  ~ T, T ~ F)) %>%
  select(-shift_i_booked_no, -trend_i_booked_no,-shift_i_booked_gest, -trend_i_booked_gest) %>%
  ungroup()

saveRDS(ante_booking, "shiny_app/data/ante_booking.rds")
saveRDS(ante_booking, paste0(data_folder,"final_app_files/ante_booking_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## ANTENATAL DATA DOWNLOAD FILE FOR SHINY APP
## Data download to include weekly Scotland data for age/deprivation breakdown PLUS monthly booking data for all NHS boards (even the small island boards)

## Monthly booking numbers and average gestation at booking data 
gest_booking_download <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyAveGestation_Charts_",antenatal_booking_date,".xlsx"),
                                    sheet = "Monthly Data for Download") %>%
  janitor::clean_names()

# Match area names from lookup & format for shinyapp
gest_booking_download <- left_join(gest_booking_download, hb_lookup, by = c("area" = "hb_cypher")) %>%
  mutate(area_name=case_when(area=="Scotland" ~ "Scotland", T~ area_name),
         area_type=case_when(area=="Scotland" ~ "Scotland", T~ area_type),
         time_period="monthly") %>%
  select(-area) %>%
  rename(booking_month=month_booking, number_of_bookings=booked, average_gestation_at_booking=ave_gest) %>%
  arrange(area_type, booking_month)

# Weekly scotland level booking numbers and gestation
ante_booking_download1 <- ante_booking %>%
  mutate(time_period="weekly") %>%
  filter(week_book_starting < "2021-01-11") %>% 
  rename(booking_week_beginning=week_book_starting, number_of_bookings=booked_g, average_gestation_at_booking=ave_gest)
  

# Add weekly and month files into one file
ante_booking_download <- bind_rows(ante_booking_download1, gest_booking_download) %>%
  rename(chart_type=type,chart_category=category,
         number_of_women_booking=number_of_bookings,
         centreline_number=centreline_no,
         dottedline_number=dottedline_no,
         number_of_women_booking_gest_under_10wks=g_u10wks,
         number_of_women_booking_gest_10to12wks=g_10to12wks,
         number_of_women_booking_gest_over_12wks=g_13pluswks,
         centreline_gestation=centreline_g,
         dottedline_gestation=dottedline_g) %>%
  select(time_period, booking_week_beginning, booking_month, area_name, area_type, chart_type, chart_category,
         number_of_women_booking, centreline_number, dottedline_number,
         number_of_women_booking_gest_under_10wks,number_of_women_booking_gest_10to12wks,number_of_women_booking_gest_over_12wks,
         average_gestation_at_booking, centreline_gestation, dottedline_gestation)

saveRDS(ante_booking_download, "shiny_app/data/ante_booking_download.rds")
saveRDS(ante_booking_download, paste0(data_folder,"final_app_files/ante_booking_download_", 
                                      format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Pregnancy (terminations) ----
###############################################.
#field with date all antenatal booking data files prepared
top_date <- "2021-02-09"

## Termination data for run chart (scotland and nhs board) - monthly
top_runchart <- readRDS(paste0(data_folder, "pregnancy/terminations/",top_date,
                               "/WI_TERMINATIONS_RUNCHARTS_",top_date,".rds")) %>%  
  janitor::clean_names() %>%
  rename(area_name=hbres, month=date,
         centreline_no = av_pre_pan_terminations,
         dottedline_no = ext_av_count,
         centreline_g = pre_pan_av_gest,
         dottedline_g = ext_av_gest) %>%
  mutate(terminations=as.numeric(terminations),
         month=as.Date(month),
         type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                        area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         area_type=case_when(type=="Health board" ~ "Health board", TRUE ~ area_name), 
         category=case_when(type=="Scotland" ~ "All",
                            type=="Health board" ~ "All"))

## Termination data for scotland only by age and dep
top_scot <- readRDS(paste0(data_folder, "pregnancy/terminations/",top_date,
                           "/WI_TERMINATIONS_SCOTLAND_CHARTS_",top_date,".rds")) %>%  
  janitor::clean_names() %>%
  ungroup() %>% # for some reason dataset appears to be grouped which prevents formatting 
  rename(area_name=hbres, month=date, category=variable) %>%
  mutate(month=as.Date(month),
         type=case_when(chart=="AGEGRP" ~ "age",chart=="SIMD" ~ "dep",TRUE ~ "other"),
         area_type="Scotland",
         category=as.character(case_when(category=="40+" ~ "40 and over", 
                                         category=="under 20" ~ "Under 20", 
                                         TRUE ~ as.character(category))))

## Combine area based and age/dep terminations data, format and add shifts/trends
top <- bind_rows(top_runchart, top_scot) %>%
  select(-chart) %>%
  #dotted line used to assess shifts or trends therefore need to fill cells which are set to NA 
  mutate(dottedline_no= case_when(is.na(dottedline_no)~centreline_no,TRUE ~ dottedline_no),
         dottedline_g= case_when(is.na(dottedline_g)~centreline_g,TRUE ~ dottedline_g)) %>% #recode age group as required
  #sort data to ensure trends/shifts compare correct data points
  group_by(area_name, area_type, type) %>%
  mutate(# Shift: run of 6 or more consecutive data points above or below the centreline
    # First id when this run is happening and then finding all points part of it
    # SHIFT NUMBER OF terminations
    shift_i_top_no = case_when((terminations > dottedline_no & lag(terminations, 1) > dottedline_no 
                                & lag(terminations, 2) > dottedline_no & lag(terminations, 3) > dottedline_no 
                                & lag(terminations, 4) > dottedline_no & lag(terminations, 5) > dottedline_no) |
                                 (terminations < dottedline_no & lag(terminations, 1) < dottedline_no 
                                  & lag(terminations, 2) < dottedline_no & lag(terminations, 3) < dottedline_no 
                                  & lag(terminations, 4) < dottedline_no & lag(terminations, 5) < dottedline_no) ~ T , T ~ F),
    shift_top_no = case_when(shift_i_top_no == T | lead(shift_i_top_no, 1) == T | lead(shift_i_top_no, 2) == T
                             | lead(shift_i_top_no, 3) == T | lead(shift_i_top_no, 4) == T
                             | lead(shift_i_top_no, 5) == T  ~ T, T ~ F),
    # SHIFT FOR AVERAGE GESTATION
    shift_i_top_gest = case_when((av_gest > dottedline_g & lag(av_gest, 1) > dottedline_g 
                                  & lag(av_gest, 2) > dottedline_g & lag(av_gest, 3) > dottedline_g 
                                  & lag(av_gest, 4) > dottedline_g & lag(av_gest, 5) > dottedline_g) |
                                   (av_gest < dottedline_g & lag(av_gest, 1) < dottedline_g 
                                    & lag(av_gest, 2) < dottedline_g & lag(av_gest, 3) < dottedline_g 
                                    & lag(av_gest, 4) < dottedline_g & lag(av_gest, 5) < dottedline_g) ~ T , T ~ F),
    shift_top_gest = case_when(shift_i_top_gest == T | lead(shift_i_top_gest, 1) == T | lead(shift_i_top_gest, 2) == T
                               | lead(shift_i_top_gest, 3) == T | lead(shift_i_top_gest, 4) == T
                               | lead(shift_i_top_gest, 5) == T  ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points - NUMBERS OF TOP
    trend_i_top_no = case_when((terminations > lag(terminations ,1) & lag(terminations, 1) > lag(terminations, 2) 
                                & lag(terminations, 2) > lag(terminations, 3)  & lag(terminations, 3) > lag(terminations, 4)) |
                                 (terminations < lag(terminations ,1) & lag(terminations, 1) < lag(terminations, 2) 
                                  & lag(terminations, 2) < lag(terminations, 3)  & lag(terminations, 3) < lag(terminations, 4) )  
                               ~ T , T ~ F),
    trend_top_no = case_when(trend_i_top_no == T | lead(trend_i_top_no, 1) == T | lead(trend_i_top_no, 2) == T
                             | lead(trend_i_top_no, 3) == T | lead(trend_i_top_no, 4) == T
                             ~ T, T ~ F),
    # Trend: A run of 5 or more consecutive data points - AVERAGE GESTATION TOP
    trend_i_top_gest = case_when((av_gest > lag(av_gest ,1) & lag(av_gest, 1) > lag(av_gest, 2) 
                                  & lag(av_gest, 2) > lag(av_gest, 3)  & lag(av_gest, 3) > lag(av_gest, 4)) |
                                   (av_gest < lag(av_gest ,1) & lag(av_gest, 1) < lag(av_gest, 2) 
                                    & lag(av_gest, 2) < lag(av_gest, 3)  & lag(av_gest, 3) < lag(av_gest, 4) )  
                                 ~ T , T ~ F),
    trend_top_gest = case_when(trend_i_top_gest == T | lead(trend_i_top_gest, 1) == T | lead(trend_i_top_gest, 2) == T
                               | lead(trend_i_top_gest, 3) == T | lead(trend_i_top_gest, 4) == T
                               ~ T, T ~ F)) %>%
  select(-shift_i_top_no, -trend_i_top_no,-shift_i_top_gest, -trend_i_top_gest) %>%
  ungroup()

saveRDS(top, "shiny_app/data/top.rds")
saveRDS(top, paste0(data_folder,"final_app_files/top_", 
                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

## TERMINATIONS DATA DOWNLOAD FILE FOR SHINY APP
## Data download to include monthly Scotland data for age/deprivation breakdown PLUS monthly data for NHS boards (excluding the small island boards)

top_download_board <- read_csv(paste0(data_folder, "pregnancy/terminations/",top_date,
                                      "/WI_TERMINATIONS_DOWNLOAD_",top_date,".csv"))%>%  
  janitor::clean_names() %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"),
         termination_month=format(date,"%b %Y")) %>%
  rename(area_name=hbres, 
         number_of_terminations=terminations,
         centreline_number=av_pre_pan_terminations,
         dottedline_number=ext_av_count,
         number_of_terminations_gest_under_10wks=x9_weeks,
         number_of_terminations_gest_10to12wks=x10_12_weeks,
         number_of_terminations_gest_over_12wks=x13_weeks,
         average_gestation_at_termination = av_gest,
         centreline_gestation = pre_pan_av_gest,
         dottedline_gestation = ext_av_gest) %>%
  mutate(average_gestation_at_termination =format(average_gestation_at_termination,digits = 1, nsmall = 1),
         centreline_gestation =format(centreline_gestation,digits = 1, nsmall = 1),
         dottedline_gestation =format(dottedline_gestation,digits = 1, nsmall = 1),
         area_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                             area_name=="Scotland" ~ "Scotland", TRUE ~ "Other"),
         chart_category=case_when(area_type=="Scotland" ~ "All",
                                  area_type=="Health board" ~ "All"),
         chart_type=case_when(substr(area_name,1,3)=="NHS" ~ "Health board",
                              area_name=="Scotland" ~ "Scotland", TRUE ~ "Other")) %>%
  select(termination_month, area_name, area_type, chart_type, chart_category, 
         number_of_terminations, centreline_number, dottedline_number,
         number_of_terminations_gest_under_10wks,
         number_of_terminations_gest_10to12wks,
         number_of_terminations_gest_over_12wks,
         average_gestation_at_termination, centreline_gestation, dottedline_gestation,date) %>%
  arrange(area_name, area_type,chart_type, date) %>% 
  select(-date)

top_download_scot <- top_scot %>%
  mutate(month=as.Date(month,format="%Y-%m-%d"),
         termination_month=format(month,"%b %Y"),
         av_gest =format(av_gest,digits = 1, nsmall = 1)) %>%
  rename(number_of_terminations=terminations,
         average_gestation_at_termination = av_gest,
         chart_category=category,
         chart_type=type) %>%
  select(-chart, -month)

top_download <- bind_rows(top_download_board, top_download_scot)

saveRDS(top_download, "shiny_app/data/top_download.rds")
saveRDS(top_download, paste0(data_folder,"final_app_files/top_download_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## Pregnancy (mode of delivery) ----
###############################################.
#field with date data files prepared
mod_folder <- "20210112"
mod_date <- "2021-01-12"

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
## Pregnancy (inductions) ----
###############################################.
induct_folder <- "20210112"
induct_date <- "2021-01-12"

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
## Pregnancy (gestation at delivery) ----
###############################################.

gestation_folder <- "20210112"
gestation_date <- "2021-01-12"

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
