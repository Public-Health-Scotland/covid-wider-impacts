# Data preparation for pregnancy tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## Pregnancy (antenatal booking) ----
###############################################.

#field with date all antenatal booking data files prepared
create_antebooking <- function(booking_date, max_book_date) {
  
# Excel workbook containing number of women booking for antenatal care - weekly file (Scotland and NHS board except small islands)
ante_booking_no <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyNosBooked_Charts_",booking_date,".xlsx"),
                              sheet = "Data for Dashboard Charts") %>%
  janitor::clean_names() %>%
  rename(centreline_no=centreline, dottedline_no=dottedline, booked_no=booked) %>%
  mutate(week_book_starting=as.Date(week_book_starting,format="%d-%b-%y")) %>%
  filter(week_book_starting < max_book_date)

# Excel workbook containing avergage gestation of women booking for antenatal care  - weekly file (Scotland and NHS board except small islands)
ante_booking_gest <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyAveGestation_Charts_",booking_date,".xlsx"),
                                sheet = "Data for Dashboard Charts",
                                col_types = c("text", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  janitor::clean_names() %>%
  rename(centreline_g=centreline, dottedline_g=dottedline, 
         centreline_g_t=tcentreline, dottedline_g_t=tdottedline,
         centreline_g_v=vcentreline, dottedline_g_v = vdottedline,
         booked_g=booked) %>%
  mutate(week_book_starting=as.Date(week_book_starting,format="%d-%b-%y")) %>%
  filter(week_book_starting < max_book_date)

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

ante_booking %<>% 
  mutate(centreline_g = case_when(area_name == "NHS Tayside" & !(is.na(centreline_g_t)) ~ centreline_g_t,
         T ~ centreline_g)) %>% 
  mutate(dottedline_g = case_when(area_name == "NHS Tayside" & !(is.na(dottedline_g_t)) ~ dottedline_g_t, 
                                  T ~ dottedline_g)) %>%
  mutate(centreline_g = case_when(area_name == "NHS Forth Valley" & !(is.na(centreline_g_v)) ~ centreline_g_v,
                                  T ~ centreline_g))

# test filter june2022 update
# ante_booking <- ante_booking %>%
#   filter(case_when(area_name == "NHS Greater Glasgow & Clyde" ~ week_book_starting < "2022-04-19"))
  

#add control chart flags for charting
ante_booking <- ante_booking %>%
  group_by(area_name, area_type, type, category) %>% 
  # Creating shift and trend flags for booked numbers and gestation week
  runchart_flags(shift="shift_booked_no", trend="trend_booked_no", 
                   value=booked_no, median=dottedline_no) %>%
  runchart_flags(shift="shift_booked_gest", trend="trend_booked_gest", 
                     value=ave_gest, median=dottedline_g) %>% 
  ungroup()

saveRDS(ante_booking, "shiny_app/data/ante_booking.rds")
saveRDS(ante_booking, paste0(data_folder,"final_app_files/ante_booking_", 
                             format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

print("ante_booking.rds data prepared and saved")

ante_booking <<- ante_booking

## ANTENATAL DATA DOWNLOAD FILE FOR SHINY APP
## Data download to include weekly Scotland data for age/deprivation breakdown PLUS monthly booking data for all NHS boards (even the small island boards)

## Monthly booking numbers and average gestation at booking data 
gest_booking_download <- read_excel(paste0(data_folder,"pregnancy/antenatal_booking/WeeklyAveGestation_Charts_",booking_date,".xlsx"),
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

# Weekly scotland level booking numbers and gestation - ASK WHAT TIME PERIOD SHOULD BE AVAILABLE HERE
ante_booking_download1 <- ante_booking %>%
  mutate(time_period="weekly") %>%
  filter(week_book_starting < max_book_date) %>% 
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
         dottedline_gestation=dottedline_g,
         centreline_gestation_tayside=centreline_g_t,
         dottedline_gestation_tayside=dottedline_g_t,
         centreline_gestation_forthvalley=centreline_g_v) %>%
  select(time_period, booking_week_beginning, booking_month, area_name, area_type, chart_type, chart_category,
         number_of_women_booking, centreline_number, dottedline_number,
         number_of_women_booking_gest_under_10wks,number_of_women_booking_gest_10to12wks,number_of_women_booking_gest_over_12wks,
         average_gestation_at_booking, centreline_gestation, dottedline_gestation, centreline_gestation_tayside, dottedline_gestation_tayside, centreline_gestation_forthvalley)

saveRDS(ante_booking_download, "shiny_app/data/ante_booking_download.rds")
saveRDS(ante_booking_download, paste0(data_folder,"final_app_files/ante_booking_download_", 
                                      format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

print("ante_booking_download.rds data prepared and saved")

# Saving data for open data platform
ante_booking_download <- ante_booking_download %>% 
  select(area_name, area_type, booking_month, booking_week_beginning, category = chart_category,
         number_of_women_booking, number_of_women_booking_gest_under_10wks,
         number_of_women_booking_gest_10to12wks, number_of_women_booking_gest_over_12wks,
         average_gestation_at_booking) %>% 
  mutate(category = case_when(category %in% c("20-24", "25-29", "30-34", "35-39", 
                                              "40 and over", "Under 20", "1 - most deprived", "2", "3", "4", 
                                              "5 - least deprived") ~ paste0(category),
                              TRUE ~ "All"))


saveRDS(ante_booking_download, paste0(open_data,"ante_booking.rds"))
print("Open data prepared and saved")
}

###############################################.
## Pregnancy (terminations) ----
###############################################.
create_terminations <- function(top_date) {
  
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
  # Creating shift and trend flags for top numbers and gestation week
  runchart_flags(shift="shift_top_no", trend="trend_top_no", 
                 value=terminations, median=dottedline_no) %>%
  runchart_flags(shift="shift_top_gest", trend="trend_top_gest", 
                 value=av_gest, median=dottedline_g) %>% 
  ungroup()

saveRDS(top, "shiny_app/data/top.rds")
saveRDS(top, paste0(data_folder,"final_app_files/top_", 
                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

print("top.rds data prepared and saved")

top <<- top

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

print("top_download.rds data prepared and saved")

# Saving data for open data platform
top_download <- top_download %>% 
  select(area_name, area_type, termination_month, category = chart_category,
         number_of_terminations, number_of_terminations_gest_under_10wks,
         number_of_terminations_gest_10to12wks, number_of_terminations_gest_over_12wks,
         average_gestation_at_termination)

saveRDS(top_download, paste0(open_data,"terminations_preg.rds"))

print("Open data prepared and saved")

print("###########################################")
print("Remember to change final_app_files script dates")
file.edit("data_prep/final_app_files.R")

}

##END
