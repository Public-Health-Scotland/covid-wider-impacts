# Extracting google analytics data for wider impacts
# From https://code.markedmondson.me/googleAnalyticsR/articles/setup.html
# Google Analytics API guide: https://ga-dev-tools.web.app/dimensions-metrics-explorer/

###############################################.
## Packages ----
###############################################.

library(googleAnalyticsR) #to extract google analytics data
library(plotly) # for charts
library(dplyr) #for data manipulation
library(magrittr) # for pipe operators
library(lubridate) #for date operations
library(rmarkdown) # for running report
library(janitor) #to clean names

###############################################.
## Lookups/filepaths ----
###############################################.

data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/google_analytics/"

# Lookup of event names and their equivalent tab name
list_events <- data.frame( stringsAsFactors = F,
  eventlabel = c("apgar",  "booking", "breastfeeding", "cancer", 
                 "cardio",  "child_dev", "child_health", 
                 "comment", "drugs", "gestation", "imm", "inductions", "injuries", "intro", 
                 "mentalhealth", "mod", "perinatal_mortality",  "preterm", 
                 "sact", "summary", "table", "tears", "terminations"),
  tabname = c("Apgar scores", "Antenatal bookings", "Breastfeeding", "Cancer pathology",
              "Cardiovascular", "Child development", "Child health reviews", "Commentary",
              "Substance use", "Gestation at delivery", "Immunisations", "Induction of labour", "Injuries", "Home page",
              "Mental health", "Mode of delivery", "Stillbirths and infant deaths",
              "Location of extremely preterm deliveries", "SACT", "Summary trends",
              "Data", "Perineal tears", "Termination of pregnancy"),
  tabarea = c("Pregnancy/births/babies", "Pregnancy/births/babies", "Child health", 
              "Cancer", "Others", "Child health", "Child health", "Others",
              "Substance use", "Pregnancy/births/babies", "Child health", "Pregnancy/births/babies", "Others", "Others",
              "Others", "Pregnancy/births/babies", "Pregnancy/births/babies",
              "Pregnancy/births/babies", "Cancer", "Others",
              "Others", "Pregnancy/births/babies", "Pregnancy/births/babies"))


###############################################.
## Connecting to GA universal and extracting data ----
###############################################.
# Select 1: Yes to say you wish to keep your OAuth access credentials.
# The library should then launch a browser window and ask you to login to Google - 
# log in with an email that has access to your Google Analytics - it will take you 
# to a screen with an OOB token. Copy-paste that token back into RStudio:
# email_login is an object with the email address of the account with access to GA
# It sits in the environment file. To create that file, create a new text file and add
# email = "emailyouareusing@providerofyouremail.com" then save in the root folder
# of the project with the name ".env" This file is not picked by Git
readRenviron(".env")
email_login <- Sys.getenv("email_login")
ga_auth(email = email_login)

# See list of views/tables you have access to
View(ga_account_list())

## View account_list and pick the viewId you want to extract data from. 
ga_id <- 222359188
last_date <- "2022-09-06"
last_year <- "2021-09-06"

## Query getting sessions data from first day we got data from to today
sessions_ga <- google_analytics(ga_id,
                 date_range = c("2020-06-29", last_date),
                 metrics = c("sessions", "sessionDuration"),
                 dimensions = c("date"),
                 max = -1) # this brings all results

## Query to obtain where people comes from and what they use
source_ga <- google_analytics(ga_id,
                                date_range = c(last_year, last_date),
                                metrics = c("sessions", "sessionDuration"),
                                dimensions = c('source','medium', "deviceCategory"),
                                max = -1) # this brings all results

## Query to obtain country/city of users
geo_ga <- google_analytics(ga_id,
                              date_range = c(last_year, last_date),
                              metrics = c("sessions", "sessionDuration"),
                              dimensions = c('city','region', 'metro', 'country'),
                              max = -1) # this brings all results

## Query getting events data from first day we got data from to today
events_ga <- google_analytics(ga_id,
                 date_range = c("2022-02-07", "2022-09-12"),
                 metrics = c("totalEvents"),
                 dimensions = c("date", "eventlabel"),
                 max = -1) %>%  # this brings all results
  rename(event_count = totalEvents)

###############################################.
## Extracting data from Google analytics 4 ----
###############################################.
# Both systems running at the same time, but account things differently. A boundary
# between both needs to be taken
## View account_list and pick the viewId you want to extract data from. 
ga_id <- 281606947 
start_date <- "2022-09-07"
last_date <- paste0(Sys.Date())
# last_year <- paste0(Sys.Date() - 365)

View(ga_meta("data", propertyId = ga_id)) # all variables available, including custom

## Query getting sessions data from first day we got data from to today
sessions_ga4 <- ga_data(ga_id,
                       date_range = c(start_date, last_date),
                       metrics = c("sessions", "averageSessionDuration"),
                       dimensions = c("date"))

## Query to obtain where people comes from and what they use
source_ga4 <- ga_data(ga_id,
                     date_range = c(start_date, last_date),
                     metrics = c("sessions", "averageSessionDuration"),
                     dimensions = c('sessionSource','sessionMedium', "deviceCategory"))

## Query to obtain country/city of users
geo_ga4 <- ga_data(ga_id,
                  date_range = c(start_date, last_date),
                  metrics = c("sessions", "averageSessionDuration"),
                  dimensions = c('city','region', 'country'))

## Query getting events data from first day we got data from to today
events_ga4 <- ga_data(ga_id,
                     date_range = c("2022-09-12", last_date),
                     metrics = c("eventCount"),
                     dimensions = c("date", "customEvent:event_label")) %>% 
  clean_names() %>%  rename(eventlabel = custom_event_event_label)

## Query to obtain pages consulted
pages_ga4 <- ga_data(ga_id,
                    date_range = c(start_date, last_date),
                    metrics = c("sessions"),
                    dimensions = c('pagePathPlusQueryString', "hostName"))

###############################################.
## Formatting data ----
###############################################.
# Session data 
# Calculating week endding and aggregating to obtain totals
sessions_un <- sessions_ga %>% 
  mutate(week_ending = ceiling_date(date, "week", change_on_boundary = F)) %>% 
  rename(tot_session = sessionDuration)

# Calculating week ending and aggregating to obtain totals
sessions_4 <- sessions_ga4 %>% 
  mutate(week_ending = ceiling_date(date, "week", change_on_boundary = F),
         tot_session = sessions * averageSessionDuration) %>% 
  select(-averageSessionDuration)

sessions <- rbind(sessions_un, sessions_4) %>%   
  group_by(week_ending) %>% 
  summarise(count = sum(sessions, na.rm = T),
            tot_session = sum(tot_session)) %>% ungroup() %>% 
  mutate(session_ave = round(tot_session/count/60, 1)) 

saveRDS(sessions, paste0(data_folder, "sessions.rds"))

###############################################.
# Events data 
# Joining to obtain names
events <- rbind(events_ga, events_ga4) %>% left_join(list_events) 

# Filtering out tab menus and not sets (these are internal links with no value)
events %<>% 
  filter(!eventlabel %in% c("(not set)", "Births and babies", "Cancer",
                            "Child health", "Pregnancy", "intro"))

# Creating yearly totals
events_year <- events %>% 
  group_by(tabname, tabarea) %>% 
  summarise(count = sum(event_count, na.rm = T)) %>% ungroup()

saveRDS(events_year, paste0(data_folder, "tabvisits_yearly.rds"))

# Calculating week ending and aggregating to obtain totals
events %<>% 
  mutate(week_ending = ceiling_date(date, "week", change_on_boundary = F)) %>% 
  group_by(week_ending, tabname, tabarea) %>% 
  summarise(count = sum(event_count, na.rm = T)) %>% ungroup()

# Splitting data for each tab area to help with report/plotting
events <- split.data.frame(events, events$tabarea)

saveRDS(events, paste0(data_folder, "tabvisits.rds"))

###############################################.
# Source data
source_agg_un <- source_ga %>% 
  group_by(source, medium) %>% 
  summarise(count = sum(sessions),
            tot_session = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(tot_session/count/60, 1)) 

source_agg_ga4 <- source_ga4 %>% 
  mutate(tot_session = sessions * averageSessionDuration) %>% 
  rename(source = sessionSource, medium = sessionMedium) %>% 
  group_by(source, medium) %>% 
  summarise(count = sum(sessions),
            tot_session = sum(tot_session, na.rm = T)) %>% ungroup() %>% 
  mutate(session_ave = round(tot_session/count/60, 1))

source_agg <- rbind(source_agg_un, source_agg_ga4) %>% 
  slice_max(count, n = 20) %>% # selecting only top 20
  select(Source = source, Sessions = count, 
         "Average session length (minutes)" = session_ave)

saveRDS(source_agg, paste0(data_folder, "source.rds"))

###############################################.
# Device + session duration 
device_un <- source_ga %>% 
  group_by(deviceCategory) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1),
         device = make_clean_names(deviceCategory, case = "title")) 

device_ga4 <- source_ga4 %>% 
  mutate(tot_session = sessions * averageSessionDuration) %>% 
  group_by(deviceCategory) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(tot_session)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1),
         device = make_clean_names(deviceCategory, case = "title")) 

device <- rbind(device_un, device_ga4)

tot <- device %>%
  mutate(device = "Total") %>% 
  group_by(device) %>% 
  summarise(count = sum(count),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1))

device <- bind_rows(tot, device) %>% 
  select(Device = device, Sessions = count, 
         "Average session length (minutes)" = session_ave)

saveRDS(device, paste0(data_folder, "device.rds"))

###############################################.
# Geography of users - Not adding this at the moment to the report
city_top <- geo_ga %>% 
  group_by(city) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) 

country_top <- geo_ga %>% 
  group_by(country) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(sessionDuration)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) 

city_top <- geo_ga4 %>% 
  mutate(tot_session = sessions * averageSessionDuration) %>% 
  group_by(city) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(tot_session)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) 

country_top <- geo_ga4 %>% 
  mutate(tot_session = sessions * averageSessionDuration) %>% 
  group_by(country) %>% 
  summarise(count = sum(sessions),
            sessionDuration = sum(tot_session)) %>% ungroup() %>% 
  mutate(session_ave = round(sessionDuration/count/60, 1)) 

###############################################.
# Pages
# This could be used to clean up visits from us running code
saveRDS(pages_ga4, paste0(data_folder, "pages.rds"))

###############################################.
## Creating report ----
###############################################.
rmarkdown::render("google_analytics/ga_report.Rmd")

## END