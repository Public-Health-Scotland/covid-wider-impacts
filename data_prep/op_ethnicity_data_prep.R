# Original Authors - Catherine Perkins
# Orginal Date - November 2021
#
# Written/run on - RStudio server
# Version of R - 3.6.1
#
# Description - Wider Impacts dashboard - outpatients by ethnicity group 
#
# Approximate run time - 2 minutes

# All new and return appointments are included; 
# DNAs are excluded (following full outpatients methodology);
# A&E and Genitourinary specialties are excluded;
# Consultant-led appointments included only


###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.

library(odbc)          # For accessing SMRA databases
library(dplyr)         # For data manipulation in the "tidy" way
library(readr)         # For reading/writing CSVs
library(janitor)       # For 'cleaning' variable names
library(magrittr)      # For %<>% operator
library(lubridate)     # For dates
library(tidyr)         # For data manipulation in the "tidy" way
library(phsmethods)    # For a couple of useful functions


# Define the database connection with SMRA 
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

WI_data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/final_app_files/"

### read in disclosure script if required - add this to folder
#source("outpatient_functions.R")

###############################################.
## Data extraction ----
###############################################.

smr00  <- as_tibble(dbGetQuery(channel, statement=paste(
  "SELECT CLINIC_DATE, REFERRAL_TYPE, ETHNIC_GROUP ethnic_code
  FROM ANALYSIS.SMR00_PI
  WHERE CLINIC_DATE between '1 January 2018' AND '31 March 2021'
    AND SEX IN ('1', '2')
    AND CLINIC_TYPE IN ('1', '2')
    AND CLINIC_ATTENDANCE IN ('1', '5')
    AND substr(specialty,1,2) not in ('C2','AA','G6','D1','F1','R8','R9','RD','RG','RK')"))) %>% 
  setNames(tolower(names(.))) # converting variable names into lower case


# Create month & appointment type variables
smr00 <- smr00 %>%
  mutate(month_ending = floor_date(as.Date(clinic_date), "month"),
         admission_type = ifelse(referral_type %in% c(1,2), "New", "Return")) %>%
  select(-(c("referral_type")))

# Assign ethnic groups based on disaggregated groupings in COVID-19 weekly report data tables
smr00 <- smr00 %>% 
  mutate(ethnic_group = case_when(ethnic_code == "1A"~ "White Scottish",
                                  ethnic_code == "1B" ~ "White Other British",
                                  ethnic_code == "1C" ~ "White Irish",
                                  ethnic_code %in% c("1K", "1Z") ~ "White Other",
                                  ethnic_code == "1L" ~ "White Polish",
                                  ethnic_code == "2A" ~ "Mixed",
                                  ethnic_code == "3F" ~ "Pakistani",
                                  ethnic_code == "3G" ~ "Indian",
                                  ethnic_code %in% c("3H", "3Z") ~ "Other Asian",
                                  ethnic_code == "3J" ~ "Chinese",
                                  ethnic_code %in% c("4D", "4Y") ~ "African",
                                  ethnic_code %in% c("5C", "5D", "5Y") ~ "Caribbean or Black",
                                  ethnic_code %in% c("6A", "6Z") ~ "Other ethnic group",
                                  ethnic_code %in% c("98", "99") ~ "Missing",
                                  is.na(ethnic_code) ~ "Missing"))



# create monthly totals by ethnic group and admission type - New/Return
newreturn <- smr00 %>% 
  group_by(month_ending, admission_type, ethnic_group) %>% 
  summarise(count=n()) %>% ungroup
  
# create an 'All' admission type to add onto New/Return data
all <- smr00 %>% 
  group_by(month_ending, ethnic_group) %>% 
  summarise(count=n()) %>% 
  ungroup %>%
  mutate(admission_type = "All")

# add data together
outpats <- rbind(newreturn, all)

# create total for each admission type by month 
outpats <- outpats %>%  
  group_by(month_ending, admission_type) %>% 
  mutate(total = sum(count),
         percent = count/total *100) %>% #not required - just for interest
  ungroup %>% 
  mutate(area_type = "Scotland",
         area_name = "Scotland",
         type = "eth",
         spec = "All") %>% 
  rename(week_ending = month_ending, #rename to be consistent with other outpats data
         category = ethnic_group) %>% 
  select(week_ending, area_type, area_name, spec, admission_type, type, category, count, percent)


# Creating "week" (actually month) number to be able to compare pre-covid to covid period 
outpats <- outpats %>%
  mutate(week_no = as.numeric(format(week_ending,"%m")))

# Creating average appts of pre-covid data (2018 & 2019) by month of the year 
data_201819 <- outpats %>% 
  filter(year(week_ending) %in% c("2018", "2019")) %>%
  group_by(category, type, area_name, area_type, week_no, admission_type, spec) %>%
  summarise(count_average = round((sum(count, na.rm = T))/2, 1)) %>%
  ungroup()

# Join with 2020/21 data
data_202021 <- left_join(outpats %>% 
  filter(year(week_ending) %in% c("2020", "2021")), data_201819) %>% 
  # Creating %variation from precovid to covid period
  mutate(count_average = ifelse(is.na(count_average), 0, count_average),
         variation = round(-1 * ((count_average - count)/count_average * 100), 1),
         # Dealing with infinite values from historic average = 0
         variation = case_when(is.na(count) ~ 0,
                               is.na(count_average) ~ 0,
                               is.nan(variation) ~ 0,
                               is.infinite(variation) ~ 0,
                               TRUE ~ variation))


# add ethnicity data to full outpatients file
outpats_full <- readRDS(paste0(WI_data_folder, "outpats_17_May_21.rds"))
outpats_all <- bind_rows(outpats_full, data_202021) %>% 
  select(-percent)

# save for checking
#write_csv(outpats, "//PHI_conf/ScotPHO/1.Analysts_space/Catherine/wider-impacts-ethnicity/outpats_ethnicity_with_consult_only.csv")
#saveRDS(outpats_all, paste0("//PHI_conf/ScotPHO/1.Analysts_space/Catherine/wider-impacts-ethnicity/outpats_ethnicity_variation.rds"))

# save final output to shiny folder
saveRDS(outpats_all, paste0("shiny_app/data/outpats_ethnicity.rds"))
saveRDS(outpats_all, paste0(WI_data_folder,"outpats_ethnicity_", 
                        format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

### END ###.
