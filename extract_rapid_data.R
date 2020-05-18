###
# Name: SW_Records_by_Category.R
# Original Author(s): Rory Madigan
# Original Date:  April 2020
#   
# Written/run on: R Studio Server Pro 
# Base R Version: 3.5.1
# Description: This file creates an output of RAPID records to be saved in /conf/PHSCOVID19_Analysis/
# for Jaime and Vicky to use in their Shiny App for a weekly Covid publication.
# 
# Approximate run time: 5 minutes
###

###############################################.
## Section 1: Setup ----
###############################################.

#Install/Load Packages
library(data.table) #Uses functions fread() and rbindlist()
library(dplyr)

data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/" # folder for files

#Set variable values. 
health.boards <- c('A', 'B', 'F', 'G', 'H', 'L', 'N','R', 'S', 'T', 'V', 'W', 'Y', 'Z', 'X')
hb_trim <- c(A = 1, B = 2, F = 1, G = 1, H = 2, L = 2, N = 1, R = 1, S = 1, T = 2, V = 1, W = 1, Y = 2, Z = 1)

###############################################.
## Section 2: Retrieve Stay Records from RAPID ----
###############################################.
stay_by_hb <- list()
for(hb in health.boards[health.boards != 'X']) {
  
    cat('\n','Reading in Health Board:' , hb)
    
    #Read in stay file. #fread() is from the data.table package.
    stay <- fread(paste0('/syswatch/extract_files/SYSWATCH_STAY_', hb, '.csv'), header = F, data.table = F,
                  colClasses = c('character', 'character', 'character', 'numeric', 'character',
                                  'numeric',  'numeric',   'numeric',   'numeric',  'numeric', 'numeric', 'numeric',
                                  'character', 'numeric', 'character', 'character', 'character',
                                  'character', 'numeric', 'numeric', 'numeric', 'character', 'character', 'character'))

    
    ### Add column names
    colnames(stay) <- c('crn','spec','hosp','emergency','hb', 'year_adm','month_adm',
                        'day_adm','year_dis','month_dis','day_dis','medsur','age',
                        'chi', 'gp_practice',  'postcode',  'hbr',  'ipdc', 'year_adm_last', 
                        'month_adm_last', 'day_adm_last',  'in_last_extract', 'hscp_code', 'hscp_name')
    
    
    #Combine day, month, and year into a single date for both admission and discharge.
    stay$date_adm <- as.Date(paste0(stay$year_adm, '-', stay$month_adm, '-', stay$day_adm))
    
    stay$date_dis <- as.Date(NA)
    
    #The line below had the section changed from 'is.na(stay$year_dis)' to '(!stay$year_dis %in% 2000:2100)' because 
    #there was a missing year_dis that equaled "" instead of NA.
    has_discharge_date <- !((!stay$year_dis %in% 2000:2100) | is.na(stay$month_dis) | is.na(stay$day_dis)) 
    stay$date_dis[has_discharge_date]  <- as.Date(paste0(stay$year_dis[has_discharge_date], '-',
                                                         stay$month_dis[has_discharge_date], '-',
                                                         stay$day_dis[has_discharge_date]))
    
    stay <- stay %>% select(crn, chi, hb, hosp, hscp_code, hscp_name, emergency, spec, age, 
                            date_dis, date_adm, medsur, ipdc, postcode) 
    
    stay_by_hb[[hb]] <- stay
  
}#end for loop


#Combine the stays for each healthboard all into a single data frame.
stay_files <- rbindlist(stay_by_hb) %>% as.data.frame()

###############################################.
## Section 3: Filter Out Unnecessary Records from Stay Files ----
###############################################.
#This line is needed for cases where there is no hospital code given.  So far, HB 'Y' Dumfries
#and Galloway is the only health board to have had this problem.
stay_files <- stay_files %>% 
  filter(hosp != "") %>% 
#Only include inpatient cases (no day cases). Day cases data quality is problematic
# as it's not being until recently that boards started to submit
  filter(ipdc == 'I') %>% 
# Discard records belonging to Hospitals G303H in Glasgow and W106H in Western Isles.
#This list can of course be expanded to include other hospitals and in other health boards.
  filter(!hosp %in% c("G303H", "W106H"))

#---#This section was added on 04-Mar-2019 to bring the results closer to what they would be in the rest of System Watch.
#Exclude specialties G1,G3,G4,and G5.  Not sure if this should be included for data given to Jaime and Vicky?
excluded_specialties <- c('G1', 'G3', 'G4', 'G5')
stay_files <- stay_files %>% filter(!stay_files$spec %in% excluded_specialties)

#This line switches any cases with the specialty of 'AJ' (Integrative Care) from medsur = 0 to medsur = 2.
stay_files[stay_files$spec == 'AJ', 'medsur'] <- 2

#This line excludes any specialties where medsur = 0 so that they won't be included anymore in the 'adm' or 'bed'
#stay types which used to sum any specialty group.
stay_files <- stay_files %>% filter(medsur != 0)

#Odd CHI numbers represent males, evens represent females.
stay_files$sex <- ifelse(stay_files$chi %% 2 == 1, yes = 'male', no = 'female')

rapid <- stay_files %>% select(hb, hosp, hscp_code, hscp_name, emergency, spec, age, date_adm, date_dis, sex, postcode)

rapid$source <- 'RAPID'

###############################################.
## Section 4: Extra Steps to perform on RAPID records  ----
###############################################.
rapid$admission_type <- ifelse(rapid$emergency == 1, yes = 'emergency', no = 'elective')

rapid$age <- as.numeric(rapid$age)

rapid <- rapid %>% mutate(age_group = case_when(
                              age   >= 85    ~ '85+',
                              age %in% 75:84 ~ '75_thru_84',
                              age %in% 65:74 ~ '65_thru_74',
                              age %in% 45:64 ~ '45_thru_64',
                              age %in% 15:44 ~ '15_thru_44', 
                              age %in% 5:14  ~ '5_thru_14',
                              age %in% 0:4   ~ 'Under_5',
                              TRUE ~ 'missing') ) 

### Subsection: Lookup SIMDs by postcode.
simd_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2019_2_simd2020.rds') %>% 
  select(pc7, simd2020_sc_quintile) %>% as.data.table

#Using a data table lookup is much faster than using an index lookup in base R.  
system.time( setkey(simd_lookup, pc7) )
system.time( rapid$simd_quintile <- simd_lookup[ .(rapid$postcode), nomatch = NA]$simd2020_sc_quintile )
rapid <- rapid %>% select(-postcode) #The postcodes are no longer needed now that we have the SIMDs.

rapid <- rapid %>% arrange(admission_type, hb, hosp, hscp_code, emergency, spec, sex, simd_quintile, date_adm)

###############################################.
## Section 5: Create and add records that are totals for Scotland, each board, and each location ----
###############################################.

# First, get data frame for all records within Scotland by Admission_Type, spec, SIMD_quintile, age_group, sex, and date_adm.
scot_totals <- rapid %>% 
    group_by(admission_type, spec, simd_quintile, age_group, sex, date_adm) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(hb = 'X', hosp = 'X_All', hscp_code = '', hscp_name = 'All_Scotland') %>% 
    select(hb, hosp, hscp_code, hscp_name, admission_type, spec, #put the columns in the proper order.
           simd_quintile, age_group, sex, date_adm, count)

# Second, get health board totals by Admission_Type, spec, SIMD_quintile, age_group, sex, and date_adm.
hb_totals <- rapid %>% 
    group_by(hb, admission_type, spec, simd_quintile, age_group, sex, date_adm) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(hosp = paste0(hb, '_All'), hscp_code = '', hscp_name = paste0(hb, '_All')) %>% 
    select(hb, hosp, hscp_code, hscp_name, admission_type, spec,  #put the columns in the proper order.
           simd_quintile, age_group, sex, date_adm, count)

# Third, get all totals within each location (hosp and HSCP) by Admission_Type, spec, SIMD_quintile, age_group, sex, and date_adm.
all_records_within_locations <- rapid %>% 
    group_by(hb, hosp, hscp_code, hscp_name, admission_type, spec, simd_quintile, age_group, sex, date_adm) %>% 
    summarise(count = n()) %>% 
    ungroup() %>%  
    select(hb, hosp, hscp_code, hscp_name, admission_type,  #put the columns in the proper order.
           spec, simd_quintile, age_group, sex, date_adm, count)

combined_records <- rbind(all_records_within_locations, hb_totals, scot_totals ) #Merge all three data frames into one.

###############################################.
## Section 6: Determining Start and End Dates ----
###############################################.
#I'm guessing RAPID should be good until 4 years from the present, this will have to be checked however.
combined_records <- combined_records %>% filter(date_adm >= paste0(year(Sys.Date()) - 4, '-01-01'))
#This takes care of the start date.

recent_admissions_by_hosp <- rapid %>% filter(date_adm %in% Sys.Date():(Sys.Date() - 100)) %>% 
  group_by(hosp, hb) %>% #Get the count of admissions for each hospital within the last 100 days
  summarise(mean_adm_per_day = n()/101, end = max(date_adm)) #as well as the last date of admission for each one.

hb_end_dates <- recent_admissions_by_hosp %>% 
  filter(mean_adm_per_day >= 2.5) %>% #A board should only wait for a hospital if it has more than about 2.5 admissions per day.
  select(hosp, hb, end) %>% group_by(hb) %>% 
  summarise(end = min(end)) #Only take the earliest hospital end dates within each health board.

#Here we are removing a set number of days (either 1 or 2) from the end dates.  
#These days to cut off of the end have tradionally been used in System Watch to ensure we have complete data from each board.
hb_end_dates$end = hb_end_dates$end - HB_trim[hb_end_dates$hb] 

lookup_end_date_by_hb <- hb_end_dates$end
names(lookup_end_date_by_hb) <- hb_end_dates$hb

#The end date for Scotland is the earliest end from all of the health boards.
#This ensures that the Scotland totals won't be missing records from any health boards.
lookup_end_date_by_hb['X'] <- min(lookup_end_date_by_hb) 

#Here we finally remove any records where the date of admission is after the date for which we are sure a HB has complete data.
combined_records <- combined_records %>% filter(date_adm <= lookup_end_date_by_hb[combined_records$hb])

#Save the file to a shared area. 
date_on_filename <- format(Sys.Date(), format = '%d-%b')
saveRDS(combined_records, paste0(data_folder, 'rapid/Admissions_by_category_', date_on_filename, '.rds') ) 


##END
