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


#### Section 1: Setup ####

#Install/Load Packages
library(data.table) #Uses functions fread() and rbindlist()
library(dplyr)

#Set variable values. 
health.boards <- c('A', 'B', 'F', 'G', 'H', 'L', 'N','R', 'S', 'T', 'V', 'W', 'Y', 'Z', 'X')
HB_trim <- c(A = 1, B = 2, F = 1, G = 1, H = 2, L = 2, N = 1, R = 1, S = 1, T = 2, V = 1, W = 1, Y = 2, Z = 1)

#### End of Section 1 #### #---------------------------------------------------#



#### Section 2: Retrieve Stay Records from RAPID ####

Stay_by_HB <- list()
for(HB in health.boards[health.boards != 'X']) {
  
    cat('\n','Reading in Health Board:' , HB)
    
    #Read in stay file. #fread() is from the data.table package.
    Stay <- fread(paste0('/syswatch/extract_files/SYSWATCH_STAY_', HB, '.csv'), header = F, data.table = F,
                  colClasses = c('character', 'character', 'character', 'numeric', 'character',
                                  'numeric',  'numeric',   'numeric',   'numeric',  'numeric', 'numeric', 'numeric',
                                  'character', 'numeric', 'character', 'character', 'character',
                                  'character', 'numeric', 'numeric', 'numeric', 'character', 'character', 'character'))

    
    ### Add column names
    colnames(Stay) <- c('CRN','spec','hosp','emergency','hb', 'year_adm','month_adm',
                        'day_adm','year_dis','month_dis','day_dis','medsur','age',
                        'chi', 'gp_practice',  'postcode',  'hbr',  'ipdc', 'year_adm_last', 
                        'month_adm_last', 'day_adm_last',  'in_last_extract', 'HSCP_code', 'HSCP_name')
    
    
    #Combine day, month, and year into a single date for both admission and discharge.
    Stay$date_adm <- as.Date(paste0(Stay$year_adm, '-', Stay$month_adm, '-', Stay$day_adm))
    
    Stay$date_dis <- as.Date(NA)
    
    #The line below had the section changed from 'is.na(Stay$year_dis)' to '(!Stay$year_dis %in% 2000:2100)' because 
    #there was a missing year_dis that equaled "" instead of NA.
    has_discharge_date <- !((!Stay$year_dis %in% 2000:2100) | is.na(Stay$month_dis) | is.na(Stay$day_dis)) 
    Stay$date_dis[has_discharge_date]  <- as.Date(paste0(Stay$year_dis[has_discharge_date], '-',
                                                         Stay$month_dis[has_discharge_date], '-',
                                                         Stay$day_dis[has_discharge_date]))
    
    Stay <- Stay %>% select(CRN, chi, hb, hosp, HSCP_code, HSCP_name, emergency, spec, age, 
                            date_dis, date_adm, medsur, ipdc, postcode) 
    
    Stay_by_HB[[HB]] <- Stay
  
}#end for loop


#Combine the stays for each healthboard all into a single data frame.
Stay_Files <- rbindlist(Stay_by_HB) %>% as.data.frame()

#### End of Section 2 #### #---------------------------------------------------#



#### Section 3: Filter Out Unnecessary Records from Stay Files ####

#This line is needed for cases where there is no hospital code given.  So far, HB 'Y' Dumfries
#and Galloway is the only health board to have had this problem.
Stay_Files <- Stay_Files %>% filter(hosp != "") 

#Only include inpatient cases (no day cases). 
Stay_Files <- Stay_Files %>% filter(ipdc == 'I') 

#These two lines discard records belonging to Hospitals G303H in Glasgow and W106H in Western Isles.
#This list can of course be expanded to include other hospitals and in other health boards.
Discarded_Hospitals <- readRDS('/conf/rstudiosystemwatch/Object_Workspace/Discarded_Hospitals.rds') %>% unlist()
Stay_Files <- Stay_Files %>% filter(!hosp %in% Discarded_Hospitals)


#---#This section was added on 04-Mar-2019 to bring the results closer to what they would be in the rest of System Watch.
#Exclude specialties G1,G3,G4,and G5.  Not sure if this should be included for data given to Jaime and Vicky?
excluded_specialties <- c('G1', 'G3', 'G4', 'G5')
Stay_Files <- Stay_Files %>% filter(!Stay_Files$spec %in% excluded_specialties)

#This line switches any cases with the specialty of 'AJ' (Integrative Care) from medsur = 0 to medsur = 2.
Stay_Files[Stay_Files$spec == 'AJ', 'medsur'] <- 2

#This line excludes any specialties where medsur = 0 so that they won't be included anymore in the 'adm' or 'bed'
#stay types which used to sum any specialty group.
Stay_Files <- Stay_Files %>% filter(medsur != 0)

#Odd CHI numbers represent males, evens represent females.
Stay_Files$sex <- ifelse(Stay_Files$chi %% 2 == 1, yes = 'male', no = 'female')


RAPID <- Stay_Files %>% select(hb, hosp, HSCP_code, HSCP_name, emergency, spec, age, date_adm, date_dis, sex, postcode)

RAPID$source <- 'RAPID'

#### End of Section 3 #### #---------------------------------------------------#




#### Section 4: Extra Steps to perform on RAPID records ####

RAPID$Admission_Type <- ifelse(RAPID$emergency == 1, yes = 'emergency', no = 'elective')

RAPID$age <- as.numeric(RAPID$age)

RAPID <- RAPID %>% mutate(age_group = case_when(
                              age   >= 85    ~ '85+',
                              age %in% 75:84 ~ '75_thru_84',
                              age %in% 65:74 ~ '65_thru_74',
                              age %in% 45:64 ~ '45_thru_64',
                              age %in% 15:44 ~ '15_thru_44', 
                              age %in% 5:14  ~ '5_thru_14',
                              age %in% 0:4   ~ 'Under_5',
                              TRUE ~ 'missing') ) 


### Subsection: Lookup SIMDs by postcode.
SIMD_Table <- readRDS('/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2019_2_simd2020.rds')

#Using a data table lookup is much faster than using an index lookup in base R.  
SIMD_Lookup <- SIMD_Table[ , c('pc7', 'simd2020_sc_quintile')]
SIMD_Lookup <- as.data.table(SIMD_Lookup)
system.time( setkey(SIMD_Lookup, pc7) )
system.time( RAPID$SIMD_quintile <- SIMD_Lookup[ .(RAPID$postcode), nomatch = NA]$simd2020_sc_quintile )
RAPID <- RAPID %>% select(-postcode) #The postcodes are no longer needed now that we have the SIMDs.
### End of Subsection ###

RAPID <- RAPID %>% arrange(Admission_Type, hb, hosp, HSCP_code, emergency, spec, sex, SIMD_quintile, date_adm)

#### End of Section 4 #### #---------------------------------------------------#



#### Section 5: Create and add records that are totals for Scotland, each board, and each location ####

# First, get data frame for all records within Scotland by Admission_Type, spec, SIMD_quintile, age_group, sex, and date_adm.
Scotland_Totals <- RAPID %>% 
    group_by(Admission_Type, spec, SIMD_quintile, age_group, sex, date_adm) %>% 
    summarise(Count = n()) %>% 
    ungroup() %>% 
    mutate(hb = 'X', hosp = 'X_All', HSCP_code = '', HSCP_name = 'All_Scotland') %>% 
    select(hb, hosp, HSCP_code, HSCP_name, Admission_Type, spec, #put the columns in the proper order.
           SIMD_quintile, age_group, sex, date_adm, Count)


# Second, get health board totals by Admission_Type, spec, SIMD_quintile, age_group, sex, and date_adm.
Totals_by_HB <- RAPID %>% 
    group_by(hb, Admission_Type, spec, SIMD_quintile, age_group, sex, date_adm) %>% 
    summarise(Count = n()) %>% 
    ungroup() %>% 
    mutate(hosp = paste0(hb, '_All'), HSCP_code = '', HSCP_name = paste0(hb, '_All')) %>% 
    select(hb, hosp, HSCP_code, HSCP_name, Admission_Type, spec,  #put the columns in the proper order.
           SIMD_quintile, age_group, sex, date_adm, Count)


# Third, get all totals within each location (hosp and HSCP) by Admission_Type, spec, SIMD_quintile, age_group, sex, and date_adm.
All_Records_within_Location <- RAPID %>% 
    group_by(hb, hosp, HSCP_code, HSCP_name, Admission_Type, spec, SIMD_quintile, age_group, sex, date_adm) %>% 
    summarise(Count = n()) %>% 
    ungroup() %>%  
    select(hb, hosp, HSCP_code, HSCP_name, Admission_Type,  #put the columns in the proper order.
           spec, SIMD_quintile, age_group, sex, date_adm, Count)


Combined_Records <- rbind(All_Records_within_Location, Totals_by_HB, Scotland_Totals ) #Merge all three data frames into one.

#### End of Section 5 #### #---------------------------------------------------#



#### Section 6: Determining Start and End Dates ####

#I'm guessing RAPID should be good until 4 years from the present, this will have to be checked however.
Combined_Records <- Combined_Records %>% filter(date_adm >= paste0(year(Sys.Date()) - 4, '-01-01'))
#This takes care of the start date.

Recent_Admissions_by_hosp <- RAPID %>% filter(date_adm %in% Sys.Date():(Sys.Date() - 100)) %>% 
  group_by(hosp, hb) %>% #Get the count of admissions for each hospital within the last 100 days
  summarise(mean_adm_per_day = n()/101, end = max(date_adm)) #as well as the last date of admission for each one.

HB_end_Dates <- Recent_Admissions_by_hosp %>% 
  filter(mean_adm_per_day >= 2.5) %>% #A board should only wait for a hospital if it has more than about 2.5 admissions per day.
  select(hosp, hb, end) %>% group_by(hb) %>% 
  summarise(end = min(end)) #Only take the earliest hospital end dates within each health board.

#Here we are removing a set number of days (either 1 or 2) from the end dates.  
#These days to cut off of the end have tradionally been used in System Watch to ensure we have complete data from each board.
HB_end_Dates$end = HB_end_Dates$end - HB_trim[HB_end_Dates$hb] 

lookup_end_date_by_HB <- HB_end_Dates$end
names(lookup_end_date_by_HB) <- HB_end_Dates$hb

#The end date for Scotland is the earliest end from all of the health boards.
#This ensures that the Scotland totals won't be missing records from any health boards.
lookup_end_date_by_HB['X'] <- min(lookup_end_date_by_HB) 

#Here we finally remove any records where the date of admission is after the date for which we are sure a HB has complete data.
Combined_Records <- Combined_Records %>% filter(date_adm <= lookup_end_date_by_HB[Combined_Records$hb])

#### End of Section 6 #### #---------------------------------------------------#



#### Section 7: Save the output! ####

#Save the file to a shared area. 
date_on_filename <- format(Sys.Date(), format = '%d-%b')
saveRDS(Combined_Records, paste0('/conf/PHSCOVID19_Analysis/Admissions_by_category_', date_on_filename, '.rds') ) 



