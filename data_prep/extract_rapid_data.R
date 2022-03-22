###
# Written/run on: R Studio Server Pro 
# Base R Version: 3.5.1
# Description: This file creates an output of RAPID records to be saved in /conf/PHSCOVID19_Analysis/
# then it goes through further transformations in app_data_preparation.
# 
# Notes:
# Discard records belonging to Hospitals G303H in Glasgow (Mearnskirk House) and W106H in Western Isles (St Brendans Cot Hosp).
# Only include inpatient cases (no day cases). Day cases data quality is problematic as not all boards submitted this data until recently.
# Exclude specialties G1,G3,G4,and G5 - psychiatric care specialties. Have also added G2 which seemed to be missing before.
    # G1	General Psychiatry (Mental Illness)
    # G1A	Community Psychiatry
    # G2	Child & Adolescent Psychiatry
    # G21	Child Psychiatry
    # G22	Adolescent Psychiatry
    # G3	Forensic Psychiatry
    # G4	Psychiatry of Old Age
    # G5	Learning Disability

# In WID notes, it says: Exclusions from the RAPID dataset are day cases, neonatal, maternity and 
# psychiatric care admissions. Admissions to the Golden Jubilee National Hospital are also not included. 

# Approximate run time: 5 minutes
###

###############################################.
## Section 1: Setup ----
###############################################.

#Load packages.
library(odbc)
library(dplyr)
library(readr)
library(lubridate)


#Make connection to the RAPID Database through the Denodo Virtualisation Platform.
RAPID_connection <- dbConnect(odbc(),
                              dsn="DVPROD",
                              uid=.rs.askForPassword("Username:"),
                              pwd=.rs.askForPassword("Password:"))


rapid_extract <- as_tibble(dbGetQuery(RAPID_connection, statement=paste0(
                        "SELECT crn, chi, patient_gender_code sex, hospital_of_treatment_location_code hosp, 
                      emergency_admission_flag, hospital_of_treatment_nhs_board_code_current hb, admission_date date_adm,
                      discharge_date, specialty spec, age_on_admission age, 
                      postcode, inpatient_daycase_identifier_code ipdc, hscp_of_residence_code_current hscp_code, 
                      hscp_of_residence_name_current hscp_name, ethnic_group_code, ethnic_group_description
                      FROM rapid.syswatch_hosp_stay      #name of RAPID Stay Table 
                      WHERE ADMISSION_DATE >= '2017-01-01' 
                      AND inpatient_daycase_identifier_code ='IP'
                      AND hospital_of_treatment_location_code NOT IN ('W106H', 'G303H', 'D102H')
                      CONTEXT ('i18n'='gb', 'cache_wait_for_load'='true');")))  #Not sure if the CONTEXT line is needed.


data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/" # folder for files

convert_spec_to_spec_grouping <- function(spec, return_spec_group_lists = FALSE) {
  
  spec_group_1_surgical <- c('C1', 'C11', 'C12', 'C13', 
                             'C14', #new on 01-Dec-2021.
                             'C4', 'C41', 'C42', 'C5', 'C51', 'C6', 'C7', 'C8', 
                             #'C9', 'CB', 'D3', 'D4', 'D5', 'D6', 'D8', 'F2') 
                             'C9', 'C91', 'CB', 'CC', 'D3', 'D4', 'D5', 'D6', 'D8', 'F2') #modified by RM on 29-Sep-2020.
  
  spec_group_2_medical <- c('A1', 'A11', 'A2', 'A3', 'A6', 'A7', 'A8', 'A81', 'A82', 'A9',
                            # 'AA', 'AB', 'AC', 'AD', 'AG', 'AH', 'AM', 'AP', 'AQ', 'AR', 'AV', #This is the old line.
                            'AA', 'AB', 'AC', 'AD', 'AG', 'AH', 'AJ', 'AM', 'AP', 'AQ', 'AR', 'AV', #This is the new line as of July 26, 2017.
                            'AW', 'C2', 'C3', 'C31', 'D1', 'E12', 'H1', 'H2', 'J3', 'J4', 
                            'J5', 'R1', 'R11')
  
  spec_group_3_paediatric <- c('A21', 'AF', 'CA')
  
  #This is if return_spec_group_lists == TRUE, which essentially makes it another function that now just 
  if(return_spec_group_lists == TRUE) {  # returns all of the included specialties instead of returning the specialty groupings.
    return(list(spec_group_1_surgical   = spec_group_1_surgical, 
                spec_group_2_medical    = spec_group_2_medical, 
                spec_group_3_paediatric = spec_group_3_paediatric) )
  } #end if statement
  
  #Create a vector for the specialty groupings and set them to what they should be.
  spec_grouping <- numeric(length(spec))
  spec_grouping[ spec %in% spec_group_1_surgical ]   <- 1L
  spec_grouping[ spec %in% spec_group_2_medical ]    <- 2L
  spec_grouping[ spec %in% spec_group_3_paediatric ] <- 3L
  
  return(spec_grouping)
  
} #end function convert_spec_to_spec_grouping().



###############################################.
## Data manipulation ----
###############################################.

# Exclude psychiatric care specialties. 
# Exclude maternity and neonatal specialties?
rapid_extract <- rapid_extract %>% 
  filter(!spec %in% c('G1', 'G1A', 'G2', 'G21', 'G22', 'G3', 'G4', 'G5'
                     # , 'F1', 'F1A', 'F1B', 'F2', 'F3', 'F31', 'F32', 'F4'
                      )) %>%
  
  mutate(admission_type = case_when(emergency_admission_flag == 'Y' ~ 'emergency',
                                    TRUE ~ 'elective'),
         sex = case_when(sex == '1' ~ 'male',
                         sex == '2' ~ 'female'),
         age = as.numeric(age),
         date_adm = as.Date(date_adm),
         # create age groups
         age_group = case_when(
           age   >= 85    ~ '85+',
           age %in% 75:84 ~ '75_thru_84',
           age %in% 65:74 ~ '65_thru_74',
           age %in% 45:64 ~ '45_thru_64',
           age %in% 15:44 ~ '15_thru_44', 
           age %in% 5:14  ~ '5_thru_14',
           age %in% 0:4   ~ 'Under_5',
           TRUE ~ 'missing'))


### Add SIMD from lookup by postcode.
simd_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2021_2_simd2020v2.rds') %>% 
  select(pc7, simd2020v2_sc_quintile) %>% 
  rename(postcode = pc7,
         simd_quintile = simd2020v2_sc_quintile)

# Add SIMD to rapid dataset
rapid <- left_join(rapid_extract, simd_lookup, "postcode") %>% 
  select(-postcode, -crn, -chi, -emergency_admission_flag, -discharge_date) %>% 
  arrange(admission_type, hb, hosp, hscp_code, spec, sex, simd_quintile, date_adm)

###############################################.
## Create totals for Scotland, each board, and each location ----
###############################################.

# Scotland totals - by admission_type, spec, SIMD_quintile, age_group, sex, and date_adm.
scot_totals <- rapid %>% 
    group_by(admission_type, spec, simd_quintile, age_group, sex, date_adm) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(hb = 'X', hosp = 'X_All', hscp_code = '', hscp_name = 'All_Scotland') %>% 
    select(hb, hosp, hscp_code, hscp_name, admission_type, spec, #put the columns in the proper order.
           simd_quintile, age_group, sex, date_adm, count)

# Health Board totals - by admission_type, spec, SIMD_quintile, age_group, sex, and date_adm.
hb_totals <- rapid %>% 
    group_by(hb, admission_type, spec, simd_quintile, age_group, sex, date_adm) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    mutate(hosp = paste0(hb, '_All'), hscp_code = '', hscp_name = paste0(hb, '_All')) %>% 
    select(hb, hosp, hscp_code, hscp_name, admission_type, spec,  #put the columns in the proper order.
           simd_quintile, age_group, sex, date_adm, count)

# Hosp and hscp totals - by admission_type, spec, SIMD_quintile, age_group, sex, and date_adm.
all_locations <- rapid %>% 
    group_by(hb, hosp, hscp_code, hscp_name, admission_type, spec, simd_quintile, age_group, sex, date_adm) %>% 
    summarise(count = n()) %>% 
    ungroup() %>%  
    select(hb, hosp, hscp_code, hscp_name, admission_type,  #put the columns in the proper order.
           spec, simd_quintile, age_group, sex, date_adm, count)

combined_records <- rbind(all_locations, hb_totals, scot_totals) #Merge all three data frames into one.

###############################################.
## Determining Start and End Dates ----
###############################################.

# I'm guessing RAPID should be good until 4 years from the present, this will have to be checked however.
combined_records <- combined_records %>% filter(date_adm >= paste0(year(Sys.Date()) - 4, '-01-01'))

### DOES THIS SECTION NEED TO BE INCLUDED? I THINK WE SPECIFY LAST_WEEK DATE IN THE DATA PREP SCRIPT?
# recent_admissions_by_hosp <- rapid %>% filter(date_adm %in% Sys.Date():(Sys.Date() - 100)) %>% 
#   group_by(hosp, hb) %>% #Get the count of admissions for each hospital within the last 100 days
#   summarise(mean_adm_per_day = n()/101, 
#             end = max(date_adm)) #as well as the last date of admission for each one.
# 
# hb_end_dates <- recent_admissions_by_hosp %>% 
#   filter(mean_adm_per_day >= 2.5) %>% #A board should only wait for a hospital if it has more than about 2.5 admissions per day.
#   select(hosp, hb, end) %>% group_by(hb) %>% 
#   summarise(end = min(end)) #Only take the earliest hospital end dates within each health board.
# 
# # Here we are removing a set number of days (either 1 or 2) from the end dates.  
# # These days to cut off of the end have tradionally been used in System Watch to ensure we have complete data from each board.
# hb_end_dates$end = hb_end_dates$end - hb_trim[hb_end_dates$hb] 
# 
# lookup_end_date_by_hb <- hb_end_dates$end
# names(lookup_end_date_by_hb) <- hb_end_dates$hb
# 
# # The end date for Scotland is the earliest end from all of the health boards.
# # This ensures that the Scotland totals won't be missing records from any health boards.
# lookup_end_date_by_hb['X'] <- min(lookup_end_date_by_hb) 
# 
# # Remove any records where the date of admission is after the date for which we are sure a HB has complete data.
# combined_records <- combined_records %>% filter(date_adm <= lookup_end_date_by_hb[combined_records$hb])


# Save file with date
date_on_filename <<- format(Sys.Date(), format = '%Y-%m-%d')
saveRDS(combined_records, paste0(data_folder, 'rapid/', date_on_filename, '-admissions-by-category.rds')) 

#temp save for checking
#write_csv(combined_records, paste0("//PHI_conf/ScotPHO/1.Analysts_space/Catherine/wid-rapid-update/", date_on_filename, "-admissions-by-category.csv"))



##END

