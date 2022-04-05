###
# Written/run on: R Studio Server Pro 
# Base R Version: 3.5.1
# Description: This file creates an output of RAPID records to be saved in /conf/PHSCOVID19_Analysis/
# then it goes through further transformations in app_data_preparation.
# 
# Notes:
# Discard records belonging to Hospitals G303H in Glasgow (Mearnskirk House) and W106H in Western Isles (St Brendans Cot Hosp).
# Admissions to the Golden Jubilee National Hospital (D102H) are also not included.
# Exclusions from the RAPID dataset are day cases, neonatal, maternity and psychiatric care admissions. 


# Approximate run time: 5 minutes
###

###############################################.
## Section 1: Setup and functions ----
###############################################.

# For functions and file paths
source("data_prep/functions_packages_data_prep.R")

library(odbc)
#Make connection to the RAPID Database through the Denodo Virtualisation Platform.
RAPID_connection <- dbConnect(odbc(),
                              dsn="DVPROD",
                              uid=.rs.askForPassword("Username:"),
                              pwd=.rs.askForPassword("Password:"))


rapid_extract <- as_tibble(dbGetQuery(RAPID_connection, statement=paste0(
                        "SELECT hospital_of_treatment_nhs_board_code_current, admission_date, 
                      emergency_admission_flag, specialty, patient_gender_code, 
                      age_on_admission, postcode, hscp_of_residence_code_current, 
                      hscp_of_residence_name_current, 
                      ethnic_group_code, ethnic_group_description
                      FROM rapid.syswatch_hosp_stay      #name of RAPID Stay Table 
                      WHERE ADMISSION_DATE >= '2018-01-01' 
                      AND inpatient_daycase_identifier_code ='IP'
                      AND hospital_of_treatment_nhs_board_code_current IN ('A', 'B', 'F', 'G', 'H', 'L', 'N','R', 'S', 'T', 'V', 'W', 'Y', 'Z')
                      AND hospital_of_treatment_location_code NOT IN ('W106H', 'G303H', 'D102H')
                      CONTEXT ('i18n'='gb', 'cache_wait_for_load'='true');"))) 
                      # i18: Internationalization configuration for the results of the query.
                      # cache: the query does not finish until the data is completely stored in cache.


# Create a function to exclude specialties that are not reported in Systemwatch
convert_spec_to_spec_grouping <- function(spec, return_spec_group_lists = FALSE) {
  
  spec_group_1_surgical <- c('C1', 'C11', 'C12', 'C13', 'C14', 
                             'C4', 'C41', 'C42', 'C5', 'C51', 'C6', 'C7', 'C8', 
                             'C9', 'C91', 'CB', 'CC', 'D3', 'D4', 'D5', 'D6', 'D8', 'F2')
  
  spec_group_2_medical <- c('A1', 'A11', 'A2', 'A3', 'A6', 'A7', 'A8', 'A81', 'A82', 'A9',
                            'AA', 'AB', 'AC', 'AD', 'AG', 'AH', 'AJ', 'AM', 'AP', 'AQ', 'AR', 'AV', 
                            'AW', 'C2', 'C3', 'C31', 'D1', 'E12', 'H1', 'H2', 'J3', 'J4', 
                            'J5', 'R1', 'R11')
  
  spec_group_3_paediatric <- c('A21', 'AF', 'CA')
  
# This is if return_spec_group_lists == TRUE, which essentially makes it another function that now just 
  if(return_spec_group_lists == TRUE) {  # returns all of the included specialties instead of returning the specialty groupings.
    return(list(spec_group_1_surgical   = spec_group_1_surgical, 
                spec_group_2_medical    = spec_group_2_medical, 
                spec_group_3_paediatric = spec_group_3_paediatric) )
  } # end if statement
  
# Create a vector for the specialty groupings and set them to what they should be.
  spec_grouping <- numeric(length(spec))
  spec_grouping[ spec %in% spec_group_1_surgical ]   <- 1L
  spec_grouping[ spec %in% spec_group_2_medical ]    <- 2L
  spec_grouping[ spec %in% spec_group_3_paediatric ] <- 3L
  
  return(spec_grouping)
  
} # end function convert_spec_to_spec_grouping().



###############################################.
## Data manipulation ----
###############################################.

# Rename and format variables
rapid_extract <- rapid_extract %>% 
  rename(hb = hospital_of_treatment_nhs_board_code_current,
         date_adm = admission_date,
         spec = specialty,
         sex = patient_gender_code,
         age = age_on_admission,
         hscp_code = hscp_of_residence_code_current,
         hscp_name = hscp_of_residence_name_current) %>% 
  mutate(age = as.numeric(age),
         date_adm = as.Date(date_adm))

# Exclude the specialties by creating medsur variable
rapid_extract$medsur <- convert_spec_to_spec_grouping(spec = rapid_extract$spec)

rapid_extract <- rapid_extract %>% filter(medsur != 0)

# Add SIMD from lookup by postcode.
simd_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2021_2_simd2020v2.rds') %>% 
  select(pc7, simd2020v2_sc_quintile) %>% 
  rename(postcode = pc7,
         dep = simd2020v2_sc_quintile)

# Add SIMD to rapid dataset
rapid <- left_join(rapid_extract, simd_lookup, "postcode")

# Recode variables into agegroups, sexes, deprivation groups and admission types
rapid %<>%
  mutate(admission_type = case_when(emergency_admission_flag == 'Y' ~ 'Emergency',
                                    TRUE ~ 'Planned')) %>% 
  create_agegroups() %>% 
  create_sexgroups () %>% 
  create_depgroups() %>% 
  select(-age, -age_grp1, -postcode, -emergency_admission_flag, -medsur) %>% 
  rename(age = age_grp)


###############################################.
## Create totals for Scotland, each board, and each location ----
###############################################.

# # Scotland totals - by admission_type, spec, dep, age, sex, and date_adm.
# scot_totals <- rapid %>% 
#     group_by(admission_type, spec, dep, age, sex, date_adm) %>% 
#     summarise(count = n()) %>% 
#     ungroup() %>% 
#     mutate(hb = 'X', hscp_code = '', hscp_name = 'All_Scotland') %>% 
#     select(hb, hscp_code, hscp_name, admission_type, spec, #put the columns in the proper order.
#            dep, age, sex, date_adm, count)

# Hscp totals - by admission_type, spec, dep, age, sex, and date_adm.
all_locations <- rapid %>% 
    group_by(hb, hscp_code, hscp_name, admission_type, spec, dep, age, sex, date_adm) %>% 
    summarise(count = n()) %>% 
    ungroup() %>%  
    select(hb, hscp_code, hscp_name, admission_type,  #put the columns in the proper order.
           spec, dep, age, sex, date_adm, count)

#Merge all three data frames into one and filter data.
#combined_records <- rbind(all_locations, scot_totals)

# Save file with today's date
date_on_filename <<- format(Sys.Date(), format = '%Y-%m-%d')
saveRDS(all_locations, paste0(data_folder, 'rapid/', date_on_filename, '-admissions-by-category.rds')) 


##END
