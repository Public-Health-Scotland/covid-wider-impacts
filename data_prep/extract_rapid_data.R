###
# Written/run on: R Studio Server Pro 
# Base R Version: 3.6.1
# Description: This file creates an output of RAPID records to be saved in /conf/PHSCOVID19_Analysis/rapid/ folder
# then it goes through further transformations in summary_data_prep.R.
# 
# Notes:
# Remove records belonging to hospitals G303H (Mearnskirk House, GGC) and W106H (St Brendans Cot Hosp, WI).
# Admissions to the Golden Jubilee National Hospital (D102H) are also removed.
# Day cases, neonatal, maternity and psychiatric care admissions are excluded from the RAPID dataset. 

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


###############################################.
## Data manipulation ----
###############################################.

# Rename and format variables
rapid_extract %<>% 
  rename(hb = hospital_of_treatment_nhs_board_code_current,
         date_adm = admission_date,
         spec = specialty,
         sex = patient_gender_code,
         age = age_on_admission,
         hscp_code = hscp_of_residence_code_current,
         hscp_name = hscp_of_residence_name_current) %>% 
  mutate(age = as.numeric(age),
         date_adm = as.Date(date_adm))


# Add SIMD from lookup by postcode.
simd_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2021_2_simd2020v2.rds') %>% 
  select(pc7, simd2020v2_sc_quintile) %>% 
  rename(postcode = pc7,
         dep = simd2020v2_sc_quintile)

# Add SIMD to rapid dataset
rapid <- left_join(rapid_extract, simd_lookup, "postcode")


# Assign specialties to medical, surgical and paediatric groups and 
# create medsur variable to exclude specialties that are not reported in Systemwatch.

# Specialty group: surgical 
spec_gp1 <- c('C1', 'C11', 'C12', 'C13', 'C14', 
              'C4', 'C41', 'C42', 'C5', 'C51', 'C6', 'C7', 'C8', 
              'C9', 'C91', 'CB', 'CC', 'D3', 'D4', 'D5', 'D6', 'D8', 'F2')
# Specialty group: medical
spec_gp2 <- c('A1', 'A11', 'A2', 'A3', 'A6', 'A7', 'A8', 'A81', 'A82', 'A9',
              'AA', 'AB', 'AC', 'AD', 'AG', 'AH', 'AJ', 'AM', 'AP', 'AQ', 'AR', 'AV', 
              'AW', 'C2', 'C3', 'C31', 'D1', 'E12', 'H1', 'H2', 'J3', 'J4', 
              'J5', 'R1', 'R11')
# Specialty group paediatric
spec_gp3 <- c('A21', 'AF', 'CA')

# Filter the specialties that are not included
rapid %<>% 
  mutate(medsur = case_when(spec %in% spec_gp1 ~ 1,
                            spec %in% spec_gp2 ~ 2,
                            spec %in% spec_gp3 ~ 3,
                            TRUE ~ 0)) %>% 
  filter(medsur != 0)


# Recode variables into admission types, agegroups, sexes and deprivation groups 
rapid %<>%
  mutate(admission_type = case_when(emergency_admission_flag == 'Y' ~ 'Emergency',
                                    TRUE ~ 'Planned')) %>% 
  create_agegroups() %>% 
  create_sexgroups() %>% 
  create_depgroups() %>% 
  select(-age, -age_grp1, -postcode, -emergency_admission_flag, -medsur) %>% 
  rename(age = age_grp)


###############################################.
## Create totals for each category and location ----
###############################################.

# Totals - by admission_type, spec, dep, age, sex, and date_adm.
rapid_output <- rapid %>% 
    group_by(hb, hscp_code, hscp_name, admission_type, spec, dep, age, sex, date_adm) %>% 
    summarise(count = n()) %>% 
    ungroup() %>%  
    select(hb, hscp_code, hscp_name, admission_type, spec, dep, age, sex, date_adm, count)


# Save file with today's date
date_on_filename <<- format(Sys.Date(), format = '%Y-%m-%d')
saveRDS(rapid_output, paste0(data_folder, 'rapid/', date_on_filename, '-admissions-by-category.rds')) 


##END