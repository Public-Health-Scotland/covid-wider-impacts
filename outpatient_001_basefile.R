
## Data Release - COVID Wider Impacts Dashboard            ##
## Original Authors - David Caldwell & Ciara Gribben       ##
## Original Date - July 2017                               ##
## Latest Update Author - Roisin Farrell                   ##
## Latest Update Date - 26/10/2020                         ##
##                                                         ##
## Type - Extraction/preparation                           ##
## Written/run on - R Studio SERVER                        ##
## R version - 3.6.1                                       ##
## Description - Extracts SMR00 data, tidies and creates   ##
## the outpatient basefile used for this publication       ##
##                                                         ##
## Approximate run time: 4 minutes                         ##

### SECTION 1 - HOUSE KEEPING ----
start <- Sys.time()

### 1 - Load packages ----
library(odbc)       # for accessing SMRA
library(dplyr)      # for data wrangling
library(haven)      # for reading spss files
library(magrittr)   # for %<>% operator
library(stringr)    # for manipulation of strings
library(tidyr)      # for manipulating data in the "tidy way"
library(readxl)
library(readr)
library(lubridate)  # for dates
library(fst)
library(janitor)    # round up function

# Define timezone (to prevent lubridate from applying daylight savings)
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

uc_ref            <- "/conf/linkage/output/lookups/Unicode/"
spec_grp_lookup2  <- read_excel("../specialty_groups.xlsx")
hosp_lookup       <- read_sav(paste0(uc_ref, 
                                     "National Reference Files/location.sav"))
simd_lookup       <- readRDS(paste0(uc_ref, "Deprivation/",
                            "postcode_2020_2_all_simd_carstairs.rds"))
pc_lookup         <- readRDS(paste0(uc_ref, "Geography/Scottish Postcode Directory/",
                            "Scottish_Postcode_Directory_2020_2.rds")) %>%
  select(pc7, hscp2019, hscp2019name, hb2019, hb2019name)
hscp_lookup = read.csv(paste0("https://www.opendata.nhs.scot/dataset/",
                              "9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/",
                              "944765d7-d0d9-46a0-b377-abb3de51d08e/download/",
                              "hscp16_hscp19.csv"),
                       stringsAsFactors = FALSE) %>%
  filter(is.na(HBDateArchived))
hb2019_lookup = read.csv(paste0("https://www.opendata.nhs.scot/dataset/",
                                "9f942fdb-e59e-44f5-b534-d6e17229cc7b/",
                                "resource/652ff726-e676-4a20-abda-435b98dd7bdc/",
                                "download/hb14_hb19.csv"),
                         stringsAsFactors = FALSE)
sb_lookup = read.csv(paste0("https://www.opendata.nhs.scot/dataset/",
                            "65402d20-f0f1-4cee-a4f9-a960ca560444/resource/",
                            "0450a5a2-f600-4569-a9ae-5d6317141899/download/",
                            "special-health-boards_06042020.csv"),
                     stringsAsFactors = FALSE)
hbres_lookup = read.csv(paste0("https://www.opendata.nhs.scot/dataset/",
                               "65402d20-f0f1-4cee-a4f9-a960ca560444/resource/",
                               "32164b83-c9ec-495a-ac9f-dbeeb6ed5e59/download/",
                               "other-residential-categories.csv"),
                        stringsAsFactors = FALSE)

setwd("/conf/linkage/output/Roisin F/Wider Impacts")

# start_date = as.Date("2017-01-01")
# end_date = as.Date("2020-09-30")
# 
# ### SECTION 2 - DATA EXTRACTION ----
# ### 1 - Connect to SMRA ----
# # Define the database connection with SMRA
# #
# SMRA_connect <- dbConnect(odbc(),
#                           dsn = "SMRA",
#                           uid = .rs.askForPassword("SMRA Username:"),
#                           pwd = .rs.askForPassword("SMRA Password:"))
# 
# 
# # ### 2 - Data extraction ----
# # Write SQL query
# query_smr00 <- paste0(
#   "select SPECIALTY, referral_type, CLINIC_DATE,
#     clinic_attendance, CLINIC_TYPE,
#     LOCATION, SEX, DR_POSTCODE,
#     hbtreat_currentdate, HBRES_CURRENTDATE,
#     AGE_IN_YEARS, MODE_OF_CONTACT, MAIN_CONDITION, MAIN_OPERATION
#    from ANALYSIS.SMR00_PI_OCT2020
#    where CLINIC_DATE >= to_date('", start_date, "', 'yyyy-MM-dd')
#      AND CLINIC_DATE <= to_date('", end_date, "', 'yyyy-MM-dd')
#      AND CLINIC_TYPE IN ('1', '2')
#   ")
# # clinic type 1 = consultant, 2 = dentist.
# # DR_POSTCODE is the 7 character postcode (i.e. pc7) field on SMRA
# 
# # Extract outpatients data from SMR00 using SQL query above into a dataframe
# outpats <- as_tibble(dbGetQuery(SMRA_connect, query_smr00))
# 
# ### Ensure variable names are all lowercase
# names(outpats)    <- tolower(names(outpats))
# 
# write_fst(outpats, "outpats.fst")

outpats_all = read_fst("outpats.fst")

### SECTION 3 - DATA PREPARATION ----

### 1 - Create week variables ----
# Assign dates to weeks e.g. 05/01/2020
# The dates we want are the Sunday in each week.
# To get this we use ceiling_date, which returns the Sunday of each week.
# change_on_boundary=T ensures that dates which are already the first day of a 
# week (e.g. 01/01/2019) will be correctly rolled forwards.
outpats = outpats_all %>%
  mutate(clinic_date = as_date(clinic_date))

outpats <- outpats %>% 
  mutate(year = year(clinic_date),
         week_date = ceiling_date(clinic_date, "week", 
                                  change_on_boundary = F),
         month = month(clinic_date))

### 2 - Create area names ----
# use open data file to ensure we have the most current geography codes/names

# match SMR01 postcode to postcode file to get HB and HSCP codes and names.
outpats = left_join(outpats, pc_lookup, 
                     by = c("dr_postcode" = "pc7"))

# 2a - health board of residence ----
outpats = mutate(outpats, hbres_currentdate = 
                   recode(hbres_currentdate, 
                          "S08200001"="RA2702", 
                          "S08200002"="RA2701",
                          "S08200003"="RA2704",
                          "S08200004"="RA2703"))

outpats = outpats %>%
  mutate(hb2019 = case_when(is.na(hb2019) ~ hbres_currentdate,
                            TRUE ~ hb2019))

## match lookup and outpats by hb code to create health board name- hb of treatment.
outpats = outpats %>%
  mutate(hbres_new = hb2019,
         hbres_new_name = hb2019name)

## match lookup and outpats by hb code to create health board name- hb of residence
outpats = left_join(outpats, hb2019_lookup, 
                     by = c("hb2019" = "HB")) 

outpats = left_join(outpats, hbres_lookup, 
                    by = c("hb2019" = "CustomResidency")) 

outpats = outpats %>%
  mutate(hbres_new_name = case_when(is.na(hbres_new_name) ~ CustomResidencyName,
                                    TRUE ~ hbres_new_name))

outpats = outpats %>%
  mutate(hbres_new_name = case_when(is.na(hbres_new_name) ~ HBName,
                                    TRUE ~ hbres_new_name))

# check hbres names matched on.
table(outpats$hbres_new_name, useNA = "ifany")

## match lookup and outpats by hb code to create health board name- hb of treatment.
# recode hb codes to reflect changes and match on to lookup.
# 2b - health board of treatment ----

outpats = outpats %>%
  select(-c(HBName:Country))

outpats = outpats %>%
  mutate(hbtreat_new =
           case_when(hbtreat_currentdate == "S08000018" ~ "S08000029",
                     hbtreat_currentdate == "S08000027" ~ "S08000030",
                     hbtreat_currentdate == "S08000021" ~ "S08000031",
                     hbtreat_currentdate == "S08000023" ~ "S08000032",
                     hbtreat_currentdate == "S08100001" ~ "SB0801",
                     hbtreat_currentdate == "S08100008" ~ "SB0802",
                     hbtreat_currentdate == "S27000001" ~ "SB0803",
                     hbtreat_currentdate == "S08200002" ~ "RA2701",
                     hbtreat_currentdate == "S08200001" ~ "RA2702",
                     hbtreat_currentdate == "S08200003" ~ "RA2704",
                     TRUE ~ hbtreat_currentdate))

## match lookup and outpats by hb code to create health board name- hb of residence
outpats = left_join(outpats, hb2019_lookup, 
                    by = c("hbtreat_new" = "HB")) 

outpats = outpats %>%
  select(-c(HBDateEnacted:Country))

sb_lookup = sb_lookup %>%
  mutate(SHBName = case_when(SHB == "SB0801" ~ "National Waiting Times Centre",
                             TRUE ~ SHBName))

outpats = left_join(outpats, sb_lookup,
                    by = c("hbtreat_new" = "SHB")) 

outpats = outpats %>%
  select(-c(CustomResidencyName, Country))

outpats = left_join(outpats, hbres_lookup,
                    by = c("hbtreat_new" = "CustomResidency")) 

outpats = outpats %>%
  mutate(hbtreat_new_name = HBName)

outpats = outpats %>%
  mutate(hbtreat_new_name = case_when(is.na(hbtreat_new_name) ~ SHBName,
                                      TRUE ~ hbtreat_new_name))

outpats = outpats %>%
  mutate(hbtreat_new_name = case_when(is.na(hbtreat_new_name) ~ 
                                        CustomResidencyName,
                                      TRUE ~ hbtreat_new_name))

outpats = outpats %>%
  mutate(hbtreat_new_name = case_when(hbtreat_new_name == "Unknown Residency" ~ 
                                        "Unknown",
                                      TRUE ~ hbtreat_new_name))

# check hb names matched on correctly.
table(outpats$hbtreat_new_name, useNA = "ifany")

rm(hb2019_lookup, hbres_lookup, sb_lookup)

### 2c - HSCP ----
outpats = outpats %>%
  select(-c(CustomResidencyName, HBName))

hscp_lookup = hscp_lookup %>%
  select(HSCP, HSCPName)

outpats = left_join(outpats, hscp_lookup, 
                    by = c("hscp2019" = "HSCP")) 

outpats = outpats %>%
  mutate(hscp_new_name = HSCPName)

outpats = outpats %>%
  mutate(hscp_new_name = case_when(is.na(hscp_new_name) ~ hbres_new_name,
                                      TRUE ~ hscp_new_name)) %>%
  select(-c(hscp2019:hb2019name, SHBName, HSCPName))

table(outpats$hscp_new_name, useNA = "ifany")

# Remove HSCP and postcode dataframes
rm(hscp_lookup, pc_lookup)

### 3 - Match on specialty name ----
outpats = outpats %>%
  mutate(specialty = toupper(specialty))

outpats = left_join(outpats, spec_grp_lookup2, 
                    by = c("specialty" = "Speccode")) 

rm(spec_grp_lookup2)

### 4 - Create new variable showing attendance status ----
outpats = outpats %>%
  mutate(attendance_status = case_when(clinic_attendance %in% c(1,5) ~ 
                                         'Attended',
                                       TRUE ~ 'DNA'))

table(outpats$attendance_status, useNA = "ifany")

### 4 - Create new variable showing whether appt was new or return ----
outpats = outpats %>%
  mutate(appt_type = case_when(referral_type %in% c(1,2) ~ "New", 
                               TRUE ~ "Return"))

table(outpats$appt_type, useNA = "ifany")

### 5 - Recode sex variable ----
outpats = outpats %>%
  mutate(sex_name = case_when(sex == 0 ~ "Unidentified",
                              sex == 1 ~ "Male",
                              sex == 2 ~ "Female",
                              sex == 9 ~ "Missing"))

table(outpats$sex_name, useNA = "ifany")

### 6 - Recode age variable ----
outpats <- outpats %>% 
  mutate(age_grp = case_when(age_in_years >= 0 & age_in_years <= 4 ~ "Under 5",
                             age_in_years >= 5 & age_in_years <= 14 ~ "5 - 14",
                             age_in_years >= 15 & age_in_years <= 44 ~ "15 - 44",
                             age_in_years >= 45 & age_in_years <= 64 ~ "45 - 64",
                             age_in_years >= 65 & age_in_years <= 74 ~ "65 - 74",
                             age_in_years >= 75 & age_in_years <= 84 ~ "75 - 84",
                             age_in_years >= 85 ~ "85 and over"))

table(outpats$age_grp, useNA = "ifany")

### 7 - Match on SIMD quintiles ----
simd_lookup = simd_lookup %>%
  select(pc7, dplyr::contains("_sc_quintile"))
# Note: dplyr needs to be stated here because purrr (which might be loaded) masks contains

# Join to the postcode lookup
outpats = left_join(outpats, simd_lookup, c("dr_postcode"="pc7"))

# create year variable to match approporiate SIMD quintile on.
outpats <- mutate(outpats, year = year(clinic_date))

# Assign the appropriate SIMD value to a patient depending on the year they
# were admitted
outpats <- outpats %>%
  mutate(simd = case_when(
    year >= 2017 ~ simd2020v2_sc_quintile,
    year >= 2014 & year <= 2016 ~ simd2016_sc_quintile
  )) 

# Remove the not needed year-specific SIMD variables
outpats <- select(outpats, -dplyr::contains("_sc_quintile"))

rm(simd_lookup)

### 8 - Recode mode of contact ----
outpats = outpats %>%
  mutate(mode_contact_new = case_when(mode_of_contact == 1 ~ "Face to Face",
                                      mode_of_contact == 2 ~ "Telephone",
                                      mode_of_contact == 3 ~ "Videolink",
                                      mode_of_contact == 4 ~ "Written"))

table(outpats$mode_contact_new, useNA = "ifany")

### 9 - Remove unneeded columns ----
outpats <- outpats %>% 
  select(-one_of(c("age_in_years", "referral_type", "clinic_attendance", 
                   "clinic_type", "dr_postcode")))

### SECTION 4 - SAVE OUTPUT ----

# Save outpatients basefile so it can be used in subsequent syntax
write_fst(outpats, 
          "/PHI_conf/SecondaryCare/Quarterly Publication/TPP/Development/Wider-Impacts/Outpatients_basefile.fst", 
          compress=100)

# time taken.
end <- Sys.time()
end - start

### END OF SCRIPT ###
