##########################################################
# Create PHS COVID Wider Impact Open Data
# Original author: Csilla Scharle
# Original date: 13/05/20
# Latest update author
# Latest update date
# Latest update description
# Type of script
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Description: create open data csv for Weekly COVID-19 Wider Impacts update
# Approximate run time 
##########################################################


#############################################################
### Housekeeping ----
#############################################################

# empty workspace

rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(stringi)
library(readxl)
library(glue)
library(janitor)
library(reshape2)
library(readr)
library(ckanr)
library(here)


# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

#Set Open Data folder filepaths & Dashboard data filenames

adm_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                          "Open Data (Non Health Topic)", "Data", 
                          "OD2000024 - COVID-19 Wider Impact - Hospital Admissions")
ae_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                         "Open Data (Non Health Topic)", "Data", 
                         "OD2000025 - COVID-19 Wider Impact - A&E")
nhs24_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                            "Open Data (Non Health Topic)", "Data", 
                            "OD2000026 - COVID-19 Wider Impact - NHS24")
ooh_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                          "Open Data (Non Health Topic)", "Data", 
                          "OD2000027 - COVID-19 Wider Impact - OOH Consultations")
sas_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                          "Open Data (Non Health Topic)", "Data", 
                          "OD2000028 - COVID-19 Wider Impact - Scottish Ambulance Services")

#set filenames for dashboard data tables --- UPDATE TO MATCH WHAT FILE NAMES WILL BE
hospital_admissions <- "hospital_admissions"
a_and_e <- "a_and_e"
nhs24 <- "nhs24"
ooh <- "ooh"
sas <- "sas"

hospital_admissions <- read_csv(glue("{adm_filepath}/{hospital_admissions}.csv"))
a_and_e <- read_csv(glue("{ae_filepath}/{a_and_e}.csv"))
nhs24 <- read_csv(glue("{nhs24_filepath}/{nhs24}.csv"))
ooh <- read_csv(glue("{ooh_filepath}/{ooh}.csv"))
sas <- read_csv(glue("{sas_filepath}/{sas}.csv"))

# Set resources to use

resource1 <- "hospital_admissions_hb_agesex"
resource2 <- "hospital_admissions_hb_simd"
resource3 <- "hospital_admissions_hb_specialty"
resource4 <- "hospital_admissions_hscp_agesex"
resource5 <- "hospital_admissions_hscp_simd"
resource6 <- "hospital_admissions_hscp_specialty"
resource7 <- "a_and_e_hb_agesex"
resource8 <- "a_and_e_hb_simd"
resource9 <- "a_and_e_hscp_agesex"
resource10 <- "a_and_e_hscp_simd"
resource11 <- "nhs24_hb_agesex"
resource12 <- "nhs24_hb_simd"
resource13 <- "nhs24_hscp_agesex"
resource14 <- "nhs24_hscp_simd"
resource15 <- "ooh_hb_agesex"
resource16 <- "ooh_hb_simd"
resource17 <- "ooh_hscp_agesex"
resource18 <- "ooh_hscp_simd"
resource19 <- "sas_hb_agesex"
resource20 <- "sas_hb_simd"
resource21 <- "sas_hscp_agesex"
resource22 <- "sas_hscp_simd"

#Geo codes
#may need to run proxy config

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id_hb <- "f177be64-e94c-4ddf-a2ee-ea58d648d55a"
res_id_hscp <- "ccfeea67-2407-413c-9676-01c03527046a"

hb_codes <- dplyr::tbl(src = ckan$con, from = res_id_hb) %>% 
  as_tibble() %>%
  rename("Code" = "HB") %>%
  rename("Area_name" = "HBName")%>%
  select(Code, Area_name)

hscp_codes <- dplyr::tbl(src = ckan$con, from = res_id_hscp) %>% 
  as_tibble() %>%
  rename("Code" = "HSCP")%>%
  rename("Area_name" = "HSCPName")%>%
  select(Code, Area_name)

geo_codes <- bind_rows(hb_codes, hscp_codes) %>%
  add_row(Code = "S92000003", Area_name = "Scotland") %>%
  mutate(Area_name = recode(Area_name, 
                            "NHS Dumfries and Galloway" = "NHS Dumfries & Galloway",    
                            "NHS Ayrshire and Arran" = "NHS Ayrshire & Arran",       
                            "NHS Greater Glasgow and Clyde" = "NHS Greater Glasgow & Clyde",
                            "Argyll and Bute" = "Argyll & Bute",              
                            "Clackmannanshire and Stirling" = "Clackmannanshire & Stirling",
                            "Dumfries and Galloway"  = "Dumfries & Galloway",        
                            "Perth and Kinross" = "Perth & Kinross"))

#add geo codes to source data files
hospital_admissions <- left_join(hospital_admissions, geo_codes, by = "Area_name")
a_and_e <- left_join(a_and_e, geo_codes, by = "Area_name")
nhs24 <- left_join(nhs24, geo_codes, by = "Area_name")
ooh <- left_join(ooh, geo_codes, by = "Area_name")
sas <- left_join(sas, geo_codes, by = "Area_name")

##############################################
###FUNCTIONS
###############################################

#format names for ckan
od_names <- function(dataset) {
  dataset_hb <- dataset %>%
    rename("Average20182019" = "Count_average_pre2020") %>%
    rename("PercentVariation" = "Variation (%)") %>%
    rename("WeekEnding" = "Week_ending")
}


###filter HB
hb_filter <- function(dataset) {
  dataset_hb <- dataset %>%
    filter(Area_type != "HSC partnership") %>%
    rename("HB" = "Code")
}


###filter HSCP
hscp_filter <- function(dataset) {
  dataset_hscp <- dataset %>%
    filter(Area_type == "HSC partnership") %>%
    rename("HSCP" = "Code")
}

###Age+Sex
age_sex_od <- function(dataset) {
  #filter for sex, rename, add AgeGroup
  dataset_sex <- dataset %>%
    filter(Category %in% c("Male", "Female", "All")) %>%
    mutate(AgeGroup = "All ages") %>%
    rename("Sex" = "Category")
  
  #filter for age, rename, add sex
  dataset_age <- dataset %>%
    filter(Category %in% c("Aged 5 to 14", "Aged 15 to 44", 
                           "Aged 45 to 64", "Aged 65 to 74",
                           "Aged 75 to 84", "Aged 85 and over",
                           "Aged under 5"))%>%
    mutate(Sex = "All") %>%
    rename("AgeGroup" = "Category")
  
  #join age+sex
  dataset_agesex <- full_join(dataset_age, dataset_sex)
  
  #add qualifiers,  select+reorder
  dataset_agesex <- dataset_agesex %>%
    mutate(AgeGroupQF = if_else(AgeGroup == "All ages", "d", ""), 
           SexQF = if_else(Sex == "All", "d", ""))
}


###SIMD
simd_od <- function(dataset) {
  #filter for simd, rename, select, reorder
  dataset_simd <- dataset %>%
    filter(Category %in% c("Quintile 1 - most deprived", "Quintile 2", 
                           "Quintile 3", "Quintile 4", "Quintile 5 - least deprived")) %>%
    rename("SIMDQuintile" = "Category") %>%
    mutate(SIMDQuintile = recode("Quintile 1 - most deprived" = "1",
                                 "Quintile 2" = "2",
                                 "Quintile 3" = "3",
                                 "Quintile 4" = "4",
                                 "Quintile 5 - least deprived" = "5"))
}


##############################################
#1##Create Weekly_hospital_admissions OD Resources (6 files)
##############################################

#format date
hospital_admissions <- hospital_admissions %>%
  mutate(Week_ending = as.Date(Week_ending, format = "%d %b %y")) %>%
  mutate(Week_ending = strftime(Week_ending, format = "%Y%m%d"))


#rename common colnames
hospital_admissions <- od_names(hospital_admissions) %>%
  rename("AdmissionType" = "Admission_type") %>%
  rename("NumberAdmissions" = "Count")

###1A-1##By HB and Age + Sex

#filter for HB (+Scotland)
adm_hb <- hb_filter(hospital_admissions)

#rename, add qualifiers
adm_hb <- adm_hb %>%
  mutate(AdmissionTypeQF = if_else(AdmissionType == "All", "d", ""),
         HBQF = if_else(HB == "S92000003", "d", ""))

#age-sex
adm_hb_age_sex <- adm_hb %>%
  filter(Specialty == "All")%>%
  age_sex_od()

#select+reorder
adm_hb_age_sex <- adm_hb_age_sex %>%
  select(WeekEnding, HB, HBQF, AgeGroup, AgeGroupQF, Sex, SexQF, AdmissionType,
         AdmissionTypeQF, NumberAdmissions, Average20182019, PercentVariation) %>%
  arrange(AdmissionType, WeekEnding, HB, Sex, AgeGroup)

#create OD csv file
write_csv(adm_hb_age_sex, glue("{adm_filepath}/{resource1}_{date}.csv"))

###1A-2##By HB and SIMD

#filter for simd
adm_hb_simd <- adm_hb %>%
  filter(Specialty == "All")%>%
  simd_od()

#select, reorder
adm_hb_simd <- adm_hb_simd %>%
  select(WeekEnding, HB, HBQF, SIMDQuintile, AdmissionType,
         AdmissionTypeQF, NumberAdmissions, Average20182019, PercentVariation) %>%
  arrange(AdmissionType, WeekEnding, HB, SIMDQuintile)

#create OD csv file
write_csv(adm_hb_simd, glue("{adm_filepath}/{resource2}_{date}.csv"))

###1A-3##By HB and Specialty

#filter for specialty, add QF, rename, select, reorder
adm_hb_spec <- adm_hb %>%
  filter(Category == "All") %>%
  mutate(SpecialtyQF = if_else(Specialty == "All", "d", "")) %>%
  select(WeekEnding, HB, HBQF, AdmissionType, AdmissionTypeQF, Specialty,
         SpecialtyQF, NumberAdmissions, Average20182019, PercentVariation) %>%
  arrange(AdmissionType, WeekEnding, HB)

#create OD csv file

write_csv(adm_hb_spec, glue("{adm_filepath}/{resource3}_{date}.csv"))

###1B-1##By HSCP +Age +sex

#filter for HB (+Scotland)
adm_hscp <- hscp_filter(hospital_admissions)

#rename, add qualifiers
adm_hscp <- adm_hscp %>%
  mutate(AdmissionTypeQF = if_else(AdmissionType == "All", "d", ""))

#age-sex
adm_hscp_age_sex <- adm_hscp %>%
  filter(Specialty == "All")%>%
  age_sex_od()

#select+reorder
adm_hscp_age_sex <- adm_hscp_age_sex %>%
  select(WeekEnding, HSCP, AgeGroup, AgeGroupQF, Sex, SexQF, AdmissionType,
         AdmissionTypeQF, NumberAdmissions, Average20182019, PercentVariation) %>%
  arrange(AdmissionType, WeekEnding, HSCP, Sex, AgeGroup)

#create OD csv file
write_csv(adm_hscp_age_sex, glue("{adm_filepath}/{resource4}_{date}.csv"))

###1B-2##By HSCP and SIMD

#filter for simd
adm_hscp_simd <- adm_hscp %>%
  filter(Specialty == "All")%>%
  simd_od()

#select, reorder
adm_hscp_simd <- adm_hscp_simd %>%
  select(WeekEnding, HSCP, SIMDQuintile, AdmissionType, AdmissionTypeQF, 
         NumberAdmissions, Average20182019, PercentVariation) %>%
  arrange(AdmissionType, WeekEnding, HSCP, SIMDQuintile)

#create OD csv file
write_csv(adm_hscp_simd, glue("{adm_filepath}/{resource5}_{date}.csv"))

###1B-3##By HSCP and Specialty

#filter for specialty, add QF, rename, select, reorder
adm_hscp_spec <- adm_hscp %>%
  filter(Category == "All") %>%
  mutate(SpecialtyQF = if_else(Specialty == "All", "d", "")) %>%
  select(WeekEnding, HSCP, AdmissionType, AdmissionTypeQF, Specialty, 
         SpecialtyQF, NumberAdmissions, Average20182019, PercentVariation) %>%
  arrange(AdmissionType, WeekEnding, HSCP)

#create OD csv file

write_csv(adm_hscp_spec, glue("{adm_filepath}/{resource6}_{date}.csv"))

##############################################
#2##Create Weekly_a_and_e OD Resources
##############################################

#format date
a_and_e <- a_and_e %>%
  mutate(Week_ending = as.Date(Week_ending, format = "%d %b %y"))%>%
  mutate(Week_ending = strftime(Week_ending, format = "%Y%m%d"))

#rename common colnames
a_and_e <- od_names(a_and_e) %>%
  rename("NumberAttendances" = "Count") 

###2A-1##By HB and Age + Sex

#filter for HB (+Scotland)
ae_hb <- hb_filter(a_and_e)

#rename, add qualifiers
ae_hb <- ae_hb %>%
  mutate(HBQF = if_else(HB == "S92000003", "d", ""))

#age-sex
ae_hb_age_sex <- age_sex_od(ae_hb)

#select+reorder
ae_hb_age_sex <- ae_hb_age_sex %>%
  select(WeekEnding, HB, HBQF, AgeGroup, AgeGroupQF, Sex, SexQF,
         NumberAttendances, Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HB, Sex, AgeGroup)

#create OD csv file
write_csv(ae_hb_age_sex, glue("{ae_filepath}/{resource7}_{date}.csv"))

###2A-2##By HB and SIMD

#filter for simd
ae_hb_simd <- simd_od(ae_hb)

#select, reorder
ae_hb_simd <- ae_hb_simd %>%
  select(WeekEnding, HB, HBQF, SIMDQuintile,NumberAttendances, 
         Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HB, SIMDQuintile)

#create OD csv file
write_csv(ae_hb_simd, glue("{ae_filepath}/{resource8}_{date}.csv"))

###2B-1##By HSCP +Age +sex

#filter for HB (+Scotland)
ae_hscp <- hscp_filter(a_and_e)

#age-sex
ae_hscp_age_sex <- age_sex_od(ae_hscp)

#select+reorder
ae_hscp_age_sex <- ae_hscp_age_sex %>%
  select(WeekEnding, HSCP, AgeGroup, AgeGroupQF, Sex, SexQF, 
         NumberAttendances, Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HSCP, Sex, AgeGroup)

#create OD csv file
write_csv(ae_hscp_age_sex, glue("{ae_filepath}/{resource9}_{date}.csv"))

###2B-2##By HSCP and SIMD

#filter for simd
ae_hscp_simd <- simd_od(ae_hscp)

#select, reorder
ae_hscp_simd <- ae_hscp_simd %>%
  select(WeekEnding, HSCP, SIMDQuintile, NumberAttendances, 
         Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HSCP, SIMDQuintile)

#create OD csv file
write_csv(ae_hscp_simd, glue("{ae_filepath}/{resource10}_{date}.csv"))


##############################################
#3##Create Weekly_nhs24_calls OD Resources
##############################################

#format date
nhs24 <- nhs24 %>%
  mutate(Week_ending = as.Date(Week_ending, format = "%d %b %y"))%>%
  mutate(Week_ending = strftime(Week_ending, format = "%Y%m%d"))

#rename common colnames
nhs24 <- od_names(nhs24) %>%
  rename("CompletedContacts" = "Count")

###3A-1##By HB and Age + Sex

#filter for HB (+Scotland)
nhs24_hb <- hb_filter(nhs24)

#rename, add qualifiers
nhs24_hb <- nhs24_hb %>%
  mutate(HBQF = if_else(HB == "S92000003", "d", ""))

#age-sex
nhs24_hb_age_sex <- age_sex_od(nhs24_hb)

#select+reorder
nhs24_hb_age_sex <- nhs24_hb_age_sex %>%
  select(WeekEnding, HB, HBQF, AgeGroup, AgeGroupQF, Sex, SexQF,
         CompletedContacts, Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HB, Sex, AgeGroup)

#create OD csv file
write_csv(nhs24_hb_age_sex, glue("{nhs24_filepath}/{resource11}_{date}.csv"))

###3A-2##By HB and SIMD

#filter for simd
nhs24_hb_simd <- simd_od(nhs24_hb)

#select, reorder
nhs24_hb_simd <- nhs24_hb_simd %>%
  select(WeekEnding, HB, HBQF, SIMDQuintile, CompletedContacts, 
         Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HB, SIMDQuintile)

#create OD csv file
write_csv(nhs24_hb_simd, glue("{nhs24_filepath}/{resource12}_{date}.csv"))

###3B-1##By HSCP +Age +sex

#filter for HB (+Scotland)
nhs24_hscp <- hscp_filter(nhs24)

#age-sex
nhs24_hscp_age_sex <- age_sex_od(nhs24_hscp)

#select+reorder
nhs24_hscp_age_sex <- nhs24_hscp_age_sex %>%
  select(WeekEnding, HSCP, AgeGroup, AgeGroupQF, Sex, SexQF, 
         CompletedContacts, Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HSCP, Sex, AgeGroup)

#create OD csv file
write_csv(nhs24_hscp_age_sex, glue("{nhs24_filepath}/{resource13}_{date}.csv"))

###3B-2##By HSCP and SIMD

#filter for simd
nhs24_hscp_simd <- simd_od(nhs24_hscp)

#select, reorder
nhs24_hscp_simd <- nhs24_hscp_simd %>%
  select(WeekEnding, HSCP, SIMDQuintile, CompletedContacts, 
         Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HSCP, SIMDQuintile)

#create OD csv file
write_csv(nhs24_hscp_simd, glue("{nhs24_filepath}/{resource14}_{date}.csv"))



##############################################
#4##Create Weekly_ooh_consults OD Resources
##############################################

#format date
ooh <- ooh %>%
  mutate(Week_ending = as.Date(Week_ending, format = "%d %b %y"))%>%
  mutate(Week_ending = strftime(Week_ending, format = "%Y%m%d"))

#rename common colnames
ooh <- od_names(ooh) %>%
  rename("Consultations" = "Count")

###4A-1##By HB and Age + Sex

#filter for HB (+Scotland)
ooh_hb <- hb_filter(ooh)

#rename, add qualifiers
ooh_hb <- ooh_hb %>%
  mutate(HBQF = if_else(HB == "S92000003", "d", ""))

#age-sex
ooh_hb_age_sex <- age_sex_od(ooh_hb)

#select+reorder
ooh_hb_age_sex <- ooh_hb_age_sex %>%
  select(WeekEnding, HB, HBQF, AgeGroup, AgeGroupQF, Sex, SexQF,
         Consultations, Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HB, Sex, AgeGroup)

#create OD csv file
write_csv(ooh_hb_age_sex, glue("{ooh_filepath}/{resource15}_{date}.csv"))

###4A-2##By HB and SIMD

#filter for simd
ooh_hb_simd <- simd_od(ooh_hb)

#select, reorder
ooh_hb_simd <- ooh_hb_simd %>%
  select(WeekEnding, HB, HBQF, SIMDQuintile, Consultations, 
         Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HB, SIMDQuintile)

#create OD csv file
write_csv(ooh_hb_simd, glue("{ooh_filepath}/{resource16}_{date}.csv"))

###4B-1##By HSCP +Age +sex

#filter for HB (+Scotland)
ooh_hscp <- hscp_filter(ooh)

#age-sex
ooh_hscp_age_sex <- age_sex_od(ooh_hscp)

#select+reorder
ooh_hscp_age_sex <- ooh_hscp_age_sex %>%
  select(WeekEnding, HSCP, AgeGroup, AgeGroupQF, Sex, SexQF, 
         Consultations, Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HSCP, Sex, AgeGroup)

#create OD csv file
write_csv(ooh_hscp_age_sex, glue("{ooh_filepath}/{resource17}_{date}.csv"))

###4B-2##By HSCP and SIMD

#filter for simd
ooh_hscp_simd <- simd_od(ooh_hscp)

#select, reorder
ooh_hscp_simd <- ooh_hscp_simd %>%
  select(WeekEnding, HSCP, SIMDQuintile, Consultations, 
         Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HSCP, SIMDQuintile)

#create OD csv file
write_csv(ooh_hscp_simd, glue("{ooh_filepath}/{resource18}_{date}.csv"))


##############################################
#5##Create Weekly_sas OD Resources
##############################################

#format date
sas <- sas %>%
  mutate(Week_ending = as.Date(Week_ending, format = "%d %b %y"))%>%
  mutate(Week_ending = strftime(Week_ending, format = "%Y%m%d"))

#rename common colnames
sas <- od_names(sas) %>%
  rename("SASIncidents" = "Count")

###5A-1##By HB and Age + Sex

#filter for HB (+Scotland)
sas_hb <- hb_filter(sas)

#rename, add qualifiers
sas_hb <- sas_hb %>%
  mutate(HBQF = if_else(HB == "S92000003", "d", ""))

#age-sex
sas_hb_age_sex <- age_sex_od(sas_hb)

#select+reorder
sas_hb_age_sex <- sas_hb_age_sex %>%
  select(WeekEnding, HB, HBQF, AgeGroup, AgeGroupQF, Sex, SexQF,
         SASIncidents, Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HB, Sex, AgeGroup)

#create OD csv file
write_csv(sas_hb_age_sex, glue("{sas_filepath}/{resource19}_{date}.csv"))

###5A-2##By HB and SIMD

#filter for simd
sas_hb_simd <- simd_od(sas_hb)

#select, reorder
sas_hb_simd <- sas_hb_simd %>%
  select(WeekEnding, HB, HBQF, SIMDQuintile, SASIncidents, 
         Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HB, SIMDQuintile)

#create OD csv file
write_csv(sas_hb_simd, glue("{sas_filepath}/{resource20}_{date}.csv"))

###5B-1##By HSCP +Age +sex

#filter for HB (+Scotland)
sas_hscp <- hscp_filter(sas)

#age-sex
sas_hscp_age_sex <- age_sex_od(sas_hscp)

#select+reorder
sas_hscp_age_sex <- sas_hscp_age_sex %>%
  select(WeekEnding, HSCP, AgeGroup, AgeGroupQF, Sex, SexQF, 
         SASIncidents, Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HSCP, Sex, AgeGroup)

#create OD csv file
write_csv(sas_hscp_age_sex, glue("{sas_filepath}/{resource21}_{date}.csv"))

###5B-2##By HSCP and SIMD

#filter for simd
sas_hscp_simd <- simd_od(sas_hscp)

#select, reorder
sas_hscp_simd <- sas_hscp_simd %>%
  select(WeekEnding, HSCP, SIMDQuintile, SASIncidents, 
         Average20182019, PercentVariation) %>%
  arrange(WeekEnding, HSCP, SIMDQuintile)

#create OD csv file
write_csv(sas_hscp_simd, glue("{sas_filepath}/{resource22}_{date}.csv"))
