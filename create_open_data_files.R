
#############################################################.
### Housekeeping ----
#############################################################.

# empty workspace

rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(stringi)
library(glue)
library(janitor)
library(readr)
library(ckanr)

# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

#set filepath for httr configuration for api & source
config_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards",
                             "GPD", "5_GitHub", "GPD", "Geography", "Scottish Postcode Directory")

source(glue("{config_filepath}/Set httr configuration for API.R"))

#set file paths to read in dashboard data

generic_source_filepath <- file.path("//Isdsf00d03", "PHSCOVID19_Analysis", "Publication outputs", 
                                     "open_data")

adm_source_filepath <- file.path(generic_source_filepath,
                                 "Rapid_data")
ae_source_filepath <- file.path(generic_source_filepath,
                                "A&E_data")
nhs24_source_filepath <- file.path(generic_source_filepath,
                                   "NHS24_data")
ooh_source_filepath <- file.path(generic_source_filepath,
                                 "GP-OOH_data")
sas_source_filepath <- file.path(generic_source_filepath,
                                 "SAS_data")

#Set Open Data folder filepaths for output

generic_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                              "Open Data (Non Health Topic)", "Data")

adm_filepath <- file.path(generic_filepath,
                          "OD2000024 - COVID-19 Wider Impact - Hospital Admissions")
ae_filepath <- file.path(generic_filepath,
                         "OD2000025 - COVID-19 Wider Impact - A&E")
nhs24_filepath <- file.path(generic_filepath,
                            "OD2000026 - COVID-19 Wider Impact - NHS24")
ooh_filepath <- file.path(generic_filepath,
                          "OD2000027 - COVID-19 Wider Impact - OOH Consultations")
sas_filepath <- file.path(generic_filepath,
                          "OD2000028 - COVID-19 Wider Impact - Scottish Ambulance Services")


#set filenames for dashboard data tables --- UPDATE TO MATCH WHAT FILE NAMES WILL BE
hospital_admissions_data <- "rapid_finaldata"
a_and_e_data <- "AE_finaldata"
nhs24_data <- "NHS24_finaldata"
ooh_data <- "OOH_finaldata"
sas_data <- "SAS_finaldata"

hospital_admissions <- read_rds(glue("{adm_source_filepath}/{hospital_admissions_data}.rds"))
a_and_e <- read_rds(glue("{ae_source_filepath}/{a_and_e_data}.rds"))
nhs24 <- read_rds(glue("{nhs24_source_filepath}/{nhs24_data}.rds"))
ooh <- read_rds(glue("{ooh_source_filepath}/{ooh_data}.rds"))
sas <- read_rds(glue("{sas_source_filepath}/{sas_data}.rds"))

# Set resources to use

resource1 <- "hospital_admissions_hb_agesex"
resource2 <- "hospital_admissions_hb_simd"
resource3 <- "hospital_admissions_hb_specialty"
resource4 <- "hospital_admissions_hscp_agesex"
resource5 <- "hospital_admissions_hscp_simd"
resource6 <- "hospital_admissions_hscp_specialty"

#Geo codes

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id_hb <- "f177be64-e94c-4ddf-a2ee-ea58d648d55a"
res_id_hscp <- "ccfeea67-2407-413c-9676-01c03527046a"

hb_codes <- dplyr::tbl(src = ckan$con, from = res_id_hb) %>% 
  as_tibble() %>%
  rename("Code" = "HB") %>%
  rename("area_name" = "HBName")%>%
  select(Code, area_name)

hscp_codes <- dplyr::tbl(src = ckan$con, from = res_id_hscp) %>% 
  as_tibble() %>%
  rename("Code" = "HSCP")%>%
  rename("area_name" = "HSCPName")%>%
  select(Code, area_name)

geo_codes <- bind_rows(hb_codes, hscp_codes) %>%
  add_row(Code = "S92000003", area_name = "Scotland") %>%
  mutate(Area_name = recode(area_name, 
                            "NHS Dumfries and Galloway" = "NHS Dumfries & Galloway",    
                            "NHS Ayrshire and Arran" = "NHS Ayrshire & Arran",       
                            "NHS Greater Glasgow and Clyde" = "NHS Greater Glasgow & Clyde",
                            "Argyll and Bute" = "Argyll & Bute",              
                            "Clackmannanshire and Stirling" = "Clackmannanshire & Stirling",
                            "Dumfries and Galloway"  = "Dumfries & Galloway",        
                            "Perth and Kinross" = "Perth & Kinross"))

#add geo codes to source data files
hospital_admissions <- left_join(hospital_admissions, geo_codes, by = "area_name")
a_and_e <- left_join(a_and_e, geo_codes, by = "area_name")
nhs24 <- left_join(nhs24, geo_codes, by = "area_name")
ooh <- left_join(ooh, geo_codes, by = "area_name")
sas <- left_join(sas, geo_codes, by = "area_name")


##############################################.
###FUNCTIONS ----
###############################################.

#format names for ckan

od_names <- function(dataset) {
  dataset_hb <- dataset %>%
    rename("Average20182019" = "count_average") %>%
    rename("PercentVariation" = "variation") %>%
    rename("WeekEnding" = "week_ending")
}


###filter HB

hb_filter <- function(dataset) {
  dataset_hb <- dataset %>%
    filter(area_type != "HSC partnership") %>%
    rename("HB" = "Code")
}


###filter HSCP

hscp_filter <- function(dataset) {
  dataset_hscp <- dataset %>%
    filter(area_type == "HSC partnership") %>%
    rename("HSCP" = "Code")
}

###Age+Sex

age_sex_od <- function(dataset) {
  #filter for sex, rename, add AgeGroup
  dataset_sex <- dataset %>%
    filter(category %in% c("Male", "Female", "All")) %>%
    mutate(AgeGroup = "All ages") %>%
    rename("Sex" = "category")
  
  #filter for age, rename, add sex
  dataset_age <- dataset %>%
    filter(category %in% c("Aged 5 to 14", "Aged 15 to 44", 
                           "Aged 45 to 64", "Aged 65 to 74",
                           "Aged 75 to 84", "Aged 85 and over",
                           "Aged under 5"))%>%
    mutate(Sex = "All") %>%
    rename("AgeGroup" = "category")
  
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
    filter(category %in% c("Quintile 1 - most deprived", "Quintile 2", 
                           "Quintile 3", "Quintile 4", "Quintile 5 - least deprived")) %>%
    rename("SIMDQuintile" = "category") %>%
    mutate(SIMDQuintile = recode(SIMDQuintile, "Quintile 1 - most deprived" = "1",
                                 "Quintile 2" = "2",
                                 "Quintile 3" = "3",
                                 "Quintile 4" = "4",
                                 "Quintile 5 - least deprived" = "5"))
}

###Create open data

create_open_data <- function(dataset, count_variable, filepath_chosen,
                             data_name) {
  
  filepath <- filepath_chosen
  
  open_data <- dataset %>%
    mutate(week_ending = as.Date(week_ending, format = "%d %b %y")) %>%
    mutate(week_ending = strftime(week_ending, format = "%Y%m%d")) %>% 
    #rename common colnames
    od_names 
  
  names(open_data)[names(open_data) == 'count'] <- count_variable
  
  hb <- hb_filter(open_data)
  
  #rename, add qualifiers
  hb <- hb %>%
    mutate(HBQF = if_else(HB == "S92000003", "d", ""))
  
  #age-sex
  hb_age_sex <- age_sex_od(hb)
  
  #select+reorder
  hb_age_sex <- hb_age_sex %>%
    select_at(c("WeekEnding", "HB", "HBQF", "AgeGroup", "AgeGroupQF", "Sex", "SexQF",
                count_variable, "Average20182019", "PercentVariation")) %>%
    arrange(WeekEnding, HB, Sex, AgeGroup)
  
  #create OD csv file 1
  write_csv(hb_age_sex, glue("{filepath}/{data_name}_hb_agesex_{date}.csv"))
  
  ###By HB and SIMD
  
  #filter for simd
  hb_simd <- simd_od(hb)
  
  #select, reorder
  hb_simd <- hb_simd %>%
    select_at(c("WeekEnding", "HB", "HBQF", "SIMDQuintile", count_variable, 
                "Average20182019", "PercentVariation")) %>%
    arrange(WeekEnding, HB, SIMDQuintile)
  
  #create OD csv file 2
  write_csv(hb_simd, glue("{filepath}/{data_name}_hb_simd_{date}.csv"))
  
  ###By HSCP +Age +sex
  
  #filter for HB (+Scotland)
  hscp <- hscp_filter(open_data)
  
  #age-sex
  hscp_age_sex <- age_sex_od(hscp)
  
  #select+reorder
  hscp_age_sex <- hscp_age_sex %>%
    select_at(c("WeekEnding", "HSCP", "AgeGroup", "AgeGroupQF", "Sex", "SexQF", 
                count_variable, "Average20182019", "PercentVariation")) %>%
    arrange(WeekEnding, HSCP, Sex, AgeGroup)
  
  #create OD csv file 3
  write_csv(hscp_age_sex, glue("{filepath}/{data_name}_hscp_agesex_{date}.csv"))
  
  ###By HSCP and SIMD
  
  #filter for simd
  hscp_simd <- simd_od(hscp)  
  
  #select, reorder
  hscp_simd <- hscp_simd %>%
    select_at(c("WeekEnding", "HSCP", "SIMDQuintile", count_variable, 
                "Average20182019", "PercentVariation")) %>%
    arrange(WeekEnding, HSCP, SIMDQuintile)
  
  #create OD csv file 4
  write_csv(hscp_simd, glue("{filepath}/{data_name}_hscp_simd_{date}.csv"))
  
}


##############################################.
#1##Create Weekly_hospital_admissions OD Resources (6 files) ----
##############################################.

#format date
hospital_admissions <- hospital_admissions %>%
  mutate(week_ending = as.Date(week_ending, format = "%d %b %y")) %>%
  mutate(week_ending = strftime(week_ending, format = "%Y%m%d"))


#rename common colnames
hospital_admissions <- od_names(hospital_admissions) %>%
  rename("AdmissionType" = "admission_type") %>%
  rename("NumberAdmissions" = "count") %>%
  rename("Specialty" = "spec")

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
  filter(category == "All") %>%
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
  filter(category == "All") %>%
  mutate(SpecialtyQF = if_else(Specialty == "All", "d", "")) %>%
  select(WeekEnding, HSCP, AdmissionType, AdmissionTypeQF, Specialty, 
         SpecialtyQF, NumberAdmissions, Average20182019, PercentVariation) %>%
  arrange(AdmissionType, WeekEnding, HSCP)

#create OD csv file

write_csv(adm_hscp_spec, glue("{adm_filepath}/{resource6}_{date}.csv"))

# Tidy Global Environment
rm(adm_hb, adm_hb_age_sex, adm_hb_simd, adm_hb_spec, adm_hscp, adm_hscp_age_sex, 
   adm_hscp_simd, adm_hscp_spec)



##############################################.
#2##Create Weekly_a_and_e OD Resources ----
##############################################.

create_open_data(dataset = a_and_e, count_variable = "NumberAttendances",
                 filepath_chosen = ae_filepath, data_name = "a_and_e")



##############################################.
#3##Create Weekly_nhs24_calls OD Resources ----
##############################################.

create_open_data(dataset = nhs24, count_variable = "CompletedContacts",
                 filepath_chosen = nhs24_filepath, data_name = "nhs24")



##############################################.
#4##Create Weekly_ooh_consults OD Resources----
##############################################.

create_open_data(dataset = ooh, count_variable = "Consultations",
                 filepath_chosen = ooh_filepath, data_name = "ooh")



##############################################.
#5##Create Weekly_sas OD Resources----
##############################################.

create_open_data(dataset = sas, count_variable = "SASIncidents",
                 filepath_chosen = sas_filepath, data_name = "sas")

