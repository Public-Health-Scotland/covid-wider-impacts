
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

date <- strftime(Sys.Date(), format = "%Y%m%d")

#set filepath for httr configuration for api & source
config_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards",
                             "GPD", "5_GitHub", "GPD", "Geography", "Scottish Postcode Directory")

source(glue("{config_filepath}/Set httr configuration for API.R"))

#set file paths to read in dashboard data

source_filepath <- file.path("//Isdsf00d03", "PHSCOVID19_Analysis", "Publication outputs", 
                             "open_data")

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
deaths_filepath <- file.path(generic_filepath,
                             "OD2000030 - COVID-19 Wider Impact - Deaths")
cardio_filepath <- file.path(generic_filepath,
                             "OD2000032 - COVID-19 Wider Impact - Cardio")
cardio_drugs_filepath <- file.path(generic_filepath,
                                   "OD2000032 - COVID-19 Wider Impact - Cardio")
immunisation_filepath <- file.path(generic_filepath,
                                   "OD2000029 - COVID-19 Wider Impact - Immunisation")
childhealth_filepath <- file.path(generic_filepath,
                                  "OD2000031 - COVID-19 Wider Impact - Child Health")
cancer_filepath <- file.path(generic_filepath,
                             "OD2100002 - COVID-19 Wider Impact - Cancer")
breastfeeding_filepath <- file.path(generic_filepath,
                                    "OD2100003 - COVID-19 Wider Impact - Breastfeeding")
perinatal_filepath <- file.path(generic_filepath,
                                "OD2000033 - COVID-19 Wider Impact - Perinatal")
pregnancy_filepath <- file.path(generic_filepath,
                                "OD2100004 - COVID-19 Wider Impact - Pregnancy")


#set filenames for dashboard data tables --- UPDATE TO MATCH WHAT FILE NAMES WILL BE
hospital_admissions_dat <- "rapid_data"
a_and_e_dat <- "ae_data"
nhs24_dat <- "nhs24_data"
ooh_dat <- "ooh_data"
sas_dat <- "sas_data"
deaths_dat <- "deaths_data"
cardio_dat <- "ae_cardio_data"
cardio_drugs_dat <- "cardio_drugs_data"
cardio_labs_dat <- "cath_lab_data"
review_6_8_dat <- "six_to_eight_data"
review_13_15_dat <- "thirteen_data"
review_27_30_dat <- "twentyseven_data"
review_4_5_dat <- "fourtofive_data"
child_dev_dat <- "child_dev_data"
perinatal_dat <- "perinatal_data"
termination_dat <- "terminations_preg"
gestation_dat <- "gestation"
induction_dat <- "induction_labour"
delivery_dat <- "method_delivery"
antenatal_dat <- "ante_booking"


hospital_admissions <- read_rds(glue("{source_filepath}/{hospital_admissions_dat}.rds"))
a_and_e <- read_rds(glue("{source_filepath}/{a_and_e_dat}.rds"))
nhs24 <- read_rds(glue("{source_filepath}/{nhs24_dat}.rds"))
ooh <- read_rds(glue("{source_filepath}/{ooh_dat}.rds"))
sas <- read_rds(glue("{source_filepath}/{sas_dat}.rds"))
deaths <- read_rds(glue("{source_filepath}/{deaths_dat}.rds"))
cardio <- read_rds(glue("{source_filepath}/{cardio_dat}.rds"))
cardio_drugs <- read_rds(glue("{source_filepath}/{cardio_drugs_dat}.rds"))
cardio_labs <- read_rds(glue("{source_filepath}/{cardio_labs_dat}.rds"))
review_6_8 <- read_rds(glue("{source_filepath}/{review_6_8_dat}.rds"))
review_13_15 <- read_rds(glue("{source_filepath}/{review_13_15_dat}.rds"))
review_27_30 <- read_rds(glue("{source_filepath}/{review_27_30_dat}.rds"))
review_4_5 <- read_rds(glue("{source_filepath}/{review_4_5_dat}.rds"))
child_dev <- read_rds(glue("{source_filepath}/{child_dev_dat}.rds"))
perinatal <- read_rds(glue("{source_filepath}/{perinatal_dat}.rds"))
termination <- read_rds(glue("{source_filepath}/{termination_dat}.rds"))
gestation <- read_rds(glue("{source_filepath}/{gestation_dat}.rds"))
induction <- read_rds(glue("{source_filepath}/{induction_dat}.rds"))
delivery <- read_rds(glue("{source_filepath}/{delivery_dat}.rds"))
antenatal <- read_rds(glue("{source_filepath}/{antenatal_dat}.rds"))


# remove HSCP tag from child_dev area_name

child_dev <- child_dev %>%
  mutate(area_name = if_else(substr(child_dev$area_name, (nchar(area_name)-3), nchar(area_name))== "HSCP",
                             substr(child_dev$area_name, 1, (nchar(area_name) -5)), area_name))


# Set resources to use

resource1 <- "hospital_admissions_hb_agesex"
resource2 <- "hospital_admissions_hb_simd"
resource3 <- "hospital_admissions_hb_specialty"
resource4 <- "hospital_admissions_hscp_agesex"
resource5 <- "hospital_admissions_hscp_simd"
resource6 <- "hospital_admissions_hscp_specialty"

#Geo codes

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id_hb <- "652ff726-e676-4a20-abda-435b98dd7bdc"
res_id_hscp <- "944765d7-d0d9-46a0-b377-abb3de51d08e"

hb_codes <- dplyr::tbl(src = ckan$con, from = res_id_hb) %>% 
  as_tibble() %>%
  rename("Code" = "HB") %>%
  rename("area_name" = "HBName")%>%
  filter(is.na(HBDateArchived))%>%
  select(Code, area_name)

hscp_codes <- dplyr::tbl(src = ckan$con, from = res_id_hscp) %>% 
  as_tibble() %>%
  rename("Code" = "HSCP")%>%
  rename("area_name" = "HSCPName")%>%
  filter(is.na(HBDateArchived))%>%
  select(Code, area_name)

geo_codes <- bind_rows(hb_codes, hscp_codes) %>%
  add_row(Code = "S92000003", area_name = "Scotland") %>%
  # mutate(area_name = recode(area_name, 
  #                           "NHS Dumfries and Galloway" = "NHS Dumfries & Galloway",    
  #                           "NHS Ayrshire and Arran" = "NHS Ayrshire & Arran",       
  #                           "NHS Greater Glasgow and Clyde" = "NHS Greater Glasgow & Clyde",
  #                           "Argyll and Bute" = "Argyll & Bute",              
  #                           "Clackmannanshire and Stirling" = "Clackmannanshire & Stirling",
  #                           "Dumfries and Galloway"  = "Dumfries & Galloway",        
  #                           "Perth and Kinross" = "Perth & Kinross"))%>%
  add_row(Code = "S08000017", area_name = "NHS Dumfries & Galloway") %>%
  add_row(Code = "S08000015", area_name = "NHS Ayrshire & Arran") %>%
  add_row(Code = "S08000031", area_name = "NHS Greater Glasgow & Clyde") %>%
  add_row(Code = "S37000004", area_name = "Argyll & Bute") %>%
  add_row(Code = "S37000005", area_name = "Clackmannanshire & Stirling") %>%
  add_row(Code = "S37000006", area_name = "Dumfries & Galloway") %>%
  add_row(Code = "S37000033", area_name = "Perth & Kinross")

#add geo codes to source data files
hospital_admissions <- left_join(hospital_admissions, geo_codes, by = "area_name")
a_and_e <- left_join(a_and_e, geo_codes, by = "area_name")
nhs24 <- left_join(nhs24, geo_codes, by = "area_name")
ooh <- left_join(ooh, geo_codes, by = "area_name")
sas <- left_join(sas, geo_codes, by = "area_name")
deaths <- left_join(deaths, geo_codes, by = "area_name")
cardio <- left_join(cardio, geo_codes, by = "area_name")
cardio_drugs <- left_join(cardio_drugs, geo_codes, by = "area_name")
review_6_8 <- left_join(review_6_8, geo_codes, by = "area_name")
review_13_15 <- left_join(review_13_15, geo_codes, by = "area_name")
review_27_30 <- left_join(review_27_30, geo_codes, by = "area_name")
review_4_5 <- left_join(review_4_5, geo_codes, by = "area_name")
child_dev <- left_join(child_dev, geo_codes, by = "area_name")
perinatal <- left_join(perinatal, geo_codes, by = "area_name")
termination <- left_join(termination, geo_codes, by = "area_name")
gestation <- left_join(gestation, geo_codes, by = "area_name")
induction <- left_join(induction, geo_codes, by = "area_name")
delivery <- left_join(delivery, geo_codes, by = "area_name")
antenatal <- left_join(antenatal, geo_codes, by = "area_name")
# Set resources to use

##############################################.
###FUNCTIONS ----
###############################################.

#format names for ckan

od_names <- function(dataset) {
  dataset_hb <- dataset %>%
    #rename("Average20182019" = "count_average") %>%
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


###filter Weekly data

week_filter <- function(dataset) {
  dataset_weekly <- dataset %>%
    filter(substr(rev_6_8$Cohort, 1, 3)=="W/B") %>%
    rename("WeekBeginning" = "Cohort")
}


###filter Monthly & 2019 data

month_filter <- function(dataset) {
  dataset_monthly <- dataset %>%
    filter(substr(rev_6_8$Cohort, 1, 3)!="W/B") %>%
    rename("PeriodEligible" = "Cohort")
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
    filter(category %in% c("5 - 14", "15 - 44", 
                           "45 - 64", "65 - 74",
                           "75 - 84", "85 and over",
                           "Under 5", "Under 65", "65 and over",
                           "Under 60", "60 and over"))%>%
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
    filter(category %in% c("1 - most deprived", "2", 
                           "3", "4", "5 - least deprived")) %>%
    rename("SIMDQuintile" = "category") %>%
    mutate(SIMDQuintile = recode(SIMDQuintile, "1 - most deprived" = "1",
                                 "5 - least deprived" = "5"))
}

###Create open data

create_open_data <- function(dataset, count_variable, filepath_chosen,
                             data_name, average = "Average20182019", countrify = F) {
  
  filepath <- filepath_chosen
  
  open_data <- dataset %>%
    mutate(week_ending = as.Date(week_ending, format = "%d %b %y")) %>%
    mutate(week_ending = strftime(week_ending, format = "%Y%m%d")) %>% 
    #rename common colnames
    od_names
  
  names(open_data)[names(open_data) == 'count'] <- count_variable
  
  names(open_data)[names(open_data) == 'count_average'] <- average
  
  hb <- hb_filter(open_data)
  
  #rename, add qualifiers
  hb <- hb %>%
    mutate(HBQF = if_else(HB == "S92000003", "d", ""))
  
  #age-sex
  hb_age_sex <- age_sex_od(hb)
  
  #select+reorder
  hb_age_sex <- hb_age_sex %>%
    select_at(c("WeekEnding", "HB", "HBQF", "AgeGroup", "AgeGroupQF", "Sex", "SexQF",
                count_variable, average, "PercentVariation")) %>%
    arrange(WeekEnding, HB, Sex, factor(AgeGroup, levels = c("Under 5", "5 - 14", "15 - 44", 
                                                             "45 - 64", "65 - 74",
                                                             "75 - 84", "85 and over",
                                                             "Under 65", "65 and over")))
  
  #add PercentVariationQF if blanks 
  if(NA %in% hb_age_sex$PercentVariation){
    hb_age_sex <- hb_age_sex %>%
      mutate(PercentVariationQF = if_else(is.na(PercentVariation), "z", ""))
  }
  
  #create OD csv file 1
  write_csv(hb_age_sex, glue("{filepath}/{data_name}_hb_agesex_{date}.csv"), na = "")
  
  ###By HB and SIMD
  
  #filter for simd
  hb_simd <- simd_od(hb)
  
  #select, reorder
  hb_simd <- hb_simd %>%
    select_at(c("WeekEnding", "HB", "HBQF", "SIMDQuintile", count_variable, 
                average, "PercentVariation")) %>%
    arrange(WeekEnding, HB, SIMDQuintile)
  
  #rename HB to Country for Deaths
  if(countrify == T){
    hb_simd <- hb_simd %>%
      rename("Country" = "HB") %>%
      select_at(c("WeekEnding", "Country", "SIMDQuintile", count_variable, 
                  average, "PercentVariation")) %>%
      arrange(WeekEnding, SIMDQuintile)
  }
  
  
  #add PercentVariationQF if blanks 
  if("" %in% hb_simd$PercentVariation){
    hb_simd <- hb_simd %>%
      mutate(PercentVariationQF = if_else(is.na(PercentVariation), "z", ""))
  }
  
  #create OD csv file 2
  write_csv(hb_simd, glue("{filepath}/{data_name}_hb_simd_{date}.csv"), na = "")
  
  ###By HSCP +Age +sex
  
  #filter for HB (+Scotland)
  hscp <- hscp_filter(open_data)
  
  #age-sex
  hscp_age_sex <- age_sex_od(hscp)
  
  #select+reorder
  hscp_age_sex <- hscp_age_sex %>%
    select_at(c("WeekEnding", "HSCP", "AgeGroup", "AgeGroupQF", "Sex", "SexQF", 
                count_variable, average, "PercentVariation")) %>%
    arrange(WeekEnding, HSCP, Sex, factor(AgeGroup, levels = c("Under 5", "5 - 14", "15 - 44", 
                                                               "45 - 64", "65 - 74",
                                                               "75 - 84", "85 and over",
                                                               "Under 65", "65 and over")))
  
  #add PercentVariationQF if blanks 
  if(NA %in% hscp_age_sex$PercentVariation){
    hscp_age_sex <- hscp_age_sex %>%
      mutate(PercentVariationQF = if_else(is.na(PercentVariation), "z", ""))
  }
  
  #create OD csv file 3
  write_csv(hscp_age_sex, glue("{filepath}/{data_name}_hscp_agesex_{date}.csv"), na = "")
  
  ###By HSCP and SIMD
  #filter for simd
  hscp_simd <- simd_od(hscp)
  #select, reorder
  hscp_simd <- hscp_simd %>%
    select_at(c("WeekEnding", "HSCP", "SIMDQuintile", count_variable, 
                average, "PercentVariation")) %>%
    arrange(WeekEnding, HSCP, SIMDQuintile)
  
  #add PercentVariationQF if blanks 
  if(NA %in% hscp_simd$PercentVariation){
    hscp_simd <- hscp_simd %>%
      mutate(PercentVariationQF = if_else(is.na(PercentVariation), "z", ""))
  }
  
  if(countrify == F){
    #create OD csv file 4
    write_csv(hscp_simd, glue("{filepath}/{data_name}_hscp_simd_{date}.csv"), na = "")
  } 
  
}

###create pregnancy files

create_maternity_open_data <- function(dataset, colheaders, data_name) {
  
  open_data <- dataset %>%
    mutate(Month = glue("01 ", "{Month}"))%>%
    mutate(Month = dmy(Month))%>%
    mutate(Month = strftime(Month, format = "%Y%m"))
  
  if(data_name != "terminations_preg") open_data <- open_data %>%
      rename("category" = "variable") 
  
  # HB file
  hb <- open_data %>%
    filter(category == "All")%>%
    rename("HB" = "Code") %>%
    mutate(HBQF = if_else(HB == "S92000003", "d", "")) 
  
  if(data_name == "terminations_preg") hb <- hb %>%
    mutate(NumberGestation10to12WksQF = if_else(NumberGestation10to12Wks == "*", "c", "")) %>%
    mutate(NumberGestationOver12WksQF = if_else(NumberGestationOver12Wks == "*", "c", "")) %>%
    mutate(NumberGestationUnder10WksQF = if_else(NumberGestationUnder10Wks == "*", "c", "")) 
  
  # remove *
  hb[hb == "*"] <- ""
  
  hb <- hb %>%
    select(HB, HBQF, Month, colheaders)%>%
    arrange(Month, HB)
  
  #create OD csv file 1
  write_csv(hb, glue("{pregnancy_filepath}/{data_name}_hb_{date}.csv"), na = "")
  
  
  # Age file
  age <- open_data %>%
    filter(category %in% c("Under 20", "20-24", "25-29", "30-34",
                           "35-39", "40 and over"))%>%
    rename("Country" = "Code",
           "AgeGroup" = "category")
  
  if(data_name != "terminations_preg") age <- age %>%
    select(Country, Month, AgeGroup, colheaders)%>%
    arrange(Month, AgeGroup)
  
  if(data_name == "terminations_preg") age <- age %>%
    select(Country, Month, AgeGroup, AverageGestation, NumberTerminations)%>%
    arrange(Month, AgeGroup)
  
  #create OD csv file 2
  write_csv(age, glue("{pregnancy_filepath}/{data_name}_age_{date}.csv"), na = "")
  
  
  # SIMD file
  simd <- open_data %>%
    filter(category %in% c("1 - most deprived", "2", "3", "4", "5 - least deprived"))%>%
    rename("Country" = "Code",
           "SIMDQuintile" = "category") %>%
    mutate(SIMDQuintile = recode(SIMDQuintile, 
                                 "1 - most deprived" = "1",
                                 "5 - least deprived" = "5"))
  
  if(data_name != "terminations_preg") simd <- simd %>%
    select(Country, Month, SIMDQuintile, colheaders)%>%
    arrange(Month, SIMDQuintile)
  
  if(data_name == "terminations_preg") simd <- simd %>%
    select(Country, Month, SIMDQuintile, AverageGestation, NumberTerminations)%>%
    arrange(Month, SIMDQuintile)
  
  #create OD csv file 3
  write_csv(simd, glue("{pregnancy_filepath}/{data_name}_simd_{date}.csv"), na = "")
  
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
  rename("Average20182019" = "count_average")%>%
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
  arrange(AdmissionType, WeekEnding, HB, Sex, factor(AgeGroup, levels = c("Under 5", "5 - 14", "15 - 44", 
                                                                          "45 - 64", "65 - 74",
                                                                          "75 - 84", "85 and over")))

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
  arrange(AdmissionType, WeekEnding, HSCP, Sex, factor(AgeGroup, levels = c("Under 5", "5 - 14", "15 - 44", 
                                                                            "45 - 64", "65 - 74",
                                                                            "75 - 84", "85 and over")))

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



##############################################.
#6##Create Weekly_nrs OD Resources----
##############################################.

#format date
#deaths <- deaths %>%
#  mutate(week_ending = as.Date(week_ending, format = "%d %b %y")) %>%
#  mutate(week_ending = strftime(week_ending, format = "%Y%m%d"))

create_open_data(dataset = deaths, count_variable = "Deaths",
                 filepath_chosen = deaths_filepath, data_name = "deaths",
                 average = "Average20152019", countrify = T)

#simd only scotland, Average20152019



##############################################.
#7##Create Weekly_cardio OD Resources----
##############################################.

#format date
cardio <- cardio %>%
  mutate(week_ending = as.Date(week_ending, format = "%d %b %y")) %>%
  mutate(week_ending = strftime(week_ending, format = "%Y%m%d"))

#general formatting
cardio <- cardio %>%
  od_names() %>%
  rename("Country" = "Code")%>%
  rename("Average20182019" = "count_average")%>%
  rename("CardioAdmissions" = "count")

###By Age

#filter for age, rename
cardio_age <- cardio %>%
  filter(category %in% c("<65", "65+"))%>%
  rename("AgeGroup" = "category")%>%
  mutate(AgeGroup = recode(AgeGroup, 
                           "<65" = "Under 65",
                           "65+" = "65 and over"))%>%
  select(WeekEnding, Country, AgeGroup, CardioAdmissions, Average20182019, 
         PercentVariation)%>%
  arrange(WeekEnding, AgeGroup)

#create OD csv file 1
write_csv(cardio_age, glue("{cardio_filepath}/{cardio_dat}_age_{date}.csv"), na = "")

###By SIMD

cardio_simd <- cardio %>%
  simd_od() %>%
  select_at(c("WeekEnding", "Country", "SIMDQuintile", "CardioAdmissions", 
              "Average20182019", "PercentVariation")) %>%
  arrange(WeekEnding, SIMDQuintile)

#create OD csv file 2
write_csv(cardio_simd, glue("{cardio_filepath}/{cardio_dat}_simd_{date}.csv"), na = "")


##############################################.
#8##Create Weekly_cardio_drugs OD Resources----
##############################################.

#format date
cardio_drugs <- cardio_drugs %>%
  mutate(week_ending = as.Date(week_ending, format = "%d %b %y")) %>%
  mutate(week_ending = strftime(week_ending, format = "%Y%m%d"))

#general formatting
cardio_drugs <- cardio_drugs %>%
  od_names() %>%
  rename("Average20182019" = "count_average")%>%
  rename("NumberPrescriptions" = "count")%>%
  rename("DrugGroup" = "category")%>%
  mutate(DrugGroupQF = if_else(DrugGroup == "All", "d", ""))%>%
  mutate(DrugGroup = recode(DrugGroup,
                            "Antihypertensive, anti-anginal, anti-arrhythmic and heart failure drugs" =
                              "Antihypertensive anti-anginal anti-arrhythmic and heart failure drugs"))


###HB
cardio_drugs_hb <- cardio_drugs %>%
  hb_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(WeekEnding, HB, HBQF, DrugGroup, DrugGroupQF, NumberPrescriptions,
         Average20182019, PercentVariation)%>%
  arrange(WeekEnding, HB, DrugGroup)

#create OD csv file 1
write_csv(cardio_drugs_hb, glue("{cardio_drugs_filepath}/{cardio_drugs_dat}_hb_{date}.csv"), na = "")

###HB
cardio_drugs_hscp <- cardio_drugs %>%
  hscp_filter()%>%
  #rename, add qualifiers
  select(WeekEnding, HSCP, DrugGroup, DrugGroupQF, NumberPrescriptions,
         Average20182019, PercentVariation)%>%
  arrange(WeekEnding, HSCP, DrugGroup)

#create OD csv file 1
write_csv(cardio_drugs_hscp, glue("{cardio_drugs_filepath}/{cardio_drugs_dat}_hscp_{date}.csv"), na = "")



##############################################.
#9##Create Monthly_cardio_drugs OD Resources----
##############################################.

#updated approximately monthly

#format date
#cardio_labs <- cardio_labs %>%
#  mutate(week_ending = as.Date(week_ending, format = "%d %b %y")) %>%
#  mutate(week_ending = strftime(week_ending, format = "%Y%m%d"))

#general formatting
#cardio_labs <- cardio_labs %>%
#  od_names() %>%
#  rename("Average20182019" = "count_average")%>%
#  rename("NumberCases" = "count")%>%
#  rename("Intervention" = "groups")%>%
#  age_sex_od()%>%
#  mutate(lab = recode(lab, "Royal Infirmary of Edinburgh" = "S314H",
#                      "Golden Jubilee National Hospital" = "D102H"))%>%
#  rename("HospitalCode" = "lab")%>%
#  mutate(HospitalCodeQF = ifelse(HospitalCode == "All", "d", ""),
#         NumberCasesQF = ifelse(is.na(NumberCases), "c", ""),
#         PercentVariationQF = ifelse(is.na(PercentVariation), "z", ""))%>%
#  select(WeekEnding, HospitalCode, HospitalCodeQF, AgeGroup, AgeGroupQF, Sex, SexQF, 
#         Intervention, NumberCases, NumberCasesQF, Average20182019, PercentVariation, PercentVariationQF)%>%
#  arrange(WeekEnding, HospitalCode, factor(AgeGroup, 
#                      levels = c("Under 60", "60 and over", "All ages")), 
#                      Sex, Intervention)

#create OD csv file

#write_csv(cardio_labs, glue("{cardio_filepath}/{cardio_labs_dat}_{date}.csv"), na = "")


##############################################.
#10##Create Child Health Review OD Resources----
##############################################.


### create 6-8 months review files (By week, By Month, By HB, By HSCP)

#general formatting

rev_6_8 <- review_6_8 %>%
  rename("PercentCoverage10Weeks" = "coverage_10weeks_percent",
         "Coverage10Weeks" = "coverage_10weeks_num",
         "PercentCoverage22Weeks" = "coverage_22weeks_percent",
         "Coverage22Weeks" = "coverage_22weeks_num",
         "PercentTotalCoverage" = "coverage_tot_percent",
         "TotalCoverage" = "coverage_tot_num",
         "TotalNumberChildren" = "denominator")%>%
  mutate_if(is.factor, as.character)

###HB files

rev_6_8_hb <- rev_6_8 %>%
  #filter for hb and scotland
  filter(area_name %in% c(hb_codes$area_name, "Scotland", "NHS Ayrshire & Arran",
                          "NHS Dumfries & Galloway", "NHS Greater Glasgow & Clyde")) %>%
  rename("HB" = "Code")

#Weekly
rev_6_8_hb_week <- rev_6_8_hb %>%
  filter(substr(rev_6_8_hb$time_period_eligible, 1, 3)=="W/B") %>%
  rename("WeekBeginning" = "time_period_eligible")%>%
  #week_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(WeekBeginning, HB, HBQF, PercentCoverage10Weeks, Coverage10Weeks,
         PercentCoverage22Weeks, Coverage22Weeks, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(WeekBeginning = substr(WeekBeginning, 4, nchar(WeekBeginning)))%>%
  mutate(WeekBeginning = as.Date(WeekBeginning, format = "%d-%b-%Y")) %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d"))%>%
  arrange(WeekBeginning, HB)

#create OD csv file 1
write_csv(rev_6_8_hb_week, glue("{childhealth_filepath}/{review_6_8_dat}_hb_weekly_{date}.csv"), na = "")

#monthly
rev_6_8_hb_monthly <- rev_6_8_hb %>%
  filter(substr(rev_6_8_hb$time_period_eligible, 1, 3)!="W/B") %>%
  filter(time_period_eligible != "2019")%>%
  rename("PeriodEligible" = "time_period_eligible")%>%
  #month_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(PeriodEligible, HB, HBQF, PercentCoverage10Weeks, Coverage10Weeks,
         PercentCoverage22Weeks, Coverage22Weeks, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(PeriodEligible = glue("01 ", "{PeriodEligible}"))%>%
  mutate(PeriodEligible = dmy(PeriodEligible))%>%
  mutate(PeriodEligible = strftime(PeriodEligible, format = "%Y%m"))%>%
  arrange(PeriodEligible, HB)

#create OD csv file 2
write_csv(rev_6_8_hb_monthly, glue("{childhealth_filepath}/{review_6_8_dat}_hb_monthly_{date}.csv"), na = "")


###HSCP files
rev_6_8_hscp <- rev_6_8 %>%
  #filter for hscps
  filter(area_name %in% hscp_codes$area_name) %>%
  rename("HSCP" = "Code")%>%
  mutate(HSCP = if_else(
    area_name == "Argyll and Bute", "S37000004",
    if_else(area_name == "Clackmannanshire and Stirling", "S37000005",
            if_else(area_name == "Dumfries and Galloway", "S37000006",
                    if_else(area_name == "Perth and Kinross", "S37000033", HSCP)))
  ))

#Weekly
rev_6_8_hscp_weekly <- rev_6_8_hscp %>%
  filter(substr(rev_6_8_hscp$time_period_eligible, 1, 3)=="W/B") %>%
  rename("WeekBeginning" = "time_period_eligible")%>%
  #rename, add qualifiers
  select(WeekBeginning, HSCP, PercentCoverage10Weeks, Coverage10Weeks,
         PercentCoverage22Weeks, Coverage22Weeks, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(WeekBeginning = substr(WeekBeginning, 4, nchar(WeekBeginning)))%>%
  mutate(WeekBeginning = as.Date(WeekBeginning, format = "%d-%b-%Y")) %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d"))%>%
  arrange(WeekBeginning, HSCP)

#create OD csv file 3
write_csv(rev_6_8_hscp_weekly, glue("{childhealth_filepath}/{review_6_8_dat}_hscp_weekly_{date}.csv"), na = "")

#monthly
rev_6_8_hscp_monthly <- rev_6_8_hscp %>%
  filter(substr(rev_6_8_hscp$time_period_eligible, 1, 3)!="W/B") %>%
  filter(time_period_eligible != "2019")%>%
  rename("PeriodEligible" = "time_period_eligible")%>%
  #month_filter()%>%
  #rename, add qualifiers
  select(PeriodEligible, HSCP, PercentCoverage10Weeks, Coverage10Weeks,
         PercentCoverage22Weeks, Coverage22Weeks, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(PeriodEligible = glue("01 ", "{PeriodEligible}"))%>%
  mutate(PeriodEligible = dmy(PeriodEligible))%>%
  mutate(PeriodEligible = strftime(PeriodEligible, format = "%Y%m"))%>%
  arrange(PeriodEligible, HSCP)

#create OD csv file 4
write_csv(rev_6_8_hscp_monthly, glue("{childhealth_filepath}/{review_6_8_dat}_hscp_monthly_{date}.csv"), na = "")


### create 13-15 months review files (By week, By Month, By HB, By HSCP)

#general formatting

rev_13_15 <- review_13_15 %>%
  rename("PercentCoverage14Months" = "coverage_14months_percent",
         "Coverage14Months" = "coverage_14months_num",
         "PercentCoverage17Months" = "coverage_17months_percent",
         "Coverage17Months" = "coverage_17months_num",
         "PercentTotalCoverage" = "coverage_tot_percent",
         "TotalCoverage" = "coverage_tot_num",
         "TotalNumberChildren" = "denominator")%>%
  mutate_if(is.factor, as.character)

###HB files

rev_13_15_hb <- rev_13_15 %>%
  #filter for hb and scotland
  filter(area_name %in% c(hb_codes$area_name, "Scotland", "NHS Ayrshire & Arran",
                          "NHS Dumfries & Galloway", "NHS Greater Glasgow & Clyde")) %>%
  rename("HB" = "Code")

#Weekly
rev_13_15_hb_week <- rev_13_15_hb %>%
  filter(substr(rev_13_15_hb$time_period_eligible, 1, 3)=="W/B") %>%
  rename("WeekBeginning" = "time_period_eligible")%>%
  #week_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(WeekBeginning, HB, HBQF, PercentCoverage14Months, Coverage14Months,
         PercentCoverage17Months, Coverage17Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(WeekBeginning = substr(WeekBeginning, 4, nchar(WeekBeginning)))%>%
  mutate(WeekBeginning = as.Date(WeekBeginning, format = "%d-%b-%Y")) %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d"))%>%
  arrange(WeekBeginning, HB)

#create OD csv file 1
write_csv(rev_13_15_hb_week, glue("{childhealth_filepath}/{review_13_15_dat}_hb_weekly_{date}.csv"), na = "")

#monthly
rev_13_15_hb_monthly <- rev_13_15_hb %>%
  filter(substr(rev_13_15_hb$time_period_eligible, 1, 3)!="W/B") %>%
  filter(time_period_eligible != "2019")%>%
  rename("PeriodEligible" = "time_period_eligible")%>%
  #month_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(PeriodEligible, HB, HBQF, PercentCoverage14Months, Coverage14Months,
         PercentCoverage17Months, Coverage17Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(PeriodEligible = glue("01 ", "{PeriodEligible}"))%>%
  mutate(PeriodEligible = dmy(PeriodEligible))%>%
  mutate(PeriodEligible = strftime(PeriodEligible, format = "%Y%m"))%>%
  arrange(PeriodEligible, HB)

#create OD csv file 2
write_csv(rev_13_15_hb_monthly, glue("{childhealth_filepath}/{review_13_15_dat}_hb_monthly_{date}.csv"), na = "")


###HSCP files
rev_13_15_hscp <- rev_13_15 %>%
  #filter for hscps
  filter(area_name %in% hscp_codes$area_name) %>%
  rename("HSCP" = "Code")%>%
  mutate(HSCP = if_else(
    area_name == "Argyll and Bute", "S37000004",
    if_else(area_name == "Clackmannanshire and Stirling", "S37000005",
            if_else(area_name == "Dumfries and Galloway", "S37000006",
                    if_else(area_name == "Perth and Kinross", "S37000033", HSCP)))
  ))

#Weekly
rev_13_15_hscp_weekly <- rev_13_15_hscp %>%
  filter(substr(rev_13_15_hscp$time_period_eligible, 1, 3)=="W/B") %>%
  rename("WeekBeginning" = "time_period_eligible")%>%
  #rename, add qualifiers
  select(WeekBeginning, HSCP, PercentCoverage14Months, Coverage14Months,
         PercentCoverage17Months, Coverage17Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(WeekBeginning = substr(WeekBeginning, 4, nchar(WeekBeginning)))%>%
  mutate(WeekBeginning = as.Date(WeekBeginning, format = "%d-%b-%Y")) %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d"))%>%
  arrange(WeekBeginning, HSCP)

#create OD csv file 3
write_csv(rev_13_15_hscp_weekly, glue("{childhealth_filepath}/{review_13_15_dat}_hscp_weekly_{date}.csv"), na = "")

#monthly
rev_13_15_hscp_monthly <- rev_13_15_hscp %>%
  filter(substr(rev_13_15_hscp$time_period_eligible, 1, 3)!="W/B") %>%
  filter(time_period_eligible != "2019")%>%
  rename("PeriodEligible" = "time_period_eligible")%>%
  #month_filter()%>%
  #rename, add qualifiers
  select(PeriodEligible, HSCP, PercentCoverage14Months, Coverage14Months,
         PercentCoverage17Months, Coverage17Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(PeriodEligible = glue("01 ", "{PeriodEligible}"))%>%
  mutate(PeriodEligible = dmy(PeriodEligible))%>%
  mutate(PeriodEligible = strftime(PeriodEligible, format = "%Y%m"))%>%
  arrange(PeriodEligible, HSCP)

#create OD csv file 4
write_csv(rev_13_15_hscp_monthly, glue("{childhealth_filepath}/{review_13_15_dat}_hscp_monthly_{date}.csv"), na = "")


### create 27-30 months review files (By week, By Month, By HB, By HSCP)

#general formatting

rev_27_30 <- review_27_30 %>%
  rename("PercentCoverage28Months" = "coverage_28months_percent",
         "Coverage28Months" = "coverage_28months_num",
         "PercentCoverage31Months" = "coverage_31months_percent",
         "Coverage31Months" = "coverage_31months_num",
         "PercentTotalCoverage" = "coverage_tot_percent",
         "TotalCoverage" = "coverage_tot_num",
         "TotalNumberChildren" = "denominator") %>%
  mutate_if(is.factor, as.character)

###HB files

rev_27_30_hb <- rev_27_30 %>%
  #filter for hb and scotland
  filter(area_name %in% c(hb_codes$area_name, "Scotland", "NHS Ayrshire & Arran",
                          "NHS Dumfries & Galloway", "NHS Greater Glasgow & Clyde")) %>%
  rename("HB" = "Code")

#Weekly
rev_27_30_hb_week <- rev_27_30_hb %>%
  filter(substr(rev_27_30_hb$time_period_eligible, 1, 3)=="W/B") %>%
  rename("WeekBeginning" = "time_period_eligible")%>%
  #week_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(WeekBeginning, HB, HBQF, PercentCoverage28Months, Coverage28Months,
         PercentCoverage31Months, Coverage31Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(WeekBeginning = substr(WeekBeginning, 4, nchar(WeekBeginning)))%>%
  mutate(WeekBeginning = as.Date(WeekBeginning, format = "%d-%b-%Y")) %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d"))%>%
  arrange(WeekBeginning, HB)

#create OD csv file 1
write_csv(rev_27_30_hb_week, glue("{childhealth_filepath}/{review_27_30_dat}_hb_weekly_{date}.csv"), na = "")

#monthly
rev_27_30_hb_monthly <- rev_27_30_hb %>%
  filter(substr(rev_27_30_hb$time_period_eligible, 1, 3)!="W/B") %>%
  filter(time_period_eligible != "2019")%>%
  rename("PeriodEligible" = "time_period_eligible")%>%
  #month_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(PeriodEligible, HB, HBQF, PercentCoverage28Months, Coverage28Months,
         PercentCoverage31Months, Coverage31Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(PeriodEligible = glue("01 ", "{PeriodEligible}"))%>%
  mutate(PeriodEligible = dmy(PeriodEligible))%>%
  mutate(PeriodEligible = strftime(PeriodEligible, format = "%Y%m"))%>%
  arrange(PeriodEligible, HB)

#create OD csv file 2
write_csv(rev_27_30_hb_monthly, glue("{childhealth_filepath}/{review_27_30_dat}_hb_monthly_{date}.csv"), na = "")


###HSCP files
rev_27_30_hscp <- rev_27_30 %>%
  #filter for hscps
  filter(area_name %in% hscp_codes$area_name) %>%
  rename("HSCP" = "Code")%>%
  mutate(HSCP = if_else(
    area_name == "Argyll and Bute", "S37000004",
    if_else(area_name == "Clackmannanshire and Stirling", "S37000005",
            if_else(area_name == "Dumfries and Galloway", "S37000006",
                    if_else(area_name == "Perth and Kinross", "S37000033", HSCP)))
  ))

#Weekly
rev_27_30_hscp_weekly <- rev_27_30_hscp %>%
  filter(substr(rev_27_30_hscp$time_period_eligible, 1, 3)=="W/B") %>%
  rename("WeekBeginning" = "time_period_eligible")%>%
  #rename, add qualifiers
  select(WeekBeginning, HSCP, PercentCoverage28Months, Coverage28Months,
         PercentCoverage31Months, Coverage31Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(WeekBeginning = substr(WeekBeginning, 4, nchar(WeekBeginning)))%>%
  mutate(WeekBeginning = as.Date(WeekBeginning, format = "%d-%b-%Y")) %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d"))%>%
  arrange(WeekBeginning, HSCP)

#create OD csv file 3
write_csv(rev_27_30_hscp_weekly, glue("{childhealth_filepath}/{review_27_30_dat}_hscp_weekly_{date}.csv"), na = "")

#monthly
rev_27_30_hscp_monthly <- rev_27_30_hscp %>%
  filter(substr(rev_27_30_hscp$time_period_eligible, 1, 3)!="W/B") %>%
  filter(time_period_eligible != "2019")%>%
  rename("PeriodEligible" = "time_period_eligible")%>%
  #month_filter()%>%
  #rename, add qualifiers
  select(PeriodEligible, HSCP, PercentCoverage28Months, Coverage28Months,
         PercentCoverage31Months, Coverage31Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(PeriodEligible = glue("01 ", "{PeriodEligible}"))%>%
  mutate(PeriodEligible = dmy(PeriodEligible))%>%
  mutate(PeriodEligible = strftime(PeriodEligible, format = "%Y%m"))%>%
  arrange(PeriodEligible, HSCP)

#create OD csv file 2
write_csv(rev_27_30_hscp_monthly, glue("{childhealth_filepath}/{review_27_30_dat}_hscp_monthly_{date}.csv"), na = "")


### create 4-5 year review files (By week, By Month, By HB, By HSCP)

#general formatting

rev_4_5 <- review_4_5 %>%
  rename("PercentCoverage49Months" = "coverage_49months_percent",
         "Coverage49Months" = "coverage_49months_num",
         "PercentCoverage52Months" = "coverage_52months_percent",
         "Coverage52Months" = "coverage_52months_num",
         "PercentTotalCoverage" = "coverage_tot_percent",
         "TotalCoverage" = "coverage_tot_num",
         "TotalNumberChildren" = "denominator") %>%
  mutate_if(is.factor, as.character)

###HB files

rev_4_5_hb <- rev_4_5 %>%
  #filter for hb and scotland
  filter(area_name %in% c(hb_codes$area_name, "Scotland", "NHS Ayrshire & Arran",
                          "NHS Dumfries & Galloway", "NHS Greater Glasgow & Clyde")) %>%
  rename("HB" = "Code")

#Weekly
rev_4_5_hb_week <- rev_4_5_hb %>%
  filter(substr(rev_4_5_hb$time_period_eligible, 1, 3)=="W/B") %>%
  rename("WeekBeginning" = "time_period_eligible")%>%
  #week_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(WeekBeginning, HB, HBQF, PercentCoverage49Months, Coverage49Months,
         PercentCoverage52Months, Coverage52Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(WeekBeginning = substr(WeekBeginning, 4, nchar(WeekBeginning)))%>%
  mutate(WeekBeginning = as.Date(WeekBeginning, format = "%d-%b-%Y")) %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d"))%>%
  arrange(WeekBeginning, HB)

#create OD csv file 1
write_csv(rev_4_5_hb_week, glue("{childhealth_filepath}/{review_4_5_dat}_hb_weekly_{date}.csv"), na = "")

#monthly
rev_4_5_hb_monthly <- rev_4_5_hb %>%
  filter(substr(rev_4_5_hb$time_period_eligible, 1, 3)!="W/B") %>%
  filter(time_period_eligible != "2019")%>%
  rename("PeriodEligible" = "time_period_eligible")%>%
  #month_filter()%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(PeriodEligible, HB, HBQF, PercentCoverage49Months, Coverage49Months,
         PercentCoverage52Months, Coverage52Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(PeriodEligible = glue("01 ", "{PeriodEligible}"))%>%
  mutate(PeriodEligible = dmy(PeriodEligible))%>%
  mutate(PeriodEligible = strftime(PeriodEligible, format = "%Y%m"))%>%
  arrange(PeriodEligible, HB)

#create OD csv file 2
write_csv(rev_4_5_hb_monthly, glue("{childhealth_filepath}/{review_4_5_dat}_hb_monthly_{date}.csv"), na = "")


###HSCP files
rev_4_5_hscp <- rev_4_5 %>%
  #filter for hscps
  filter(area_name %in% hscp_codes$area_name) %>%
  rename("HSCP" = "Code")%>%
  mutate(HSCP = if_else(
    area_name == "Argyll and Bute", "S37000004",
    if_else(area_name == "Clackmannanshire and Stirling", "S37000005",
            if_else(area_name == "Dumfries and Galloway", "S37000006",
                    if_else(area_name == "Perth and Kinross", "S37000033", HSCP)))
  ))

#Weekly
rev_4_5_hscp_weekly <- rev_4_5_hscp %>%
  filter(substr(rev_4_5_hscp$time_period_eligible, 1, 3)=="W/B") %>%
  rename("WeekBeginning" = "time_period_eligible")%>%
  #rename, add qualifiers
  select(WeekBeginning, HSCP, PercentCoverage49Months, Coverage49Months,
         PercentCoverage52Months, Coverage52Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(WeekBeginning = substr(WeekBeginning, 4, nchar(WeekBeginning)))%>%
  mutate(WeekBeginning = as.Date(WeekBeginning, format = "%d-%b-%Y")) %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d"))%>%
  arrange(WeekBeginning, HSCP)

#create OD csv file 3
write_csv(rev_4_5_hscp_weekly, glue("{childhealth_filepath}/{review_4_5_dat}_hscp_weekly_{date}.csv"), na = "")

#monthly
rev_4_5_hscp_monthly <- rev_4_5_hscp %>%
  filter(substr(rev_4_5_hscp$time_period_eligible, 1, 3)!="W/B") %>%
  filter(time_period_eligible != "2019")%>%
  rename("PeriodEligible" = "time_period_eligible")%>%
  #month_filter()%>%
  #rename, add qualifiers
  select(PeriodEligible, HSCP, PercentCoverage49Months, Coverage49Months,
         PercentCoverage52Months, Coverage52Months, PercentTotalCoverage,
         TotalCoverage, TotalNumberChildren)%>%
  mutate(PeriodEligible = glue("01 ", "{PeriodEligible}"))%>%
  mutate(PeriodEligible = dmy(PeriodEligible))%>%
  mutate(PeriodEligible = strftime(PeriodEligible, format = "%Y%m"))%>%
  arrange(PeriodEligible, HSCP)

#create OD csv file 4
write_csv(rev_4_5_hscp_monthly, glue("{childhealth_filepath}/{review_4_5_dat}_hscp_monthly_{date}.csv"), na = "")



### Create child development review resources

child_dev <- child_dev %>%
  rename("Month" = "month_review",
         "NumberReviews" = "number_reviews",
         "MeaningfulReviews" = "meaningful_reviews",
         "PercentMeaningful" = "% meaningful reviews",
         "Concerns" = "One or more concerns",
         "PercentConcerns" = "% one or more concerns",
         "Review" = "review")%>%
  mutate(Month = strftime(Month, format = "%Y%m"))

###HB file

child_dev_hb <- child_dev %>%
  #filter for hb and scotland
  filter(area_name %in% c(hb_codes$area_name, "Scotland", "NHS Ayrshire & Arran",
                          "NHS Dumfries & Galloway", "NHS Greater Glasgow & Clyde")) %>%
  rename("HB" = "Code")%>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(Month, HB, HBQF, Review, NumberReviews, MeaningfulReviews, PercentMeaningful,
         Concerns, PercentConcerns)%>%
  arrange(Month, HB, Review)

#create OD csv file 1
write_csv(child_dev_hb, glue("{childhealth_filepath}/{child_dev_dat}_hb_{date}.csv"), na = "")


###HSCP file
child_dev_hscp <- child_dev %>%
  #filter for hscps
  filter(area_name %in% hscp_codes$area_name) %>%
  rename("HSCP" = "Code")%>%
  mutate(HSCP = if_else(
    area_name == "Argyll and Bute", "S37000004",
    if_else(area_name == "Clackmannanshire and Stirling", "S37000005",
            if_else(area_name == "Dumfries and Galloway", "S37000006",
                    if_else(area_name == "Perth and Kinross", "S37000033", HSCP)))
  ))%>%
  #rename, add qualifiers
  select(Month, HSCP, Review, NumberReviews, MeaningfulReviews, PercentMeaningful,
         Concerns, PercentConcerns)%>%
  arrange(Month, HSCP, Review)

#create OD csv file 3
write_csv(child_dev_hscp, glue("{childhealth_filepath}/{child_dev_dat}_hscp_{date}.csv"), na = "")



##############################################.
#11##Create Cancer OD Resources----
##############################################.

cancer_dat <- "cancer_widerimpacts"

#read in data
cancer <- read_csv(glue("{cancer_filepath}/{cancer_dat}.csv"))

#add geo codes to source data
cancer <- left_join(cancer, geo_codes, by = c("Area name" = "area_name"))

#general formatting

cancer <- cancer %>%
  rename("CancerType" = "Cancer type",
         "IndividualPathologies2019" = "Count 2019",
         "IndividualPathologies2020" = "Count 2020",
         "PercentVariation" = "Variation (%)",
         "WeekEnding" = "Week ending",
         "Area_name" = "Area name")%>%
  mutate(WeekEnding = strftime(WeekEnding, format = "%Y%m%d"))

#remove NA-s
cancer[is.na(cancer)] <- ""

###HB file

cancer_hb <- cancer %>%
  #filter for hb and scotland
  filter(Area_name %in% c(hb_codes$area_name, "Scotland", "NHS Ayrshire & Arran",
                          "NHS Dumfries & Galloway", "NHS Greater Glasgow & Clyde")) %>%
  rename("HB" = "Code") %>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  mutate(CancerTypeQF = if_else(CancerType == "All Cancers", "d", "")) %>%
  mutate(SexQF = if_else(Sex == "All Persons", "d", "")) %>%
  mutate(DifferenceQF = if_else(Difference == "", "z", "")) %>%
  mutate(PercentVariationQF = if_else(PercentVariation == "", "z", "")) %>%
  select(WeekEnding, HB, HBQF, Sex, SexQF, CancerType, CancerTypeQF,
         IndividualPathologies2019, IndividualPathologies2020, Difference, DifferenceQF, 
         PercentVariation, PercentVariationQF)%>%
  arrange(WeekEnding, HB, Sex, CancerType)

#create OD csv file 1
write_csv(cancer_hb, glue("{cancer_filepath}/{cancer_dat}_hb__{date}.csv"), na = "")


###Cancer Network Region file

cancer_cnr <- cancer %>%
  #filter for cnr
  filter(Area_name %in% c("NCA", "SCAN", "WOSCAN")) %>%
  rename("Region" = "Area_name") %>%
  #rename, add qualifiers
  mutate(CancerTypeQF = if_else(CancerType == "All Cancers", "d", "")) %>%
  mutate(SexQF = if_else(Sex == "All Persons", "d", "")) %>%
  mutate(DifferenceQF = if_else(Difference == "", "z", "")) %>%
  mutate(PercentVariationQF = if_else(PercentVariation == "", "z", "")) %>%
  select(WeekEnding, Region, Sex, SexQF, CancerType, CancerTypeQF,
         IndividualPathologies2019, IndividualPathologies2020, Difference, DifferenceQF, 
         PercentVariation, PercentVariationQF)%>%
  arrange(WeekEnding, Region, Sex, CancerType)

#create OD csv file 2
write_csv(cancer_cnr, glue("{cancer_filepath}/{cancer_dat}_cnr__{date}.csv"), na = "")

###Scotland file

cancer_scot <- cancer %>%
  #filter for Scotland
  filter(Area_name == "Scotland") %>%
  rename("Country" = "Code") %>%
  #rename, add qualifiers
  mutate(CancerTypeQF = if_else(CancerType == "All Cancers", "d", "")) %>%
  mutate(SexQF = if_else(Sex == "All Persons", "d", "")) %>%
  mutate(DifferenceQF = if_else(Difference == "", "z", "")) %>%
  mutate(PercentVariationQF = if_else(PercentVariation == "", "z", "")) %>%
  select(WeekEnding, Country, Sex, SexQF, CancerType, CancerTypeQF,
         IndividualPathologies2019, IndividualPathologies2020, Difference, DifferenceQF, 
         PercentVariation, PercentVariationQF)%>%
  arrange(WeekEnding, Sex, CancerType)

#create OD csv file 3
write_csv(cancer_scot, glue("{cancer_filepath}/{cancer_dat}_scot__{date}.csv"), na = "")



##############################################.
#12##Create Breastfeeding OD Resources----
##############################################.

breast_data <- "breastfeeding_data"

#read in data
breast_dat <- read_csv(glue("{breastfeeding_filepath}/{breast_data}.csv"))

# remove HSCP tag from Area_name and fix geo names discrepancies

breast_dat <- breast_dat %>%
  mutate(Area_name = if_else(substr(breast_dat$Area_name, (nchar(Area_name)-3), nchar(Area_name))== "HSCP",
                             substr(breast_dat$Area_name, 1, (nchar(Area_name) -5)), Area_name)) %>%
  mutate(Area_name = recode(Area_name, 
                            "Perth and Kinross" = "Perth & Kinross",
                            "Dumfries and Galloway" = "Dumfries & Galloway",
                            "NHS Clackmannanshire and Stirling" = "Clackmannanshire & Stirling",
                            "Argyll and Bute" = "Argyll & Bute"))


#add geo codes to source data
breast_dat <- left_join(breast_dat, geo_codes, by = c("Area_name" = "area_name"))

#general formatting

breast_dat <- breast_dat %>%
  rename("Month" = "Month_review",
         "NumberReviews" = "Number_reviews",
         "ValidReviews" = "Number_valid_reviews",
         "BreastfeedingExclusively" = "Exclusive_breastfeeding",
         "PcBreastfeedingExclusively" = "% Exclusive breastfeeding",
         "BreastfeedingOverall" = "Overall_breastfeeding",
         "PcBreastfeedingOverall" = "% Overall breastfeeding",
         "BreastfeedingEver" = "Ever_breastfeeding",
         "PcrBreastfeedingEver" = "% Ever breastfeeding")%>%
  mutate(Month = strftime(Month, format = "%Y%m"))

#remove NA-s
#cancer[is.na(cancer)] <- ""

###HB file

breast_hb <- breast_dat %>%
  #filter for hb and scotland
  filter(Area_name %in% c(hb_codes$area_name, "Scotland", "NHS Ayrshire & Arran",
                          "NHS Dumfries & Galloway", "NHS Greater Glasgow & Clyde")) %>%
  rename("HB" = "Code") %>%
  #rename, add qualifiers
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(Month, HB, HBQF, Review, NumberReviews, ValidReviews, 
         BreastfeedingExclusively, PcBreastfeedingExclusively,
         BreastfeedingOverall, PcBreastfeedingOverall, 
         BreastfeedingEver, PcBreastfeedingEver)%>%
  arrange(Month, HB, Review)

#create OD csv file 1
write_csv(breast_hb, glue("{breastfeeding_filepath}/{breast_data}_hb__{date}.csv"), na = "")


###hscp file

breast_hscp <- breast_dat %>%
  #filter for hscp
  filter(Area_name %in% c(hscp_codes$area_name, "Argyll & Bute", "Perth & Kinross",
                          "Dumfries & Galloway", "Clackmannanshire & Stirling")) %>%
  rename("HSCP" = "Code") %>%
  #rename, add qualifiers
  select(Month, HSCP, Review, NumberReviews, ValidReviews, 
         BreastfeedingExclusively, PcBreastfeedingExclusively,
         BreastfeedingOverall, PcBreastfeedingOverall, 
         BreastfeedingEver, PcBreastfeedingEver)%>%
  arrange(Month, HSCP, Review)

#create OD csv file 2
write_csv(breast_hscp, glue("{breastfeeding_filepath}/{breast_data}_hscp__{date}.csv"), na = "")

##############################################.
#13##Create Perinatal OD Resources----
##############################################.

perinatal <- perinatal %>%
  rename("Country" = "area_name",
         "Month" = "month_of_year",
         "NumberDeaths" = "number_of_deaths_in_month",
         "RelevantBirths" = "relevant_births",
         "Rate" = "rate",
         "Type" = "type") %>%
  mutate(Month = strftime(Month, format = "%Y%m")) %>%
  mutate(Country = recode(Country, "Scotland" = "S92000003")) %>%
  select(Country, Month, Type, NumberDeaths, Rate, 
         RelevantBirths)%>%
  arrange(Month, Type)

#create OD csv file 1
write_csv(perinatal, glue("{perinatal_filepath}/{perinatal_dat}_{date}.csv"), na = "")


##############################################.
#14##Create Pregnancy OD Resources----
##############################################.

#Termination files: HB, Age, Deprivation
#rename cols and format date

termination <- termination %>%
  rename("Month" = "termination_month",
         "AverageGestation" = "average_gestation_at_termination",
         "NumberTerminations" = "number_of_terminations",
         "NumberGestation10to12Wks" = "number_of_terminations_gest_10to12wks",
         "NumberGestationOver12Wks" = "number_of_terminations_gest_over_12wks",
         "NumberGestationUnder10Wks" = "number_of_terminations_gest_under_10wks")

term_cols <- c("AverageGestation", "NumberTerminations", 
               "NumberGestation10to12Wks", "NumberGestation10to12WksQF", 
               "NumberGestationOver12Wks", "NumberGestationOver12WksQF",
               "NumberGestationUnder10Wks", "NumberGestationUnder10WksQF")

create_maternity_open_data(termination, term_cols, "terminations_preg")

###INDUCTION FILES
#rename cols and format date

induction <- induction %>%
  rename("Month" = "month_of_discharge",
         "NumberInduced" = "Number of singleton live births at 37-42 weeks gestation that followed induction of labour",
         "NumberNotInduced" = "Number of singleton live births at 37-42 weeks gestation that were not induced",
         "NumberInductionUnknown" = "Number of singleton live births at 37-42 weeks gestation unknown",
         "PercentInduced" = "Percentage (%) of singleton live births at 37-42 weeks gestation that followed induction of labour",
         "PercentNotInduced" = "Percentage (%) of singleton live births at 37-42 weeks gestation that were not induced",
         "PercentUnknown" = "Percentage (%) of singleton live births at 37-42 weeks gestation unknown",
         "SingletonLiveBirths37to42Wks" = "Total number of singleton live births at 37-42 weeks gestation")

ind_cols <- c("SingletonLiveBirths37to42Wks",
              "NumberInduced", "NumberNotInduced", "NumberInductionUnknown", 
              "PercentInduced", "PercentNotInduced", "PercentUnknown")

create_maternity_open_data(induction, ind_cols, "induction_labour")

###Gestation FILES
#rename cols and format date

gestation <- gestation %>%
  rename("Month" = "month_of_discharge",
         "AllBirths" = "Number of births - All births",
         "Gestation32to36Wks" = "Number of births - 32-36 weeks gestation",
         "Gestation37to41Wks" = "Number of births - 37-41 weeks gestation",
         "Gestation42WksOrOver" = "Number of births - At or over 42 weeks gestation",
         "GestationUnder32Wks" = "Number of births - Under 32 weeks gestation",
         "GestationUnder37Wks" = "Number of births - Under 37 weeks gestation",
         "Gestation18to44Wks" = "Number of births - All births (18-44 weeks gestation)",
         "GestationUnknown" = "Number of births - Unknown gestation",
         "Percent32to36Wks" = "Percentage (%) of births - 32-36 weeks gestation",
         "Percent37to41Wks" = "Percentage (%) of births - 37-41 weeks gestation",
         "Percent42WksOrOver" = "Percentage (%) of births - At or over 42 weeks gestation",
         "PercentUnder32Wks" = "Percentage (%) of births - Under 32 weeks gestation",
         "PercentUnder37Wks" = "Percentage (%) of births - Under 37 weeks gestation") 

gest_cols <- c("AllBirths", "Gestation32to36Wks", "Gestation37to41Wks",
               "Gestation42WksOrOver", "GestationUnder32Wks", "GestationUnder37Wks", 
               "Gestation18to44Wks", "GestationUnknown", "Percent32to36Wks",
               "Percent37to41Wks", "Percent42WksOrOver", "PercentUnder32Wks",
               "PercentUnder37Wks")

create_maternity_open_data(gestation, gest_cols, "gestation")

###Delivery FILES
#rename cols and format date

delivery <- delivery %>%
  rename("Month" = "month_of_discharge",
         "AssistedVaginal" = "Number of births - assisted vaginal delivery including breech",
         "CSectionAll" = "Number of births - Caesarean section",
         "CSectionElected" = "Number of births - elective Caesarean section",
         "CSectionEmergency" = "Number of births - emergency Caesarean section",
         "SpontaneousVaginal" = "Number of births - spontaneous_vaginal_delivery",
         "OtherNotKnown" = "Number of births - Other/Not Known",
         "AllBirths" = "Number of births - All births",
         "PercentAssistedVaginal" = "Percentage (%) of births - assisted vaginal delivery including breech",
         "PercentCSectionAll" = "Percentage (%) of births - Caesarean section",
         "PercentCSectionElected" = "Percentage (%) of births - elective Caesarean section",
         "PercentCSectionEmergency" = "Percentage (%) of births - emergency Caesarean section",
         "PercentSpontaneousVaginal" = "Percentage (%) of births - spontaneous vaginal delivery",
         "PercentOtherNotKnown" = "Percentage (%) of births - other/not known")

delivery_cols <- c("AllBirths", "SpontaneousVaginal", "AssistedVaginal", 
                   "CSectionAll", "CSectionElected", "CSectionEmergency", "OtherNotKnown",
                   "PercentSpontaneousVaginal", "PercentAssistedVaginal", "PercentCSectionAll",
                   "PercentCSectionElected", "PercentCSectionEmergency", "PercentOtherNotKnown")

create_maternity_open_data(delivery, delivery_cols, "method_delivery")

## ANTENATAL  files:
# HB by week
# HB by month
# SIMD by week
# Age by week

#rename cols and format date

antenatal <- antenatal %>%
  rename("Month" = "booking_month",
         "WeekBeginning" = "booking_week_beginning",
         "NumberWomenBooking" = "number_of_women_booking",
         "NumberGestation10to12Wks" = "number_of_women_booking_gest_10to12wks",
         "NumberGestationUnder10Wks" = "number_of_women_booking_gest_under_10wks",
         "NumberGestationOver12Wks" = "number_of_women_booking_gest_over_12wks") %>%
  mutate(WeekBeginning = strftime(WeekBeginning, format = "%Y%m%d")) %>%
  mutate(Month = paste0(substr(Month, 1, 4), substr(Month, 6, 7)))


# # HB file by month
# antenatal_hb <- antenatal %>%
#   filter(Month != "NANA")%>%
#   rename("HB" = "Code") %>%
#   mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
#   select(HB, HBQF, Month, NumberWomenBooking)%>%
#   arrange(Month, HB)
# 
# #create OD csv file 1
# write_csv(antenatal_hb, glue("{pregnancy_filepath}/{antenatal_dat}_hb_month_{date}.csv"), na = "")

# HB weekly
antenatal_hb_week <- antenatal %>%
  filter(category == "All", !is.na(WeekBeginning))%>%
  rename("HB" = "Code") %>%
  mutate(HBQF = if_else(HB == "S92000003", "d", "")) %>%
  select(HB, HBQF, WeekBeginning, NumberWomenBooking, 
         NumberGestation10to12Wks,NumberGestationOver12Wks,
         NumberGestationUnder10Wks)%>%
  arrange(WeekBeginning, HB)

#create OD csv file 2
write_csv(antenatal_hb_week, glue("{pregnancy_filepath}/{antenatal_dat}_hb_week_{date}.csv"), na = "")

# Age file
antenatal_age <- antenatal %>%
  filter(category %in% c("Under 20", "20-24", "25-29", "30-34",
                         "35-39", "40 and over"), !is.na(WeekBeginning))%>%
  rename("Country" = "Code",
         "AgeGroup" = "category") %>%
  select(Country, WeekBeginning, AgeGroup, NumberWomenBooking,
         NumberGestation10to12Wks,NumberGestationOver12Wks,
         NumberGestationUnder10Wks)%>%
  arrange(WeekBeginning, AgeGroup)


#create OD csv file 3
write_csv(antenatal_age, glue("{pregnancy_filepath}/{antenatal_dat}_age_{date}.csv"), na = "")


# SIMD file
antenatal_simd <- antenatal %>%
  filter(category %in% c("1 - most deprived", "2", "3", "4", "5 - least deprived"),  !is.na(WeekBeginning))%>%
  rename("Country" = "Code",
         "SIMDQuintile" = "category") %>%
  mutate(SIMDQuintile = recode(SIMDQuintile, 
                               "1 - most deprived" = "1",
                               "5 - least deprived" = "5")) %>%
  select(Country, WeekBeginning, SIMDQuintile, NumberWomenBooking,
         NumberGestation10to12Wks,NumberGestationOver12Wks,
         NumberGestationUnder10Wks)%>%
  arrange(WeekBeginning, SIMDQuintile)

#create OD csv file 3
write_csv(antenatal_simd, glue("{pregnancy_filepath}/{antenatal_dat}_simd_{date}.csv"), na = "")