#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Cancer Pathology Data prep for
# PHS COVID-Wider-Impact dashboard ----
# 
#
# M.Turner - Cancer Waits Team
# murdo.turner@phs.scot
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Housekeeping ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(tidyverse)||install.packages("tidyverse")
require(remotes)||install.packages("remotes")
require(janitor)||install.packages("janitor")
require(lubridate)||install.packages("lubridate")
require(writexl)||install.packages("writexl")
require(tidylog)||install.packages("tidylog")
require(naniar)||install.packages("naniar")

remotes::install_local("/PHI_conf/CancerGroup1/Topics/CancerStatistics/Projects/20200804-pathology-as-proxy-for-2020-regs/RShiny/CancerPathologyDashboard/Cancer Pathology Jan 22 update/phsmethods-master.zip",
                       upgrade = "never"
)

# remotes::install_github("Public-Health-Scotland/phsmethods", upgrade = "never")
library(phsmethods)




# Helper function
`%notin%` <- Negate(`%in%`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Import Data and rename variables ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


input_folder <- paste0("////PHI_conf//CancerGroup1//Topics//CancerStatistics//Projects",
                       "//20200804-pathology-as-proxy-for-2020-regs//RShiny//CancerPathologyData//")
cl_out <- "/conf/linkage/output/lookups/Unicode/"

cancer <- read_csv(paste0(input_folder,"Pathology_Data_Mar_22.csv"), col_names = T) %>%  
  clean_names() %>% 
  select(year:data_source, icd10_conv, person_id:chi_number, sex:postcode) %>%
  mutate(incidence_date = dmy(incidence_date)) %>%
  mutate(chi_number = replace_na(chi_number, "0")) 

cancer2017_18 <- read_csv(paste0(input_folder,"2017_2018 Covid source data pathology detail.csv"), col_names = T) %>%
  clean_names() %>%
  select(year:data_source, icd10_conv, person_id:chi_number, sex:postcode) %>%
  mutate(incidence_date = dmy(incidence_date)) %>%
  mutate(chi_number = replace_na(chi_number, 0)) %>%
  mutate(chi_number = as.character(chi_number)) %>%
  mutate(chi_number = chi_pad(chi_number)) %>% 
  mutate(derived_upi = as.character(derived_upi))

cancer <- bind_rows(cancer, cancer2017_18)

rm(cancer2017_18)


# Allocate records to Quarter groupings
cancer <- cancer %>% 
  mutate(quarter = qtr(incidence_date)) %>% 
  mutate(quarter = case_when(str_detect(quarter, "January") ~ "Jan-Mar 20",
                             str_detect(quarter, "April") ~ "Apr-Jun 20",
                             str_detect(quarter, "July") ~ "Jul-Sep 20",
                             str_detect(quarter, "October") ~ "Oct-Dec 20")) 


# import deprivation lookup
depriv_dir <- readRDS(paste0(cl_out,"Deprivation/postcode_2021_2_simd2020v2.rds")) %>%
  clean_names() %>%
  select(pc8, hb2019name, simd2020v2_sc_quintile) %>%
  rename(postcode = pc8, hbres = hb2019name, dep = simd2020v2_sc_quintile) %>% 
  mutate(postcode = postcode(postcode, format = 'pc8')) 


# allocate to ICD10 site
cancer <- cancer %>% 
  mutate(site10 = substring(icd10_conv, 1, 3))

icd_cancer <- sprintf('C%02d', seq(00,97))

# filter & recode sex values
cancer <- cancer %>%
  filter(site10 %in% icd_cancer) %>% 
  filter(sex %in% c(1,2)) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Classify by Cancer Sites----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cancer <- cancer %>% 
  mutate(siteno = case_when(str_detect(site10, "C67")  ~ 210,
                            str_detect(site10, "C50")  ~ 510,
                            str_detect(site10, "C56")  ~ 740,
                            str_detect(site10, "C53")  ~ 755,
                            str_detect(site10, "C52")  ~ 760,
                            str_detect(site10, "C51")  ~ 770,
                            str_detect(site10, "C73")  ~ 870,
                            str_detect(site10, "C81")  ~ 910,
                            str_detect(site10, "C22")  ~ 1210,
                            str_detect(site10, "C45")  ~ 1320,
                            str_detect(site10, "C60")  ~ 1410,
                            str_detect(site10, "C61")  ~ 1420,
                            str_detect(site10, "C62")  ~ 1430,
                            str_detect(site10, "C90")  ~ 1510,
                            str_detect(site10, "C15")  ~ 1710,
                            str_detect(site10, "C25")  ~ 1810,
                            str_detect(site10, "C43")  ~ 1910,
                            str_detect(site10, "C44")  ~ 1920,
                            str_detect(site10, "C16")  ~ 2010,
                            str_detect(site10, "C40") | 
                              str_detect(site10, "C41") | 
                              str_detect(site10, "C47") |
                              str_detect(site10, "C49")  ~ 320,
                            str_detect(site10, "C71") ~ 410,
                            str_detect(site10, "C17") | 
                              str_detect(site10, "C18") | 
                              str_detect(site10, "C19") | 
                              str_detect(site10, "C20") ~ 610,
                            str_detect(site10, "C54") | 
                              str_detect(site10, "C55") ~ 750,
                            str_detect(site10, "C0") | 
                              str_detect(site10, "C10") | 
                              str_detect(site10, "C11") | 
                              str_detect(site10, "C12") | 
                              str_detect(site10, "C13") | 
                              str_detect(site10, "C14") |
                              str_detect(site10, "C30") |
                              str_detect(site10, "C31") | 
                              str_detect(site10, "C32") ~ 810,
                            str_detect(site10, "C64") | 
                              str_detect(site10, "C65") ~ 1010,
                            str_detect(site10, "C91") | 
                              str_detect(site10, "C92") | 
                              str_detect(site10, "C93") | 
                              str_detect(site10, "C94") ~ 1110,
                            str_detect(site10, "C33") | 
                              str_detect(site10, "C34") ~ 1310,
                            str_detect(site10, "C82") |
                              str_detect(site10, "C83") |
                              str_detect(site10, "C84") |
                              str_detect(site10, "C85") |
                              str_detect(site10, "C86") ~ 1610,
                            str_detect(site10, "C") ~ 999))



# add cancer description

cancer <- cancer %>%
  filter(!(is.na(siteno))) %>% 
  mutate(site = case_when(siteno == 210 ~ "Bladder",
                          siteno == 320 ~ "Bone and Connective Tissue",
                          siteno == 410 ~ "Brain Tumour",
                          siteno == 510 ~ "Breast",
                          siteno == 610 ~ "Colorectal",
                          siteno == 740 ~ "Ovary - Females only",
                          siteno == 750 ~ "Uterus - Females only",
                          siteno == 755 ~ "Cervical - Females only",
                          siteno == 760 ~ "Vagina - Females only",
                          siteno == 770 ~ "Vulva - Females only",
                          siteno == 810 ~ "Head and Neck",
                          siteno == 870 ~ "Thyroid",
                          siteno == 910 ~ "Hodgkin Lymphoma",
                          siteno == 1010 ~ "Kidney",
                          siteno == 1110 ~ "Leukaemias",
                          siteno == 1210 ~ "Liver and Intrahepatic Bile Ducts",
                          siteno == 1310 ~ "Trachea, Bronchus and Lung",
                          siteno == 1320 ~ "Mesothelioma",
                          siteno == 1410 ~ "Penis - Males only",
                          siteno == 1420 ~ "Prostate - Males only",
                          siteno == 1430 ~ "Testis - Males only",
                          siteno == 1510 ~ "Multiple Myeloma and malignant plasma cell neoplasms",
                          siteno == 1610 ~ "Non-Hodgkin Lymphoma",
                          siteno == 1710 ~ "Oesophagus",
                          siteno == 1810 ~ "Pancreas",
                          siteno == 1910 ~ "Malignant Melanoma of the Skin",
                          siteno == 1920 ~ "Non-Melanoma Skin Cancer",
                          siteno == 2010 ~ "Stomach",
                          siteno == 999 ~ "Other"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Exclude unreliable records and set time period----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# exclude records where DOB = NA 
cancer <- cancer %>%
  filter(!is.na(date_of_birth)) 

# filter impossible sex/cancer combos
cancer <- cancer %>% 
  filter(!(sex == 2 & site10 %in% sprintf('C%02d', seq(60, 63)))) %>% 
  filter(!(sex == 1 & site10 %in% sprintf('C%02d', seq(51, 58))))

# fix incorrect week numbers (2021 only) and include data to last week
# of complete data (check this with DM)
cancer <- cancer %>%
  mutate(week_number = case_when(year == 2021 & week_number == 53 ~ 1,
                                 TRUE ~ week_number)) %>% 
  # mutate(week_number = case_when(year == 2020 & week_number == 53 ~ 52,
  #                                TRUE ~ week_number)) %>%
  filter(!(year == 2021 & week_number > 45))


# extract invalid age records
cancer <- cancer %>% 
  mutate(dob = dmy(date_of_birth), 
         doi = incidence_date,
         age = floor(difftime(doi, dob, units = "weeks")/52.25)) %>% 
  filter((age >= 0 & age < 130) & dob <= doi)

# create age group column
cancer <- cancer %>%
  mutate(age_group = case_when(between(age, 0, 49) ~ "0 - 49",
                               between(age, 50, 69) ~ "50 - 69",
                               age > 69  ~ "70+"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Allocate records to HBs and networks ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Format postcode 
# Firstly, correct situations where the Postcode District is denoted as '01' to '09', 
# e.g. G01, EH01 or there is a space after Postcode Area, e.g. "G 1" or "EH 1"
# for checking
cancer <- cancer %>% 
  mutate(original_postcode = postcode)

cancer <- cancer %>%
  mutate(postcode = toupper(postcode)) %>%
  mutate(postcode = case_when(
    substr(postcode, 1, 1) %in% LETTERS & substr(postcode, 2, 2) %in% c('0', ' ') ~
      paste0(substr(postcode, 1, 1), substr(postcode, 3, 8)),
    substr(postcode, 2, 2) %in% LETTERS & substr(postcode, 3, 3) %in% c('0', ' ')~
      paste0(substr(postcode, 1, 2), substr(postcode, 4, 8)),
    TRUE ~ postcode))

# Convert 8-character postcodes to 7-character postcodes.
# 'pc7' always assumes a 7 character length and there could be zero, one or two spaces 
# between Postcode District and Postcode Sector / Walk.


cancer <- cancer %>%
  mutate(postcode = case_when(
    str_length(postcode) == 8 ~ paste0(substr(postcode, 1, 4), substr(postcode, 6, 8)),
    str_length(postcode) == 7 ~ postcode,
    str_length(postcode) == 6 ~ paste0(substr(postcode, 1, 3), ' ', substr(postcode, 4, 6)),
    str_length(postcode) == 5 ~ paste0(substr(postcode, 1, 2), '  ', substr(postcode, 3, 5)),
    TRUE ~ postcode))

# VARIABLE LABELS pc7 'Postcode of Residence (7 character format)'.
#
# 'pc8' works in a strange way, always assuming only one space between Postcode District and 
# Postcode Sector / Walk.
#
cancer <- cancer %>%
  mutate(postcode = case_when(
    substr(postcode, 5, 5) == 'O' ~ paste0(substr(postcode, 1, 4), '0', substring(postcode, 6)),
    substr(postcode, 5, 5) == 'I' ~ paste0(substr(postcode, 1, 4), '1', substring(postcode, 6)),
    TRUE ~ postcode))

#
# VARIABLE LABELS pc8 'Postcode of Residence (8 character format)'.

cancer <- cancer %>%
  mutate(postcode = case_when(
    substr(postcode, 4, 4) != ' ' & substr(postcode, 5, 5) != ' ' & substr(postcode,7,7) != ' ' ~
      paste0(substr(postcode,1,4), " ", substr(postcode, 5, 7)),
    TRUE ~ postcode)) %>%
  mutate(postcode = case_when(
    substr(postcode, 3, 4) == '  ' & substr(postcode, 5, 5) != ' ' & substr(postcode,7,7) != ' ' ~
      paste0(substr(postcode,1,2), " ", substr(postcode, 5, 7)),
    TRUE ~ postcode))

#format postcode to 8 digit
cancer <- cancer %>%
  mutate(postcode = postcode(postcode, format = "pc8"))

### get Health Boards of residence and deprivation quintile rank from postcodes

cancer <- cancer %>% 
  left_join(depriv_dir) %>%
  filter(!(is.na(dep))) %>% 
  mutate(region = "Health Boards")

rm(depriv_dir)

# Change the health board labels

cancer$hbres <- recode(cancer$hbres, 
                       "NHS Ayrshire and Arran" = "NHS Ayrshire & Arran", 
                       "NHS Dumfries and Galloway" = "NHS Dumfries & Galloway", 
                       "NHS Greater Glasgow and Clyde" = "NHS Greater Glasgow & Clyde") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEFINE BASE DATASETS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# need datasets for: - All cancer (Scotland, Networks, HBs)
#                    - All cancer xNMSC (Scotland, Networks, HBs)
#                    - All individual sites (Scotland, Networks, HBs)
#
# 1. modify original dataset for different site groupings

cancer_all <- cancer %>% 
  mutate(site = "All Cancers")

cancer_xnmsc <- cancer %>% 
  filter(site != "Non-Melanoma Skin Cancer") %>% 
  mutate(site = "All Malignant Neoplasms (Excl. C44)")

cancer_sites <- cancer

# 2. Modify each of above for area groupings

## 2.1 Scotland:

cancer_all_scotland <- cancer_all %>% 
  mutate(hbres = "Scotland", region = "Scotland")

cancer_xnmsc_scotland <- cancer_xnmsc %>% 
  mutate(hbres = "Scotland", region = "Scotland")

cancer_sites_scotland <- cancer_sites %>% 
  mutate(hbres = "Scotland", region = "Scotland")

## 2.2 Networks

cancer_networks <- cancer %>%
  mutate(hbres = case_when(hbres == "NHS Grampian" ~ "NCA",
                           hbres == "NHS Highland" ~ "NCA",
                           hbres == "NHS Orkney" ~ "NCA",
                           hbres == "NHS Shetland" ~ "NCA",
                           hbres == "NHS Tayside" ~ "NCA",
                           hbres == "NHS Western Isles" ~ "NCA",
                           hbres == "NHS Borders" ~ "SCAN",
                           hbres == "NHS Dumfries & Galloway" ~ "SCAN",
                           hbres == "NHS Fife" ~ "SCAN",
                           hbres == "NHS Lothian" ~ "SCAN",
                           hbres == "NHS Ayrshire & Arran" ~ "WOSCAN",
                           hbres == "NHS Forth Valley" ~ "WOSCAN",
                           hbres == "NHS Greater Glasgow & Clyde" ~ "WOSCAN",
                           hbres == "NHS Lanarkshire" ~ "WOSCAN",
                           TRUE ~ hbres)) %>% 
  mutate(region = "Cancer Networks")

cancer_all_networks <- cancer_networks %>% 
  mutate(site = "All Cancers")

cancer_xnmsc_networks <- cancer_networks %>%
  filter(site != "Non-Melanoma Skin Cancer") %>% 
  mutate(site = "All Malignant Neoplasms (Excl. C44)")

cancer_sites_networks <- cancer_networks 

## 2.3 Health Boards

cancer_all_hb <- cancer %>% 
  mutate(site = "All Cancers")

cancer_xnmsc_hb <- cancer_xnmsc 

cancer_sites_hb <- cancer

rm(cancer)


################# SCOTLAND DATASETS - ALL CANCER ##################################################

# Sort total Scotland numbers by person ID, one per year 

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_all_scotland_dupes <- cancer_all_scotland %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_all_scotland_dupes_id <- cancer_all_scotland_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_all_scotland_not_dupes <- anti_join(cancer_all_scotland, cancer_all_scotland_dupes_id)

# add duplicate and non-duplicate records: 
cancer_scotland_all <- bind_rows(cancer_all_scotland_not_dupes, cancer_all_scotland_dupes)

rm(cancer_all_scotland_dupes, cancer_all_scotland_dupes_id, cancer_all_scotland_not_dupes)

# - both sexes
cancer_scotland_all_allsex <- cancer_scotland_all %>%   
  mutate(sex = 3)


################# SCOTLAND DATASETS - ALL CANCER(xNMSC) ##################################################

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_xnmsc_scotland_dupes <- cancer_xnmsc_scotland %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_xnmsc_scotland_dupes_id <- cancer_xnmsc_scotland_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_xnmsc_scotland_not_dupes <- anti_join(cancer_xnmsc_scotland, cancer_xnmsc_scotland_dupes_id)

# add duplicate and non-duplicate records: 
cancer_scotland_xnmsc <- bind_rows(cancer_xnmsc_scotland_not_dupes, cancer_xnmsc_scotland_dupes)

rm(cancer_xnmsc_scotland_dupes, cancer_xnmsc_scotland_dupes_id, cancer_xnmsc_scotland_not_dupes)

# - both sexes
cancer_scotland_xnmsc_allsex <- cancer_scotland_xnmsc %>%   
  mutate(sex = 3)

################# SCOTLAND DATASETS - ALL SITES ##################################################

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_sites_scotland_dupes <- cancer_sites_scotland %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, site, incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year, site) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_sites_scotland_dupes_id <- cancer_sites_scotland_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_sites_scotland_not_dupes <- anti_join(cancer_sites_scotland, cancer_sites_scotland_dupes_id)

# add duplicate and non-duplicate records: 
cancer_scotland_sites <- bind_rows(cancer_sites_scotland_not_dupes, cancer_sites_scotland_dupes)

rm(cancer_sites_scotland_dupes, cancer_sites_scotland_dupes_id, cancer_sites_scotland_not_dupes)

# - both sexes
cancer_scotland_sites_allsex <- cancer_scotland_sites %>%   
  mutate(sex = 3)


################# NETWORK DATASETS - ALL CANCER ##################################################

# Sort total network numbers by person ID, one per year 

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_all_networks_dupes <- cancer_all_networks %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, hbres, incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year, hbres) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_all_networks_dupes_id <- cancer_all_networks_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_all_networks_not_dupes <- anti_join(cancer_all_networks, cancer_all_networks_dupes_id)

# add duplicate and non-duplicate records: 
cancer_networks_all <- bind_rows(cancer_all_networks_not_dupes, cancer_all_networks_dupes)

rm(cancer_all_networks_dupes, cancer_all_networks_dupes_id, cancer_all_networks_not_dupes)

# - both sexes
cancer_networks_all_allsex <- cancer_networks_all %>%   
  mutate(sex = 3)


################# NETWORK DATASETS - ALL cancer(xNMSC) ##################################################

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_xnmsc_networks_dupes <- cancer_xnmsc_networks %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, hbres, incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year, hbres) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_xnmsc_networks_dupes_id <- cancer_xnmsc_networks_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_xnmsc_networks_not_dupes <- anti_join(cancer_xnmsc_networks, cancer_xnmsc_networks_dupes_id)

# add duplicate and non-duplicate records: 
cancer_networks_xnmsc <- bind_rows(cancer_xnmsc_networks_not_dupes, cancer_xnmsc_networks_dupes)

rm(cancer_xnmsc_networks_dupes, cancer_xnmsc_networks_dupes_id, cancer_xnmsc_networks_not_dupes)

# - both sexes
cancer_networks_xnmsc_allsex <- cancer_networks_xnmsc %>%   
  mutate(sex = 3)

################# NETWORK DATASETS - ALL SITES ##################################################

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_sites_networks_dupes <- cancer_sites_networks %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, hbres, site,  incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year, hbres, site) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_sites_networks_dupes_id <- cancer_sites_networks_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_sites_networks_not_dupes <- anti_join(cancer_sites_networks, cancer_sites_networks_dupes_id)

# add duplicate and non-duplicate records: 
cancer_networks_sites <- bind_rows(cancer_sites_networks_not_dupes, cancer_sites_networks_dupes)

rm(cancer_sites_networks_dupes, cancer_sites_networks_dupes_id, cancer_sites_networks_not_dupes)

# - both sexes
cancer_networks_sites_allsex <- cancer_networks_sites %>%   
  mutate(sex = 3)


################# HEALTH BOARD DATASETS - ALL CANCER #######################################################

# Sort total network numbers by person ID, one per year 

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_all_hb_dupes <- cancer_all_hb %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, hbres, incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year, hbres) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_all_hb_dupes_id <- cancer_all_hb_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_all_hb_not_dupes <- anti_join(cancer_all_hb, cancer_all_hb_dupes_id)

# add duplicate and non-duplicate records: 
cancer_hb_all <- bind_rows(cancer_all_hb_not_dupes, cancer_all_hb_dupes)

rm(cancer_all_hb_dupes, cancer_all_hb_dupes_id, cancer_all_hb_not_dupes)

# - both sexes
cancer_hb_all_allsex <- cancer_hb_all %>%   
  mutate(sex = 3)


################# HEALTH BOARD DATASETS - ALL CANCER(xNMSC) ##################################################

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_xnmsc_hb_dupes <- cancer_xnmsc_hb %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, hbres, incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year, hbres) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_xnmsc_hb_dupes_id <- cancer_xnmsc_hb_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_xnmsc_hb_not_dupes <- anti_join(cancer_xnmsc_hb, cancer_xnmsc_hb_dupes_id)

# add duplicate and non-duplicate records: 
cancer_hb_xnmsc <- bind_rows(cancer_xnmsc_hb_not_dupes, cancer_xnmsc_hb_dupes)

rm(cancer_xnmsc_hb_dupes, cancer_xnmsc_hb_dupes_id, cancer_xnmsc_hb_not_dupes)

# - both sexes
cancer_hb_xnmsc_allsex <- cancer_hb_xnmsc %>%   
  mutate(sex = 3)

################# HEALTH BOARD DATASETS - ALL SITES ##################################################

# get duplicates for person id, remove NA postcodes and select first for each ID
cancer_sites_hb_dupes <- cancer_sites_hb %>% 
  get_dupes(person_id) %>%
  arrange(person_id, year, hbres, site, incidence_date, derived_upi, postcode) %>%
  filter(!(is.na(postcode))) %>% 
  group_by(person_id, year, hbres, site) %>%
  slice(1L) %>% 
  ungroup()

# create list of person id values of duplicates
cancer_sites_hb_dupes_id <- cancer_sites_hb_dupes %>% 
  select(person_id)

# take duplicate IDs from total cancer dataset
cancer_sites_hb_not_dupes <- anti_join(cancer_sites_hb, cancer_sites_hb_dupes_id)

# add duplicate and non-duplicate records: 
cancer_hb_sites <- bind_rows(cancer_sites_hb_not_dupes, cancer_sites_hb_dupes)

rm(cancer_sites_hb_dupes, cancer_sites_hb_dupes_id, cancer_sites_hb_not_dupes)

# - both sexes
cancer_hb_sites_allsex <- cancer_hb_sites %>%   
  mutate(sex = 3)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Combine datasets for each area/site ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

base_cancer <- bind_rows(cancer_scotland_all,
                         cancer_scotland_all_allsex,
                         cancer_scotland_xnmsc,
                         cancer_scotland_xnmsc_allsex,
                         cancer_scotland_sites,
                         cancer_scotland_sites_allsex,
                         cancer_networks_all,
                         cancer_networks_all_allsex,
                         cancer_networks_xnmsc,
                         cancer_networks_xnmsc_allsex,
                         cancer_networks_sites,
                         cancer_networks_sites_allsex,
                         cancer_hb_all,
                         cancer_hb_all_allsex,
                         cancer_hb_xnmsc,
                         cancer_hb_xnmsc_allsex,
                         cancer_hb_sites,
                         cancer_hb_sites_allsex)

rm(cancer_scotland_all,
   cancer_scotland_all_allsex,
   cancer_scotland_xnmsc,
   cancer_scotland_xnmsc_allsex,
   cancer_scotland_sites,
   cancer_scotland_sites_allsex,
   cancer_networks_all,
   cancer_networks_all_allsex,
   cancer_networks_xnmsc,
   cancer_networks_xnmsc_allsex,
   cancer_networks_sites,
   cancer_networks_sites_allsex,
   cancer_hb_all,
   cancer_hb_all_allsex,
   cancer_hb_xnmsc,
   cancer_hb_xnmsc_allsex,
   cancer_hb_sites,
   cancer_hb_sites_allsex)

rm(cancer_all,
   cancer_all_hb,
   cancer_all_networks,
   cancer_all_scotland,
   cancer_networks,
   cancer_sites,
   cancer_sites_hb,
   cancer_sites_networks,
   cancer_sites_scotland,
   cancer_xnmsc,
   cancer_xnmsc_hb,
   cancer_xnmsc_networks,
   cancer_xnmsc_scotland)


# Sex labels
base_cancer <- base_cancer %>% 
  mutate(sex = factor(sex, levels = c(1,2, 3), 
                      labels = c("Male", "Female", "All")))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BASE DATASET ----
#
# This is the dataset to derive all aggregated counts from 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

base_cancer_slim <- base_cancer %>% 
  select(-c(data_source, icd10_conv, chi_number, date_of_birth,postcode,  site10,
            siteno, dob:age, original_postcode, dupe_count))

rm(base_cancer)
gc()


#### SET Q4 IN EACH YEAR TO QUARTER 0 FOR DIFFERENCE GRAPH

base_cancer_slim_q0 <- base_cancer_slim %>%
  filter(quarter == "Oct-Dec 20") %>% 
  mutate(year = case_when(year == 2017 ~ 2018,
                          year == 2018 ~ 2019,
                          year == 2019 ~ 2020,
                          year == 2020 ~ 2021)) %>% 
  mutate(quarter = "Oct-Dec 19")

base_cancer_slim_quarters <- bind_rows(base_cancer_slim, base_cancer_slim_q0) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get weekly counts for each value of hbres and All cancer ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


base_cancer_counts <- base_cancer_slim %>% 
  group_by(year,  week_number, region, hbres, site, sex) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  complete(week_number, nesting(year,  region, hbres, site, sex), fill = list(count = 0)) %>%
  pivot_wider(names_from = year, 
              values_from = count,
              values_fill = list(count = 0)) %>% 
  rename(area = hbres, count17 = "2017",count18 = "2018", count19 = "2019", 
         count20 = "2020", count21 = "2021") %>% 
  mutate(age_group = "All Ages", dep = 0, breakdown = "None") 

base_cancer_counts19_wk53 <- base_cancer_counts %>% 
  filter(week_number == 53) %>% 
  mutate(count17 = NA, count18 = NA, count19 = NA)

base_cancer_counts19_notwk53 <- base_cancer_counts %>% 
  filter(week_number != 53)

base_cancer_counts <- bind_rows(base_cancer_counts19_notwk53, base_cancer_counts19_wk53) 

rm(base_cancer_counts19_notwk53, base_cancer_counts19_wk53)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get weekly counts for each value of hbres and All cancer by age group----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

base_cancer_counts_agegroups <- base_cancer_slim %>% 
  group_by(year, week_number, region, hbres, site, sex, age_group) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  complete(week_number, nesting(year, region, hbres, site, sex, age_group), 
           fill = list(count = 0)) %>%
  pivot_wider(names_from = year, 
              values_from = count,
              values_fill = list(count = 0)) %>% 
  rename(area = hbres, count17 = "2017", count18 = "2018", count19 = "2019", 
         count20 = "2020", count21 = "2021") %>% 
  mutate(dep = 0, breakdown = "Age Group")

base_cancer_counts_agegroups_19_wk53 <- base_cancer_counts_agegroups %>% 
  filter(week_number == 53) %>% 
  mutate(count17 = NA, count18 = NA, count19 = NA)

base_cancer_counts_agegroups_19_notwk53 <- base_cancer_counts_agegroups %>% 
  filter(week_number != 53)

base_cancer_counts_agegroups <- bind_rows(base_cancer_counts_agegroups_19_notwk53, 
                                          base_cancer_counts_agegroups_19_wk53) 

rm(base_cancer_counts_agegroups_19_notwk53, base_cancer_counts_agegroups_19_wk53)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get weekly counts for each value of hbres and All cancer by deprivation ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

base_cancer_counts_dep <- base_cancer_slim %>% 
  group_by(year, week_number, region, hbres, site, sex, dep) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  complete(week_number, nesting(year, region, hbres, site, sex, dep), 
           fill = list(count = 0)) %>%
  pivot_wider(names_from = year, 
              values_from = count,
              values_fill = list(count = 0)) %>% 
  rename(area = hbres, count17 = "2017", count18 = "2018", count19 = "2019", 
         count20 = "2020", count21 = "2021") %>% 
  mutate(age_group = "All Ages", breakdown = "Deprivation")

base_cancer_counts_dep_19_wk53 <- base_cancer_counts_dep %>% 
  filter(week_number == 53) %>% 
  mutate(count17 = NA, count18 = NA, count19 = NA)

base_cancer_counts_dep_19_notwk53 <- base_cancer_counts_dep %>% 
  filter(week_number != 53)

base_cancer_counts_dep <- bind_rows(base_cancer_counts_dep_19_notwk53, 
                                    base_cancer_counts_dep_19_wk53)

rm(base_cancer_counts_dep_19_notwk53, base_cancer_counts_dep_19_wk53)

# combine for base cancer counts with age group split and no split

base_cancer_counts_all <- bind_rows(base_cancer_counts, base_cancer_counts_agegroups, base_cancer_counts_dep)

rm(base_cancer_counts_agegroups, base_cancer_counts_dep)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BASE WEEKLY COUNTS---- 
# DOWNLOAD FILE FOR DASHBOARD??
# (before means or cumulative sums added?)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get mean of weekly counts from 2017-2019

base_cancer_mean <- base_cancer_counts_all %>%
  group_by(week_number, region, area, site, sex) %>%
  mutate(count_mean_17_19 = round((count17 + count18 + count19)/3)) %>%
  ungroup() %>%
  select(-count18)


# Get Cumulative Counts for each year

base_cancer_cum <- base_cancer_mean %>%
  select(region, area, site, sex, age_group, dep, week_number, count19, count20, count21, count_mean_17_19, breakdown) %>%
  group_by(area, site, sex, age_group, dep) %>%
  mutate(cum_count19 = cumsum(count19),
         cum_count20 = cumsum(count20),
         cum_count21 = cumsum(count21),
         cum_count_mean_17_19 = cumsum(count_mean_17_19)) %>%
  ungroup()

rm(base_cancer_mean)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CUM/INC DATASET ----
# base_cancer_cum has all values needed for graphs 1 and 2: cumulative,
# incidence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Add % weekly variation between 2020/2019 and 2020/mean(2017-19)
# (BELOW VARIABLES USED FOR WEEKLY DIFFERENCE GRAPH ONLY - NOT
# ON DASHBOARD FROM SEP 21 UPDATE BUT MAY BRING BACK AS OPTION) 

diff_data_base <- base_cancer_cum %>%
  mutate(difference20 = case_when(count19 > 0 ~ 100*(count20 - count19)/count19,
                                  (count19 = 0 & count20 != 0) ~ 100*(count20 - count19)/1,
                                  TRUE ~ 0),
         difference21 = case_when(count19 > 0 ~ 100*(count21 - count19)/count19,
                                  (count19 = 0 & count21 != 0) ~ 100*(count21 - count19)/1,
                                  TRUE ~ 0),
         difference20_ave = case_when(count_mean_17_19 > 0 ~ 100*(count20 - count_mean_17_19)/count_mean_17_19,
                                      (count_mean_17_19 = 0 & count20 != 0) ~ 100*(count20 - count_mean_17_19)/1,
                                      TRUE ~ 0),
         difference21_ave = case_when(count_mean_17_19 > 0 ~ 100*(count21 - count_mean_17_19)/count_mean_17_19,
                                      (count_mean_17_19 = 0 & count21 != 0) ~ 100*(count21 - count_mean_17_19)/1,
                                      TRUE ~ 0)) %>% 
  mutate(week_ending = dmy("05/01/2020") + days(7*(week_number-1))) 

diff_data_base_24 <- diff_data_base %>% 
  filter(week_number > 45) %>% 
  mutate(count21 = NA,
         cum_count21 = NA)

diff_data_base <- diff_data_base %>% 
  filter(week_number <= 45)

diff_data_base <- bind_rows(diff_data_base, diff_data_base_24)



rm(base_cancer_counts, base_cancer_counts_agegroups, base_cancer_counts_dep,
   base_cancer_cum, base_cancer_slim_q0, base_cancer_counts_all, diff_data_base_24)

saveRDS(diff_data_base, paste0("/conf/PHSCOVID19_Analysis/shiny_input_files/final_app_files/", "cancer_data_2_",
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))
saveRDS(diff_data_base, "shiny_app/data/cancer_data_2.rds")


#######################################################################
# SECTION TO GENERATE CUMULATIVE TOTALS OVER 6 QUARTERS ---- 
# Dec2018-Jun2020 v Dec2019-Jun2021
#######################################################################

base_cancer_slim_cont <- base_cancer_slim %>%
  mutate(quarter2 = qtr(incidence_date)) %>% 
  filter((year > 2018) | (year == 2018 & quarter2 == "October to December 2018")) %>% 
  mutate(year2 = substr(year, 3, 4)) %>% 
  mutate(quarter = case_when(str_detect(quarter2, "January") ~ paste0("Jan-Mar ", year2),
                             str_detect(quarter2, "April") ~ paste0("Apr-Jun ", year2),
                             str_detect(quarter2, "July") ~ paste0("Jul-Sep ", year2),
                             str_detect(quarter2, "October") ~ paste0("Oct-Dec ", year2))) %>% 
  select(-c(derived_upi, quarter2, year2))
##################################

#######################################################################################
# counts broken down quarter, area, sex and cancer type - no deprivation or age groups
# 2019/20
base_cancer_slim_cont_1920 <- base_cancer_slim_cont %>% 
  filter(incidence_date <= "2020-09-30") %>% 
  group_by(quarter, region, hbres, site, sex) %>% 
  summarise(count1920 = n()) %>% 
  ungroup() %>% 
  complete(quarter, nesting(region, hbres, site, sex), fill = list(count1920 = 0))

base_cancer_slim_1920 <- base_cancer_slim_cont_1920 %>%
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 18", "Jan-Mar 19",
                                              "Apr-Jun 19", "Jul-Sep 19",
                                              "Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20"), ordered = TRUE)) %>%
  arrange(quarter) %>% 
  group_by(region, hbres, site, sex) %>% 
  mutate(cum_count1920 = cumsum(count1920)) %>%
  ungroup() %>% 
  mutate(quarter = case_when(quarter == "Oct-Dec 18" ~ "Oct-Dec 19",
                             quarter == "Jan-Mar 19" ~ "Jan-Mar 20",
                             quarter == "Apr-Jun 19" ~ "Apr-Jun 20",
                             quarter == "Jul-Sep 19" ~ "Jul-Sep 20",
                             quarter == "Oct-Dec 19" ~ "Oct-Dec 20",
                             quarter == "Jan-Mar 20" ~ "Jan-Mar 21",
                             quarter == "Apr-Jun 20" ~ "Apr-Jun 21",
                             quarter == "Jul-Sep 20" ~ "Jul-Sep 21")) %>% 
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20",
                                              "Oct-Dec 20", "Jan-Mar 21",
                                              "Apr-Jun 21", "Jul-Sep 21"), ordered = TRUE))

# counts broken down quarter, area, sex and cancer type - no deprivation or age groups
# 2020/21
base_cancer_slim_cont_2021 <- base_cancer_slim_cont %>% 
  filter((incidence_date >= "2019-10-01") & (incidence_date <= "2021-09-30")) %>% 
  group_by(quarter, region, hbres, site, sex) %>% 
  summarise(count2021 = n()) %>% 
  ungroup() %>% 
  complete(quarter, nesting(region, hbres, site, sex), fill = list(count2021 = 0))

base_cancer_slim_2021 <- base_cancer_slim_cont_2021 %>%
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20",
                                              "Oct-Dec 20", "Jan-Mar 21",
                                              "Apr-Jun 21", "Jul-Sep 21"), ordered = TRUE)) %>%
  arrange(quarter) %>% 
  group_by(region, hbres, site, sex) %>% 
  mutate(cum_count2021 = cumsum(count2021)) %>% 
  ungroup()

base_cancer_diff <- left_join(base_cancer_slim_2021, base_cancer_slim_1920) %>% 
  mutate(dif = case_when(count1920 > 0 ~ 100*(count2021-count1920)/count1920,
                         count1920 == 0 & count2021 != 0 ~ 100*(count2021-count1920)/1,
                         TRUE ~ 0),
         cum_dif = case_when(cum_count1920 > 0 ~ 100*(cum_count2021-cum_count1920)/cum_count1920,
                             cum_count1920 == 0 & cum_count2021 != 0 ~ 100*(cum_count2021-cum_count1920)/1,
                             TRUE ~ 0),
         age_group = "All Ages", dep = 0, breakdown = "None") %>% 
  select(quarter, region, hbres, site, sex, age_group, dep, dif, cum_dif, breakdown)
######################################################################################

#######################################################################################
# counts broken down quarter, area, sex, cancer type and deprivation - no age groups
# 2019/20
base_cancer_slim_cont_1920_dep <- base_cancer_slim_cont %>% 
  filter(incidence_date <= "2020-09-30") %>% 
  group_by(quarter, region, hbres, site, sex, dep) %>% 
  summarise(count1920 = n()) %>% 
  ungroup() %>% 
  complete(quarter, nesting(region, hbres, site, sex, dep), fill = list(count1920 = 0))

base_cancer_slim_1920_dep <- base_cancer_slim_cont_1920_dep %>%
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 18", "Jan-Mar 19",
                                              "Apr-Jun 19", "Jul-Sep 19",
                                              "Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20"), ordered = TRUE)) %>%
  arrange(quarter) %>% 
  group_by(region, hbres, site, sex, dep) %>% 
  mutate(cum_count1920 = cumsum(count1920)) %>%
  ungroup() %>% 
  mutate(quarter = case_when(quarter == "Oct-Dec 18" ~ "Oct-Dec 19",
                             quarter == "Jan-Mar 19" ~ "Jan-Mar 20",
                             quarter == "Apr-Jun 19" ~ "Apr-Jun 20",
                             quarter == "Jul-Sep 19" ~ "Jul-Sep 20",
                             quarter == "Oct-Dec 19" ~ "Oct-Dec 20",
                             quarter == "Jan-Mar 20" ~ "Jan-Mar 21",
                             quarter == "Apr-Jun 20" ~ "Apr-Jun 21",
                             quarter == "Jul-Sep 20" ~ "Jul-Sep 21")) %>% 
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20",
                                              "Oct-Dec 20", "Jan-Mar 21",
                                              "Apr-Jun 21", "Jul-Sep 21"), ordered = TRUE))

# counts broken down quarter, area, sex, cancer type and deprivation - no age groups
# 2020/21
base_cancer_slim_cont_2021_dep <- base_cancer_slim_cont %>% 
  filter((incidence_date >= "2019-10-01") & (incidence_date <= "2021-09-30")) %>% 
  group_by(quarter, region, hbres, site, sex, dep) %>% 
  summarise(count2021 = n()) %>% 
  ungroup() %>% 
  complete(quarter, nesting(region, hbres, site, sex, dep), fill = list(count2021 = 0))

base_cancer_slim_2021_dep <- base_cancer_slim_cont_2021_dep %>%
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20",
                                              "Oct-Dec 20", "Jan-Mar 21",
                                              "Apr-Jun 21", "Jul-Sep 21"), ordered = TRUE)) %>%
  arrange(quarter) %>% 
  group_by(region, hbres, site, sex, dep) %>% 
  mutate(cum_count2021 = cumsum(count2021)) %>% 
  ungroup()

base_cancer_diff_dep <- left_join(base_cancer_slim_2021_dep, base_cancer_slim_1920_dep) %>% 
  mutate(dif = case_when(count1920 > 0 ~ 100*(count2021-count1920)/count1920,
                         count1920 == 0 & count2021 != 0 ~ 100*(count2021-count1920)/1,
                         TRUE ~ 0),
         cum_dif = case_when(cum_count1920 > 0 ~ 100*(cum_count2021-cum_count1920)/cum_count1920,
                             cum_count1920 == 0 & cum_count2021 != 0 ~ 100*(cum_count2021-cum_count1920)/1,
                             TRUE ~ 0),
         age_group = "All Ages", breakdown = "Deprivation") %>% 
  select(quarter, region, hbres, site, sex, age_group, dep, dif, cum_dif, breakdown)
#######################################################################################


#######################################################################################
# counts broken down quarter, area, sex, cancer type and age groups - no deprivation
# 2019/20
base_cancer_slim_cont_1920_age <- base_cancer_slim_cont %>% 
  filter(incidence_date <= "2020-06-30") %>% 
  group_by(quarter, region, hbres, site, sex, age_group) %>% 
  summarise(count1920 = n()) %>% 
  ungroup() %>% 
  complete(quarter, nesting(region, hbres, site, sex, age_group), fill = list(count1920 = 0))

base_cancer_slim_1920_age <- base_cancer_slim_cont_1920_age %>%
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 18", "Jan-Mar 19",
                                              "Apr-Jun 19", "Jul-Sep 19",
                                              "Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20"), ordered = TRUE)) %>%
  arrange(quarter) %>% 
  group_by(region, hbres, site, sex, age_group) %>% 
  mutate(cum_count1920 = cumsum(count1920)) %>%
  ungroup() %>% 
  mutate(quarter = case_when(quarter == "Oct-Dec 18" ~ "Oct-Dec 19",
                             quarter == "Jan-Mar 19" ~ "Jan-Mar 20",
                             quarter == "Apr-Jun 19" ~ "Apr-Jun 20",
                             quarter == "Jul-Sep 19" ~ "Jul-Sep 20",
                             quarter == "Oct-Dec 19" ~ "Oct-Dec 20",
                             quarter == "Jan-Mar 20" ~ "Jan-Mar 21",
                             quarter == "Apr-Jun 20" ~ "Apr-Jun 21",
                             quarter == "Jul-Sep 20" ~ "Jul-Sep 21")) %>% 
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20",
                                              "Oct-Dec 20", "Jan-Mar 21",
                                              "Apr-Jun 21", "Jul-Sep 21"), ordered = TRUE))

# counts broken down quarter, area, sex, cancer type and age groups - no deprivation
# 2020/21
base_cancer_slim_cont_2021_age <- base_cancer_slim_cont %>% 
  filter((incidence_date >= "2019-10-01") & (incidence_date <= "2021-06-30")) %>% 
  group_by(quarter, region, hbres, site, sex, age_group) %>% 
  summarise(count2021 = n()) %>% 
  ungroup() %>% 
  complete(quarter, nesting(region, hbres, site, sex, age_group), fill = list(count2021 = 0))

base_cancer_slim_2021_age <- base_cancer_slim_cont_2021_age %>%
  mutate(quarter = factor(quarter, levels = c("Oct-Dec 19", "Jan-Mar 20",
                                              "Apr-Jun 20", "Jul-Sep 20",
                                              "Oct-Dec 20", "Jan-Mar 21",
                                              "Apr-Jun 21", "Jul-Sep 21"), ordered = TRUE)) %>%
  arrange(quarter) %>% 
  group_by(region, hbres, site, sex, age_group) %>% 
  mutate(cum_count2021 = cumsum(count2021)) %>% 
  ungroup()

base_cancer_diff_age <- left_join(base_cancer_slim_2021_age, base_cancer_slim_1920_age) %>% 
  mutate(dif = case_when(count1920 > 0 ~ 100*(count2021-count1920)/count1920,
                         count1920 == 0 & count2021 != 0 ~ 100*(count2021-count1920)/1,
                         TRUE ~ 0),
         cum_dif = case_when(cum_count1920 > 0 ~ 100*(cum_count2021-cum_count1920)/cum_count1920,
                             cum_count1920 == 0 & cum_count2021 != 0 ~ 100*(cum_count2021-cum_count1920)/1,
                             TRUE ~ 0),
         dep = 0, breakdown = "Age Group") %>% 
  select(quarter, region, hbres, site, sex, age_group, dep, dif, cum_dif, breakdown)
#######################################################################################

# final dataset for use in dashboard??
cancer_diff_cumulative <- bind_rows(base_cancer_diff,base_cancer_diff_age,base_cancer_diff_dep) %>% 
  rename(area = hbres)

rm(base_cancer_diff, base_cancer_diff_age, base_cancer_diff_dep,
   base_cancer_slim_cont, base_cancer_slim_cont_1920, base_cancer_slim_cont_2021,
   base_cancer_slim_cont_1920_dep, base_cancer_slim_cont_1920_age,
   base_cancer_slim_cont_2021_dep, base_cancer_slim_cont_2021_age,
   base_cancer_slim_1920, base_cancer_slim_2021, base_cancer_slim_1920_age,
   base_cancer_slim_1920_dep, base_cancer_slim_2021_dep, base_cancer_slim_2021_age,
   base_cancer_slim, diff_data_base_quarters, diff_data_base_quarters_NA, diff_data_base_quarters_notNA)
                 

saveRDS(cancer_diff_cumulative, paste0("/conf/PHSCOVID19_Analysis/shiny_input_files/final_app_files/", "cancer_data_diff_", 
                                        format(Sys.Date(), format = '%d_%b_%y'), ".rds"))


saveRDS(cancer_diff_cumulative, "shiny_app/data/cancer_data_diff.rds")

                                    