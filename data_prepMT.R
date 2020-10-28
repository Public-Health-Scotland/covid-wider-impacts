##########################################
#
# Cancer Pathology Data prep for
# PHS COVID-Wider-Impact dashboard
#
# M.Turner - Cancer Team
# 
##########################################

##########################################
# Housekeeping
##########################################

require(tidyverse)||install.packages("tidyverse")
require(janitor)||install.packages("janitor")
require(lubridate)||install.packages("lubridate")
require(writexl)||install.packages("writexl")
require(plotly)||install.packages("plotly")

##########################################
# Import Data
##########################################

input_folder <- paste0("////PHI_conf//CancerGroup1//Topics//CancerStatistics//Projects",
                       "//20200804-pathology-as-proxy-for-2020-regs//RShiny//CancerPathologyData//")
cl_out <- "/conf/linkage/output/lookups/Unicode/"

# import pathology data
cancer <- read_csv(paste0(input_folder,"Pathology_Data.csv"), col_names = T) %>%  
  clean_names() 

# import deprivation lookup
depriv_dir <- readRDS(paste0(cl_out,"Deprivation/postcode_2020_2_simd2020v2.rds")) %>% 
  clean_names() %>% 
  select(pc8, hb2019name, simd2020v2_sc_quintile) %>% 
  rename(postcode = pc8, hbres = hb2019name, dep = simd2020v2_sc_quintile) 


###########################################################
# Add Age Groups, Cancer Types and Screening Cohorts
###########################################################

# temporarily needed 

cancer <- cancer %>% 
  mutate(sex = case_when(person_id == 24654927 ~ 2,
               person_id == 24936892 ~ 2,
               person_id == 24942426 ~ 2,
               person_id == 24942658 ~ 1,
               person_id == 25068183 ~ 1,
      TRUE ~ sex ))

# creates age column
cancer <- cancer %>% 
  mutate(dob = dmy(date_of_birth), 
         doi = dmy(incidence_date),
         age = floor(difftime(doi, dob, units = "days")/365))

# creates age group column
cancer <- cancer %>% 
  mutate(age_group = case_when(between(age, 0, 4) ~ "Under 5",
                               between(age, 5, 9) ~ "5-9",
                               between(age, 10, 14) ~ "10-14",
                               between(age, 15, 19)  ~ "15-19",
                               between(age, 20, 24) ~ "20-24",
                               between(age, 25, 29) ~ "25-29",
                               between(age, 30, 34) ~ "30-34",
                               between(age, 35, 39) ~ "35-39",
                               between(age, 40, 44) ~ "40-44",
                               between(age, 45, 49) ~ "45-49",
                               between(age, 50, 54) ~ "50-54",
                               between(age, 55, 59) ~ "55-59",
                               between(age, 60, 64) ~ "60-64",
                               between(age, 65, 69) ~ "65-69",
                               between(age, 70, 74)~ "70-74",
                               between(age, 75, 79) ~ "75-79",
                               age > 79 ~ "80 and over"))


# create cancer siteno

cancer <- cancer %>% 
  filter(str_detect(icd10_conv, "C")) %>% 
  mutate(icd_2 = as.integer(substr(icd10_conv, 2, 3))) %>% 
  mutate(siteno = case_when(icd_2 == 67 ~ 210,
                            icd_2 == 71 ~ 410,
                            icd_2 == 50 ~ 510,
                            icd_2 == 56 ~ 740,
                            icd_2 == 52 ~ 760,
                            icd_2 == 51 ~ 770,
                            icd_2 == 73 ~ 870,
                            icd_2 == 81 ~ 910,
                            icd_2 == 22 ~ 1210,
                            icd_2 == 45 ~ 1320,
                            icd_2 == 60 ~ 1410,
                            icd_2 == 61 ~ 1420,
                            icd_2 == 62 ~ 1430,
                            icd_2 == 90 ~ 1510,
                            icd_2 == 15 ~ 1710,
                            icd_2 == 25 ~ 1810,
                            icd_2 == 43 ~ 1910,
                            icd_2 == 44 ~ 1920,
                            icd_2 == 16 ~ 2010,
                            between(icd_2, 40, 41)| icd_2 == 47| icd_2 == 49 ~ 320,
                            between(icd_2, 18, 20) ~ 610,
                            between(icd_2, 53, 55) ~ 750,
                            between(icd_2, 00, 14)| between(icd_2, 30,32) ~ 810,
                            between(icd_2, 64, 65) ~ 1010,
                            between(icd_2, 91, 95) ~ 1110,
                            between(icd_2, 33, 34) ~ 1310,
                            between(icd_2, 82,86) ~ 1610,
                            TRUE ~ 999))

# add cancer description

cancer <- cancer %>%
  filter(!(is.na(siteno))) %>% 
  mutate(site = case_when(siteno == 210 ~ "Bladder",
                          siteno == 320 ~ "Bone and Connective Tissue",
                          siteno == 410 ~ "Malignant Brain Cancer",
                          siteno == 510 ~ "Breast",
                          siteno == 610 ~ "Colorectal",
                          siteno == 740 ~ "Ovary - Females only",
                          siteno == 750 ~ "Uterus - Females only",
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
                          siteno == 1610 ~ "Non-Hodgkin lymphoma",
                          siteno == 1710 ~ "Oesophagus",
                          siteno == 1810 ~ "Pancreas",
                          siteno == 1910 ~ "Malignant Melanoma of the Skin",
                          siteno == 1920 ~ "Non-Melanoma Skin Cancer",
                          siteno == 2010 ~ "Stomach",
                          siteno == 999 ~ "Other"))

# create screening cohort indicators

cancer <- cancer %>% 
  mutate(breast_scr = if_else(siteno == 510 & age >= 50 & age <= 70 & sex == 2, 1, 0)) %>% 
  mutate(colo_scr = if_else(siteno == 610 & age >= 50 & age <= 74, 1, 0)) %>% 
  mutate(cerv_scr = case_when(str_detect(icd10_conv, "C53") & age >= 25 & age <= 49 ~ 1,
                              str_detect(icd10_conv, "C53") & age >= 50 & age <= 64 ~ 2,
                              str_detect(icd10_conv, "C53") & age < 25 & age > 64 ~ 3,
                              TRUE ~ 0))


# get Health Boards of residence and deprivation quintil rank from postcodes

cancer_joined <- inner_join(cancer, depriv_dir) %>%
  replace_na(list(hbres = "Unknown", dep = 9, sex = 9))

# filter impossible sex cancer combo

cancer_joined <- cancer_joined %>% 
  filter(!(sex != 2 & (siteno >= 740 & siteno <= 770))) %>% 
  filter(!(sex != 1 & (siteno >= 1410 & siteno <= 1430)))

# Sex labels

cancer_joined$sex <- factor(cancer_joined$sex, levels = c(1,2,9), 
                            labels = c("Male", "Female", "Unknown"))

# Change the health board labels

cancer_joined$hbres <- recode(cancer_joined$hbres, 
                              "NHS Ayrshire and Arran" = "NHS Ayrshire & Arran", 
                              "NHS Dumfries and Galloway" = "NHS Dumfries & Galloway", 
                              "NHS Greater Glasgow and Clyde" = "NHS Greater Glasgow & Clyde") 

##########################################
# Counting Distinct CHIs
##########################################

# Individual cancer types 

# health boards
base_cancer <- cancer_joined %>% 
  group_by(year, week_number, hbres, site, sex, person_id) %>% 
  summarise() %>% 
  ungroup()

# networks

networks <- base_cancer %>% 
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
                           hbres == "NHS Lanarkshire" ~ "WOSCAN")) %>% 
  group_by(year, week_number, hbres, site, sex, person_id) %>% 
  summarise() %>% 
  ungroup()

# scotland

scotland <- base_cancer %>%
  mutate(hbres = "Scotland") %>% 
  group_by(year, week_number, hbres, site, sex, person_id) %>% 
  summarise() %>% 
  ungroup()

# bind all health board values

base_cancer <-rbind(base_cancer, networks, scotland)

# create all persons

base_allperson <- base_cancer %>% 
  mutate(sex = "All Persons") %>% 
  group_by(year, week_number, hbres, site, sex, person_id) %>% 
  summarise() %>% 
  ungroup()

base_cancer <-rbind(base_cancer, base_allperson)


##########################################
# All cancer types
##########################################


allcancers <- base_cancer %>%
  mutate(site = "All Cancers") %>% 
  group_by(year, week_number, hbres, site, sex, person_id) %>% 
  summarise()


##########################################
# All cancer types excl C44
##########################################

exccancers <- base_cancer %>%
  filter(site != "Non-Melanoma Skin Cancer") %>%
  mutate(site = "All Malignant Neoplasms (Excl. C44)") %>% 
  group_by(year, week_number, hbres, site, sex, person_id) %>% 
  summarise()

##########################################
# combine cancer types
##########################################


cancer_dist <- bind_rows(base_cancer, allcancers, exccancers) %>% 
  ungroup()

cancer_dist <- cancer_dist %>% 
  group_by(year, week_number, hbres, site, sex) %>% 
  summarise(count = n())
  

########################################################
#
# count19 and count20 totals 
#
#########################################################

cancer_2019 <- cancer_dist %>% 
  filter(year == 2019) %>%
  group_by(hbres, site, sex, week_number) %>%
  summarise(count19 = sum(count))

cancer_2020 <- cancer_dist %>% 
  filter(year == 2020) %>% 
  group_by(hbres, site, sex, week_number) %>%
  summarise(count20 = sum(count))

cancer_dist <- full_join(cancer_2020, cancer_2019) %>% 
  ungroup()

cancer_dist <- cancer_dist %>% 
  mutate(count20 = case_when(
    is.na(count20) ~ 0,
    count20 >= 0 ~ as.double(count20)),
    count19 = case_when(
      is.na(count19) ~ 0,
      count19 >= 0 ~ as.double(count19)))

# for comparison of sex
cancer_sex <- cancer_dist %>%
  mutate(category = "sex", sex = as.character(sex)) %>%
  filter(sex != "Unknown") %>% 
  rename(type = sex) %>% 
  group_by(hbres, site, week_number, category, type) %>% 
  summarise(count20 = sum(count20),
            count19 = sum(count19))

# for comparison of health board 
cancer_hb <- cancer_dist %>%
  mutate(category = "hb", hbres = as.character(hbres)) %>%
  mutate(type = hbres) %>% 
  group_by(hbres, site, week_number, category, type) %>%
  summarise(count19 = sum(count19),
            count20 = sum(count20))

#Bind rows

cancer_combined <- rbind(cancer_hb, cancer_sex) %>% 
  rename(area = hbres)


##########################################
# Calculate % Difference
##########################################

diff_data <- cancer_combined %>%
  mutate(difference = case_when(
    count19 > 0 ~ 100*(count20 - count19)/count19))


##########################################
# Week ending labels
##########################################

diff_data <-  diff_data %>% 
  mutate(week_ending = dmy("05/01/2020") + days(7*(week_number-1))) 


##########################################
# Complete Weeks only
##########################################

diff_data <-  diff_data %>% 
  filter(week_number <= 26) %>%
  ungroup()


##########################################
# Export Data
##########################################

# save as excel  and RDS file
#write_xlsx(cancer_joined, "CWT Dashboard Input Data.xlsx")

saveRDS(diff_data, "shiny_app/data/cancer_data.rds")


##########################################
# To be added to app data prep
##########################################

cancer_cum <- diff_data %>%
  filter(category == "sex") %>%
  rename(sex = type) %>%
  select(area, site, sex, week_ending, count19, count20, difference) %>%
  group_by(area, site, sex) %>%
  mutate(cum_count19 = cumsum(count19),
         cum_count20 = cumsum(count20))


saveRDS(cancer_cum, "shiny_app/data/cancer_data_2.rds")

