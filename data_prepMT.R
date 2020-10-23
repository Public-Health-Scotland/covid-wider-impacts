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

input_folder <- "////PHI_conf//CancerGroup1//Topics//CancerStatistics//Projects//20200804-pathology-as-proxy-for-2020-regs//RShiny//CancerPathologyData//"
cl_out <- "/conf/linkage/output/lookups/Unicode/"

# import pathology data
cancer <- read_csv(paste0(input_folder,"20200825 Covid source data pathology detail.csv"), 
                   col_names = T, skip = 3) %>%  
  clean_names() 

# import deprivation lookup
depriv_dir <- readRDS(paste0(cl_out,"Deprivation/postcode_2020_2_simd2020v2.rds")) %>% 
  clean_names() %>% 
  select(pc8, hb2019name, simd2020v2_sc_quintile) %>% 
  rename(postcode = pc8, hbres = hb2019name, dep = simd2020v2_sc_quintile) 
  

###########################################################
# Add Age Groups, Cancer Types and Screening Cohorts
###########################################################

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
                               age > 79 | (age < 0  & age > -100) ~ "80 and over"))
                               

# create cancer siteno
# there's 100% an easier way to do this, filter based on starts with C then use numbers?

cancer <- cancer %>% 
  mutate(siteno = case_when(str_detect(icd10_conv, "C67")  ~ 210,
                               str_detect(icd10_conv, "C50")  ~ 510,
                               str_detect(icd10_conv, "C56")  ~ 740,
                               str_detect(icd10_conv, "C52")  ~ 760,
                               str_detect(icd10_conv, "C51")  ~ 770,
                               str_detect(icd10_conv, "C73")  ~ 870,
                               str_detect(icd10_conv, "C81")  ~ 910,
                               str_detect(icd10_conv, "C22")  ~ 1210,
                               str_detect(icd10_conv, "C45")  ~ 1320,
                               str_detect(icd10_conv, "C60")  ~ 1410,
                               str_detect(icd10_conv, "C61")  ~ 1420,
                               str_detect(icd10_conv, "C62")  ~ 1430,
                               str_detect(icd10_conv, "C90")  ~ 1510,
                               str_detect(icd10_conv, "C15")  ~ 1710,
                               str_detect(icd10_conv, "C25")  ~ 1810,
                               str_detect(icd10_conv, "C43")  ~ 1910,
                               str_detect(icd10_conv, "C44")  ~ 1920,
                               str_detect(icd10_conv, "C16")  ~ 2010,
                               str_detect(icd10_conv, "C40") | 
                                 str_detect(icd10_conv, "C41") | 
                                 str_detect(icd10_conv, "C47") |
                                 str_detect(icd10_conv, "C49")  ~ 320,
                               str_detect(icd10_conv, "C71") ~ 410,
                               str_detect(icd10_conv, "C17") | 
                                 str_detect(icd10_conv, "C18") | 
                                 str_detect(icd10_conv, "C19") | 
                                 str_detect(icd10_conv, "C20") ~ 610,
                               str_detect(icd10_conv, "C53") | 
                                 str_detect(icd10_conv, "C54") | 
                                 str_detect(icd10_conv, "C55") ~ 750,
                               str_detect(icd10_conv, "C0") | 
                                 str_detect(icd10_conv, "C10") | 
                                 str_detect(icd10_conv, "C11") | 
                                 str_detect(icd10_conv, "C12") | 
                                 str_detect(icd10_conv, "C13") | 
                                 str_detect(icd10_conv, "C14") |
                                 str_detect(icd10_conv, "C30") |
                                 str_detect(icd10_conv, "C31") | 
                                 str_detect(icd10_conv, "C32") ~ 810,
                               str_detect(icd10_conv, "C64") | 
                                 str_detect(icd10_conv, "C65") ~ 1010,
                               str_detect(icd10_conv, "C91") | 
                                 str_detect(icd10_conv, "C92") | 
                                 str_detect(icd10_conv, "C93") | 
                                 str_detect(icd10_conv, "C94") ~ 1110,
                                 str_detect(icd10_conv, "C33") | 
                                 str_detect(icd10_conv, "C34") ~ 1310,
                                 str_detect(icd10_conv, "C82") |
                                 str_detect(icd10_conv, "C83") |
                                 str_detect(icd10_conv, "C84") |
                                 str_detect(icd10_conv, "C85") |
                                 str_detect(icd10_conv, "C86") ~ 1610,
                               str_detect(icd10_conv, "C") ~ 999)) 

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
                                   siteno == 740 ~ "Vulva - Females only",
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
                                   siteno == 999 ~ "Other")) %>% 
  mutate(breast_scr = if_else(siteno == 510 & age >= 50 & age <= 70 & sex == 2, 1, 0)) %>% 
  mutate(colo_scr = if_else(siteno == 610 & age >= 50 & age <= 74, 1, 0)) %>% 
  mutate(cerv_scr = case_when(str_detect(icd10_conv, "C53") & age >= 25 & age <= 49 ~ 1,
                              str_detect(icd10_conv, "C53") & age >= 50 & age <= 64 ~ 2,
                              str_detect(icd10_conv, "C53") & age < 25 & age > 64 ~ 3,
                              TRUE ~ 0))


# get Health Boards of residence and deprivation quintil rank from postcodes

cancer_joined <- left_join(cancer, depriv_dir) %>%
  filter(!(is.na(age_group))) %>% 
  replace_na(list(hbres = "Unknown", site = "Other", dep = 9, sex = 9))


# Sex labels
cancer_joined$sex <- factor(cancer_joined$sex, levels = c(1,2,9), 
                            labels = c("Male", "Female", "Unspecified"))

# Change the health board labels
cancer_joined$hbres <- recode(cancer_joined$hbres, 
                              "NHS Ayrshire and Arran" = "NHS Ayrshire & Arran", 
                              "NHS Dumfries and Galloway" = "NHS Dumfries & Galloway", 
                              "NHS Greater Glasgow and Clyde" = "NHS Greater Glasgow & Clyde") 

##########################################
# Counting Distinct CHIs
##########################################

# Individual cancer types 

cancer_types <- cancer_joined %>% 
  group_by(year, site, person_id, sex, hbres) %>% 
  summarise(week_number = first(week_number), 
            age_group = first(age_group),
            dep = min(dep))

# All cancer types - IS THIS NEEDED?
cancer_all <- cancer_joined %>%
  group_by(year, person_id, sex, hbres) %>%
  summarise(week_number = first(week_number),
            age_group = first(age_group),
            dep = min(dep)) %>% 
  mutate(site = "All")

# # All cancer types excluding C44
# cancer_excl <- cancer_joined %>% 
#   filter(siteno != 1920) %>% 
#   group_by(year, person_id, sex, hbres) %>% 
#   summarise(week_number = first(week_number), 
#             age_group = first(age_group),
#             dep = min(dep)) %>%
#   mutate(site = "All Malignant Neoplasms (Excl. C44)")

cancer_dist <- bind_rows(cancer_types,
                     cancer_all)

########################################################
#
# Cumulative totals 
#
#########################################################

# # to add cumulative totals for all cancer
# cancer_tot_2019 <- cancer_dist %>%
#   filter(year == 2019) %>% 
#   group_by(week_number) %>%
#   summarise(count19 = n()) %>%
#   mutate(cum_count19 = cumsum(count19)) %>% 
#   mutate(category = "All", type = "All Cancer Types")
# 
# # # to add cumulative totals for all cancer
# cancer_tot_2020 <- cancer_dist %>%
#   filter(year == 2020) %>% 
#   group_by(week_number) %>%
#   summarise(count20 = n()) %>%
#   mutate(cum_count20 = cumsum(count20)) %>% 
#   mutate(category = "All", type = "All Cancer Types")
# 
# cancer_tot <- left_join(cancer_tot_2020, cancer_tot_2019)

# # to add cumulative totals for each HB
cancer_hb_2019 <- cancer_dist %>%
 filter(year == 2019, site == "All") %>%
 group_by(hbres, week_number) %>%
 summarise(count19 = n()) %>%
 mutate(cum_count19 = cumsum(count19)) %>% 
  mutate(category = "HB") %>%
  rename(area = hbres) %>% 
  mutate(type = "All")

cancer_hb_2020 <- cancer_dist %>%
  filter(year == 2020, site == "All") %>%
  group_by(hbres, week_number) %>%
  summarise(count20 = n()) %>%
  mutate(cum_count20 = cumsum(count20)) %>% 
  mutate(category = "HB") %>% 
  rename(area = hbres) %>% 
  mutate(type = "All")

cancer_hb <- left_join(cancer_hb_2020, cancer_hb_2019)

# # to add cumulative totals for each sex
cancer_sex_2019 <- cancer_dist %>%
 filter(year == 2019, site == "All") %>%
 group_by(sex, week_number) %>%
 summarise(count19 = n()) %>%
 mutate(cum_count19 = cumsum(count19)) %>% 
  mutate(category = "sex", sex = as.character(sex)) %>%
  mutate(area = "Scotland") %>% 
  rename(type = sex)

cancer_sex_2020 <- cancer_dist %>%
  filter(year == 2020, site == "All") %>%
  group_by(sex, week_number) %>%
  summarise(count20 = n()) %>%
  mutate(cum_count20 = cumsum(count20)) %>% 
  mutate(category = "sex", sex = as.character(sex)) %>%
  mutate(area = "Scotland") %>%  
  rename(type = sex) 

cancer_sex <- left_join(cancer_sex_2020, cancer_sex_2019)

# # to add cumulative totals for each dep group
cancer_simd_2019 <- cancer_dist %>%
 filter(year == 2019, site == "All") %>%
 group_by(dep, week_number) %>%
 summarise(count19 = n()) %>%
 mutate(cum_count19 = cumsum(count19)) %>% 
  mutate(category = "SIMD", dep = as.character(dep)) %>%
  mutate(area = "Scotland") %>% 
  rename(type = dep)

cancer_simd_2020 <- cancer_dist %>%
  filter(year == 2020, site == "All") %>%
  group_by(dep, week_number) %>%
  summarise(count20 = n()) %>%
  mutate(cum_count20 = cumsum(count20)) %>% 
  mutate(category = "SIMD", dep = as.character(dep)) %>%
  mutate(area = "Scotland") %>%  
  rename(type = dep)

cancer_simd <- left_join(cancer_simd_2020, cancer_simd_2019)

# # to add cumulative totals for each age group
cancer_age_2019 <- cancer_dist %>%
 filter(year == 2019, site == "All") %>%
 group_by(age_group, week_number) %>%
 summarise(count19 = n()) %>%
 mutate(cum_count19 = cumsum(count19))%>% 
  mutate(category = "Age", age_group = as.character(age_group)) %>%
  mutate(area = "Scotland") %>%  
  rename(type = age_group) 

# # to add cumulative totals for each age group
cancer_age_2020 <- cancer_dist %>%
  filter(year == 2020, site == "All") %>%
  group_by(age_group, week_number) %>%
  summarise(count20 = n()) %>%
  mutate(cum_count20 = cumsum(count20))%>% 
  mutate(category = "Age", age_group = as.character(age_group)) %>%
  mutate(area = "Scotland") %>%  
  rename(type = age_group)

cancer_age <- left_join(cancer_age_2020, cancer_age_2019)

# # to add cumulative totals for each cancer site
cancer_site_2019 <- cancer_dist %>%
  filter(year == 2019) %>%
  group_by(site, week_number) %>%
  summarise(count19 = n()) %>%
  mutate(cum_count19 = cumsum(count19)) %>% 
  mutate(category = "Site", site = as.character(site)) %>%
  mutate(area = "Scotland") %>% 
  rename(type = site)

cancer_site_2020 <- cancer_dist %>%
  filter(year == 2020) %>%
  group_by(site, week_number) %>%
  summarise(count20 = n()) %>%
  mutate(cum_count20 = cumsum(count20)) %>% 
  mutate(category = "Site", site = as.character(site)) %>%
  mutate(area = "Scotland") %>% 
  rename(type = site)

cancer_site <- left_join(cancer_site_2020, cancer_site_2019)

#Bind rows

cancer_combined <- rbind(cancer_hb,
                     cancer_site,
                     cancer_age,
                     cancer_sex,
                     cancer_simd) 


##########################################
# Calculate % Difference
##########################################


diff_data <- cancer_combined %>%
  mutate(difference = 100*(count20 - count19)/count19)  %>%
  mutate(week_ending = dmy("05/01/2020") + days(7*(week_number-1))) %>%
  mutate(month = format(week_ending, "%B")) %>% 
  replace_na(list(count19 = 0, count20 = 0))



##########################################
# Export Data
##########################################

# save as excel  and RDS file
#write_xlsx(cancer_joined, "CWT Dashboard Input Data.xlsx")

saveRDS(diff_data, "shiny_app/data/cancer_data_1.rds")



#############################################
# Sort Data for download
#############################################

cancer_dl <- cancer_dist %>%
  group_by(hbres, year, week_number, site, sex) %>%
  summarise(count_dl = n()) %>% 
  mutate(category = "Gender") %>%
  rename(type = sex)

cancer_dl2 <- cancer_dist %>%
  group_by(hbres, year, week_number, site, age_group) %>%
  summarise(count_dl = n()) %>%
  mutate(category = "Age") %>%
  rename(type = age_group)

cancer_dl3 <- cancer_dist %>%
  group_by(hbres, year, week_number, site, dep) %>%
  summarise(count_dl = n()) %>%
  mutate(category = "SIMD") %>%
  rename(type = dep)

cancer_dl3$type <- as.character(cancer_dl3$type)

cancer_base_data <- rbind(cancer_dl, cancer_dl2, cancer_dl3) 

cancer_base_data <- cancer_base_data %>% 
  mutate(week_ending = dmy("05/01/2020") + days(7*(week_number-1)))


saveRDS(cancer_base_data, "shiny_app/data/cancer_data_2.rds")
