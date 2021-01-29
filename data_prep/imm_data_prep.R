# Data preparation for immunisations tab

###############################################.
## Functions/Packages/filepaths/lookups ----
###############################################.
source("data_prep/functions_packages_data_prep.R")

###############################################.
## 6-in-1 data ----
###############################################.

#field with date all immunisation data files prepared
imms_date <- "20210125"

six_alldose <- read_csv(paste0(data_folder,"immunisations/6in1/", imms_date, "/six_in_one_dashboard_",imms_date,".csv"), 
                        col_types =list(eligible_start=col_date(format="%m/%d/%Y"),
                                        time_period_eligible=col_factor())) %>%
  janitor::clean_names()

week_var <- "eligible_start"

# Creating levels for factor in chronological order
six_alldose$time_period_eligible <- factor(six_alldose$time_period_eligible,
                                           levels=unique(six_alldose$time_period_eligible[order(six_alldose[[week_var]], decreasing = T)]),
                                           ordered=TRUE)

six_alldose <- left_join(six_alldose, hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(eligible_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, exclude, immunisation, eligible_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no)

saveRDS(six_alldose, "shiny_app/data/six_alldose.rds")
saveRDS(six_alldose, paste0(data_folder,"final_app_files/six_alldose_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## immunisations data table dataset prep ----
## immunisation team supply a single csv file that is split into two rds files (one for each immunisation, mmr and 6-in-1)

imms_datatable <- format_immchild_table(paste0("immunisations/dashboardtable_", imms_date), save_file = F)

six_datatable <- imms_datatable %>%
  filter(str_detect(immunisation,"six-in-one")) %>%
  select(-uptake_13m_num:-uptake_3y8m_percent) #remove uptake columns that related to mmr 

saveRDS(six_datatable, paste0("shiny_app/data/","sixinone_datatable.rds"))
saveRDS(six_datatable, paste0(data_folder,"final_app_files/sixinone_datatable_", 
                              format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

mmr_datatable <- imms_datatable %>%
  filter(str_detect(immunisation,"mmr")) %>%
  select(-uptake_12weeks_num:-uptake_32weeks_percent) #remove uptake columns that related to mmr 
saveRDS(mmr_datatable, paste0("shiny_app/data/","mmr_datatable.rds"))
saveRDS(mmr_datatable, paste0(data_folder,"final_app_files/mmr_datatable_", 
                              format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# Grampian data
mmr_dose2_datatable_grampian <- format_immchild_table(paste0("immunisations/mmr/", imms_date, "/dashboardtable_grampian_",imms_date),
                                                      "mmr_dose2_grampian") 

###############################################.
## 6-in-1 simd data ---- 
six_dose1_simdtable <- format_immsimd_data(paste0("immunisations/6in1/", imms_date, "/six-in-one dose 1_simd_", imms_date),
                                           "six_dose1")
six_dose2_simdtable <- format_immsimd_data(paste0("immunisations/6in1/", imms_date, "/six-in-one dose 2_simd_", imms_date),
                                           "six_dose2")
six_dose3_simdtable <- format_immsimd_data(paste0("immunisations/6in1/", imms_date, "/six-in-one dose 3_simd_", imms_date),
                                           "six_dose3")

###############################################.
# Immunisation definitions ----
# apply both for MRR and 6 in one
age_defs_imm_mmr <- read_excel(paste0(data_folder, "immunisations/age definitions.xlsx"),
                               sheet = "mmr_dash") %>% 
  mutate(defined = case_when(is.na(defined) ~ "", T ~ paste0(defined)))

age_defs_imm_mmr <- age_defs_imm_mmr %>% flextable() %>% 
  set_header_labels(defined = "Defined in weeks as:",
                    "...1" = "") %>% 
  merge_at(i =1, j = 1:2, part ="body") %>% 
  merge_at(i =6, j = 1:2, part ="body") %>% 
  merge_at(i =11, j = 1:3, part ="body") %>% 
  align(j = 1) %>% 
  bold(i =1) %>% bold(i =6) %>% bold(i = 11)

age_defs_imm_mmr # checking

saveRDS(age_defs_imm_mmr, "shiny_app/data/age_elig_mmr.rds")
saveRDS(age_defs_imm_mmr, paste0(data_folder,"final_app_files/age_defs_imm_mmr_", 
                                 format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# 6 in one age eligibility
age_defs_imm_6inone <- read_excel(paste0(data_folder, "immunisations/age definitions.xlsx"),
                                  sheet = "6inone_dash") 

age_defs_imm_6inone <- age_defs_imm_6inone %>% flextable() %>% 
  set_header_labels("...1" = "") %>% 
  merge_at(i =1, j = 1:2, part ="body") %>% 
  merge_at(i =6, j = 1:2, part ="body") %>% 
  merge_at(i =11, j = 1:2, part ="body") %>% 
  align(j =1) %>% 
  bold(i =1) %>% bold(i =6) %>% bold(i = 11) 
age_defs_imm_6inone #checking

saveRDS(age_defs_imm_6inone, "shiny_app/data/age_elig_6inone.rds")
saveRDS(age_defs_imm_6inone, paste0(data_folder,"final_app_files/age_defs_imm_6inone_", 
                                    format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

# month eligibility table
month_defs_imm <- read_excel(paste0(data_folder, "immunisations/month eligible definitions.xlsx"),
                             sheet = "for_dash") %>% 
  mutate("Month eligible" = format(as.Date(`Month eligible`), "%b-%Y")) %>% 
  flextable() %>%
  add_header_row(values = c("Month eligible", "Defined as children reaching relevant age in period:", "", "Number of weeks")) %>% 
  merge_at(i = 1, j = 2:3, part = "header") %>% 
  merge_v(j = 1:2, part = "header") %>% 
  merge_v(j = 4, part = "header") %>%
  theme_vanilla

month_defs_imm #checking everything looks ok

saveRDS(month_defs_imm, "shiny_app/data/month_eligibility_immun.rds")
saveRDS(month_defs_imm, paste0(data_folder,"final_app_files/month_eligibility_immun_", 
                               format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## MMR data ----
###############################################.
#field with date all immunisation data files prepared
imms_date <- "20210125"

# mmr dose 1 & 2 - scurve data
mmr_alldose <- read_csv(paste0(data_folder,"immunisations/mmr/", imms_date, "/mmr_dashboard_",imms_date,".csv"),
                        col_types =list(eligible_start=col_date(format="%m/%d/%Y"),
                                        time_period_eligible=col_factor())) %>%
  janitor::clean_names()

week_var <- "eligible_start"

# Creating levels for factor in chronological order
mmr_alldose$time_period_eligible <- factor(mmr_alldose$time_period_eligible,
                                           levels=unique(mmr_alldose$time_period_eligible[order(mmr_alldose[[week_var]], decreasing = T)]),
                                           ordered=TRUE)

mmr_alldose <- left_join(mmr_alldose, hb_lookup, by = c("geography" = "hb_cypher")) %>%
  mutate(area_name=case_when(geography=="M" ~ "Scotland",TRUE~ area_name), #Scotland not in lookup but present in data
         area_type=case_when(geography=="M" ~ "Scotland",TRUE~area_type),
         weeks=interv/7,
         week_no= isoweek(eligible_start),
         cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
  arrange(cohort) %>%
  select (extract_date, exclude, immunisation, eligible_start, time_period_eligible, tabno, surv, interv, cohort, area_name, area_type, week_no)

saveRDS(mmr_alldose, paste0("shiny_app/data/","mmr_alldose.rds"))
saveRDS(mmr_alldose, paste0(data_folder,"final_app_files/mmr_alldose_", 
                            format(Sys.Date(), format = '%d_%b_%y'), ".rds"))

###############################################.
## MMR simd data ----
###############################################.
mmr_dose1_simdtable <- format_immsimd_data(paste0("immunisations/mmr/", imms_date,"/mmr dose 1_simd_", imms_date),
                                           "mmr_dose1")
mmr_dose2_simdtable <- format_immsimd_data(paste0("immunisations/mmr/", imms_date, "/mmr dose 2_simd_", imms_date),
                                           "mmr_dose2")

##END