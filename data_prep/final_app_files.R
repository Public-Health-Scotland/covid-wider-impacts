# This script reads the latest version of each file and saves it in your local repository.
###############################################.
## Functions and filepaths ----
###############################################.
# Filepath changes depending on Desktop/Server
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  data_folder <- "/conf/PHSCOVID19_Analysis/shiny_input_files/"
  open_data <- "/conf/PHSCOVID19_Analysis/Publication outputs/open_data/"
} else {
  data_folder <- "//Isdsf00d03/PHSCOVID19_Analysis/shiny_input_files/"
  open_data <- "//Isdsf00d03/PHSCOVID19_Analysis/Publication outputs/open_data/"
}

# Function to save final file from common folder to local app folder
# Takes two arguments, the dataset filename and the date stamp on the name of the file
# using a dd_mmm_yy format
save_final_file <- function(dataset, date_saved) {
  final_data <- readRDS(paste0(data_folder,"final_app_files/", dataset, "_", date_saved, ".rds"))
  saveRDS(final_data, paste0("shiny_app/data/", dataset,".rds"))

  file_you_saved <<- final_data
}

###############################################.
## Saving data ----
###############################################.
###############################################.
## Deaths ----
save_final_file("deaths", "05_Sep_22")
##########################################################.
## Cancer ----#
save_final_file("cancer_data_2", "15_Jul_22")
save_final_file("cancer_data_quarters", "15_Jul_22")
save_final_file("cancer_data_quarters_2yr", "15_Jul_22")

##########################################################.
## SACT ----
save_final_file("sact_data", "26_Sep_22")
save_final_file("sact_weekly_data", "26_Sep_22")

##########################################################.
## DCE ----
# save_final_file("dce_data", "19_Jan_22")

##########################################################.
##########################################################.
## Injuries ----
ui_update_date <- "29_Jun_22"
save_final_file("ui_smr01_all", ui_update_date)
save_final_file("ui_smr01_rta", ui_update_date)
save_final_file("ui_smr01_poison", ui_update_date)
save_final_file("ui_smr01_other", ui_update_date)
save_final_file("ui_smr01_falls", ui_update_date)
save_final_file("ui_smr01_assaults", ui_update_date)

##########################################################.
## RAPID ----
save_final_file("rapid", "05_Sep_22")
save_final_file("ethnicity_lookup", "14_Feb_22")
save_final_file("spec_lookup_rapid", "09_Mar_21")
## OOH ----
save_final_file("ooh", "01_Aug_22")
## OOH_cons ----
save_final_file("ooh_cons", "01_Aug_22")
## A&E ----
save_final_file("ae", "05_Sep_22")
## NHS24 ----
save_final_file("nhs24", "05_Sep_22")
## SAS ----
save_final_file("sas", "05_Sep_22")
### Outpatients ----
save_final_file("outpats", "24_May_22")
save_final_file("spec_lookup_op", "24_May_22")
##########################################################.
## Cardio ----
save_final_file("cath_lab", "16_Nov_20") # cath labs
save_final_file("ae_cardio", "05_Sep_22") # A&E
save_final_file("ae_cardio_codes", "30_Nov_20")
save_final_file("ooh_cardiac", "01_Aug_22") # GP OOH
save_final_file("sas_cardiac", "05_Sep_22") # SAS
save_final_file("cardio_drugs", "05_Sep_22") # prescribing
save_final_file("cardio_deaths", "05_Sep_22") # deaths
save_final_file("cardio_admissions", "05_Sep_22") # admissions

##########################################################.
## Immunisations - six in one ----
save_final_file("six_alldose", "31_Aug_22")
save_final_file("six_dose1_simdtable", "31_Aug_22")
save_final_file("six_dose2_simdtable", "31_Aug_22")
save_final_file("six_dose3_simdtable", "31_Aug_22")
save_final_file("sixinone_datatable", "31_Aug_22")
## Immunisations - MMR ----
save_final_file("mmr_alldose", "31_Aug_22")
save_final_file("mmr_dose1_simdtable", "31_Aug_22")
save_final_file("mmr_dose2_simdtable", "31_Aug_22")
save_final_file("mmr_datatable", "31_Aug_22")
save_final_file("mmr_dose2_grampian_datatable", "31_Aug_22")
# Immunisations - definitions
save_final_file("age_defs_imm_6inone", "31_Aug_22")
save_final_file("age_defs_imm_mmr", "31_Aug_22")
save_final_file("month_eligibility_immun", "31_Aug_22")
##########################################################.
ch_date <- "30_Aug_22"
## Child health reviews - first visit ----
save_final_file("first_visit", ch_date)
save_final_file("first_visit_datatable", ch_date)
save_final_file("first_visit_data", ch_date)
## Child health reviews - 6-8 weeks ----
save_final_file("six_to_eight", ch_date)
save_final_file("six_to_eight_datatable", ch_date)
save_final_file("six_to_eight_data", ch_date)
## Child health reviews - 13-15 months ----
save_final_file("thirteen", ch_date)
save_final_file("thirteen_datatable", ch_date)
save_final_file("thirteen_data", ch_date)
## Child health reviews - 27-30 months ----
save_final_file("twentyseven", ch_date)
save_final_file("twentyseven_datatable", ch_date)
save_final_file("twentyseven_data", ch_date)
## Child health reviews - 4-5 years ----
save_final_file("fourtofive", ch_date)
save_final_file("fourtofive_datatable", ch_date)
save_final_file("fourtofive_data", ch_date)
## Child development ----
save_final_file("child_dev", ch_date)
save_final_file("child_dev_depr", ch_date)
save_final_file("child_dev_domains", ch_date)
## Breastfeeding ----
save_final_file("breastfeeding", ch_date)
##########################################################.
## Pregnancy - Antenatal bookings ----
save_final_file("ante_booking", "22_Aug_22")
save_final_file("ante_booking_download", "22_Aug_22")
## Pregnancy - Terminations ----
save_final_file("top", "16_Aug_22")
save_final_file("top_download", "16_Aug_22")
##########################################################.
births_date <- "22_Aug_22"
## B&B - Mode/Method of Delivery ----
save_final_file("mod_runchart_data", births_date)
save_final_file("mod_scot_data", births_date)
save_final_file("mod_linechart_data", births_date)
save_final_file("mod_download_data", births_date)
## B&B - Inductions ----
save_final_file("induct_runchart_data", births_date)
save_final_file("induct_scot_data", births_date)
save_final_file("induct_linechart_data", births_date)
save_final_file("induct_download_data", births_date)
## B&B - Gestation at Delivery ----
save_final_file("gestation_runchart_data", births_date)
save_final_file("gestation_scot_data", births_date)
save_final_file("gestation_linechart_data", births_date)
save_final_file("gestation_download_data", births_date)
## B&B - Apgar ----
save_final_file("apgar_runchart_data", births_date)
save_final_file("apgar_scot_data", births_date)
save_final_file("apgar_linechart_data", births_date)
save_final_file("apgar_download_data", births_date)
## B&B - Preterm ----
save_final_file("preterm_linechart_data", births_date)
save_final_file("preterm", births_date)
## B&B - Tears ----
save_final_file("tears_runchart_data", births_date)
save_final_file("tears_scot_data", births_date)
save_final_file("tears_linechart_data", births_date)
save_final_file("tears_download_data", births_date)

## B&B - Stillbirths and perinatal mortality ----
save_final_file("perinatal", "29_Aug_22")
##########################################################.
## Mental health  ----
save_final_file("mentalhealth_drugs", "05_Sep_22") # Prescribing
save_final_file("mh_A&E", "05_Sep_22") # A&E
save_final_file("mh_ooh", "01_Aug_22") # OOH
##########################################################.
## Drugs - substance use ----

save_final_file("ADP_names", "25_Mar_22")
save_final_file("Health_board", "25_Mar_22")
save_final_file("DTR_data", "15_Jun_22")
save_final_file("THN_by_HB", "27_May_22")
save_final_file("OST_paid", "15_Jun_22")
save_final_file("OST_paid_quantity", "15_Jun_22")
save_final_file('SASdata', "15_Jun_22")
save_final_file('Drug_AE_attendances', "07_Jun_22")

##END

