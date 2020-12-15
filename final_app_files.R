# This script reads the latest version of each file and saves it in your local repository.
###############################################.
## Functions ----
###############################################.
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
deaths <- readRDS(paste0(data_folder, "deaths/deaths_data.rds"))
saveRDS(deaths, "shiny_app/data/deaths.rds")
saveRDS(deaths, paste0(open_data, "deaths_data.rds"))
##########################################################.
## Cancer ----
cancer <- readRDS(paste0(data_folder,"cancer/cancer_data_2.rds"))
saveRDS(cancer, "shiny_app/data/cancer_data_2.rds")
##########################################################.
## RAPID ----
save_final_file("rapid", "30_Nov_20")
save_final_file("spec_lookup", "30_Nov_20")
## OOH ----
save_final_file("ooh", "30_Nov_20")
## A&E ----
save_final_file("ae", "30_Nov_20")
## NHS24 ----
save_final_file("nhs24", "30_Nov_20")
## SAS ----
save_final_file("sas", "30_Nov_20")
##########################################################.
## Cardio - cath labs ----
save_final_file("cath_lab", "16_Nov_20")
## Cardio - A&E ----
save_final_file("ae_cardio", "30_Nov_20")
save_final_file("ae_cardio_codes", "30_Nov_20")
## Cardio - OOH ----
save_final_file("ooh_cardiac", "09_Dec_20")
## Cardio - SAS ----
save_final_file("sas_cardiac", "08_Dec_20")
## Cardio - Prescribing ----
save_final_file("cardio_drugs", "30_Nov_20")
##########################################################.
## Immunisations - six in one ----
save_final_file("six_alldose", "16_Nov_20")
save_final_file("six_dose1_simdtable", "16_Nov_20")
save_final_file("six_dose2_simdtable", "16_Nov_20")
save_final_file("six_dose3_simdtable", "16_Nov_20")
save_final_file("sixinone_datatable", "16_Nov_20")
## Immunisations - MMR ----
save_final_file("mmr_alldose", "16_Nov_20")
save_final_file("mmr_dose1_simdtable", "16_Nov_20")
save_final_file("mmr_dose2_simdtable", "16_Nov_20")
save_final_file("mmr_datatable", "16_Nov_20")
save_final_file("mmr_dose2_datatable_grampian", "16_Nov_20")
# Immunisations - definitions
save_final_file("age_defs_imm_6inone", "16_Nov_20")
save_final_file("age_defs_imm_mmr", "16_Nov_20")
save_final_file("month_eligibility_immun", "16_Nov_20")
##########################################################.
## Child health reviews - first visit ----
save_final_file("first_visit", "15_Dec_20")
save_final_file("first_visit_datatable", "15_Dec_20")
## Child health reviews - 6-8 weeks ----
save_final_file("six_to_eight", "15_Dec_20")
save_final_file("six_to_eight_datatable", "15_Dec_20")
## Child health reviews - 13-15 months ----
save_final_file("thirteen", "15_Dec_20")
save_final_file("thirteen_datatable", "15_Dec_20")
## Child health reviews - 27-30 months ----
save_final_file("twentyseven", "15_Dec_20")
save_final_file("twentyseven_datatable", "15_Dec_20")
## Child health reviews - 4-5 years ----
save_final_file("fourtofive", "15_Dec_20")
save_final_file("fourtofive_datatable", "15_Dec_20")
## Child development ----
save_final_file("child_dev", "15_Dec_20")
## Breastfeeding ----
save_final_file("breastfeeding", "15_Dec_20")
##########################################################.
## Stillbirths and perinatal mortality ----
save_final_file("perinatal", "18_Nov_20")
##########################################################.
## Pregnancy - Antenatal bookings ----
save_final_file("ante_booking", "24_Nov_20")
save_final_file("ante_booking_download", "24_Nov_20")
## Pregnancy - Terminations ----
save_final_file("top", "20_Nov_20")
save_final_file("top_download", "20_Nov_20")
## Pregnancy - Mode/Method of Delivery ----
save_final_file("mod_runchart_data", "01_Dec_20")
save_final_file("mod_scot_data", "01_Dec_20")
save_final_file("mod_linechart_data", "01_Dec_20")
save_final_file("mod_download_data", "01_Dec_20")
## Pregnancy - Inductions ----
save_final_file("induct_runchart_data", "01_Dec_20")
save_final_file("induct_scot_data", "01_Dec_20")
save_final_file("induct_linechart_data", "01_Dec_20")
save_final_file("induct_download_data", "01_Dec_20")
## Pregnancy - Gestation at Delivery ----
save_final_file("gestation_runchart_data", "01_Dec_20")
save_final_file("gestation_scot_data", "01_Dec_20")
save_final_file("gestation_linechart_data", "01_Dec_20")
save_final_file("gestation_download_data", "01_Dec_20")
##########################################################.
## Mental health - Prescribing ----
save_final_file("mentalhealth_drugs", "30_Nov_20")
## Mental health - A&E ----
save_final_file("mh_A&E", "30_Nov_20")
## Mental health - OOH ----
save_final_file("mh_ooh", "30_Nov_20")

##END