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
deaths <- readRDS(paste0(data_folder, "deaths/deaths_data.rds"))
saveRDS(deaths, "shiny_app/data/deaths.rds")
saveRDS(deaths, paste0(open_data, "deaths_data.rds"))
##########################################################.
## Cancer ----
save_final_file("cancer_data_2", "18_Dec_20")
##########################################################.
## RAPID ----
save_final_file("rapid", "01_Mar_21")
save_final_file("spec_lookup", "01_Mar_21")
## OOH ----
save_final_file("ooh", "01_Mar_21")
## A&E ----
save_final_file("ae", "01_Mar_21")
## NHS24 ----
save_final_file("nhs24", "01_Mar_21")
## SAS ----
save_final_file("sas", "01_Mar_21")
##########################################################.
## Cardio - cath labs ----
save_final_file("cath_lab", "16_Nov_20")
## Cardio - A&E ----
save_final_file("ae_cardio", "01_Mar_21")
save_final_file("ae_cardio_codes", "30_Nov_20")
## Cardio - OOH ----
save_final_file("ooh_cardiac", "01_Mar_21")
## Cardio - SAS ----
save_final_file("sas_cardiac", "01_Mar_21")
## Cardio - Prescribing ----
save_final_file("cardio_drugs", "01_Mar_21")
##########################################################.
## Immunisations - six in one ----
save_final_file("six_alldose", "24_Feb_21")
save_final_file("six_dose1_simdtable", "24_Feb_21")
save_final_file("six_dose2_simdtable", "24_Feb_21")
save_final_file("six_dose3_simdtable", "24_Feb_21")
save_final_file("sixinone_datatable", "24_Feb_21")
## Immunisations - MMR ----
save_final_file("mmr_alldose", "24_Feb_21")
save_final_file("mmr_dose1_simdtable", "24_Feb_21")
save_final_file("mmr_dose2_simdtable", "24_Feb_21")
save_final_file("mmr_datatable", "24_Feb_21")
save_final_file("mmr_dose2_grampian_datatable", "24_Feb_21")
# Immunisations - definitions
save_final_file("age_defs_imm_6inone", "24_Feb_21")
save_final_file("age_defs_imm_mmr", "24_Feb_21")
save_final_file("month_eligibility_immun", "24_Feb_21")
##########################################################.
## Child health reviews - first visit ----
save_final_file("first_visit", "01_Mar_21")
save_final_file("first_visit_datatable", "01_Mar_21")
## Child health reviews - 6-8 weeks ----
save_final_file("six_to_eight", "01_Mar_21")
save_final_file("six_to_eight_datatable", "01_Mar_21")
## Child health reviews - 13-15 months ----
save_final_file("thirteen", "01_Mar_21")
save_final_file("thirteen_datatable", "01_Mar_21")
## Child health reviews - 27-30 months ----
save_final_file("twentyseven", "01_Mar_21")
save_final_file("twentyseven_datatable", "01_Mar_21")
## Child health reviews - 4-5 years ----
save_final_file("fourtofive", "01_Mar_21")
save_final_file("fourtofive_datatable", "01_Mar_21")
## Child development ----
save_final_file("child_dev", "01_Mar_21")
## Breastfeeding ----
save_final_file("breastfeeding", "01_Mar_21")
##########################################################.
## Pregnancy - Antenatal bookings ----
save_final_file("ante_booking", "15_Feb_21")
save_final_file("ante_booking_download", "15_Feb_21")
## Pregnancy - Terminations ----
save_final_file("top", "24_Feb_21")
save_final_file("top_download", "24_Feb_21")
##########################################################.
## B&B - Mode/Method of Delivery ----
save_final_file("mod_runchart_data", "24_Feb_21")
save_final_file("mod_scot_data", "24_Feb_21")
save_final_file("mod_linechart_data", "24_Feb_21")
save_final_file("mod_download_data", "24_Feb_21")
## B&B - Inductions ----
save_final_file("induct_runchart_data", "24_Feb_21")
save_final_file("induct_scot_data", "24_Feb_21")
save_final_file("induct_linechart_data", "24_Feb_21")
save_final_file("induct_download_data", "24_Feb_21")
## B&B - Gestation at Delivery ----
save_final_file("gestation_runchart_data", "24_Feb_21")
save_final_file("gestation_scot_data", "24_Feb_21")
save_final_file("gestation_linechart_data", "24_Feb_21")
save_final_file("gestation_download_data", "24_Feb_21")
## B&B - Stillbirths and perinatal mortality ----
save_final_file("perinatal", "17_Feb_21")
## B&B - Apgar ----
save_final_file("apgar_runchart_data", "15_Mar_21")
save_final_file("apgar_scot_data", "11_Mar_21")
save_final_file("apgar_linechart_data", "15_Mar_21")
save_final_file("apgar_download_data", "11_Mar_21")
## B&B - Preterm ----
save_final_file("preterm_linechart_data", "11_Mar_21")
save_final_file("preterm", "15_Mar_21")
##########################################################.
## Mental health - Prescribing ----
save_final_file("mentalhealth_drugs", "01_Mar_21")
## Mental health - A&E ----
save_final_file("mh_A&E", "01_Mar_21")
## Mental health - OOH ----
save_final_file("mh_ooh", "01_Mar_21")

##END
