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
save_final_file("deaths", "02_Aug_21")
##########################################################.
## Cancer ----
save_final_file("cancer_data_2", "27_Jul_21")

##########################################################.
## SACT ----
save_final_file("sact_data", "23_Aug_21")

save_final_file("sact_weekly_data", "23_Aug_21")

##########################################################.
## RAPID ----
save_final_file("rapid", "02_Aug_21")
save_final_file("spec_lookup_rapid", "09_Mar_21")

## OOH ----
save_final_file("ooh", "02_Aug_21")
## A&E ----
save_final_file("ae", "02_Aug_21")
## NHS24 ----
save_final_file("nhs24", "02_Aug_21")
## SAS ----
save_final_file("sas", "02_Aug_21")
### Outpatients ----
save_final_file("outpats", "17_May_21")
save_final_file("spec_lookup_op", "24_Mar_21")
save_final_file("area_type_op", "24_Mar_21")
##########################################################.
## Cardio - cath labs ----
save_final_file("cath_lab", "16_Nov_20")
## Cardio - A&E ----
save_final_file("ae_cardio", "02_Aug_21")
save_final_file("ae_cardio_codes", "30_Nov_20")
## Cardio - OOH ----
save_final_file("ooh_cardiac", "02_Aug_21")
## Cardio - SAS ----
save_final_file("sas_cardiac", "02_Aug_21")
## Cardio - Prescribing ----
save_final_file("cardio_drugs", "02_Aug_21")
##########################################################.
## Immunisations - six in one ----
save_final_file("six_alldose", "26_Aug_21")
save_final_file("six_dose1_simdtable", "26_Aug_21")
save_final_file("six_dose2_simdtable", "26_Aug_21")
save_final_file("six_dose3_simdtable", "26_Aug_21")
save_final_file("sixinone_datatable", "26_Aug_21")
## Immunisations - MMR ----
save_final_file("mmr_alldose", "26_Aug_21")
save_final_file("mmr_dose1_simdtable", "26_Aug_21")
save_final_file("mmr_dose2_simdtable", "26_Aug_21")
save_final_file("mmr_datatable", "26_Aug_21")
save_final_file("mmr_dose2_grampian_datatable", "26_Aug_21")
# Immunisations - definitions
save_final_file("age_defs_imm_6inone", "26_Aug_21")
save_final_file("age_defs_imm_mmr", "26_Aug_21")
save_final_file("month_eligibility_immun", "26_Aug_21")
##########################################################.
## Child health reviews - first visit ----
save_final_file("first_visit", "28_Jul_21")
save_final_file("first_visit_datatable", "28_Jul_21")
save_final_file("first_visit_data", "28_Jul_21")
## Child health reviews - 6-8 weeks ----
save_final_file("six_to_eight", "28_Jul_21")
save_final_file("six_to_eight_datatable", "28_Jul_21")
save_final_file("six_to_eight_data", "28_Jul_21")
## Child health reviews - 13-15 months ----
save_final_file("thirteen", "28_Jul_21")
save_final_file("thirteen_datatable", "28_Jul_21")
save_final_file("thirteen_data", "28_Jul_21")
## Child health reviews - 27-30 months ----
save_final_file("twentyseven", "28_Jul_21")
save_final_file("twentyseven_datatable", "28_Jul_21")
save_final_file("twentyseven_data", "28_Jul_21")
## Child health reviews - 4-5 years ----
save_final_file("fourtofive", "28_Jul_21")
save_final_file("fourtofive_datatable", "28_Jul_21")
save_final_file("fourtofive_data", "28_Jul_21")
## Child development ----
save_final_file("child_dev", "28_Jul_21")
## Breastfeeding ----
save_final_file("breastfeeding", "28_Jul_21")
##########################################################.
## Pregnancy - Antenatal bookings ----
save_final_file("ante_booking", "23_Aug_21")
save_final_file("ante_booking_download", "23_Aug_21")
## Pregnancy - Terminations ----
save_final_file("top", "18_Aug_21")
save_final_file("top_download", "18_Aug_21")
##########################################################.
## B&B - Mode/Method of Delivery ----
save_final_file("mod_runchart_data", "18_Aug_21")
save_final_file("mod_scot_data", "18_Aug_21")
save_final_file("mod_linechart_data", "18_Aug_21")
save_final_file("mod_download_data", "18_Aug_21")
## B&B - Inductions ----
save_final_file("induct_runchart_data", "18_Aug_21")
save_final_file("induct_scot_data", "18_Aug_21")
save_final_file("induct_linechart_data", "18_Aug_21")
save_final_file("induct_download_data", "18_Aug_21")
## B&B - Gestation at Delivery ----
save_final_file("gestation_runchart_data", "18_Aug_21")
save_final_file("gestation_scot_data", "18_Aug_21")
save_final_file("gestation_linechart_data", "18_Aug_21")
save_final_file("gestation_download_data", "18_Aug_21")
## B&B - Stillbirths and perinatal mortality ----
save_final_file("perinatal", "26_Aug_21")
## B&B - Apgar ----
save_final_file("apgar_runchart_data", "24_Aug_21")
save_final_file("apgar_scot_data", "24_Aug_21")
save_final_file("apgar_linechart_data", "24_Aug_21")
save_final_file("apgar_download_data", "24_Aug_21")
## B&B - Preterm ----
save_final_file("preterm_linechart_data", "28_Jun_21")
save_final_file("preterm", "28_Jun_21")
## B&B - Tears ----
save_final_file("tears_runchart_data", "24_Aug_21")
save_final_file("tears_scot_data", "24_Aug_21")
save_final_file("tears_linechart_data", "24_Aug_21")
save_final_file("tears_download_data", "24_Aug_21")
##########################################################.
## Mental health - Prescribing ----
save_final_file("mentalhealth_drugs", "30_Jul_21")
## Mental health - A&E ----
save_final_file("mh_A&E", "02_Aug_21")
## Mental health - OOH ----
save_final_file("mh_ooh", "05_Jul_21")
##########################################################.
## Drugs - substance use ----
save_final_file("ADP_names", "27_Aug_21")
save_final_file("Health_board", "27_Aug_21")
save_final_file("DTR_July_update", "27_Aug_21")
save_final_file("THN_by_HB", "27_Aug_21")


##END
