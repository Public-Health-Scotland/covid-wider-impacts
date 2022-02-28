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
save_final_file("deaths", "31_Jan_22")
##########################################################.
## Cancer ----
save_final_file("cancer_data_2", "10_Sep_21")
save_final_file("cancer_data_diff", "10_Sep_21")

##########################################################.
## SACT ----
save_final_file("sact_data", "28_Feb_22")

save_final_file("sact_weekly_data", "28_Feb_22")

##########################################################.
## DCE ----
# save_final_file("dce_data", "18_Oct_21")


##########################################################.
##########################################################.
## Injuries ----
save_final_file("ui_smr01_all", "31_Jan_22")
save_final_file("ui_smr01_rta", "31_Jan_22")
save_final_file("ui_smr01_poison", "31_Jan_22")
save_final_file("ui_smr01_other", "31_Jan_22")
save_final_file("ui_smr01_falls", "31_Jan_22")
save_final_file("ui_smr01_assaults", "31_Jan_22")

##########################################################.
## RAPID ----
save_final_file("rapid", "31_Jan_22")
save_final_file("spec_lookup_rapid", "09_Mar_21")
## OOH ----
save_final_file("ooh", "31_Jan_22")
## A&E ----
save_final_file("ae", "31_Jan_22")
## NHS24 ----
save_final_file("nhs24", "31_Jan_22")
## SAS ----
save_final_file("sas", "31_Jan_22")
### Outpatients ----
save_final_file("outpats", "03_Dec_21")
save_final_file("outpats_ethnicity", "28_Jan_22")
save_final_file("ethnicity_lookup", "14_Feb_22")
save_final_file("spec_lookup_op", "24_Mar_21")
save_final_file("area_type_op", "24_Mar_21")
##########################################################.
## Cardio - cath labs ----
save_final_file("cath_lab", "16_Nov_20")
## Cardio - A&E ----
save_final_file("ae_cardio", "31_Jan_22")
save_final_file("ae_cardio_codes", "30_Nov_20")
## Cardio - OOH ----
save_final_file("ooh_cardiac", "31_Jan_22")
## Cardio - SAS ----
save_final_file("sas_cardiac", "29_Nov_21")
## Cardio - Prescribing ----
save_final_file("cardio_drugs", "31_Jan_22")
##########################################################.
## Immunisations - six in one ----
save_final_file("six_alldose", "24_Feb_22")
save_final_file("six_dose1_simdtable", "24_Feb_22")
save_final_file("six_dose2_simdtable", "24_Feb_22")
save_final_file("six_dose3_simdtable", "24_Feb_22")
save_final_file("sixinone_datatable", "24_Feb_22")
## Immunisations - MMR ----
save_final_file("mmr_alldose", "24_Feb_22")
save_final_file("mmr_dose1_simdtable", "24_Feb_22")
save_final_file("mmr_dose2_simdtable", "24_Feb_22")
save_final_file("mmr_datatable", "24_Feb_22")
save_final_file("mmr_dose2_grampian_datatable", "24_Feb_22")
# Immunisations - definitions
save_final_file("age_defs_imm_6inone", "24_Feb_22")
save_final_file("age_defs_imm_mmr", "24_Feb_22")
save_final_file("month_eligibility_immun", "24_Feb_22")
##########################################################.
## Child health reviews - first visit ----
save_final_file("first_visit", "27_Jan_22")
save_final_file("first_visit_datatable", "27_Jan_22")
save_final_file("first_visit_data", "27_Jan_22")
## Child health reviews - 6-8 weeks ----
save_final_file("six_to_eight", "27_Jan_22")
save_final_file("six_to_eight_datatable", "27_Jan_22")
save_final_file("six_to_eight_data", "27_Jan_22")
## Child health reviews - 13-15 months ----
save_final_file("thirteen", "27_Jan_22")
save_final_file("thirteen_datatable", "27_Jan_22")
save_final_file("thirteen_data", "27_Jan_22")
## Child health reviews - 27-30 months ----
save_final_file("twentyseven", "27_Jan_22")
save_final_file("twentyseven_datatable", "27_Jan_22")
save_final_file("twentyseven_data", "27_Jan_22")
## Child health reviews - 4-5 years ----
save_final_file("fourtofive", "27_Jan_22")
save_final_file("fourtofive_datatable", "27_Jan_22")
save_final_file("fourtofive_data", "27_Jan_22")
## Child development ----
save_final_file("child_dev", "27_Jan_22")
save_final_file("child_dev_depr", "27_Jan_22")
## Breastfeeding ----
save_final_file("breastfeeding", "27_Jan_22")
##########################################################.
## Pregnancy - Antenatal bookings ----
save_final_file("ante_booking", "02_Feb_22")
save_final_file("ante_booking_download", "02_Feb_22")
## Pregnancy - Terminations ----
save_final_file("top", "02_Feb_22")
save_final_file("top_download", "02_Feb_22")
##########################################################.
## B&B - Mode/Method of Delivery ----
save_final_file("mod_runchart_data", "22_Feb_22")
save_final_file("mod_scot_data", "22_Feb_22")
save_final_file("mod_linechart_data", "22_Feb_22")
save_final_file("mod_download_data", "22_Feb_22")
## B&B - Inductions ----
save_final_file("induct_runchart_data", "22_Feb_22")
save_final_file("induct_scot_data", "22_Feb_22")
save_final_file("induct_linechart_data", "22_Feb_22")
save_final_file("induct_download_data", "22_Feb_22")
## B&B - Gestation at Delivery ----
save_final_file("gestation_runchart_data", "22_Feb_22")
save_final_file("gestation_scot_data", "22_Feb_22")
save_final_file("gestation_linechart_data", "22_Feb_22")
save_final_file("gestation_download_data", "22_Feb_22")
## B&B - Stillbirths and perinatal mortality ----
save_final_file("perinatal", "24_Jan_22")
## B&B - Apgar ----
save_final_file("apgar_runchart_data", "02_Feb_22")
save_final_file("apgar_scot_data", "02_Feb_22")
save_final_file("apgar_linechart_data", "02_Feb_22")
save_final_file("apgar_download_data", "02_Feb_22")
## B&B - Preterm ----
save_final_file("preterm_linechart_data", "02_Feb_22")
save_final_file("preterm", "02_Feb_22")
## B&B - Tears ----
save_final_file("tears_runchart_data", "02_Feb_22")
save_final_file("tears_scot_data", "02_Feb_22")
save_final_file("tears_linechart_data", "02_Feb_22")
save_final_file("tears_download_data", "02_Feb_22")
##########################################################.
## Mental health - Prescribing ----
save_final_file("mentalhealth_drugs", "31_Jan_22")
## Mental health - A&E ----
save_final_file("mh_A&E", "31_Jan_22")
## Mental health - OOH ----
save_final_file("mh_ooh", "31_Jan_22")
##########################################################.
## Drugs - substance use ----
save_final_file("ADP_names", "26_Nov_21")
save_final_file("Health_board", "26_Nov_21")
save_final_file("DTR_data", "26_Nov_21")
save_final_file("THN_by_HB", "26_Nov_21")
save_final_file("OST_paid", "26_Nov_21")
save_final_file("OST_paid_quantity", "26_Nov_21")

# save_final_file("ADP_names_", format(Sys.Date(), format = '%d_%b_%y'))
# save_final_file("Health_board_", format(Sys.Date(), format = '%d_%b_%y'))
# save_final_file("DTR_data_", format(Sys.Date(), format = '%d_%b_%y'))
# save_final_file("THN_by_HB_", format(Sys.Date(), format = '%d_%b_%y'))
# save_final_file('SASdata_',format(Sys.Date(), format = '%d_%b_%y'))
# save_final_file('OST_paid_',format(Sys.Date(), format = '%d_%b_%y'))
# save_final_file('OST_paid_quantity_',format(Sys.Date(), format = '%d_%b_%y'))


##END

