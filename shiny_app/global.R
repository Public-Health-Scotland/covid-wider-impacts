# Global

###############################################.
## Packages ----
###############################################.

library(shiny)
library(plotly) # for charts
library(shinyWidgets) # for dropdowns  
library(dplyr) # for data manipulation 
library(DT) # for data table  
library(shinycssloaders) #for loading icons, see line below 
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app
library(shinyjs) # for enable/disable functions  
library(readr) # for writing/reading csvs
library(stringr) #for manipulating strings
library(flextable)
library(shinyBS) #for collapsible panels in commentary 
library(zoo)
library(magrittr)
library(shinymanager) 

###############################################.
## Functions ----
###############################################.
plot_box <- function(title_plot, plot_output) {
  tagList(h4(title_plot),
          withSpinner(plotlyOutput(plot_output)))
}

plot_cut_box <- function(title_plot1, plot_output1,
                         title_plot2, plot_output2, extra_content = NULL) {
  tagList(
    fluidRow(column(6, h4(title_plot1)),
             column(6, h4(title_plot2))),
    extra_content,
    fluidRow(column(6, withSpinner(plotlyOutput(plot_output1))),
             column(6, withSpinner(plotlyOutput(plot_output2))))
    )
}

#Function to create boxes for intro sumamry
#Creating big boxes for main tabs in the landing page (see ui for formatting css)
intro_box <- function(title_box, button_name, description) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      actionButton(button_name, NULL, class="landing-page-button")
      )
}

###############################################.
## Data ----
###############################################.
geo_lookup <- readRDS("data/geo_lookup.rds")
area_type_op <- readRDS("data/area_type_op.rds")
spec_lookup_rapid <- readRDS("data/spec_lookup.rds")
spec_lookup_op <- readRDS("data/spec_lookup_op.rds")
ae_cardio_codes <- readRDS("data/ae_cardio_codes.rds")

rapid <- readRDS("data/rapid.rds") #RAPID data
aye <- readRDS("data/ae.rds") #A&E data
ooh <- readRDS("data/ooh.rds") # OOH data
nhs24 <- readRDS("data/nhs24.rds") # OOH data
sas <- readRDS("data/sas.rds") # OOH data
deaths <- readRDS("data/deaths.rds") # deaths data
outpats <- readRDS("data/outpats.rds") # outpatients data

#Cardiovascular data
ae_cardio <- readRDS("data/ae_cardio.rds") # A&E cardio data
cardio_drugs <- readRDS("data/cardio_drugs.rds") # Cardio drugs data
cath_lab <- readRDS("data/cath_lab.rds") # Cath lab data
ooh_cardiac <-  readRDS("data/ooh_cardiac.rds") # OOH cardiac data
sas_cardiac <-  readRDS("data/sas_cardiac.rds") # SAS cardiac data

#Cancer data
cancer_data2 <- readRDS("data/cancer_data_2.rds")
# cancer_data3 <- readRDS("data/cancer_data_dep.rds")
cancer_extract_date <- "22nd February 2021"

# mental health data
mentalhealth_drugs <- readRDS("data/mentalhealth_drugs.rds")
ae_mh <- readRDS("data/mh_A&E.rds")
mh_ooh <- readRDS("data/mh_ooh.rds")

## Child Health Data
child_extract_date <- "22nd February 2021"
first <- readRDS("data/first_visit.rds") # first health visit at 2 weeks
firsttable <- readRDS("data/first_visit_datatable.rds")
sixtoeight <- readRDS("data/six_to_eight.rds")
sixtoeighttable <- readRDS("data/six_to_eight_datatable.rds")
thirteen <- readRDS("data/thirteen.rds")
thirteentable <- readRDS("data/thirteen_datatable.rds")
twentyseven <- readRDS("data/twentyseven.rds")
twentyseventable <- readRDS("data/twentyseven_datatable.rds")
fourtofive <- readRDS("data/fourtofive.rds")
fourtofivetable <- readRDS("data/fourtofive_datatable.rds")

## Immunisation Data
immunisation_extract_date <- "22nd February 2021"
month_elig_imm <- readRDS("data/month_eligibility_immun.rds") #flextable with imm month eligibility
age_defs_imm_6inone <- readRDS("data/age_defs_imm_6inone.rds")
age_defs_imm_mmr <- readRDS("data/age_defs_imm_mmr.rds")

#Immunisations s-curve data
six_alldose <- readRDS("data/six_alldose.rds")
mmr_alldose <- readRDS("data/mmr_alldose.rds") # mmr immunisation scurve data for all doses

#Immunisations data table data
sixtable <- readRDS("data/sixinone_datatable.rds") # 6-in-1 summary table (all dose)
mmrtable <- readRDS("data/mmr_datatable.rds") # mmr summary table (all dose)

#data quality issues require additional data file for NHS grampian
mmrtable_dose2_gramp <- readRDS("data/mmr_dose2_grampian_datatable.rds") # mmr immunisation data table summary for just grampian mmr dose 2

#Immunisations SIMD data
six_simd_dose1 <- readRDS("data/six_dose1_simdtable.rds")
six_simd_dose2 <- readRDS("data/six_dose2_simdtable.rds")
six_simd_dose3 <- readRDS("data/six_dose3_simdtable.rds")
mmr_simd_dose1 <- readRDS("data/mmr_dose1_simdtable.rds")
mmr_simd_dose2 <- readRDS("data/mmr_dose2_simdtable.rds")

# perinatal mortality data
perinatal <- readRDS("data/perinatal.rds")

#Pregnancy tab
#antenatal booking

booking_extract_date <- "11th February 2021"
booking <- readRDS("data/ante_booking.rds")
booking_download <- readRDS("data/ante_booking_download.rds")

#terminations
top_extract_date <- "9th February 2021"
top <- readRDS("data/top.rds")
top_download <- readRDS("data/top_download.rds")

#mode of delivery (pregnanacy tab)
mod_extract_date <- "17th March 2021"
mod_runchart <- readRDS("data/mod_runchart_data.rds")
mod_scot <- readRDS("data/mod_scot_data.rds")
mod_linechart <- readRDS("data/mod_linechart_data.rds")
mod_download <- readRDS("data/mod_download_data.rds")

#inductions (pregnanacy tab)
induct_extract_date <- "17th March 2021"
induct_runchart <- readRDS("data/induct_runchart_data.rds")
induct_scot <- readRDS("data/induct_scot_data.rds")
induct_linechart <- readRDS("data/induct_linechart_data.rds")
induct_download <- readRDS("data/induct_download_data.rds")

#gestation at delivery (pregnanacy tab)
gestation_extract_date <- "17th March 2021"
gestation_runchart <- readRDS("data/gestation_runchart_data.rds")
gestation_scot <- readRDS("data/gestation_scot_data.rds")
gestation_linechart <- readRDS("data/gestation_linechart_data.rds")
gestation_download <- readRDS("data/gestation_download_data.rds")

# Breastfeeding data
breastfeeding <- readRDS("data/breastfeeding.rds")
#Child development data
child_dev <- readRDS("data/child_dev.rds")

###############################################.
## Objects, names, lists ----
###############################################.

spec_list_rapid <- sort(c(unique(spec_lookup_rapid$'Specialty group'),
                          "Medical (incl. Cardiology & Cancer)",
                          "Paediatrics (medical & surgical)")) # specialty list
spec_list_op <- sort(c(unique(spec_lookup_op$Grouping))) # specialty list

data_list <- c(
  "Hospital admissions" = "rapid", "A&E attendances" = "aye",
               "NHS 24 completed contacts" = "nhs24",
               "Out of hours cases" = "ooh", "Scottish Ambulance Service" = "sas",
               "Excess mortality" = "deaths",
               "Outpatient appointments" = "outpats")

#List of data items available in step 2 of immunisation tab
data_list_immun <- c("6-in-1 first dose" = "sixin_dose1",
                     "6-in-1 second dose" = "sixin_dose2",
                     "6-in-1 third dose" = "sixin_dose3",
                     "MMR first dose" = "mmr_dose1",
                     "MMR second dose" = "mmr_dose2")

# List of data items available in step 2 of child health tab
data_list_child <- c("Health Visitor first visit" = "first_visit",
            "6-8 Week Review" = "six_eightwks",
            "13-15 Month Review" = "13_15mnth",
            "27-30 Month Review" = "27_30mnth",
            "4-5 Year Review" = "4_5yr")

## Data lists for pregnancy tab
# List of data items available in step 1 of antenatal booking
data_list_booking <- c("Number" = "booking_number",
                    "Average gestation" = "booking_gestation")
# List of data items available in step 1 of terminations
data_list_top <- c("Number" = "top_number",
                       "Average gestation" = "top_gestation")

data_list_childdev <- c("13-15 month review" = "13_15mnth",
                     "27-30 month review" = "27_30mnth")

data_list_data_tab <- c(data_list, "Cardiovascular prescribing" = "cardio_drugs",
                        "A&E cardiovascular attendances" = "ae_cardio",
                        "Cardiac procedures" = "cath_lab",
                        "Cardiovascular OOH cases" = "ooh_cardiac",
                        "Cardiovascular SAS incidents" = "sas_cardiac",
                        "6-in-1 first dose"  = "sixin_8wks",
                        "6-in-1 second dose" = "sixin_8wks_second",
                        "6-in-1 third dose" = "sixin_8wks_third",
                        "Health Visitor first visit" = "first_visit",
                        "6-8 week child health review" = "sixtoeight_visit",
                        "13-15 month child health review" = "thirteen_visit",
                        "27-30 month child health review" = "twentyseven_visit",
                        "4-5 year child health review" = "fourtofive_visit",
                        "Child development" = "childdev",
                        "Breastfeeding" = "breastfeeding",
                        "Stillbirths and infant deaths" = "perinatal",
                        "Termination of pregnancy" = "top",
                        "Antenatal bookings" = "ante_booking",
                        "Induction of labour" = "induct",
                        "Method of delivery" = "mod",
                        "Gestation at delivery" = "gestation",
                        "Mental health prescribing" = "mhdrugs",
                        "A&E mental health attendances" = "ae_mh",
                        "Out of hours mental health cases" = "ooh_mh",
                        "Cancer" = "cancer")

cancer_type_list <- c("All Malignant Neoplasms (Excl. C44)" = "All Malignant Neoplasms (Excl. C44)",
                      "All Cancers" = "All Cancers",
                      "Bladder" = "Bladder",
                      "Bone and Connective Tissue" = "Bone and Connective Tissue",
                      "Brain Tumour" = "Brain Tumour",
                      "Breast" = "Breast", 
                      "Colorectal" = "Colorectal",
                      "Head and Neck" = "Head and Neck",
                      "Hodgkin Lymphoma" = "Hodgkin Lymphoma",
                      "Kidney" = "Kidney",
                      "Leukaemias" = "Leukaemias",
                      "Liver and Intrahepatic Bile Ducts" = "Liver and Intrahepatic Bile Ducts",
                      "Malignant Melanoma of the Skin" = "Malignant Melanoma of the Skin",
                      "Mesothelioma" = "Mesothelioma",
                      "Multiple Myeloma and malignant plasma cell neoplasms" = "Multiple Myeloma and malignant plasma cell neoplasms",
                      "Non-Melanoma Skin Cancer" = "Non-Melanoma Skin Cancer",
                      "Oesophagus" = "Oesophagus",
                      "Other" = "Other",
                      "Ovary - Females only" = "Ovary - Females only",
                      "Pancreas" = "Pancreas",
                      "Penis - Males only" = "Penis - Males only",
                      "Prostate - Males only" = "Prostate - Males only",
                      "Stomach" = "Stomach",
                      "Testis - Males only" = "Testis - Males only",
                      "Thyroid" = "Thyroid",
                      "Trachea, Bronchus and Lung" = "Trachea, Bronchus and Lung",
                      "Uterus - Females only" = "Uterus - Females only",
                      "Vagina - Females only" = "Vagina - Females only",
                      "Vulva - Females only" = "Vulva - Females only")

cardio_list <- c("Prescribing" = "drug_presc", "A&E attendances" = "aye",
                 "Out of hours cases" = "ooh_cardiac",
                 "Scottish Ambulance Service" = "sas_cardiac", "Cardiac procedures" = "cath")

#List of data items available in step 2 of perinatal tab
data_list_perinatal <- c("Stillbirths"="stillbirths",
                         "Neonatal deaths"="nnd",
                         "Extended perinatal deaths"="extperi",
                         "Post-neonatal deaths"="pnnd",
                         "Infant deaths"="infantdeaths")

data_list_bf <- c("Health Visitor first visit" = "First visit",
                  "6-8 week review" = "6-8 week")

mentalhealth_list <- c("Prescribing" = "mhdrugs", "A&E attendances" = "aye", "Out of hours cases" = "ooh")

###############################################.
## Palettes and plot parameters ----
###############################################.
pal_depr <- c('#2c7fb8', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#7fcdbb')
#Palette for 9 series in a gradient
pal_age <- c('#543005', '#8c510a', '#bf812d',  '#d0d1e6',
                    '#74add1', '#4575b4', '#313695')
pal_moc <- c('#543005', '#8c510a', '#bf812d', '#d0d1e6')
#Palette for those with a single category per sex and overall
pal_sex <- c('#000000', '#9ebcda','#8856a7')
pal_overall <- c('#000000', '#009900')

pal_2ages <- c('#9ebcda','#8856a7') # for those with only two age groups
pal_med <- c('#543005', '#bf812d', '#74add1', '#80cdc1') # Palettes for medicine groupings

pal_immun <- c("2019" = '#000000',
               "JAN 2020" = "#ffffd9", "FEB 2020" = "#edf8b1", "MAR 2020" = "#c7e9b4",
               "APR 2020" = "#7fcdbb", "MAY 2020" = "#41b6c4", "JUN 2020" = "#1d91c0",
               "JUL 2020" = "#225ea8", "AUG 2020" = "#253494", "SEP 2020" = "#081d58",
               "OCT 2020" = "#080859", "NOV 2020" = "#1c0859", "DEC 2020" = "#990099")

pal_child <- c("2019" = '#000000',
               "JAN 2020" = "#ffffd9", "FEB 2020" = "#edf8b1", "MAR 2020" = "#c7e9b4",
               "APR 2020" = "#7fcdbb", "MAY 2020" = "#3CB371", "JUN 2020" = "#32CD32",
               "JUL 2020" = "#41b6c4", "AUG 2020" = "#1d91c0", "SEP 2020" = "#225ea8",
               "OCT 2020" = "#253494", "NOV 2020" = "#081d58", "DEC 2020" = "#00004d")

# Style of x and y axis
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                    showline = TRUE, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                    tickfont = list(size=14), titlefont = list(size=14))

# Buttons to remove
bttn_remove <-  list('select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d',  
                       'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                        'hoverClosestCartesian')

## END
