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
spec_lookup <- readRDS("data/spec_lookup.rds")
ae_cardio_codes <- readRDS("data/ae_cardio_codes.rds")

rapid <- readRDS("data/rapid_data.rds") #RAPID data
aye <- readRDS("data/ae_data.rds") #A&E data
ooh <- readRDS("data/ooh_data.rds") # OOH data
nhs24 <- readRDS("data/nhs24_data.rds") # OOH data
sas <- readRDS("data/sas_data.rds") # OOH data
deaths <- readRDS("data/deaths_data.rds") # deaths data

#Cardiovascular data
ae_cardio <- readRDS("data/ae_cardio_data.rds") # A&E cardio data
cardio_drugs <- readRDS("data/cardio_drugs_data.rds") # Cardio drugs data
cath_lab <- readRDS("data/cath_lab_data.rds") # Cath lab data

## Child Health Data
child_extract_date <- "24th August 2020"
first <- readRDS("data/first_visit_data.rds") # first health visit at 2 weeks
firsttable <- readRDS("data/first_visit_datatable.rds")
sixtoeight <- readRDS("data/six_to_eight_data.rds")
sixtoeighttable <- readRDS("data/six_to_eight_datatable.rds")
thirteen <- readRDS("data/thirteen_data.rds")
thirteentable <- readRDS("data/thirteen_datatable.rds")
twentyseven <- readRDS("data/twentyseven_data.rds")
twentyseventable <- readRDS("data/twentyseven_datatable.rds")
fourtofive <- readRDS("data/fourtofive_data.rds")
fourtofivetable <- readRDS("data/fourtofive_datatable.rds")

## Immunisation Data
immunisation_extract_date <- "24th August 2020"
month_elig_imm <- readRDS("data/month_eligibility_immun.rds") #flextable with imm month eligibility
age_defs_imm_6inone <- readRDS("data/age_elig_6inone.rds")
age_defs_imm_mmr <- readRDS("data/age_elig_mmr.rds")

#Immunisations s-curve data
six_alldose <- readRDS("data/six_alldose_data.rds")
mmr_alldose <- readRDS("data/mmr_alldose_data.rds") # mmr immunisation scurve data for all doses

#Immunisations data table data
sixtable <- readRDS("data/sixinone_datatable.rds") # 6-in-1 summary table (all dose)
mmrtable <- readRDS("data/mmr_datatable.rds") # mmr summary table (all dose)

#data quality issues require additional data file for NHS grampian
mmrtable_dose2_gramp <- readRDS("data/mmr_dose2_datatable_grampian.rds") # mmr immunisation data table summary for just grampian mmr dose 2

#Immunisations SIMD data
six_simd_dose1 <- readRDS("data/six_dose1_simdtable.rds")
six_simd_dose2 <- readRDS("data/six_dose2_simdtable.rds")
six_simd_dose3 <- readRDS("data/six_dose3_simdtable.rds")
mmr_simd_dose1 <- readRDS("data/mmr_dose1_simdtable.rds")
mmr_simd_dose2 <- readRDS("data/mmr_dose2_simdtable.rds")

#Pregancy data
#antenatal booking
booking_extract_date <- "6th October 2020"
booking <- readRDS("data/ante_booking_data.rds")
#booking_number_download <- readRDS("data/ante_booking_number_download.rds")
booking_download <- readRDS("data/ante_booking_download.rds")

#terminations
top_extract_date <- "2nd October 2020"
top <- readRDS("data/top_data.rds")
top_download <- readRDS("data/top_download.rds")


###############################################.
## Objects, names, lists ----
###############################################.

## perinatal mortality data
perinatal <- readRDS("data/perinatal_data.rds")

spec_list <- sort(c(unique(spec_lookup$'Specialty group'),
                    "Medical (incl. Cardiology & Cancer)",
                    "Paediatrics (medical & surgical)")) # specialty list

data_list <- c("Hospital admissions" = "rapid", "A&E attendances" = "aye", 
               "NHS 24 completed contacts" = "nhs24", 
               "Out of hours consultations" = "ooh", "Scottish Ambulance Service" = "sas",
               "Excess mortality" = "deaths")

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

data_list_data_tab <- c(data_list, "Cardiovascular prescribing" = "cardio_drugs",
                        "A&E cardiovascular attendances" = "ae_cardio",
                        "Cardiac procedures" = "cath_lab",
                        "6-in-1 first dose"  = "sixin_8wks",
                        "6-in-1 second dose" = "sixin_8wks_second",
                        "6-in-1 third dose" = "sixin_8wks_third",
                        "MMR first dose" = "mmr_1dose",
                        "MMR second dose" = "mmr_2dose",
                        "Health Visitor first visit" = "first_visit",
                        "6-8 week child health review" = "sixtoeight_visit",
                        "13-15 month child health review" = "thirteen_visit",
                        "27-30 month child health review" = "twentyseven_visit",
                        "4-5 year child health review" = "fourtofive_visit",
                        "Stillbirths and infant deaths" = "perinatal"
)

cardio_list <- c("Prescribing" = "drug_presc", "A&E attendances" = "aye", 
                 "Cardiac procedures" = "cath")

#List of data items available in step 2 of perinatal tab
data_list_perinatal <- c("Stillbirths"="stillbirths",
                         "Neonatal deaths"="nnd",
                         "Extended perinatal deaths"="extperi",
                         "Post-neonatal deaths"="pnnd",
                         "Infant deaths"="infantdeaths")

###############################################.
## Palettes and plot parameters ----
###############################################.
pal_depr <- c('#2c7fb8', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#7fcdbb')
#Palette for 9 series in a gradient
pal_age <- c('#543005', '#8c510a', '#bf812d',  '#d0d1e6',
             '#74add1', '#4575b4', '#313695')
# '#abd9e9', '#dfc27d',
#Palette for those with a single category per sex and overall
pal_sex <- c('#000000', '#9ebcda','#8856a7')
pal_overall <- c('#000000', '#009900')

pal_2ages <- c('#9ebcda','#8856a7') # for those with only two age groups
pal_med <- c('#543005', '#bf812d', '#74add1', '#313695') # Palettes for medicine groupings

pal_immun <- c("2019" = '#000000',
               "JAN 2020" = "#abd9e9", "FEB 2020" = "#74add1", "MAR 2020" = "#7477d1",
               "APR 2020" = "#045a8d", "MAY 2020" = "#022b43",
               "W/B 01-JUN-2020" = "#fee391", "W/B 08-JUN-2020" = "#fec44f",
               "W/B 15-JUN-2020" = "#e49901", "W/B 22-JUN-2020" = "#ec7014",
               "W/B 29-JUN-2020" = "#cc4c02", "W/B 06-JUL-2020" = "#8c2d04",
               "W/B 13-JUL-2020" = "#662506")

# second colour palette for SIMD immunisation chart - ideally they could use same colour palette but during build dfferent time frame available
pal_immun2 <- c("2019" = '#000000',
                "MAR 2020" = "#abd9e9", "APR 2020" = "#74add1", "MAY 2020" = "#7477d1", "JUN 2020" ="#045a8d" )

pal_child <- c("2019" = '#000000', "JAN 2020" = "#abd9e9", "FEB 2020" = "#74add1",
               "MAR 2020" = "#7477d1", "APR 2020" = "#0570b0", 
               "MAY 2020" = "#045a8d",
               "W/B 01-JUN-2020" = "#fec44f",
               "W/B 08-JUN-2020" = "#fe9929", "W/B 15-JUN-2020" = "#ec7014",
               "W/B 22-JUN-2020" = "#cc4c02", "W/B 29-JUN-2020" = "#8c2d04",
               "W/B 06-JUL-2020" = "#662506")

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