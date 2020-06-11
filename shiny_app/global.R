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

ae_cardio <- readRDS("data/ae_cardio_data.rds") # A&E cardio data
cardio_drugs <- readRDS("data/cardio_drugs_data.rds") # Cardio drugs data

cath_lab <- readRDS("data/cath_lab_data.rds") # Cath lab data

## Immunisation Data
six <- readRDS("data/sixinone_data.rds") # 6 in 1 immunisation data at 8 weeks
sixtable <- readRDS("data/sixinone_datatable.rds") # 6 in 1 immunisation data at 8 weeks datatable summary at 12 weeks

## Child Health Data
first <- readRDS("data/first_visit_data.rds") # first health visit at 2 weeks
firsttable <- readRDS("data/first_visit_datatable.rds")
## Immunisation Data
six <- readRDS("data/sixinone_data.rds") # 6 in 1 immunisation data at 8 weeks
sixtable <- readRDS("data/sixinone_datatable.rds") # 6 in 1 immunisation data at 8 weeks datatable summary at 12 weeks

six_dose2 <- readRDS("data/sixinone_dose2_data.rds") # 6 in 1 immunisation data at 8 weeks
sixtable_dose2 <- readRDS("data/sixinone_dose2_datatable.rds") # 6 in 1 immunisation data at 8 weeks datatable summary at 12 weeks

six_dose3 <- readRDS("data/sixinone_dose3_data.rds") # 6 in 1 immunisation data at 8 weeks
sixtable_dose3 <- readRDS("data/sixinone_dose3_datatable.rds") # 6 in 1 immunisation data at 8 weeks datatable summary at 12 weeks


spec_list <- sort(c(unique(spec_lookup$'Specialty group'),
                    "Medical (incl. Cardiology & Cancer)")) # specialty list

data_list <- c("Hospital admissions" = "rapid", "A&E attendances" = "aye", 
               "NHS 24 completed contacts" = "nhs24", 
               "Out of hours consultations" = "ooh", "Scottish Ambulance Service" = "sas",
               "Excess mortality" = "deaths")

#List of data items available in step 2 of immunisation tab
data_list_immun <- c("6-in-1 first dose" = "sixin_dose1",
                     "6-in-1 second dose" = "sixin_dose2",
                     "6-in-1 third dose" = "sixin_dose3")

# List of data items available in step 2 of immunisation tab
data_list_child <- c("Health Visitor first visit" = "first_visit",
            "6-8 Week Review *COMING 8th July 2020*" = "six_eightwks",
            "13-15 Month Review *COMING 8th July 2020*" = "13_15mnth")

data_list_data_tab <- c(data_list, "6-in-1 first dose"  = "sixin_8wks", 
                        "Health Visitor first visit" = "first_visit")


cardio_list <- c("Prescribing" = "drug_presc",
                 "A&E attendances" = "aye", "Cardiac catheterisation labs" = "cath")

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
pal_con <- c('#bf812d', '#74add1', '#313695') # Palettes for conditions

pal_immun <- c("2019" = '#000000', "JAN 2020" = "#abd9e9", "FEB 2020" = "#74add1",
               "W/B 02-MAR-2020" = "#fee391", "W/B 09-MAR-2020" = "#fec44f",
               "W/B 16-MAR-2020" = "#fe9929", "W/B 23-MAR-2020" = "#ec7014",
               "W/B 30-MAR-2020" = "#cc4c02", "W/B 06-APR-2020" = "#8c2d04")

pal_immun <- c("2019" = '#000000', "JAN 2020" = "#abd9e9", "FEB 2020" = "#74add1",
               "W/B 02-MAR-2020" = "#fee391", "W/B 09-MAR-2020" = "#fec44f",
               "W/B 16-MAR-2020" = "#fe9929", "W/B 23-MAR-2020" = "#ec7014",
               "W/B 30-MAR-2020" = "#cc4c02", "W/B 06-APR-2020" = "#8c2d04")

pal_child <- c("2019" = '#000000', "JAN 2020" = "#abd9e9", "FEB 2020" = "#74add1",
               "W/B 02-MAR-2020" = "#fee391", "W/B 09-MAR-2020" = "#fec44f",
               "W/B 16-MAR-2020" = "#fe9929", "W/B 23-MAR-2020" = "#ec7014",
               "W/B 30-MAR-2020" = "#cc4c02", "W/B 06-APR-2020" = "#8c2d04",
               "W/B 13-APR-2020" = "#662506")

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