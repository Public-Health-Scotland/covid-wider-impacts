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

###############################################.
## Functions ----
###############################################.
plot_box <- function(title_plot, plot_output) {
  tagList(h4(title_plot),
          withSpinner(plotlyOutput(plot_output)))
}


plot_cut_box <- function(title_plot1, plot_output1,
                         title_plot2, plot_output2) {
  tagList(
    fluidRow(column(6, h4(title_plot1)),
             column(6, h4(title_plot2))),
    fluidRow(column(6, withSpinner(plotlyOutput(plot_output1))),
             column(6, withSpinner(plotlyOutput(plot_output2))))
    )
}

###############################################.
## Data ----
###############################################.
geo_lookup <- readRDS("data/geo_lookup.rds")
spec_lookup <- readRDS("data/spec_lookup.rds")

rapid <- readRDS("data/rapid_data.rds") #RAPID data
aye <- readRDS("data/ae_data.rds") #A&E data
ooh <- readRDS("data/ooh_data.rds") # OOH data
nhs24 <- readRDS("data/nhs24_data.rds") # OOH data
sas <- readRDS("data/sas_data.rds") # OOH data

cath_lab <- readRDS(paste0("shiny_app/data/cath_lab_data.rds"))
angio_lab <- readRDS(paste0("shiny_app/data/angio_lab_data.rds"))# Data: GJNH Coronary Angios/PCI 

spec_list <- sort(c(unique(spec_lookup$'Specialty group'), 
                  "Medical (incl. Cardiology & Cancer)")) # specialty list

data_list <- c("Hospital admissions" = "rapid", "A&E attendances" = "aye", 
               "NHS 24 completed contacts" = "nhs24", 
               "Out of hours consultations" = "ooh", "Scottish Ambulance Service" = "sas")

###############################################.
## Palettes and plot parameters ----
###############################################.
pal_depr <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')
#Palette for 9 series in a gradient
pal_age <- c('#543005', '#8c510a', '#bf812d',  '#d0d1e6',
                    '#74add1', '#4575b4', '#313695')
# '#abd9e9', '#dfc27d',
#Palette for those with a single category per sex and overall
pal_sex <- c('#000000', '#08519c','#bdd7e7')
pal_overall <- c('#000000', '#009900')


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