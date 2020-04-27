# Global

###############################################.
## Packages ----
###############################################.

library(shiny)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(DT)
library(shinycssloaders) #for loading icons

###############################################.
## Functions ----
###############################################.

plot_box <- function(title_plot, plot_output) {
  div(h4(title_plot),
      withSpinner(plotlyOutput(plot_output)))
}

###############################################.
## Data ----
###############################################.
geo_lookup <- readRDS("data/geo_lookup.rds")

rapid <- readRDS("data/rapid_data.rds") #RAPID data
aye <- readRDS("data/ae_data.rds") #A&E data

spec_list <- sort(unique(rapid$spec)) # specialty list
data_list <- c("Hospital admissions", "A&E attendances")

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

# Style of x and y axis
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                    showline = TRUE, tickangle = 270, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                    tickfont = list(size=14), titlefont = list(size=14))

## END