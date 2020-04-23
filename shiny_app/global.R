# Global

###############################################.
## Packages ----
###############################################.

library(shiny)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(DT)

###############################################.
## Functions ----
###############################################.

plot_box <- function(title_plot, plot_output) {
  div(p(title_plot),
      plotlyOutput(plot_output))
}

###############################################.
## Data ----
###############################################.
geo_lookup <- readRDS("data/geo_lookup.rds")

rapid <- readRDS("data/rapid_data.rds") #RAPID data


# For dummy data for table
measure_list <- c("OOH calls", "Testing",
                  "Admissions", "Discharges", "Calls to NHS 24", 
                  "A&E attendances")

table_data <- data.frame(date_event = seq(as.Date('2020-01-02'), as.Date('2020-03-31'), by = 'day'),
                         value = runif(90, 500, 1000),
                         measure = rep(measure_list, 15)) %>% 
  mutate(value = round(value, 0))


###############################################.
## Palettes ----
###############################################.
pal_depr <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')
#Palette for 9 series in a gradient
pal_age <- c('#543005', '#8c510a', '#bf812d', '#dfc27d', '#d0d1e6',
                   '#abd9e9', '#74add1', '#4575b4', '#313695')
#Palette for those with a single category per sex and overall
pal_sex <- c('#000000', '#08519c','#bdd7e7')


## END