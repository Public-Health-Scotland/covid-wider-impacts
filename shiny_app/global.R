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
  div(h4(title_plot),
      p("Source: Blablabla"),
      plotlyOutput(plot_output))
}

measure_list <- c("Visits to selected media", "Retweets/favourites",
                  "Visits to coronavirus pages in HPS", "Sentiment analysis",
                  "Positive cases of coronavirus", "Calls to NHS 24", 
                  "Most used word", "Deaths attributed to coronavirus",
                  "A&E attendances")

## END