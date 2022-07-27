# What else:
# Geotype and geoname functions, this might include some module on the server
# Download data button, also will need module on server
# Jump to commentary
# One for the run chart rules modal and simd. Are these in the server side?
###############################################.
## Layout functions ----
###############################################.
filters_ui <- function(id, measure_choices) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will invoke later.
  ns <- NS(id)
  
  wellPanel(
    column(4, selectdata_ui("bf", measure_choices = measure_choices) ),
    column(4, div(title="Select a geography level first, then select the area you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                  p(tags$b("Step 2. Select a geography level and then an area of interest.")),
                  selectInput(ns("geotype"), label = NULL, choices= c("Scotland", "Health board"),
                              selected = "Scotland")),
           uiOutput(ns("geoname-ui"))),
    column(4,actionButton(ns("btn-modal"), "Data source and definitions", icon = icon('question-circle')),
           fluidRow(br()),
           downloadButton(ns("download-data"), "Download data"),
           fluidRow(br()),
           actionButton(ns("jump-commentary"),"Go to commentary"))
  ) #well panel
}
###############################################.
## Select data ----
###############################################.
# Selection of different indicators, input will follow a style like this input$`id-measure-select`
# measure_choices - list of datasets available
selectdata_ui <- function(id, measure_choices) {
           div(title="Select the data you want to explore.", # tooltip
               radioGroupButtons(NS(id, "measure"),
                                 label= "Step 1. Select the data you want to explore.",
                                 choices = measure_choices, status = "primary",
                                 direction = "vertical", justified = T))
}

###############################################.
## Data source and definitions modal ----
###############################################.
sourcemodal_ui <- function(id) {
  actionButton(NS(id, "source-modal"), "Data source and definitions", icon = icon('question-circle'))
}
##END