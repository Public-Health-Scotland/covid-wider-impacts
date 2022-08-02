# What else:
# Geotype and geoname functions, this might include some module on the server
# Download data button, also will need module on server
# Jump to commentary
# One for the run chart rules modal and simd. Are these in the server side?
###############################################.
## Layout functions ----
###############################################.
filters_ui <- function(id, measure_choices, area_choices) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will invoke later.
  ns <- NS(id)
  
  wellPanel(
    column(4, selectdata_ui(id, measure_choices = measure_choices) ),
    column(4, selectgeo_ui(id, area_choices = area_choices)),
    column(4, sourcemodal_ui(id),
           fluidRow(br()),
           downloadButton(ns("download-data"), "Download data"),
           fluidRow(br()),
           actionButton(ns("commentary"),"Go to commentary"))
  ) #well panel
}
###############################################.
## Select dataset to show ----
###############################################.
# Selection of different indicators, input will follow a style like this input$`id-measure-select`
# id - namespace
# measure_choices - list of datasets available
selectdata_ui <- function(id, measure_choices) {
           div(title="Select the data you want to explore.", # tooltip
               radioGroupButtons(NS(id, "measure"),
                                 label= "Step 1. Select the data you want to explore.",
                                 choices = measure_choices, status = "primary",
                                 direction = "vertical", justified = T))
}

###############################################.
## Select geography ----
###############################################.
# Selectors for geography type and geography name 
# id - namespace
# area_choices - types of geography levels available. Vector.
selectgeo_ui <- function(id, area_choices, step_no = "2") {
  div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
      selectInput(NS(id, "geotype"), label = paste0("Step ", step_no,". Select a geography level and then an area of interest."),
                  choices = area_choices,
                  selected = "Scotland"),
      uiOutput(NS(id, "geoname")))
}

###############################################.
## Data source and definitions modal ----
###############################################.
# id - namespace
sourcemodal_ui <- function(id) {
  actionButton(NS(id, "source-modal"), "Data source and definitions", icon = icon('question-circle'))
}

##END