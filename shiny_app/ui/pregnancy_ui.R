##############################################.
# Antenatal booking ----
##############################################.
antenatal_tab <- 
  tabPanel(title = "Antenatal booking", value = "booking",
         wellPanel(
           column(4, selectgeo_ui("booking", area_choices =  c("Scotland", "Health board"), step_no = "1")),
           column(4,offset=2,
                  sourcemodal_ui("booking"),
                  fluidRow(br()),
                  downloadButton("download_ante_booking_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_booking","Go to commentary"))
           #actionButton("browser", "Browser")
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("booking_explorer")
         )# mainPanel bracket
) #tab panel
###############################################.
## Termination of pregnancy  ----
###############################################.
terminations_tab <- 
  tabPanel(title = "Termination of pregnancy", value = "terminations",
         wellPanel(
           column(4, div(title="Select a breakdown",
                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                         selectInput("geotype_top", label = NULL, choices= c("Scotland", "Health board"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui_top")),
           column(4,offset=4,
                  actionButton("btn_top_modal", "Data source: Notifications of Abortion", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_termination_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_top","Go to commentary"))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("top_explorer")
         )# mainPanel bracket
) # tabPanel bracket