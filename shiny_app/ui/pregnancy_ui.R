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
           column(4, selectgeo_ui("top", area_choices =  c("Scotland", "Health board"), step_no = "1")),
           column(4,offset=2,
                  sourcemodal_ui("top"),
                  fluidRow(br()),
                  downloadButton("download_termination_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_top","Go to commentary"))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("top_explorer")
         )# mainPanel bracket
) # tabPanel bracket