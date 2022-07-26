cardio_tab <- 
  tabPanel(title = "Cardiovascular", icon = icon("heartbeat"), value = "cardio",
           wellPanel(
             column(4, div(title="Select the data you want to explore.", # tooltip
                           radioGroupButtons("measure_cardio_select",
                                             label= "Step 1 – Select the data you want to explore.",
                                             choices = cardio_list, status = "primary",
                                             direction = "vertical", justified = T))),
             column(4, selectizeInput("area_cardio_select", "Step 2 - Select the area of interest",
                                      choices = c("Scotland"), selected = "Scotland"),
                    uiOutput("geoname_cardio_ui")),
             column(4,  selectInput("diagnosis_select", label = "Step 3. Select diagnosis",
                                    choices = c("Heart Attack","Heart Failure","Stroke"), selected = "Heart Attack")),
             column(4, downloadButton('download_cardio_data', 'Download data'),
                    fluidRow(br()),
                    actionButton('jump_commentary_cardio','Go to commentary'))
           ), #wellPanel bracket
           mainPanel(width = 12,
                     uiOutput("cardio_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket
