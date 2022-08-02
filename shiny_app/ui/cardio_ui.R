cardio_tab <- 
  tabPanel(title = "Cardiovascular", icon = icon("heartbeat"), value = "cardio",
           wellPanel(
             column(4, selectdata_ui("cardio", measure_choices = cardio_list)),
             column(4, selectgeo_ui("cardio", area_choices =  c("Scotland", "Health board", "HSC partnership"))
                    ),
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
