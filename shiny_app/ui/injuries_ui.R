injuries_tab <- 
  tabPanel(title = "Injuries", icon = icon("user-injured"), value = "injuries",
           wellPanel(actionButton("browser", "browser"),
             column(4, selectdata_ui("injury", measure_choices = injury_data_list)),
             column(4,  selectgeo_ui("injury", area_choices =  c("Scotland", "Health board", "HSC partnership"))),
             column(4,  selectInput("type_select", label = "Step 3. Select type of split",
                                    choices = injury_split_list, selected="Age group")),
             column(4, sourcemodal_ui("injury"),
                    fluidRow(br()),
                    downloadButton("download_injuries_data", "Download data"),
                    fluidRow(br()),
                    actionButton('jump_commentary_injuries','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("injuries_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket