injuries_tab <- 
  tabPanel(title = "Injuries", icon = icon("user-injured"), value = "injuries",
           wellPanel(
             column(4, div(title="Select the data you want to explore.", # tooltip
                           radioGroupButtons("measure_injury_select",
                                             label= "Step 1 – Select injury type",
                                             choices = injury_data_list, status = "primary",
                                             direction = "vertical", justified = T))),
             column(4, selectizeInput("area_injuries_select", "Step 2. Select a geography level",
                                      choices = c("Scotland", "Health board", "HSC partnership"), selected = "Scotland"),
                    uiOutput("geoname_injuries_ui")),
             
             column(4,  selectInput("type_select", label = "Step 3. Select type of split",
                                    choices = injury_split_list, selected="Age group")),
             
             # div(radioButtons("type", "Data Filter", list("Age","SIMD"), inline = TRUE, selected = "age"))),
             
             column(4,downloadButton("download_injuries_data", "Download data"),
                    fluidRow(br()),
                    actionButton('jump_commentary_injuries','Go to commentary'),
                    fluidRow(br()))
             # div(radioButtons("data", "Data Type", list("Cumulative","Incidence"),
             #                  inline = TRUE, selected = "Cumulative")))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("injuries_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket