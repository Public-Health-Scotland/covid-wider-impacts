mh_tab <- 
  tabPanel(title = "Mental health", icon = icon("brain"), value = "mentalhealth",
           wellPanel(
             column(4, div(title="Select the data you want to explore.", # tooltip
                           radioGroupButtons("measure_mh_select",
                                             label= "Step 1 - Select the data you want to explore.",
                                             choices = mentalhealth_list, status = "primary",
                                             direction = "vertical", justified = T))),
             column(4, uiOutput("geotype_mh_ui"),
                    uiOutput("geoname_mh_ui")),
             column(4, downloadButton("download_mentalhealth_data", "Download data"),
                    fluidRow(br()),
                    actionButton('jump_commentary_mentalhealth','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("mh_explorer")
           )# mainPanel bracket
  ) #tabPanel bracket