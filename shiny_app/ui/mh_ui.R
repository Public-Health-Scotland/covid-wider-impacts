mh_tab <- 
  tabPanel(title = "Mental health", icon = icon("brain"), value = "mentalhealth",
           wellPanel(
             column(4, selectdata_ui("mh", measure_choices = mentalhealth_list)),
             column(4, selectgeo_ui("mh", area_choices =  c("Scotland", "Health board", "HSC partnership"))),
             column(4, downloadButton("download_mentalhealth_data", "Download data"),
                    fluidRow(br()),
                    actionButton('jump_commentary_mentalhealth','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("mh_explorer")
           )# mainPanel bracket
  ) #tabPanel bracket