drugs_tab <- 
  tabPanel(title = "Substance use", icon = icon("tablets"), value = "drugs",
         wellPanel(
           column(4, selectdata_ui("drugs", measure_choices =  c('Take home naloxone kits',
                                                                 'Scottish Ambulance Service naloxone administration'= 'SAS naloxone administration',
                                                                 'Drug and alcohol treatment referrals',
                                                                 'Opioid substitution therapy prescribing'='OST prescribing',
                                                                 'A&E attendances for drug overdose/intoxication'))),
           column(4,uiOutput('area_drugs_select'),
                  uiOutput("geoname_ui_drugs")),
           
           column(4, uiOutput("types")),
           column(4,downloadButton('download_drugs_data', 'Download data'),
                  actionButton('jump_commentary_drugs','Go to commentary'),
                  fluidRow(br()),
                  sourcemodal_ui("drugs")
           )
         ),#wellPanel bracket
         
         mainPanel(width = 12,
                   #actionButton('browser','browser'),
                   p('Last updated: 29 June 2022'),
                   fluidRow(br()),
                   uiOutput('drugs_quan_plot'),
                   fluidRow(br()),
                   uiOutput('drugs_2yr_comp'),
                   fluidRow(br()),
                   fluidRow(br()),
                   uiOutput('drugs_cum_plot'),
                   fluidRow(br()),
                   fluidRow(br()),
                   uiOutput('drugs_prop_barplot'),
                   uiOutput('drugs_perc_change'),
                   uiOutput('drugs_ae_explorer'),
                   fluidRow(br())
                   
         )# mainPanel bracket
         
) # tabpanel bracket