drugs_tab <- 
  tabPanel(title = "Substance use", icon = icon("tablets"), value = "drugs",
         wellPanel(
           column(4, div(title="Select the data you want to explore", # tooltip
                         radioGroupButtons("drug_subcategories",
                                           label= "Step 1 – Select the data you want to explore",
                                           choices = c('Take home naloxone kits',
                                                       'Scottish Ambulance Service naloxone administration'= 'SAS naloxone administration',
                                                       'Drug and alcohol treatment referrals',
                                                       'Opioid substitution therapy prescribing'='OST prescribing',
                                                       'A&E attendances for drug overdose/intoxication'),
                                           status = "primary",
                                           direction = "vertical", justified = T))),
           column(4,uiOutput('area_drugs_select'),
                  uiOutput("geoname_ui_drugs")),
           
           column(4, uiOutput("types")),
           column(4,downloadButton('download_drugs_data', 'Download data'),
                  actionButton('jump_commentary_drugs','Go to commentary'),
                  fluidRow(br()),
                  actionButton("btn_drugs_modal", "Data source and definitions",
                               icon = icon('question-circle'))
           )
         ),#wellPanel bracket
         
         mainPanel(width = 12,
                   #actionButton('browser','browser'),
                   p('Last updated: 29 June 2022'),
                   fluidRow(br()),
                   uiOutput('Quan_plot'),
                   fluidRow(br()),
                   uiOutput('TwoYrComparison'),
                   fluidRow(br()),
                   fluidRow(br()),
                   uiOutput('Cum_plot'),
                   fluidRow(br()),
                   fluidRow(br()),
                   uiOutput('Prop_barplot'),
                   uiOutput('PercentChange'),
                   uiOutput('drug_AE_explorer'),
                   fluidRow(br())
                   
         )# mainPanel bracket
         
) # tabpanel bracket