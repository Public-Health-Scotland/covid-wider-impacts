# Code for the UI side of the summary trends tab
summary_tab <- 
  tabPanel(title = "Summary trends", icon = icon("area-chart"), value = "summary",
           wellPanel(#actionButton("browser", "browser"),
             column(4,
                    conditionalPanel(condition = "input['summary-measure'] != 'outpats' ", 
                                     div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                                         selectInput("geotype", label = NULL,
                                                     choices= c("Scotland", "Health board", "HSC partnership"),
                                                     selected = "Scotland")),
                                     uiOutput("geoname_ui")),
                    # If outpatients selected bring other set of choices
                    conditionalPanel(condition = "input['summary-measure'] == 'outpats' ", 
                                     div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                                         selectInput("geotype_op", label = NULL,
                                                     choices= c("Scotland", "Health board of treatment",
                                                                "Health board of residence",
                                                                "HSC partnership of residence"),
                                                     selected = "Scotland")),
                                     uiOutput("geoname_op_ui"))
             ),
             column(4, selectdata_ui("summary", measure_choices = data_list)),
             column(4,
                    conditionalPanel(condition = "input['summary-measure'] != 'outpats' ", 
                                     selectInput("adm_type", label = "Step 3. Select type of admission.",
                                                 choices = c("All", "Emergency", "Planned"), selected = "All")),
                    conditionalPanel(condition = "input['summary-measure'] == 'ooh' ", 
                                     selectInput("ooh_appt_type", label = "Step 4. Select type of appointment for overall chart.",
                                                 choices = c("All cases", "All consultations" = "ALL",
                                                             "Covid consultations" = "COVID", "Non-covid consultations" = "NON COVID"),
                                                 selected = "All Cases")),
                    conditionalPanel(condition = "input['summary-measure'] == 'outpats' ",
                                     selectInput("appt_type", label = "Step 3. Select type of appointment.",
                                                 choices = c("All", "New", "Return"), selected = "All")),
                    conditionalPanel(condition = "input['summary-measure'] == 'outpats' ",
                                     selectInput("time_type", label = "Step 4. Select weekly or monthly data.",
                                                 choices = c("Weekly", "Monthly"), selected = "Weekly")),
                    downloadButton('download_chart_data', 'Download data'),
                    fluidRow(br()),
                    actionButton('jump_commentary_summary','Go to commentary')
             )
           ), #wellPanel bracket
           mainPanel(width = 12,
                     uiOutput("data_explorer")
           )# mainPanel bracket
         
) # tabpanel bracket