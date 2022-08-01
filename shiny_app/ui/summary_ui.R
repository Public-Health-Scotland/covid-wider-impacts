# Code for the UI side of the summary trends tab
summary_tab <- 
  tabPanel(title = "Summary trends", icon = icon("area-chart"), value = "summary",
           wellPanel(#actionButton("browser", "browser"),
             column(4, selectdata_ui("summary", measure_choices = data_list)),
             column(4,
                    conditionalPanel(condition = "input['summary-measure'] != 'outpats' ", 
                                     selectgeo_ui("summary", area_choices =  c("Scotland", "Health board", "HSC partnership"))),
                    # If outpatients selected bring other set of choices
                    conditionalPanel(condition = "input['summary-measure'] == 'outpats' ", 
                                     selectgeo_ui("op", area_choices = c("Scotland", "Health board of treatment",
                                                                "Health board of residence",
                                                                "HSC partnership of residence")))
             ),
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
                    actionButton('summary-commentary','Go to commentary')
             )
           ), #wellPanel bracket
           mainPanel(width = 12,
                     uiOutput("data_explorer")
           )# mainPanel bracket
         
) # tabpanel bracket