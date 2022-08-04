# Code for the UI side of the summary trends tab
summary_tab <- 
  tabPanel(title = "Summary trends", icon = icon("area-chart"), value = "summary",
           wellPanel(#actionButton("browser", "browser"),
             column(3, selectdata_ui("summary", measure_choices = data_list)),
             column(4, selectgeo_ui("summary", area_choices =  c("Scotland", "Health board", "HSC partnership"))),
             column(3,
                    conditionalPanel(condition = "input['summary-measure'] != 'op' ", 
                                     selectInput("adm_type", label = "Step 3. Select type of admission.",
                                                 choices = c("All", "Emergency", "Planned"), selected = "All")),
                    conditionalPanel(condition = "input['summary-measure'] == 'ooh' ", 
                                     selectInput("ooh_appt_type", label = "Step 4. Select type of appointment for overall chart.",
                                                 choices = c("All cases", "All consultations" = "ALL",
                                                             "Covid consultations" = "COVID", "Non-covid consultations" = "NON COVID"),
                                                 selected = "All cases")),
                    conditionalPanel(condition = "input['summary-measure'] == 'op' ",
                                     selectInput("appt_type", label = "Step 3. Select type of appointment.",
                                                 choices = c("All", "New", "Return"), selected = "All")),
                    conditionalPanel(condition = "input['summary-measure'] == 'op' ",
                                     selectInput("time_type", label = "Step 4. Select weekly or monthly data.",
                                                 choices = c("Weekly", "Monthly"), selected = "Weekly"))),
             column(2,
                    downloadButton('download_chart_data', 'Download data'), br(), br(),
                    actionButton('summary-commentary','Go to commentary'))
           ), #wellPanel bracket
           mainPanel(width = 12,
                     uiOutput("data_explorer")
           )# mainPanel bracket
         
) # tabpanel bracket