#Code for child health ui sections

##############################################.
# Immunisations ----
##############################################.
immunisations_tab <- 
  tabPanel(title = "Immunisations", value = "imm",
         wellPanel(
           column(4, selectdata_ui("immun", measure_choices = data_list_immun)),
           column(4, selectgeo_ui("immun", area_choices =  c("Scotland", "Health board")),
                  div(title="Select the time periods you want to explore. You can click in the box then click on time periods in the dropdown to add them, or click on the x to remove a time period.",
                      p(tags$b("Step 3. Select time periods of interest.")),
                      uiOutput("dates_ui_immun"),
                      actionButton("btn_update_time_immun", "Update time periods")
                  )
           ),
           column(4, sourcemodal_ui("immun"),
                  fluidRow(br()),
                  actionButton("imm_elig_defs", "Eligibility definitions",  icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton('download_imm_data', 'Download data'),
                  fluidRow(br()),
                  actionButton('immun-commentary','Go to commentary')
           )
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("immunisation_explorer")
         )# mainPanel bracket
) # tabpanel bracket
##############################################.
# Child Health reviews ----
#############################################.
childreview_tab <- 
  tabPanel(title = "Child health reviews", value = "child_health",
         wellPanel(
           column(4, selectdata_ui("childr", measure_choices = data_list_child)), 
           column(4, selectgeo_ui("childr", area_choices =  c("Scotland", "Health board")),
                  div(title="Select the time periods you want to explore. You can click in the box then click on time periods in the dropdown to add them, or click on the x to remove a time period.",
                      p(tags$b("Step 3. Select time periods of interest.")),
                      uiOutput("dates_ui_child"),
                      actionButton("btn_update_time_child", "Update time periods"))),
           column(4, sourcemodal_ui("childr"),
                  fluidRow(br()),
                  downloadButton("download_visit_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_hv","Go to commentary"))
           #actionButton("browser", "Browser")
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("child_health_explorer")
         )# mainPanel bracket
)# tabpanel bracket
##############################################.
# Breastfeeding  ----
#############################################.
breastfeeding_tab <- 
  tabPanel(title = "Breastfeeding", value = "breastfeeding",
           filters_ui(id = "bf", measure_choices = data_list_bf,
                      area_choices = c("Scotland", "Health board")), # Filters and options
         mainPanel(width = 12,
                   uiOutput("breastfeeding_explorer")
         )# mainPanel bracket
) # tabpanel bracket
###############################################.
## Child development ----
###############################################.
childdev_tab <- 
  tabPanel(title = "Child development", value = "child_dev",
           filters_ui(id = "childdev", measure_choices = data_list_childdev,
                      area_choices = c("Scotland", "Health board")), # Filters and options
         mainPanel(width = 12,
                   uiOutput("childdev_explorer")
         )# mainPanel bracket
) # tabpanel bracket