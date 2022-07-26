#Code for child health ui sections

##############################################.
# Immunisations ----
##############################################.
immunisations_tab <- 
  tabPanel(title = "Immunisations", value = "imm",
         wellPanel(
           column(4, div(title="Select the data you want to explore.", # tooltip
                         radioGroupButtons("measure_select_immun",
                                           label= "Step 1. Select the data you want to explore.",
                                           choices = data_list_immun, status = "primary",
                                           direction = "vertical", justified = T))),
           column(4, div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                         p(tags$b("Step 2. Select a geography level and then an area of interest.")),
                         selectInput("geotype_immun", label = NULL, choices= c("Scotland", "Health board"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui_immun"),
                  div(title="Select the time periods you want to explore. You can click in the box then click on time periods in the dropdown to add them, or click on the x to remove a time period.",
                      p(tags$b("Step 3. Select time periods of interest.")),
                      uiOutput("dates_ui_immun"),
                      actionButton("btn_update_time_immun", "Update time periods")
                  )
           ),
           column(4,actionButton("btn_immune_modal", "Data source: PHS SIRS", icon = icon('question-circle')),
                  fluidRow(br()),
                  actionButton("imm_elig_defs", "Eligibility definitions",  icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton('download_imm_data', 'Download data'),
                  fluidRow(br()),
                  actionButton('jump_commentary_imm','Go to commentary')
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
           column(4, div(title="Select the data you want to explore.", # tooltip
                         radioGroupButtons("measure_select_child",
                                           label= "Step 1. Select the data you want to explore.",
                                           choices = data_list_child, status = "primary",
                                           direction = "vertical", justified = T))),
           column(4, div(title="Select a geography level first, then select the area you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                         p(tags$b("Step 2. Select a geography level and then an area of interest.")),
                         selectInput("geotype_child", label = NULL, choices= c("Scotland", "Health board"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui_child"),
                  div(title="Select the time periods you want to explore. You can click in the box then click on time periods in the dropdown to add them, or click on the x to remove a time period.",
                      p(tags$b("Step 3. Select time periods of interest.")),
                      uiOutput("dates_ui_child"),
                      actionButton("btn_update_time_child", "Update time periods"))),
           column(4,actionButton("btn_child_modal", "Data source: CHSP-PS, SIRS", icon = icon('question-circle')),
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
           #actionButton("browser", "browser"),
           filters_ui(id = "bf", measure_choices = data_list_bf), # Filters and options
           
#          wellPanel(
#            column(4,   filters_ui(id = "bf", measure_choices = data_list_bf)), # Filters and options
# # div(title="Select the data you want to explore.", # tooltip
# #                          radioGroupButtons("measure_select_bf",
# #                                            label= "Step 1. Select the data you want to explore.",
# #                                            choices = data_list_bf, status = "primary",
# #                                            direction = "vertical", justified = T))),
#            column(4, div(title="Select a geography level first, then select the area you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
#                          p(tags$b("Step 2. Select a geography level and then an area of interest.")),
#                          selectInput("geotype_bf", label = NULL, choices= c("Scotland", "Health board"),
#                                      selected = "Scotland")),
#                   uiOutput("geoname_ui_bf")),
#            column(4,actionButton("btn_breastfed_modal", "Data source and definitions", icon = icon('question-circle')),
#                   fluidRow(br()),
#                   downloadButton("download_bf_data", "Download data"),
#                   fluidRow(br()),
#                   actionButton("jump_commentary_breastfed","Go to commentary"))
#          ), #well panel
         mainPanel(width = 12,
                   uiOutput("breastfeeding_explorer")
         )# mainPanel bracket
) # tabpanel bracket
###############################################.
## Child development ----
###############################################.
childdev_tab <- 
  tabPanel(title = "Child development", value = "child_dev",
         wellPanel(
           column(4, div(title="Select the data you want to explore.", # tooltip
                         radioGroupButtons("measure_select_childdev",
                                           label= "Step 1 - Select the data you want to explore.",
                                           choices = data_list_childdev, status = "primary",
                                           direction = "vertical", justified = T))),
           column(4, selectizeInput("geotype_childdev", "Step 2 - Select a geography level and then an area of interest.",
                                    choices = c("Scotland", "Health board"), selected = "Scotland"),
                  uiOutput("geoname_childdev_ui")),
           column(4,actionButton("btn_childdev_modal", "Data source and definitions",
                                 icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_childdev_data", "Download data"),
                  fluidRow(br()),
                  actionButton('jump_commentary_childdev','Go to commentary'))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("childdev_explorer")
         )# mainPanel bracket
) # tabpanel bracket