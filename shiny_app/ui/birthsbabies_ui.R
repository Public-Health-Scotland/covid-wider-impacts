##############################################.
# Inductions ----
##############################################.
inductions_tab <- 
  tabPanel(title = "Induction of labour", value = "inductions",
           wellPanel(
             column(4, selectgeo_ui("induct", area_choices =  c("Scotland", "Health board"), step_no = "1")),
             column(4,offset=2,
                    sourcemodal_ui("induct"),
                    fluidRow(br()),
                    downloadButton("download_induct_data", "Download data"),
                    fluidRow(br()),
                    actionButton("induct-commentary","Go to commentary"))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("induct_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket
##############################################.
# Mode of delivery ----
##############################################.
mode_delivery_tab <- 
  tabPanel(title = "Method of delivery", value = "mod",
           wellPanel(
             column(4, div(title="Select a breakdown",
                           p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                           selectInput("geotype_mod", label = NULL, choices= c("Scotland", "Health board"),
                                       selected = "Scotland")),
                    uiOutput("geoname_ui_mod")),
             column(4,offset=4,
                    actionButton("btn_mod_modal", "Data source: SMR02", icon = icon('question-circle')),
                    fluidRow(br()),
                    downloadButton("download_mod_data", "Download data"),
                    fluidRow(br()),
                    
                    actionButton('jump_commentary_mod','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("mod_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket
##############################################.
# Gestation at delivery ----
##############################################.
gestation_tab <- 
  tabPanel(title = "Gestation at delivery", value = "gestation",
           wellPanel(
             column(4, selectgeo_ui("gest", area_choices =  c("Scotland", "Health board"), step_no = "1")),
             column(4,offset=2,
                    sourcemodal_ui("gest"),
                    fluidRow(br()),
                    downloadButton("download_gest_data", "Download data"),
                    fluidRow(br()),
                    actionButton("gest-commentary","Go to commentary"))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("gestation_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket
###############################################.
# Apgar ----
###############################################.
apgar_tab <- 
  tabPanel(title = "Apgar scores", value = "apgar",
           wellPanel(
             column(4, selectgeo_ui("apgar", area_choices =  c("Scotland", "Health board"), step_no = "1")),
             column(4,offset=4,
                    sourcemodal_ui("apgar"),
                    fluidRow(br()),
                    downloadButton("download_apgar_data", "Download data"),
                    fluidRow(br()),
                    actionButton("jump_commentary_apgar","Go to commentary"))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("apgar_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket
###############################################.
## Preterm ----
###############################################.
preterm_tab <- 
  tabPanel(title = "Location of extremely preterm deliveries", value = "preterm",
           wellPanel(
             column(6, div(title="",
                           p(tags$b("Location of extremely preterm deliveries data is only available at Scotland level.")))),
             column(4,offset=2,
                    actionButton("btn_preterm_modal", "Data source: SMR02", icon = icon('question-circle')),
                    fluidRow(br()),
                    downloadButton("download_preterm_data", "Download data"),
                    fluidRow(br()),
                    actionButton("jump_commentary_preterm","Go to commentary"))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("preterm_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket
##############################################.
# Perineal tears  ----
##############################################.
perineal_tab <- tabPanel(title = "Perineal tears", value = "tears",
                         wellPanel(
                           column(4, div(title="Select a breakdown",
                                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                                         selectInput("geotype_tears", label = NULL, choices= c("Scotland", "Health board"),
                                                     selected = "Scotland")),
                                  uiOutput("geoname_ui_tears")),
                           column(4,offset=4,
                                  actionButton("btn_tears_modal", "Data source: SMR02", icon = icon('question-circle')),
                                  fluidRow(br()),
                                  downloadButton("download_tears_data", "Download data"),
                                  fluidRow(br()),
                                  actionButton("jump_commentary_tears","Go to commentary"))
                         ), #well panel
                         mainPanel(width = 12,
                                   uiOutput("tears_explorer")
                         )# mainPanel bracket
) # tabPanel bracket
#############################################.
# Perinatal ----
###############################################.
perinatal_tab <- 
  tabPanel(title = "Stillbirths and infant deaths", value = "perinatal_mortality",
           wellPanel(
             column(4, div(title="Select the data you want to explore.", # tooltip
                           radioGroupButtons("measure_select_perinatal",
                                             label= "Step 1 - Select the data you want to explore.",
                                             choices = data_list_perinatal, status = "primary",
                                             direction = "vertical", justified = T))),
             column(4,actionButton("btn_perinatal_modal", "Data source: NRS vital event registrations",
                                   icon = icon('question-circle')),
                    fluidRow(br()),
                    downloadButton("download_perinatal_data", "Download data"),
                    fluidRow(br()),
                    actionButton('jump_commentary_perinatal','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("perinatal_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket

##END