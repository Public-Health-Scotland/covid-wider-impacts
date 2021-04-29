#UI
 #secure_app( #uncomment if needing password protection
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  navbarPage(id = "intabset", # id used for jumping between tabs
  title = div(tags$a(img(src="phs-logo.png", width=120, alt = "Public Health Scotland logo"), 
                     href= "https://www.publichealthscotland.scot/",
                     target = "_blank"),
              style = "position: relative; top: -10px;"), 
  windowTitle = "COVID-19 wider impacts", #title for browser tab
  header = tags$head(includeCSS("www/styles.css"), # CSS styles
                     HTML("<html lang='en'>"),
                     tags$link(rel="shortcut icon", href="favicon_phs.ico"), #Icon for browser tab
                     #Including Google analytics
                     includeScript("google-analytics.js")), 
##############################################.
# Introduction ----
##############################################.
tabPanel("Home", icon = icon("info-circle"), value = "intro",

         fluidRow(column(9, h3("COVID-19 wider impacts on the health care system")),
                  column(3, actionButton("new_next", tags$b("New content and future updates"),
                                         icon = icon('calendar-alt')))),
         p("The COVID-19 pandemic has wider impacts on individuals’ health, and their use of healthcare services,
                    than those that occur as the direct result of infection"),
                  p("Reasons for this may include:"),
                  tags$ul(
                    tags$li("Individuals being reluctant to use health services because they do not want to burden
                            the NHS or are anxious about the risk of infection."),
                    tags$li("The health service delaying preventative and non-urgent care such as some screening
                            services and planned surgery."),
                  tags$li("Other indirect effects of interventions to control COVID-19, such as changes to employment and income, changes in access to education, social isolation, family violence and abuse, changes in the accessibility and use of food, alcohol, drugs and gambling, or changes in physical activity and transport pattern.")
                ),
         p("More detailed background information on these potential impacts is provided by the Scottish Public Health Observatory in a section on ",
           tags$a(href="https://www.scotpho.org.uk/comparative-health/coronavirus-covid-19/covid-19-wider-impacts/",
                  "Covid-19 wider impacts", class="externallink",target="_blank"),"."),
         p("This information tool provides an overview of changes in health and use of healthcare during the COVID-19
                    pandemic in Scotland, drawing on a range of national data sources."),
         p("We are providing information on different topics as quickly as we can, given the different time lags
                    that apply to different national data sources. For example, Public Health Scotland receives information
                    on patients attending Accident & Emergency within days; but there can be a delay of at least six weeks
                    before we receive detailed information on patients discharged from hospital after having a baby."),
                  p("Depending on the topic being looked at, information will be shown for patients in different age groups;
                    for males and females; and for people living in areas with different levels of material deprivation.
                    Information will also be shown for different locations across Scotland, such as NHS Board areas."),
                p("This tool will be updated monthly. New releases will be published at the same time as the Public Health Scotland ",
                  tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-statistical-report/",
                         "COVID-19 report for Scotland.",  target="_blank")),
                p("Note that some numbers may not sum to the total as disclosure control methods have been applied
                    to the data in order to protect patient confidentiality."),
                p("If you have any questions relating to the data presented please contact us at: ",
                  tags$b(tags$a(href="mailto:phs.statsgov@phs.scot", "phs.statsgov@phs.scot",  target="_blank")), "."),
                p("You can access the code used to produce this tool in this ",
                  tags$a(href="https://github.com/Public-Health-Scotland/covid-wider-impacts", "GitHub repository",  target="_blank"), "."),
         h3("Other sources of information: "),
         tags$ul(
           tags$li("Public Health Scotland publishes ",
                   tags$a(href="https://publichealthscotland.scot/our-areas-of-work/sharing-our-data-and-intelligence/coronavirus-covid-19-data-and-guidance/", "information",  target="_blank"),
                    "on the direct health
                   impacts of COVID-19 as well as guidance for professionals and public."),
           tags$li("The Scottish Government publishes a ",
                   tags$a(href="https://data.gov.scot/coronavirus-covid-19/", "dashboard",  target="_blank"),
                   " which brings together data and
          evidence on the impacts of COVID-19 on health, society and the economy."),
           tags$li("The Improvement Service publishes a ",
                   tags$a(href="https://scotland.shinyapps.io/is-covid-economic-impact", "dashboard",  target="_blank"),
                   " on the economic impacts of the pandemic in Scotland."),
           tags$li("Public Health Scotland publishes ",
                   tags$a(href="https://publichealthscotland.scot/our-areas-of-work/covid-19/covid-19-data-and-intelligence/covid-19-and-children-research/",
                          "a series of reports",  target="_blank"),
                   " on the direct and wider impacts of the pandemic on children and young people."),
           tags$li("Transport Scotland publishes ",
                   tags$a(href="https://www.transport.gov.scot/publications/", "information",  target="_blank"),
            " on transport trends and public attitudes towards
                   transport for the pandemic period.")
         )
    ), #tabPanel bracket
###############################################.
## Commentary ----
###############################################.

tabPanel(title = "Commentary", icon = icon("list-ul"), value = "comment",
         wellPanel(column(12,
                          p("Select topic areas to find commentary relating to data presented in this tool."))),
         wellPanel(column(2,
                          p("Select topic:"),
                          actionLink("summary_button", "Summary trends", width = "150px"),br(),
                          actionLink("cardio_button", "Cardiovascular", width="150px"),br(),
                          actionLink("immunisation_button", "Immunisation", width = "150px"),br(),
                          actionLink("ch_review_button", "Child health reviews", width="150px"), br(),
                          actionLink("breastfeeding_button", "Breastfeeding", width="150px"), br(),
                          actionLink("childdev_button", "Child development", width="150px"), br(),
                          actionLink("perinatal_button", "Stillbirths and infant deaths", width="150px"), br(),
                          actionLink("booking_button", "Antenatal bookings", width="150px"), br(),
                          actionLink("top_button", "Termination of pregnancy", width="150px"),br(),
                          actionLink("mentalhealth_button", "Mental health", width="150px"),br(),
                          actionLink("mod_button", "Method of delivery", width="150px"), br(),
                          actionLink("induction_button", "Induction of labour", width="150px"),br(),
                          actionLink("gestation_button", "Gestation at delivery", width="150px"), br(),
                          actionLink("apgar_button", "Apgar scores", width="150px"),br(),
                          actionLink("preterm_button", "Location of extremely preterm deliveries", width="150px"),br(),
                          actionLink("cancer_button", "Cancer", width="150px")

                         ),
                   column(10,
                          bsCollapse(id = "collapse_commentary", open = "Panel 1", #PanelSet id
                                     bsCollapsePanel("Summary trends", uiOutput("summary_comment")), #collapsible panel for summary tab
                                     bsCollapsePanel("Cardiovascular",uiOutput("cardio_commentary")),#collapsible panel for cardiovascular tab
                                     bsCollapsePanel("Immunisation", uiOutput("immun_commentary_section")),
                                     bsCollapsePanel("Child health reviews", uiOutput("child_comments")),
                                     bsCollapsePanel("Breastfeeding", uiOutput("breastfeeding_commentary")),
                                     bsCollapsePanel("Child development", uiOutput("childdev_commentary")),
                                     bsCollapsePanel("Stillbirths and infant deaths", uiOutput("perinatal_commentary")),
                                     bsCollapsePanel("Mental health", uiOutput("mentalhealth_commentary")),
                                     bsCollapsePanel("Antenatal bookings", uiOutput("booking_commentary")),
                                     bsCollapsePanel("Termination of pregnancy", uiOutput("top_commentary")),
                                     bsCollapsePanel("Method of delivery", uiOutput("mod_commentary")),
                                     bsCollapsePanel("Induction of labour", uiOutput("induction_commentary")),
                                     bsCollapsePanel("Gestation at delivery", uiOutput("gestation_commentary")),
                                     bsCollapsePanel("Apgar scores", uiOutput("apgar_commentary")),
                                     bsCollapsePanel("Location of extremely preterm deliveries", uiOutput("preterm_commentary")),
                                     bsCollapsePanel("Cancer", uiOutput("cancer_commentary"))

                          )))
), #tab panel

###############################################.
# Summary trends ----
##############################################.
tabPanel(title = "Summary trends", icon = icon("area-chart"), value = "summary",
  wellPanel(
    column(4,
           conditionalPanel(condition = "input.measure_select != 'outpats' ",
           div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                  p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                  selectInput("geotype", label = NULL,
                              choices= c("Scotland", "Health board", "HSC partnership"),
                              selected = "Scotland")),
           uiOutput("geoname_ui")),
           # If outpatients selected bring other set of choices
           conditionalPanel(condition = "input.measure_select == 'outpats' ",
                            div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                                p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                                selectInput("geotype_op", label = NULL,
                                            choices= c("Scotland", "Health board of treatment",
                                                       "Health board of residence",
                                                       "HSC partnership of residence"),
                                            selected = "Scotland")),
                            uiOutput("geoname_op_ui"))
           ),
    column(4, div(title="Select the data you want to explore.", # tooltip
        radioGroupButtons("measure_select",
                          label= "Step 2 – Select the data you want to explore.",
                          choices = data_list, status = "primary",
                          direction = "vertical", justified = T))),
    column(4,
           selectInput("adm_type", label = "Step 3. Select type of admission.",
                       choices = c("All", "Emergency", "Planned"), selected = "All"),
           downloadButton('download_chart_data', 'Download data'),
           fluidRow(br()),
           actionButton('jump_commentary_summary','Go to commentary')
    )
  ), #wellPanel bracket
  mainPanel(width = 12,
            uiOutput("data_explorer")
  )# mainPanel bracket
), # tabpanel bracket
##############################################.
## Cardiovascular ----
##############################################.
tabPanel(title = "Cardiovascular", icon = icon("heartbeat"), value = "cardio",
         wellPanel(
           column(4, div(title="Select the data you want to explore.", # tooltip
                         radioGroupButtons("measure_cardio_select",
                                           label= "Step 1 – Select the data you want to explore.",
                                           choices = cardio_list, status = "primary",
                                           direction = "vertical", justified = T))),
           column(4, selectizeInput("area_cardio_select", "Step 2 - Select the area of interest",
                                    choices = c("Scotland"), selected = "Scotland"),
                  uiOutput("geoname_cardio_ui")),
           column(4, downloadButton('download_cardio_data', 'Download data'),
                  fluidRow(br()),
                  actionButton('jump_commentary_cardio','Go to commentary'))
         ), #wellPanel bracket
         mainPanel(width = 12,
                   uiOutput("cardio_explorer")
         )# mainPanel bracket
), # tabpanel bracket


###############################################.
## Cancer ----
###############################################.
tabPanel(title = "Cancer", icon = icon("disease"), value = "cancer",
  wellPanel(
           column(4, selectInput("geotype_cancer", label = "Step 1. Select a geography level and then an area of interest.",
                                 choices= c("Scotland", "Cancer Network", "Health Board"),
                                     selected = "Scotland"),
                  uiOutput("geoname_ui_cancer")),

           column(4,  selectInput("cancer_type", label = "Step 2. Select all or specific cancer type", choices = cancer_type_list,
                                     selected = "All Malignant Neoplasms (Excl. C44)"),
                     div(radioButtons("gender", "Step 3. Select sex",
                                      list("All Persons","Male","Female"), inline = TRUE,
                                      selected = "All Persons"))),
                     # div(radioButtons("split", "Data Filter", list("Age","SIMD"), inline = TRUE, selected = "Age"))),

           column(4,actionButton("btn_cancer_modal", "Data source: ", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton('download_cancer_data', 'Download data'),
                  fluidRow(br()),
                  actionButton('jump_commentary_cancer','Go to commentary'),
                  fluidRow(br()))
                  # div(radioButtons("data", "Data Type", list("Cumulative","Incidence"),
                  #                  inline = TRUE, selected = "Cumulative")))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("cancer_explorer")
         )# mainPanel bracket
), # tabpanel bracket
##############################################.
# Mental Health ----
##############################################.
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
),#tabPanel bracket
###############################################.
## Pregnancy ----
##############################################.
navbarMenu("Pregnancy", icon = icon("venus"),
###############################################.
## Antenatal booking Tab ----
###############################################.
tabPanel(title = "Antenatal booking", icon = icon("book-open"), value = "booking",
         wellPanel(
           column(4, div(title="Select a breakdown",
                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                         selectInput("geotype_booking", label = NULL, choices= c("Scotland", "Health board"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui_booking")),
           column(4,offset=4,
                  actionButton("btn_booking_modal", "Data source: Antenatal Booking Collection", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_ante_booking_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_booking","Go to commentary"))
           #actionButton("browser", "Browser")
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("booking_explorer")
         )# mainPanel bracket
), #tab panel
###############################################.
## Termination of pregnancy Tab ----
###############################################.
tabPanel(title = "Termination of pregnancy", icon = icon("bars"), value = "terminations",
         wellPanel(
           column(4, div(title="Select a breakdown",
                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                         selectInput("geotype_top", label = NULL, choices= c("Scotland", "Health board"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui_top")),
           column(4,offset=4,
                  actionButton("btn_top_modal", "Data source: Notifications of Abortion", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_termination_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_top","Go to commentary"))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("top_explorer")
         )# mainPanel bracket
) # tabPanel bracket
 ), # navbar menu bracket
###############################################.
## Births and Babies ----
##############################################.
navbarMenu("Births and babies", icon = icon("baby"),
###############################################.
## Inductions Tab ----
###############################################.
tabPanel(title = "Induction of labour", icon = icon("hand-holding-medical"), value = "inductions",
        wellPanel(
          column(4, div(title="Select a breakdown",
                        p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                        selectInput("geotype_induct", label = NULL, choices= c("Scotland", "Health board"),
                                    selected = "Scotland")),
                  uiOutput("geoname_ui_induct")),
          column(4,offset=4,
                  actionButton("btn_induct_modal", "Data source: SMR02", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_induct_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_induction","Go to commentary"))
        ), #well panel
        mainPanel(width = 12,
                   uiOutput("induct_explorer")
        )# mainPanel bracket
), # tabPanel bracket
###############################################.
## Mode of delivery Tab ----
###############################################.
tabPanel(title = "Method of delivery", icon = icon("hospital-user"), value = "mod",
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
), # tabPanel bracket
###############################################.
## Gestation at delivery Tab ----
###############################################.
tabPanel(title = "Gestation at delivery", icon = icon("calendar-alt"), value = "gestation",
        wellPanel(
          column(4, div(title="Select a breakdown",
                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                         selectInput("geotype_gest", label = NULL, choices= c("Scotland", "Health board"),
                                    selected = "Scotland")),
                  uiOutput("geoname_ui_gest")),
          column(4,offset=4,
                  actionButton("btn_gest_modal", "Data source: SMR02", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_gest_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_gestation","Go to commentary"))
        ), #well panel
        mainPanel(width = 12,
                  uiOutput("gestation_explorer")
        )# mainPanel bracket
 ), # tabPanel bracket
###############################################.
## Apgar Tab ----
###############################################.
tabPanel(title = "Apgar scores", icon = icon("notes-medical"), value = "apgar",
         wellPanel(
           column(4, div(title="Select a breakdown",
                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                         selectInput("geotype_apgar", label = NULL, choices= c("Scotland", "Health board"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui_apgar")),
           column(4,offset=4,
                  actionButton("btn_apgar_modal", "Data source: SMR02", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_apgar_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_apgar","Go to commentary"))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("apgar_explorer")
         )# mainPanel bracket
), # tabPanel bracket
###############################################.
## Preterm Tab ----
###############################################.
tabPanel(title = "Location of extremely preterm deliveries", icon = icon("hospital-alt"), value = "preterm",
         wellPanel(
           column(6, div(title="",
                         p(tags$b("Location of extremely preterm deliveries data is only available at Scotland level.")))),
           # ,
           #               selectInput("geotype_preterm", label = NULL, choices= c("Scotland"),
           #                           selected = "Scotland")),
           #        uiOutput("geoname_ui_preterm")),
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
), # tabPanel bracket
##############################################.
## Perinatal ----
###############################################.
tabPanel(title = "Stillbirths and infant deaths", icon = icon("female"), value = "perinatal_mortality",
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
), # navbar menu bracket
###############################################.
## Child health navbarmenu ----
###############################################.
navbarMenu("Child health", icon = icon("child"),
           ##############################################.
           # Immunisation Tab ----
           ##############################################.
           tabPanel(title = "Immunisations", icon = icon("syringe"), value = "imm",
                    wellPanel(
                      column(4, div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                                    p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                                    selectInput("geotype_immun", label = NULL, choices= c("Scotland", "Health board"),
                                                selected = "Scotland")),
                             uiOutput("geoname_ui_immun")),
                      column(4, div(title="Select the data you want to explore.", # tooltip
                                    radioGroupButtons("measure_select_immun",
                                                      label= "Step 2 – Select the data you want to explore.",
                                                      choices = data_list_immun, status = "primary",
                                                      direction = "vertical", justified = T))),
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
           ), # tabpanel bracket
           ##############################################.
           # Child Health reviews ----
           #############################################.
           tabPanel(title = "Child health reviews", icon = icon("child"), value = "child_health",
                    wellPanel(
                      column(4, div(title="Select a geography level first, then select the area you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                                    p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                                    selectInput("geotype_child", label = NULL, choices= c("Scotland", "Health board"),
                                                selected = "Scotland")),
                             uiOutput("geoname_ui_child")),
                      column(4, div(title="Select the data you want to explore.", # tooltip
                                    radioGroupButtons("measure_select_child",
                                                      label= "Step 2. Select the data you want to explore.",
                                                      choices = data_list_child, status = "primary",
                                                      direction = "vertical", justified = T))),
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
           ), # tabpanel bracket
           ###############################################.
           ## Breastfeeding tab ----
           ##############################################.
           tabPanel(title = "Breastfeeding", icon = icon("baby"), value = "breastfeeding",
                    wellPanel(
                      column(4, div(title="Select the data you want to explore.", # tooltip
                                    radioGroupButtons("measure_select_bf",
                                                      label= "Step 1. Select the data you want to explore.",
                                                      choices = data_list_bf, status = "primary",
                                                      direction = "vertical", justified = T))),
                      column(4, div(title="Select a geography level first, then select the area you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                                    p(tags$b("Step 2. Select a geography level and then an area of interest.")),
                                    selectInput("geotype_bf", label = NULL, choices= c("Scotland", "Health board"),
                                                selected = "Scotland")),
                             uiOutput("geoname_ui_bf")),
                      column(4,actionButton("btn_breastfed_modal", "Data source and definitions", icon = icon('question-circle')),
                             fluidRow(br()),
                             downloadButton("download_bf_data", "Download data"),
                             fluidRow(br()),
                             actionButton("jump_commentary_breastfed","Go to commentary"))
                    ), #well panel
                    mainPanel(width = 12,
                              uiOutput("breastfeeding_explorer")
                    )# mainPanel bracket
           ), # tabpanel bracket
           ###############################################.
           ## Child development ----
           ###############################################.
           tabPanel(title = "Child development", icon = icon("seedling"), value = "child_dev",
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
), #navbarMenu bracket
##############################################.
# Data ----
##############################################.
 tabPanel(title = "Data", icon = icon("table"), value = "table",
          p("This section allows you to view the data in table format.
         You can use the filters to select the data you are interested in.
         You can also download the data as a csv using the download button.
         Some of the data is also hosted in the",

            tags$a(href="https://www.opendata.nhs.scot/dataset?groups=covid-19",
                   "Scottish Health and Social Care Open Data portal",  target="_blank"), "."),
          column(6, selectInput("data_select", "Select the data you want to explore.",
                                choices = data_list_data_tab)),
          column(6, downloadButton('download_table_csv', 'Download data')),
          mainPanel(width = 12,
                    DT::dataTableOutput("table_filtered"))

       ) # tabpanel bracket
   ) # page bracket
 )# taglist bracket
 #)#secure app
#END
