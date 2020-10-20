#UI
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  navbarPage(id = "intabset", # id used for jumping between tabs
  title = div(tags$a(img(src="phs-logo.png", height=40), href= "https://www.publichealthscotland.scot/"),
              style = "position: relative; top: -5px;"), 
  windowTitle = "COVID-19 wider impacts", #title for browser tab
  header = tags$head(includeCSS("www/styles.css"), # CSS styles
                     tags$link(rel="shortcut icon", href="favicon_phs.ico"), #Icon for browser tab
                     #Including Google analytics
                     includeScript("google-analytics.js")), 
###############################################.
## Introduction ----
###############################################.
tabPanel("Home", icon = icon("info-circle"), value = "intro",
                  h3("COVID-19 wider impacts on the health care system"),
                p("The COVID-19 pandemic has wider impacts on individuals’ health, and their use of healthcare services,
                    than those that occur as the direct result of infection"),
                p("Reasons for this may include:"),
                tags$ul(
                  tags$li("Individuals being reluctant to use health services because they do not want to burden
                            the NHS or are anxious about the risk of infection."),
                  tags$li("The health service delaying preventative and non-urgent care such as some screening
                            services and planned surgery."),
                  tags$li("Other indirect effects of interventions to control COVID-19, such as mental or physical
                            consequences of distancing measures.")
                ),
                p("This information tool provides an overview of changes in health and use of healthcare during the COVID-19
                    pandemic in Scotland, drawing on a range of national data sources."),
                p("We are providing information on different topics as quickly as we can, given the different time lags
                    that apply to different national data sources. For example, Public Health Scotland receives information
                    on patients attending Accident & Emergency within days; but there can be a delay of at least six weeks
                    before we receive detailed information on patients discharged from hospital after having a baby."),
                p("Depending on the topic being looked at, information will be shown for patients in different age groups;
                    for males and females; and for people living in areas with different levels of material deprivation.
                    Information will also be shown for different locations across Scotland, such as NHS Board areas."),
                p("This tool will be updated weekly. New releases will be published at the same time as the Public Health Scotland ",
                  tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-statistical-report/",
                         "COVID-19 weekly report for Scotland.", class="externallink")),
                p("Note that some numbers may not sum to the total as disclosure control methods have been applied
                    to the data in order to protect patient confidentiality."),
                p("If you have any questions relating to the data presented please contact us at: ",
                  tags$b(tags$a(href="mailto:phs.statsgov@nhs.net", "phs.statsgov@nhs.net", class="externallink")), "."),
                p("You can access the code used to produce this tool in this ",
                  tags$a(href="https://github.com/Health-SocialCare-Scotland/covid-wider-impact", "GitHub repository", class="externallink"), "."),
         h3("Other sources of information: "),
         tags$ul(
           tags$li("Public Health Scotland publishes ",
                   tags$a(href="(https://publichealthscotland.scot/our-areas-of-work/sharing-our-data-and-intelligence/coronavirus-covid-19-data-and-guidance/", "information", class="externallink"), 
                    "on the direct health
                   impacts of COVID-19 as well as guidance for professionals and public"),
           tags$li("The Scottish Government publishes a ",
                   tags$a(href="https://data.gov.scot/coronavirus-covid-19/", "dashboard", class="externallink"),
                   " which brings together data and 
          evidence on the impacts of COVID-19 on health, society and the economy."),
           tags$li("The Improvement Service publishes a ", 
                   tags$a(href="https://scotland.shinyapps.io/is-covid-economic-impact", "dashboard", class="externallink"),
                   " on the economic impacts of the pandemic in Scotland."),
           tags$li("Transport Scotland publishes ", 
                   tags$a(href="https://www.transport.gov.scot/publications/", "information", class="externallink"),
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
                          # actionLink("summary_button", "Summary trends", width = "150px"),br(),
                          # actionLink("cardio_button", "Cardiovascular", width="150px"),br() ,
                          # actionLink("immunisation_button", "Immunisation", width = "150px"),br(),
                          # actionLink("ch_review_button", "Child health reviews", width="150px"), br(),
                          # actionLink("perinatal_button", "Stillbirths and infant deaths", width="150px"),
                          actionLink("cancer_button", "Cancer", width="150px")),
                   column(10,
                          bsCollapse(id = "collapse_commentary", open = "Panel 1", #PanelSet id
                                     # bsCollapsePanel("Summary trends", uiOutput("summary_comment")), #collapsible panel for summary tab
                                     # bsCollapsePanel("Cardiovascular",uiOutput("cardio_commentary")),#collapsible panel for cardiovascular tab
                                     # bsCollapsePanel("Immunisation", uiOutput("immun_commentary_section")),
                                     # bsCollapsePanel("Child health reviews", uiOutput("child_comments")),
                                     # bsCollapsePanel("Stillbirths and infant deaths", uiOutput("perinatal_commentary")),
                                     bsCollapsePanel("Cancer", uiOutput("cancer_commentary"))

                          )))
), #tab panel
             
###############################################.
## Summary trends ----
###############################################.
#     tabPanel(title = "Summary trends", icon = icon("area-chart"), value = "summary",
#       wellPanel(
#         column(4, div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
#                       p(tags$b("Step 1. Select a geography level and then an area of interest.")),
#                       selectInput("geotype", label = NULL, choices= c("Scotland", "Health board", "HSC partnership"),
#                                   selected = "Scotland")),
#                uiOutput("geoname_ui")),
#         column(4, div(title="Select the data you want to explore.", # tooltip
#             radioGroupButtons("measure_select",
#                               label= "Step 2 – Select the data you want to explore.",
#                               choices = data_list, status = "primary",
#                               direction = "vertical", justified = T))),
#         column(4,
#                selectInput("adm_type", label = "Step 3. Select type of admission.",
#                            choices = c("All", "Emergency", "Planned"), selected = "All"),
#                downloadButton('download_chart_data', 'Download data'),
#                fluidRow(br()),
#                actionButton('jump_commentary_summary','Go to commentary')
#         )
#       ), #wellPanel bracket
#       mainPanel(width = 12,
#                 uiOutput("data_explorer")
#       )# mainPanel bracket
#     ), # tabpanel bracket
# 
# ###############################################.
# ## Cardiovascular ----
# ###############################################.
# tabPanel(title = "Cardiovascular", icon = icon("heartbeat"), value = "cardio",
#          wellPanel(
#            column(4, div(title="Select the data you want to explore.", # tooltip
#                          radioGroupButtons("measure_cardio_select",
#                                            label= "Step 1 – Select the data you want to explore.",
#                                            choices = cardio_list, status = "primary",
#                                            direction = "vertical", justified = T))),
#            column(4, selectizeInput("area_cardio_select", "Step 2 - Select the area of interest",
#                                     choices = c("Scotland"), selected = "Scotland"),
#                   uiOutput("geoname_cardio_ui")),
#            column(4, downloadButton('download_cardio_data', 'Download data'),
#                   fluidRow(br()),
#                   actionButton('jump_commentary_cardio','Go to commentary'))
#          ), #wellPanel bracket
#          mainPanel(width = 12,
#                    uiOutput("cardio_explorer")
#          )# mainPanel bracket
# ), # tabpanel bracket
# ###############################################.
# ## Child health navbarmenu ----
# ###############################################.
# navbarMenu("Child health", icon = icon("child"),
# ###############################################.
# ## Immunisation Tab ----
# ###############################################.
# tabPanel(title = "Immunisations", icon = icon("syringe"), value = "imm",
#          wellPanel(
#            column(4, div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
#                          p(tags$b("Step 1. Select a geography level and then an area of interest.")),
#                          selectInput("geotype_immun", label = NULL, choices= c("Scotland", "Health board"),
#                                      selected = "Scotland")),
#                   uiOutput("geoname_ui_immun")),
#            column(4, div(title="Select the data you want to explore.", # tooltip
#                          radioGroupButtons("measure_select_immun",
#                                            label= "Step 2 – Select the data you want to explore.",
#                                            choices = data_list_immun, status = "primary",
#                                            direction = "vertical", justified = T))),
#            column(4,actionButton("btn_immune_modal", "Data source: PHS SIRS", icon = icon('question-circle')),
#                   fluidRow(br()),
#                   actionButton("imm_elig_defs", "Eligibility definitions",  icon = icon('question-circle')),
#                   fluidRow(br()),
#                   downloadButton('download_imm_data', 'Download data'),
#                   fluidRow(br()),
#                   actionButton('jump_commentary_child','Go to commentary')
#                   )
#          ), #well panel
#          mainPanel(width = 12,
#                    uiOutput("immunisation_explorer")
#          )# mainPanel bracket
# ), # tabpanel bracket
# ###############################################.
# ## Child Health review tab ----
# ##############################################.
# tabPanel(title = "Child health reviews", icon = icon("user-check"), value = "child_review",
#          wellPanel(
#            column(4, div(title="Select a geography level first, then select the area you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
#                          p(tags$b("Step 1. Select a geography level and then an area of interest.")),
#                          selectInput("geotype_child", label = NULL, choices= c("Scotland", "Health board"),
#                                      selected = "Scotland")),
#                   uiOutput("geoname_ui_child")),
#            column(4, div(title="Select the data you want to explore.", # tooltip
#                          radioGroupButtons("measure_select_child",
#                                            label= "Step 2. Select the data you want to explore.",
#                                            choices = data_list_child, status = "primary",
#                                            direction = "vertical", justified = T))),
#            column(4,actionButton("btn_child_modal", "Data source: CHSP-PS, SIRS", icon = icon('question-circle')),
#                   fluidRow(br()),
#                   downloadButton("download_visit_data", "Download data"),
#                   fluidRow(br()),
#                   actionButton("jump_commentary_hv","Go to commentary"))
#            #actionButton("browser", "Browser")
#          ), #well panel
#          mainPanel(width = 12,
#                    uiOutput("child_health_explorer")
#          )# mainPanel bracket
#    ), # tabpanel bracket
# ###############################################.
# ## Perinatal Tab ----
# ###############################################.
# tabPanel(title = "Stillbirths and infant deaths", icon = icon("female"), value = "perinatal",
#          wellPanel(
#            column(4, div(title="Select the data you want to explore.", # tooltip
#                          radioGroupButtons("measure_select_perinatal",
#                                            label= "Step 1 - Select the data you want to explore.",
#                                            choices = data_list_perinatal, status = "primary",
#                                            direction = "vertical", justified = T))),
#            column(4,actionButton("btn_perinatal_modal", "Data source: NRS vital event registrations",
#                                  icon = icon('question-circle')),
#                   fluidRow(br()),
#                   downloadButton("download_perinatal_data", "Download data"),
#                   fluidRow(br()),
#                   actionButton('jump_commentary_perinatal','Go to commentary'))
#          ), #well panel
#          mainPanel(width = 12,
#                    uiOutput("perinatal_explorer")
#          )# mainPanel bracket
# ) # tabpanel bracket
# ), #navbarMenu bracket
###############################################.
## Cancer ----
###############################################.
tabPanel(title = "Cancer", icon = icon("disease"), value = "cancer",
  wellPanel(
           column(4, div(p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                         selectInput("geotype_cancer", label = NULL, choices= c("Scotland", "Health board"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui_cancer")),
                  
           column(4, div(p(tags$b("Step 2. Select all or specific cancer type")),
                         selectInput("cancer_type", label = NULL, choices = cancer_type_list,
                                     selected = "All Cancer Types")),
                     div(radioButtons("gender", "Gender", list("All","Male","Female"), inline = TRUE, selected = "All")),
                     div(radioButtons("split", "Data Filter", list("Age","SIMD"), inline = TRUE, selected = "Age"))),
           
           column(4,actionButton("btn_cancer_modal", "Data source: ", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton('download_cancer_data', 'Download data'),
                  fluidRow(br()),
                  actionButton('jump_commentary_cancer','Go to commentary'),
                  fluidRow(br()),
                  div(radioButtons("data", "Data Type", list("Cumulative","Incidence"), 
                                   inline = TRUE, selected = "Cumulative")))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("cancer_explorer")
         )# mainPanel bracket
) # tabpanel bracket

###############################################.
## Data ----
###############################################.
    # tabPanel(title = "Data", icon = icon("table"), value = "table",
    #   p("This section allows you to view the data in table format.
    #     You can use the filters to select the data you are interested in.
    #     You can also download the data as a csv using the download button.
    #     The data is also hosted in the",
    #     tags$a(href="https://www.opendata.nhs.scot/dataset?groups=covid-19",
    #            "Scottish Health and Social Care Open Data portal", class="externallink"), "."),
    #   column(6, selectInput("data_select", "Select the data you want to explore.",
    #                        choices = data_list_data_tab)),
    #   column(6, downloadButton('download_table_csv', 'Download data')),
    #   mainPanel(width = 12,
    #             DT::dataTableOutput("table_filtered"))
    #   ) # tabpanel bracket
    )# page bracket
 )# taglist bracket
##END
