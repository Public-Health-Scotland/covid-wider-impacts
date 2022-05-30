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
         p("Pre-Release Access: under terms of the 'Pre-Release Access to Official Statistics (Scotland) Order 2008',
            PHS is obliged to publish information on those receiving Pre-Release Access ('Pre-Release Access' refers to
            statistics in their final form prior to publication). Shown below are details of those receiving standard
            Pre-Release Access. "),
         p("Standard Pre-Release Access:"),
         tags$ul(
           tags$li("Scottish Government Health Department"),
           tags$li("NHS Board Chief Executives"),
           tags$li("NHS Board Communication Leads")),
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
                         actionLink("tears_button", "Perineal tears", width="150px"),br(),
                         actionLink("cancer_button", "Cancer", width="150px"),br(),
                         actionLink("injuries_button", "Injuries", width="150px"), br(),
                         actionLink("drug_button", "Substance use", width="150px")
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
                                     bsCollapsePanel("Perineal tears", uiOutput("tears_commentary")),
                                    bsCollapsePanel("Cancer", uiOutput("cancer_commentary")),
                                    bsCollapsePanel("Injuries", uiOutput("injuries_commentary")),
                                    bsCollapsePanel("Substance use", uiOutput("drug_commentary"))
                          )))
), #tab panel

##############################################.
# Summary trends ----
#############################################.
tabPanel(title = "Summary trends", icon = icon("area-chart"), value = "summary",
  wellPanel(#actionButton("browser", "browser"),
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
           conditionalPanel(condition = "input.measure_select != 'outpats' ",
           selectInput("adm_type", label = "Step 3. Select type of admission.",
                       choices = c("All", "Emergency", "Planned"), selected = "All")),

           conditionalPanel(condition = "input.measure_select == 'outpats' ",
                            selectInput("appt_type", label = "Step 3. Select type of appointment.",
                                        choices = c("All", "New", "Return"), selected = "All")),
           conditionalPanel(condition = "input.measure_select == 'outpats' ",
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

 ), # tabpanel bracket
#############################################.
# Cardiovascular ----
#############################################.
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

# ###############################################.
# ## Cancer ----
# ###############################################.
             navbarMenu("Cancer", icon = icon("disease"),

                        # CANCER PATHOLOGY

                        tabPanel(title = "Cancer Pathology", icon = icon("microscope"), value = "cancer",
                                 wellPanel(width = 12,
                                           uiOutput("cancer_explorer2")),
                                 wellPanel(
                                   column(4, selectInput("geotype_cancer", label = "Select a geography level and then an area of interest.",
                                                         choices= c("Scotland", "Cancer Networks", "Health Boards"),
                                                         selected = "Scotland"),
                                          uiOutput("geoname_ui_cancer")),
                                   column(4,  selectInput("cancer_type", label = "Select all or specific cancer type", choices = cancer_type_list,
                                                          selected = "All Malignant Neoplasms (Excl. C44)")),
                                   column(4,
                                          fluidRow(br()),
                                          actionButton("btn_cancer_modal", "Data source: ", icon = icon('question-circle')),
                                          fluidRow(br()),
                                          downloadButton('download_cancer_data', 'Download data'),
                                          fluidRow(br()),
                                          actionButton('jump_commentary_cancer','Go to commentary'))
                                 ), #well panel
                                 wellPanel(
                                   column(4,
                                          div(radioButtons("baseline", "Select Baseline for comparison",
                                                           list("2019", "Mean 2017-2019"), inline = TRUE,
                                                           selected = "2019"))),
                                   column(8,
                                          div(radioButtons("gender", "Select sex",
                                                           list("All","Male","Female"), inline = TRUE,
                                                           selected = "All")))

                                 ) ,# wellPanel bracket

                                 wellPanel(width = 12,
                                           uiOutput("cancer_explorer")
                                 ) ,# wellPanel bracket

                                 wellPanel(
                                   column(6,
                                          div(radioButtons("cum_baseline", "Select standard/cumulative baseline",
                                                           list("Standard", "Cumulative"), inline = TRUE,
                                                           selected = "Standard"))),
                                   column(6,
                                          div(radioButtons("breakdown", "Select Breakdown Type",
                                                           list("None","Age Group","Deprivation"), inline = TRUE,
                                                           selected = "None")))),
                                 wellPanel(width = 12,
                                           uiOutput("cancer_explorer3"))
                        ) , # tabpanel bracket

                         ###############################################.
                         ## SACT ----
                         ###############################################.


                         #### MONTHLY TAB

                        tabPanel(title = "SACT (Chemotherapy) Monthly Patients ", icon = icon("syringe"), value = "sact",
                                 wellPanel(h4(strong("SACT Treatment Activity in Scotland - Monthly Patient Data")),
                                           p("Systemic Anti-Cancer Treatments (SACT) is a collective term for drugs that are used in the treatment
                                             of cancer. The main type of drugs are cytotoxic chemotherapy drugs but there are other treatments
                                             such as targeted agents and immunotherapies."),
                                           p("The weekly and monthly activity reports are generated from the SACT national MVP data platform
                                             held by PHS, which is updated weekly from the five instances of ChemoCare across Scotland.
                                             All SACT and non-SACT (e.g. other drugs used to treat cancer such as hormones and supportive medicines
                                             such as anti-sickness medicines and steroids) activity which is prescribed
                                             in secondary care settings and is recorded on ChemoCare is included. Paediatric patient activity
                                             and prescriptions not recorded on a ChemoCare system are not included."),
                                           p("Local values have been used in the calculations, however, national mappings
                                             and derivations were applied to define tumour groups and identify the administration route."),
                                           p("Due to differences in recording practice",
                                             em(strong("it would be inappropriate to make direct comparisons between the cancer networks.")),
                                             style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),

                                           actionButton("btn_sact_modal", "FAQs", icon = icon('question-circle')),
                                           downloadButton('download_sact_monthly_data', 'Download data')), # well panel

                                 wellPanel(column(7, selectInput("geotype_sact", label = "Select a geography level and then an area of interest",
                                                                 choices= c("Scotland", "Cancer Network", "Health Board"),selected = "Scotland"),
                                                  uiOutput("geoname_ui_sact"),
                                                  uiOutput("treatment_ui_sact")),
                                           column(5,  selectInput("sact_type", label = "Select all or specific cancer type",
                                                                  choices = c("All","Bone Sarcoma", "Breast", "Cancer of Unknown Origin", "Central Nervous System",
                                                                              "Germ Cell", "Gynaecology", "Haematology", "Head & Neck", "Lower GI",
                                                                              "Lung & Chest", "Neuroendocrine", "Other", "Skin", "Soft Tissue Sarcoma",
                                                                              "Upper GI", "Urological", "Unknown"), selected = "All"),
                                                  div(radioButtons("sact_plot_filter", "Select data breakdown to display together:",
                                                                   list("Geographic area","Treatment administration", "Standard graph"), inline = TRUE,
                                                                   selected = "Standard graph")))
                                 ), #well panel

                                 mainPanel(width = 12,
                                           uiOutput("sact_explorer")
                                 )# mainPanel bracket
                        ), # tabpanel bracket

                        #### WEEKLY TAB

                        tabPanel(title = "SACT (Chemotherapy) Weekly Appointments", icon = icon("syringe"), value = "sact",
                                 wellPanel(h4(strong("SACT Treatment Activity in Scotland - Weekly Appointment Data")),
                                           #p(strong("Data from the ChemoCare system in the North Cancer Alliance (NCA) Highland has not
                                           #          refreshed on the 28th of June 2021. Data from this instance is therefore only deemed
                                           #         complete up to the week beginning 7th of June and only presented in the graphs up to
                                           #        that date. This affects Scotland level data, NCA network level data and NCA Highland
                                           #       health boards: NHS Highland and NHS Western Isles."),
                                           #style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
                                           p("Systemic Anti-Cancer Treatments (SACT) is a collective term for drugs that are used in the treatment
                                             of cancer. The main type of drugs are cytotoxic chemotherapy drugs but there are other treatments
                                             such as targeted agents and immunotherapies."),
                                           p("The weekly and monthly activity reports are generated from the SACT national MVP data platform
                                             held by PHS, which is updated weekly from the five instances of ChemoCare across Scotland.
                                             All SACT and non-SACT (e.g. other drugs used to treat cancer such as hormones and supportive medicines
                                             such as anti-sickness medicines and steroids) activity which is prescribed
                                             in secondary care settings and is recorded on ChemoCare is included. Paediatric patient activity
                                             and prescriptions not recorded on a ChemoCare system are not included."),
                                           p("Local values have been used in the calculations, however, national mappings
                                             and derivations were applied to define tumour groups and identify the administration route."),
                                           p("Due to differences in recording practice",
                                             em(strong("it would be inappropriate to make direct comparisons between the cancer networks.")),
                                             style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
                                           p("Activity data is released two week in arrears. The latest data currently available in the
                                             dashboard is for the week beginning", strong(format(max(sact_weekly_data$week_beginning), "%d %B %Y"))),

                                           actionButton("btn_sact_wk_modal", "FAQs", icon = icon('question-circle')),
                                           downloadButton('download_sact_weekly_data', 'Download data')), # well panel

                                 wellPanel(column(7, selectInput("geotype_wk_sact", label = "Select a geography level and then an area of interest",
                                                                 choices= c("Scotland", "Cancer Network", "Health Board"),selected = "Scotland"),
                                                  uiOutput("geoname_ui_wk_sact"),
                                                  uiOutput("treatment_ui_wk_sact")),

                                           column(5, selectInput("sact_wk_type", label = "Select all or specific cancer type",
                                                                 choices = c("All","Bone Sarcoma", "Breast", "Cancer of Unknown Origin", "Central Nervous System",
                                                                             "Germ Cell", "Gynaecology", "Haematology", "Head & Neck", "Lower GI",
                                                                             "Lung & Chest", "Neuroendocrine", "Other", "Skin", "Soft Tissue Sarcoma",
                                                                             "Upper GI", "Urological", "Unknown"), selected = "All"),
                                                  div(radioButtons("sact_wk_appt_reg", "Select method of administration route derivation",
                                                                   list("Appointment level","Regimen level"), inline = TRUE,
                                                                   selected = "Appointment level")),
                                                  div(radioButtons("sact_plot_wk_filter", "Select data breakdown to display together:",
                                                                   list("Geographic area","Treatment administration", "Standard graph"), inline = TRUE,
                                                                   selected = "Standard graph")))
                                 ), #well panel

                                 mainPanel(width = 12,
                                           uiOutput("sact_wk_explorer")
                                 )# mainPanel bracket
                              ) #, # tabpanel bracket

                        ###############################################.
                        ## DCE ----
                        ###############################################.

                        # tabPanel(title = "Cancer Staging - DCE Data", icon = icon("clock"), value = "dce",
                        #          wellPanel(h4(strong("Cancer Staging - Detect Cancer Early Data (Breast, Colorectal & Lung)")),
                        #            # p("Cancer is one of the major causes of death in Scotland. In 2018, 16,153 people died of cancer
                        #            #    in Scotland and approximately 34,000 people were diagnosed with cancer, excluding non-melanoma
                        #            #      skin cancer. The most common causes of cancer diagnosis are lung, breast, prostate and colorectal cancer."),
                        #            # p("In February 2012 the Cabinet Secretary for Health and Wellbeing formally launched the Detect Cancer Early
                        #            #    programme . One aim of the Detect Cancer Early programme was to increase the proportion of people who were
                        #            #    diagnosed early in the disease process (with stage 1 disease). The programme concentrates on breast, colorectal
                        #            #    and lung cancers, which collectively account for 42.6% of all cancers diagnosed in Scotland in 2018."),
                        #            p("Cancer staging is the process of determining the extent to which a cancer has developed and spread.
                        #               For the majority of patients with cancer it is common practice to assign a number from 1 to 4 to a cancer,
                        #               with 1 indicating the cancer is confined to the original organ in which it occurred and 4 being a cancer
                        #               which has spread beyond the original organ and its local lymph glands (regional lymph nodes). Patients
                        #               diagnosed with stage 1 disease tend to have better outcomes and longer survival compared with patients
                        #               diagnosed with stage 4 disease."),
                        #            p("This dashboard looks at each of breast, colorectal and lung cancer staging data separately to examine the
                        #              different impacts of the pandemic, and
                        #              how well cancer services are recovering to the expected pre-pandemic levels."),
                        #            p("The proportion of patients with cancer diagnosed with stage 1 disease can vary because of a number of
                        #               factors, including the presence and uptake of national screening programmes. On March 30th 2020, the
                        #               Scottish Government suspended the national screening programmes for breast and colorectal cancer due to
                        #               COVID-19, restarting gradually from July onwards."),
                        #            p("During the nine months of the pandemic in 2020 (April-December), there were 2,681 patients diagnosed with
                        #              breast cancer, 1,958 patients diagnosed with colorectal cancer and 3,287 patients diagnosed with lung cancer.
                        #              These numbers are 19% (breast), 25% (colorectal) and 9% (lung) lower than would have been expected in this
                        #              period had COVID-19 not happened."),
                        #            tags$ul(
                        #              tags$li("For breast cancer, there were large falls numbers in stages 1 and 2 (35% and 15% respectively). In
                        #                      contrast, there were small increases in stages 3 and 4 (5% and 7%), with the biggest increase seen for
                        #                      those of unknown stage (34%)."),


                        #
                        #              tags$li("For Colorectal Cancer, there were substantial drops (30% and more) in the numbers diagnosed with
                        #                      stages 1, 2 or 3 colorectal cancer; whereas there was only a 4% drop for metastatic colorectal cancer."),
                        #
                        #              tags$li("For Lung Cancer, there were falls of 11%-13% for stages 1, 2 and 3; but only a fall of 4% for stage 4
                        #                      diagnoses, which was only lower than expected in April 2020.")),
                        #


                        #            p(strong(paste0("Figures presented based on data extracted on ",dce_extract_date)))
                        #          ),
                        #          wellPanel(
                        #            column(5, selectInput("geotype_dce", label = "Select a geography level",
                        #                                  choices= c("Scotland", "Cancer Network"),
                        #                                  selected = "Scotland"),
                        #                   uiOutput("geoname_ui_dce")),

                        #
                        #            column(5,  selectInput("dce_type", label = "Select all or specific cancer type",

                        #                                   choices = c("Breast", "Colorectal", "Lung"), selected = "Breast")),
                        #            column(2,
                        #                   fluidRow(br()),
                        #                   actionButton("btn_dce_modal", "Data source: ", icon = icon('question-circle')),
                        #                   fluidRow(br()),
                        #                   downloadButton('download_dce_data', 'Download data')) #,
                        #                   # fluidRow(br()),
                        #                   # actionButton('jump_commentary_cancer','Go to commentary'))
                        #          ) , #well panel
                        #          mainPanel(width = 12,
                        #                    uiOutput("dce_explorer1"),
                        #                    div(radioButtons("dce_stage", "Select stage of cancer (NK - Not Known)",
                        #                                     list("1","2","3","4","NK"), inline = TRUE,
                        #                                     selected = "1")),
                        #                    uiOutput("dce_explorer2")
                        #          )# mainPanel bracket
                        # ) # tabpanel bracket
                ) , # navbar bracket
###############################################.
## Unintentional Injuries ----
###############################################.
tabPanel(title = "Injuries", icon = icon("user-injured"), value = "injuries",
         wellPanel(
           column(4, div(title="Select the data you want to explore.", # tooltip
                         radioGroupButtons("measure_injury_select",
                                           label= "Step 1 – Select injury type",
                                           choices = injury_data_list, status = "primary",
                                           direction = "vertical", justified = T))),
           column(4, selectizeInput("area_injuries_select", "Step 2. Select a geography level",
                                    choices = c("Scotland", "Health board", "HSC partnership"), selected = "Scotland"),
                  uiOutput("geoname_injuries_ui")),

           column(4,  selectInput("type_select", label = "Step 3. Select type of split",
                              choices = injury_split_list, selected="Age group")),

           # div(radioButtons("type", "Data Filter", list("Age","SIMD"), inline = TRUE, selected = "age"))),

           column(4,downloadButton("download_injuries_data", "Download data"),
                  fluidRow(br()),
                  actionButton('jump_commentary_injuries','Go to commentary'),
                  fluidRow(br()))
           # div(radioButtons("data", "Data Type", list("Cumulative","Incidence"),
           #                  inline = TRUE, selected = "Cumulative")))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("injuries_explorer")
         )# mainPanel bracket
), # tabpanel bracket
#############################################.
# Mental Health ----
#############################################.
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
##############################################.
# Pregnancy menu ----
#############################################.
navbarMenu("Pregnancy", icon = icon("venus"),
##############################################.
# Antenatal booking ----
##############################################.
tabPanel(title = "Antenatal booking", value = "booking",
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
# ###############################################.
# ## Termination of pregnancy  ----
# ###############################################.
tabPanel(title = "Termination of pregnancy", value = "terminations",
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
#############################################.
# Births and Babies menu ----
############################################.
navbarMenu("Births and babies", icon = icon("baby"),
##############################################.
# Inductions ----
##############################################.
tabPanel(title = "Induction of labour", value = "inductions",
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
##############################################.
# Mode of delivery ----
##############################################.
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
), # tabPanel bracket
##############################################.
# Gestation at delivery ----
##############################################.
tabPanel(title = "Gestation at delivery", value = "gestation",
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
## Apgar ----
###############################################.
tabPanel(title = "Apgar scores", value = "apgar",
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
# ###############################################.
# ## Preterm ----
# ###############################################.
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
), # tabPanel bracket
###############################################.
## Perineal tears  ----
###############################################.
tabPanel(title = "Perineal tears", value = "tears",
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
) , # tabPanel bracket
##############################################.
## Perinatal ----
# ###############################################.
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
 ), # navbar menu bracket
###############################################.
## Child health menu ----
###############################################.
navbarMenu("Child health", icon = icon("child"),
           ##############################################.
           # Immunisations ----
           ##############################################.
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
           ), # tabpanel bracket
#            ##############################################.
#            # Child Health reviews ----
#            #############################################.
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
           ), # tabpanel bracket
           ##############################################.
           # Breastfeeding  ----
           #############################################.
           tabPanel(title = "Breastfeeding", value = "breastfeeding",
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
), #navbarMenu bracket

###############################################.
## Drugs ----
###############################################.
tabPanel(title = "Substance use", icon = icon("tablets"), value = "drugs",
         wellPanel(
           column(4, div(title="Select the data you want to explore", # tooltip
                         radioGroupButtons("drug_subcategories",
                                           label= "Step 1 – Select the data you want to explore",
                                           choices = c('Take home naloxone kits',
                                                       'Scottish Ambulance Service naloxone administration'= 'SAS naloxone administration',
                                                       'Drug and alcohol treatment referrals',
                                                       'Opioid substitution therapy prescribing'='OST prescribing'),
                                           status = "primary",
                                           direction = "vertical", justified = T))),
           column(4,uiOutput('area_drugs_select'),
                  uiOutput("geoname_ui_drugs")),

           column(4, uiOutput("types")),
           column(4,downloadButton('download_drugs_data', 'Download data'),
                  actionButton('jump_commentary_drugs','Go to commentary'),
                  fluidRow(br()),
                  actionButton("btn_drugs_modal", "Data source and definitions",
                                                    icon = icon('question-circle')))
         ),#wellPanel bracket


         mainPanel(width = 12,
                   #actionButton('browser','browser'),
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
                   fluidRow(br()),
                   fluidRow(br())

        )# mainPanel bracket

), # tabpanel bracket
#############################################.
## Data ----
#############################################.
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

  #) #secure app



#END
