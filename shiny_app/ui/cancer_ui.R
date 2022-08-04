cancerpath_tab <- 
  # CANCER PATHOLOGY
  
  tabPanel(title = "Cancer pathology", icon = icon("microscope"), value = "cancer",
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
                    sourcemodal_ui("cancer"),
                    fluidRow(br()),
                    downloadButton('download_cancer_data', 'Download data'),
                    fluidRow(br()),
                    actionButton('jump_commentary_cancer','Go to commentary'))
           ), #well panel
           wellPanel(
             column(4,
                    div(radioButtons("baseline", "Select baseline for comparison",
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
                    div(radioButtons("breakdown", "Select breakdown type",
                                     list("None","Age Group","Deprivation"), inline = TRUE,
                                     selected = "None")))),
           wellPanel(width = 12,
                     uiOutput("cancer_explorer3"))
  )  # tabpanel bracket


###############################################.
## SACT monthly tab ----
###############################################.

sact_tabm <- 
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
                                            choices = c("All", "Breast", "Cancer of Unknown Origin", "Central Nervous System",
                                                        "Germ Cell", "Gynaecology", "Haematology", "Head & Neck", "Lower GI",
                                                        "Lung & Chest", "Neuroendocrine", "Other", "Sarcoma", "Skin",
                                                        "Upper GI", "Urological", "Unknown"), selected = "All"),
                            div(radioButtons("sact_plot_filter", "Select data breakdown to display together:",
                                             list("Geographic area","Treatment administration", "Standard graph"), inline = TRUE,
                                             selected = "Standard graph")))
           ), #well panel
           
           mainPanel(width = 12,
                     uiOutput("sact_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket

###############################################.
## SACT weekly tab ----
###############################################.

sact_tabw <- 
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
                                           choices = c("All", "Breast", "Cancer of Unknown Origin", "Central Nervous System",
                                                       "Germ Cell", "Gynaecology", "Haematology", "Head & Neck", "Lower GI",
                                                       "Lung & Chest", "Neuroendocrine", "Other", "Sarcoma", "Skin",
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
## DCE - not in use ----
###############################################.

# dce_tab <-             
#   tabPanel(title = "Cancer Staging - DCE Data", icon = icon("clock"), value = "dce",
#            wellPanel(h4(strong("Cancer Staging - Detect Cancer Early Data (Breast, Colorectal & Lung)")),
#                      # p("Cancer is one of the major causes of death in Scotland. In 2018, 16,153 people died of cancer
#                      #    in Scotland and approximately 34,000 people were diagnosed with cancer, excluding non-melanoma
#                      #      skin cancer. The most common causes of cancer diagnosis are lung, breast, prostate and colorectal cancer."),
#                      # p("In February 2012 the Cabinet Secretary for Health and Wellbeing formally launched the Detect Cancer Early
#                      #    programme . One aim of the Detect Cancer Early programme was to increase the proportion of people who were
#                      #    diagnosed early in the disease process (with stage 1 disease). The programme concentrates on breast, colorectal
#                      #    and lung cancers, which collectively account for 42.6% of all cancers diagnosed in Scotland in 2018."),
#                      p("Cancer staging is the process of determining the extent to which a cancer has developed and spread.
#                                   For the majority of patients with cancer it is common practice to assign a number from 1 to 4 to a cancer,
#                                   with 1 indicating the cancer is confined to the original organ in which it occurred and 4 being a cancer
#                                   which has spread beyond the original organ and its local lymph glands (regional lymph nodes). Patients
#                                   diagnosed with stage 1 disease tend to have better outcomes and longer survival compared with patients
#                                   diagnosed with stage 4 disease."),
#                      p("This dashboard looks at breast, colorectal and lung cancer data separately to examine the
#                                   different changes on stage at diagnosis from the year the coronavirus pandemic began compared with 2019."),
#                      p("The proportions of patients with any given cancer stage may be affected by a number of things, including
#                                   changes in the proportions of other stages (including those that are not known).  These data can only
#                                   describe patients who were diagnosed with cancer and in a separate section of this dashboard, it is estimated
#                                   that total breast, colorectal and lung cancer diagnoses fell by 16%, 21% and 21%, respectively, in 2020
#                                   compared with 2019. Temporary pausing of the national screening programmes for breast and colorectal cancer
#                                   in 2020 is likely to have particularly reduced numbers of early stage cancers being diagnosed.  A full
#                                   understanding of the various determinants of any changes in stage of cancer when it was diagnosed after
#                                   the pandemic began, and of the status of the people who were not diagnosed with cancer as expected in 2020,
#                                   will take time to be reached."),
#                      # tags$ul(
#                      #   tags$li("For breast cancer, there were large falls numbers in stages 1 and 2 (35% and 15% respectively). In
#                      #           contrast, there were small increases in stages 3 and 4 (5% and 7%), with the biggest increase seen for
#                      #           those of unknown stage (34%)."),
#                      #
#                      #   tags$li("For Colorectal Cancer, there were substantial drops (30% and more) in the numbers diagnosed with
#                      #           stages 1, 2 or 3 colorectal cancer; whereas there was only a 4% drop for metastatic colorectal cancer."),
#                      #
#                      #   tags$li("For Lung Cancer, there were falls of 11%-13% for stages 1, 2 and 3; but only a fall of 4% for stage 4
#                      #           diagnoses, which was only lower than expected in April 2020.")),
#                      
#                      p(strong(paste0("Figures presented based on data extracted on ",dce_extract_date)))
#            ),
#            wellPanel(
#              column(5, selectInput("geotype_dce", label = "Select a geography level",
#                                    choices= c("Scotland", "Cancer Network"),
#                                    selected = "Scotland"),
#                     uiOutput("geoname_ui_dce")),
#              column(5,  selectInput("dce_type", label = "Select all or specific cancer type",
#                                     choices = c("Breast", "Colorectal", "Lung"), selected = "Breast")),
#              column(2,
#                     fluidRow(br()),
#                     actionButton("btn_dce_modal", "Data source: ", icon = icon('question-circle')),
#                     fluidRow(br()),
#                     downloadButton('download_dce_data', 'Download data')) #,
#            ) , #well panel
#            mainPanel(width = 12,
#                      uiOutput("dce_explorer1") ,
#                      div(radioButtons("dce_stage", "Select stage of cancer (NK - Not Known)",
#                                       list("1","2","3","4","NK"), inline = TRUE,
#                                       selected = "1")),
#                      uiOutput("dce_explorer2")
#            )# mainPanel bracket
#   ) # tabpanel bracket
