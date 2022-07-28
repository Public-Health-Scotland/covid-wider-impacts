#UI
# Sourcing UI scripts
source(file.path('ui/modules_ui.R'),  local = TRUE)$value 
source(file.path('ui/intro_ui.R'),  local = TRUE)$value 
source(file.path('ui/summary_ui.R'),  local = TRUE)$value 
source(file.path('ui/cardio_ui.R'),  local = TRUE)$value 
source(file.path('ui/cancer_ui.R'),  local = TRUE)$value 
source(file.path('ui/injuries_ui.R'),  local = TRUE)$value 
source(file.path('ui/mh_ui.R'),  local = TRUE)$value 
source(file.path('ui/pregnancy_ui.R'),  local = TRUE)$value 
source(file.path('ui/birthsbabies_ui.R'),  local = TRUE)$value 
source(file.path('ui/childhealth_ui.R'),  local = TRUE)$value 
source(file.path('ui/drugs_ui.R'),  local = TRUE)$value 
source(file.path('ui/data_ui.R'),  local = TRUE)$value 

# secure_app( #uncomment if needing password protection
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
             intro_tab, #Introduction ----
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
             # Section tabs ----
             #############################################.
             summary_tab, # Summary trends
             cardio_tab, # Cardiovascular
             # Cancer sections
             navbarMenu("Cancer", icon = icon("disease"),
                        cancerpath_tab, # Cancer pathology
                        sact_tabm,
                        sact_tabw), # SACT
             injuries_tab, # Unintentional injuries
             mh_tab, # mental health 
             # Pregnancy sections
             navbarMenu("Pregnancy", icon = icon("venus"),
                        antenatal_tab, # antenatal bookings
                        terminations_tab # terminations
             ), 
             # Births and Babies sections
             navbarMenu("Births and babies", icon = icon("baby"),
                        inductions_tab,
                        mode_delivery_tab,
                        gestation_tab,
                        apgar_tab,
                        preterm_tab,
                        perineal_tab,
                        perinatal_tab
             ), # navbar menu bracket.
             # Child health sections.
             navbarMenu("Child health", icon = icon("child"),
                        immunisations_tab,
                        childreview_tab,
                        breastfeeding_tab,
                        childdev_tab
             ), #navbarMenu bracket
             drugs_tab, # substance use
             data_tab # data 
  ) # page bracket
)# taglist bracket
# )#secure app
#END
