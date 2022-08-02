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
         sidebarLayout(
           sidebarPanel(width = 3,
                        radioGroupButtons("commentary_select",
                                          choices = commentary_list, status = "primary",
                                          direction = "vertical", justified = T)),
           mainPanel(width = 9,
                     # Summary trends
                     conditionalPanel(condition= 'input.commentary_select == "summary"',
                                      tagList(uiOutput("summary_comment"))),
                     # Cardiovascular
                     conditionalPanel(condition= 'input.commentary_select == "cardio"',
                                      tagList( uiOutput("cardio_commentary"))),
                     # Immunisation
                     conditionalPanel(condition= 'input.commentary_select == "immunisation"',
                                      tagList( uiOutput("immun_commentary_section"))),
                     # Child health
                     conditionalPanel(condition= 'input.commentary_select == "child-health"',
                                      tagList( uiOutput("child_comments"))),
                     # Breastfeeding
                     conditionalPanel(condition= 'input.commentary_select == "breastfeeding"',
                                      tagList( uiOutput("breastfeeding_commentary"))),
                     # Child development
                     conditionalPanel(condition= 'input.commentary_select == "child-dev"',
                                      tagList( uiOutput("childdev_commentary"))),
                     # Stillbirths and infant deaths
                     conditionalPanel(condition= 'input.commentary_select == "perinatal"',
                                      tagList( uiOutput("perinatal_commentary"))),
                     # Mental health
                     conditionalPanel(condition= 'input.commentary_select == "mental-health"',
                                      tagList( uiOutput("mentalhealth_commentary"))),
                     # Antenatal bookings
                     conditionalPanel(condition= 'input.commentary_select == "booking"',
                                      tagList( uiOutput("booking_commentary"))),
                     # Terminations
                     conditionalPanel(condition= 'input.commentary_select == "termination"',
                                      tagList( uiOutput("top_commentary"))),
                     # Mode of delivery
                     conditionalPanel(condition= 'input.commentary_select == "delivery"',
                                      tagList( uiOutput("mod_commentary"))),
                     # Induction of labour
                     conditionalPanel(condition= 'input.commentary_select == "induction"',
                                      tagList( uiOutput("induction_commentary"))),
                     # Gestation at delivery
                     conditionalPanel(condition= 'input.commentary_select == "gestation"',
                                      tagList( uiOutput("gestation_commentary"))),
                     # Apgar scores
                     conditionalPanel(condition= 'input.commentary_select == "apgar"',
                                      tagList( uiOutput("apgar_commentary"))),
                     # Location of extremely preterm babies
                     conditionalPanel(condition= 'input.commentary_select == "preterm"',
                                      tagList( uiOutput("preterm_commentary"))),
                     # Perineal tears
                     conditionalPanel(condition= 'input.commentary_select == "tears"',
                                      tagList( uiOutput("tears_commentary"))),
                     # Cancer
                     conditionalPanel(condition= 'input.commentary_select == "cancer"',
                                      tagList( uiOutput("cancer_commentary"))),
                     # Injuries
                     conditionalPanel(condition= 'input.commentary_select == "injuries"',
                                      tagList( uiOutput("injuries_commentary"))),
                     # Substance use
                     conditionalPanel(condition= 'input.commentary_select == "drugs"',
                                      tagList( uiOutput("drug_commentary")))
                     
           )#mainPanel
         ) #sidebarLayout
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
