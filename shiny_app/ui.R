#UI
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  navbarPage(id = "intabset", # id used for jumping between tabs
  title = div(tags$a(img(src="phs-logo.png", height=40), href= "https://www.publichealthscotland.scot/"),
              style = "position: relative; top: -5px;"), 
  windowTitle = "COVID-19 wider impacts", #title for browser tab
  header = tags$head(includeCSS("www/styles.css"), # CSS styles
                     tags$link(rel="shortcut icon", href="favicon_phs.ico")), #Icon for browser tab
###############################################.
## Introduction ----
###############################################.
tabPanel("Introduction", icon = icon("info-circle"), value = "intro",
         wellPanel(
           column(4,
                  h3("COVID-19 wider impacts on the health care system")),
           column(8,
                  tags$br(),
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
                    that apply to different national data sources.  For example, Public Health Scotland receives information
                    on patients attending Accident & Emergency within days; but there can be a delay of at least six weeks
                    before we receive detailed information on patients discharged from hospital after having a baby."),
                  p("Depending on the topic being looked at, information will be shown for patients in different age groups;
                    for males and females; and for people living in areas with different levels of material deprivation.
                    Information will also be shown for different locations across Scotland, such as NHS Board areas."),
                  p("Interactive charts on each of the topics are available in the ",
                    actionLink("jump_summary","'Summary trends' tab.")),
                  p("The underlying data used to create the interactive charts can be downloaded using the ",
                    actionLink( "jump_table", "'Data' tab."),
                    "Note that some numbers may not sum to the total as disclosure control methods have been applied
                    to the data in order to protect patient confidentiality."),
                  p("This tool will be updated weekly. New releases will be published at the same time as the Public Health Scotland ",
                    tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-statistical-report/",
                           "COVID-19 weekly report for Scotland.", class="externallink")),
                  p("If you have any questions relating to the data presented please contact us at: ",
                    tags$b(tags$a(href="mailto:phs.covid19analytics@nhs.net", "phs.covid19analytics@nhs.net", class="externallink")), ".")
                      )
             ), #wellPanel bracket
             mainPanel(width = 12
                      #reserve space for summary text
             )# mainPanel bracket
    ), #tabPanel bracket
###############################################.
## Commentary ----
###############################################.
tabPanel(title = "Commentary", icon = icon("list-ul"), value = "comment",
         mainPanel(width = 12,
                   # uiOutput("summary_comment"),
                   uiOutput("immun_commentary_section")
         )#main panel bracket
), #tab panel
###############################################.
## Summary trends ----
###############################################.
    tabPanel(title = "Summary trends", icon = icon("area-chart"), value = "summary",
      wellPanel(
        column(4, div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                      p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                      selectInput("geotype", label = NULL, choices= c("Scotland", "Health board", "HSC partnership"),
                                  selected = "Scotland")),
               uiOutput("geoname_ui")),
        column(4, div(title="Select the data you want to explore.", # tooltip
            radioGroupButtons("measure_select",
                              label= "Step 2 – Select the data you want to explore.",
                              choices = data_list, status = "primary",
                              direction = "vertical", justified = T))),
        column(4,
               selectInput("adm_type", label = "Step 3. Select type of admission.",
                           choices = c("All", "Emergency", "Planned"), selected = "All"),
               downloadButton('download_chart_data', 'Download data')#,
               # actionButton("browser", "Browser")
               )
      ), #wellPanel bracket
      mainPanel(width = 12,
                uiOutput("data_explorer")
      )# mainPanel bracket
    ), # tabpanel bracket
###############################################.
## Immunisation Tab ----
###############################################.
tabPanel(title = "Immunisations", icon = icon("syringe"), value = "child",
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
                  downloadButton('download_imm_data', 'Download data'))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("immunisation_explorer")
         )# mainPanel bracket 
), # tabpanel bracket
###############################################.
## Data ----
###############################################.
    tabPanel(title = "Data", icon = icon("table"), value = "table",
      p("This section allows you to view the data in table format.
        You can use the filters to select the data you are interested in.
        You can also download the data as a csv using the download button."),
      column(6, selectInput("data_select", "Select the data you want to explore.",
                           choices = data_list)),
      column(6, downloadButton('download_table_csv', 'Download data')),
      mainPanel(width = 12,
                DT::dataTableOutput("table_filtered"))
     ) # tabpanel bracket
   ) # page bracket
 )# taglist bracket
##END
