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
                  p("This tool provides a high level overview of how the COVID-19 response 
is impacting more widely on health and health inequalities. The initial version focuses on 
hospital admissions, unscheduled care and volume of calls to NHS24. The data are presented by age, sex, 
deprivation quintile, specialty, planned versus emergency admissions and by geographical location. 
Further data will be added as it becomes available and the dashboard will be updated weekly,
aligned to the release of the ", tags$a(href="https://beta.isdscotland.org/recent-publications/https://www.isdscotland.org/index.asp?Co=Y", "Coronavirus (COVID-19) weekly report for Scotland.", 
                           class="externallink")),
                  p("Disclosure control methods have been applied to the data in order to protect 
                    patient confidentiality therefore figures may not be additive. "),
                  p("If you have any questions relating to the data or the tool, then please contact us at: ",
                      tags$b(tags$a(href="mailto:phs.covid19analytics@nhs.net", "phs.covid19analytics@nhs.net", class="externallink")), "."),
                  h3("Contents"),    
                  tags$ul( 
                      tags$li(
                        tags$b(actionLink("jump_summary","Summary trends")),
                        icon("area-chart"),
                        " - for hospital admissions, unscheduled care & calls to NHS24."),
                      tags$li(
                        tags$b(actionLink( "jump_table", "Data")),
                        icon("table"),
                        " - view & download data behind this tool."))
                      )
             ), #wellPanel bracket
             mainPanel(width = 12
                      #reserve space for summary text 
             )# mainPanel bracket
    ), #tabPanel bracket
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
