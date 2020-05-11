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
                  p("The COVID-19 pandemic has wider impacts than those that occur as the direct result of infection. There are a range of possible reasons for this, including public reluctance to burden the NHS, anxiety about using the NHS because of fears of infection, pressure on health services, delays to routine non-urgent care, impacts on existing preventive programmes and possible direct effects of COVID-19 on other conditions such as heart disease."),
                  p("This information tool provides an overview of changes in health and healthcare during the COVID-19 pandemic in Scotland, drawing on a range of national data sources. This first release focuses on recent trends in calls to NHS24, out-of-hours primary care consultations, Accident and Emergency attendances and hospital admissions. Because of the lag in submissions to routine hospital discharge datasets it has been necessary to use the Rapid dataset to monitor hospital admissions. This provides much less detail than usual sources, but has the advantage of offering more timely data."),
                  p("The intention is to enhance this selection in future releases as the data become available. The dashboard will be updated weekly, aligned to the release of the ", tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-statistical-report/", "Coronavirus (COVID-19) weekly report for Scotland.", 
                                                                                                                                                                                               class="externallink")),
                  p("Depending on the subject the data are presented by age, sex, measures of material deprivation, consultant specialty, planned and emergency hospital admissions and geographical location. Note that some numbers may not sum to the total as disclosure control methods have been applied to the data in order to protect patient confidentiality."),
                  p("If you have any questions on the data presented then please contact us at: ",
                    tags$b(tags$a(href="mailto:phs.statsgov@nhs.net", "phs.statsgov@nhs.net", class="externallink")), "."),
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
