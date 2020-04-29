#UI

fluidPage(
  useShinyjs(), # to allow shinyjs to work
  HTML('<meta name="viewport" content="width=1200">'), # needed for embedding as iframe in website
  style = "width: 100%; height: 100%; max-width: 1200px;", 
  tags$head(includeCSS("www/styles.css")), # CSS used to style app
  title = "Coronavirus wider impact", #title for browser tab
  tabsetPanel(id = "intabset", # id used for jumping between tabs
###############################################.
## Introduction ----
###############################################.
    tabPanel("Introduction", icon = icon("info-circle"), value = "intro",
             column(2,
                    h3("COVID-19 wider impacts to the health care system")),
             column(10,
                    tags$br(),
                    p("This tool allows you to find the data you want",
                      "and visualise it in different ways. Within each of the",
                      "following sections there are filters that let you",
                      "select the data you are interested in:"),
                    tags$ul( 
                      tags$li(
                        tags$b(actionLink("jump_summary","Summary trends")),
                        icon("area-chart"),
                        " - shows the data over time for different areas."),
                      tags$li(
                        tags$b(actionLink( "jump_table", "Data")),
                        icon("table"),
                        " - allows you to view and download the data as a table."))
                      )
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
               downloadButton('download_chart_data', 'Download data'),
               actionButton("browser", "Browser")
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
  ) # tabset panel bracket
) # fluidpage bracket

##END
