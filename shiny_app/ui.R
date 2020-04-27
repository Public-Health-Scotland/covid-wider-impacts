#UI

fluidPage(
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
        column(5, uiOutput("time_period_ui"),
               selectInput("adm_type", label = "Step 3. Select type of admission.",
                           choices = c("All", "Emergency", "Planned"), selected = "All")),
        column(3, downloadButton('download_chart_data', 'Download data')
               # actionButton("browser", "Browser")
               ),
        div(title="Select what data you want to explore.", # tooltip
            style = "margin-top: 10px; margin-bottom: 20px;", 
            radioGroupButtons("measure_select", 
                              label= "Step 4 - Select what data you want to explore.", 
                              choices = data_list, status = "primary",
                              justified = TRUE
            ))
      ), #wellPanel bracket
      mainPanel(width = 12,
                # fluidRow(h4("A&E attendances"),
                #          column(4, plot_box("By sex", "aye_sex")),
                #          column(4, plot_box("By age group", "aye_age")),
                #          column(4, plot_box("By deprivation quintile", "aye_depr"))
                # ),
                h4("Admissions to hospital"),
                plot_box("By sex", "adm_sex"),
                plot_box("By age group", "adm_age"),
                plot_box("By deprivation quintile", "adm_depr"),
                pickerInput("adm_specialty", "Select one or more specialties",
                            choices = spec_list, multiple = TRUE, 
                            selected = c("Accident & Emergency")),
                plot_box("By specialty (not distinguishing between planned or emergency admissions)", "adm_spec")
                # fluidRow(h4("NHS 24 calls"),
                #          column(4, plot_box("By sex", "nhs24_sex")),
                #          column(4, plot_box("By age group", "nhs24_age")),
                #          column(4, plot_box("By deprivation quintile", "nhs24_depr"))
                # ),
                # fluidRow(h4("Out of hours"),
                #          column(4, plot_box("By sex", "ooh_sex")),
                #          column(4, plot_box("By age group", "ooh_age")),
                #          column(4, plot_box("By deprivation quintile", "ooh_depr"))
                # ),
                # fluidRow(h4("Assessment centres testing"),
                #          column(4, plot_box("By sex", "test_sex")),
                #          column(4, plot_box("By age group", "test_age")),
                #          column(4, plot_box("By deprivation quintile", "test_depr"))
                # )
      )# mainPanel bracket
    ), # tabpanel bracket
###############################################.
## Data ----
###############################################.
    tabPanel(title = "Data", icon = icon("table"), value = "table",
      p("This section allows you to view the data in table format. 
        You can use the filters to select the data you are interested in. 
        You can also download the data as a csv using the download buttons."),
      downloadButton('download_table_csv', 'Download data'),
      mainPanel(width = 12,
                DT::dataTableOutput("table_filtered"))
    ) # tabpanel bracket
  ) # tabset panel bracket
) # fluidpage bracket

##END
