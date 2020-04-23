#UI

fluidPage(
  HTML('<meta name="viewport" content="width=1200">'), # needed for embedding as iframe in website
  style = "width: 100%; height: 100%; max-width: 1200px;", 
  tags$head(includeCSS("www/styles.css")),
  title = "Coronavirus wider impact", #title for browser tab
  tabsetPanel(
    id = "Panels", 
    tabPanel("Introduction", icon = icon("info-circle")
    ),
    tabPanel(
      title = "Summary trends", icon = icon("area-chart"),
      wellPanel(
        column(5, div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
                      p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                      selectInput("geotype", label = NULL, choices= c("Scotland", "Health board", "HSC partnership"),
                                  selected = "Scotland")),
               uiOutput("geoname_ui")),
        column(5, uiOutput("time_period_ui")),
        column(2, downloadButton('download_chart_data', 'Download data'),
               actionButton("browser", "Browser"))
      ), #wellPanel bracket
      mainPanel(width = 12,
                # fluidRow(h4("A&E attendances"),
                #          column(4, plot_box("By sex", "aye_sex")),
                #          column(4, plot_box("By age group", "aye_age")),
                #          column(4, plot_box("By deprivation quintile", "aye_depr"))
                # ),
                fluidRow(h4("Admissions to hospital"),
                         column(4, plot_box("By sex", "adm_sex")),
                         column(4, plot_box("By age group", "adm_age")),
                         column(4, plot_box("By deprivation quintile", "adm_depr"))
                )
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
    tabPanel(
      title = "Data", icon = icon("table"),
      p("This tab could include the data used in the app. "),
      downloadButton('download_table_csv', 'Download data'),
      mainPanel(width = 12,
                DT::dataTableOutput("table_filtered"))
    ) # tabpanel bracket
  ) # tabset panel bracket
) # fluidpage bracket

##END
