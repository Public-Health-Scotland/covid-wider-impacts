#UI
navbarPage(
  title = div(tags$a(img(src="phs-logo.png", height=40), href= "https://www.publichealthscotland.scot/"),
              style = "position: relative; top: -5px;",  
              "Coronavirus wider impact to health care systems - DRAFT - NO REAL DATA USED"), 
  windowTitle = "Coronavirus wider impact", #title for browser tab
  header = tags$head(includeCSS("www/styles.css")),
  tabPanel(
    title = "Summary trends", icon = icon("area-chart"),
    wellPanel(
      column(5, div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
          p(tags$b("Step 1. Select a geography level and then an area of interest.")),
          selectInput("geotype", label = NULL, choices= c("Scotland", "Health board", "HSC partnership"),
                      selected = "Scotland")),
          uiOutput("geoname_ui")),
      column(5, sliderInput("time_period", label = "Step 2 - Select the time period of interest",
                            min = as.Date('2020-01-01'), max = as.Date('2020-03-31'),
                            value = c(as.Date('2020-03-24'), as.Date('2020-03-31')),
                            step = 1)),
      actionButton("browser", "Browser")
    ), #wellPanel bracket
    mainPanel(width = 12,
              fluidRow(h4("A&E attendances"),
                       column(4, plot_box("By sex", "aye_sex")),
                       column(4, plot_box("By age group", "aye_age")),
                       column(4, plot_box("By deprivation quintile", "aye_depr"))
              ),
              fluidRow(h4("Admissions to hospital"),
                       column(4, plot_box("By sex", "adm_sex")),
                       column(4, plot_box("By age group", "adm_age")),
                       column(4, plot_box("By deprivation quintile", "adm_depr"))
              ),
              fluidRow(h4("Discharges from hospital"),
                       column(4, plot_box("By sex", "disch_sex")),
                       column(4, plot_box("By age group", "disch_age")),
                       column(4, plot_box("By deprivation quintile", "disch_depr"))
              ),
              fluidRow(h4("NHS 24 calls"),
                       column(4, plot_box("By sex", "nhs24_sex")),
                       column(4, plot_box("By age group", "nhs24_age")),
                       column(4, plot_box("By deprivation quintile", "nhs24_depr"))
              ),
              fluidRow(h4("Out of hours"),
                       column(4, plot_box("By sex", "ooh_sex")),
                       column(4, plot_box("By age group", "ooh_age")),
                       column(4, plot_box("By deprivation quintile", "ooh_depr"))
              ),
              fluidRow(h4("Assessment centres testing"),
                       column(4, plot_box("By sex", "test_sex")),
                       column(4, plot_box("By age group", "test_age")),
                       column(4, plot_box("By deprivation quintile", "test_depr"))
              )
    )# mainPanel bracket
  ), # tabpanel bracket
  tabPanel(
    title = "Data", icon = icon("table"),
    p("This tab could include the data used in the app. "),
    downloadButton('download_table_csv', 'Download data'),
    mainPanel(width = 12,
              DT::dataTableOutput("table_filtered"))
  ) # tabpanel bracket
) # navbar bracket

##END
