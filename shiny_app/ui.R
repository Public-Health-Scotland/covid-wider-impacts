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
          selectInput("geotype", label = NULL, choices= c("Scotland", "Health board", "Health and social care partnership"),
                      selected = "Scotland")),
          uiOutput("geoname_ui")),
      column(5, sliderInput("time_media", label = "Step 2 - Select the time period of interest",
                            min = as.Date('2020-01-01'), max = as.Date('2020-03-31'),
                            value = c(as.Date('2020-03-24'), as.Date('2020-03-31')),
                            step = 1)),
      actionButton("browser", "Browser")
    ), #wellPanel bracket
    mainPanel(width = 12,
              column(4, 
                     plot_box("A&E attendances", "aye_sex"),
                     plot_box("Admissions to hospital", "adm_sex"),
                     plot_box("Discharges from hospital", "disch_sex"),
                     plot_box("NHS 24 calls", "nhs24_sex"),
                     plot_box("Out of hours", "ooh_sex"),
                     plot_box("Assessment centres testing", "test_sex")),
              column(4, 
                     plot_box("          ", "aye_age"),
                     plot_box("          ", "adm_age"),
                     plot_box("          ", "disch_age"),
                     plot_box("          ", "nhs24_age"),
                     plot_box("          ", "ooh_age"),
                     plot_box("          ", "disch_age")),
              column(4, 
                     plot_box("          ", "aye_depr"),
                     plot_box("          ", "adm_depr"),
                     plot_box("          ", "disch_depr"),
                     plot_box("          ", "nhs24_depr"),
                     plot_box("          ", "ooh_depr"),
                     plot_box("          ", "disch_depr"))
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
