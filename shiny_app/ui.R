#UI
navbarPage(
  title = "Media response to coronavirus - DRAFT - NO REAL DATA USED",
  windowTitle = "Media response to coronavirus", #title for browser tab
  header = tags$head(includeCSS("www/styles.css")),
  tabPanel(
    title = "Public health communications", icon = icon("newspaper"),
    wellPanel(
      div(title="Select a geography level first, then select the are you want from the list. You can click in the box, hit backspace and start to type if you want to start searching.",
          p(tags$b("Step 1. Select a geography level and then an area of interest.")),
          selectInput("geotype", label = NULL, choices= c("Scotland", "Health board", "Health and social care partnership"),
                      selected = "Scotland"),
          uiOutput("geoname_ui"))
    ), #wellPanel bracket
    mainPanel(width = 12,
              column(4, 
                     plot_box("Visits to selected media", "hits_media_plot"),
                     plot_box("Number of retweets/favourites of selected media", "retweets_plot"),
                     plot_box("Visits to coronavirus pages in HPS", "hits_hps_plot")),
              column(4, 
                     plot_box("Sentiment analysis, 0 negative, 1 positive", "sentiment_plot"),
                     plot_box("Positive cases of coronavirus", "coronacases_plot"),
                     plot_box("Calls to NHS 24", "nhs24_plot")),
              column(4, 
                     plot_box("Most used words", "word_cloud_plot"),
                     plot_box("Deaths attributed to coronavirus", "coronadeaths_plot"),
                     plot_box("A&E attendances", "aye_plot"))
    )# mainPanel bracket
  ), # tabpanel bracket
  tabPanel(
    title = "Correlations", icon = icon("chart-line"),
    wellPanel(
      column(6,
             selectInput("measure1", label = "Step 1 - Select a measure",
                         choices = measure_list, selected = "Visits to selected media")),
      column(6,
             selectInput("measure2", label = "Step 2 - Select another measure",
                         choices = measure_list, selected = "Positive cases of coronavirus"))
    ), #wellPanel bracket
    mainPanel(width = 12,
              p("This will show the relationship between some of the measures available."),
              p("Here some results of the modelling between both measures could
                be shown (ARIMA?)"),
              plotlyOutput("correlation_plot")
    )# mainPanel bracket
  ), # tabpanel bracket
  tabPanel(
    title = "Data", icon = icon("table"),
    p("This tab could include the data used in the app. And potentially could have
      the reactions to individual articles and tweets"),
    downloadButton('download_table_csv', 'Download data'),
    mainPanel(width = 12,
              DT::dataTableOutput("table_filtered"))
  ) # tabpanel bracket
) # navbar bracket

##END
