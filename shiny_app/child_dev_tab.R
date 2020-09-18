##Server script for child development tab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_childdev_modal,
             showModal(modalDialog(
               title = "What is the data source?",
               p("Data source: CHSP Pre-School"),
               tags$b("Meaningful data"),
               p("This refers to records where a value of N (no concerns), C (concern newly suspected), or P 
(concern previously identified) has been recorded for all eight developmental domains. See the Early Child 
Development publication for further details."),
               tags$b("Denominators used in calculations"),
               p("The denominator used in the child development indicators is the total number of reviews, 
                 rather than the number of reviews with meaningful data recorded. It should be noted that it is possible 
                 for children to have a developmental concern identified without having meaningful data recorded for all 
                 developmental domains."), 
               p("Data for NHS Greater Glasgow and Clyde for the 13-15 month review is only available from May 2019 onwards."),
               p("The average is calculated as the median value of the period specified."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SPC charts rules
observeEvent(input$btn_childdev_rules,
             showModal(modalDialog(
               title = "How do we identify patterns in the data?",
               p("Controls charts follow a series of rules that help identify important changes in the data. 
                 These are the ones we used in this chart:"),
               tags$ul(tags$li("Shifts: Six or more consecutive data points above or below the centreline."),
                       tags$li("Trends: Five or more consecutive data points which are increasing or decreasing.")),
               p("Different control charts are used depending on the type of data involved.
                 For the child development ones we have used run charts."),
               p("Further information on these methods of presenting data can be found at the ",                      
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts.')),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
output$geoname_childdev_ui <- renderUI({
  
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_childdev])
  
  selectizeInput("geoname_childdev", label = NULL,
                 choices = areas_summary, selected = "")
  
})

###############################################.
##  Reactive datasets  ----
###############################################.
child_dev_filt <- reactive({
  
  review_chosen <- case_when( input$measure_select_childdev == "13_15mnth" ~ "13-15 month",
                              input$measure_select_childdev == "27_30mnth" ~ "27-30 month")
  
  child_dev %>% filter(area_name == input$geoname_childdev &
                         area_type == input$geotype_childdev &
                         review == review_chosen)
})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$childdev_explorer <- renderUI({
  
  review_title <- case_when(input$measure_select_childdev == "13_15mnth" ~
                             "13-15 month" ,
                            input$measure_select_childdev == "27_30mnth" ~
                              "27-30 month")
  
  control_chart_commentary <- p("We have used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf', 
                                                      "‘control charts’",class="externallink"), "to present the percentages above.", br(),
                                "Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation.  
                      Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’", br(),
                                "The dots joined by a solid line in the chart above show the monthly percentage of children with developmental concerns recorded during the review selected from January 2019 onwards.", br(),  
                                "The other line, the centreline, is there to help show how unexpected any observed changes are. 
                      The centreline is an average (median) over the time period specified in the legend of the chart. ")
                            
  tagList(
    fluidRow(column(12, 
                    h4(paste0("Percentage of children with 1 or more developmental concerns at the ",  
                              review_title, " review")))),
    actionButton("btn_childdev_rules", "How do we identify patterns in the data?", 
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("childdev_no_concerns"))),
    fluidRow(column(12, 
                    h4(paste0("Number of ", review_title, 
                              " reviews and reviews with meaningful data recorded")))),
    fluidRow(withSpinner(plotlyOutput("childdev_no_reviews"))),
    control_chart_commentary
    )#tagLIst bracket
  
  }) #close perinatal_explorer function

###############################################.
## Charts ----
###############################################.
output$childdev_no_reviews <- renderPlotly({
  
  trend_data <- child_dev_filt() %>% mutate(dummy = 0)
  
    #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Number of reviews"
  
    tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                              "<br>", "Number of reviews: ", trend_data$no_reviews,
                              "<br>", "Number of reviews with meaningful data:  ", trend_data$no_meaningful_reviews,
                              "<br>", "Number of children with recorded concerns: ", trend_data$concerns_1_plus))
  
    #Creating time trend plot
    plot_ly(data=trend_data, x=~month_review) %>%
      add_lines(y = ~no_reviews, name = "Number of reviews", 
              line = list(color = "#bf812d"), text=tooltip_trend, hoverinfo="text") %>% 
      add_lines(y = ~no_meaningful_reviews, name = "Number of reviews with meaningful data",
              line = list(color = "#74add1"), text=tooltip_trend, hoverinfo="text") %>% 
      add_lines(y = ~concerns_1_plus, name = "Number of children with developmental concerns",
                line = list(color = "black"), text=tooltip_trend, hoverinfo="text") %>% 
      # Dummy line so Glasgow axis shows from January onwards
      add_lines(y = ~dummy, line = list(color = "white"), showlegend = F) %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
    
  }
})

output$childdev_no_concerns <- renderPlotly({
  trend_data <- child_dev_filt()
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Percentage (%)"
  yaxis_plots[["range"]] <- c(0, 38)  # forcing range from 0 to 100%
  xaxis_plots[["range"]] <- c(min(trend_data$month_review), max(trend_data$month_review))
  
  tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                            "<br>", "% children with developmental concerns: ", trend_data$pc_1_plus, "%"))
  
  average_title <- case_when(input$geoname_childdev %in% c("Scotland", "NHS Greater Glasgow & Clyde") & 
                               input$measure_select_childdev == "13_15mnth" ~ "Average from May 19 to February 20",
                             T ~ "Average from January 19 to February 20")
  
  #Creating time trend plot
  run_plot <- plot_ly(data=trend_data, x=~month_review) %>%
    add_lines( y = ~pc_1_plus,  
              line = list(color = "black"), text=tooltip_trend, hoverinfo="text",
              marker = list(color = "black"), name = "% children with developmental concerns")
  
  # Dotted line for projected tails of centreline. It changes depending on area.
  if (input$geoname_childdev %in% c("Scotland", "NHS Greater Glasgow & Clyde") & input$measure_select_childdev == "13_15mnth") {
    run_plot %<>%     
      add_lines(data=trend_data %>% filter(as.Date(month_review) < as.Date("2020-03-01") &
                                             as.Date(month_review) >= as.Date("2019-05-01")), 
                                y = ~pc_1_plus_centreline, name = average_title,
                                line = list(color = "blue", dash = "solid"), hoverinfo="none") %>% 
      add_lines(data=trend_data %>% filter(as.Date(month_review) >= as.Date("2020-02-01") |
                                             as.Date(month_review) < as.Date("2019-05-01")), 
                y = ~pc_1_plus_centreline, showlegend = FALSE, 
                line = list(color = "blue", dash = "longdash"), hoverinfo="none")
  } else {
    run_plot %<>%     
      add_lines(data=trend_data %>% filter(as.Date(month_review) < as.Date("2020-03-01")), 
                y = ~pc_1_plus_centreline, name = average_title,
                line = list(color = "blue", dash = "solid"), hoverinfo="none") %>% 
      add_lines(data=trend_data %>% filter(as.Date(month_review) >= as.Date("2020-02-01")), 
                y = ~pc_1_plus_centreline, showlegend = FALSE, 
                line = list(color = "blue", dash = "longdash"), hoverinfo="none")
  }
  
  
 run_plot %>% 
   # adding shifts
   add_markers(data = trend_data %>% filter(shift == T), y = ~ pc_1_plus,
               marker = list(color = "orange", size = 10, symbol = "circle"), name = "Shifts") %>% 
   # adding trends
   add_markers(data = trend_data %>% filter(trend == T), y = ~ pc_1_plus,
               marker = list(color = "green", size = 10, symbol = "square"), name = "Trends") %>%  
   #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,  xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )}
})

###############################################.
## Data downloads ----
###############################################.

output$download_childdev_data <- downloadHandler(
  filename ="child_development_extract.csv",
  content = function(file) {
    write_csv(child_dev_filt() %>% select(-shift, -trend), file) } 
)


###############################################.
## Commentary ----
###############################################.
output$childdev_commentary <- renderUI({
  tagList(
    bsButton("jump_to_childdev",label = "Go to data"), #this button can only be used once
    h2("Child development reviews - 30th September 2020"),
    p("Placeholder")
  ) #tagLIst bracket
})
