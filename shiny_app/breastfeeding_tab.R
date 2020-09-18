##Server script for breastfeeding tab

###############################################.
## Modal ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$btn_breastfed_modal,
             showModal(modalDialog(
               title = "What is the data source?",
               p("Data source: CHSP Pre-School"),
               tags$b("Meaningful data"),
               p("This refers to records where a value of N (no concerns), C (concern newly suspected), or P 
(concern previously identified) has been recorded for all eight developmental domains. See the ",
                 tags$a(href = "https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/early-child-development/15-september-2020/dashboard/",
                        "Early Child Development", class="externallink", target="_blank"), " publication for further details."),
               tags$b("Denominators used in calculations"),
               p("The denominator for the breastfeeding indicators is the number of reviews with valid infant 
                 feeding data recorded (i.e. not ‘missing’ or ‘unknown’)."), 
               p("The average is calculated as the median value of the period specified."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SPC charts rules
observeEvent(input$btn_breastfed_rules,
             showModal(modalDialog(
               title = "How do we identify patterns in the data?",
               p("Controls charts follow a series of rules that help identify important changes in the data. 
                 These are the ones we used in this chart:"),
               tags$ul(tags$li("Shifts: Six or more consecutive data points above or below the centreline."),
                       tags$li("Trends: Five or more consecutive data points which are increasing or decreasing.")),
               p("Different control charts are used depending on the type of data involved.
                 For the breastfeeding ones we have used run charts."),
               p("Further information on these methods of presenting data can be found at the ",                      
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts.', class="externallink", target="_blank")),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))


###############################################.
## Reactive controls  ----
###############################################.
# Breastfeeding reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_bf <- renderUI({
  
  #Lists areas available in   
  areas_summary_bf <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_bf])
  
  selectizeInput("geoname_bf", label = NULL, choices = areas_summary_bf, selected = "")
})

###############################################.
## Reactive data ----
###############################################.
# Reactive breastfeeding dataset
breastfeeding_filt <- reactive({
  
  breastfeeding %>% filter(area_type == input$geotype_bf &
                             area_name == input$geoname_bf &
                             review == input$measure_select_bf)
})

###############################################.
## Reactive layout  ----
###############################################.

# Breastfeeding explorer
output$breastfeeding_explorer <- renderUI({
  
  if (input$measure_select_bf == "First visit") {
    run_charts_bf <- tagList(
      fluidRow(
        column(4,
               h4(paste0("Exclusively breastfed at ", tolower(input$measure_select_bf))),
               actionButton("btn_breastfed_rules", "How do we identify patterns in the data?", 
                            icon = icon('question-circle')),
               withSpinner(plotlyOutput("bf_excl_pc"))),
        column(4,
               h4(paste0("Overall breastfed at ", tolower(input$measure_select_bf))),
               fluidRow(br(), br()),
               withSpinner(plotlyOutput("bf_over_pc"))),
        column(4,
               h4(paste0("Ever breastfed")),
               fluidRow(br(), br()),
               withSpinner(plotlyOutput("bf_ever_pc"))))
    )
    
  } else if (input$measure_select_bf == "6-8 week") {
    run_charts_bf <- tagList(
      fluidRow(
        column(6,
               h4(paste0("Exclusively breastfed at ", tolower(input$measure_select_bf), " review")),
               actionButton("btn_breastfed_rules", "How do we identify patterns in the data?", 
                            icon = icon('question-circle')),
               withSpinner(plotlyOutput("bf_excl_pc"))),
        column(6,
               h4(paste0("Overall breastfed at ", tolower(input$measure_select_bf), " review")),
               br(),
               withSpinner(plotlyOutput("bf_over_pc"))))
    )
  }
  
  control_chart_commentary <- p("We have used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf', 
                                                       "‘control charts’",class="externallink"), "to present the percentages above.", br(),
                                "Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation.  
                      Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’", br(),
                                "The dots joined by a solid line in the chart above show the monthly percentage of children with developmental concerns recorded during the review selected from January 2019 onwards.", br(),  
                                "The other line, the centreline, is there to help show how unexpected any observed changes are. 
                      The centreline is an average (median) over the time period specified in the legend of the chart. ")
  
  tagList(
    run_charts_bf,
    fluidRow(control_chart_commentary),
    fluidRow(
      h4(paste0("Number of children by breastfeeding regularity at ", tolower(input$measure_select_bf), " review")),
      withSpinner(plotlyOutput("bf_types"))),
    fluidRow( # Valid Reviews,
      h4(paste0("Number of ", tolower(input$measure_select_bf), " reviews")),
      withSpinner(plotlyOutput("bf_reviews")))
  )
})

###############################################.
## Charts ----
###############################################.

# Breastfeeding charts
output$bf_reviews <- renderPlotly({
  
  trend_data <- breastfeeding_filt()
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
    
    # Modifying standard layout
    yaxis_plots[["title"]] <- "Number of visits"
    
    tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%B %y"),
                              "<br>", "Number of health visitor first visits: ", trend_data$no_reviews,
                              "<br>", "Number of valid first visits:  ", trend_data$no_valid_reviews,
                              "<br>", "Percentage of valid first visits: ", trend_data$pc_valid, "%"))
    
    # Creating time trend plot
    plot_ly(data = trend_data, x = ~month_review) %>% 
      add_lines(y = ~no_reviews, line = list(color = "#bf812d"),
                text = tooltip_trend, hoverinfo="text",
                name = "Number of first visits") %>% 
      add_lines(y = ~no_valid_reviews, line = list(color = "#74add1", dash = "dash"), 
                text = tooltip_trend, hoverinfo = "text", name = "Number of valid first visits") %>% 
      # Layout
      layout(margin = list(b = 80, t=5),
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% 
      # Configure modebar buttons
      config(displaylogo = F, displayModeBar = T, modeBarButtonsToRemove = bttn_remove)
  }
})

output$bf_types <- renderPlotly({
  
  trend_data <- breastfeeding_filt()
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
    
    # Modifying standard layout
    yaxis_plots[["title"]] <- "Number of children"
    
    if (input$measure_select_bf == "First visit") {
    tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%B %y"),
                              "<br>", "Exclusively breastfed: ", trend_data$exclusive_bf, 
                              " (", trend_data$pc_excl, "%)",
                              "<br>", "Overall breastfed: ", trend_data$overall_bf, 
                              " (", trend_data$pc_overall, "%)",
                              "<br>", "Ever breastfed: ", trend_data$ever_bf, 
                              " (", trend_data$pc_ever, "%)"))
    } else if (input$measure_select_bf == "6-8 week") {
      tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%B %y"),
                                "<br>", "Exclusively breastfed: ", trend_data$exclusive_bf, 
                                " (", trend_data$pc_excl, "%)",
                                "<br>", "Overall breastfed: ", trend_data$overall_bf, 
                                " (", trend_data$pc_overall, "%)"))
    }
    
    # Creating time trend plot
    plot_ly(data = trend_data, x = ~month_review) %>% 
      add_lines(y = ~exclusive_bf, line = list(color = "#bf812d"),
                text = tooltip_trend, hoverinfo="text",
                name = "Exclusively breastfed") %>% 
      add_lines(y = ~overall_bf, line = list(color = "black"),
                text = tooltip_trend, hoverinfo="text",
                name = "Overall breastfed") %>% 
      add_lines(y = ~ever_bf, line = list(color = "#74add1"),
                text = tooltip_trend, hoverinfo="text",
                name = "Ever breastfed") %>% 
      # Layout
      layout(margin = list(b = 80, t=5),
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% 
      # Configure modebar buttons
      config(displaylogo = F, displayModeBar = T, modeBarButtonsToRemove = bttn_remove)
  }
})

plot_runchart_bf <- function(var_chosen, centreline, shift, trend) {
  trend_data <- breastfeeding_filt()
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- "Percentage (%)"
    yaxis_plots[["range"]] <- c(0, 100)  # forcing range from 0 to 100%
    xaxis_plots[["range"]] <- c(min(trend_data$month_review), max(trend_data$month_review))
    
    measure_bf <- case_when(var_chosen == "pc_excl" ~ "% exclusively breastfed",
                            var_chosen == "pc_overall" ~ "% overall breastfed",
                            var_chosen == "pc_ever" ~ "% ever breastfed")
    
    if (var_chosen == "pc_excl") {
      tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                                "<br>", measure_bf, ": ", trend_data$pc_excl, "%"))
    } else  if (var_chosen == "pc_overall") {
      tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                                "<br>", measure_bf, ": ", trend_data$pc_overall, "%"))
    } else if (var_chosen == "pc_ever") {
      tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                                "<br>", measure_bf, ": ", trend_data$pc_ever, "%"))
    }

    
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~month_review) %>%
      add_lines(y = ~get(var_chosen),
                line = list(color = "black"), text=tooltip_trend, hoverinfo="text",
                marker = list(color = "black"), name = measure_bf) %>%
      add_lines(data=trend_data %>% filter(as.Date(month_review) < as.Date("2020-03-01")), 
                y = ~get(centreline), name = "Average up to February 2020",
                line = list(color = "blue", dash = "solid"), hoverinfo="none") %>% 
      add_lines(data=trend_data %>% filter(as.Date(month_review) >= as.Date("2020-02-01")),
                y = ~get(centreline), showlegend = FALSE,
                line = list(color = "blue", dash = "dash"), hoverinfo="none") %>%
      # adding shifts
      add_markers(data = trend_data %>% filter_at(shift, all_vars(. == T)), y = ~ get(var_chosen),
                  marker = list(color = "orange", size = 10, symbol = "circle"), name = "Shifts", hoverinfo="none") %>% 
      # adding trends
      add_markers(data = trend_data %>% filter_at(trend, all_vars(. == T)), y = ~ get(var_chosen),
                  marker = list(color = "green", size = 10, symbol = "square"), name = "Trends", hoverinfo="none") %>%  
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }
}

output$bf_excl_pc <- renderPlotly({
  plot_runchart_bf("pc_excl", "pc_excl_centreline", shift = "shift_excl", trend = "trend_excl")
})

output$bf_over_pc <- renderPlotly({
  plot_runchart_bf("pc_overall", "pc_overall_centreline", shift = "shift_over", trend = "trend_over")
})

output$bf_ever_pc <- renderPlotly({
  plot_runchart_bf("pc_ever", "pc_ever_centreline", shift = "shift_ever", trend = "trend_ever")
})

###############################################.
## Commentary ----
###############################################.
output$breastfeeding_commentary <- renderUI({
  tagList(
    bsButton("jump_to_breastfed",label = "Go to data"), #this button can only be used once
    h2("Breastfeeding - 30th September 2020"),
    p("Placeholder")
  ) #tagLIst bracket
})

###############################################.
## Data downloads ----
###############################################.

output$download_bf_data <- downloadHandler(
  filename ="breastfeeding_extract.csv",
  content = function(file) {
    write_csv(breastfeeding_filt() %>% select(-shift_excl, -trend_excl,
                                              -shift_ever, -trend_ever,
                                              -shift_over, -trend_over), file) } 
)


#END

