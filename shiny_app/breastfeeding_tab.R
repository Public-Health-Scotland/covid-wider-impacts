##Server script for breastfeedingtab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_child_modal, 
             showModal(modalDialog(#CHILD HEALTH MODAL
               title = "What is the data source?",
               p("The information shown on the numbers of children eligible for routine preschool reviews is taken from the",
                 tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12", 
                        "Scottish Immunisation and Recall System (SIRS)", class="externallink"),
                 ". The information recorded at each review is taken from the",
                 tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=10",
                        "Child Health Systems Programme-PreSchool (CHSP-PS)", class="externallink"),
                 "."),
               p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation of children when a scheduled vaccination is due."),
               p("CHSP-PS is an electronic system used by all NHS Boards in Scotland. The CHSP Pre-School system supports the delivery of the child health programme by facilitating the automated call and recall of children for the agreed schedule of child health reviews for pre-school children. Child health reviews incorporate assessment of children's health, development, and wider wellbeing alongside provision of health promotion advice and parenting support."),
               p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink")," routinely receives quarterly data extracts from SIRS and CHSP-PS for the purpose of producing and ",
                 (tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/child-health-pre-school-review-coverage/","publishing",class="externallink"))," coverage rates for child health reviews. To allow more rapid monitoring of the impact of Covid-19 on child health review coverage rates, PHS is also currently extracting a sub-set of data from SIRS & CHSP-PS each month."),
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
  tagList(
    # Valid Reviews
    fluidRow(
      column(6,
             h4(paste0("Number of (valid) reviews at ", input$measure_select_bf)),
             withSpinner(plotlyOutput("bf_reviews"))),
      column(6,
             h4(paste0("Percentage of (valid) reviews at ", input$measure_select_bf)),
             withSpinner(plotlyOutput("bf_reviews_pc")))),
    # Exclusively breastfed
    fluidRow(
      column(6,
             h4(paste0("Number of exclusively breastfed at ", input$measure_select_bf)),
             withSpinner(plotlyOutput("bf_excl"))),
      column(6,
             h4(paste0("Percentage of exclusively breastfed at ", input$measure_select_bf)),
             withSpinner(plotlyOutput("bf_excl_pc"))))
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
    yaxis_plots[["title"]] <- "Number of (valid) first visits"
    
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

output$bf_reviews_pc <- renderPlotly({
  
  trend_data <- breastfeeding_filt()
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- "Percentage (%)"
    xaxis_plots[["range"]] <- c(min(trend_data$month_review), max(trend_data$month_review))
    
    tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                              "<br>", "% valid first visits: ", trend_data$pc_valid, "%"))
    
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~month_review) %>%
      add_lines(y = ~pc_valid,  
                line = list(color = "black"), text=tooltip_trend, hoverinfo="text",
                marker = list(color = "black"), name = "% valid first visits") %>% 
      add_lines(y = ~pc_valid_centreline, name = "Average up to February 2020",
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline") %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }
})

output$bf_excl <- renderPlotly({
  
  trend_data <- breastfeeding_filt()
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
    
    # Modifying standard layout
    yaxis_plots[["title"]] <- "Number of (valid) first visits"
    
    tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%B %y"),
                              "<br>", "Number exclusively breastfed: ", trend_data$exclusive_bf,
                              "<br>", "Percentage exclusively breastfed: ", trend_data$pc_excl, "%"))
    
    # Creating time trend plot
    plot_ly(data = trend_data, x = ~month_review) %>% 
      add_lines(y = ~exclusive_bf, line = list(color = "#bf812d"),
                text = tooltip_trend, hoverinfo="text",
                name = "Number exclusively breastfed") %>% 
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

output$bf_excl_pc <- renderPlotly({
  
  trend_data <- breastfeeding_filt()
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- "Percentage (%)"
    xaxis_plots[["range"]] <- c(min(trend_data$month_review), max(trend_data$month_review))
    
    tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                              "<br>", "% valid first visits: ", trend_data$pc_excl, "%"))
    
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~month_review) %>%
      add_lines(y = ~pc_excl,  
                line = list(color = "black"), text=tooltip_trend, hoverinfo="text",
                marker = list(color = "black"), name = "% exclusively breastfed") %>% 
      add_lines(y = ~pc_excl_centreline, name = "Average up to February 2020",
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline") %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }
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



#END

