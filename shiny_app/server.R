#Server side

function(input, output, session) {
  
  # For debugging
  observeEvent(input$browser, browser())
  
  ###############################################.
  # To move around tabs 
  observeEvent(input$jump_summary, {
    updateTabsetPanel(session, "intabset", selected = "summary")
  })
  
  observeEvent(input$jump_table, {
    updateTabsetPanel(session, "intabset", selected = "table")
  })
  
  
  ###############################################.
  # Reactive controls 
  # For areaname depending on areatype selected
  output$geoname_ui <- renderUI({
    
    areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype])
    
    selectizeInput("geoname", label = NULL,  
                   choices = areas_summary, selected = "")
    
  })
  
  # Time input depending on what is available
  output$time_period_ui <- renderUI({
  
    sliderInput("time_period", label = "Step 2 - Select the time period of interest",
                min = as.Date('2014-04-22'), max = as.Date('2020-04-22'),
                value = c(as.Date('2020-03-22'), as.Date('2020-04-22')),
                step = 1)
    
  })


  ###############################################.
  # Function that creates line trend charts in Plotly for different splits
  # THree parameters: pal_chose - what palette of colours you want
  # dataset - what data to use for the chart formatted as required
  # split - age, sex, or deprivation
  plot_trend_chart <- function(dataset, pal_chose, split) {
    
    trend_data <- dataset %>% filter(type == split) %>%
        filter(between(date, as.Date(input$time_period[1]), as.Date(input$time_period[2])))

    # Style of x and y axis
    xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                        showline = TRUE, tickangle = 270, fixedrange=TRUE)

    yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                        tickfont = list(size=14), titlefont = list(size=14))

    #Creating time trend plot
    plot_ly(data=trend_data, x=~date,  y = ~count) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~category, colors = pal_chose) %>%
      #Layout
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>% 
             #legend = list(orientation = 'h', x = 50, y = 100)) %>%
      config(displaylogo = F) # taking out plotly logo button

  }
  
  # Creating plots for each cut and dataset
  # output$aye_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  # output$aye_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  # output$aye_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  # output$ooh_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  # output$ooh_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  # output$ooh_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  output$adm_sex <- renderPlotly({plot_trend_chart(rapid, pal_sex, "sex")})
  output$adm_age <- renderPlotly({plot_trend_chart(rapid, pal_age, "age")})
  output$adm_depr <- renderPlotly({plot_trend_chart(rapid, pal_depr, "depr")})
  # output$test_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  # output$test_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  # output$test_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  # output$nhs24_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  # output$nhs24_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  # output$nhs24_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})

###############################################.
# Table 
  
  data_table <- reactive({
    # Add switch and filter to swap between different datasets
    # https://github.com/Health-SocialCare-Scotland/Hospital-Acute-Activity/blob/master/Data-Explorer/server.R
    # Reformat dates? so they become 22 March 2020?.
    # Think about the variable names
    
    # Note: character variables are converted to factors in each
    # dataset for use in the table
    # This is because dropdown prompts on the table filters only
    # appear for factors
    rapid %>% 
      rename(Date = date, Count = count, Type = type, Category = category) %>% 
      mutate_if(is.character, as.factor)
    
  })
  
  output$table_filtered <- DT::renderDataTable({
    
    # Remove the underscore from column names in the table
    table_colnames  <-  gsub("_", " ", colnames(data_table()))
    
    DT::datatable(data_table(), style = 'bootstrap',
                  class = 'table-bordered table-condensed',
                  rownames = FALSE,
                  options = list(pageLength = 20,
                                 dom = 'tip',
                                 autoWidth = TRUE),
                  filter = "top",
                  colnames = table_colnames)
    
  })
  
  
  output$download_table_csv <- downloadHandler(
    filename ="data_extract.csv",
    content = function(file) {
      write.csv(rapid,
                file, row.names=FALSE) } 
  )
  
} # server end