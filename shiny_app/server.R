#Server side

function(input, output) {
  
  plot_trend_chart <- function(min = 500, max = 1000, color_chosen = "blue") {
    
    trend_data <- data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                             value = runif(91, min, max)) %>% 
      filter(between(date_event, input$time_media[1], input$time_media[2]))
    
    xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
                        showline = TRUE, tickangle = 270, fixedrange=TRUE)
    
    yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
                        tickfont = list(size=14), titlefont = list(size=14)) 
    
    #Creating time trend plot
    trend_plot <- plot_ly(data=trend_data, x=~date_event,  y = ~value) %>% 
      add_trace(type = 'scatter', mode = 'lines+markers', 
                marker = list(size = 8, color = color_chosen), 
                line = list(color = color_chosen)) %>% 
      #Layout 
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>%  
      config(displayModeBar = FALSE, displaylogo = F) # taking out plotly logo button
    
  }
  
  output$hits_media_plot <- renderPlotly(plot_trend_chart())
  output$sentiment_plot <- renderPlotly(plot_trend_chart(0, 1))
  output$nhs24_plot <- renderPlotly(plot_trend_chart(color_chosen = "green"))
  output$aye_plot <- renderPlotly(plot_trend_chart(color_chosen = "green"))
  output$coronacases_plot <- renderPlotly(plot_trend_chart(color_chosen = "orange"))
  output$coronadeaths_plot <- renderPlotly(plot_trend_chart(color_chosen = "orange"))
  output$hits_hps_plot <- renderPlotly(plot_trend_chart(color_chosen = "green"))
  output$retweets_plot <- renderPlotly(plot_trend_chart(color_chosen = "orange"))
  output$word_cloud_plot <- renderPlotly({
    
    trend_data <- data.frame(word = c("Flu", "UK", "Crisis", "Outbreak", "Panic",
                                      "Calm", "Health", "Sneeze", "Caution", "Cases"),
                             value = exp(seq(1, 5.5, by = 0.5)))
    
    xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
                        showline = TRUE, tickangle = 270, fixedrange=TRUE,
                        categoryorder = "array", categoryarray = sort(trend_data[,"value"]))
    
    yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
                        tickfont = list(size=14), titlefont = list(size=14)) 
    
    
    plot_ly(data=trend_data, x=~word, y=~value,
            type = "bar", marker = list(color = "blue")) %>% 
      #Layout 
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>%  
      config(displayModeBar = FALSE, displaylogo = F) # taking out plotly logo button
    
  })
  
  output$correlation_plot <- renderPlotly({
    
    trend_data <- data.frame(ind1 = runif(91, 500, 1000),
                             ind2 = runif(91, 400, 700),
                             date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'))
    
    xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
                        showline = TRUE, tickangle = 270, fixedrange=TRUE)
    
    yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
                        tickfont = list(size=14), titlefont = list(size=14)) 
    
    fit <- lm(ind2 ~ ind1, data = trend_data)
    
    
    plot_ly(data=trend_data) %>% 
      add_markers(x=~ind1, y=~ind2,
                  type = "scatter", marker = list(color = "blue")) %>% 
      add_lines(x=~ind1, y = fitted(fit), mode = "lines") %>%
      #Layout 
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>%  
      config(displayModeBar = FALSE, displaylogo = F) # taking out plotly logo button
    
  })
  
  
  table_data <- data.frame(date_event = seq(as.Date('2020-01-02'), as.Date('2020-03-31'), by = 'day'),
                           value = runif(90, 500, 1000),
                           measure = rep(measure_list, 10)) %>% 
    mutate(value = round(value, 0))
  
  output$table_filtered <- DT::renderDataTable({
    
    DT::datatable(table_data)
  })
  
  
  output$download_table_csv <- downloadHandler(
    filename ="data_extract.csv",
    content = function(file) {
      write.csv(table_data,
                file, row.names=FALSE) } 
  )
  
} # server end