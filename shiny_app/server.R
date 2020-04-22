#Server side

function(input, output, session) {
  
  # For debugging
  observeEvent(input$browser, browser())
  
  # Reactive controls for areaname depending on areatype selected
  output$geoname_ui <- renderUI({
    
    areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype])
    
    selectizeInput("geoname", label = NULL,  
                   choices = areas_summary, selected = "")
    
  })

  
  
  
  plot_trend_chart <- function(pal_chose, type) {

    if (type == "sex") {
      trend_data <- rbind(data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                               value = runif(91, 1000, 1200), type = "All"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 500, 700), type = "Female"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 700, 900), type = "Male")) #%>%
       # filter(between(date_event, input$time_media[1], input$time_media[2]))
    } else if (type == "age") {
      trend_data <- rbind(data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 300, 400), type = "0-10"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 400, 500), type = "11-20"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 500, 600), type = "21-30"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 600, 700), type = "31-40"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 700, 800), type = "41-50"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 800, 900), type = "51-65"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 900, 1000), type = "+75")) #%>%
        #filter(between(date_event, input$time_media[1], input$time_media[2]))
    } else if (type == "depr") {
      trend_data <- rbind(data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 900, 1000), type = "1 - Most deprived"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 800, 900), type = "2"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 700, 800), type = "3"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 600, 700), type = "4"),
                          data.frame(date_event = seq(as.Date('2020-01-01'), as.Date('2020-03-31'), by = 'day'),
                                     value = runif(91, 500, 600), type = "5 - least deprived")) #%>%
        #filter(between(date_event, input$time_media[1], input$time_media[2]))
    }

    xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14),
                        showline = TRUE, tickangle = 270, fixedrange=TRUE)

    yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4,
                        tickfont = list(size=14), titlefont = list(size=14))

    #Creating time trend plot
    plot_ly(data=trend_data, x=~date_event,  y = ~value) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                color = ~type, colors = pal_chose) %>%
      #Layout
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>%
      config(displayModeBar = FALSE, displaylogo = F) # taking out plotly logo button

  }
  
  output$aye_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  output$aye_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  output$aye_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  output$ooh_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  output$ooh_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  output$ooh_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  output$adm_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  output$adm_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  output$adm_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  output$disch_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  output$disch_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  output$disch_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  output$test_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  output$test_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  output$test_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  output$nhs24_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  output$nhs24_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  output$nhs24_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})

###############################################.
# Table 
  
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