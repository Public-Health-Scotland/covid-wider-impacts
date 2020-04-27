#Server side

function(input, output, session) {
  
  # For debugging
  # observeEvent(input$browser, browser())
  
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
  ## Reactive layout  ----
  ###############################################.
  # The charts and text shown on the app will depend on what the user wants to see
  output$data_explorer <- renderUI({
    if (input$measure_select == "Hospital admissions") {
      tagList(#Hospital admissions
    h4("Admissions to hospital"),
    plot_box("By sex", "adm_sex"),
    plot_box("By age group", "adm_age"),
    plot_box("By deprivation quintile", "adm_depr"),
    pickerInput("adm_specialty", "Select one or more specialties",
                choices = spec_list, multiple = TRUE, 
                selected = c("Accident & Emergency")),
    plot_box("By specialty (not distinguishing between planned or emergency admissions)", "adm_spec"))
} else if (input$measure_select == "A&E attendances") {
  tagList(#A&E Attendances
    h4("Attendances to A&E departments"),
    plot_box("2020 compared with average from previous years", "aye_overall"))
  
} else if (input$measure_select == "NHS 24 calls") {
  
}
    
  })
  


  ###############################################.
  ## Charts ----
  ###############################################.
  
  ###############################################.
  # Function that creates line trend charts in Plotly for different splits
  # THree parameters: pal_chose - what palette of colours you want
  # dataset - what data to use for the chart formatted as required
  # split - age, sex, or deprivation
  plot_trend_chart <- function(dataset, pal_chose, split) {
    
    trend_data <- dataset %>% filter(type == split) %>%
        filter(between(date, as.Date(input$time_period[1]), as.Date(input$time_period[2])) &
                 area_name == input$geoname &
                 admission_type == input$adm_type &
                 spec == "All")
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$category, "<br>", trend_data$date,
                              "<br>", "Admissions: ", trend_data$count))

    #Creating time trend plot
    plot_ly(data=trend_data, x=~date,  y = ~count) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~category, colors = pal_chose,
                text=tooltip_trend, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>% 
             #legend = list(orientation = 'h', x = 50, y = 100)) %>%
      config(displaylogo = F) # taking out plotly logo button

  }
  
  plot_overall_chart <- function(dataset, yaxis_title) {
    
    trend_data <- dataset %>% filter(type == "sex") %>%
      filter(area_name == input$geoname &
               category == "All")
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- yaxis_title
    
    #Text for tooltip
    tooltip_trend <- c(paste0("Week ending: ", trend_data$date,
                              "<br>", "Admissions: ", trend_data$count,
                              "<br>", "Historic average: ", trend_data$count_average))
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~date) %>%
      add_lines(y = ~count, line = list(color = pal_overall[1]),
                text=tooltip_trend, hoverinfo="text",
                name = "2020") %>%
      add_lines(y = ~count_average, line = list(color = pal_overall[2], dash = 'dash'),
                text=tooltip_trend, hoverinfo="text",
                name = "Average 2018-2019") %>%
      #Layout
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>% 
      config(displaylogo = F) # taking out plotly logo button
    
  }
  
  ###############################################.
  # Creating plots for each cut and dataset
  output$aye_overall <- renderPlotly({plot_overall_chart(aye, "Number of attendances to A&E")})
  # output$aye_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  # output$aye_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  # output$aye_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  # output$ooh_sex <- renderPlotly({plot_trend_chart(pal_sex, "sex")})
  # output$ooh_age <- renderPlotly({plot_trend_chart(pal_age, "age")})
  # output$ooh_depr <- renderPlotly({plot_trend_chart(pal_depr, "depr")})
  output$adm_sex <- renderPlotly({plot_trend_chart(rapid, pal_sex, "sex")})
  output$adm_age <- renderPlotly({plot_trend_chart(rapid, pal_age, "age")})
  output$adm_depr <- renderPlotly({plot_trend_chart(rapid, pal_depr, "depr")})
  output$adm_spec <- renderPlotly({
    
    trend_data <- rapid %>% filter(type == "sex") %>%
      filter(between(date, as.Date(input$time_period[1]), as.Date(input$time_period[2])) &
               area_name == input$geoname &
               # admission_type == input$adm_type &
               category == "All" &
               spec %in% input$adm_specialty)
    
    #Creating palette of colors: colorblind proof
    #First obtaining length of each geography type, if more than 6, then 6, 
    # this avoids issues. Extra selections will not be plotted
    trend_length <- length(input$adm_specialty)
    
    # First define the palette of colours used, then set a named vector, so each color
    # gets assigned to an area. I think is based on the order in the dataset, which
    # helps because Scotland is always first so always black.
    trend_palette <- c("#000000", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                       "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928")
    
    trend_scale <- c(setNames(trend_palette, unique(trend_data$spec)[1:trend_length]))
    trend_col <- trend_scale[1:trend_length]
    
    # Same approach for symbols
    symbols_palette <-  c('circle', 'diamond', 'circle', 'diamond', 'circle', 'diamond',
                          'square','triangle-up', 'square','triangle-up', 'square','triangle-up')
    symbols_scale <- c(setNames(symbols_palette, unique(trend_data$spec)[1:trend_length]))
    symbols_trend <- symbols_scale[1:trend_length]
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$spec, "<br>", trend_data$date,
                              "<br>", "Admissions: ", trend_data$count))
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~date,  y = ~count) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~spec, colors = trend_palette,
                text=tooltip_trend, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>% 
      config(displaylogo = F) # taking out plotly logo button
    
  })
  

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
  
  output$download_chart_data <- downloadHandler(
    filename ="data_extract.csv",
    content = function(file) {
      write.csv(rapid,
                file, row.names=FALSE) } 
  )
  
} # server end