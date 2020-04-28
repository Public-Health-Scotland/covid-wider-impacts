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
  
  # Disabling  admissions type if no admissions to hospital selected
  observeEvent({input$measure_select}, {
      if (input$measure_select == "Hospital admissions") {
        enable("adm_type")

        updateSelectInput(session, "adm_type",
                          label = "Step 3. Select type of admission.")

      } else if (input$measure_select != "Hospital admissions") {
        disable("adm_type")
        
        updateSelectInput(session, "adm_type",
                          label = "Step 3. Select type of admission (not available).")

      }
      
    })
  
  ###############################################.
  ## Reactive datasets ----
  ###############################################.
  # Rapid dataset filtered for 
  rapid_filt <- reactive({
    rapid %>% filter(admission_type == input$adm_type &
                       spec == "All")
  })
  
  # Function to filter the datasets for the overall charts and download data based on user input
  filter_data <- function(dataset) {
    dataset %>% filter(type == "sex") %>%
      filter(area_name == input$geoname &
               category == "All")
  }
  
  ###############################################.
  ## Reactive layout  ----
  ###############################################.
  # The charts and text shown on the app will depend on what the user wants to see
  output$data_explorer <- renderUI({
    if (input$measure_select == "Hospital admissions") {
      tagList(#Hospital admissions
        h4("Admissions to hospital"),
        plot_box("2020 compared with average from previous years", "adm_overall"),
        plot_box("Variation of 2020 against average of previous years by sex group", "adm_sex"),
        plot_box("Variation of 2020 against average of previous years by age group", "adm_age"),
        plot_box("Variation of 2020 against average of previous years by deprivation quintile", "adm_depr"),
        h4("Variation of 2020 against average of previous years by specialty (not distinguishing between planned or emergency admissions)"),
        pickerInput("adm_specialty", "Select one or more specialties",
                    choices = spec_list, multiple = TRUE,
                    selected = c("Medical", "Surgery")),
        withSpinner(plotlyOutput("adm_spec"))
      )
} else if (input$measure_select == "A&E attendances") {
  tagList(#A&E Attendances
    h4("Attendances to A&E departments"),
    plot_box("2020 compared with average from previous years", "aye_overall"),
    plot_box("Variation of 2020 against average of previous years by sex", "aye_sex"),
    plot_box("Variation of 2020 against average of previous years by age group", "aye_age"),
    plot_box("Variation of 2020 against average of previous years by deprivation quintile", "aye_depr")
    )
  
} else if (input$measure_select == "NHS 24 calls") {
  tagList(# NHS 24 callw
    h4("Calls to NHS24 service"),
    plot_box("2020 compared with average from previous years", "nhs24_overall"),
    plot_box("Variation of 2020 against average of previous years by sex", "nhs24_sex"),
    plot_box("Variation of 2020 against average of previous years by age group", "nhs24_age"),
    plot_box("Variation of 2020 against average of previous years by deprivation quintile", "nhs24_depr")
  )
} else if (input$measure_select == "Out of hours consultations") {
  tagList(#Out of hours consultations
    h4("Consultations to out of hours services"),
    plot_box("2020 compared with average from previous years", "ooh_overall")
  )
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
      filter(area_name == input$geoname )

    if (split == "age") {
      trend_data <- trend_data %>% 
        mutate(category = factor(category, levels = c("Under 5", "5 - 14", "15 - 44", "
                                                        45 - 64", "65 -74", 
                                                        "75 -84", "85 and over"))) 
    }
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                              "Week ending: ", format(trend_data$date, "%d %b %y"),
                              "<br>", "Change from average: ", trend_data$variation, "%"))
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- "% change compared with historic average"

    #Creating time trend plot
    plot_ly(data=trend_data, x=~date,  y = ~variation) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~category, colors = pal_chose,
                text=tooltip_trend, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>%
      config(displaylogo = F) # taking out plotly logo button

  }
  
  plot_overall_chart <- function(dataset, data_name, yaxis_title) {
    
    # Filtering dataset to include only overall figures
    trend_data <- filter_data(dataset)
    
    ###############################################
    # Creating objects that change depending on dataset
    yaxis_title <- case_when(data_name == "adm" ~ "Number of admissions",
                             data_name == "aye" ~ "Number of attendances",
                             data_name == "ooh" ~ "Number of consultations",
                             data_name == "nhs24" ~ "Number of calls")
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- yaxis_title
    
    hist_legend <- case_when(data_name == "adm" ~ "Average 2016-2019",
                             data_name %in% c("aye", "nhs24") ~ "Average 2018-2019",
                             data_name == "ooh" ~ "Average xxx")
      
    measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                             data_name == "aye" ~ "Attendances: ",
                             data_name == "ooh" ~ "Consultations: ",
                             data_name == "nhs24" ~ "Calls: ")
    
    #Text for tooltip
    tooltip_trend <- c(paste0("Week ending: ", format(trend_data$date, "%d %b %y"),
                              "<br>", measure_name, trend_data$count,
                              "<br>", "Historic average: ", trend_data$count_average))
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~date) %>%
      add_lines(y = ~count, line = list(color = pal_overall[1]),
                text=tooltip_trend, hoverinfo="text",
                name = "2020") %>%
      add_lines(y = ~count_average, line = list(color = pal_overall[2], dash = 'dash'),
                text=tooltip_trend, hoverinfo="text",
                name = hist_legend) %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>% 
      config(displaylogo = F) # taking out plotly logo button
    
  }
  
  ###############################################.
  # Creating plots for each cut and dataset
  output$aye_overall <- renderPlotly({plot_overall_chart(aye, "aye")})
  output$aye_sex <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex")})
  output$aye_age <- renderPlotly({plot_trend_chart(aye, pal_age, "age")})
  output$aye_depr <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep")})
  
  output$ooh_overall <- renderPlotly({plot_overall_chart(ooh, "ooh")})
  
  output$adm_overall <- renderPlotly({plot_overall_chart(rapid_filt(), "adm")})
  output$adm_sex <- renderPlotly({plot_trend_chart(rapid_filt(), pal_sex, "sex")})
  output$adm_age <- renderPlotly({plot_trend_chart(rapid_filt(), pal_age, "age")})
  output$adm_depr <- renderPlotly({plot_trend_chart(rapid_filt(), pal_depr, "dep")})
  
  output$nhs24_overall <- renderPlotly({plot_overall_chart(nhs24, "nhs24")})
  output$nhs24_sex <- renderPlotly({plot_trend_chart(nhs24, pal_sex, "sex")})
  output$nhs24_age <- renderPlotly({plot_trend_chart(nhs24, pal_age, "age")})
  output$nhs24_depr <- renderPlotly({plot_trend_chart(nhs24, pal_depr, "dep")})
  output$adm_spec <- renderPlotly({

    trend_data <- rapid %>% filter(type == "sex") %>%
      filter(area_name == input$geoname &
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
                              "<br>", "Change from average: ", trend_data$variation))

    #Creating time trend plot
    plot_ly(data=trend_data, x=~date,  y = ~variation) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                color = ~spec, colors = trend_palette, marker = list(size = 8),
                symbol = ~spec, symbols = symbols_trend,
                text=tooltip_trend, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             showlegend = TRUE,
             yaxis = yaxis_plots, xaxis = xaxis_plots) %>%
      config(displaylogo = F) # taking out plotly logo button

  })

###############################################.
## Table ----
###############################################.

  
  data_table <- reactive({
    # Reformat dates? so they become 22 March 2020?.
    # Think about the variable names
    switch(
      input$data_select,
      "Hospital admissions" = rapid,
      "A&E attendances" = aye,
      "NHS24 calls" = nhs24,
      "Out of hours consultations" = ooh
    ) %>% 
      rename_all(list(~str_to_sentence(.))) %>% # initial capital letter
      # Note: character variables are converted to factors in each
      # dataset for use in the table
      # This is because dropdown prompts on the table filters only
      # appear for factors
      mutate_if(is.character, as.factor) %>% 
      select(sort(current_vars())) # orde columns alphabetically
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
  
  ###############################################.
  ## Data downloads ----
  ###############################################.
  
  output$download_table_csv <- downloadHandler(
    filename ="data_extract.csv",
    content = function(file) {
      write_csv(data_table()
                [input[["table_filtered_rows_all"]], ],
                file) } 
  )
  
  # Reactive dataset that gets the data the user is visualisaing ready to download
  overall_data_download <- reactive({
    switch(
      input$measure_select,
      "Hospital admissions" = filter_data(rapid_filt()),
      "A&E attendances" = filter_data(aye),
      "NHS24 calls" = filter_data(nhs24),
      "Out of hours consultations" = filter_data(ooh)
    ) %>% 
      select(area_name, date, count, count_average) %>% 
      rename(average_pre2020 = count_average)
  })

  output$download_chart_data <- downloadHandler(
    filename ="data_extract.csv",
    content = function(file) {
      write_csv(overall_data_download(),
                file) } 
  )
  
} # server end