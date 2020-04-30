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
  # Show list of area names depending on areatype selected
  output$geoname_ui <- renderUI({
    
    areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype])
    
    selectizeInput("geoname", label = NULL,  
                   choices = areas_summary, selected = "")
    
  })
  
  # Disabling  admissions type if no admissions to hospital selected and
  # updating labels to say it's not available
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
  ## Modals ----
  ###############################################.
  #modal to provide information on what specialties are included in each group
  spec_modal <- modalDialog(
    h5(" List of specialties and what group they correspond to"),
    renderTable(spec_lookup), # creating table based on specialty lookup
    size = "l", align= "center",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
  # Link action button click to modal launch 
  observeEvent(input$btn_spec_groups, { showModal(spec_modal) }) 
  
  
  ###############################################.
  ## Reactive datasets ----
  ###############################################.
  # Rapid dataset filtered for admission_type, then used to create the admissions charts
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
    
    # text for titles of cut charts
    dataset <- case_when(input$measure_select == "Hospital admissions" ~ "admissions",
                         input$measure_select == "A&E attendances" ~ "attendances",
                         input$measure_select == "NHS 24 calls" ~ "calls",
                         input$measure_select == "Out of hours consultations" ~ "consultations")
    variation_title <- paste0("Percentage change in ", dataset, 
                              " compared with the corresponding time in ")
    
    total_title <- paste0("Weekly number of ", dataset)
    
    # Charts and rest of UI
    if (input$measure_select == "Hospital admissions") {
      tagList(#Hospital admissions
        h3("Weekly admissions to hospital (Source: RAPID dataset)"),
        plot_box("2020 compared with the 2016-2019 average", "adm_overall"),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by sex"), "adm_sex_var")),
                 column(6, plot_box(paste0(total_title, " by sex"), "adm_sex_tot"))),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by age group"), "adm_age_var")),
                 column(6, plot_box(paste0(total_title, " by age group"), "adm_age_tot"))),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by SIMD quintile"), "adm_depr_var")),
                 column(6, plot_box(paste0(total_title, " by SIMD quintile"), "adm_depr_tot"))),
        h4(paste0(variation_title, " 2016-2019 by specialty group")),
        fluidRow(column(4, pickerInput("adm_specialty", "Select one or more specialty groups",
                    choices = spec_list, multiple = TRUE,
                    selected = c("Medical", "Surgery"))),
        column(8, actionButton("btn_spec_groups", "Specialties and their groups", icon = icon('question-circle')))),
        withSpinner(plotlyOutput("adm_spec"))
      )
    } else if (input$measure_select == "A&E attendances") {
      tagList(#A&E Attendances
        h3("Weekly attendances to A&E departments (Source: Unscheduled Care Datamart)"),
        plot_box("2020 compared with the 2018-2019 average", "aye_overall"),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by sex"), "aye_sex_var")),
                 column(6, plot_box(paste0(total_title, " by sex"), "aye_sex_tot"))),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by age group"), "aye_age_var")),
                 column(6, plot_box(paste0(total_title, " by age group"), "aye_age_tot"))),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by SIMD quintile"), "aye_depr_var")),
                 column(6, plot_box(paste0(total_title, " by SIMD quintile"), "aye_depr_tot")))
      )
      
    } else if (input$measure_select == "NHS 24 calls") {
      tagList(# NHS 24 callw
        h3("Weekly calls to NHS24 service (Source: Unscheduled Care Datamart)"),
        plot_box("2020 compared with the 2018-2019 average", "nhs24_overall"),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by sex"), "nhs24_sex_var")),
                 column(6, plot_box(paste0(total_title, " by sex"), "nhs24_sex_tot"))),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by age group"), "nhs24_age_var")),
                 column(6, plot_box(paste0(total_title, " by age group"), "nhs24_age_tot"))),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by SIMD quintile"), "nhs24_depr_var")),
                 column(6, plot_box(paste0(total_title, " by SIMD quintile"), "nhs24_depr_tot")))
      )
    } else if (input$measure_select == "Out of hours consultations") {
      tagList(#Out of hours consultations
        h3("Weekly consultations to out of hours services (Source: Unscheduled Care Datamart)"),
        plot_box("2020 compared with the 2018-2019 average", "ooh_overall"),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by sex"), "ooh_sex_var")),
                 column(6, plot_box(paste0(total_title, " by sex"), "ooh_sex_tot"))),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by age group"), "ooh_age_var")),
                 column(6, plot_box(paste0(total_title, " by age group"), "ooh_age_tot"))),
        fluidRow(column(6, plot_box(paste0(variation_title, " 2018-2019 by SIMD quintile"), "ooh_depr_var")),
                 column(6, plot_box(paste0(total_title, " by SIMD quintile"), "ooh_depr_tot")))
      )
    }
  }) 
  
  ###############################################.
  ## Charts ----
  ###############################################.
  
  ###############################################.
  # Function that creates line trend charts in Plotly for different splits: age, sex, depr
  # THree parameters: pal_chose - what palette of colours you want
  # dataset - what data to use for the chart formatted as required
  # split - age, sex, or dep (simd deprivation)
  plot_trend_chart <- function(dataset, pal_chose, split, type = "variation", data_name = NULL) {

    trend_data <- dataset %>% # filtering data by cut and area name
      filter(type == split &
              area_name == input$geoname )

    # Formatting age groups as factor so they appear in the correct order in the legend
    if (split == "age") {
      trend_data <- trend_data %>% 
        mutate(category = factor(category, levels = c("Under 5", "5 - 14", "15 - 44", 
                                                      "45 - 64", "65 - 74", 
                                                        "75 - 84", "85 and over"))) 
    }
    
    if (type == "variation") {
      
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                              "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", "Change from average: ", trend_data$variation, "%"))
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- "% change compared with previous years average"
    # yaxis_plots[["range"]] <- c(x = -100, 80)
    
    #Creating time trend plot
    trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~variation) 
    
    
    } else if (type == "total") {
      
      ###############################################
      # Creating objects that change depending on dataset
      yaxis_title <- case_when(data_name == "adm" ~ "Number of admissions",
                               data_name == "aye" ~ "Number of attendances",
                               data_name == "ooh" ~ "Number of consultations",
                               data_name == "nhs24" ~ "Number of calls")
      
      #Modifying standard layout
      yaxis_plots[["title"]] <- yaxis_title
      
      hist_legend <- case_when(data_name == "adm" ~ "Average 2016-2019",
                               data_name %in% c("aye", "nhs24", "ooh") ~ "Average 2018-2019")
      
      measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                                data_name == "aye" ~ "Attendances: ",
                                data_name == "ooh" ~ "Consultations: ",
                                data_name == "nhs24" ~ "Calls: ")
      
      #Text for tooltip
      tooltip_trend <- c(paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                                "<br>", measure_name, trend_data$count,
                                "<br>", "Historic average: ", trend_data$count_average))

      #Creating time trend plot
      trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~count) 
    
    }
    
    #Creating time trend plot
    trend_plot %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~category, colors = pal_chose,
                text=tooltip_trend, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 

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
                             data_name %in% c("aye", "nhs24", "ooh") ~ "Average 2018-2019")
      
    measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                             data_name == "aye" ~ "Attendances: ",
                             data_name == "ooh" ~ "Consultations: ",
                             data_name == "nhs24" ~ "Calls: ")
    
    #Text for tooltip
    tooltip_trend <- c(paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", measure_name, trend_data$count,
                              "<br>", "Historic average: ", trend_data$count_average))
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~week_ending) %>%
      # 2020 line
      add_lines(y = ~count, line = list(color = pal_overall[1]),
                text=tooltip_trend, hoverinfo="text",
                name = "2020") %>%
      # Average of previous years line
      add_lines(y = ~count_average, line = list(color = pal_overall[2], dash = 'dash'),
                text=tooltip_trend, hoverinfo="text",
                name = hist_legend) %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
    
  }
  
  ###############################################.
  # Creating plots for each cut and dataset
  output$aye_overall <- renderPlotly({plot_overall_chart(aye, "aye")})
  output$aye_sex_var <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex")})
  output$aye_age_var <- renderPlotly({plot_trend_chart(aye, pal_age, "age")})
  output$aye_depr_var <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep")})
  output$aye_sex_tot <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex", "total", "aye")})
  output$aye_age_tot <- renderPlotly({plot_trend_chart(aye, pal_age, "age", "total", "aye")})
  output$aye_depr_tot <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep", "total", "aye")})
  
  output$ooh_overall <- renderPlotly({plot_overall_chart(ooh, "ooh")})
  output$ooh_sex_var <- renderPlotly({plot_trend_chart(ooh, pal_sex, "sex")})
  output$ooh_age_var <- renderPlotly({plot_trend_chart(ooh, pal_age, "age")})
  output$ooh_depr_var <- renderPlotly({plot_trend_chart(ooh, pal_depr, "dep")})
  output$ooh_sex_tot <- renderPlotly({plot_trend_chart(ooh, pal_sex, "sex", "total", "ooh")})
  output$ooh_age_tot <- renderPlotly({plot_trend_chart(ooh, pal_age, "age", "total", "ooh")})
  output$ooh_depr_tot <- renderPlotly({plot_trend_chart(ooh, pal_depr, "dep", "total", "ooh")})
  
  output$nhs24_overall <- renderPlotly({plot_overall_chart(nhs24, "nhs24")})
  output$nhs24_sex_var <- renderPlotly({plot_trend_chart(nhs24, pal_sex, "sex")})
  output$nhs24_age_var <- renderPlotly({plot_trend_chart(nhs24, pal_age, "age")})
  output$nhs24_depr_var <- renderPlotly({plot_trend_chart(nhs24, pal_depr, "dep")})
  output$nhs24_sex_tot <- renderPlotly({plot_trend_chart(nhs24, pal_sex, "sex", "total", "nhs24")})
  output$nhs24_age_tot <- renderPlotly({plot_trend_chart(nhs24, pal_age, "age", "total", "nhs24")})
  output$nhs24_depr_tot <- renderPlotly({plot_trend_chart(nhs24, pal_depr, "dep", "total", "nhs24")})
  
  output$adm_overall <- renderPlotly({plot_overall_chart(rapid_filt(), "adm")})
  output$adm_sex_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_sex, "sex")})
  output$adm_age_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_age, "age")})
  output$adm_depr_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_depr, "dep")})
  output$adm_sex_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_sex, "sex", "total", "adm")})
  output$adm_age_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_age, "age", "total", "adm")})
  output$adm_depr_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_depr, "dep", "total", "adm")})
  
  output$adm_spec <- renderPlotly({

    trend_data <- rapid %>% filter(type == "sex") %>%
      filter(area_name == input$geoname &
               admission_type == input$adm_type &
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
    tooltip_trend <- c(paste0(trend_data$spec, "<br>", 
                              "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", "Change from average: ", trend_data$variation))
    
    yaxis_plots[["title"]] <- "% change compared with previous years average"
    # yaxis_plots[["range"]] <- c(-100, 100)

    #Creating time trend plot
    plot_ly(data=trend_data, x=~week_ending,  y = ~variation) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                color = ~spec, colors = trend_palette, marker = list(size = 8),
                symbol = ~spec, symbols = symbols_trend,
                text=tooltip_trend, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             showlegend = TRUE, # in case only one spec selected, it still shows
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% # position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  })

###############################################.
##reactive data to show in app
  data_table <- reactive({
    # Change dataset depending on what user selected
    switch(input$data_select,
      "Hospital admissions" = rapid %>% rename(specialty = spec),
      "A&E attendances" = aye,
      "NHS 24 calls" = nhs24,
      "Out of hours consultations" = ooh) %>% 
      # Formatting to a "nicer" style
      select(-type) %>% 
      rename(average_pre2020 = count_average) %>% 
      # Note: character variables are converted to factors in each
      # dataset for use in the table
      # This is because dropdown prompts on the table filters only
      # appear for factors
      mutate_if(is.character, as.factor) %>% 
      mutate(category = recode_factor(category, "All" = "All", "Female" = "Female", "Male" = "Male",
                                      "1 - most deprived" = "Quintile 1 - most deprived",
                                      "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4",
                                      "5 - least deprived" = "Quintile 5 - least deprived",
                                      "Under 5" = "Aged under 5", "5 - 14"= "Aged 5 to 14",
                                      "15 - 44" = "Aged 15 to 44","45 - 64" = "Aged 45 to 64",
                                      "65 - 74" = "Aged 65 to 74", "75 - 84" = "Aged 75 to 84", 
                                      "85 and over" = "Aged 85 and over"),
             week_ending = format(week_ending, "%d %b %y")) %>% 
      rename_all(list(~str_to_sentence(.))) %>% # initial capital letter
      select(sort(current_vars())) # order columns alphabetically
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
  # Data download of data table. 
  output$download_table_csv <- downloadHandler(
    filename ="data_extract.csv",
    content = function(file) {
      # This downloads only the data the user has selected using the table filters
      write_csv(data_table()[input[["table_filtered_rows_all"]], ], file) 
      } 
  )
  
  # For the charts at the moment the data download is for the overall one,
  # need to think how to allow downloading for each chart
  # Reactive dataset that gets the data the user is visualisaing ready to download
  overall_data_download <- reactive({
    switch(
      input$measure_select,
      "Hospital admissions" = filter_data(rapid_filt()),
      "A&E attendances" = filter_data(aye),
      "NHS 24 calls" = filter_data(nhs24),
      "Out of hours consultations" = filter_data(ooh)
    ) %>% 
      select(area_name, week_ending, count, count_average) %>% 
      rename(average_pre2020 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y"))
  })

  output$download_chart_data <- downloadHandler(
    filename ="data_extract.csv",
    content = function(file) {
      write_csv(overall_data_download(),
                file) } 
  )
  
} # server end