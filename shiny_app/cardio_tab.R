# Server side for cardiovascular tab

###############################################.
## Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
output$geoname_cardio_ui <- renderUI({
  
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype])
  
  selectizeInput("geoname_cardio", label = NULL,  
                 choices = areas_summary, selected = "")
  
})

###############################################.
## Modals ----
###############################################.

# Link action button click to modal launch 
observeEvent(input$btn_cardio_modal, 
             
             if (input$measure_cardio_select == "rapid") {
               showModal(modalDialog(#RAPID ADMISSIONS MODAL
                 title = "What is the data source?",
                 p(""),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }  else if (input$measure_cardio_select == "aye") {
               showModal(modalDialog(#A&E MODAL
                 title = "What is the data source?",
                 p(""),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_cardio_select == "cath") {
               showModal(modalDialog(#CATH A&E MODAL
                 title = "What is the data source?",
                 p(""),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
)
###############################################.
## Reactive datasets ----
###############################################.


###############################################.
## Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$cardio_explorer <- renderUI({
  # Charts and rest of UI
  if (input$measure_cardio_select == "cath") {
    tagList( # Cath cases Golden Jubilee
      h3("Weekly coronary cases at the Golden Jubilee Hospital"),
      actionButton("btn_cardio_modal", "Data source: Golden Jubilee", icon = icon('question-circle')),
      plot_box("2020 compared with 2019", "cath_gj_overall"),
      plot_cut_box("Percentage change in cases compared with the 
                   corresponding time in 2019 by admission type", "cath_adm_gj_var",
                   "Weekly number of cases by admission type", "cath_adm_gj_tot")
    )
    } else if (input$measure_cardio_select == "rapid") {
      tagList(#Hospital admissions
      )
    } else if (input$measure_cardio_select == "aye") {
      tagList(# A&E attendances (cardiovascular only)
        h3("Weekly cardiovascular A&E attendances"),
        actionButton("btn_cardio_modal", "Data source: A&E", icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "ae_cardio_overall"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances compared with the corresponding
                     time in 2018-2019 by age group", "ae_cardio_age_var",
                     "Weekly number of cardiovascular A&E attendances by age group", "ae_cardio_age_tot"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances compared with the corresponding
                     time in 2018-2019 by SIMD quintile", "ae_cardio_dep_var",
                     "Weekly number of cardiovascular A&E attendances by SIMD quintile", "ae_cardio_dep_tot")
      )
    }
})

###############################################.
## Charts ----
###############################################.
# Cath labs Golden Jubilee charts
output$cath_gj_overall <- renderPlotly({plot_overall_chart(cath_lab, "cath", area = F)})
output$cath_adm_gj_var <- renderPlotly({plot_trend_chart(cath_lab, pal_depr)})
output$cath_adm_gj_tot <- renderPlotly({plot_trend_chart(cath_lab, pal_sex, 
                                                         type = "total", data_name = "cath")})

# A&E Cardio charts
output$ae_cardio_overall <- renderPlotly({plot_overall_chart(ae_cardio, data_name = "aye", area = "All")})
output$ae_cardio_age_var <- renderPlotly({
  # Due to different age groups (<65 & 65+) it is not possible to use 'plot_trend_chart' function
  # Hence the standalone chart
  
  # Filter for required data
  trend_data <- ae_cardio %>% 
    filter(type == "age") %>% 
    mutate(category = factor(category, levels = c("<65", "65+")))
  
  aver_period <- "2018-2019"
  
  #Text for tooltip
  tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                            "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                            "<br>", "Change from ", aver_period, " average: ", trend_data$variation, "%"))
  
  # Setting y-axis title
  yaxis_plots[["title"]] <- paste0("% change from ", aver_period, " average")
  
  # Creating time trend plot
  trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~variation) 
  
  trend_plot %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category, colors = c('#543005', '#8c510a'),
              text=tooltip_trend, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
})
output$ae_cardio_age_tot <- renderPlotly({
  # Due to different age groups (<65 & 65+) it is not possible to use 'plot_trend_chart' function
  # Hence the standalone chart
  
  # Filter for required data
  trend_data <- ae_cardio %>% 
    filter(type == "age") %>% 
    mutate(category = factor(category, levels = c("<65", "65+")))
  
  # Setting y-axis title
  yaxis_plots[["title"]] <- "Number of attendances"
  
  # Setting measure
  measure_name <- "Attendances: "
  
  #Text for tooltip
  tooltip_trend <- c(paste0(trend_data$category, "<br>",
                            "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                            "<br>", measure_name, trend_data$count,
                            "<br>", "Historic average: ", trend_data$count_average))
  
  # Creating time trend plot
  trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~count)
  
  trend_plot %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category, colors = c('#543005', '#8c510a'),
              text=tooltip_trend, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
})
output$ae_cardio_dep_var <- renderPlotly({plot_trend_chart(dataset = ae_cardio, pal_chose = pal_depr, split = "dep", type = "variation", data_name = "aye", tab = "cardio")})
output$ae_cardio_dep_tot <- renderPlotly({plot_trend_chart(ae_cardio, pal_depr, split = "dep", type = "total", data_name = "aye", tab = "cardio")})

###############################################.
## Data downloads ----
###############################################.

overall_cardio_download <- reactive({
  switch(
    input$measure_cardio_select,
    "cath" = filter_data(cath, area = F)
  ) %>% 
    select(week_ending, count, count_average, variation) %>% 
    rename(count_2019 = count_average) %>% 
    mutate(week_ending = format(week_ending, "%d %b %y"))
})

output$download_cardio_data <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    write_csv(overall_cardio_download(),
              file) } 
)


##END