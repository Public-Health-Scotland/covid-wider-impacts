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
                   "Weekly number of cases by admission type", "cath_adm_gj_tot"),
      plot_cut_box("Angiographies/PCI carried out", "angio_gj_overall",
                   "Percentage of total for females and over 70 years old people", "angio_gj_perc")
    )
    } else if (input$measure_cardio_select == "rapid") {
      tagList(#Hospital admissions
      )
    } else if (input$measure_cardio_select == "aye") {
      tagList(# A&E attendances
      )
    }
})

###############################################.
## Charts ----
###############################################.
# Cath labs Golden Jubilee charts
output$cath_gj_overall <- renderPlotly({plot_overall_chart(cath_lab, "cath", area = F)})
output$cath_adm_gj_var <- renderPlotly({plot_trend_chart(cath_lab, pal_sex)})
output$cath_adm_gj_tot <- renderPlotly({plot_trend_chart(cath_lab, pal_sex, type = "total", data_name = "cath")})
output$angio_gj_overall <- renderPlotly({
  
  #Text for tooltip
  tooltip_trend <- c(paste0("Month: ", format(angio_lab$month_date, "%b %y"),
                            "<br>", "Angiographies: ", angio_lab$n))
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Number of angiographies"
  
  plot_ly(data=angio_lab, x=~month_date,  y = ~n) %>% 
    add_trace(type = 'scatter', mode = 'lines', line = list(color = "black"),
              text=tooltip_trend, hoverinfo="text") %>% 
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots) %>% 
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  
})

output$angio_gj_perc <- renderPlotly({
  
  #Text for tooltip
  tooltip_fem <- c(paste0("Month: ", format(angio_lab$month_date, "%b %y"),
                          "<br>", "Angiographies: ", angio_lab$n_female,
                            "<br>", "Percentage: ", angio_lab$percent_female))
  
  tooltip_70 <- c(paste0("Month: ", format(angio_lab$month_date, "%b %y"),
                          "<br>", "Angiographies: ", angio_lab$n70,
                          "<br>", "Percentage: ", angio_lab$percent_70))
  
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Percentage over the total"
  
  plot_ly(data=angio_lab, x=~month_date) %>% 
    add_trace(y= ~percent_female, type = 'scatter', mode = 'lines', line = list(color = "blue"),
              text=tooltip_fem, hoverinfo="text", name = "% Females") %>% 
    add_trace(y= ~percent_70, type = 'scatter', mode = 'lines', line = list(color = "brown"),
              text=tooltip_70, hoverinfo="text", name = "% aged 70+") %>% 
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  
})

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