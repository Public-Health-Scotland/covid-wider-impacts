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
        plot_box("2020 campred with 2018/19", "ae_cardio_overall")
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
output$ae_cardio_overall <- renderPlotly({plot_overall_chart(filter(ae_cardio, category == "all"), "aye")})

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