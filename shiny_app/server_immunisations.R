##Server script for immunisations tab


###############################################.
## Immunisation Reactive controls  ----
###############################################.

# 'Immunisation' reactive drop-down control showing list of area names depending on areatype selected

output$geoname_ui_immun <- renderUI({

  #Lists areas available in   
  areas_summary_immun <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_immun])

  selectizeInput("geoname_immun", label = NULL,
                 choices = areas_summary_immun, selected = "")

})


###############################################.
## Immunisation Tab Reactive layout  ----
###############################################.


# The charts and text shown on the app will depend on what the user wants to see
output$immunisation_explorer <- renderUI({
  
  # text for titles of cut charts
   # dataset <- case_when(input$measure_select_immun == "sixin_8wks" ~ "immunisation",
   #                      input$measure_select_immun == "sixin_12wk" ~ "visits")
   # 
  
  # Charts and rest of UI
  if (input$measure_select_immun == "sixin_8wks") {
    tagList(
      fluidRow(column(12, p("Data Table"))),
      fluidRow(column(8, withSpinner(plotlyOutput("immun_scurve"))),
             column(4, p("Space for commentary here"))))
  }  else if (input$measure_select_immun == "sixin_12wks"){
    p("6 in 1 at 12 weeks coming 10th June 2020")
  }
})


output$immun_scurve <- renderPlotly({plot_scurve(six)})
