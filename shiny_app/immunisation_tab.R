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

plot_scurve <- function(dataset) {
  
  scurve_data <- dataset %>% filter(area_name == input$geoname_immun) %>%
    droplevels()
  
  #Create tooltip for scurve
  tooltip_scurve <- c(paste0("cohort", scurve_data$cohort_eligible_name))
  
  #Creating time trend plot
  s_plot <- plot_ly(data=scurve_data, x=~interv,  y = ~surv)
  
  #Creating time trend plot
  s_plot %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~cohort_eligible_name, colors = "BrBG",
              text= tooltip_scurve, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )}


# The charts and text shown on the app will depend on what the user wants to see
output$immunisation_explorer <- renderUI({
  
  # text for titles of cut charts
  dataset <- case_when(input$measure_select_immun == "sixin_8wks" ~ "immunisation",
                       input$measure_select_immun == "sixin_12wk" ~ "visits")
  
  
  
  
  # Charts and rest of UI
  if (input$measure_select_immun == "sixin_8wks") {
    fluidRow(column(8, withSpinner(plotlyOutput("immun_scurve"))),
             column(4, p("Space for commentary here")))
  }  else if (input$measure_select_immun == "sixin_12wks"){
    p("6 in 1 at 12 weeks coming 10th June 2020")
  }
})


output$immun_scurve <- renderPlotly({plot_scurve(six)})
