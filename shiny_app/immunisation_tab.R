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


# Reactive dataset filtered for flextable - four possible combinations of data
table_data <- reactive({  
  table <- sixtable %>%
    filter(geography_name=="Scotland") %>%
    mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
    arrange(cohort)
})


immune_table <- function() {
  
  table_data() %>%
    select (time_period_eligible, denominator,uptake_12weeks_num,uptake_12weeks_percent,uptake_tot_num,uptake_tot_percent) %>%
    flextable() %>%
    set_header_labels(time_period_eligible="8 Weeks of age reached", denominator="Cohort",uptake_12weeks_num="Uptake before 12 weeks",uptake_12weeks_percent="Uptake before 12 weeks",uptake_tot_num="Total uptake",uptake_tot_percent="Total uptake") %>%
    merge_at(i = 1, j = 3:4, part = "header") %>%
    merge_at(i = 1, j = 5:6, part = "header") %>%
    add_header_row(values=c("","","N","%","N","%"), top = FALSE ) %>%
    theme_box() %>%
    autofit() %>%
    htmltools_value()
}



###############################################.
## Immunisation Tab Reactive layout  ----
###############################################.

# The charts and text shown on the app will depend on what the user wants to see
output$immunisation_explorer <- renderUI({

  # text for titles of cut charts
  immune_title <- paste0(case_when(input$measure_select_immun == "sixin_8wks" ~ "Uptake of first dose 6-in-1 vaccine (routinely scheduled at 8 weeks of age)",
                            input$measure_select_immun == "sixin_12wks" ~ "Uptake of second dose 6-in-1 vaccine (routinely scheduled at 12 weeks of age)",
                            input$measure_select_immun == "sixin_16wks" ~ "Uptake of third dose 6-in-1 vaccine (routinely scheduled at 16 weeks of age)"))
  
  immune_subtitle <- paste0(input$geoname_immun)
  
  # Charts and rest of UI
  if (input$measure_select_immun == "sixin_8wks") {
    tagList(
            fluidRow(column(10, h4(paste0(immune_title)), p(immune_subtitle)),
               column(8, withSpinner(plotlyOutput("immun_scurve"))),
               column(4, p("Space for commentary here"))),
            fluidRow(column(10,renderUI(immune_table())))
    )
  }  else if (input$measure_select_immun == "sixin_12wks"){
    p("6-in-1 at 12 weeks coming 10th June 2020")
  }  else if (input$measure_select_immun == "sixin_16wks"){
  p("6-in-1 at 16 weeks coming 10th June 2020")
}
})


output$immun_scurve <- renderPlotly({plot_scurve(six)})
