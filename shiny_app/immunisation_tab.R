##Server script for immunisations tab

observeEvent(input$btn_immune_modal, 
                 showModal(modalDialog(#RAPID ADMISSIONS MODAL
                 title = "What is the data source?",
                 p("These data show uptake rates of the first dose of the 6-in-1 vaccine, which children should receive at 8 weeks old. The vaccine protects against diphtheria, tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and Hepatitis B. Children should also receive a second dose of the vaccine at 12 weeks and a third dose at 16 weeks."),
                 p("Data are provided for cohorts of children reaching 8 weeks of age in the recent past as well as historical information (the baseline) for comparison purposes."),
                 p("The chart shows the progression of uptake of the first dose of vaccine as children age. The data table provides the uptake rates at two time-points in the chart: the uptake rate reached by 12 weeks old and the overall uptake rate recorded by the time of reporting."),
                 p("There is a reporting-lag of 6 weeks to allow time for the recording of data on vaccinations given. Although the vast majority of data on vaccinations given will be recorded within 6 weeks, data shown for the most recent cohorts of children reaching 8 weeks old will not be fully complete at this stage. "),
                 p("The analyses are derived from data extracted from the  ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12",
                          "Scottish Immunisation and Recall System (SIRS)",class="externallink"),". This is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation of children when a scheduled immunisation is due and allows recording of immunisation data. After an immunisation contact has taken place, the immunisation details are keyed into the system by administrative staff in NHS Boards."),
                 p(tags$a(href="https://www.isdscotland.org/Health-Topics/Child-Health/Immunisation/",
                          "Public Health Scotland (PHS)",class="externallink"),"routinely receive quarterly data extracts from SIRS for the purpose of producing and publishing immunisation uptake rates. For the immediate monitoring of the impact of COVID-19 on childhood immunisation uptake rates, a sub-set of data from SIRS will be extracted each month."),
                 size = "l",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))
    

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
    filter(area_name==input$geoname_immun) %>%
    mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
    arrange(cohort)
})


immune_table <- function() {
    table_data() %>%
    select (time_period_eligible, denominator,uptake_12weeks_num,uptake_12weeks_percent,uptake_tot_num,uptake_tot_percent) %>%
    flextable() %>%
    set_header_labels(time_period_eligible="8 weeks of age reached", denominator="Children",uptake_12weeks_num="Uptake before 12 weeks of age",uptake_12weeks_percent="Uptake before 12 weeks of age",uptake_tot_num="Total uptake recorded by 18 May",uptake_tot_percent="Total uptake recorded by 18 May") %>%
    merge_at(i = 1, j = 3:4, part = "header") %>%
    merge_at(i = 1, j = 5:6, part = "header") %>%
    add_header_row(values=c("","","N","%","N","%"), top = FALSE ) %>%
    font(fontname="Helvetica", part = "all") %>%
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
  immune_chart_title <- paste0(case_when(input$measure_select_immun == "sixin_8wks" ~ paste0("Uptake of first dose 6-in-1 vaccine (routinely scheduled at 8 weeks of age): ",
                                                                                             input$geoname_immun),
                            input$measure_select_immun == "sixin_12wks" ~ paste0("Uptake of second dose 6-in-1 vaccine (routinely scheduled at 12 weeks of age): ", input$geoname_immun),
                            input$measure_select_immun == "sixin_16wks" ~ paste0("Uptake of third dose 6-in-1 vaccine (routinely scheduled at 16 weeks of age): ", input$geoname_immun)))
  
  #immune_subtitle <- paste0(input$geoname_immun)
  
  immune_extract_date <- "Uptake rates are based on data on vaccinations given recorded by 18 May 2020; data for the most recent cohorts of children eligible for immunisation will not be fully complete at this stage." 
  
  # text for titles of cut tables
  immune_table_title <- paste0(case_when(input$measure_select_immun == "sixin_8wks" ~ "Uptake of first dose 6-in-1 vaccine (routinely scheduled at 8 weeks of age)",
                                   input$measure_select_immun == "sixin_12wks" ~ "Uptake of second dose 6-in-1 vaccine (routinely scheduled at 12 weeks of age)",
                                   input$measure_select_immun == "sixin_16wks" ~ "Uptake of third dose 6-in-1 vaccine (routinely scheduled at 16 weeks of age)"))
  
  
  # Charts and rest of UI
#   if (input$measure_select_immun == "sixin_8wks") {
#     tagList(
#             actionButton("btn_immune_modal", "Data source: PHS SIRS", icon = icon('question-circle')),
#             fluidRow(column(10, h4(paste0(immune_chart_title)), p(immune_extract_date))),
#             fluidRow(column(8, withSpinner(plotlyOutput("immun_scurve"))),
#                column(4, p("Space for commentary here"))),
#             fluidRow(column(10, h4(paste0(immune_table_title)), p(immune_extract_date)),
#                     column(6,renderUI(immune_table())))
#     )
#   }  else if (input$measure_select_immun == "sixin_12wks"){
#     p("6-in-1 at 12 weeks coming 10th June 2020")
#   }  else if (input$measure_select_immun == "sixin_16wks"){
#   p("6-in-1 at 16 weeks coming 10th June 2020")
# }
# })

  if (input$measure_select_immun == "sixin_8wks") {
    tagList(
      #actionButton("btn_immune_modal", "Data source: PHS SIRS", icon = icon('question-circle')),
      fluidRow(column(10, h4(paste0(immune_chart_title)), p(immune_extract_date))),
      fluidRow(column(7, withSpinner(plotlyOutput("immun_scurve"))),
               column(5,renderUI(immune_table()))),
      fluidRow(column(12, p("Space for commentary here")))
    )
  }  else if (input$measure_select_immun == "sixin_12wks"){
    p("6-in-1 at 12 weeks coming 10th June 2020")
  }  else if (input$measure_select_immun == "sixin_16wks"){
    p("6-in-1 at 16 weeks coming 17th June 2020")
  }
})
  
  
  
  
output$immun_scurve <- renderPlotly({plot_scurve(six)})
