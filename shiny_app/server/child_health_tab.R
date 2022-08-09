# Wider impacts dashboard - Child health tab - Child health reviews section
# Server code


# Pop-up modal explaining source of data
observeEvent(input$`childr-source-modal`, 
                 showModal(modalDialog(#CHILD HEALTH MODAL
                 title = "What is the data source?",
                 p("The information shown on the numbers of children eligible for routine preschool reviews is taken from the",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12", 
                          "Scottish Immunisation and Recall System (SIRS) (external website)",  target="_blank"),
                    ". The information recorded at each review is taken from the",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=10",
                          "Child Health Systems Programme-PreSchool (CHSP-PS) (external website)",  target="_blank"),
                    "."),
                 p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation of children when a scheduled vaccination is due."),
                 p("CHSP-PS is an electronic system used by all NHS Boards in Scotland. The CHSP Pre-School system supports the delivery of the child health programme by facilitating the automated call and recall of children for the agreed schedule of child health reviews for pre-school children. Child health reviews incorporate assessment of children's health, development, and wider wellbeing alongside provision of health promotion advice and parenting support."),
                 p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)", target="_blank")," routinely receives quarterly data extracts from SIRS and CHSP-PS for the purpose of producing and ",
                   (tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/child-health-pre-school-review-coverage/","publishing", target="_blank"))," coverage rates for child health reviews. To allow more rapid monitoring of the impact of Covid-19 on child health review coverage rates, PHS is also currently extracting a sub-set of data from SIRS & CHSP-PS each month."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))
    

###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("childr")

# Get list of available time periods for plotting
# Assumes that the time periods available are the same for all data
available_time_periods_child = 
  first %>%
  # using pull to get a vector rather than select because the selectizeInput didn't work otherwise
  pull(time_period_eligible) %>%
  unique()

# Set the default time periods for plotting
# Assumes that the months are listed in ascending order in first, followed by the years
default_time_periods_child = tail(available_time_periods_child, 8)

# Child reactive drop-down control showing list of time periods
output$dates_ui_child <- renderUI({
  selectizeInput("dates_child", label = NULL, choices = available_time_periods_child, 
                 selected = default_time_periods_child, multiple = TRUE,
                 options = list(placeholder = 'Select time periods',
                                plugins = c('remove_button')))
})

# Reactive dataset for flextable filter on geographical area
filter_table_data_child <- function(dataset){
  
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_child
  
  dataset %>% filter(area_name == input$`childr-geoname` &
                       # we don't want this function to re-execute every time dates_immun changes, so isolate()
                       time_period_eligible %in% isolate(input$dates_child))
}

###############################################.
## Child Health Tab Reactive layout  ----
###############################################.

#run chart function to generate s curve  
output$child_first_scurve <- renderPlotly({plot_scurve_child(first, "2 weeks")})
output$child_first_table <- renderUI({child_table(firsttable, "2 weeks", "18 weeks")})

output$child_sixtoeight_scurve <- renderPlotly({plot_scurve_child(sixtoeight, "6 weeks")})
output$child_sixtoeight_table <- renderUI({child_table(sixtoeighttable, "6 weeks", "22 weeks")})

output$child_thirteen_scurve <- renderPlotly({plot_scurve_child(thirteen, "13 months")})
output$child_thirteen_table <- renderUI({child_table(thirteentable, "13 months", "17 months")})

output$child_twentyseven_scurve <- renderPlotly({plot_scurve_child(twentyseven, "27 months")})
output$child_twentyseven_table <- renderUI({child_table(twentyseventable, "27 months", "31 months")})

output$child_fourtofive_scurve <- renderPlotly({plot_scurve_child(fourtofive, "4 years")})
output$child_fourtofive_table <- renderUI({child_table(fourtofivetable, "4 years", "52 months")})

# The charts and text shown on the app will depend on what the user wants to see
output$child_health_explorer <- renderUI({

  # text for titles of cut charts
  child_title <- paste0(case_when(input$`childr-measure` == "first_visit" ~ paste0("Coverage of health visitor first visit (offered to children at 2 weeks of age): ",
                                                                                             input$`childr-geoname`),
                            input$`childr-measure` == "six_eightwks" ~ paste0("Coverage of child health review offered at 6-8 weeks of age: ", input$`childr-geoname`),
                            input$`childr-measure` == "13_15mnth" ~ paste0("Coverage of child health review offered at 13-15 months of age: ", input$`childr-geoname`),
                            input$`childr-measure` == "27_30mnth" ~ paste0("Coverage of child health review offered at 27-30 months of age: ", input$`childr-geoname`),
                            input$`childr-measure` == "4_5yr" ~ paste0("Coverage of child health review offered at 4-5 years of age: ", input$`childr-geoname`)))
  child_subtitle <-  paste0("Figures based on data extracted from SIRS and CHSP-PS on ",child_extract_date)

  #commentary to appear in child health tab
  commentary_first <-
    if (input$`childr-measure` == "4_5yr") {
      p("All preschool children should be offered the following health reviews: health visitor first visit, 6-8 week review, 13-15 month review, 27-30 month review, and 4-5 year review. Although the 4-5 year review only became mandated by government policy for children turning 4 from April 2020 onwards.", br(),
        "The charts show the progression of coverage of the relevant review as children age. The data tables provide the coverage rates at three specific time-points. Data is shown for children who have become eligible for review during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for review before the pandemic (in 2019 and in January and February 2020) for comparison.", br(), 
        "After a child becomes eligible for a review, it takes time for them to attend their appointment, and for a record of the review provided to subsequently be entered into the CHSP-PS system. We have allowed a 6-week window for this, therefore each release of this page will report on children becoming eligible for a review up to 6 weeks before the date these data were extracted for analysis. Although children will generally have their review, and their CHSP-PS record updated accordingly, within 6 weeks of becoming eligible, the pandemic may have influenced not only how quickly eligible children receive their reviews, but also how long it takes for children’s CHSP-PS records to be updated once a review has been given. Any disruption to CHSP-PS data entry may vary across NHS Boards. Data shown for the most recent cohorts of children will therefore not be fully complete in CHSP-PS and should be viewed as provisional. The coverage rates for each cohort will be refreshed with more up-to-date data every 4 weeks, and rates for the most recent cohorts may increase slightly as relevant records are updated in CHSP-PS.", br(), 
        "Data is shown for Scotland and for all NHS Board areas. Data for Health & Social Care Partnerships is also available in the data download. Weekly data is no longer shown in the charts as these were becoming difficult to interpret due to the number of lines, however the weekly data is available in the data download for all areas except the island boards, which have been excluded due to small numbers.", br(),
        "Coverage rates based on small numbers are prone to fluctuation. Therefore, in Boards with small numbers of children eligible for review, it is important to consider this when interpreting the rates.
        ")
    } else { 
      p("All preschool children should be offered the following health reviews: health visitor first visit, 6-8 week review, 13-15 month review, 27-30 month review, and 4-5 year review. Although the 4-5 year review only became mandated by government policy for children turning 4 from April 2020 onwards.", br(),
        "The charts show the progression of coverage of the relevant review as children age. The data tables provide the coverage rates at three specific time-points. Data is shown for children who have become eligible for review during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for review before the pandemic (in 2019 and in January and February 2020) for comparison.", br(), 
        "After a child becomes eligible for a review, it takes time for them to attend their appointment, and for a record of the review provided to subsequently be entered into the CHSP-PS system. We have allowed a 6-week window for this, therefore each release of this page will report on children becoming eligible for a review up to 6 weeks before the date these data were extracted for analysis. Although children will generally have their review, and their CHSP-PS record updated accordingly, within 6 weeks of becoming eligible, the pandemic may have influenced not only how quickly eligible children receive their reviews, but also how long it takes for children’s CHSP-PS records to be updated once a review has been given. Any disruption to CHSP-PS data entry may vary across NHS Boards. Data shown for the most recent cohorts of children will therefore not be fully complete in CHSP-PS and should be viewed as provisional. The coverage rates for each cohort will be refreshed with more up-to-date data every 4 weeks, and rates for the most recent cohorts may increase slightly as relevant records are updated in CHSP-PS.", br(), 
        "Data is shown for Scotland and for all NHS Board areas. Data for Health & Social Care Partnerships is also available in the data download. Weekly data is no longer shown in the charts as these were becoming difficult to interpret due to the number of lines, however the weekly data is available in the data download for all areas except the island boards, which have been excluded due to small numbers.", br(),
        "Coverage rates based on small numbers are prone to fluctuation. Therefore, in Boards with small numbers of children eligible for review, it is important to consider this when interpreting the rates.
        ")
    }
  
  explorer_child <-     tagList(
    fluidRow(column(12, h4(paste0(child_title)),
                    p(child_subtitle))))
  
  # Specify items to display in child health ui based on step 2 selection 
  if (input$`childr-measure` == "first_visit") {
    tagList(explorer_child,
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_first_scurve"))),
               column(6, uiOutput("child_first_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  }  else if (input$`childr-measure` == "six_eightwks"){
    tagList(explorer_child,
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_sixtoeight_scurve"))),
               column(6, uiOutput("child_sixtoeight_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else if (input$`childr-measure` == "13_15mnth") {
    tagList(explorer_child,
      fluidRow(column(12, em("13 months defined as 57 weeks"))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_thirteen_scurve"))),
               column(6, uiOutput("child_thirteen_table"))),

      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else if (input$`childr-measure` == "27_30mnth") {
    tagList(explorer_child,
      fluidRow(column(12, em("27 months defined as 117 weeks"))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_twentyseven_scurve"))),
               column(6, uiOutput("child_twentyseven_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else {
    tagList(explorer_child,
            fluidRow(column(12, em("4 years defined as 209 weeks"))),
            fluidRow(column(6,br(), br(),
                            withSpinner(plotlyOutput("child_fourtofive_scurve"))),
                     column(6, uiOutput("child_fourtofive_table"))),
            fluidRow(column(12, renderUI(commentary_first)))
    )
  }
  
}) #close child_health_explorer function


###############################################.
## Data downloads ----
###############################################.

# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
visit_data_download <- reactive({
  switch(
    input$`childr-measure`,
    "first_visit" = firstdata,
    "six_eightwks" = sixtoeightdata,
    "13_15mnth" = thirteendata,
    "27_30mnth" = twentysevendata,
    "4_5yr" = fourtofivedata
  ) %>% 
    select(area_name, time_period_eligible, denominator, starts_with("coverage"), cohort) %>% 
    mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
    arrange(desc(cohort)) %>% 
    select(-cohort) %>% 
    rename(cohort = time_period_eligible) 
})

output$download_visit_data <- downloadHandler(
  filename ="child_visits_extract.csv",
  content = function(file) {
    write_csv(visit_data_download(),
              file) } 
)

#END

