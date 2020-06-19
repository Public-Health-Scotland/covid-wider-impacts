##Server script for child health tab


# Pop-up modal explaining source of data
observeEvent(input$btn_child_modal, 
                 showModal(modalDialog(#CHILD HEALTH MODAL
                 title = "What is the data source?",
                 p("The information shown on the numbers of children eligible for routine preschool reviews is taken from the",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12", 
                          "Scottish Immunisation and Recall System (SIRS)", class="externallink"),
                    ". The information recorded at each review is taken from the",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=10",
                          "Child Health Systems Programme-PreSchool (CHSP-PS)", class="externallink"),
                    "."),
                 p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation of children when a scheduled vaccination is due."),
                 p("CHSP-PS is an electronic system used by all NHS Boards in Scotland. The CHSP Pre-School system supports the delivery of the child health programme by facilitating the automated call and recall of children for the agreed schedule of child health reviews for pre-school children. Child health reviews incorporate assessment of children's health, development, and wider wellbeing alongside provision of health promotion advice and parenting support."),
                 p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink")," routinely receives quarterly data extracts from SIRS and CHSP-PS for the purpose of producing and ",
                   (tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/child-health-pre-school-review-coverage/","publishing",class="externallink"))," coverage rates for child health reviews. To allow more rapid monitoring of the impact of Covid-19 on child health review coverage rates, PHS is also currently extracting a sub-set of data from SIRS & CHSP-PS each month."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))
    

###############################################.
## Child Health Reactive controls  ----
###############################################.

# Child Health reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_child <- renderUI({
  
  #Lists areas available in   
  areas_summary_child <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_child])
  
  selectizeInput("geoname_child", label = NULL, choices = areas_summary_child, selected = "")
})


# Reactive dataset for flextable filter on geographical area
filter_table_data_child <- function(dataset){
  dataset %>% filter(area_name == input$geoname_child)
}

###############################################.
## Child Health Tab Reactive layout  ----
###############################################.

#run chart function to generate s curve  
output$child_first_scurve <- renderPlotly({plot_scurve_child(first, 2)})
output$child_first_table <- renderUI({child_table(firsttable, 2, 12)})

output$child_sixtoeight_scurve <- renderPlotly({plot_scurve_child(sixtoeight, 8)})
output$child_sixtoeight_table <- renderUI({child_table(sixtoeighttable, 8, 16)})


# The charts and text shown on the app will depend on what the user wants to see
output$child_health_explorer <- renderUI({

  # text for titles of cut charts
  child_title <- paste0(case_when(input$measure_select_child == "first_visit" ~ paste0("Coverage of health visitor first visit (offered to children at 2 weeks of age): ",
                                                                                             input$geoname_child),
                            input$measure_select_child == "six_eightwks" ~ paste0("Coverage of 6-8 week reviews: ", input$geoname_child),
                            input$measure_select_child == "13_15mnth" ~ paste0("Coverage of 13-15 month reviews (offered to children at 13-15 months of age): ", input$geoname_child),
                            input$measure_select_child == "27_30mnth" ~ paste0("Coverage of 27-30 month reviews (offered to children at 27-30 months of age): ", input$geoname_child)))
  
  #commentary to appear in child health tab
  commentary_first <-p("All preschool children should be offered the following health reviews: health visitor first visit, 6-8 week review, 13-15 month review, 27-30 month review, and 4-5 year review. Although the 4-5 year review only became mandated by government policy for children turning 4 from April 2020 onwards.", br(),
"The charts show the progression of coverage of the relevant review as children age. The data tables provide the coverage rates at three specific time-points. Data is shown for children who have become eligible for review during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for review before the pandemic (in 2019 and in January and February 2020) for comparison.", br(), 
"After a child receives a review, it takes time for a record of the review to be entered into the CHSP-PS system. We have allowed a 6 week window for data entry. Each release of this page will therefore report on reviews provided up to 6 weeks previously. Although the vast majority of data on reviews given will be recorded within 6 weeks, data shown for the most recent cohorts of children will not be fully complete in CHSP-PS at this stage.", br(), 
"Data is shown for Scotland and for NHS Board areas. Due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland, and NHS Western Isles are not shown separately, however the Island Boards are included within the Scotland total. NHS Grampian has had difficulty recording reviews given on the CHSP-PS system since the start of the Covid-19 pandemic. Information on children in Grampian has therefore been excluded, and Grampian is not included in the Scotland totals. We hope to include NHS Grampian in future releases once local data recording difficulties are resolved.", br(),
"Coverage rates based on small numbers are prone to fluctuation. Therefore, in Boards with small numbers of children eligible for review each week, particularly NHS Borders and NHS Dumfries & Galloway, it is important to consider this when interpreting the rates.
")
  
  # Specify items to display in child health ui based on step 2 selection 
  if (input$measure_select_child == "first_visit") {
    tagList(
      fluidRow(column(10, h4(paste0(child_title)))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_first_scurve"))),
               column(6, uiOutput("child_first_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  }  else if (input$measure_select_child == "six_eightwks"){
    tagList(
      fluidRow(column(10, h4(paste0(child_title)))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_sixtoeight_scurve"))),
               column(6, uiOutput("child_sixtoeight_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else if (input$measure_select_child == "13_15mnth") {
    p("13-15 Month Review coming 8th July 2020")
  } else {
    p("27-30 Month Review coming 8th July 2020")
  }
  
}) #close child_health_explorer function


###############################################.
## Child Health Commentary tab content  ----
###############################################.


 output$child_comments <- renderUI({
     tagList(h2("Child Health reviews - 10th June 2020"), 
   p("Information on the uptake of child health reviews that are routinely offered to all preschool children by Health Visitors has been included in this tool for the first time on 10 June 2020.", br(),
      "Child health reviews incorporate assessment of children's health, development, and wider wellbeing alongside provision of health promotion advice and parenting support.  Routine child health reviews help ensure that children’s health and development is progressing as expected for their age and stage, and allow any concerns to be addressed.  It is important that children continue to receive their routine health reviews during the Covid-19 pandemic.", br(),
      "On 10 June 2020, information has been provided on the coverage of the Health Visitor first visit, which is offered to children at 10-14 days of age. Children receive subsequent Health Visitor reviews at 6-8 weeks, 13-15 months, 27-30 month, and 4-5 years of age. ", br(),
      "Coverage rates for the Health Visitor first visit have remained high during the pandemic. Coverage continues to exceed 90% among children who were due their review in March and early April. The recording of data on reviews undertaken by the reporting date will not be fully complete at this stage, particularly for the most recent cohorts, so coverage rates are slightly under-reported."
      )
     )
 })

###############################################.
## Data downloads ----
###############################################.

# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
visit_data_download <- reactive({
  switch(
    input$measure_select_child,
    "first_visit" = filter(firsttable, area_name == input$geoname_child),
    "six_eightwks" = filter(sixtoeighttable, area_name == input$geoname_child),
    "13_15mnth" = filter(firsttable, area_name == input$geoname_child),
    "27_30mnth" = filter(firsttable, area_name == input$geoname_child)
  ) %>% 
    select(area_name, time_period_eligible, denominator, starts_with("coverage")) %>% 
    rename(cohort = time_period_eligible)
})

output$download_visit_data <- downloadHandler(
  filename ="child_visits_extract.csv",
  content = function(file) {
    write_csv(visit_data_download(),
              file) } 
)

#END

