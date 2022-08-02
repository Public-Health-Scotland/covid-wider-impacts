##Server script for child health tab..


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
## Child Health Commentary tab content  ----
###############################################.
# This also includes breastfeeding and child development data
 output$child_comments <- renderUI({
     tagList(
       bsButton("jump_to_childreview",label = "Go to data"),
       h2("Child Health reviews - 1st September 2021"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 1 September, and includes information on children becoming eligible for review up to June 2021. Background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       p("Please note that going forward the dashboard will continue to be updated on the first Wednesday of each month, but the commentary will only be updated in the case of exceptions."),
       h2("Child Health reviews - 4th August 2021"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 4 August, and includes information on children becoming eligible for review up to May 2021. Background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 7th July 2021"),
       h4("What is reported?"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 7 July. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to April 2021) as well as before the pandemic (2019, January 2020, and February 2020). The month and year time periods for which data is shown in the chart and table is now selectable using “Step 3. Select time periods of interest.” The data downloads include more detailed information, including by Health and Social Care Partnership, and weekly cohorts (note that due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are presented for monthly and yearly cohorts only)."),
       h4("Findings"),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews was lower for children who became eligible in the early months of the pandemic, than in 2019. However, data from summer 2020 onwards shows that coverage appears to be recovering, with reviews happening in a more timely manner."),
       p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through PHS ",
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 2nd June 2021"),
       h4("What is reported?"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 2 June. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to March 2021) as well as before the pandemic (2019, January 2020, and February 2020). The month and year time periods for which data is shown in the chart and table is now selectable using “Step 3. Select time periods of interest.” The data downloads include more detailed information, including by Health and Social Care Partnership, and weekly cohorts (note that due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are presented for monthly and yearly cohorts only)."),
       h4("Findings"),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews was lower for children who became eligible in the early months of the pandemic, than in 2019. However, data from summer 2020 onwards shows that coverage appears to be recovering, with reviews happening in a more timely manner."),
       p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through PHS ",
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 5th May 2021"),
       h4("What is reported?"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 5 May. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to February 2021) as well as before the pandemic (2019, January 2020, and February 2020). Due to the volume of data available, the charts and table now show annual data for children who became eligible for review in 2019 and 2020, and monthly data for children who became eligible for review in the most recent 6 months for which data are available. The data downloads include more detailed information, including by Health and Social Care Partnership, and weekly cohorts (note that due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are presented for monthly and yearly cohorts only)."),
       h4("Findings"),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews was lower for children who became eligible in the early months of the pandemic, than in 2019. However, data from summer 2020 onwards shows that coverage appears to be recovering, with reviews happening in a more timely manner."),
       p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through PHS ",
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 7th April 2021"),
       h4("What is reported?"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 7 April. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to January 2021) as well as before the pandemic (2019, January 2020, and February 2020). Due to the volume of data available, the charts and table now show annual data for children who became eligible for review in 2019 and 2020, and monthly data for children who became eligible for review in the most recent 6 months for which data are available. The data downloads include more detailed information, including by Health and Social Care Partnership, and weekly cohorts (note that due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are presented for monthly and yearly cohorts only)."),
       p("Data for a small number children are not included in the eligible cohort and coverage figures due to an issue in the source data. The impact on the reported rates at Scotland level will be minor."),
       h4("Findings"),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews was lower for children who became eligible in the early months of the pandemic, than in 2019. However, data from summer 2020 onwards shows that coverage appears to be recovering, with reviews happening in a more timely manner."),
       p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through PHS ",
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 3rd March 2021"),
       h4("What is reported?"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 3 March. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to December 2020) as well as before the pandemic (2019, January 2020, and February 2020). Weekly data are no longer shown in the charts and tables but are available through the data download, and includes data for children eligible up to week beginning 4 January 2021. It should be noted that the coverage data recorded for the most recent eligible cohorts will not be fully complete at this stage. Data for a few children are not included in the eligible cohort and coverage figures due to an issue in the source data. The impact on the reported rates at Scotland level will be minor."),
       h4("Findings"),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews was lower for children who became eligible in March and April 2020, than in 2019. There is some evidence of ‘catch-up’, with coverage for March and April improving with time, however coverage still lags behind 2019 levels."),
       p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through PHS ",
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 3rd February 2021"),
       h4("What is reported?"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 3 February. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to November 2020) as well as before the pandemic (2019, January 2020, and February 2020). Weekly data are no longer shown in the charts and tables but are available through the data download, and includes data for children eligible up to week beginning 7 December 2020. It should be noted that the coverage data recorded for the most recent eligible cohorts will not be fully complete at this stage."),
       h4("Data quality"),
       p("The data issue affecting the figures in the previous release appears to be resolved, with further checks on the data ongoing (see commentary for 23 December 2020 below)."),
       h4("Findings"),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible throughout the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of the 6-8 week review (at 10 weeks), was 6 percentage points lower in March 2020 than in 2019, but in subsequent months coverage has been similar to the previous year. For the older age group reviews, an impact on coverage is apparent for children who became eligible in March and April 2020. There is some evidence of ‘catch-up’, with coverage for these cohorts improving with time, however coverage to date still lags behind 2019 levels for these months. For children becoming eligible from May to September 2020, coverage to date for the 13-15 month review is slightly higher than the 2019 level, and for the 27-30 month review is slightly lower. Although, for the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through PHS",
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 23rd December 2020"),
       h4("What is reported?"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 23 December. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to September 2020) as well as before the pandemic (2019, January 2020, and February 2020). Weekly data are no longer shown in the charts and tables but are available through the data download, and includes data for children eligible up to week beginning 19 October 2020. It should be noted that the coverage data recorded for the most recent eligible cohorts will not be fully complete at this stage."),
       h4("Data quality"),
       p("In this update some coverage rates are slightly under-reported; this is due to an issue this month with the source data which has affected the accuracy of the eligible cohort data for all time-periods. At Scotland level the monthly and weekly coverage rates are thought to be under-reported by -0.1 to -1.0 %. The impact on some NHS Board and Health & Social Care Partnership rates will be greater. It is anticipated the data issue will have been corrected in the next planned update of the dashboard on 3 February 2020."),
       p("It should also be noted that NHS Greater Glasgow & Clyde have a backlog of 13-15 month, 27-30 month, and 4-5 year reviews to be entered into CHSP due to staffing shortages. Entry of the first visit and 6-8 week review data has been prioritised, so these are up-to-date."),
       h4("Findings"),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews was lower for children who became eligible in March and April 2020, than in 2019. There is some evidence of ‘catch-up’, with coverage for March and April improving with time, however coverage still lags behind 2019 levels."),
       p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through PHS ",
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 4th November 2020"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 4 November. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to August 2020) as well as before the pandemic (2019, January 2020, and February 2020). Information has now been added at Health & Social Care Partnership level and this available through the data download function. Weekly data are no longer shown in the charts but are available through the data download."),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews was lower for children who became eligible in March and April 2020, than in 2019. There is some evidence of ‘catch-up’, with coverage for March and April improving with time, however coverage still lags behind 2019 levels. There is some evidence that coverage has been lower in June and July, which may be attributable to services focusing on delivery of ‘catch-up’ reviews for children who became eligible earlier in the year."),
       p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through PHS ",   
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 7th October 2020"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 7 October. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to early August 2020) as well as before the pandemic (2019, January 2020, and February 2020)."),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews was lower for children who became eligible in March and April 2020, than in 2019. Recent data show that rates are recovering in the majority of NHS Boards, with coverage for the 6-8 week and 13-15 month reviews returning to pre-pandemic levels by May 2020. For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. There is some evidence of ‘catch-up’, with coverage for March and April improving with time, and in some boards coverage in May and June exceeds pre-pandemic levels. Information on final achieved coverage will continue to be provided through PHS ",
         tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 2nd September 2020"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 2 September. Information is provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to early July 2020) as well as before the pandemic (2019, January 2020, and February 2020)."),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all other reviews had fallen for children eligible since March 2020. Recent data show that rates are beginning to recover in most, but not all, NHS Boards. There is some evidence of ‘catch-up’, with coverage for March and April improving with time, but this has still not reached the levels achieved in 2019. For the later child health reviews, which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year review, it will take some time for final achieved coverage to be known. Information on final achieved coverage will continue to be provided through official statistics publications."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 12th August 2020"),
       p("Information on uptake of pre-school child health reviews was updated in this tool on 12 August. Information is now provided on children becoming eligible for a review during the Covid-19 pandemic (in March 2020 to early June 2020) as well as before the pandemic (2019, January 2020, and February 2020)."),
       p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. 
Coverage of all other review had fallen for children eligible in March 2020, but recent data for April and May show that rates are beginning to recover in most, but not all, NHS Boards. For the later child health reviews, which have a much longer timeframe for reviews to be delivered, it will take some time for final achieved coverage to be known.  Information on final achieved coverage will continue to be provided through ",
tags$a(href = "https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/", "official statistics publications",  target="_blank"), "."),
       p("Further background information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
       h2("Child Health reviews - 15th July 2020"), 
   p("Information on the uptake of child health reviews that are routinely offered to all preschool children by Health Visitors was included in this tool for the first time on 10 June 2020. Data was subsequently refreshed on 8 July 2020.  Commentary relating to those releases is provided below.", br(), br(),
      "Information on coverage of the 4-5 year review was included for the first time on 15 July 2020. Data from before the pandemic, for children becoming eligible in 2019, show that coverage of the 4-5 year review by 49 months was 11%, rising to 29% by 52 months. Coverage continues to increase as children age beyond this point. Overall coverage for children eligible in 2019 was 52% by the time data was extracted for analysis (22 June 2020). This is a fairly new review, which has actually not been implemented in all board areas yet (no data is shown for NHS Dumfries & Galloway as they implemented the review in May 2020, and NHS Highland are scheduled to implement the review on 3 August 2020) and Government policy states that this review should be offered to all children turning 4 years old from April 2020 onwards. Therefore, we expect the baseline for 2019 to be low for this review as it is still becoming established. However, data for children eligible in January and February 2020 show that coverage was gradually beginning to rise before it fell in March 2020, and weekly data for April shows coverage by 49 months is between 5-9%. These children have not yet reached 52 months of age, and we would expect coverage to increase over time.", br(), br(),
      "In general, the impact of the Covid-19 pandemic on early coverage of the 4-5 year review has been very variable between Boards. Coverage has been well maintained in some areas, but is very low for children becoming eligible for review during the pandemic in other areas.  In areas showing low coverage, this may be due to Health Visitors prioritising the earlier reviews (first visit and 6-8 weeks). In addition, ", 
     tags$a(href="https://www.gov.scot/publications/coronavirus-covid-19-nursing-and-community-health-staff-guidance/",
            "national guidance (external website)", target="_blank"), 
     " during the pandemic has recommended that the earlier reviews (first visit, 6-8w) should continue as face to face contacts whereas the later reviews (13-15m, 27-30m, 4-5 yr) should also continue, but be provided via NHS near-me (a secure video conferencing facility) or telephone where possible. In some NHS Board areas this remote delivery of reviews has not been possible, and this is reflected in the data."
      ),
   h2("Child Health reviews - 8th July 2020"), 
   p("Information on the uptake of child health reviews that are routinely offered to all preschool children by Health Visitors was included in this tool for the first time on 10 June 2020. Child health reviews incorporate assessment of children's health, development, and wider wellbeing alongside provision of health promotion advice and parenting support. Routine child health reviews help ensure that children’s health and development is progressing as expected for their age and stage, and allow any concerns to be addressed. It is important that children continue to receive their routine health reviews during the Covid-19 pandemic. On 10 June 2020, information was provided on the coverage of the Health Visitor first visit, which is offered to children at 10-14 days of age. Coverage of the 6-8 week review was added on 24 June, and coverage of the 13-15 month and 27-30 month review were added on 8 July (and data for the first visit and 6-8 week review were also updated).", br(), br(),
     "Coverage rates for the Health Visitor first visit have remained high during the pandemic. Coverage continues to exceed 90% among children who were due their review in March and April.", br(), br(),
     "Data from before the pandemic, for children becoming eligible in 2019, show that coverage of the 6-8 week review by 10 weeks was 83%. Coverage has gradually fallen since the beginning of 2020, and was around 69-77% for children becoming eligible for the review in March and April.", br(), br(),
     "There are a number of important factors to take into account when interpreting this information. Unlike all the other pre-school child health reviews, the 6-8 week review is a two stage process involving the baby’s Health Visitor and their GP. Usually, the Health Visitor first visits the family at home to conduct a general assessment of the baby’s progress and the family’s wellbeing. Then, the GP offers an appointment in the practice to conduct a detailed physical examination of the baby. Usually the GP appointment happens shortly after the Health Visitor visit, and the data from both assessments is then returned together to the NHS Board child health administrative department for entry into the CHSP-PS system. Since the start of the Covid-19 pandemic, Scottish Government policy has been that the 6-8 week review should continue to be provided. In practice, to minimise the number of times babies are brought into the practice, in some areas the GP element of the review may have been deferred, for example until the baby is due a routine immunisation appointment at a later stage. Areas may then vary in terms of whether Health Visitors return information on their part of the 6-8 week review for entry into the baby’s CHSP-PS record, or whether no information is returned until the GP part of the review is completed. As GPs start to ‘catch up’ with their part of outstanding 6-8 week reviews, we would expect to see coverage rates for children becoming eligible for this review during the pandemic increasing. It is important to note therefore that no record of a 6-8 week review on the CHSP-PS system does not necessarily mean that the baby has not been seen at all: they may have been visited by their Health Visitor, but not yet examined by their GP.", br(), br(),      
     "For babies born prematurely, the 6-8 week review is offered to children 6-8 weeks following their expected date of delivery rather than their actual date of birth. This is to ensure a ‘fair’ assessment of children’s progress against what would be expected of a baby at that stage. As the information shown in the dashboard is based on children’s actual date of birth rather than due dates, premature babies will appear to have their review ‘late’, when in fact it was offered appropriately. This partially accounts for why coverage of the 6-8 week review continues to increase as babies attain older ages. Finally, it should also be restated that we have allowed a 6 week window for review completion and data entry, that is we have reported on reviews provided to children becoming eligible for their reviews up to 6 weeks prior to the date we extracted data from the CHSP-PS system. The results of a completed review would generally be expected to be entered into the CHSP-PS system within this 6 week time frame. In practice however occasional data entry delays occur. These may be worse during the Covid-19 pandemic, and may vary between areas. For all these reasons, review coverage for the most recent cohorts should therefore be taken as provisional, and is likely to increase over time as relevant data accumulates.", br(), br(),
     "Data for the 13-15 month review show that 41% of children becoming eligible for review in 2019 had received their review by 14 months, rising to 81% by 17 months. Coverage continues to increase as children age beyond this point.  Overall coverage for children eligible in 2019 was 84% by the time data was extracted for analysis (22 June 2020).
        Early coverage of the 13-15 month review was noticeably lower for children becoming eligible during the Covid-19 pandemic. Among children eligible in March and April 2020, 26-35% had received their review by 14 months. These children have not yet reached 17 months of age, and we would expect coverage to increase over time.", br(), br(),
     "Data for the 27-30 month review show that 33% of children becoming eligible for review in 2019 had received their review by 28 months, rising to 81% by 31 months. Coverage continues to increase as children age beyond this point. Overall coverage for children eligible in 2019 was 90% by the time data was extracted for analysis (22 June 2020).
        Early coverage of the 27-30 month review was noticeably lower for children becoming eligible during the Covid-19 pandemic. Among children eligible in March and April 2020, 20-26% had received their review by 28 months. These children have not yet reached 31 months of age, and we would expect coverage to increase over time. It should be noted that NHS Greater Glasgow & Clyde have a different policy for calling children for this review: they call at 30 months rather than 27 months as in the rest of Scotland. Hence, coverage by 28 months for children in NHS Greater Glasgow & Clyde is very low: this also affects the overall figures for Scotland as NHS GG&C is a large Board.", br(), br(),
     "In general, the impact of the Covid-19 pandemic on early coverage of the 13-15 month and 27-30 month reviews has been very variable between Boards. Coverage has been well maintained in some areas, but is very low for children becoming eligible for review during the pandemic in other areas.  In areas showing low coverage, this may be due to Health Visitors prioritising the earlier reviews (first visit and 6-8 weeks). In addition, ", tags$a(href="https://www.gov.scot/publications/coronavirus-covid-19-nursing-and-community-health-staff-guidance/","national guidance (external website)", target="_blank"), " during the pandemic has recommended that the earlier reviews (first visit, 6-8w) should continue as face to face contacts whereas the later reviews (13-15m, 27-30m) should also continue, but be provided via NHS near-me (a secure video conferencing facility) or telephone where possible. In some NHS Board areas this remote delivery of reviews has not been possible, and this is reflected in the data."
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

