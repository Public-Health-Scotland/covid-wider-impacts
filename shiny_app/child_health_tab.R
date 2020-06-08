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
child_table_data <- reactive({
  firsttable %>%
    filter(area_name==input$geoname_child)
    #mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%  # required if table sort order is to change
    #arrange(cohort)
})


###############################################.
## Child Health Tab Reactive layout  ----
###############################################.

#run chart function to generate s curve  
output$child_first_scurve <- renderPlotly({plot_scurve_child(first)})
output$child_first_table <- renderUI({child_table()})


# The charts and text shown on the app will depend on what the user wants to see
output$child_health_explorer <- renderUI({

  # text for titles of cut charts
  child_title <- paste0(case_when(input$measure_select_child == "first_visit" ~ paste0("Coverage of health visitor first visit (offered to children at 2 weeks of age): ",
                                                                                             input$geoname_child),
                            input$measure_select_child == "six_eightwks" ~ paste0("Coverage of 6-8 week reviews (offered to children at 6-8 weeks of age): ", input$geoname_child),
                            input$measure_select_child == "13_15mnth" ~ paste0("Coverage of 13-15 month reviews (offered to children at 13-15 months of age): ", input$geoname_child)))
  
  #commentary to appear in child health tab
  commentary_first <-p("All preschool children should be offered the following health reviews: health visitor first visit, 6-8 week review, 13-15 month review, 27-30 month review, and 4-5 year review. Although the 4-5 year review only became mandated by government policy for children turning 4 from April 2020 onwards.
The charts show the progression of coverage of the relevant review as children age. The data tables provide the coverage rates at three specific time-points. Data is shown for children who have become eligible for review during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for review before the pandemic (in 2019 and in January and February 2020) for comparison. 
After a child receives a review, it takes time for a record of the review to be entered into the CHSP-PS system. We have allowed a 6 week window for data entry. Each release of this page will therefore report on reviews provided up to 6 weeks previously. Although the vast majority of data on reviews given will be recorded within 6 weeks, data shown for the most recent cohorts of children will not be fully complete in CHSP-PS at this stage. 
Data is shown for Scotland and for NHS Board areas. Due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland, and NHS Western Isles are not shown separately, however the Island Boards are included within the Scotland total. NHS Grampian has had difficulty recording reviews given on the CHSP-PS system since the start of the Covid-19 pandemic. Information on children in Grampian has therefore been excluded, and Grampian is not included in the Scotland totals. We hope to include NHS Grampian in future releases once local data recording difficulties are resolved.
Coverage rates based on small numbers are prone to fluctuation. Therefore, in Boards with small numbers of children eligible for review each week, particularly NHS Borders and NHS Dumfries & Galloway, it is important to consider this when interpreting the rates.
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
    p("6-8 Week Review coming 24th June 2020")
  } else if (input$measure_select_child == "13_15mnth") {
    p("13-15 Month Review coming 1st July 2020")
  }
  
}) #close child_health_explorer function


###############################################.
## Child Health Commentary tab content  ----
###############################################.


# output$child_comments <- renderUI({
#     
#   p("Information on the uptake of ",
#       tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","vaccinations that are routinely offered to all preschool children",class="externallink"),
#       " has been included in this tool for the first time on 3 June 2020.", br(),
#       "Vaccination protects children against many serious infectious diseases including diphtheria, whooping cough, and measles.",
#       tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening","Immunisation services throughout Scotland are continuing during the Covid-19 pandemic",class="externallink"),".",
#      "It is important to maintain the best possible vaccination uptake rates to ensure children remain protected and to prevent a resurgence of these infections.  Including information on childhood vaccination rates in this tool will help us to ensure that vaccination rates remain high throughout the pandemic.",
#     "On 3 June 2020, information has been provided on the uptake of the first dose of the 6-in-1 vaccine, which is offered to children at 8 weeks of age. The vaccine protects against diphtheria, tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and Hepatitis B. Children should also receive a second dose of the vaccine at 12 weeks and a third dose at 16 weeks.",br(),
#     "Uptake rates for this vaccination have remained high during the pandemic.  Uptake continues to exceed 90% among children who were due their first dose of the 6-in-1 vaccine in March and early April. The recording of data on vaccinations given by the reporting date will not be fully complete at this stage, particularly for the most recent cohorts, so uptake rates are slightly under-reported. In addition, some children will receive the vaccine at a later age, for example due to missed or rescheduled appointments, so uptake rates are expected to continue to increase as children age (as shown in the baseline)."
#     )
# })

# ###############################################.
# ## Data downloads ----
# ###############################################.
# 
# # For the charts at the moment the data download is for the overall one,
# # need to think how to allow downloading for each chart
# # Reactive dataset that gets the data the user is visualisaing ready to download
# overall_data_download_child <-
#   # reactive({
#   # switch(
#   #   input$measure_select,
#   #   "rapid" = filter_data(rapid_filt()),
#   #   "aye" = filter_data(aye),
#   #   "nhs24" = filter_data(nhs24),
#   #   "ooh" = filter_data(ooh),
#   #   "sas" = filter_data(sas)
#   # ) %>%
#   first_datatable %>%
#     select(area_name, time_period_eligible, denominator, coverage_tot_num, coverage_tot_percent) %>%
#     rename(children_tot_num = denominator) #%>%
# #    mutate(week_ending = format(week_ending, "%d %b %y"))
# #})
# 
# output$download_chart_data_child <- downloadHandler(
#   filename ="data_extract.csv",
#   content = function(file) {
#     write_csv(overall_data_download_child,
#               file) }
# )

#END

