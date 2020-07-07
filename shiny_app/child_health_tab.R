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
output$child_first_scurve <- renderPlotly({plot_scurve_child(first, "2 weeks")})
output$child_first_table <- renderUI({child_table(firsttable, "2 weeks", "18 weeks")})

output$child_sixtoeight_scurve <- renderPlotly({plot_scurve_child(sixtoeight, "6 weeks")})
output$child_sixtoeight_table <- renderUI({child_table(sixtoeighttable, "6 weeks", "24 weeks")})

output$child_thirteen_scurve <- renderPlotly({plot_scurve_child(thirteen, "13 months")})
output$child_thirteen_table <- renderUI({child_table(thirteentable, "13 months", "17 months")})

output$child_twentyseven_scurve <- renderPlotly({plot_scurve_child(twentyseven, "27 months")})
output$child_twentyseven_table <- renderUI({child_table(twentyseventable, "27 months", "31 months")})

# The charts and text shown on the app will depend on what the user wants to see
output$child_health_explorer <- renderUI({

  # text for titles of cut charts
  child_title <- paste0(case_when(input$measure_select_child == "first_visit" ~ paste0("Coverage of health visitor first visit (offered to children at 2 weeks of age): ",
                                                                                             input$geoname_child),
                            input$measure_select_child == "six_eightwks" ~ paste0("Coverage of child health review offered at 6-8 weeks of age: ", input$geoname_child),
                            input$measure_select_child == "13_15mnth" ~ paste0("Coverage of child health review offered at 13-15 months of age: ", input$geoname_child),
                            input$measure_select_child == "27_30mnth" ~ paste0("Coverage of child health review offered at 27-30 months of age: ", input$geoname_child)))
  child_subtitle <-  paste0("Figures based on data extracted from SIRS and CHSP-PS on ",child_extract_date)
  
  #commentary to appear in child health tab
  commentary_first <-p("All preschool children should be offered the following health reviews: health visitor first visit, 6-8 week review, 13-15 month review, 27-30 month review, and 4-5 year review. Although the 4-5 year review only became mandated by government policy for children turning 4 from April 2020 onwards.", br(),
"The charts show the progression of coverage of the relevant review as children age. The data tables provide the coverage rates at three specific time-points. Data is shown for children who have become eligible for review during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for review before the pandemic (in 2019 and in January and February 2020) for comparison.", br(), 
"After a child becomes eligible for a review, it takes time for them to attend their appointment, and for a record of the review provided to subsequently be entered into the CHSP-PS system. We have allowed a 6-week window for this, therefore each release of this page will report on children becoming eligible for a review up to 6 weeks before the date these data were extracted for analysis. Although children will generally have their review, and their CHSP-PS record updated accordingly, within 6 weeks of becoming eligible, the pandemic may have influenced not only how quickly eligible children receive their reviews, but also how long it takes for children’s CHSP-PS records to be updated once a review has been given. Any disruption to CHSP-PS data entry may vary across NHS Boards. Data shown for the most recent cohorts of children will therefore not be fully complete in CHSP-PS and should be viewed as provisional. The coverage rates for each cohort will be refreshed with more up-to-date data every 4 weeks, and rates for the most recent cohorts may increase slightly as relevant records are updated in CHSP-PS.", br(), 
"Data is shown for Scotland and for NHS Board areas. Due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland, and NHS Western Isles are not shown separately, however the Island Boards are included within the Scotland total. NHS Grampian has had difficulty recording reviews given on the CHSP-PS system since the start of the Covid-19 pandemic. Information on children in Grampian has therefore been excluded, and Grampian is not included in the Scotland totals. We hope to include NHS Grampian in future releases once local data recording difficulties are resolved.", br(),
"Coverage rates based on small numbers are prone to fluctuation. Therefore, in Boards with small numbers of children eligible for review each week, particularly NHS Borders and NHS Dumfries & Galloway, it is important to consider this when interpreting the rates.
")
  
  explorer_child <-     tagList(
    fluidRow(column(12, h4(paste0(child_title)),
                    p(child_subtitle))))
  
  # Specify items to display in child health ui based on step 2 selection 
  if (input$measure_select_child == "first_visit") {
    tagList(explorer_child,
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_first_scurve"))),
               column(6, uiOutput("child_first_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  }  else if (input$measure_select_child == "six_eightwks"){
    tagList(explorer_child,
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_sixtoeight_scurve"))),
               column(6, uiOutput("child_sixtoeight_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else if (input$measure_select_child == "13_15mnth") {
    tagList(explorer_child,
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_thirteen_scurve"))),
               column(6, uiOutput("child_thirteen_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else if (input$measure_select_child == "27_30mnth") {
    tagList(explorer_child,
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_twentyseven_scurve"))),
               column(6, uiOutput("child_twentyseven_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else {
    p("4-5 Year Review coming 15th July 2020")
  }
  
}) #close child_health_explorer function


###############################################.
## Child Health Commentary tab content  ----
###############################################.


 output$child_comments <- renderUI({
     tagList(
       bsButton("jump_to_childhealth",label = "Go to data"),
       h2("Child Health reviews - 8th July 2020"), 
   p("Information on the uptake of child health reviews that are routinely offered to all preschool children by Health Visitors was included in this tool for the first time on 10 June 2020. Child health reviews incorporate assessment of children's health, development, and wider wellbeing alongside provision of health promotion advice and parenting support. Routine child health reviews help ensure that children’s health and development is progressing as expected for their age and stage, and allow any concerns to be addressed. It is important that children continue to receive their routine health reviews during the Covid-19 pandemic. On 10 June 2020, information was provided on the coverage of the Health Visitor first visit, which is offered to children at 10-14 days of age. Coverage of the 6-8 week review was added on 24 June, and coverage of the 13-15 month and 27-30 month review were added on 8 July (and data for the first visit and 6-8 week review were also updated).", br(), br(),
      "Coverage rates for the Health Visitor first visit have remained high during the pandemic. Coverage continues to exceed 90% among children who were due their review in March and April.", br(), br(),
      "Data from before the pandemic, for children becoming eligible in 2019, show that coverage of the 6-8 week review by 10 weeks was 83%. Coverage has gradually fallen since the beginning of 2020, and was around 67-77% for children becoming eligible for the review in March and April.", br(), br(),
      "There are a number of important factors to take into account when interpreting this information. Unlike all the other pre-school child health reviews, the 6-8 week review is a two stage process involving the baby’s Health Visitor and their GP. Usually, the Health Visitor first visits the family at home to conduct a general assessment of the baby’s progress and the family’s wellbeing. Then, the GP offers an appointment in the practice to conduct a detailed physical examination of the baby. Usually the GP appointment happens shortly after the Health Visitor visit, and the data from both assessments is then returned together to the NHS Board child health administrative department for entry into the CHSP-PS system. Since the start of the Covid-19 pandemic, Scottish Government policy has been that the 6-8 week review should continue to be provided. In practice, to minimise the number of times babies are brought into the practice, in some areas the GP element of the review may have been deferred, for example until the baby is due a routine immunisation appointment at a later stage. Areas may then vary in terms of whether Health Visitors return information on their part of the 6-8 week review for entry into the baby’s CHSP-PS record, or whether no information is returned until the GP part of the review is completed. As GPs start to ‘catch up’ with their part of outstanding 6-8 week reviews, we would expect to see coverage rates for children becoming eligible for this review during the pandemic increasing. It is important to note therefore that no record of a 6-8 week review on the CHSP-PS system does not necessarily mean that the baby has not been seen at all: they may have been visited by their Health Visitor, but not yet examined by their GP.", br(), br(),      
      "For babies born prematurely, the 6-8 week review is offered to children 6-8 weeks following their expected date of delivery rather than their actual date of birth. This is to ensure a ‘fair’ assessment of children’s progress against what would be expected of a baby at that stage. As the information shown in the dashboard is based on children’s actual date of birth rather than due dates, premature babies will appear to have their review ‘late’, when in fact it was offered appropriately. This partially accounts for why coverage of the 6-8 week review continues to increase as babies attain older ages. Finally, it should also be restated that we have allowed a 6 week window for data entry, that is we have reported on reviews provided to children becoming eligible for their reviews up to 6 weeks prior to the date we extracted data from the CHSP-PS system. The results of a completed review would generally be expected to be entered into the CHSP-PS system within this 6 week time frame. In practice however occasional data entry delays occur. These may be worse during the Covid-19 pandemic, and may vary between areas. For all these reasons, review coverage for the most recent cohorts should therefore be taken as provisional, and is likely to increase over time as relevant data accumulates.", br(), br(),
      "Data for the 13-15 month review show that for children eligible for review in 2019, 41% had received their review by 14 months, rising to 81% by 17 months. For children eligible in March and April 25-35% had received their review by 14 months. These children have not yet reached 17 months of age.", br(), br(),
      "Data for the 27-30 month review show that for children eligible for review in 2019, 33% had received their review by 28 months, rising to 81% by 31 months. For children eligible in March and April 18-26% had received their review by 31 months. These children have not yet reached 31 months of age. It should be noted that NHS Greater Glasgow & Clyde have a different policy for calling children for this review, as they call at 30 months rather than 27 months in the rest of Scotland. Hence, their coverage by 28 months looks very poor.", br(), br(),
      "Data for the 13-15 month and 27-30 month reviews show that coverage has fallen since the start of the pandemic, and this may be due to Health Visitors prioritising the earlier reviews (first visit and 6-8 weeks). Health Visitors were advised that, if possible, the 13-15 month and 27-30 month reviews should continue via NHS near-me (a video conferencing facility) or telephone, but in some NHS Board areas this has not been able to happen, and this is reflected in the data."
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
    "first_visit" = filter(first_datatable_download, area_name == input$geoname_child),
    "six_eightwks" = filter(sixtoeight_datatable_download, area_name == input$geoname_child),
    "13_15mnth" = filter(thirteen_datatable_download, area_name == input$geoname_child),
    "27_30mnth" = filter(twentyseven_datatable_download, area_name == input$geoname_child)
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

