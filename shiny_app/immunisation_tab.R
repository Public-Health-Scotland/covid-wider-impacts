##Server script for immunisations tab


# Pop-up modal explaining source of data
observeEvent(input$btn_immune_modal, 
                 showModal(modalDialog(#RAPID ADMISSIONS MODAL
                 title = "What is the data source?",
                 p("The information shown on the numbers of children eligible for, and receiving, routine preschool immunisations is taken from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12",
                          "Scottish Immunisation and Recall System (SIRS)",class="externallink")),
                 p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation of children when a scheduled vaccination is due.  When a child receives a vaccine, relevant information is returned to administrative staff in the NHS Board child health department.  The administrative staff then update the child’s SIRS record accordingly."),
                 p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink")," routinely receives quarterly data extracts from SIRS for the purpose of producing and ",
                   (tags$a(href="https://www.isdscotland.org/Health-Topics/Child-Health/Immunisation/","publishing",class="externallink"))," immunisation uptake rates.  To allow more rapid monitoring of the impact of Covid-19 on childhood immunisation uptake rates, PHS is also currently extracting a sub-set of data from SIRS each month."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))
    

###############################################.
## Immunisation Reactive controls  ----
###############################################.

# Immunisation reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_immun <- renderUI({
  
  #Lists areas available in   
  areas_summary_immun <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_immun])
  
  selectizeInput("geoname_immun", label = NULL, choices = areas_summary_immun, selected = "")
})


# Reactive dataset for flextable filter on geographical area
table_data <- reactive({  
  table <- sixtable %>%
    filter(area_name==input$geoname_immun)
    #mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%  # required if table sort order is to change
    #arrange(cohort)
})


###############################################.
## Immunisation Tab Reactive layout  ----
###############################################.

#run chart function to generate s curve  
output$immun_6in8_scurve <- renderPlotly({plot_scurve(six)})
output$immun_6in8_table <- renderUI({immune_table()})


# The charts and text shown on the app will depend on what the user wants to see
output$immunisation_explorer <- renderUI({

  # text for titles of cut charts
  immune_title <- paste0(case_when(input$measure_select_immun == "sixin_8wks" ~ paste0("Uptake of first dose of 6-in-1 vaccine (offered to children at 8 weeks of age): ",
                                                                                             input$geoname_immun),
                            input$measure_select_immun == "sixin_12wks" ~ paste0("Uptake of second dose 6-in-1 vaccine (offered to children at 12 weeks of age): ", input$geoname_immun),
                            input$measure_select_immun == "sixin_16wks" ~ paste0("Uptake of third dose 6-in-1 vaccine (offered to children at 16 weeks of age): ", input$geoname_immun)))
  
  #6-in-1: 8 weeks commentary to appear in immunisations tab
  commentary_6in1 <-p("Vaccination protects children against certain serious infections.  It is important that children ",
                      tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
                             "continue to receive their routine vaccinations during the Covid-19 pandemic",class="externallink"),".",br(),
                      "Public Health Scotland and Scottish Government have produced a range of communications reminding parents that the NHS is still open for childhood immunisations, signposting parents to up to date advice at ",
                      tags$a(href="https://twitter.com/NHSImmuniseScot","https://twitter.com/NHSImmuniseScot ",class="externallink"),
                      " and ",tags$a(href="https://www.nhsinform.scot/immunisation","https://www.nhsinform.scot/immunisation",class="externallink"),".",br(),
                      #"This page provides information on the uptake of ",
                      #tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","vaccinations that are routinely offered to all preschool children",class="externallink"),
                      #". This will help us to ensure that vaccination rates remain high throughout the pandemic.",br(),
                      "All preschool children are offered a total of five vaccination appointments as they reach the following ages: 8, 12, and 16 weeks; 12-13 months; and 3 years and 4 months of age.  Multiple vaccinations are offered at each appointment.",br(), 
                      "Here, for simplicity, we have just shown the uptake of one of the vaccines offered at each appointment. The charts show the progression of uptake of the relevant vaccine as children age.  The data tables provide the uptake rates at two time-points in the chart.  Firstly, the uptake rate reached by a fixed age is shown.  For example, for the first dose of the 6-in-1 vaccine which is offered at the 8 week appointment, uptake by the time children turn 12 weeks old is shown.  Secondly, the overall uptake rate recorded by the date the data was extracted from SIRS for analysis is shown.",br(),
                      "Data is shown for children who have become eligible for vaccination during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for vaccination before the pandemic (in 2019 and in January and February 2020) for comparison. After a child receives a vaccination, it can take some time for the record of the vaccination to be returned to the NHS Board child health department and entered into the SIRS system.  We have allowed a 6 week window for data entry.  So, for the first release of this page on 3 June 2020, information was extracted from SIRS on 25 May, and results were reported for children becoming eligible for vaccination up to 6 weeks previously,
                      i.e. up to the week beginning 6 April.  Although the vast majority of data on vaccinations given will be recorded within 6 weeks, data shown for the most recent cohorts of children will not be fully complete in SIRS at this stage.",br(),  
                      "Data is shown for Scotland and for NHS Board areas separately.  Due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland, and NHS Western Isles are not shown separately, however the Island Boards are included within the Scotland total.  Aberdeenshire local authority area within NHS Grampian has had difficulty recording vaccinations given on the SIRS system since the start of the Covid-19 pandemic.  Information on children in Aberdeenshire has therefore been excluded from figures provided for NHS Grampian and Scotland as a whole.  We hope to include Aberdeenshire in future releases once local data recording difficulties are resolved.")
  
  # Specify items to display in immunisation ui based on step 2 selection 
  if (input$measure_select_immun == "sixin_8wks") {
    tagList(
      fluidRow(column(10, h4(paste0(immune_title)))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("immun_6in8_scurve"))),
               column(6, uiOutput("immun_6in8_table"))),
      fluidRow(column(12, renderUI(commentary_6in1)))
    )
  }  else if (input$measure_select_immun == "sixin_12wks"){
    p("6-in-1 at 12 weeks coming 10th June 2020")
  }  else if (input$measure_select_immun == "sixin_16wks"){
    p("6-in-1 at 16 weeks coming 17th June 2020")}
  
}) #close immunisation_explorer function


###############################################.
## Immunisation Commentary tab content  ----
###############################################.


output$immun_comments <- renderUI({
    
  p("Information on the uptake of ",
      tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","vaccinations that are routinely offered to all preschool children",class="externallink"),
      " has been included in this tool for the first time on 3 June 2020.", br(),
      "Vaccination protects children against many serious infectious diseases including diphtheria, whooping cough, and measles.",
      tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening","Immunisation services throughout Scotland are continuing during the Covid-19 pandemic",class="externallink"),".",
     "It is important to maintain the best possible vaccination uptake rates to ensure children remain protected and to prevent a resurgence of these infections.  Including information on childhood vaccination rates in this tool will help us to ensure that vaccination rates remain high throughout the pandemic.",
    "On 3 June 2020, information has been provided on the uptake of the first dose of the 6-in-1 vaccine, which is offered to children at 8 weeks of age. The vaccine protects against diphtheria, tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and Hepatitis B. Children should also receive a second dose of the vaccine at 12 weeks and a third dose at 16 weeks.",br(),
    "Uptake rates for this vaccination have remained high during the pandemic.  Uptake continues to exceed 90% among children who were due their first dose of the 6-in-1 vaccine in March and early April. The recording of data on vaccinations given by the reporting date will not be fully complete at this stage, particularly for the most recent cohorts, so uptake rates are slightly under-reported. In addition, some children will receive the vaccine at a later age, for example due to missed or rescheduled appointments, so uptake rates are expected to continue to increase as children age (as shown in the baseline)."
    )
})


#END

