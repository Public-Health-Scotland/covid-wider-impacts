##Server script for immunisations tab

# Pop-up modal explaining source of data
observeEvent(input$btn_immune_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The information shown on the numbers of children eligible for, and receiving, routine preschool 
                 immunisations is taken from the ",
                 tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12",
                        "Scottish Immunisation and Recall System (SIRS)",class="externallink")),
               p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the 
                 invitation of children when a scheduled immunisation is due.  When a child receives a immunisation, 
                 relevant information is returned to administrative staff in the NHS Board child health department.  
                 The administrative staff then update the child’s SIRS record accordingly."),
               p("After a child receives an immunisation, it takes time for a record of the immunisation to be entered 
                 into the SIRS system. We have allowed a 6-week window for data entry. Each release of this page will 
                 therefore report on immunisations given up to 6 weeks before the date these data were extracted for 
                 analysis. Although the vast majority of data on immunisations given are usually recorded within 6 weeks, 
                 the pandemic will have affected some data recording and this will vary across NHS Boards. Data shown for 
                 the most recent cohorts of children will therefore not be fully complete in SIRS at this stage. 
                 The uptake rates for each cohort will be refreshed with more up-to-date data every 4 weeks."),
               p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink"),
                 " routinely receives quarterly data extracts from SIRS for the purpose of producing and ",
                 (tags$a(href="https://www.isdscotland.org/Health-Topics/Child-Health/Immunisation/","publishing",class="externallink"))," 
                 immunisation uptake rates.  To allow more rapid monitoring of the impact of Covid-19 on 
                 childhood immunisation uptake rates, PHS is also currently extracting a sub-set of
                 data from SIRS each month."),
               p("Uptake rates based on small numbers are prone to fluctuation. Therefore in boards 
                 with small numbers of children eligible for immunisation each week, particularly NHS Borders 
                 and NHS Dumfries & Galloway, it is important to consider this when interpreting the rates."),
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
filter_table_data_immun <- function(dataset){
  dataset %>% filter(area_name == input$geoname_immun)
}

###############################################.
## Immunisation Tab Reactive layout  ----
###############################################.

# Creating plots for each dataset
#run chart function to generate s curve  

output$immun_6in1_scurve_dose1 <- renderPlotly({plot_scurve(six, age_week = "8")})
output$immun_6in1_table_dose1 <- renderUI({immune_table(sixtable, age_week = 8)})

output$immun_6in1_scurve_dose2 <- renderPlotly({plot_scurve(six_dose2, age_week = "12")})
output$immun_6in1_table_dose2 <- renderUI({immune_table(sixtable_dose2, age_week = 12)})

output$immun_6in1_scurve_dose3 <- renderPlotly({plot_scurve(six_dose3, age_week = "16")})
output$immun_6in1_table_dose3 <- renderUI({immune_table(sixtable_dose3, age_week = 16)})

# The charts and text shown on the app will depend on what the user wants to see
output$immunisation_explorer <- renderUI({
  
  # text for titles of cut charts
  immune_title <- paste0(case_when(input$measure_select_immun == "sixin_dose1" ~ paste0("Uptake of first dose of 6-in-1 vaccine (offered to children at 8 weeks of age): ",
                                                                                       input$geoname_immun),
                                   input$measure_select_immun == "sixin_dose2" ~ paste0("Uptake of second dose 6-in-1 vaccine (offered to children at 12 weeks of age): ", input$geoname_immun),
                                   input$measure_select_immun == "sixin_dose3" ~ paste0("Uptake of third dose 6-in-1 vaccine (offered to children at 16 weeks of age): ", input$geoname_immun)))
  
  # Intro paragraph within imumunisation tab
  intro_6in1 <- p("Immunisation protects children against certain serious infections.  It is important that children ",
                  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
                         "continue to receive their routine immunisations during the Covid-19 pandemic",class="externallink"),".",
                  "Public Health Scotland and Scottish Government have produced a range of communications reminding parents that the NHS is still open for childhood immunisations, signposting parents to up to date advice via ",
                  tags$a(href="https://twitter.com/NHSImmuniseScot"," Immunise Scotland ",class="externallink"),
                  " and ",tags$a(href="https://www.nhsinform.scot/immunisation","NHS inform",class="externallink"),".")
  
  #Additional commentart/meta data to appear on immunisation tab
  commentary_6in1 <-  p("All preschool children are offered a total of five immunisation appointments as they reach the following ages: 8, 12, and 16 weeks; 12-13 months; and 3 years and 4 months of age. Multiple immunisations are offered at each appointment. Here, for simplicity, we have just shown the uptake of one of the immunisations offered at each appointment. ",br(), 
                        "The charts show the progression of uptake of the relevant immunisation as children age. The data tables provide the uptake rates at three specific time-points. Data is shown for children who have become eligible for immunisation during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for immunisation before the pandemic (in 2019 and in January and February 2020) for comparison.",br(),
                        "After a child receives an immunisation, it takes time for a record of the immunisation to be entered into the SIRS system. We have allowed a 6-week window for data entry. Each release of this page will therefore report on immunisations given up to 6 weeks before the date these data were extracted for analysis. Although the vast majority of data on immunisations given are usually recorded within 6 weeks, the pandemic will have affected some data recording and this will vary across NHS Boards. Data shown for the most recent cohorts of children will therefore not be fully complete in SIRS at this stage. The uptake rates for each cohort will be refreshed with more up-to-date data every 4 weeks.",br(),
                        "Data is shown for Scotland and for NHS Board areas. Due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland, and NHS Western Isles are not shown separately, however the Island Boards are included within the Scotland total. Aberdeenshire local authority area within NHS Grampian has had difficulty recording immunisations given on the SIRS system since the start of the Covid-19 pandemic. Information on children in Aberdeenshire has therefore been excluded from figures provided for NHS Grampian and Scotland as a whole. We hope to include Aberdeenshire in future releases once local data recording difficulties are resolved.",br(),
                        "Uptake rates based on small numbers are prone to fluctuation. Therefore, in boards with small numbers of children eligible for immunisation each week, particularly NHS Borders and NHS Dumfries & Galloway, it is important to consider this when interpreting the rates.")
  
  # Specify items to display in immunisation ui based on step 2 selection 
  if (input$measure_select_immun == "sixin_dose1") {
    tagList(
      fluidRow(column(12, renderUI(intro_6in1),
                      h4(paste0(immune_title)))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("immun_6in1_scurve_dose1"))),
               column(6, uiOutput("immun_6in1_table_dose1"))),
      fluidRow(column(12, renderUI(commentary_6in1)))
    )
  }  else if (input$measure_select_immun == "sixin_dose2"){
    tagList(
      fluidRow(column(12, renderUI(intro_6in1),
                      h4(paste0(immune_title)))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("immun_6in1_scurve_dose2"))),
               column(6, uiOutput("immun_6in1_table_dose2"))),
      fluidRow(column(12, renderUI(commentary_6in1)))
    )
  }  else if (input$measure_select_immun == "sixin_dose3"){
    tagList(
      fluidRow(column(12, renderUI(intro_6in1),
                      h4(paste0(immune_title)))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("immun_6in1_scurve_dose3"))),
               column(6, uiOutput("immun_6in1_table_dose3"))),
      fluidRow(column(12, renderUI(commentary_6in1)))
    )}

}) #close immunisation_explorer function

###############################################.
## Data downloads ----
###############################################.

# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
imm_data_download <- reactive({
  switch(
    input$measure_select_immun,
    "sixin_dose1" = sixtable,
    "sixin_dose2" = sixtable_dose2,
    "sixin_dose3" = sixtable_dose3
  ) %>% 
    select(area_name, time_period_eligible, denominator, starts_with("uptake"))  %>% 
    rename(cohort = time_period_eligible)
})

output$download_imm_data <- downloadHandler(
  filename ="immunisation_extract.csv",
  content = function(file) {
    write_csv(imm_data_download(),
              file) } 
)

###############################################.
## Immunisation Commentary tab content  ----
###############################################.

output$immun_commentary_section <- renderUI({
  tagList(h2("Immunisations - 3rd June 2020"), 
          p("Information on the uptake of ",
            tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","immunisations that 
                   are routinely offered to all preschool children",class="externallink"),
            " has been included in this tool for the first time on 3 June 2020.", br(),
            "Immunisation protects children against many serious infectious diseases including diphtheria, 
            whooping cough, and measles.",
            tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
                   "Immunisation services throughout Scotland are continuing during the Covid-19 pandemic", class="externallink"),".",
            "It is important to maintain the best possible immunisation uptake rates to ensure children 
            remain protected and to prevent a resurgence of these infections.  Including information on 
            childhood immunisation rates in this tool will help us to ensure that immunisation rates remain 
            high throughout the pandemic.",br(),
            "On 3 June 2020, information has been provided on the uptake of the first dose of the 6-in-1 
            vaccine, which is offered to children at 8 weeks of age. The vaccine protects against diphtheria, 
            tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and Hepatitis B. 
            Children should also receive a second dose of the vaccine at 12 weeks and a third dose at 16 weeks.",br(),
            "Uptake rates for this immunisation have remained high during the pandemic.  Uptake continues to exceed 90% among
            children who were due their first dose of the 6-in-1 vaccine in March and early April. The recording of data on
            immunisations given by the reporting date will not be fully complete at this stage, particularly for the most recent
            cohorts, so uptake rates are slightly under-reported. In addition, some children will receive the vaccine at a later age,
            for example due to missed or rescheduled appointments, so uptake rates are expected to continue to increase as children age 
            (as shown in the 2019 data provided for comparison)."
          ))
})


output$immun_commentary_1705 <- renderUI({
  tagList(h2("Immunisations - 17th June 2020"), 
          p("Information on the uptake of ",
            tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","immunisations that 
                   are routinely offered to all preschool children",class="externallink"),
            " has been included in this tool for the first time on 3 June 2020.", br(),
            "Immunisation protects children against many serious infectious diseases including diphtheria, 
            whooping cough, and measles.",
            tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
                   "Immunisation services throughout Scotland are continuing during the Covid-19 pandemic", class="externallink"),".",
            "It is important to maintain the best possible immunisation uptake rates to ensure children 
            remain protected and to prevent a resurgence of these infections.  Including information on 
            childhood immunisation rates in this tool will help us to ensure that immunisation rates remain 
            high throughout the pandemic.",br(),
            "The 6-in-1 vaccine is given to babies at 8, 12 and 16 weeks of age. The vaccine protects against diphtheria, tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and Hepatitis B.",br(),
            "On 3 June 2020, information was provided on the uptake of the first dose of the 6-in-1 vaccine, offered at 8 weeks of age. This showed uptake continues to exceed 90% among children who were due their first dose of the 6-in-1 vaccine in March and early April.",br(),
            "On 17 June, information on the uptake of the second and third doses was added to the tool. The second dose of 6-in-1 vaccine is offered at 12 weeks of age. Data before the pandemic, for children eligible in 2019, show that uptake of the second dose by 16 weeks was 84.5%. Uptake by 16 weeks continues to exceed 80% among children who were due their second dose of the 6-in-1 vaccine in March and early April.",br(),
            "The third dose of 6-in-1 vaccine is offered at 16 weeks of age. Data before the pandemic, for children eligible in 2019, show that uptake of the third dose by 20 weeks was 72.3%. Uptake by 20 weeks continues to exceed 70% among children who were due their third dose of the 6-in-1 vaccine in March and early April.",br(),
            "It is important to note that uptake of the second and third doses take longer to reach 90% and above compared to the first dose, as demonstrated by the data on uptake before the pandemic. This is because some children receive the first dose later than when first offered the vaccine, for example due to missed appointments. As each dose of vaccine is offered 4 weeks apart, missed appointments has a cumulative effect in increasing the time it takes for uptake of the second and third doses to reach and exceed 90%."
          ))
})

#END

