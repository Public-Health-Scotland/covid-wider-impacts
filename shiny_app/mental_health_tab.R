##Server script for MENTAL HEALTH tab

# Helper function
`%notin%` <- Negate(`%in%`)
# Show list of area names depending on areatype selected
output$geoname_mh_ui <- renderUI({
  
  areas_summary_mh <- sort(geo_lookup$areaname[geo_lookup$areatype == ifelse(input$area_mh_select %notin%
                                                                            c("Scotland", "Health board", "HSC partnership"),
                                                                          "Scotland",
                                                                          input$area_mh_select)])
  selectizeInput("geoname_mh", label = NULL,
                 choices = areas_summary_mh, selected = "")
  
})

# Adding 'observeEvent' to allow reactive 'area of interest' 
observeEvent(input$measure_mh_select, {
    mh_label = "Step 2 - Select geography level for mental health medicine prescriptions"
    mh_choices = c("Scotland", "Health board", "HSC partnership")
    shinyjs::show("geoname_mh_ui")
    enable("area_mh_select")
  
})

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_mentalhealth_modal,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p(strong("Data source: ePrescribed Messages.")),
               p("This section of the PHS Covid-19 - Wider Impacts dashboard provides weekly information on the 
                   number of prescriptions for mental health drugs issued. The data ranges from the start of 2020 
                   to the latest available week and is shown alongside historical activity (average from 2018 and 2019) 
                   for comparison purposes. Additional breakdowns by drug grouping are provided also."),
               tags$b("What is an electronic prescription message?"),
               p("In the majority of cases, electronic messages are generated when a prescription is issued 
                   by a GP Practice. Approximately 95% of prescriptions for medicines are written by GPs and over 97% 
                   of these have electronic messaging (eMessage) support."),
               tags$b("Why are we using electronic prescription message data?"),
               p("The information from these eMessages is normally transferred into Public Health Scotland databases 
                   within 48 hours of being written and so, by using this, we are able to analyse and detect changes 
                   in prescribing behaviour in almost real-time.  This compares with a delay of two-three months, or 
                   longer, for data to become available through the prescription payment process."),
               p("Real-time intelligence is particularly important during the Covid-19 crisis and the majority of 
                   information needed is available from eMessages."),
               tags$b("How we identify new treatment courses"),
               p("When patients are receiving ongoing treatment they typically receive a prescription for their medicine every 4-8 weeks.  
                 Patients starting a new treatment course were identified as those people receiving a prescription and who had not received 
                 a prescription for the same type of medicine in the preceding 13 weeks."),
               tags$b("Data artefacts"),
               p("Data for 1 January to 31 December each year is presented as weekly data ending on Sundays.  The week from 1st January 
                 may therefore not be a full seven days and will that week, or the following week may encompass public holidays.  
                 Consequently, the number of prescribing days and measures of activity at the start of each year can be markedly reduced 
                 compared to subsequent weeks.  A similar effect also occurs in the last two weeks of the year."),
               tags$b("Limitations of electronic prescription message data"),
               p("Not all prescribers have electronic messaging support and not all prescriptions that are written 
                   will be dispensed, so it is only once all prescriptions have been submitted and processed for payment 
                   that the data can be considered as complete.  Analyses using eMessages should therefore be considered 
                   as provisional and incomplete for all prescriber types, when compared with paid data. "),
               p("The eMessage data does not have the same links to the reference data compared with processed and paid 
                   data and that is normally used to aggregate and analyse groups of medicines.  This can make analysis 
                   challenging and limit the types of analyses that can be performed."),
               p("The eMessage prescription data will also not capture most supplies made through a serial prescription.  
                   A serial prescription generates an electronic prescription message at the time of prescribing but this 
                   is then dispensed in a series of regular supplies over a period of 24, 48 or 52 weeks.  These subsequent 
                   supplies are captured in a separate dataset.  The extent to which serial prescriptions are used may vary 
                   by both geographic and therapeutic areas."),
               p("The ePrescribed messaging dataset is managed by Public Health Scotland (PHS)."),
              
               # actionButton("toggle_mh_drug_codes", "Show / Hide BNF Codes"),
               # shinyjs::onclick("toggle_mh_drug_codes",
               #                  shinyjs::toggle(id = "mh_drug_codes")),
               # shinyjs::hidden(div(id="mh_drug_codes",
               #                     br(),
               #                     p("Include codes here.")
               # )),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$mh_explorer <- renderUI({
  
    tagList(# Prescribing - items dispensed
      h3(paste0("Number of patients starting a new treatment course for selected mental health medicines in ", input$geoname_mh)),
      actionButton("btn_mentalhealth_modal", "Data source: ePrescribed Messages",
                   icon = icon('question-circle')),
      plot_box("2020 compared with 2018-2019 average", "mh_prescribing_all"),
      plot_cut_box(paste0("Percentage change in the number of patients starting a new treatment course for selected mental health medicines in ", input$geoname_mh, 
                          " compared with average of the corresponding time in 2018 and 2019 by medicine groupings"), "mh_drugs_var",
                   paste0("Weekly number of patients starting a new treatment course for selected mental health medicines in ", input$geoname_mh, " by medicine groupings"), "mh_drugs_tot"))
  
  
})

###############################################.
# MH Prescribing charts
output$mh_prescribing_all <- renderPlotly({plot_overall_chart(mentalhealth_drugs %>% filter(area_name == input$geoname_mh),
                                                              data_name = "mentalhealth_drugs", area = "All")})
output$mh_drugs_var <- renderPlotly({
  plot_trend_chart(mentalhealth_drugs, pal_med, split = "condition", 
                   data_name = "mentalhealth_drugs", tab = "mh")})
output$mh_drugs_tot <- renderPlotly({
  plot_trend_chart(mentalhealth_drugs, pal_med, split = "condition", type = "total", 
                   data_name = "mentalhealth_drugs", tab = "mh")})

###############################################.
## Data downloads ----
###############################################.

mh_down_data <- reactive({
  switch(
    input$measure_mh_select,
    "mhdrugs" = mentalhealth_drugs %>% filter(area_name == input$geoname_mh &
                                                area_type == input$area_mh_select)
  ) %>% 
    rename(average_2018_2019 = count_average) %>% select(-type)

})

output$download_mentalhealth_data <- downloadHandler(
  filename ="mentalhealth_extract.csv",
  content = function(file) {
    write_csv(mh_down_data(),
              file) }
)

###############################################.
## Commentary ----
###############################################.

output$mentalhealth_commentary <- renderUI({
  tagList(
    bsButton("jump_to_mentalhealth",label = "Go to data"), #this button can only be used once
    h2("Mental health - August 2020"),
    h3("Prescribing"),
    p("Information on the number of patients starting a new treatment course for selected mental health medicines (those commonly used for depression, anxiety or 
      insomnia) through General Practice has been included for the first time on 19 August 2020. This data indicates:"),
    tags$ul(
      tags$li("The number of patients starting new treatment with the selected medicines fell by almost 40% between the week prior to the introduction of lockdown and early April. 
              Since then, the total numbers have been gradually increasing and returned to normal levels by the end of June."),
      tags$li("The number of new treatment courses with medicines for depression and insomnia show a similar pattern of decline and recovery whereas medicines for anxiety 
              show a more prolonged decline in the number of new treatment courses and, by mid-July, remain about 15% below normal.")
  ))
})
