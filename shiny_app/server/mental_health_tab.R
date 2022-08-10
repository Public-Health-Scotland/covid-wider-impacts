# Wider impacts dashboard - Mental health tab
# Server code

###############################################.
## Reactive controls ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("mh")

# Adding 'observeEvent' to allow reactive 'area of interest' selction 
observeEvent(input$`mh-measure`, {
  
  if (input$`mh-measure` == "mhdrugs") {
    areas <- c("Scotland", "Health board", "HSC partnership")
  } else if (input$`mh-measure` %in% c("aye", "ooh")) {
    areas <- c("Scotland", "Health board")
  }
  
  updateSelectInput(session, "mh-geotype", choices = areas)
})

###############################################.
## Modal ----
###############################################.

week_standard <- " are allocated to weeks based on the ISO8601 standard. Following this standard
The year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use
the 2018-2019 average of week 52 value as a comparator for 2020’s week 53."

# Pop-up modal explaining source of data
observeEvent(input$`mh-source-modal`,
             if (input$`mh-measure` == "aye") {
               showModal(modalDialog(# MH AYE MODAL
                 title = "What is the data source?",
                 p("This tool provides a weekly summary of people attending A&E departments (Emergency Departments)
                   in the recent past, along with historical activity for
                   comparison purposes. The recent trend data is shown by age group and sex. These figures include attendances of people aged 5 and over.
                    Also, this data only include Emergency Department
                   attendances and do not include minor injury units and other small hospitals and
                   health centres in rural areas that carry out emergency department related activity,
                   for more information on what sites are included please see this ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/Hospital-Site-List/",
                          "hospital list.", class="externallink")),
                 p("Additional information relating to the overall A&E activity is available from the ",
                   tags$a(href="https://publichealthscotland.scot/publications/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics",
                          "NHS Performs - weekly update of emergency department activity and waiting time statistics.",
                          class="externallink")),
                 p("Attendances to A&E departments data sourced from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=3",
                          "Accident and Emergency Datamart (A&E2) (external website).",class="externallink"),
                   "The A&E2 dataset is managed by ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/",
                          "Public Health Scotland (PHS).", class="externallink")),
                 p("Attendances", week_standard),
                 p("Please note that, due to limitations in diagnosis recording in the A&E datamart, the data are
                           incomplete for a number of NHS Boards. Thus, the figures reported for mental health related
                           attendances offer only a very approximate indication of attendances.
                           Additionally, some NHS Boards have moved to a new recording standard which
                           has not been fully consolidated in the A&E datamart as yet."),
                 p("Mental health related A&E attendances were identified using these parameters:"),
                 tags$ul(
                   tags$li("Diagnosis of mental and behavioural disorders (excluding dementia and learning disabilities) - ICD10 codes F."),
                   tags$li("Diagnosis of intentional self-harm - ICD10 codes X60 - X84."),
                   tags$li("Diagnosis of symptoms involving emotions, perceptions and behaviour - ICD10 codes R44 - R46."),
                   tags$li("Other mental health related diagnosis codes - ICD10 codes Y871, Z914, Z915, Z004, Z046."),
                   tags$li("Psychiatry code recorded against the attendance."),
                   tags$li("Intent of injury recorded as 'Deliberate self-harm'."),
                   tags$li("A mental health related term used in the presenting complaint or diagnosis fields.")
                 ),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$`mh-measure` == "mhdrugs") {
             showModal(modalDialog(# MH DRUGS MODAL
               title = "What is the data source?",
               p(strong("Data source: ePrescribed Messages.")),
               p("This section provides weekly information on the
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
               p("Data for 1 January to 31 December each year is presented as weekly data ending on Sundays.  The week from 1st January
                 may therefore not be a full seven days and will that week, or the following week, encompass public holidays.
                 Consequently, the number of prescribing days and measures of activity at the start of each year can be markedly reduced
                 compared to subsequent weeks.  A similar effect also occurs in the last two weeks of the year."),
               p("Prescriptions", week_standard),
               p("The ePrescribed messaging dataset is managed by Public Health Scotland (PHS)."),

                actionButton("toggle_mh_drug_codes", "Show / Hide Medicine Groups"),
                shinyjs::onclick("toggle_mh_drug_codes",
                                 shinyjs::toggle(id = "mh_drug_codes")),
                shinyjs::hidden(div(id="mh_drug_codes",
                  br(),
                  tags$b("Anxiety medicines"),
                  tags$ul(
                    tags$li("Diazepam (excluding rectal preparations)"),
                    tags$li("Lorazepam"),
                    tags$li("Oxazepam")
                    ),
                  tags$b("Insomnia medicines"),
                  tags$ul(
                    tags$li("Nitrazepam"),
                    tags$li("Loprazolam"),
                    tags$li("Lormetazepam"),
                    tags$li("Temazepam"),
                    tags$li("Zolpidem"),
                    tags$li("Zopiclone")
                  ),
                  tags$b("Depression medicines"),
                  tags$ul(
                    tags$li("Citalopram"),
                    tags$li("Fluoxetine"),
                    tags$li("Fluvoxamine"),
                    tags$li("Paroxetine"),
                    tags$li("Sertraline"),
                    tags$li("Duloxetine"),
                    tags$li("Venlafaxine")
                  ),
               size = "1",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             ))
            } else if (input$`mh-measure` == "ooh") {
              showModal(modalDialog(# MH OOH MODAL
                title = "What is the data source?",
                p("The Primary Care Out of Hours service provides urgent access to a nurse or doctor,
                   when needed at times outside normal general practice hours, such as evenings,
                   overnight or during the weekend. An appointment to the service is normally arranged
                   following contact with NHS 24. The recent trend data is shown by age group, sex and
                   broad deprivation category (SIMD). These figures include attendances of people aged 5 and over."),
                p("The charts provide a weekly summary of cases related to mental health in the recent past and
                   historical trends for comparison purposes."),
                p("The figures presented include mental health out of hours cases in areas relating to anxiety/stress, depression, self harm,
                   mental health issues caused by substance misuse, psychotic conditions and mental disorders. The figures exclude cases related to dementia and
                   learning disabilities."),
                p("The figures presented in this tool exclude cases within any of the COVID-19
                   hubs or assessment centres and relate only to cases concerning non-COVID
                   issues."),
                p("If required, more detailed analysis of the Primary Care Out of Hours service may
                   be available on request to ",
                  tags$a(href="mailto:phs.unscheduledcare@phs.scot", "phs.unscheduledcare@phs.scot",
                         class="externallink"), "."),
                p("General Practice Out of Hours service data is sourced from the",
                  tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=113",
                         "GP Out of Hours Dataset (OOH) (external website).",class="externallink"),
                  "The OOH dataset is managed by ",
                  tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/GP-Out-of-Hours-Services/",
                         "Public Health Scotland (PHS).", class="externallink")),
                p("Cases", week_standard),
                p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                p("Mental health related out of hours cases were identified using codes associated to the cases related to:"),
                tags$ul(
                  tags$li("Anxiety/Stress."),
                  tags$li("Depression."),
                  tags$li("Self-harm."),
                  tags$li("Psychotic conditions."),
                  tags$li("Alcohol and drug dependence."),
                  tags$li("Personality disorders and other mental health disorders.")
                ),
                size = "m",
                easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
                })

###############################################.
## Reactive datasets ----
###############################################.
mh_filt <- reactive({
  dataset <- switch(input$`mh-measure`,
                    "aye" = ae_mh,
                    "mhdrugs" = mentalhealth_drugs,
                    "ooh" = mh_ooh)
  
  dataset %>% filter(area_type == input$`mh-geotype` & 
                       area_name == input$`mh-geoname`)
  })
# Data with 3 week rolling averages for sex, dep and age charts
mh_aver <- reactive({
  mh_filt() %>%
    group_by(type, category) %>%
    mutate(count = round(rollmean(count, k = 3, fill = NA),1),
           count_average = round(rollmean(count_average, k = 3, fill = NA),1),
           variation = round(-1 * ((count_average - count)/count_average * 100), 1)) %>%
    ungroup
})

###############################################.
## Charts ----
###############################################.
# MH Overall chart
output$mh_overall <- renderPlotly({plot_overall_chart(mh_filt(), data_name = input$`mh-measure`, area = F)})
###############################################.
# MH Sex charts
output$mh_sex_var <- renderPlotly({plot_trend_chart(mh_aver(), pal_sex, c("sex", "all"), "variation",
                                                       data_name = input$`mh-measure`, tab = "mh", aver_week = T)})
output$mh_sex_tot <- renderPlotly({plot_trend_chart(mh_aver(), pal_sex, c("sex", "all"), "total", 
                                                    data_name = input$`mh-measure`, tab = "mh",  aver_week = T)})
###############################################.
# MH age charts
output$mh_age_var <- renderPlotly({
  plot_trend_chart(mh_aver() %>% filter(type == "age") %>% 
                     mutate(category = factor(category, levels = c("5 - 17", "18 - 44", "45 - 64", "65 and over"))), 
                     pal_age, c("age", "all"), "variation",
                     data_name = input$`mh-measure`, tab = "mh", aver_week = T)})
output$mh_age_tot <- renderPlotly({
  plot_trend_chart(mh_aver() %>% filter(type == "age") %>% 
                     mutate(category = factor(category, levels = c("5 - 17", "18 - 44", "45 - 64", "65 and over"))), 
                   pal_age, c("age", "all"), "total",
                   data_name = input$`mh-measure`, tab = "mh", aver_week = T)})

###############################################.
# MH Prescribing charts
output$mh_drugs_var <- renderPlotly({
  plot_trend_chart(mentalhealth_drugs, pal_med, split = "condition",
                   data_name = "mentalhealth_drugs", tab = "mh")})
output$mh_drugs_tot <- renderPlotly({
  plot_trend_chart(mentalhealth_drugs, pal_med, split = "condition", type = "total",
                   data_name = "mentalhealth_drugs", tab = "mh")})

###############################################.
# MH OOH charts
output$mh_dep_var <- renderPlotly({plot_trend_chart(dataset = mh_aver(), pal_chose = pal_depr, split = "dep",
                                                        type = "variation", data_name = input$`mh-measure`, tab = "mh", aver_week = T)})
output$mh_dep_tot <- renderPlotly({plot_trend_chart(mh_aver(), pal_depr, split = "dep", type = "total", data_name = input$`mh-measure`, tab = "mh",  aver_week = T)})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$mh_explorer <- renderUI({

  data_last_updated <- tagList(p("Last updated: 3 August 2022"))

  note_average <- p("Please note that to ease interpretation of these charts ",
                    "we are presenting 3-week rolling average figures.",
                    "Single-week figures can be obtained from the download button at the top of the page.")

  if (input$`mh-measure` == "mhdrugs") {
    tagList(# Prescribing - items dispensed
      h3(paste0("Number of patients starting a new treatment course for selected mental health medicines in ", input$`mh-geoname`)),
      fluidRow(column(6,
                      actionButton("mh-source-modal", "Data source and definitions",
                                   icon = icon('question-circle'))),
               column(6,data_last_updated)),
      plot_box("2020 to 2022 compared with 2018-2019 average", "mh_overall"),
      plot_cut_box(paste0("Percentage change in the number of patients starting a new treatment course for selected mental health medicines in ", input$`mh-geoname`,
                          " compared with average of the corresponding time in 2018 and 2019 by medicine groupings"), "mh_drugs_var",
                   paste0("Weekly number of patients starting a new treatment course for selected mental health medicines in ", input$`mh-geoname`, " by medicine groupings"), "mh_drugs_tot"))
  } else if (input$`mh-measure` == "aye") {
    tagList(#A&E attendances
      p("Important note: It is not possible to accurately report total attendances for specific conditions using the national A&E 
              dataset, due to the quality of the data available.  Diagnosis/reason for attendance can be recorded in a variety of ways, 
              including in free text fields - and not all NHS Boards submit this information.  The numbers presented in these dashboards 
              therefore give only a high level indication of differences over time and by age and sex, and should be interpreted with 
              caution.  Breakdowns by SIMD are not felt to be reliable, as they could be heavily skewed by the demographic profile of 
              the areas represented in the data available. PHS are planning work to improve consistency."),
      h3(paste0("Weekly mental health A&E attendances in ", input$`mh-geoname`)),
      fluidRow(column(6,
                      actionButton("mh-source-modal", "Data source and definitions",
                                   icon = icon('question-circle'))),
               column(6,data_last_updated)),
      plot_box("2020 to 2022 compared with 2018-2019 average", "mh_overall"),
    if (input$`mh-geoname` == "Scotland") {
      tagList(
        plot_cut_box("Percentage change in mental health A&E attendances compared with the corresponding
                     time in 2018-2019 by sex", "mh_sex_var",
                     "Weekly number of mental health A&E attendances by sex", "mh_sex_tot",
                     extra_content = note_average),
        plot_cut_box("Percentage change in mental health A&E attendances compared with the corresponding
                     time in 2018-2019 by age group", "mh_age_var",
                     "Weekly number of mental health A&E attendances by age group", "mh_age_tot",
                     extra_content = note_average)
      ) #taglist bracket from if statement

    }
    )  #taglist bracket from aye section

    } else if (input$`mh-measure` == "ooh") {
      tagList(#OOH attendances
        h3(paste0("Weekly mental health out of hours cases in ", input$`mh-geoname`)),
        fluidRow(column(6,
                        actionButton("mh-source-modal", "Data source and definitions",
                                     icon = icon('question-circle'))),
                 column(6,data_last_updated)),
        plot_box("2020 to 2022 compared with 2018-2019 average", "mh_overall"),
        if (input$`mh-geoname` == "Scotland") {
          tagList(
            plot_cut_box("Percentage change in mental health out of hours cases compared with the corresponding
                     time in 2018-2019 by sex", "mh_sex_var",
                         "Weekly number of mental health out of hours cases by sex", "mh_sex_tot",
                         extra_content = note_average),
            plot_cut_box("Percentage change in mental health out of hours cases compared with the corresponding
                     time in 2018-2019 by age group", "mh_age_var",
                         "Weekly number of mental health out of hours cases by age group", "mh_age_tot",
                         extra_content = note_average),
            plot_cut_box("Percentage change in mental health out of hours cases compared with the corresponding
                     time in 2018-2019 by SIMD quintile", "mh_dep_var",
                         "Weekly number of mental health out of hours cases by SIMD quintile", "mh_dep_tot",
                         extra_content = tagList(actionButton("btn_modal_simd_mh", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')),
                                                 note_average)
                         )
          ) #taglist bracket from if statement
          }
      ) #taglist bracket from ooh section
      }
})
###############################################.
## Data downloads ----
###############################################.

mh_down_data <- reactive({
  switch(
    input$`mh-measure`,
    "mhdrugs" = mentalhealth_drugs %>% filter(area_name == input$`mh-geoname` &
                                                area_type == input$`mh-geotype`),
    "aye" = ae_mh_filt(),
    "ooh" = mh_ooh_filt()) %>%
    rename(average_2018_2019 = count_average) %>% select(-type)

})

output$download_mentalhealth_data <- downloadHandler(
  filename ="mentalhealth_extract.csv",
  content = function(file) {
    write_csv(mh_down_data(),
              file) }
)

#END