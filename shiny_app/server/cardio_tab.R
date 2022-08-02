# Server side for cardiovascular tab..

###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("cardio")


# Adding 'observeEvent' to allow reactive 'area of interest' selction on cardio tab
observeEvent(input$`cardio-measure`, {
  x <- input$`cardio-measure`

  if (x == "cath") {
    cardio_choices = c("All", "Royal Infirmary of Edinburgh", "Golden Jubilee National Hospital")
    shinyjs::hide("diagnosis_select")
    hide("cardio-geoname")
    enable("cardio-geotype")
  }

  if (x == "aye") {
    cardio_choices = c("Scotland")
    shinyjs::hide("diagnosis_select")
    hide("cardio-geoname")
    disable("cardio-geotype")
  }

  if (x == "drug_presc") {
    cardio_choices = c("Scotland", "Health board", "HSC partnership")
    shinyjs::hide("diagnosis_select")
    shinyjs::show("cardio-geoname")
    enable("cardio-geotype")
  }

  if (x %in% c("sas_cardiac", "ooh_cardiac")) {
    cardio_choices = c("Scotland", "Health board")
    shinyjs::hide("diagnosis_select")
    shinyjs::show("cardio-geoname")
    enable("cardio-geotype")
  }

  if (x == "cardio_admissions") {
    cardio_choices = c("Scotland", "Health board")
    shinyjs::show("diagnosis_select")
    updateSelectInput(session, "diagnosis_select", label = "Step 3. Select diagnosis",
                      choices = c("Heart Attack","Heart Failure","Stroke"), selected = "Heart Attack")
    shinyjs::show("cardio-geoname")
    enable("cardio-geotype")
  }
  
  if (x == "cardio_deaths") {
    cardio_choices = c("Scotland", "Health board")
    shinyjs::show("diagnosis_select")
    updateSelectInput(session, "diagnosis_select", label = "Step 3. Select diagnosis",
                choices = c("Heart Attack","Heart Failure","Stroke"), selected = "Heart Attack")
    shinyjs::show("cardio-geoname")
    enable("cardio-geotype")
  }

  updateSelectInput(session, "cardio-geotype",
                    choices = cardio_choices,
                    selected = cardio_choices[1]
  )
})

###############################################.
## Modals ----
###############################################.
week_standard <- " are allocated to weeks based on the ISO8601 standard. Following this standard
the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use
the 2018-2019 average of week 52 value as a comparator for 2020’s week 53.”"

# Link action button click to modal launch
observeEvent(input$`cardio-source-modal`,

             if (input$`cardio-measure` == "aye") {
               showModal(modalDialog(# Cardio A&E MODAL
                 title = "What is the data source?",
                 p("This tool provides a weekly summary of people attending A&E departments (Emergency Departments)
                    in the recent past, along with historical activity for
                   comparison purposes. The recent trend data is shown by age group and sex. This data only include Emergency Department
                   attendances and do not include minor injury units and other small hospitals and
                   health centres in rural areas that carry out emergency department related activity,
                   for more information on what sites are included please see this ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/Hospital-Site-List/",
                          "hospital list.",  target="_blank")),
                 p("Additional information relating to the overall A&E activity is available from the ",
                   tags$a(href="https://publichealthscotland.scot/publications/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics",
                          "NHS Performs - weekly update of emergency department activity and waiting time statistics.",
                           target="_blank")),
                 p("Attendances to A&E departments data sourced from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=3",
                          "Accident and Emergency Datamart (A&E2) (external website).", target="_blank"),
                   "The A&E2 dataset is managed by ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/",
                          "Public Health Scotland (PHS).",  target="_blank")),
                 p("Attendances", week_standard),
                 p(tags$em("Important note: It is not possible to accurately report total attendances for specific conditions 
                           using the national A&E dataset, due to the quality of the data available.  Diagnosis/reason for 
                           attendance can be recorded in a variety of ways, including in free text fields - and not all NHS 
                           Boards submit this information.  The numbers presented in these dashboards therefore give only a 
                           high level indication of differences over time and by age and sex, and should be interpreted with 
                           caution.  Breakdowns by SIMD are not felt to be reliable, as they could be heavily skewed by the 
                           demographic profile of the areas represented in the data available. PHS are planning work to improve 
                           consistency.")),
                 p("The table below shows the ICD-10 codes that were considered for the cardiovascular A&E data subset,
                   where this information was available."),
                 actionButton("toggleCodeTable", "Show / Hide Table"),
                 shinyjs::onclick("toggleCodeTable",
                                  shinyjs::toggle(id = "CodeTable")),
                 shinyjs::hidden(div(id = "CodeTable", br(), DT::dataTableOutput("ae_cardio_codes_tbl"))),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$`cardio-measure` == "drug_presc") {
               showModal(modalDialog(#Prescribing - Cardio Drugs
                 title = "What is the data source?",
                 p("This section of the PHS Covid-19 wider impacts tool provides weekly information on the
                   number of prescriptions for cardiovascular drugs issued. The data ranges from the start of 2020
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
                 p("Data for 1 January to 31 December each year is presented as weekly data ending on Sundays.
                   The week from 1st January may therefore not be a full seven days and will that week,
                   or the following week, encompass public holidays. Consequently, the number of prescribing
                   days and measures of activity at the start of each year can be markedly reduced compared
                   to subsequent weeks. A similar effect also occurs in the last two weeks of the year."),
                 p("Prescriptions", week_standard),
                 p("The ePrescribed messaging dataset is managed by Public Health Scotland (PHS)."),
                 tags$hr(),
                 actionButton("toggleBNFCodes", "Show / Hide BNF Codes"),
                 shinyjs::onclick("toggleBNFCodes",
                                  shinyjs::toggle(id = "BNFCodes")),
                 shinyjs::hidden(div(id="BNFCodes",
                     br(),
                     HTML({
                       "
                       <table style='width:100%'>
                       <tr>
                       <th colspan='2'>Medicines Groups</th>
                       <th>BNF Legacy</th>
                       </tr>
                       <tr>
                       <th colspan='2'>&nbsp;&nbsp;Antihypertensive, anti-anginal, anti-arrhythmic and heart failure drugs</th>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Positive inotrpic drugs</td>
                       <td>0201</td>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Diuretics</td>
                       <td>0202</td>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Anti-arrhythmic drugs</td>
                       <td>0203</td>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Beta-adrenoreceptor blocking drugs</td>
                       <td>0204</td>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Antihypertensives</td>
                       <td>0205</td>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Nitrates</td>
                       <td>020601</td>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Calcium channel blockers</td>
                       <td>020602</td>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Peripheral vasodilator and related drugs</td>
                       <td>020604</td>
                       </tr>
                       <tr>
                       <td colspan='2'>&nbsp;&nbsp;&nbsp;&nbsp;Sympathomimetics</td>
                       <td>0207</td>
                       </tr>
                       <tr>
                       <th colspan='2'>&nbsp;&nbsp;Antiplatelet drugs</th>
                       <td>0209</td>
                       </tr>
                       <tr>
                       <th colspan='2'>&nbsp;&nbsp;Oral anticoagulants</th>
                       <td>020802</td>
                       </tr>
                       <tr>
                       <th colspan='2'>&nbsp;&nbsp;Lipid-lowering drugs</th>
                       <td>0212</td>
                       </tr>
                       </table>
                       "
                     }))),
                 size = "l",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$`cardio-measure` == "cath") {
               showModal(modalDialog(#ALL  MODAL
                 title = "What is the data source?",
                 p("This tool provides an overview on how procedures in the field of
                    cardiovascular medicine have changed during the COVID-19 pandemic.
                   These include procedures such as inserting pacemakers or monitoring heart rhythms,
                   investigating possible narrowing of the arteries supplying the heart (angiography)
                   and carrying out procedures to treat any narrowing of these arteries (percutaneous coronary intervention)"),
                 p("These procedures are generally carried out in catheterisation labs.
                   There are four catheterisation labs in Scotland: Golden Jubilee Hospital, Royal Infirmary of Edinburgh,
                   Aberdeen Royal Infirmary and Ninewells Hospital in Dundee.
                   Information on the numbers of procedures taking place in these catheterisation labs has
                   been supplied to PHS directly by a number of these facilities as part of a bespoke data request."),
                 p("This data shows the number of procedures carried out by two of the
                   biggest catheterisation labs in Scotland, which provide services to the majority of the population
                  living in the Central Belt and south of Scotland."),
                 p(" We show the data split by three types of procedures: "),
                 tags$ul(
                   tags$li("Angiographies - These diagnostic procedures allow clinicians to investigate
                          the condition of the arteries of the heart. All patients who are taken to a
                          catheterisation lab will go through angiography before a narrowed artery is treated.
                          Within this tool, the number of angiographies carried out is considered
                          the total number of cases for each lab. Many angiographies
                          are carried out as planned (elective) procedures."),
                   tags$li("Percutaneous coronary intervention - Procedure used to treat
                          the narrowing of the heart arteries. In a significant portion of cases this is an urgent procedure
                          which is used when patients are suffering a heart attack."),
                   tags$li("Devices - This section includes procedures such as pacemakers (used to treat rhythm
                           problems of the heart), loop recorders (used to investigate treat rhythm problems
                           of the heart) and implantable cardiac defibrillators (ICDs).
                           The pacemakers include cardiac resynchronisation therapy (CRT) devices.
                           Fitting of cardiac devices may also be carried out in other environments
                           so these figures do not represent the total volume of cardiac devices fitted.")
                 ),
                 p("Note that during the COVID-19 lockdown period the Golden Jubilee National Hospital
                   extended their catheterisation lab catchment area to cover Ayrshire & Arran, Dumfries & Galloway
                   and southeast Glasgow, areas which previously would have gone to Hairmyres Hospital.
                   In addition, during 2019 increased catheterisation lab activity was seen at the Golden Jubilee National
                    Hospital activity due to the presence of the temporary mobile lab which was not present during other
                   time periods."),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$`cardio-measure` == "ooh_cardiac"){
               showModal(modalDialog(# OUT OF HOURS cases  MODAL
                 title = "What is the data source?",
                 p("The Primary Care Out of Hours service provides urgent access to a nurse or doctor,
                   when needed at times outside normal general practice hours, such as evenings,
                   overnight or during the weekend. An appointment to the service is normally arranged
                   following contact with NHS 24. The recent trend data is shown by age group, sex and
                   broad deprivation category (SIMD)."),
                 p("A GP out of hours case represents one patient contact with the service. Please note that the same person could
                    become a case on more than one occasion."),
                 p("The charts provide a weekly summary of chest pain cases in the recent past and
                   historical trends for comparison purposes. chest pain cases are identified using the following conditions:"),
                tags$ul(
                  tags$li("pleuritic pain, atypical chest pain, ischaemic heart disease, acute myocardial infarction, angina pectoris, ischaemic chest pain, chest pain.")),
                 p("The figures presented in this tool exclude cases within any of the COVID-19
                   hubs or assessment centres and relate only to cases concerning non-COVID
                   issues. "),
                p("Cases", week_standard),
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
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }else if (input$`cardio-measure` == "sas_cardiac") { #NHS24 CALLS MODAL
               showModal(modalDialog(
                 title = "What is the data source?",
                 p("The charts provide a weekly summary of Scottish Ambulance Service emergency calls attended with historical trends for comparison purposes.
                   The recent trend data is shown by age group, sex and broad deprivation category (SIMD). The figures presented in this tool
                   relate to incidents for chest pain and heart problems concerning both COVID-19 and non-COVID issues. Please note that the source of this data is the Unscheduled Care
                   Datamart and represents a sub-set of the total Scottish Ambulance service activity. Figures include emergencies, where a vehicle arrived
                   at the scene of the incident, and excludes both data from resources which were cleared as ‘dealt with by another vehicle’ and air ambulance data."),
                 p("SAS currently publish weekly unscheduled care operational statistics at the following ", 
                tags$a(href="https://www.scottishambulance.com/publications/unscheduled-care-operational-statistics/", 
                       "Unscheduled Care Operational Statistics (external website)", target="_blank"), ". This details unscheduled care demand, 
                   response times and turnaround times. Please note that the data published by SAS is sourced from a 
                   different operational system than that used for the PHS reporting. This means that the data published 
                   by SAS will at times be slightly different to that reported by PHS source. The data published by PHS 
                   is less timely than the data used for the SAS publication, however allows for data to be linked in order 
                   to gain further insight into patient flow through unscheduled care."),
                 p(tags$b("Chest Pain or Heart Problems defined as below:")),
                 p("Chest Pain or Heart Problems defined as below:
                   Chest Pain (non-traumatic), Chest Pain with Abnormal Breathing,
                   Chest Pain and Breathing Normally (age > 35 years), Chest Pain with Nausea or Vomitting,
                   Difficulty Speaking between Breaths, Changing Colour, Clammy or Cold Sweats with Chest Pains,
                   Heart Attack or Angina History with Chest Pains, Heart Problem with Abnormal Breathing,
                   Heart Problems with Chest Pain/discomfort (age > 35 years), Heart Problems with Cardiac History,
                   Heart Problems/Difficulty Speaking between Breaths, Heart Problems and Changing Colour,
                   Heart Problems and Clammy or Cold Sweats, Just Resuscitated and/or Defibrillated"),
                 p("Activity", week_standard),
                 p("If required, more detailed analysis of SAS activity may be available on request to ",
                   tags$a(href="mailto:phs.unscheduledcare@phs.scot", "phs.unscheduledcare@phs.scot",
                          class="externallink"), "."),
                 p("The SAS dataset is managed by ",
                   tags$a(href="https://publichealthscotland.scot/",
                          "Public Health Scotland", class="externallink"), "and ",
                   tags$a(href="https://www.scottishambulance.com/",
                          "Scottish Ambulance Service (external website)", class="externallink"), ".",
                   "This analysis is drawn from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=111", 
                          "Unscheduled Care Datamart (UCD) (external website).",class="externallink")
                 ),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }else if (input$`cardio-measure` == "cardio_admissions") { #CARDIAC DISCHARGES MODAL
               showModal(modalDialog(
                 title = "What is the data source?",
                 tags$b("Cardiovascular admissions"),
                 p("These data provide a quarterly summary of the number of hospital episodes as a result of an
                    cardiovascular condition since Jan 2020, with data from 2018-2019 for comparison purposes.
                    The recent trend data is shown by age group, sex, diagnosis, admission type and deprivation category (SIMD)."),
                 p("Please note that the stroke figures include diagnosis subarachnoid haemorrhage."),
                 p("The source of data is the Scottish Morbidity Record 01 (SMR01) database, which holds information on
                    admissions from non-obstetric and non-psychiatric acute hospitals in Scotland."),
                 p("Further information relating to the cardiovascular hospital activity is available from the ",
                   tags$a(href="https://beta.isdscotland.org/topics/heart-disease-and-stroke/",
                          "Scottish Heart disease and stroke publications.",
                          target="_blank")),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 p("Please note only the total by sex is shown for island boards due to small numbers."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))               
             }else if (input$`cardio-measure` == "cardio_deaths") { #CARDIAC DEATHS MODAL
               showModal(modalDialog(
                 title = "What is the data source?",
                 p("The analyses shown here are derived from weekly deaths registration data, and show recent trends in deaths (2020),
                   whether COVID or non-COVID related, and historic trends for comparison (five-year average, 2015-2019).
                   The recent trend data are shown by age group and sex, and the national data are also shown by broad area
                   deprivation category (Scottish Index of Multiple Deprivation, SIMD).
                   Volatility of the trends will be observed in some charts due to small counts.
                   SIMD trends are not shown for Health Boards or Health
                   and Social Care Partnerships because of the small numbers involved and the possibility for misinterpretation."),
                 p("The deaths data are derived from the National Records of Scotland (NRS) ",
                   tags$a(href="https://www.nrscotland.gov.uk/covid19stats",
                          "weekly deaths (external website)",  target="_blank"), " dataset. Deaths related to COVID-19 are included in totals.
                   Data are provisional and subject to revision."),
                 p("The figures are based on the date a death was registered rather than the date the death occurred. When someone dies,
                   their family (or a representative) have to make an appointment with a registrar to register the death.
                   Legally this must be done within 8 days, although in practice there is, on average, a 3 day gap between a
                   death occurring and being registered. More information on days between occurrence and registration can be be found on the ",
                   tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-background-information/births-and-deaths-days-until-registration",
                          "NRS website (external website)", target="_blank"),"."),
                 p("The figures are reported by week, with each week running from Monday to Sunday (the ISO8601 standard week).
                   Moveable public holidays, when registration offices are closed, affect the number of registrations made in the
                   published weeks and in the corresponding weeks in previous years."),
                 p("Figures include non-residents.  Deaths are allocated to area based on the usual residence of the deceased.
                   If the deceased was not a Scottish resident, the death is allocated to the area where the death occurred."),
                 p("Deaths are allocated to weeks . The last week of 2020 is week 53 according to this standard.
          Between 2015 and 2019 only 2015 also had a week 53, so the ‘historic average’ figure that the 2020 deaths are compared with in
          this week is the 2015 count, rather than the 2015-19 average."),
                 p("The weekly deaths dataset is managed by ",
                   tags$a(href= "https://www.nrscotland.gov.uk/",
                          "National Records of Scotland (NRS) (external website).",  target="_blank")),
                 p("For more information on deaths and health inequalities during the pandemic, please read ",
                   tags$a(href = "https://publichealthscotland.scot/publications/covid-19-weekly-excess-deaths/covid-19-weekly-excess-deaths-health-inequalities-briefing/",
                          "this report.",  target="_blank")),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))          
             }

)

# Link action button click to modal launch
observeEvent(input$btn_cath_modal,
               showModal(modalDialog(#ALL  MODAL
                 title = "What these interventions involve?",
                 tags$ul(
                   tags$li("Angiographies - These diagnostic procedures allow clinicians to investigate
                          the condition of the arteries of the heart. All patients who are taken to a
                          catheterisation lab will go through angiography before a narrowed artery is treated.
                          Within this tool, the number of angiographies carried out is considered
                          the total number of cases for each lab. Many angiographies
                          are carried out as planned (elective) procedures."),
                   tags$li("Percutaneous coronary intervention - Procedure used to treat
                          the narrowing of the heart arteries. In a significant portion of cases this is an urgent procedure
                          which is used when patients are suffering a heart attack."),
                   tags$li("Devices - This section includes procedures such as pacemakers (used to treat rhythm
                           problems of the heart), loop recorders (used to investigate treat rhythm problems
                           of the heart) and implantable cardiac defibrillators (ICDs).
                           The pacemakers include cardiac resynchronisation therapy (CRT) devices.
                           Fitting of cardiac devices may also be carried out in other environments
                           so these figures do not represent the total volume of cardiac devices fitted.")
                 ),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
)

# Rendering A&E Cardio Codes table here for inclusion to modal above
output$ae_cardio_codes_tbl <- DT::renderDataTable(
  ae_cardio_codes
)

# Including 'observeEvent' here so that SIMD modal can be called from A&E Cardio section
observeEvent(input$btn_modal_simd_cardio, simd_modal())


###############################################.
## Reactive datasets ----
###############################################.
cath_lab_chosen <- reactive({
  cath_lab %>% filter(lab == input$`cardio-geotype`) %>% droplevels()
})

cath_lab_over <- reactive({
  cath_lab_chosen() %>% filter(groups == "Angiography") %>% droplevels()
})

cath_lab_adm <- reactive({
  cath_lab_chosen() %>% filter(type == "adm" & groups == "Percutaneous coronary intervention") %>% droplevels()
})

cath_lab_type <- reactive({
  cath_lab_chosen() %>% filter(category == "All") %>%
    select(-category) %>% rename(category = groups) %>% droplevels()
})

###############################################.
## Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$cardio_explorer <- renderUI({

  data_last_updated <- tagList(p("Last updated: 3 August 2022"))

  # Charts and rest of UI
  if (input$`cardio-measure` == "cath") {
    lab_chosen <- case_when(input$`cardio-geotype` == "All" ~ "Royal Infirmary of Edinburgh and Golden Jubilee National Hospital",
                            TRUE ~ paste0(input$`cardio-geotype`))

      tagList( # Cath labs
        p("At present charts include data from only two of the four Scottish catheterisation labs.
          Data from the remaining two centres will be added as it becomes available."),
        h3(paste0("Weekly visits to the cardiac catheterisation labs at the ", lab_chosen)),
        sourcemodal_ui("cardio"),
        plot_box("2020 to 2022 compared with 2018-2019 average", "cath_overall"),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by type of intervention", "cath_type_var",
                     "Weekly number of cases by type of intervention", "cath_type_tot",
                     extra_content =  fluidRow(actionButton("btn_cath_modal", "What these interventions involve?",
                                                                   icon = icon('question-circle')))),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by sex", "cath_sex_var",
                     "Weekly number of cases by sex", "cath_sex_tot"),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by age group", "cath_age_var",
                     "Weekly number of cases by age group", "cath_age_tot")
      )
    } else if (input$`cardio-measure` == "aye") {
      tagList(# A&E attendances (cardiovascular only)
        tags$em("Please note that, due to limitations in diagnosis recording in the A&E datamart, the data are
                 incomplete for a number of NHS Boards. Thus, the figures reported for cardiovascular-related
                 attendances offer only a very approximate indication of attendances. In addition, due to a technical
                 issue in the A&E diagnosis recording for the weeks ending in 2nd and 9th August
                 figures for NHS Greater Glasgow & Clyde are incomplete for that time period.
                 Moreover, some NHS Boards have moved to a new recording standard which
                 has not been fully consolidated in the A&E datamart as yet. As a result, figures for 2020,
                 even prior to the introduction of lockdown measures, appear somewhat lower when compared to
                 previous years."),
        h3("Weekly cardiovascular A&E attendances in Scotland"),
        fluidRow(column(6, sourcemodal_ui("cardio")),
        column(6,data_last_updated)),
        plot_box("2020 to 2022 compared with 2018-2019 average", "ae_cardio_overall"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances in Scotland compared with the corresponding
                     time in 2018-2019 by age group", "ae_cardio_age_var",
                     "Weekly number of cardiovascular A&E attendances in Scotland by age group", "ae_cardio_age_tot")
        # plot_cut_box("Percentage change in cardiovascular A&E attendances in Scotland compared with the corresponding
        #              time in 2018-2019 by SIMD quintile", "ae_cardio_dep_var",
        #              "Weekly number of cardiovascular A&E attendances in Scotland by SIMD quintile", "ae_cardio_dep_tot",
        #              extra_content = actionButton("btn_modal_simd_cardio", "What is SIMD and deprivation?",
        #                                           icon = icon('question-circle')))
      )
    } else if (input$`cardio-measure` == "drug_presc") {
      tagList(# Prescribing - items dispensed
        tags$em("Please note that an improved drugs mapping procedure was implemented for the August 2021 update,
                which increased the number of prescriptions compared to what was previously published. The mean increase
                across all time periods and areas was 4%, and the majority of individual increases were less than 10%."),
        h3(paste0("Weekly number of cardiovascular medicines prescribed in ", input$`cardio-geoname`)),
        fluidRow(column(6, sourcemodal_ui("cardio")),
                 column(6,data_last_updated)),
        plot_box("2020 to 2022 compared with 2018-2019 average", "prescribing_all"),
        plot_cut_box(paste0("Percentage change in cardiovascular medicines prescribed in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by medicine groupings"), "cardio_drugs_var",
                     paste0("Weekly number of cardiovascular medicines prescribed in ", input$`cardio-geoname`, " by medicine groupings"), "cardio_drugs_tot")
      )


     } else if (input$`cardio-measure` == "ooh_cardiac") {
        tagList(# OOH Attendances
          tags$b("The numbers of cases reported from July 2021 onwards are not comparable to
                    those in earlier weeks."),
          p("The clinical codes used to categorise out of hours diagnoses changed in that month,
             affecting the number of cases that are categorised as chest pain."),
          h3(paste0("Weekly chest pain cases in out of hours services in ", input$`cardio-geoname`)),
          fluidRow(column(6, sourcemodal_ui("cardio")),
                   column(6,data_last_updated)),
          plot_box("2020 to 2022 compared with 2018-2019 average", "ooh_cardio_all"),
          plot_cut_box(paste0("Percentage change in chest pain cases in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by sex"), "ooh_cardio_sex_var",
                       paste0("Weekly number of chest pain cases in ", input$`cardio-geoname`, " by sex"), "ooh_cardio_sex_tot"),
          plot_cut_box(paste0("Percentage change in chest pain cases in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by age group"), "ooh_cardio_age_var",
                       paste0("Weekly number of chest pain cases in ", input$`cardio-geoname`, " by age group"), "ooh_cardio_age_tot"),
          plot_cut_box(paste0("Percentage change in chest pain cases in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by SIMD quintile"), "ooh_cardio_depr_var",
                       paste0("Weekly number of chest pain cases in ", input$`cardio-geoname`, " by SIMD quintile"), "ooh_cardio_depr_tot",
                       extra_content = actionButton("btn_modal_simd_cardio", "What is SIMD and deprivation?",
                                                    icon = icon('question-circle')))
        )
     } else if (input$`cardio-measure` == "sas_cardiac") {
       tagList(# SAS incidents
         tags$em(p("SAS currently publish weekly unscheduled care operational statistics at the following ", 
                tags$a(href="https://www.scottishambulance.com/publications/unscheduled-care-operational-statistics/", 
                       "Unscheduled Care Operational Statistics (external website)", target="_blank"), ". The data published by SAS is sourced from a 
                        different operational system than that used for the PHS reporting. This means that the data published 
                        by SAS will at times be slightly different to that reported by PHS source.")),
         h3(paste0("Weekly attended cardiovascular incidents by Scottish Ambulance Service in ", input$`cardio-geoname`)),
         fluidRow(column(6, sourcemodal_ui("cardio")),
                  column(6,data_last_updated)),
         plot_box("2020 to 2022 compared with 2018-2019 average", "sas_cardio_all"),
         plot_cut_box(paste0("Percentage change in cardiovascular incidents in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by sex"), "sas_cardio_sex_var",
                      paste0("Weekly number of cardiovascular incidents in ", input$`cardio-geoname`, " by sex"), "sas_cardio_sex_tot"),
         plot_cut_box(paste0("Percentage change in cardiovascular incidents ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by age group"), "sas_cardio_age_var",
                      paste0("Weekly number of cardiovascular incidents in ", input$`cardio-geoname`, " by age group"), "sas_cardio_age_tot"),
         plot_cut_box(paste0("Percentage change in cardiovascular incidents in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by SIMD quintile"), "sas_cardio_depr_var",
                      paste0("Weekly number of cardiovascular incidents in ", input$`cardio-geoname`, " by SIMD quintile"), "sas_cardio_depr_tot",
                      extra_content = actionButton("btn_modal_simd_cardio", "What is SIMD and deprivation?",
                                                   icon = icon('question-circle')))
       )
       # Removed input$type_adm_select and added text emergency "Percentage change in ", input$diagnosis_select, " ", input$type_adm_select, " admissions in "
     } else if (input$`cardio-measure` == "cardio_admissions") {
       tagList(# cardio_admissions
         h3(paste0("Quarterly ", input$diagnosis_select, " ", "Emergency admissions in ", input$`cardio-geoname`)),
         fluidRow(column(6, sourcemodal_ui("cardio")),
                  column(6,data_last_updated)),
         plot_box("2020 to 2022 compared with 2018-2019 average", "cardio_admissions_all"),
         plot_cut_box(paste0("Percentage change in ", input$diagnosis_select, " ", "Emergency admissions in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by sex"), "cardio_admissions_sex_var",
                      paste0("Quarterly number of ", input$diagnosis_select, " ", "Emergency admissions in ", input$`cardio-geoname`, " by sex"), "cardio_admissions_sex_tot"),
         plot_cut_box(paste0("Percentage change in ", input$diagnosis_select, " ", "Emergency admissions ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by age group"), "cardio_admissions_age_var",
                      paste0("Quarterly number of ", input$diagnosis_select, " ", "Emergency admissions in ", input$`cardio-geoname`, " by age group"), "cardio_admissions_age_tot"),
         plot_cut_box(paste0("Percentage change in ", input$diagnosis_select, " ", "Emergency admissions in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by SIMD quintile"), "cardio_admissions_depr_var",
                      paste0("Quarterly number of ", input$diagnosis_select, " ", "Emergency admissions in ", input$`cardio-geoname`, " by SIMD quintile"), "cardio_admissions_depr_tot",
                      extra_content = actionButton("btn_modal_simd_cardio", "What is SIMD and deprivation?",
                                                   icon = icon('question-circle')))
       )
     } else if (input$`cardio-measure` == "cardio_deaths") {
       tagList(# cardio_deaths
         h3(paste0("Quarterly ", input$diagnosis_select, " ", "deaths in ", input$`cardio-geoname`)),
         fluidRow(column(6, sourcemodal_ui("cardio")),
                  column(6,data_last_updated)),
         plot_box("2020 to 2022 compared with 2018-2019 average", "cardio_deaths_all"),
         plot_cut_box(paste0("Percentage change in ", input$diagnosis_select, " ", "deaths in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by sex (Only totals are shown at board level due to small numbers)"), "cardio_deaths_sex_var",
                      paste0("Quarterly number of ", input$diagnosis_select, " ", "deaths in ", input$`cardio-geoname`, " by sex (Only totals are shown at board level due to small numbers)"), "cardio_deaths_sex_tot"),
         plot_cut_box(paste0("Percentage change in ", input$diagnosis_select, " ", "deaths ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by age group"), "cardio_deaths_age_var",
                      paste0("Quarterly number of ", input$diagnosis_select, " ", "deaths in ", input$`cardio-geoname`, " by age group"), "cardio_deaths_age_tot"),
         plot_cut_box(paste0("Percentage change in ", input$diagnosis_select, " ", "deaths in ", input$`cardio-geoname`, " compared with the corresponding
                     time in 2018-2019 by SIMD quintile"), "cardio_deaths_depr_var",
                      paste0("Quarterly number of ", input$diagnosis_select, " ", " deaths in ", input$`cardio-geoname`, " by SIMD quintile"), "cardio_deaths_depr_tot",
                      extra_content = actionButton("btn_modal_simd_cardio", "What is SIMD and deprivation?",
                                                   icon = icon('question-circle')))
       )       
    }
})


###############################################.
## Reactive datasets ----
###############################################.

# Cardio deaths area/diagnosis reactive filter
# remove  & type_admission == input$type_adm_select
cardio_disch_filter <- reactive({
  cardio_admissions %>% 
    filter(area_name == input$`cardio-geoname` &
             diagnosis == input$diagnosis_select)
  
})

# Cardio deaths area/diagnosis reactive filter
cardio_dth_filter <- reactive({
  cardio_deaths %>% 
    filter(area_name == input$`cardio-geoname` &
             diagnosis == input$diagnosis_select)
  
})


###############################################.
## Charts ----
###############################################.
#Cath lab charts
output$cath_overall <- renderPlotly({plot_overall_chart(cath_lab_over(), data_name = "cath", area = F)})
output$cath_sex_var <- renderPlotly({plot_trend_chart(cath_lab_over(), pal_sex, split = "sex", tab = "cardio")})
output$cath_sex_tot <- renderPlotly({plot_trend_chart(cath_lab_over(),
                   pal_sex, type = "total", data_name = "cath", split = "sex", tab = "cardio")})
output$cath_age_var <- renderPlotly({plot_trend_chart(cath_lab_over() %>% filter(type == "age"), pal_2ages)})
output$cath_age_tot <- renderPlotly({plot_trend_chart(cath_lab_over() %>% filter(type == "age"),
                   pal_2ages, type = "total", data_name = "cath")})

output$cath_type_var <- renderPlotly({plot_trend_chart(cath_lab_type(), pal_sex)})
output$cath_type_tot <- renderPlotly({plot_trend_chart(cath_lab_type(),
                   pal_sex, type = "total", data_name = "cath")})

###############################################.
# A&E Cardio charts
output$ae_cardio_overall <- renderPlotly({plot_overall_chart(ae_cardio, data_name = "aye", area = "All")})
output$ae_cardio_age_var <- renderPlotly({plot_trend_chart(ae_cardio, pal_sex, c("age", "all"), data_name = "aye",tab = "cardio")})
output$ae_cardio_age_tot <- renderPlotly({plot_trend_chart(ae_cardio, pal_sex, c("age", "all"), "total", "aye", tab = "cardio")})
output$ae_cardio_dep_var <- renderPlotly({plot_trend_chart(dataset = ae_cardio, pal_chose = pal_depr, split = "dep", type = "variation", data_name = "aye", tab = "cardio")})
output$ae_cardio_dep_tot <- renderPlotly({plot_trend_chart(ae_cardio, pal_depr, split = "dep", type = "total", data_name = "aye", tab = "cardio")})

###############################################.
# Prescribing charts
output$prescribing_all <- renderPlotly({plot_overall_chart(cardio_drugs %>% filter(area_name == input$`cardio-geoname`),
                                                           data_name = "drug_presc", area = "All")})
output$cardio_drugs_var <- renderPlotly({plot_trend_chart(cardio_drugs, pal_med, c("condition"), data_name = "drug_presc", tab = "cardio")})
output$cardio_drugs_tot <- renderPlotly({plot_trend_chart(cardio_drugs, pal_med, c("condition"), "total", data_name = "drug_presc", tab = "cardio")})
###############################################.

###############################################.
# OOH charts

# Adds line and annotation about change in coding
add_cardio_ooh_coding_line = function(fig){

  hovertext =
"The clinical codes used to categorise OOH diagnoses
changed in July 2021. This affected the number of
cases that are categorised as cardiovascular; numbers
after this date are not comparable to those before it."

  add_vline(fig, x = ymd(20210701),
            text = "Change in coding<br>Data not comparable",
            margin = list(t = 35),
            hovertext = hovertext)
}


output$ooh_cardio_all <- renderPlotly({plot_overall_chart(ooh_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                          data_name = "ooh_cardiac", area = "All",
                                                          fix_x_range = TRUE) %>%
                                        add_cardio_ooh_coding_line()})
output$ooh_cardio_sex_var <- renderPlotly({plot_trend_chart(ooh_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                            pal_sex, split = "sex", type = "variation",
                                                            data_name = "ooh_cardiac", tab = "cardio",
                                                            fix_x_range = TRUE) %>%
                                            add_cardio_ooh_coding_line()})
output$ooh_cardio_sex_tot <- renderPlotly({plot_trend_chart(ooh_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                            pal_sex, split = "sex", type = "total",
                                                            data_name = "ooh_cardiac", tab = "cardio",
                                                            fix_x_range = TRUE) %>%
                                            add_cardio_ooh_coding_line()})
output$ooh_cardio_age_var <- renderPlotly({plot_trend_chart(ooh_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                            pal_age, split = "age", type = "variation",
                                                            data_name = "ooh_cardiac", tab = "cardio",
                                                            fix_x_range = TRUE) %>%
                                            add_cardio_ooh_coding_line()})
output$ooh_cardio_age_tot <- renderPlotly({plot_trend_chart(ooh_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                            pal_age, split = "age", type = "total",
                                                            data_name = "ooh_cardiac", tab = "cardio",
                                                            fix_x_range = TRUE) %>%
                                            add_cardio_ooh_coding_line()})
output$ooh_cardio_depr_var <- renderPlotly({plot_trend_chart(ooh_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                            pal_depr, split = "dep", type = "variation",
                                                            data_name = "ooh_cardiac", tab = "cardio",
                                                            fix_x_range = TRUE) %>%
                                            add_cardio_ooh_coding_line()})
output$ooh_cardio_depr_tot <- renderPlotly({plot_trend_chart(ooh_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                             pal_depr, split = "dep", type = "total",
                                                             data_name = "ooh_cardiac", tab = "cardio",
                                                             fix_x_range = TRUE) %>%
                                            add_cardio_ooh_coding_line()})
###############################################.


###############################################.
# SAS charts
output$sas_cardio_all <- renderPlotly({plot_overall_chart(sas_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                            data_name = "sas_cardiac", area = "All")})
output$sas_cardio_sex_var <- renderPlotly({plot_trend_chart(sas_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                              pal_sex, split = "sex", type = "variation", data_name = "sas_cardiac", tab = "cardio")})
output$sas_cardio_sex_tot <- renderPlotly({plot_trend_chart(sas_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                              pal_sex, split = "sex", type = "total", data_name = "sas_cardiac", tab = "cardio")})
output$sas_cardio_age_var <- renderPlotly({plot_trend_chart(sas_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                              pal_age, split = "age", type = "variation", data_name = "sas_cardiac", tab = "cardio")})
output$sas_cardio_age_tot <- renderPlotly({plot_trend_chart(sas_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                              pal_age, split = "age", type = "total", data_name = "sas_cardiac", tab = "cardio")})
output$sas_cardio_depr_var <- renderPlotly({plot_trend_chart(sas_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                               pal_depr, split = "dep", type = "variation",data_name = "sas_cardiac", tab = "cardio")})
output$sas_cardio_depr_tot <- renderPlotly({plot_trend_chart(sas_cardiac %>% filter(area_name == input$`cardio-geoname`),
                                                               pal_depr, split = "dep", type = "total",data_name = "sas_cardiac", tab = "cardio")})
###############################################.


###############################################.
# Admissions charts
output$cardio_admissions_all <- renderPlotly({plot_overall_chart(cardio_disch_filter(),
                                                          data_name = "cardio_admissions", area = "All", period = "quarterly")})
output$cardio_admissions_sex_var <- renderPlotly({plot_trend_chart(cardio_disch_filter(),
                                                            pal_sex, split = "sex", type = "variation", data_name = "cardio_admissions", tab = "cardio", period = "quarterly")})
output$cardio_admissions_sex_tot <- renderPlotly({plot_trend_chart(cardio_disch_filter(),
                                                            pal_sex, split = "sex", type = "total", data_name = "cardio_admissions", tab = "cardio", period = "quarterly")})
output$cardio_admissions_age_var <- renderPlotly({plot_trend_chart(cardio_disch_filter(),
                                                            pal_age, split = "age", type = "variation", data_name = "cardio_admissions", tab = "cardio", period = "quarterly")})
output$cardio_admissions_age_tot <- renderPlotly({plot_trend_chart(cardio_disch_filter(),
                                                            pal_age, split = "age", type = "total", data_name = "cardio_admissions", tab = "cardio", period = "quarterly")})
output$cardio_admissions_depr_var <- renderPlotly({plot_trend_chart(cardio_disch_filter(),
                                                             pal_depr, split = "dep", type = "variation",data_name = "cardio_admissions", tab = "cardio", period = "quarterly")})
output$cardio_admissions_depr_tot <- renderPlotly({plot_trend_chart(cardio_disch_filter(),
                                                             pal_depr, split = "dep", type = "total",data_name = "cardio_admissions", tab = "cardio", period = "quarterly")})
###############################################.


###############################################.
# Deaths charts
output$cardio_deaths_all <- renderPlotly({plot_overall_chart(cardio_dth_filter(),
                                                                 data_name = "cardio_deaths", area = "All", period = "quarterly")})
output$cardio_deaths_sex_var <- renderPlotly({plot_trend_chart(cardio_dth_filter(),
                                                                   pal_sex, split = "sex", type = "variation", data_name = "cardio_deaths", tab = "cardio", period = "quarterly")})
output$cardio_deaths_sex_tot <- renderPlotly({plot_trend_chart(cardio_dth_filter(),
                                                                   pal_sex, split = "sex", type = "total", data_name = "cardio_deaths", tab = "cardio", period = "quarterly")})
output$cardio_deaths_age_var <- renderPlotly({plot_trend_chart(cardio_dth_filter(),
                                                                   pal_2ages, split = "age", type = "variation", data_name = "cardio_deaths", tab = "cardio", period = "quarterly")})
output$cardio_deaths_age_tot <- renderPlotly({plot_trend_chart(cardio_dth_filter(),
                                                                   pal_2ages, split = "age", type = "total", data_name = "cardio_deaths", tab = "cardio", period = "quarterly")})
output$cardio_deaths_depr_var <- renderPlotly({plot_trend_chart(cardio_dth_filter(),
                                                                    pal_depr, split = "dep", type = "variation",data_name = "cardio_deaths", tab = "cardio", period = "quarterly")})
output$cardio_deaths_depr_tot <- renderPlotly({plot_trend_chart(cardio_dth_filter(),
                                                                    pal_depr, split = "dep", type = "total",data_name = "cardio_deaths", tab = "cardio", period = "quarterly")})
###############################################.

## Data downloads ----
###############################################.

overall_cardio_download <- reactive({

  # Branching this so that depending on input the right variables and names can be used
  # Cath branch
  if (input$`cardio-measure` == "cath") {
    selection <- c("week_ending", "count", "count_average", "variation")
    new_var_name <- "count_2019"
  }
  # A&E branch
  if (input$`cardio-measure` == "aye") {
    selection <- c("week_ending", "area_name", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }
  # Prescribing
  if (input$`cardio-measure` == "drug_presc") {
    selection <- c("week_ending", "area_name", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }

  # OOH
  if (input$`cardio-measure` == "ooh_cardiac") {
  selection <- c("week_ending", "area_name", "count", "count_average", "variation")
  new_var_name <- "average_2018_2019"
  }

  # SAS
  if (input$`cardio-measure` == "sas_cardiac") {
    selection <- c("week_ending", "area_name", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }

  # Admissions
  if (input$`cardio-measure` == "cardio_admissions") {
    selection <- c("week_ending", "area_name", "diagnosis", "type_admission", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }  

  # Deaths
  if (input$`cardio-measure` == "cardio_deaths") {
    selection <- c("week_ending", "area_name", "diagnosis", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }  
  
  # Prep data for download
  switch(
    input$`cardio-measure`,
    "cath" = filter_data(cath_lab_over(), area = F),
    "aye" = filter_data(ae_cardio, area = F),
    "drug_presc" = filter_data(cardio_drugs, area = F),
    "ooh_cardiac" = filter_data(ooh_cardiac, area = F),
    "sas_cardiac" = filter_data(sas_cardiac, area = F),
    "cardio_admissions" = filter_data(cardio_admissions, area = F),
    "cardio_deaths" = filter_data(cardio_deaths, area = F)
  ) %>%
    select_at(selection) %>%
    rename(!!new_var_name := count_average) %>%
    mutate(week_ending = format(week_ending, "%d %b %y"))
})

output$download_cardio_data <- downloadHandler(
  filename ="cardio_extract.csv",
  content = function(file) {
    write_csv(overall_cardio_download(),
              file) }
)

###############################################.
## Commentary ----
###############################################.
output$cardio_commentary <- renderUI({
  tagList(
    bsButton("jump_to_cardio",label = "Go to data"), #this button can only be used once
    h2("Cardiovascular - 5th May 2022"),
    h3("Cardiovascular Hospital admissions and Excess mortality"),
    tags$ul(
      tags$li("Data now available for Cardiovascular Hospital admissions and Excess mortality.
              Information is available by quarter for diagnosis Heart Attack, Heart Failure and Stroke.")),
    h2("Cardiovascular - 16th December 2020"),
    h3("Cardiovascular GP out of hour cases"),
    tags$ul(
      tags$li("For GP out of hours services there was a sharp fall of around 30% in cases for cardiovascular problems
               that started in early March 2020, some weeks prior to the introduction of ‘lockdown’ measures in Scotland.
               Contact numbers did not return to previous levels until early April, and during April, May and June were
               around 20% above the average for 2018-19. Trends were similar by age group and deprivation.")),
    h3("Cardiovascular Scottish Ambulance Service incidents"),
    tags$ul(
      tags$li("For Scottish Ambulance Service incidents, there was a sharp initial fall of around 40% in cardiovascular
               related incidents that started in early April 2020, shortly after the introduction of lockdown restrictions.
               This continued until mid-July. The fall in incidents was greatest in the most deprived groups.")),
    h2("Cardiovascular - 17th June 2020"),
          h3("Prescribing"),
          p("Information on prescriptions issued for cardiovascular medicines through
            General Practice has been included for the first time on 17th June 2020.
            These data indicate that:"),
          tags$ul(
            tags$li("The number of prescriptions for cardiovascular medicines overall rose
                    sharply in the third week of March, increasing by approximately 35% when
                    compared to the average for the same time period in 2018 and 2019."),
            tags$li("When examining specific groups of cardiovascular medicines routinely
                    prescribed in primary care a similar pattern is seen:",
                    tags$ul(
                      tags$li("The number of prescriptions rose sharply in March and peaked
                              in the third week."),
                      tags$li("The number of prescriptions in April was below that expected
                              from the 2018/2019 average and is likely a consequence of early
                              ordering of repeat supplies in March."),
                      tags$li("By the end of May, the numbers of prescriptions were returning
                              to normal levels.")
                    )
            )
          ),
          h3("Cardiovascular A&E attendances"),
          p("Information on cardiovascular attendances at Accident & Emergency Departments is presented in this tool.
            This data is based on coding available in the Accident & Emergency Datamart (managed by Public Health Scotland).
            Note that, due to limitations in diagnosis recording in the A&E datamart, the data are incomplete for a number of
            NHS Boards. Thus, the figures reported for cardiovascular-related attendances offer only a very approximate
            indication of attendances. Additionally, some NHS Boards have moved to a new recording standard which has not
            been fully consolidated in the A&E datamart as yet. As a result, figures for 2020, even prior to the
            introduction of lockdown measures, appear somewhat lower when compared to previous years."),
          tags$ul(
            tags$li("Overall there was a sharp drop in cardiovascular attendances at Accident and
                    Emergency Departments starting in early March 2020. Attendances were around 60%
                    lower compared to the 2018-2019 average. Levels rose again by the end of May, but
                    remain around 30% below the 2018-19 average."),
            tags$li("This drop in cardiovascular attendances was consistent across both males and
                    females, in younger and older patients and across deprivation quintiles.")),
          h3("Cardiac procedures"),
          p("Information on cardiac procedures has been obtained from two of the four cardiac care
            centres in Scotland (Royal Infirmary of Edinburgh and Golden Jubilee National Hospital). Data on
            the number of procedures was collected for:"),
          tags$ul(
            tags$li("coronary angiography (an investigation to evaluate whether there is any narrowing
                    of the arteries supplying the heart);"),
            tags$li("cardiac devices, including pacemakers to treat rhythm problems of the heart and"),
            tags$li("percutaneous coronary interventions, cardiac procedures to treat narrowing
                    of the arteries supplying the heart.")),
          p("The major observations are as follows:"),
          tags$ul(
            tags$li("Overall, the number of coronary angiographies dropped from early March 2020. A significant proportion of
                    these procedures are elective and these activities are likely to have been reduced in late March 2020."),
            tags$li("The change in percutaneous coronary interventions has been less pronounced. A significant
                    proportion of coronary interventions occur in a context of patients suffering a heart
                    attack. A proportion of coronary interventions are also planned and elective in nature. "),
            tags$li("The number of device procedures has been lower than expected since the end of March 2020."))

)

})


##END
