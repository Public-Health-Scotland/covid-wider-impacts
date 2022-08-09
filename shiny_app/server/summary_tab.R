# Wider impacts dashboard - Summary trends tab
# Server code

###############################################...
## Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
geoname_server("summary")

# op_geoname <- callModule(geotype_value, "op")

output$`op-geoname` <- renderUI({

  areas_summary <- sort(area_type_op$area_name[area_type_op$area_type == input$`op-geotype`])
  selectizeInput("op-geoname", label = NULL,
                 choices = areas_summary, selected = "")

})


# Disabling  admissions type if no admissions to hospital selected and
# updating labels to say it's not available
observeEvent({input$`summary-measure`}, {
  if (input$`summary-measure` == "rapid") {
    enable("adm_type")

    updateSelectInput(session, "adm_type",
                      label = "Step 3. Select type of admission.",
                      choices = c("All", "Emergency", "Planned"),
                      selected = "All")
  } else if (input$`summary-measure` == "op") {
    disable("adm_type")
    enable("appt_type")

    updateSelectInput(session, "appt_type",
                      label = "Step 3. Select type of appointment.",
                      choices = c("All", "New", "Return"),
                      selected = "All")
  } else if (input$`summary-measure` == "ooh") {
    disable("adm_type")
    enable("ooh_appt_type")
    
    updateSelectInput(session, "adm_type",
                      label = "Step 3. Select type of admission (not available).")
    
    updateSelectInput(session, "ooh_appt_type",
                      label = "Step 4. Select type of appointment for overall chart.",
                      choices = c("All cases", "All consultations" = "ALL", 
                                  "Covid consultations" = "COVID", "Non-covid consultations" = "NON COVID"), 
                      selected = "All cases")
  } else {
    disable("adm_type")

    updateSelectInput(session, "adm_type",
                      label = "Step 3. Select type of admission (not available).")
  }

})

###############################################.
## Modals ----
###############################################.
#modal to provide information on what specialties are included in each group
spec_modal_rapid <- modalDialog(
  h5("List of specialties and what group they correspond to"),
  renderTable(spec_lookup_rapid), # creating table based on specialty lookup
  size = "l", align= "center",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)
spec_modal_op <- modalDialog(
  h5("List of specialties and what group they correspond to"),
  renderTable(spec_lookup_op), # creating table based on specialty lookup
  size = "l", align= "center",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)

# Link action button click to modal launch
observeEvent(input$btn_spec_groups_rapid, { showModal(spec_modal_rapid) })
observeEvent(input$btn_spec_groups_op, { showModal(spec_modal_op) })

###############################################.
#modal to describe dataset
observeEvent(input$`summ-source-modal`,

             if (input$`summary-measure` == "rapid") {
               showModal(modalDialog(#RAPID ADMISSIONS MODAL
                 title = "What is the data source?",
                 p("The analyses shown here are derived from person level hospital admissions
                   data and show recent trends in admissions, whether COVID or non-COVID related,
                   and historic trends for comparison. The recent trend data is shown by age group,
                   sex, broad deprivation category and specialty groups."),
                 p("The hospital admissions analyses are derived from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=37",
                          "Rapid Preliminary Inpatient Data (RAPID) (external website)", target="_blank"),
                   "dataset. The RAPID dataset is managed by ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Predicting-Hospital-Activity/",
                          "Public Health Scotland (PHS).",  target="_blank"), "This dataset is submitted daily to 
                  PHS and relates mainly to general acute care.
                   Exclusions from the RAPID dataset are day cases, neonatal, maternity and
                   psychiatric care admissions. Admissions to the Golden Jubilee National Hospital are
                   also not included. Admissions related to COVID-19 will be included in totals. 
                  Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 p("Hospital admissions are allocated to weeks based on the ISO8601 standard. Following this standard 
                   the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use the 2018-2019 
                   average of week 52 value as a comparator for 2020’s week 53."),
                 p("The normal source of information on hospital admissions is the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=5",
                          "SMR01 (general inpatient and day cases) return (external website).", target="_blank"),
                   "However, there is generally time lag in the submission of SMR01 to PHS.
                   Therefore, RAPID is being used for the immediate monitoring of the impact of
                   COVID-19 on admissions to hospital and it provides broadly comparable figures to SMR01 on
                   numbers of admissions."),
                 p("Please note that for NHS Forth Valley data is largely incomplete for the period presented and 
                  therefore the trends for this board need to be interpreted carefully. In addition, from May 2022 onwards
                  a patient's sex is no longer derived from their CHI number; the sex recorded during the 
                  hospital admission is used. This has resulted in some minor differences in the assignation of sex across the 
                  time period presented."),
                 p(),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$`summary-measure` == "aye") { #A&E ATTENDANCES MODAL
               showModal(modalDialog(
                 title = "What is the data source?",
                 p("This tool provides a weekly summary of people attending A&E departments (Emergency Departments)
                    in the recent past, along with historical activity for
                   comparison purposes. The recent trend data is shown by age group, sex
                   and broad deprivation category (SIMD). This data only include Emergency Department
                   attendances and do not include minor injury units and other small hospitals and
                   health centres in rural areas that carry out emergency department related activity,
                   for more information on what sites are included please see this ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/Hospital-Site-List/",
                          "hospital list.",  target="_blank")),
                 p("Additional information relating to A&E activity is available from the ",
                   tags$a(href="https://publichealthscotland.scot/publications/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics",
                          "NHS Performs - weekly update of emergency department activity and waiting time statistics.",
                           target="_blank")),
                 p("Numbers of A&E attendances will include both COVID-19 and non-COVID-19 related activity." ),
                 p("Attendances are allocated to weeks based on the ISO8601 standard. Following this standard the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use the 2018-2019 average of week 52 value as a comparator for 2020’s week 53."),
                 p("Attendances to A&E departments data sourced from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=3",
                          "Accident and Emergency Datamart (A&E2) (external website).", target="_blank"),
                   "The A&E2 dataset is managed by ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/",
                          "Public Health Scotland (PHS).",  target="_blank")),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$`summary-measure` == "nhs24") { #NHS24 CALLS MODAL
               showModal(modalDialog(
                 title = "What is the data source?",
                 p("For many people an NHS 24 call provides the first point of contact for urgent access
                   to healthcare advice and, where necessary, onward treatment. At this time NHS 24 will
                   receive calls that relate to both COVID-19 and the wide range of other healthcare
                   issues that can and do occur all year round. Contacting NHS 24 provides many people
                   with access to healthcare advice, urgent clinical advice and, where necessary, onward treatment.
                   As well as telephone contact NHS 24 also provide digital (online) services through NHS Inform and self-help guidance."),
                 p("The figures presented in this tool relate to contacts concerning both COVID-19 and non-COVID
                   issues however they do not represent the complete picture of NHS 24 activity or demand.
                   The figures below only show data where there has been contact with 111 service and an individual
                   has spoken to a member of NHS 24 staff. They do not include activity relating to non-clinical
                   hotlines or digital (online) services such as NHS Inform. The charts
                   provide a weekly summary of contacts recorded in the recent past and historical
                   trends for comparison purposes. The recent trend data is shown by age group, sex and broad
                   deprivation category (SIMD)." ),
                 p("Figures by NHS health board include those calls made by residents of each health board area."),
                 p("Contacts are allocated to weeks based on the ISO8601 standard. Following this standard the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use the 2018-2019 average of week 52 value as a comparator for 2020’s week 53."),
                 p("If required, more detailed analysis of NHS24 activity may be available on request to ",
                   tags$a(href="mailto:phs.unscheduledcare@phs.scot", "phs.unscheduledcare@phs.scot",
                           target="_blank"), "."),
                 p("The NHS24 dataset is managed by ",
                   tags$a(href="https://publichealthscotland.scot/",
                          "Public Health Scotland",  target="_blank"), "and ",
                   tags$a(href="https://www.nhs24.scot/",
                          "NHS 24 (external website)",  target="_blank"), ".",
                   "This analysis is drawn from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=111", "Unscheduled Care Datamart (UCD) (external website).", target="_blank")
                 ),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))

             } else if (input$`summary-measure` == "ooh"){
               showModal(modalDialog(# OUT OF HOURS cases  MODAL
                 title = "What is the data source?",
                 p("The Primary Care Out of Hours service provides urgent access to a nurse or doctor,
                   when needed at times outside normal general practice hours, such as evenings,
                   overnight or during the weekend. An appointment to the service is normally arranged
                   following contact with NHS 24. The recent trend data is shown by age group, sex and
                   broad deprivation category (SIMD)."),
                 p("The charts provide a weekly summary of cases in the recent past and
                   historical trends for comparison purposes."),
                 p("The figures presented in this tool relate to cases/consultations concerning non-COVID
                   issues and cases/consultations within the COVID Pathway."),
                 p("A 'Case' is used to identify a patient's single encounter (service contact) with the OOH Service. 
                   Within a single case a patient may have multiple consultations with OOH health care professionals.  
                   Please note that the total number of consultations may be higher than the total number of cases as individuals 
                   may have more than one consultation during a single encounter with the Out of Hours service."),
                 p("Cases are allocated to weeks based on the ISO8601 standard. Following this standard the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use the 2018-2019 average of week 52 value as a comparator for 2020’s week 53."),
                 p("If required, more detailed analysis of the Primary Care Out of Hours service may
                   be available on request to ",
                   tags$a(href="mailto:phs.unscheduledcare@phs.scot", "phs.unscheduledcare@phs.scot",

                           target="_blank"), "."),
                 p("General Practice Out of Hours service data is sourced from the",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=113",
                          "GP Out of Hours Dataset (OOH)(external website).", target="_blank"),
                   "The OOH dataset is managed by ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/GP-Out-of-Hours-Services/",
                          "Public Health Scotland (PHS).",  target="_blank")),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))

             } else if (input$`summary-measure` == "sas"){
               showModal(modalDialog( # SAS  MODAL
                 title = "What is the data source?",
                 p("The charts provide a weekly summary of Scottish Ambulance Service emergency calls attended with historical trends for comparison purposes.
                   The recent trend data is shown by age group, sex and broad deprivation category (SIMD). The figures presented in this tool
                   relate to incidents concerning both COVID-19 and non-COVID issues. Please note that the source of this data is the Unscheduled Care
                   Datamart and represents a sub-set of the total Scottish Ambulance service activity. Figures include emergencies, where a vehicle arrived
                   at the scene of the incident, and excludes both data from resources which were cleared as ‘dealt with by another vehicle’ and air ambulance data."),
                 p("SAS currently publish weekly unscheduled care operational statistics at the following ", 
                   tags$a(href="https://www.scottishambulance.com/publications/unscheduled-care-operational-statistics/", 
                          "Unscheduled Care Operational Statistics", target="_blank"), ". This details unscheduled care demand, 
                   response times and turnaround times. Please note that the data published by SAS is sourced from a 
                   different operational system than that used for the PHS reporting. This means that the data published 
                   by SAS will at times be slightly different to that reported by PHS source. The data published by PHS 
                   is less timely than the data used for the SAS publication, however allows for data to be linked in order 
                   to gain further insight into patient flow through unscheduled care."),
                 p("Calls are allocated to weeks based on the ISO8601 standard. Following this standard the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use the 2018-2019 average of week 52 value as a comparator for 2020’s week 53."),
                 p("If required, more detailed analysis of SAS activity may be available on request to ",
                   tags$a(href="mailto:phs.unscheduledcare@phs.scot", "phs.unscheduledcare@phs.scot",
                           target="_blank"), "."),
                 p("The SAS dataset is managed by ",
                   tags$a(href="https://publichealthscotland.scot/",
                          "Public Health Scotland",  target="_blank"), "and ",
                   tags$a(href="https://www.scottishambulance.com/",
                          "Scottish Ambulance Service (external website)",  target="_blank"), ".",
                   "This analysis is drawn from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=111", "Unscheduled Care Datamart (UCD) (external website).", target="_blank")
                 ),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))

               } else if (input$`summary-measure` == "deaths"){
               showModal(modalDialog( # DEATHS  MODAL
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
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))

               } else if (input$`summary-measure` == "op"){
                 showModal(modalDialog( # OUTPATIENTS MODAL
                   title = "What is the data source?",
                   p("The analyses shown here are derived from person level",
                     " outpatient data and show recent trends in attendances, ",
                     "whether COVID or non-COVID related, and historic trends ",
                     "for comparison. The recent trend data is shown by age group,",
                     " sex, Scottish Index of Multiple Deprivation (SIMD), ",
                     "mode of clinical interaction, and specialty groups. Data are given at ",
                     "Scotland, Health board of treatment, Health board of residence, and",
                     "HSC partnership of residence level. ",
                     "'Other' Health board of treatment refers to cases where the
                     health board is NHS24 (due to small numbers), unknown or invalid.",
                     " 'Other' Health board of residence and HSC partnership of residence
                     refers to Other Residential Categories as defined ",
                     "on our ",
                     tags$a(href="https://www.opendata.nhs.scot/dataset/non-standard-geography-codes-and-labels/resource/32164b83-c9ec-495a-ac9f-dbeeb6ed5e59",
                            "Open Data portal (external website)", target = "_blank"), "."),
                   p("The outpatient analyses are derived from the ",
                     tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=4",
                            "Scottish Morbidity Record (SMR00) dataset (external website)",
                            target="_blank"),". This dataset is submitted monthly ",
                     "to Public Health Scotland (PHS), and relates to outpatient ",
                     "care. All new and return appointments from consultant-led clinics are included; ",
                     "A&E and Genitourinary specialties are excluded, as well as ",
                     "Did Not Attend (DNA) appointments. For more ",
                     "information on outpatient attendances, please see our ",
                     tags$a(href= "https://www.ndc.scot.nhs.uk/Dictionary-A-Z/Definitions/index.asp?Search=O&ID=374&Title=Outpatient%20Attendance",
                            "Data Dictionary (external website)",  target="_blank"),
                     ". Please note that there is a time lag between the submission of ",
                     "SMR00 to PHS, and the data being validated and ready for release. ",
                     "Therefore, weekly data up to 26th December 2021 and monthly data up to 31st December 2021 are given. For data quality issues, please see the ", 
                     tags$a(href = "https://publichealthscotland.scot/publications/acute-hospital-activity-and-nhs-beds-information-quarterly/",
                            "Acute Activity and NHS Beds quarterly publication",  

                            target="_blank"), ". All information presented has been ",
                     "taken from SMR00; this is different from the Acute Activity ",
                     "and NHS Beds publication, so the figures are not comparable."),
                   p("The SMR00 dataset is managed by Public Health Scotland (PHS). ",
                     "For current completeness estimates, please see ",
                     tags$a(href = "https://beta.isdscotland.org/products-and-services/data-management-hospital-activity/smr-completeness/",
                            "SMR data completeness (external website)", target = "_blank"), " web page."),
                   p(tags$a(href = "https://www.publichealthscotland.scot/publications/statistical-disclosure-protocol/statistical-disclosure-protocol/",
                            "Statistical disclosure control", target = "_blank"), 

                     "has been applied to this analysis."),
                   size = "m",
                   easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
               }
             )

###############################################.
# Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd, simd_modal())


###############################################.
# Modal to explain mode of clinical interaction graphs
moc_modal <- modalDialog(
  h5(tags$b("Interpretation of this chart")),
  p("Please note that the majority of outpatient appointments in previous years were
    conducted face to face, and there were very few appointments held by other means.
    This means that numbers in previous years are very small or zero, and the graph shows
    very high percentage changes as a result. Please treat these data with caution."),
  p("Where there were no cases for non-face to face appointments in previous years,
    variation is shown as 100%. This is not accurate but is an attempt to
    show that there has been a significant change. The way this is displayed may change in future releases."),
  size = "l",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
# Link action button click to modal launch
observeEvent(input$btn_modal_moc, { showModal(moc_modal) })


###############################################.
# Modal to explain ethnicity graphs
eth_modal <- modalDialog(
  h5(tags$b("Interpretation of this chart")),
  p("The ethnic group field in the Scottish Morbidity Record (SMR) 
  classifies the person according to their own perceived ethnic group and cultural 
  background. More information can be found in the", 
    tags$a(href="https://www.ndc.scot.nhs.uk/Dictionary-A-Z/Definitions/index.asp?Search=E&ID=243&Title=Ethnic%20Group", 
           "Health & Social Care data dictionary web page (external website).",
         target="_blank")),
  p("It became mandatory for NHS Scotland organisations to record ethnic group 
    on SMR outpatient (SMR00) returns from 1 February 2021. There is currently
  significant variation in the completeness of ethnic group recording in new 
  outpatient appointment records between NHS Boards. More information can be found in the
    ", tags$a(href="https://www.isdscotland.org/products-and-Services/Data-Support-and-Monitoring/SMR-Ethnic-Group-Recording/",
              "SMR Ethnic Group Recording (external website)", target="_blank")),
  p("The following list is the current ethnicity classification (2011 Census categories) 
  used by NHS Scotland organisations for SMR return purposes, and the ethnic groups 
  that we have used in this dashboard."),
  renderTable(eth_lookup), 
  p("The ‘Missing’ ethnic group category includes those where ethnic group was 
    recorded as 'Not Known', 'Refused/Not Provided by the Patient' or was not recorded at all."),
  p("It is important to note that the trends for ethnic groups with small populations should be 
  interpreted with caution as they will be subject to greater variability due to small numbers."),
  size = "l",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)
# Link action button click to modal launch 
observeEvent(input$btn_modal_eth, { showModal(eth_modal) }) 



###############################################.
## Reactive datasets ----
###############################################.
# Rapid dataset filtered for admission_type, then used to create the admissions charts
rapid_filt <- reactive({
  rapid %>% filter(admission_type == input$adm_type &
                     spec == "All")
})

# Rapid dataset used for specialty charts
rapid_spec <- reactive({
  rapid %>% filter(type == "sex") %>%
    filter(area_name == input$`summary-geoname` &
             admission_type == input$adm_type &
             category == "All" &
             spec %in% input$adm_specialty)

})

# Outpatients dataset filtered for admission_type, then used to create the admissions charts
op_filt <- reactive({
  outpats %>%
    filter(admission_type == input$appt_type &
             spec == "All" &
             area_type == input$`op-geotype` &
             time_split == input$time_type)
})

# # Outpatients dataset used for specialty charts
op_spec <- reactive({
  outpats %>%
    filter(type == "sex") %>%
    filter(area_name == input$`op-geoname` &
             admission_type == input$appt_type &
             category == "All" &
             spec %in% input$op_specialty &
             area_type == input$`op-geotype` &
             time_split == input$time_type)   

})

# # Outpatients dataset used for ethnicity charts
op_eth <- reactive({
  outpats %>%
    filter(type == "eth" & 
             area_type == "Scotland" &
             admission_type == input$appt_type &
             category %in% input$op_ethnicity)   
})

# Data used for overall and trend charts for most of the summary datasets
summ_chart_data <- reactive({
  switch(input$`summary-measure`,
  "rapid" = rapid_filt(),
  "aye" = aye,
  "ooh" = ooh,
  "nhs24" = nhs24,
  "sas" = sas,
  "deaths" = deaths,
  "op" = op_filt()) })

#Monthly or weekly time peiod
summ_time_period <- reactive({
  if(input$`summary-measure` == "op"){
    time_period <- tolower(input$time_type)
  } else{
    time_period <- "weekly"
  }
}) 

###############################################.
## Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$data_explorer <- renderUI({

  # text for titles of cut charts
  dataset <- case_when(input$`summary-measure` == "rapid" ~ "admissions",
                       input$`summary-measure` == "aye" ~ "attendances",
                       input$`summary-measure` == "nhs24" ~ "completed contacts",
                       input$`summary-measure` == "ooh" ~ "cases",
                       input$`summary-measure` == "sas" ~ "incidents",
                       input$`summary-measure` == "deaths" ~ "deaths",
                       input$`summary-measure` == "op" ~ "appointments")

  if (input$`summary-measure` == "deaths"){
    variation_title <- paste0("Percentage change in ", dataset,
                            " compared with the corresponding time in 2015-2019 by ")   #different averaging period for deaths
  } else {
    variation_title <- paste0("Percentage change in ", dataset,
                              " compared with the corresponding time in 2018-2019 by ")
  }

  time_period <- str_to_title (summ_time_period())
  
  total_title <- paste0(time_period, " number of ", dataset, " by ")
  

  # To make sure that both titles take the same space and are lined up doing
  # a bit of a hacky shortcut:
  diff_chars <- nchar(variation_title) - nchar(total_title) +10
  extra_chars <- paste0(c(rep("_", diff_chars), "."), collapse = '')

  #update date for outpatients and the rest is different
  upd_date_summ <- case_when(input$`summary-measure` == "op" ~ "15 June 2022",
                             TRUE ~ "3 August 2022")

  # Function to create the standard layout for all the different charts/sections
  cut_charts <- function(title, source, data_name) {
    tagList(
      fluidRow(column(10, p("Last updated: ", upd_date_summ))),
      fluidRow(column(10, h3(title)),
               column(2, sourcemodal_ui("summ"))),

      if (input$`summary-measure` == "nhs24"){
        tagList(
        p("The data used in this chart are taken from the Unscheduled Care Datamart.
          As mentioned in the", tags$a(href="https://publichealthscotland.scot/publications/covid-19-statistical-report",
                                                                                                                   "COVID-19 weekly report for Scotland",  target="_blank"),
          "NHS 24 made changes to their service delivery to respond to COVID-19. The data from March 2020
          does not reflect the full extent of the demand and activity being undertaken by NHS 24 at this time.
          As of the 31st of March 2022 the COVID-19 Community Pathway will be closed. 
          From this date patients who contact NHS 24 with Covid symptoms during the in-hours period will be 
          advised to contact their own GP practice. For the out of hours period, the Out of Hours services will 
          continue to manage Covid patients directed by NHS 24 as a matter of course. This will have an impact on 
          the NHS 24 and the Out of Hours data data contained in the dashboard. "))
        },
      if (input$`summary-measure` == "deaths"){
        tagList(
        p("The analyses below are derived from the National Records of Scotland (NRS) weekly deaths dataset (provisional numbers).
          Numbers of deaths represent the total number of deaths (from any cause) that were registered in
          Scotland in any particular week.  Comparing the number of deaths in the most recent weeks to the
          average over the past 5 years allows estimation of the numbers of excess deaths.
          Volatility of the trends will be observed in some charts due to small counts."),
        plot_box(paste0("2020 to 2022 compared with the 2015-2019 average"), "summ_overall")) #different averaging period for deaths
        } else if (input$`summary-measure` == "op") {
          plot_box(paste0("2020 to 2022 compared with the 2018-2019 average"), "op_overall")
        } else {
          plot_box(paste0("2020 to 2022 compared with the 2018-2019 average"), "summ_overall")
        },
      
       tagList(plot_cut_box(paste0(variation_title, "sex"), "summ_sex_var",
                   paste0(total_title, "sex"), "summ_sex_tot"),
      plot_cut_box(paste0(variation_title, "age group"), "summ_age_var",
                   paste0(total_title, "age group"), "summ_age_tot"),
      fluidRow(column(6, h4(paste0(variation_title, "SIMD quintile"))),
               column(6, h4(paste0(total_title, "SIMD quintile")))),
      fluidRow(actionButton("btn_modal_simd", "What is SIMD and deprivation?",
                            icon = icon('question-circle'))),
      fluidRow(column(6, withSpinner(plotlyOutput("summ_depr_var"))),
               column(6, withSpinner(plotlyOutput("summ_depr_tot"))))
      ) #tag list end
        
    ) #tag list end

  } #function end

  # Charts and rest of UI
  if (input$`summary-measure` == "rapid") {
    tagList(#Hospital admissions
      cut_charts(title= "Weekly admissions to hospital", source = "PHS RAPID Datamart",
                 data_name = "adm"),
      fluidRow(column(6, h4(paste0(variation_title, "specialty group - (admission type: ", tolower(input$adm_type), ")"))), # Adding adm_type here to make clear what is selected
               column(6, h4(paste0(total_title, "specialty group - (admission type: ", tolower(input$adm_type), ")")))), # Adding adm_type here to make clear what is selected
      fluidRow(column(6, pickerInput("adm_specialty", "Select one or more specialty groups",
                                     choices = if (input$`summary-geotype` == "Scotland") {
                                       spec_list_rapid} else {
                                         spec_list_rapid[c(1:8,11)]}, multiple = TRUE,
                                     selected = c("Medical (incl. Cardiology & Cancer)", "Surgery", "Paediatrics (medical & surgical)"))),
               column(6, actionButton("btn_spec_groups_rapid",
                                      "Specialties and their groups",
                                      icon = icon('question-circle')))),
      fluidRow(column(6, withSpinner(plotlyOutput("adm_spec_var"))),
               column(6, withSpinner(plotlyOutput("adm_spec_tot"))))
    )
  } else if (input$`summary-measure` == "aye") {
    tagList(#A&E Attendances
    cut_charts(title= "Weekly attendances to A&E departments",
               source = "PHS AE2 Datamart", data_name = "aye"))

  } else if (input$`summary-measure` == "nhs24") {# NHS 24 calls
    cut_charts(title= "Weekly completed contacts with NHS 24",
               source = "PHS Unscheduled Care Datamart", data_name ="nhs24")

  } else if (input$`summary-measure` == "ooh") { #Out of hours cases
      if (input$ooh_appt_type == "All cases"){ # For OOH cases
          tagList(
           tags$b(span("Please note that the data on this page now includes individuals coming to
                Primary Care Out of Hours services via the COVID Pathway. This pathway was closed from 31st March 2022.",
                       br(),
                       "Please note there are seven missing files for NHS Lanarkshire: 30 Jan 2022 ;01 Feb 2022; 27 Feb 2022; 12 Mar 2022; 13 Mar 2022; 19 Mar 2022; 02 May 2022, 
                       PHS are working with data suppliers to resolve this. 
                       These will impact on Scotland figures and comparisons with previous years.", 
                       style = "color:red")),
             br(),
    
          cut_charts(title = "Weekly cases in out of hours services", data_name ="ooh"))
        
        } else if(input$ooh_appt_type != "All cases") { #For OOH consultations
          ooh_cons_type <- case_when(input$ooh_appt_type == "NON COVID" ~ "Non-Covid related ",
                                     input$ooh_appt_type == "COVID" ~ "Covid related ",
                                     input$ooh_appt_type == "ALL" ~ "", T ~ "")
          
          tagList(
            h3(paste0("Weekly ", ooh_cons_type, "consultations in out of hours services")),
            fluidRow(column(6, sourcemodal_ui("summ")),
                     column(6, p("Last updated: ", upd_date_summ))),
            tags$b(span("Please note that the data on this page now includes individuals coming to
                Primary Care Out of Hours services via the COVID Pathway. This pathway was closed from 31st March 2022.", 
                        br(),
                        br(),
                        "Please note there are seven missing files for NHS Lanarkshire: 30 Jan 2022 ;01 Feb 2022; 27 Feb 2022; 12 Mar 2022; 13 Mar 2022; 19 Mar 2022; 02 May 2022, 
                       PHS are working with data suppliers to resolve this.  NHS Tayside data missing for 2 – 12 June 2022 inclusive. 
                       These will impact on Scotland figures and comparisons with previous years.",
                        style = "color:red")),
            br(),
            plot_box(paste0("2020 to 2022 compared with the 2018-2019 average"), paste0("ooh_cons_overall")),
            tags$b(p("Out of Hours demographic data is only available for cases. Please Select 'All cases' in Step 4.", style="text-align:center; font-size:16px")),
            br(),
            br())
        } #end of OOH 
        } else if (input$`summary-measure` == "sas") {
    tagList(# SAS data
        p("SAS currently publish weekly unscheduled care operational statistics at the following ", 
        tags$a(href="https://www.scottishambulance.com/publications/unscheduled-care-operational-statistics/", 
               "Unscheduled Care Operational Statistics (external website)", target="_blank"), ". The data published by SAS is sourced from a 
        different operational system than that used for the PHS reporting. This means that the data published 
        by SAS will at times be slightly different to that reported by PHS source."),
    cut_charts(title= "Weekly attended incidents by Scottish Ambulance Service",
               source = "PHS Unscheduled Care Datamart", data_name ="sas"))

  } else if (input$`summary-measure` == "deaths") { # Deaths data
    cut_charts(title= "Weekly number of deaths",
               source = "NRS Death Registrations", data_name ="deaths")

  } else if (input$`summary-measure` == "op") { # Outpatients data
      tagList(tags$b(span("Please note that these data are for management information only, and care should",
                          "be taken when interpreting these figures. For more information on methodology and data quality please see the ",
                          tags$a(href = "https://publichealthscotland.scot/publications/acute-hospital-activity-and-nhs-beds-information-quarterly/",
                                 "Acute Activity and NHS Beds quarterly publication",  
                                 target="_blank"), ". Did Not Attend appointments (DNAs) are not included in the figures shown here.",
                          style="color:red")),
              cut_charts(title= paste0(time_period, " outpatient appointments"),
                         source = "SMR00", data_name = "op"),
              fluidRow(column(6,
                              h4(paste0(variation_title, "specialty group"))),
                       # Adding adm_type here to make clear what is selected
                       column(6,
                              h4(paste0(total_title, "specialty group")))),
              ###Adding adm_type here to make clear what is selected
              fluidRow(column(6,
                              pickerInput("op_specialty", "Select one or more specialty groups",
                                          choices = spec_list_op, 
                                          multiple = TRUE,
                                          selected = c("Medical", "Surgery"))),
                       column(6,
                              actionButton("btn_spec_groups_op", 
                                           "Specialties and their groups",
                                           icon = icon('question-circle')))),
              fluidRow(column(6,
                              withSpinner(plotlyOutput("op_spec_var"))),
                       column(6,
                              withSpinner(plotlyOutput("op_spec_tot")))),
              fluidRow(column(6,
                              h4(paste0(variation_title, "mode of clinical interaction"))),
                       # Adding adm_type here to make clear what is selected
                       column(6,
                              h4(paste0(total_title, "mode of clinical interaction")))),
              fluidRow(actionButton("btn_modal_moc", "Interpretation of this chart", 
                                    icon = icon('fas fa-exclamation-circle'))),
              fluidRow(column(6,
                              withSpinner(plotlyOutput("op_moc_var"))),
                       column(6,
                              withSpinner(plotlyOutput("op_moc_tot")))),
              #Add ethnicity charts
                fluidRow(column(6,
                                h4(paste0(variation_title, "ethnic group")),
                                tags$em("Please note that this data is only available by month.")),
                         column(6,
                                h4(paste0("Monthly number of ", dataset, " by ethnic group")),
                                tags$em("Please note that this data is only available by month."))),
                
                ###Adding adm_type here to make clear what is selected
                fluidRow(column(6,
                                pickerInput("op_ethnicity", "Select one or more ethnic groups",
                                            choices = eth_list_op, 
                                            multiple = TRUE,
                                            selected = eth_list_op,
                                            options = list(
                                              `actions-box` = TRUE))),
                         column(6,
                                actionButton("btn_modal_eth", "Interpretation of this chart", 
                                             icon = icon('fas fa-exclamation-circle')))),
                fluidRow(column(6,
                                withSpinner(plotlyOutput("op_eth_var"))),
                         column(6,
                                withSpinner(plotlyOutput("op_eth_tot"))))
                                          
      ) #tag list end
  } #op end
})



###############################################.
## Charts ----
###############################################.
# Overall charts comparing pandemic period with baseline
output$summ_overall <- renderPlotly({plot_overall_chart(summ_chart_data(), data_name = input$`summary-measure`)})
output$ooh_cons_overall <- renderPlotly({plot_overall_chart(ooh_cons, data_name = "ooh_cons")})

# Sex variation and total charts
output$summ_sex_var <- renderPlotly({
  plot_trend_chart(summ_chart_data(), pal_sex, "sex", 
                   data_name = input$`summary-measure`, period = summ_time_period())})

output$summ_sex_tot <- renderPlotly({
  plot_trend_chart(summ_chart_data(), pal_sex, "sex", 
                   "total", data_name = input$`summary-measure`, period = summ_time_period())})
#Deprivation variation and total charts
output$summ_depr_var <- renderPlotly({
  plot_trend_chart(summ_chart_data(), pal_depr, "dep", 
                   data_name = input$`summary-measure`, period = summ_time_period())})

output$summ_depr_tot <- renderPlotly({
  plot_trend_chart(summ_chart_data(), pal_depr, "dep", "total",
                   data_name = input$`summary-measure`, period = summ_time_period())})

#Age variation and total charts
output$summ_age_var <- renderPlotly({
  plot_trend_chart(summ_chart_data(), pal_age, "age", 
                   data_name = input$`summary-measure`, period = summ_time_period())})

output$summ_age_tot <- renderPlotly({
  plot_trend_chart(summ_chart_data(), pal_age, "age", "total", 
                   data_name = input$`summary-measure`, period = summ_time_period())})

# Admissions to hospital charts
output$adm_spec_var <- renderPlotly({plot_spec("variation", rapid_spec())})
output$adm_spec_tot <- renderPlotly({plot_spec("total", rapid_spec())})

# Outpatients charts
output$op_overall <- renderPlotly({plot_overall_chart(op_filt(), "op", op = T, period = summ_time_period())})
output$op_moc_var <- renderPlotly({plot_trend_chart(op_filt(), pal_moc, "moc", data_name = "op", period = summ_time_period())})
output$op_moc_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_moc, "moc", "total", "op", period = summ_time_period())})
output$op_spec_var <- renderPlotly({plot_spec("variation", op_spec(), marg = 80, op = T, period = summ_time_period())})
output$op_spec_tot <- renderPlotly({plot_spec("total", op_spec(), marg = 80, op = T, period = summ_time_period())})

output$op_eth_tot <- renderPlotly({
  
  plot_nodata(text_nodata = "Data is only available for Scotland")
  plot_trend_chart(dataset = op_eth(), pal_chose = pal_eth, split = "eth", type = "total", 
                   data_name = "op", period = "monthly")})
 
output$op_eth_var <- renderPlotly({
  plot_trend_chart(dataset = op_eth(), pal_chose = pal_eth, split = "eth", 
                   data_name = "op", period = "monthly")})


# Palette for specialty
pal_spec <- reactive({
  #Creating palette of colors: colorblind proof
  #First obtaining length of each geography type, if more than 6, then 6,
  # this avoids issues. Extra selections will not be plotted
  if (input$`summary-measure` == "op") {
    trend_length <- length(input$op_specialty)
  } else {
    trend_length <- length(input$adm_specialty)
  }

  # First define the palette of colours used, then set a named vector, so each color
  # gets assigned to an area. I think is based on the order in the dataset.
  trend_palette <- c("#000000", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                     "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928")

  if (input$`summary-measure` == "op") {
    trend_scale <- c(setNames(trend_palette,
                                 unique(op_spec()$spec)[1:trend_length]))
  } else {
    trend_scale <- c(setNames(trend_palette,
                                    unique(rapid_spec()$spec)[1:trend_length]))
  }

  trend_col <- trend_scale[1:trend_length]

})

symbol_spec <- reactive({
  # First define the palette of sybols used, then set a named vector, so each color
  # gets assigned to an area. I think is based on the order in the dataset.
  symbols_palette <-  c('circle', 'diamond', 'circle', 'diamond', 'circle', 'diamond',
                        'square','triangle-up', 'square','triangle-up', 'square','triangle-up')

  if (input$`summary-measure` == "op") {
  #Creating palette of colors: colorblind proof
  #First obtaining length of each geography type, if more than 6, then 6,
  # this avoids issues. Extra selections will not be plotted
    trend_length <- length(input$op_specialty)

    symbols_scale <- c(setNames(symbols_palette,
                                unique(op_spec()$spec)[1:trend_length]))
  } else { #rapid
    trend_length <- length(input$adm_specialty)

    symbols_scale <- c(setNames(symbols_palette,
                                   unique(rapid_spec()$spec)[1:trend_length]))
  }

  symbols_trend <- symbols_scale[1:trend_length]

})

###############################################.
## Data downloads ----
###############################################.
ooh_download <- reactive({
  
  if(input$ooh_appt_type == "All cases") {
    
    filter_data(ooh) %>% 
      rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>% 
      select(area_name, week_ending, count, starts_with("average"))
    
  } else {
    filter_data(ooh_cons, data_name = "ooh_cons") %>% 
      rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>% 
      select(area_name, week_ending, count, starts_with("average"))
  }
  
})
# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
overall_data_download <- reactive({
  switch(
    input$`summary-measure`,
    "rapid" = filter_data(rapid_filt() %>% rename(average_2018_2019 = count_average)) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "aye" = filter_data(aye) %>% rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "nhs24" = filter_data(nhs24) %>% rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "ooh" = ooh_download(),
    "sas" = filter_data(sas) %>% rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "deaths" = filter_data(deaths) %>% rename(average_2015_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "op" = filter_data(op_filt() %>% rename(average_2018_2019 = count_average), op = T) %>% 
      mutate(week_ending = ifelse(time_split == "Monthly", format(week_ending, "%b %y"),
                                  format(week_ending, "%d %b %y"))) %>%
      rename(time_ending = week_ending) %>%
      select(area_name, time_split, time_ending, count, starts_with("average"))
  )

})

output$download_chart_data <- downloadHandler(
  filename = function() {
    paste(input$`summary-measure`, ".csv", sep = "")
  },
  content = function(file) {
    write_csv(overall_data_download(),
              file) }
)


#END
