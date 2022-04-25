###############################################...
#  # Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
output$geoname_ui <- renderUI({
    areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype])
    selectizeInput("geoname", label = NULL,
                   choices = areas_summary, selected = "")

})

output$geoname_op_ui <- renderUI({

  areas_summary <- sort(area_type_op$area_name[area_type_op$area_type == input$geotype_op])
  selectizeInput("geoname_op", label = NULL,
                 choices = areas_summary, selected = "")

})


# Disabling  admissions type if no admissions to hospital selected and
# updating labels to say it's not available
observeEvent({input$measure_select}, {
  if (input$measure_select == "rapid") {
    enable("adm_type")

    updateSelectInput(session, "adm_type",
                      label = "Step 3. Select type of admission.",
                      choices = c("All", "Emergency", "Planned"),
                      selected = "All")
  } else if (input$measure_select == "outpats") {
    disable("adm_type")
    enable("appt_type")

    updateSelectInput(session, "appt_type",
                      label = "Step 3. Select type of appointment.",
                      choices = c("All", "New", "Return"),
                      selected = "All")
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
# Link action button click to modal launch
observeEvent(input$btn_dataset_modal,

             if (input$measure_select == "rapid") {
               showModal(modalDialog(#RAPID ADMISSIONS MODAL
                 title = "What is the data source?",
                 p("The analyses shown here are derived from person level hospital admissions
                   data and show recent trends in admissions, whether COVID or non-COVID related,
                   and historic trends for comparison. The recent trend data is shown by age group,
                   sex, broad deprivation category and specialty groups."),
                 p("The hospital admissions analyses are derived from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=37",
                          "Rapid Preliminary Inpatient Data (RAPID)", target="_blank"),
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
                          "SMR01 (general inpatient and day cases) return.", target="_blank"),
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
             } else if (input$measure_select == "aye") { #A&E ATTENDANCES MODAL
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
                   tags$a(href="https://beta.isdscotland.org/find-publications-and-data/health-services/hospital-care/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/",
                          "NHS Performs - weekly update of emergency department activity and waiting time statistics.",
                           target="_blank")),
                 p("Numbers of A&E attendances will include both COVID-19 and non-COVID-19 related activity." ),
                 p("Attendances are allocated to weeks based on the ISO8601 standard. Following this standard the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use the 2018-2019 average of week 52 value as a comparator for 2020’s week 53."),
                 p("Attendances to A&E departments data sourced from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=3",
                          "Accident and Emergency Datamart (A&E2).", target="_blank"),
                   "The A&E2 dataset is managed by ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/",
                          "Public Health Scotland (PHS).",  target="_blank")),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_select == "nhs24") { #NHS24 CALLS MODAL
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
                   tags$a(href="mailto:phs.isdunscheduledcare@nhs.net", "phs.isdunscheduledcare@nhs.net",
                           target="_blank"), "."),
                 p("The NHS24 dataset is managed by ",
                   tags$a(href="https://publichealthscotland.scot/",
                          "Public Health Scotland",  target="_blank"), "and ",
                   tags$a(href="https://www.nhs24.scot/",
                          "NHS 24",  target="_blank"), ".",
                   "This analysis is drawn from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=111", "Unscheduled Care Datamart (UCD).", target="_blank")
                 ),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))

             } else if (input$measure_select == "ooh"){
               showModal(modalDialog(# OUT OF HOURS cases  MODAL
                 title = "What is the data source?",
                 p("The Primary Care Out of Hours service provides urgent access to a nurse or doctor,
                   when needed at times outside normal general practice hours, such as evenings,
                   overnight or during the weekend. An appointment to the service is normally arranged
                   following contact with NHS 24. The recent trend data is shown by age group, sex and
                   broad deprivation category (SIMD)."),
                 p("The charts provide a weekly summary of cases in the recent past and
                   historical trends for comparison purposes."),
                 p("The figures presented in this tool exclude cases within any of the COVID-19
                   hubs or assessment centres and relate only to cases concerning non-COVID
                   issues. "),
                 p("Cases are allocated to weeks based on the ISO8601 standard. Following this standard the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use the 2018-2019 average of week 52 value as a comparator for 2020’s week 53."),
                 p("If required, more detailed analysis of the Primary Care Out of Hours service may
                   be available on request to ",
                   tags$a(href="mailto:phs.isdunscheduledcare@nhs.net", "phs.isdunscheduledcare@nhs.net",
                           target="_blank"), "."),
                 p("General Practice Out of Hours service data is sourced from the",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=113",
                          "GP Out of Hours Dataset (OOH).", target="_blank"),
                   "The OOH dataset is managed by ",
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/GP-Out-of-Hours-Services/",
                          "Public Health Scotland (PHS).",  target="_blank")),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))

             } else if (input$measure_select == "sas"){
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
                   tags$a(href="mailto:phs.isdunscheduledcare@nhs.net", "phs.isdunscheduledcare@nhs.net",
                           target="_blank"), "."),
                 p("The SAS dataset is managed by ",
                   tags$a(href="https://publichealthscotland.scot/",
                          "Public Health Scotland",  target="_blank"), "and ",
                   tags$a(href="https://www.scottishambulance.com/",
                          "Scottish Ambulance Service",  target="_blank"), ".",
                   "This analysis is drawn from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=111", "Unscheduled Care Datamart (UCD).", target="_blank")
                 ),
                 p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))

               } else if (input$measure_select == "deaths"){
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
                          "weekly deaths",  target="_blank"), " dataset. Deaths related to COVID-19 are included in totals.
                   Data are provisional and subject to revision."),
                 p("The figures are based on the date a death was registered rather than the date the death occurred. When someone dies,
                   their family (or a representative) have to make an appointment with a registrar to register the death.
                   Legally this must be done within 8 days, although in practice there is, on average, a 3 day gap between a
                   death occurring and being registered. More information on days between occurrence and registration can be be found on the ",
                   tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-background-information/births-and-deaths-days-until-registration",
                          "NRS website", target="_blank"),"."),
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
                          "National Records of Scotland (NRS).",  target="_blank")),
                 p("For more information on deaths and health inequalities during the pandemic, please read ",
                   tags$a(href = "https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-weekly-excess-deaths/",
                          "this report.",  target="_blank")),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))

               } else if (input$measure_select == "outpats"){
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
                            "Open Data portal", target = "_blank"), "."),
                   p("The outpatient analyses are derived from the ",
                     tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=4",
                            "Scottish Morbidity Record (SMR00) dataset",
                            target="_blank"),". This dataset is submitted monthly ",
                     "to Public Health Scotland (PHS), and relates to outpatient ",
                     "care. All new and return appointments from consultant-led clinics are included; ",
                     "A&E and Genitourinary specialties are excluded, as well as ",
                     "Did Not Attend (DNA) appointments. For more ",
                     "information on outpatient attendances, please see our ",
                     tags$a(href= "https://www.ndc.scot.nhs.uk/Dictionary-A-Z/Definitions/index.asp?Search=O&ID=374&Title=Outpatient%20Attendance",
                            "Data Dictionary",  target="_blank"),
                     ". Please note that there is a time lag between the submission of ",
                     "SMR00 to PHS, and the data being validated and ready for release. ",
                     "Therefore, weekly data up to 26th September 2021 and monthly data up to 30th September 2021 are given. For data quality issues, please see the ", 
                     tags$a(href = "https://publichealthscotland.scot/publications/acute-hospital-activity-and-nhs-beds-information-quarterly/",
                            "Acute Activity and NHS Beds quarterly publication",  

                            target="_blank"), ". All information presented has been ",
                     "taken from SMR00; this is different from the Acute Activity ",
                     "and NHS Beds publication, so the figures are not comparable."),
                   p("The SMR00 dataset is managed by Public Health Scotland (PHS). ",
                     "For current completeness estimates, please see ",
                     tags$a(href = "https://beta.isdscotland.org/products-and-services/data-management-hospital-activity/smr-completeness/",
                            "the PHS website", target = "_blank"), "."),
                   p(tags$a(href = "https://www.publichealthscotland.scot/publications/statistical-disclosure-protocol/statistical-disclosure-protocol/",
                            "Statistical disclosure control", target = "_blank"), 

                     "has been applied to this analysis."),
                   size = "m",
                   easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
               }
             )

###############################################.
# Modal to explain SIMD and deprivation
simd_modal <- modalDialog(
  h5("What is SIMD and deprivation?"),
  p("The", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD).",
                   target="_blank"), "is the Scottish Government's
    official tool for identifying areas in Scotland with concentrations of deprivation
    by incorporating several different aspects of deprivation (multiple-deprivations)
    and combining them into a single index. Concentrations of deprivation are identified
    in SIMD at Data Zone level and can be analysed using this small geographical unit.
    The use of data for such small areas helps to identify 'pockets' (or concentrations)
    of deprivation that may be missed in analyses based on larger areas such as council
    areas. By identifying small areas where there are concentrations of multiple deprivation,
    the SIMD can be used to target policies and resources at the places with the greatest need.
    The SIMD identifies deprived areas, not deprived individuals."),
  p("In this tool we use the concept of quintile, which refers to a fifth of the population.
    For example when we talk about the most deprived quintile, this means the 20% of the population
    living in the most deprived areas."),
  size = "l",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
# Link action button click to modal launch
observeEvent(input$btn_modal_simd, { showModal(simd_modal) })


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
  background. More information can be found ", tags$a(href="https://www.ndc.scot.nhs.uk/Dictionary-A-Z/Definitions/index.asp?Search=E&ID=243&Title=Ethnic%20Group", "here.",
         target="_blank")),
  p("It became mandatory for NHS Scotland organisations to record ethnic group 
    on SMR outpatient (SMR00) returns from 1 February 2021. There is currently
  significant variation in the completeness of ethnic group recording in new 
  outpatient appointment records between NHS Boards. More information can be found 
    ", tags$a(href="https://www.isdscotland.org/products-and-Services/Data-Support-and-Monitoring/SMR-Ethnic-Group-Recording/",
              "here.", target="_blank")),
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
    filter(area_name == input$geoname &
             admission_type == input$adm_type &
             category == "All" &
             spec %in% input$adm_specialty)

})

# Outpatients dataset filtered for admission_type, then used to create the admissions charts
op_filt <- reactive({
  outpats %>%
    filter(admission_type == input$appt_type &
             spec == "All" &
             area_type == input$geotype_op &
             time_split == input$time_type)
})

# # Outpatients dataset used for specialty charts
op_spec <- reactive({
  outpats %>%
    filter(type == "sex") %>%
    filter(area_name == input$geoname_op &
             admission_type == input$appt_type &
             category == "All" &
             spec %in% input$op_specialty &
             area_type == input$geotype_op &
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

###############################################.
## Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$data_explorer <- renderUI({

  # text for titles of cut charts
  dataset <- case_when(input$measure_select == "rapid" ~ "admissions",
                       input$measure_select == "aye" ~ "attendances",
                       input$measure_select == "nhs24" ~ "completed contacts",
                       input$measure_select == "ooh" ~ "cases",
                       input$measure_select == "sas" ~ "incidents",
                       input$measure_select == "deaths" ~ "deaths",
                       input$measure_select == "outpats" ~ "appointments")

  if (input$measure_select == "deaths"){
    variation_title <- paste0("Percentage change in ", dataset,
                            " compared with the corresponding time in 2015-2019 by ")   #different averaging period for deaths
  } else {
    variation_title <- paste0("Percentage change in ", dataset,
                              " compared with the corresponding time in 2018-2019 by ")
  }

  if(input$measure_select == "outpats"){
    if(input$time_type == "Monthly"){
      time_period <- "Monthly"
    } else{
      time_period <- "Weekly"
    }
  } else{
      time_period <- "Weekly"
    }
  
  total_title <- paste0(time_period, " number of ", dataset, " by ")
  

  # To make sure that both titles take the same space and are lined up doing
  # a bit of a hacky shortcut:
  diff_chars <- nchar(variation_title) - nchar(total_title) +10
  extra_chars <- paste0(c(rep("_", diff_chars), "."), collapse = '')

  #update date for outpatients and the rest is different
  upd_date_summ <- case_when(input$measure_select == "outpats" ~ "9 March 2022",
                             TRUE ~ "6 April 2022")

  # Function to create the standard layout for all the different charts/sections
  cut_charts <- function(title, source, data_name) {
    tagList(
      h3(title),
      fluidRow(column(6,
                      actionButton("btn_dataset_modal", paste0("Data source: ", source),
                                   icon = icon('question-circle'))),

               column(6, p("Last updated: ", upd_date_summ))),

      if (input$measure_select == "nhs24"){
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
      if (input$measure_select == "deaths"){
        tagList(
        p("The analyses below are derived from the National Records of Scotland (NRS) weekly deaths dataset (provisional numbers).
          Numbers of deaths represent the total number of deaths (from any cause) that were registered in
          Scotland in any particular week.  Comparing the number of deaths in the most recent weeks to the
          average over the past 5 years allows estimation of the numbers of excess deaths.
          Volatility of the trends will be observed in some charts due to small counts."),
        plot_box(paste0("2020 and 2021 compared with the 2015-2019 average"), paste0(data_name, "_overall"))) #different averaging period for deaths
        } else if (input$measure_select == "outpats") {
          plot_box(paste0("2020 and 2021 compared with the 2018-2019 average"), paste0(data_name, "_overall"))
        } else {
          plot_box(paste0("2020 and 2021 compared with the 2018-2019 average"), paste0(data_name, "_overall"))
        },
      plot_cut_box(paste0(variation_title, "sex"), paste0(data_name, "_sex_var"),
                   paste0(total_title, "sex"), paste0(data_name, "_sex_tot")),
      plot_cut_box(paste0(variation_title, "age group"), paste0(data_name, "_age_var"),
                   paste0(total_title, "age group"), paste0(data_name, "_age_tot")),
      fluidRow(column(6, h4(paste0(variation_title, "SIMD quintile"))),
               column(6, h4(paste0(total_title, "SIMD quintile")))),
      fluidRow(actionButton("btn_modal_simd", "What is SIMD and deprivation?",
                            icon = icon('question-circle'))),
      fluidRow(column(6, withSpinner(plotlyOutput(paste0(data_name, "_depr_var")))),
               column(6, withSpinner(plotlyOutput(paste0(data_name, "_depr_tot")))))
    )

  }

  # Charts and rest of UI
  if (input$measure_select == "rapid") {
    tagList(#Hospital admissions
      cut_charts(title= "Weekly admissions to hospital", source = "PHS RAPID Datamart",
                 data_name = "adm"),
      fluidRow(column(6, h4(paste0(variation_title, "specialty group - (admission type: ", tolower(input$adm_type), ")"))), # Adding adm_type here to make clear what is selected
               column(6, h4(paste0(total_title, "specialty group - (admission type: ", tolower(input$adm_type), ")")))), # Adding adm_type here to make clear what is selected
      fluidRow(column(6, pickerInput("adm_specialty", "Select one or more specialty groups",
                                     choices = if (input$geotype == "Scotland") {
                                       spec_list_rapid} else {
                                         spec_list_rapid[c(1:8,11)]}, multiple = TRUE,
                                     selected = c("Medical (incl. Cardiology & Cancer)", "Surgery", "Paediatrics (medical & surgical)"))),
               column(6, actionButton("btn_spec_groups_rapid",
                                      "Specialties and their groups",
                                      icon = icon('question-circle')))),
      fluidRow(column(6, withSpinner(plotlyOutput("adm_spec_var"))),
               column(6, withSpinner(plotlyOutput("adm_spec_tot"))))
    )
  } else if (input$measure_select == "aye") {
    tagList(#A&E Attendances
    tags$em("Please note that a data recording issue has been identified and was rectified on 3/9/21 for the gender, age
            and SIMD data for the week ending 20 September 2020, mainly affecting Perth and Kinross HSCP and NHS Tayside."),
    cut_charts(title= "Weekly attendances to A&E departments",
               source = "PHS AE2 Datamart", data_name = "aye"))

  } else if (input$measure_select == "nhs24") {# NHS 24 calls
    cut_charts(title= "Weekly completed contacts with NHS 24",
               source = "PHS Unscheduled Care Datamart", data_name ="nhs24")

  } else if (input$measure_select == "ooh") { #Out of hours cases
    tagList(
    tags$b(span("Please note that the data on this page excludes individuals coming to
                Primary Care Out of Hours services via the COVID Pathway. PHS are
                investigating this to better reflect Primary Care Out of Hours service
                provision.", style = "color:red")),
    br(),

    cut_charts(title= "Weekly cases in out of hours services",
               source = "PHS GP OOH Datamart", data_name ="ooh"))

  } else if (input$measure_select == "sas") {
    tagList(# SAS data
      tags$em(p("Please note that there is currently an issue with duplicates in the SAS dataset and
              therefore the data was not updated in April 2022. This is currently being 
              investigated by PHS and the data will be updated as soon as possible."),
              p("SAS currently publish weekly unscheduled care operational statistics at the following ", 
        tags$a(href="https://www.scottishambulance.com/publications/unscheduled-care-operational-statistics/", 
               "Unscheduled Care Operational Statistics", target="_blank"), ". The data published by SAS is sourced from a 
        different operational system than that used for the PHS reporting. This means that the data published 
        by SAS will at times be slightly different to that reported by PHS source.")),
    cut_charts(title= "Weekly attended incidents by Scottish Ambulance Service",
               source = "PHS Unscheduled Care Datamart", data_name ="sas"))

  } else if (input$measure_select == "deaths") { # Deaths data
    cut_charts(title= "Weekly number of deaths",
               source = "NRS Death Registrations", data_name ="deaths")

  } else if (input$measure_select == "outpats") { # Outpatients data
      eth_op_ui <- tagList(#Add ethnicity charts
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
      )
      
    if(input$time_type == "Weekly"){
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
              eth_op_ui
                                          
      )
    } else if(input$time_type == "Monthly"){
      
      tagList(tags$b(span("Please note that these data are for management information only, and care should",
                          "be taken when interpreting these figures. For more information on methodology and data quality please see the ",
                          tags$a(href = "https://publichealthscotland.scot/publications/acute-hospital-activity-and-nhs-beds-information-quarterly/",
                                 "Acute Activity and NHS Beds quarterly publication",  
                                 target="_blank"), ". Did Not Attend appointments (DNAs) are not included in the figures shown here.",
                          style="color:red")),
              cut_charts(title= paste0(time_period, " outpatient appointments"),
                         source = "SMR00", data_name = "monthly_op"),
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
                              withSpinner(plotlyOutput("monthly_op_spec_var"))),
                       column(6,
                              withSpinner(plotlyOutput("monthly_op_spec_tot")))),
              fluidRow(column(6,
                              h4(paste0(variation_title, "mode of clinical interaction"))),
                       # Adding adm_type here to make clear what is selected
                       column(6,
                              h4(paste0(total_title, "mode of clinical interaction")))),
              fluidRow(actionButton("btn_modal_moc", "Interpretation of this chart", 
                                    icon = icon('fas fa-exclamation-circle'))),
              fluidRow(column(6,
                              withSpinner(plotlyOutput("monthly_op_moc_var"))),
                       column(6,
                              withSpinner(plotlyOutput("monthly_op_moc_tot")))),
              eth_op_ui
      )

    } 
  }
})



###############################################.
## Charts ----
###############################################.
###############################################.
# Creating plots for each cut and dataset
# A&E charts
output$aye_overall <- renderPlotly({plot_overall_chart(aye, data_name = "aye")})
output$aye_sex_var <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex", data_name = "aye")})
output$aye_age_var <- renderPlotly({plot_trend_chart(aye, pal_age, "age", data_name = "aye")})
output$aye_depr_var <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep", data_name = "aye")})
output$aye_sex_tot <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex", "total", "aye")})
output$aye_age_tot <- renderPlotly({plot_trend_chart(aye, pal_age, "age", "total", "aye")})
output$aye_depr_tot <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep", "total", "aye")})

# OOH charts
output$ooh_overall <- renderPlotly({plot_overall_chart(ooh, "ooh")})
output$ooh_sex_var <- renderPlotly({plot_trend_chart(ooh, pal_sex, "sex", data_name = "ooh")})
output$ooh_age_var <- renderPlotly({plot_trend_chart(ooh, pal_age, "age", data_name = "ooh")})
output$ooh_depr_var <- renderPlotly({plot_trend_chart(ooh, pal_depr, "dep", data_name = "ooh")})
output$ooh_sex_tot <- renderPlotly({plot_trend_chart(ooh, pal_sex, "sex", "total", "ooh")})
output$ooh_age_tot <- renderPlotly({plot_trend_chart(ooh, pal_age, "age", "total", "ooh")})
output$ooh_depr_tot <- renderPlotly({plot_trend_chart(ooh, pal_depr, "dep", "total", "ooh")})

# NHS24 charts
output$nhs24_overall <- renderPlotly({plot_overall_chart(nhs24, "nhs24")})
output$nhs24_sex_var <- renderPlotly({plot_trend_chart(nhs24, pal_sex, "sex", data_name = "nhs24")})
output$nhs24_age_var <- renderPlotly({plot_trend_chart(nhs24, pal_age, "age", data_name = "nhs24")})
output$nhs24_depr_var <- renderPlotly({plot_trend_chart(nhs24, pal_depr, "dep", data_name = "nhs24")})
output$nhs24_sex_tot <- renderPlotly({plot_trend_chart(nhs24, pal_sex, "sex", "total", "nhs24")})
output$nhs24_age_tot <- renderPlotly({plot_trend_chart(nhs24, pal_age, "age", "total", "nhs24")})
output$nhs24_depr_tot <- renderPlotly({plot_trend_chart(nhs24, pal_depr, "dep", "total", "nhs24")})

# SAS charts
output$sas_overall <- renderPlotly({plot_overall_chart(sas, "sas")})
output$sas_sex_var <- renderPlotly({plot_trend_chart(sas, pal_sex, "sex", data_name = "sas")})
output$sas_age_var <- renderPlotly({plot_trend_chart(sas, pal_age, "age", data_name = "sas")})
output$sas_depr_var <- renderPlotly({plot_trend_chart(sas, pal_depr, "dep", data_name = "sas")})
output$sas_sex_tot <- renderPlotly({plot_trend_chart(sas, pal_sex, "sex", "total", "sas")})
output$sas_age_tot <- renderPlotly({plot_trend_chart(sas, pal_age, "age", "total", "sas")})
output$sas_depr_tot <- renderPlotly({plot_trend_chart(sas, pal_depr, "dep", "total", "sas")})

# Admissions to hospital charts
output$adm_overall <- renderPlotly({plot_overall_chart(rapid_filt(), "adm")})
output$adm_sex_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_sex, "sex", data_name = "adm")})
output$adm_age_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_age, "age", data_name = "adm")})
output$adm_depr_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_depr, "dep", data_name = "adm")})
output$adm_sex_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_sex, "sex", "total", "adm")})
output$adm_age_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_age, "age", "total", "adm")})
output$adm_depr_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_depr, "dep", "total", "adm")})
output$adm_spec_var <- renderPlotly({plot_spec("variation", rapid_spec())})
output$adm_spec_tot <- renderPlotly({plot_spec("total", rapid_spec())})

# Deaths charts
output$deaths_overall <- renderPlotly({plot_overall_chart(deaths, "deaths")})
output$deaths_sex_var <- renderPlotly({plot_trend_chart(deaths, pal_sex, "sex", data_name = "deaths")})
output$deaths_age_var <- renderPlotly({plot_trend_chart(deaths, pal_age, "age", data_name = "deaths")})
output$deaths_depr_var <- renderPlotly({plot_trend_chart(deaths, pal_depr, "dep", data_name = "deaths")})
output$deaths_sex_tot <- renderPlotly({plot_trend_chart(deaths, pal_sex, "sex", "total", "deaths")})
output$deaths_age_tot <- renderPlotly({plot_trend_chart(deaths, pal_age, "age", "total", "deaths")})
output$deaths_depr_tot <- renderPlotly({plot_trend_chart(deaths, pal_depr, "dep", "total", "deaths")})

# Outpatients charts
output$op_overall <- renderPlotly({plot_overall_chart(op_filt(), "op", op = T)})
output$op_sex_var <- renderPlotly({plot_trend_chart(op_filt(), pal_sex, "sex", data_name = "op")})
output$op_age_var <- renderPlotly({plot_trend_chart(op_filt(), pal_age, "age", data_name = "op")})
output$op_depr_var <- renderPlotly({plot_trend_chart(op_filt(), pal_depr, "dep", data_name = "op")})
output$op_moc_var <- renderPlotly({plot_trend_chart(op_filt(), pal_moc, "moc", data_name = "op")})
output$op_sex_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_sex, "sex", "total", "op")})
output$op_age_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_age, "age", "total", "op")})
output$op_depr_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_depr, "dep", "total", "op")})
output$op_moc_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_moc, "moc", "total", "op")})
output$op_spec_var <- renderPlotly({plot_spec("variation", op_spec(), marg = 80, op = T)})
output$op_spec_tot <- renderPlotly({plot_spec("total", op_spec(), marg = 80, op = T)})

# Monthly Outpatients charts
output$monthly_op_overall <- renderPlotly({plot_overall_chart(op_filt(), "op", op = T, period = "monthly")})
output$monthly_op_sex_var <- renderPlotly({plot_trend_chart(op_filt(), pal_sex, "sex", data_name = "op", period = "monthly")})
output$monthly_op_age_var <- renderPlotly({plot_trend_chart(op_filt(), pal_age, "age", data_name = "op", period = "monthly")})
output$monthly_op_depr_var <- renderPlotly({plot_trend_chart(op_filt(), pal_depr, "dep", data_name = "op", period = "monthly")})
output$monthly_op_moc_var <- renderPlotly({plot_trend_chart(op_filt(), pal_moc, "moc", data_name = "op", period = "monthly")})
output$monthly_op_sex_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_sex, "sex", "total", "op", period = "monthly")})
output$monthly_op_age_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_age, "age", "total", "op", period = "monthly")})
output$monthly_op_depr_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_depr, "dep", "total", "op", period = "monthly")})
output$monthly_op_moc_tot <- renderPlotly({plot_trend_chart(op_filt(), pal_moc, "moc", "total", "op", period = "monthly")})
output$monthly_op_spec_var <- renderPlotly({plot_spec("variation", op_spec(), marg = 80, op = T, period = "monthly")})
output$monthly_op_spec_tot <- renderPlotly({plot_spec("total", op_spec(), marg = 80, op = T, period = "monthly")})


output$op_eth_tot <- renderPlotly({
  
  plot_nodata(text_nodata = "Data is only available for Scotland")
  plot_trend_chart(dataset = op_eth(), pal_chose = pal_eth, split = "eth", type = "total", 
                   data_name = "op", period = "monthly")})


# plot_nodata <- function(height_plot = 450, text_nodata = "Data not available due to small numbers") {
#   text_na <- list(x = 5, y = 5, text = text_nodata , size = 20,
#                   xref = "x", yref = "y",  showarrow = FALSE)

 
output$op_eth_var <- renderPlotly({
  plot_trend_chart(dataset = op_eth(), pal_chose = pal_eth, split = "eth", 
                   data_name = "op", period = "monthly")})


# Palette for specialty
pal_spec <- reactive({
  #Creating palette of colors: colorblind proof
  #First obtaining length of each geography type, if more than 6, then 6,
  # this avoids issues. Extra selections will not be plotted
  if (input$measure_select == "outpats") {
    trend_length <- length(input$op_specialty)
  } else {
    trend_length <- length(input$adm_specialty)
  }

  # First define the palette of colours used, then set a named vector, so each color
  # gets assigned to an area. I think is based on the order in the dataset.
  trend_palette <- c("#000000", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                     "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928")

  if (input$measure_select == "outpats") {
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

  if (input$measure_select == "outpats") {
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

# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
overall_data_download <- reactive({
  switch(
    input$measure_select,
    "rapid" = filter_data(rapid_filt() %>% rename(average_2018_2019 = count_average)) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "aye" = filter_data(aye) %>% rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "nhs24" = filter_data(nhs24) %>% rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "ooh" = filter_data(ooh) %>% rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "sas" = filter_data(sas) %>% rename(average_2018_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "deaths" = filter_data(deaths) %>% rename(average_2015_2019 = count_average) %>% 
      mutate(week_ending = format(week_ending, "%d %b %y")) %>%
      select(area_name, week_ending, count, starts_with("average")),
    "outpats" = filter_data(op_filt() %>% rename(average_2018_2019 = count_average), op = T) %>% 
      mutate(week_ending = ifelse(time_split == "Monthly", format(week_ending, "%b %y"),
                                  format(week_ending, "%d %b %y"))) %>%
      rename(time_ending = week_ending) %>%
      select(area_name, time_split, time_ending, count, starts_with("average"))
  )

})

output$download_chart_data <- downloadHandler(
  filename = function() {
    paste(input$measure_select, ".csv", sep = "")
  },
  content = function(file) {
    write_csv(overall_data_download(),
              file) }
)

###############################################.
## Commentary tab content  ----
###############################################.


output$summary_comment <- renderUI({
  tagList(
    bsButton("jump_to_summary",label = "Go to data"), #this button can only be used once

    h2("Summary - Outpatient appointments - 9th March 2022"),
    p("Data are taken from Scottish Morbidity Record (SMR00) and show weekly outpatient appointments to
      week ending 26th September 2021, with monthly information shown to 30th September 2021. Further information
      is available by following the 'Data source: SMR00' links on the dashboard."),
   
      h4("Initial findings: outpatient appointments"),
    tags$ul(
      tags$li("Outpatient appointments fell from the second week of March 2020 onwards: by week ending 19th April 2020,
              all outpatient appointments had fallen by over two-thirds (-68%) compared to the average of the same week in 2018-19
              (from an average of 87,049 in 2018-19 to 27,510 in 2020)."),
      tags$li("This impact was similar across sexes, age groups and deprivation groups. 
              For example, the fall in all appointments was greatest in patients aged 85 and over, dropping by almost three-quarters (-73%), 
              while appointments for patients aged 15-44 dropped by two-thirds (-66%). However, by the week ending 26th September 2021,
              these reductions were 10% for patients aged 85 and over and 13% for patients aged 15-44."),
      tags$li("There were larger relative falls for surgical (-76%) than medical (-64%) specialties in the early stages
              of the pandemic. However, by week ending 26th September 2021, medical specialties showed a reduction of over an eighth
              (-13%), while surgical specialties showed a reduction of around one fifth (-21%)
              compared to average values for the same week in 2018-19."),
      tags$li("There were larger decreases and a slower recovery in new outpatient appointments
              than in return outpatient appointments."),
      tags$li("Outpatient appointments have generally been recovering from the end of April 2020 onwards
               but are still not up to pre-pandemic levels. For example, for the week ending 26th September 2021, 
               the total number of appointments remains at around 16% below the average of the same week in
               2018-19."),
      tags$li("There has been a very large increase in the number of appointments carried out remotely via 
              telephone and videolink. In week ending 26th September 2021, just under one sixth (16%) of all
              appointments was conducted by telephone, and 1 in 25 (4%) was by videolink. These modes of clinical
              interaction were uncommon prior to March 2020 but have consistently made up around one fifth of 
              outpatient activity since then."),
      tags$li("The impact of the pandemic on outpatient appointments was similar across ethnic groups; however,
              interpretation by ethnic group is complicated by the mandating of recording of ethnic group on SMR outpatient (SMR00)
              returns from 1st February 2021. This is reflected in the fall in the number of appointments with a missing ethnic group, which
              were 22% lower by September 2021 than the corresponding time in 2018-19."),
      tags$li("In September 2021, appointments for patients with the 'White Scottish' ethnic group recorded were around 5% lower than the
              corresponding time in 2018-19; the number of appointments in other ethnic groups varies between 23% higher (African) and 6%
              lower (Chinese). It is important to note that the trends for ethnic groups with small populations should be interpreted with
              caution as they will be subject to greater variability due to small numbers.")
      ),
    h4("Interpreting these figures"),
    p("Please exercise caution when interpreting these figures, as these data are for management information only.
      For more information on methodology and data quality, please see the ",
      tags$a(href = "https://publichealthscotland.scot/publications/acute-hospital-activity-and-nhs-beds-information-quarterly/",
             "Acute Activity and NHS Beds quarterly publication.",
             target="_blank"),
       h2("Outpatient appointments by ethnic group - 16th February 2022"),
    p("New analyses by ethnic group have now been added. The COVID-19 pandemic has 
      highlighted the need for ongoing monitoring of health data by ethnicity. Reporting 
      analyses by ethnic group supports efforts to achieve equity in health care provision.
      Please note that the rest of the outpatients data remains unchanged from the last update
      on 15th December 2021."),
    h2("Summary - Revision of baseline OOH - 23rd September 2020"),
    p("An issue with previously published 2018 and 2019 baseline Out of Hours (OOH) data was
identified and has now been corrected. OOH figures from January 2018 to 22nd March 2020 had previously
referred to numbers of consultations whereas those presented after 23rd March referred to numbers of cases.
A correction has been applied to ensure that the full time series is now based on numbers of OOH cases.
The impact of this revision is modest and does not materially affect interpretation of the changes observed in
post-pandemic activity.
At a national level adjusting the baseline data has resulted in a reduction in the baseline OOH figure of approximately 10% (1,600).
The post-pandemic reductions in OOH activity previously reported were also over-estimated
by around 6% each week, and this has now been corrected. The impact of the data revisions at a sub-national level may vary."),
    h2("Summary - 3rd June 2020"),
          p("From the second week of March 2020 there was an abrupt and steep fall in hospital admissions,
attendances at Accident and Emergency (A&E) departments and cases in out of hours services.
Use of all of these services fell to around half the average levels seen 2018-19 and has since recovered
only slightly. Numbers of NHS 24 111 completed contacts did not change appreciably though the data presented
here do not include additional NHS 24 services specific to COVID-19. There was a small fall in attended
ambulance incidents. Further analyses are presented in this interactive online tool."),
          h4("Background"),
          p("The COVID-19 pandemic has direct impacts on health as a result of illness,
hospitalisations and deaths due to COVID-19. However, the pandemic also has wider impacts on health and
on health inequalities. Reasons for this may include:"),
          tags$ul(
            tags$li("Individuals being reluctant to use health services because they do not want to
burden the NHS or are anxious about the risk of infection."),
            tags$li("The health service delaying preventative and non-urgent care such as
some screening services and planned surgery."),
            tags$li("Other indirect effects of interventions to control COVID-19, such as mental or
physical consequences of distancing measures.")),
            p("Public Health Scotland aims to provide information and intelligence on the wider
impacts of COVID-19 on health, healthcare and health inequalities that are not directly due to COVID-19."),
            p("We have focused initially on using the national datasets that are returned to PHS most quickly,
as these allow us to monitor impacts with the minimum delay. The work to date has made use of the following data sources:"),
            tags$ul(
              tags$li("The RAPID (rapid and preliminary inpatient data) hospital admissions database."),
              tags$li("A&E attendances."),
              tags$li("NHS 24 completed contacts."),
              tags$li("Out of hours cases."),
              tags$li("Scottish Ambulance Service data.")),
          h4("Initial findings: hospital admissions"),
          tags$ul(
            tags$li("Hospital admissions fell sharply from the second week of March, reaching levels
nearly 50% below those expected on the basis of admissions during 2018-19."),
            tags$li("There has been some recovery since late April, but numbers of admissions remain
around 35% below the 2018-19 average."),
            tags$li("Similar patterns are seen by sex and by deprivation, but falls were larger for children
under 14 years and smaller for those aged 85 years and over."),
            tags$li("There were larger relative falls for surgical than medical specialties."),
            tags$li("There were much larger falls in planned admissions (around 65%) than in
emergency admissions (around 40%)."),
            tags$li("There were particularly large falls (around 60%) for emergency paediatric admissions."),
            tags$li("The pattern was broadly similar across NHS Boards; the low level of recorded admissions
in NHS Forth Valley is likely to be due to data quality problems.")),
          h4("Initial findings: unscheduled care"),
          tags$ul(
            tags$li("Other data sources showed changes with similar time patterns to those seen for
hospital admissions. There were larger falls (nearly 60%) for A&E attendances, with similar patterns by
age, sex and deprivation."),
            tags$li("NHS 24 111 completed contacts rose substantially for working age adults,
but fell to around 50% of previous levels for children under 15 years of age, with little sign of recovery
to previous levels. However it is important to note that while these figures include some contacts related
to COVID-19, they do not include additional services set up to respond directly to COVID-19.
Compared to previous years, percentage falls in completed contacts were smaller among those
living in more deprived areas."),
            tags$li("Compared to earlier years there were large percentage falls (around 55% overall) in
cases in out of hours services, especially for children, where the fall was around 70%."),
            tags$li("There were more modest falls in attended ambulance incidents (around 15% overall)
though the fall was much larger for children (around 50%). These figures include incidents related to COVID-19.")),
          h4("Interpreting these figures"),
          p("These analyses are based on a selected range of data sources that are available to describe
changes in health service use in Scotland during the COVID-19 pandemic. Hospital admissions, attendances
at A&E departments, contacts with the NHS 24 111 completed contacts and cases in out of hours
services all fell to around half the average levels seen 2018-19 and have since recovered only modestly.
There was a smaller fall in attended ambulance incidents and no appreciable change in NHS 24 111 completed
contacts. These falls are likely to reflect a range of factors, including public anxiety about using NHS
services, changes in the delivery of NHS services in response to rising numbers of COVID-19 hospital admissions
and actions to defer planned activity in order to be prepared for expected COVID-19 related demand.
The changes preceded by around a week the introduction of social distancing measures. The impact was
particularly large for children under 14 years, with larger percentage falls in hospital admissions, NHS 24 111
completed contacts, out of hours cases and ambulance incidents. As expected, the falls in hospital
admissions were larger for planned than for emergency admissions and larger for surgical than medical
admissions. There was little evidence from these data sources that social inequalities in the use of these
services increased during this period."),
          h4("Future work"),
          p("Work is under way to broaden the range of data sources available – within the next few weeks
we expect to publish information on health visitor checks, perinatal mortality,
excess mortality (in collaboration with NRS), prescribing and cardiovascular presentations"
          )))
})

output$deaths_commentary <- renderUI({
  tagList(
    h2("Excess mortality - 10th June 2020"),
    p("Each week National Records for Scotland (NRS) release provisional deaths data and a ",
      tags$a(href="https://www.nrscotland.gov.uk/covid19stats", "report",  target="_blank"),
" about the numbers of deaths involving COVID-19 in Scotland.
NRS report that weekly excess mortality (defined as deaths from any cause in 2020,
both COVID-19 and non-COVID-19, compared with the average of the previous five years)
peaked at 80% higher in the week ending 12 April, and had reduced to 17% higher by
the most recent week (ending 24 May)."),
    p("PHS are using the NRS data to provide further insight about excess mortality
by sex, age group, area deprivation (quintiles of Scottish Index of Multiple Deprivation 2020),
as well as at NHS Board and HSCP level. Thet numbers of deaths from any
cause increased markedly at all levels of area deprivation from early April 2020.
The excess deaths for each SIMD quintile compared with the 2015 to 2019 average
was between 72% and 98% in the week ending 19 April, and had reduced to less than 25%
for all quintiles by the latest week (ending 24 May).")
  )
})

#END
