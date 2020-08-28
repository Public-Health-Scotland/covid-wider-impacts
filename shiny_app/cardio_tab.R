# Server side for cardiovascular tab

###############################################.
## Reactive controls  ----
###############################################.

# Helper function
`%notin%` <- Negate(`%in%`)
# Show list of area names depending on areatype selected
output$geoname_cardio_ui <- renderUI({

  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == ifelse(input$area_cardio_select %notin% 
                                                                            c("Scotland", "Health board", "HSC partnership"), 
                                                                          "Scotland", 
                                                                          input$area_cardio_select)])

  selectizeInput("geoname_cardio", label = NULL,
                 choices = areas_summary, selected = "")

})

# Adding 'observeEvent' to allow reactive 'area of interest' selction on cardio tab
observeEvent(input$measure_cardio_select, {
  x <- input$measure_cardio_select
  
  if (x == "cath") {
    cardio_label = "Step 2 - Select a cardiac catheterisation lab"
    cardio_choices = c("All", "Royal Infirmary of Edinburgh", "Golden Jubilee National Hospital")
    hide("geoname_cardio_ui")
    enable("area_cardio_select")
  }
  
  if (x == "aye") {
    cardio_label = "Step 2 - Select geography level for cardiovascular A&E attendances"
    cardio_choices = c("Scotland")
    hide("geoname_cardio_ui")
    disable("area_cardio_select")
  }
  
  if (x == "drug_presc") {
    cardio_label = "Step 2 - Select geography level for cardiovascular medicine prescriptions"
    cardio_choices = c("Scotland", "Health board", "HSC partnership")
    shinyjs::show("geoname_cardio_ui")
    enable("area_cardio_select")
  }
  
  updateSelectInput(session, "area_cardio_select",
                    label = cardio_label,
                    choices = cardio_choices,
                    selected = cardio_choices[1]
  )
}, ignoreNULL= F)

###############################################.
## Modals ----
###############################################.

# Link action button click to modal launch 
observeEvent(input$btn_cardio_modal, 
             
             if (input$measure_cardio_select == "rapid") {
               showModal(modalDialog(#RAPID ADMISSIONS MODAL
                 title = "What is the data source?",
                 p(""),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }  else if (input$measure_cardio_select == "aye") {
               showModal(modalDialog(# Cardio A&E MODAL
                 title = "What is the data source?",
                 p("This tool provides a weekly summary of people attending A&E departments (Emergency Departments) 
                    in the recent past, along with historical activity for 
                   comparison purposes. The recent trend data is shown by age group, sex
                   and broad deprivation category (SIMD). This data only include Emergency Department 
                   attendances and do not include minor injury units and other small hospitals and 
                   health centres in rural areas that carry out emergency department related activity, 
                   for more information on what sites are included please see this ", 
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/Hospital-Site-List/",
                          "hospital list.", class="externallink")),
                 p("Additional information relating to the overall A&E activity is available from the ", 
                   tags$a(href="https://beta.isdscotland.org/find-publications-and-data/health-services/hospital-care/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/", 
                          "NHS Performs - weekly update of emergency department activity and waiting time statistics.", 
                          class="externallink")),
                 p("Attendances to A&E departments data sourced from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=3", 
                          "Accident and Emergency Datamart (A&E2).",class="externallink"), 
                   "The A&E2 dataset is managed by ", 
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/", 
                          "Public Health Scotland (PHS).", class="externallink")),
                 p(tags$em("Please note that, due to limitations in diagnosis recording in the A&E datamart, the data are 
                            incomplete for a number of NHS Boards. Thus, the figures reported for cardiovascular-related 
                            attendances offer only a very approximate indication of attendances. 
                            Additionally, some NHS Boards have moved to a new recording standard which 
                            has not been fully consolidated in the A&E datamart as yet. As a result, figures for 2020, 
                            even prior to the introduction of lockdown measures, appear somehwat lower when compared to 
                            previous years.")),
                 p("The table below shows the ICD-10 codes that were considered for the cardiovascular A&E data subset, 
                   where this information was available."),
                 actionButton("toggleCodeTable", "Show / Hide Table"),
                 shinyjs::onclick("toggleCodeTable",
                                  shinyjs::toggle(id = "CodeTable")),
                 shinyjs::hidden(div(id = "CodeTable", br(), DT::dataTableOutput("ae_cardio_codes_tbl"))),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_cardio_select == "drug_presc") {
               showModal(modalDialog(#Prescribing - Cardio Drugs
                 title = "What is the data source?",
                 p("This section of the PHS Covid-19 - Wider Impacts dashboard provides weekly information on the 
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
                   or the following week, encompass public holidays.  Consequently, the number of prescribing 
                   days and measures of activity at the start of each year can be markedly reduced compared 
                   to subsequent weeks.  A similar effect also occurs in the last two weeks of the year."),
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
             } else if (input$measure_cardio_select == "cath") {
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
observeEvent(input$btn_modal_simd_cardio, { showModal(simd_modal) }) 


###############################################.
## Reactive datasets ----
###############################################.
cath_lab_chosen <- reactive({
  cath_lab %>% filter(lab == input$area_cardio_select) %>% droplevels()
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
  # Charts and rest of UI
  if (input$measure_cardio_select == "cath") {
    lab_chosen <- case_when(input$area_cardio_select == "All" ~ "Royal Infirmary of Edinburgh and Golden Jubilee National Hospital",
                            TRUE ~ paste0(input$area_cardio_select))
    
      tagList( # Cath labs
        p("At present charts include data from only two of the four Scottish catheterisation labs.
          Data from the remaining two centres will be added as it becomes available."),
        h3(paste0("Weekly visits to the cardiac catheterisation labs at the ", lab_chosen)),
        actionButton("btn_cardio_modal", paste0("Data source: ", lab_chosen), icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "cath_overall"),
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
    } else if (input$measure_cardio_select == "aye") {
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
        actionButton("btn_cardio_modal", "Data source: PHS AE2 Datamart", icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "ae_cardio_overall"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances in Scotland compared with the corresponding
                     time in 2018-2019 by age group", "ae_cardio_age_var",
                     "Weekly number of cardiovascular A&E attendances in Scotland by age group", "ae_cardio_age_tot"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances in Scotland compared with the corresponding
                     time in 2018-2019 by SIMD quintile", "ae_cardio_dep_var",
                     "Weekly number of cardiovascular A&E attendances in Scotland by SIMD quintile", "ae_cardio_dep_tot",
                     extra_content = actionButton("btn_modal_simd_cardio", "What is SIMD and deprivation?", 
                                                  icon = icon('question-circle')))
      )
    } else if (input$measure_cardio_select == "drug_presc") {
      tagList(# Prescribing - items dispensed
        h3(paste0("Weekly number of cardiovascular medicines prescribed in ", input$geoname_cardio)),
        actionButton("btn_cardio_modal", "Data source: ePrescribed Messages", icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "prescribing_all"),
        plot_cut_box(paste0("Percentage change in cardiovascular medicines prescribed in ", input$geoname_cardio, " compared with the corresponding
                     time in 2018-2019 by medicine groupings"), "cardio_drugs_var",
                     paste0("Weekly number of cardiovascular medicines prescribed in ", input$geoname_cardio, " by medicine groupings"), "cardio_drugs_tot")
      )
    }
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
output$prescribing_all <- renderPlotly({plot_overall_chart(cardio_drugs %>% filter(area_name == input$geoname_cardio), 
                                                           data_name = "drug_presc", area = "All")})
output$cardio_drugs_var <- renderPlotly({plot_trend_chart(cardio_drugs, pal_med, c("condition"), data_name = "drug_presc", tab = "cardio")})
output$cardio_drugs_tot <- renderPlotly({plot_trend_chart(cardio_drugs, pal_med, c("condition"), "total", data_name = "drug_presc", tab = "cardio")})
###############################################.
## Data downloads ----
###############################################.

overall_cardio_download <- reactive({
  
  # Branching this so that depending on input the right variables and names can be used
  # Cath branch
  if (input$measure_cardio_select == "cath") {
    selection <- c("week_ending", "count", "count_average", "variation")
    new_var_name <- "count_2019"
  }
  # A&E branch
  if (input$measure_cardio_select == "aye") {
    selection <- c("week_ending", "area_name", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }
  # Prescribing
  if (input$measure_cardio_select == "drug_presc") {
    selection <- c("week_ending", "area_name", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }
  
  # Prep data for download
  switch(
    input$measure_cardio_select,
    "cath" = filter_data(cath_lab_over(), area = F),
    "aye" = filter_data(ae_cardio, area = F),
    "drug_presc" = filter_data(cardio_drugs, area = F)
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