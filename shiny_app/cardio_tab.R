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
    cardio_label = "Step 2 - Select a cardiac catheterization lab"
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
    cardio_label = "Step 2 - Select geography level for cardiovascular drug prescriptions"
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
                 p("This tool provides a weekly summary of people attending A&E departments (Emergency Departments 
                   and Nurse/GP led minor injury units) with cardiovascular problems. It shows data from the recent 
                   past along with historical activity for comparison purposes. The recent trend data is shown by 
                   age group (under and over 65) and broad deprivation category (SIMD)."),
                 p("Additional information relating to the overall A&E activity is available from the ", 
                   tags$a(href="https://beta.isdscotland.org/find-publications-and-data/health-services/hospital-care/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/", 
                          "NHS Performs - weekly update of emergency department activity and waiting time statistics.", 
                          class="externallink")),
                 p("There are two types of data submitted to the A&E datamart: episode and aggregate level data. 
                   All hospitals with Emergency Departments submit episode level data containing a detailed record 
                   for each patient attendance. Some smaller sites (6% of the total annual attendances) – nurse/GP 
                   led minor injury units – can only provide aggregated monthly summary attendance and compliance 
                   figures, as they do not have the IT systems and support to enable collection and submission of 
                   detailed patient level information. The data for sites that submit aggregate level data is not 
                   included in the figures presented in the tool. "),
                 p("Attendances to A&E departments data sourced from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=3", 
                          "Accident and Emergency Datamart (A&E2).",class="externallink"), 
                   "The A&E2 dataset is managed by ", 
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/", 
                          "Public Health Scotland (PHS).", class="externallink")),
                 p(tags$em("Please note that due to limitations in diagnosis recording in the A&E datamart, the 
                         figures reported for cardiology offer only a very rough indication of cardiovascular 
                         attendances and do not represent the exact figures for cardiovascular attendances at 
                         Emergency Departments. Part of the data quality issues is that the data on cardiovascular 
                         attendance only includes part or no data for NHS Ayrshire and Arran, NHS Fife, 
                         NHS Forth Valley and NHS Lothian.")),
                 p("The following ICD-10 codes were considered for the cardiovascular A&E data subset:"),
                 DT::dataTableOutput("ae_cardio_codes_tbl"),
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
                 p("In the majority of cases, electronic messages are generated when a GP10 prescription is issued 
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
                 p("The ePrescribed messaging dataset is managed by Public Health Scotland (PHS)."),
                 tags$hr(),
                 actionButton("toggleBNFCodes", "Show / Hide BNF Codes"),
                 shinyjs::onclick("toggleBNFCodes",
                                  shinyjs::toggle(id = "BNFCodes")),
                 shinyjs::hidden(div(id="BNFCodes",
                     br(),
                     tags$b("Hypertension, ischaemic heart disease and heart failure"),
                     HTML({
                       "
                       <table style='width:100%'>
                       <tr>
                       <th colspan='2'>BNF Legacy</th>
                       </tr>
                       <tr>
                       <td>0201</td>
                       <td>Positive inotrpic drugs</td>
                       </tr>
                       <tr>
                       <td>0203</td>
                       <td>Anti-arrhythmic drugs</td>
                       </tr>
                       <tr>
                       <td>0204</td>
                       <td>Beta-adrenoreceptor blocking drugs</td>
                       </tr>
                       <tr>
                       <td>0205</td>
                       <td>Antihypertensives</td>
                       </tr>
                       <tr>
                       <td>0207</td>
                       <td>Sympathomimetics</td>
                       </tr>
                       <tr>
                       <td>020505</td>
                       <td>Drugs affecting the renin-angiotensin system</td>
                       </tr>
                       <tr>
                       <td>020601</td>
                       <td>Nitrates</td>
                       </tr>
                       <tr>
                       <td>020602</td>
                       <td>Calcium channel blockers</td>
                       </tr>
                       <tr>
                       <td>020604</td>
                       <td>Peripheral vasodilator and related drugs</td>
                       </tr>
                       </table>
                       "
                     }),
                     br(),
                     tags$b("Antiplatelet drugs"),
                     HTML({
                       "
                       <table style='width:100%'>
                       <tr>
                       <th colspan='2'>BNF Legacy</th>
                       </tr>
                       <tr>
                       <td>0209</td>
                       <td>Antiplatelet drugs</td>
                       </tr>
                       </table>
                       "
                 }),
                     br(),
                     tags$b("Oral anticoagulants"),
                     HTML({
                       "
                       <table style='width:100%'>
                       <tr>
                       <th colspan='2'>BNF Legacy</th>
                       </tr>
                       <tr>
                       <td>020802</td>
                       <td>Oral anticoagulants</td>
                       </tr>
                       </table>
                       "
                     }))),
                 size = "l",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_cardio_select == "cath") {
               showModal(modalDialog(#ALL  MODAL
                 title = "What is the data source?",
                 p("This tool provides an overview on how procedures procedures in the field of 
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
                           problems of the heart) and loop recorders (used to investigate treat rhythm problems 
                           of the heart). Fitting of cardiac devices may also be carried out in other environments 
                           so these figures do not represent the total volume of cardiac devices fitted.")
                 ),
                 p("Note that during the COVID-19 lockdown period the Golden Jubilee National Hospital 
                   extended their catheterisation lab catchment area to cover Ayrshire & Arran, Dumfries & Galloway 
                   and southeast Glasgow, areas which previously would have gone to Hairmyres Hospital. 
                   In addition, during 2019 increased catheterisation lab activity was seen at the Golden Jubilee National 
                    Hospital activity due to the presence of the temporary mobile lab which was not present during other 
                   time periods."),
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
                           problems of the heart) and loop recorders (used to investigate treat rhythm problems 
                           of the heart). Fitting of cardiac devices may also be carried out in other environments 
                           so these figures do not represent the total volume of cardiac devices fitted.")
                 ),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
)

# Rendering A&E Cardio Codes table here for inclusion to modal above
output$ae_cardio_codes_tbl <- DT::renderDataTable(
  ae_cardio_codes
)

###############################################.
## Reactive datasets ----
###############################################.


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
        p("While the data presented here is not a complete picture of the cardiovascular procedures carried out
            in Scotland, it includes the number of cardic procedures carried out by two out of the four 
            catheterisation labs in Scotland. These two labs provide services to the majority of the population
            living in the Central Belt and south of Scotland."),
        h3(paste0("Weekly visits to the cardiac catheterization labs at the ", lab_chosen)),
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
        tags$em("Please note that due to limitations in diagnosis recording in the A&E datamart, the 
                         figures reported for cardiology offer only a very rough indication of cardiovascular 
                         attendances and do not represent the exact figures for cardiovascular attendances at 
                         Emergency Departments. Part of the data quality issues is that the data on cardiovascular 
                         attendance only includes part or no data for NHS Ayrshire and Arran, NHS Fife, 
                         NHS Forth Valley and NHS Lothian."),
        h3("Weekly cardiovascular A&E attendances in Scotland"),
        actionButton("btn_cardio_modal", "Data source: PHS AE2 Datamart", icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "ae_cardio_overall"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances in Scotland compared with the corresponding
                     time in 2018-2019 by age group", "ae_cardio_age_var",
                     "Weekly number of cardiovascular A&E attendances in Scotland by age group", "ae_cardio_age_tot"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances in Scotland compared with the corresponding
                     time in 2018-2019 by SIMD quintile", "ae_cardio_dep_var",
                     "Weekly number of cardiovascular A&E attendances in Scotland by SIMD quintile", "ae_cardio_dep_tot")
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
#Cath labs RIE charts
output$cath_overall <- renderPlotly({
  plot_overall_chart(cath_lab %>% filter(groups == "Angiography" & lab == input$area_cardio_select), 
                     "cath", area = F)})
output$cath_sex_var <- renderPlotly({
  plot_trend_chart(cath_lab %>% filter(groups == "Angiography"  & type == "sex"
                                       & lab == input$area_cardio_select), pal_sex)})
output$cath_sex_tot <- renderPlotly({
  plot_trend_chart(cath_lab %>% filter(groups == "Angiography" & type == "sex" 
                                       & lab == input$area_cardio_select),
                   pal_sex, type = "total", data_name = "cath")})
output$cath_age_var <- renderPlotly({
  plot_trend_chart(cath_lab %>% filter(groups == "Angiography"  & type == "age" 
                                       & lab == input$area_cardio_select), pal_2ages)})
output$cath_age_tot <- renderPlotly({
  plot_trend_chart(cath_lab %>% filter(groups == "Angiography"  & type == "age" 
                                       & lab == input$area_cardio_select), 
                   pal_2ages, type = "total", data_name = "cath")})

output$cath_type_var <- renderPlotly({
  plot_trend_chart(cath_lab %>% filter(category == "All" & lab == input$area_cardio_select) %>% 
                     select(-category) %>% rename(category = groups), pal_sex)})
output$cath_type_tot <- renderPlotly({
  plot_trend_chart(cath_lab %>% filter(category == "All" & lab == input$area_cardio_select) %>% 
                     select(-category) %>% rename(category = groups), 
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
output$cardio_drugs_var <- renderPlotly({plot_trend_chart(cardio_drugs, pal_con, c("condition"), data_name = "drug_presc", tab = "cardio")})
output$cardio_drugs_tot <- renderPlotly({plot_trend_chart(cardio_drugs, pal_con, c("condition"), "total", data_name = "drug_presc", tab = "cardio")})
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
    "cath" = filter_data(cath_lab, area = F) %>% filter(lab == input$area_cardio_select),
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


##END