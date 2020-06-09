# Server side for cardiovascular tab

###############################################.
## Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
output$geoname_cardio_ui <- renderUI({

  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$area_cardio_select])

  selectizeInput("geoname_cardio", label = NULL,
                 choices = areas_summary, selected = "")

})

# Adding 'observeEvent' to allow reactive 'area of interest' selction on cardio tab
observeEvent(input$measure_cardio_select, {
  x <- input$measure_cardio_select
  
  if (x == "cath") {
    cardio_label = "Step 2 - Select the area of interest for cardiac catheterization labs"
    cardio_choices = c("All", "Royal Infirmary of Edinburgh", "Golden Jubilee Hospital")
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
               showModal(modalDialog(#Prescribin - Cardio Drugs
                 title = "What is the data source?",
                 p("This section of the PHS Covid-19 - Wider Impacts dashboard provides weekly information on the 
                   number of cardiovascular drug items prescribed. The data ranges from the start of 2020 to the
                   latest available week and is shown alongside historical activity (average from 2018 and 2019) for 
                   comparison purposes. Additional breakdowns by drug grouping are provided also (please see below 
                   for more details)."),
                 tags$b("What are ePrescribed Messages?"),
                 p("ePrescribed Messages are electronic messages that are generated when a GP issues a GP10 prescription. 
                   They are one of the main data streams feeding into the Prescribing Information Sytem (PIS). 
                   GPs account for more than 95% of prescribing in primary care, the great majority of prescriptions 
                   processed have at least one accompanying electronic source."),
                 tags$b("Why are we using ePrescribed messaging data?"),
                 p("The ePrescribed data are loaded daily and are typically available ~48h after the prescription was written. 
                   This allows for a far more rapid turnaround in terms of data analysis of prescribing data as
                   opposed to waiting for 3 months for data to become available through standard data collection means.  
                   This “real-time” intelligence on prescribing trends across Scotland is of particular importance 
                   during the current Covid-19 crisis."),
                 tags$b("Limitations of ePrescribed messaging data?"),
                 p("Some prescribers currently cannot generate electronic messages – this information is only 
                   available when all prescriptions are processed for payment 3 months later. Therefore, e-messaging 
                   data should be regarded as incomplete for all prescriber types. (*Can we say something here about 
                   how complete for GP prescribing?*). In addition, data is not subject to the same quality assurance 
                   processes as prescribing data in the PIS and PRISMS data warehouses. Also, although a prescription 
                   might have been issued, it may not have been dispensed for various reasons. Currently, the electronic 
                   message data is unstructured which makes looking at groups of drugs e.g. medicines used for diabetes, more 
                   challenging. A serial prescription is prescribed once and generates a single electronic message, 
                   but can be dispensed multiple times within a period of 24, 48 or 52 weeks – many Health Boards use 
                   serial prescribing for suitable patients, making this analysis complicated."),
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
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_cardio_select == "cath") {
               showModal(modalDialog(#ALL  MODAL
                 title = "What is the data source?",
                 p("Catheterization labs carry out a series of procedures to observe
                    the condition of the heart and its arteries and to treat problems
                    found this way, particularly the narrowing of the arteries."),
                 p("This data shows the number of procedures carried out by two of the 
                   biggest cath labs in Scotland. We show the data split by three types of procedures: "),
                 tags$ul(
                   tags$li("Angiographies - These diagnostic procedures allow clinicians to see and investigate
                          the state of the hearts and its arteries. Patients of the labs will go
                          through this process before any other one. Therefore have used the number of
                          angiographies as the total number of cases for each lab. Many angiographies
                          are planned."),
                   tags$li("PCI - Percutaneous coronary intervention is a procedure used to treat
                          the narrowing of the heart arteries. In many cases this is an urgent procedure
                          which is used when patients are suffering a heart attack."),
                   tags$li("Devices - in these labs patients can be fitted with pacemakers and other
                          devices used to treat cardiac problems. These procedures are also carried out
                          in other environments so please be aware they are not representative of the 
                          total volume of devices fitted.")
                 ),
                 p("There are four catheterization labs in Scotland: Golden Jubilee Hospital,
                   Royal Infirmary of Edinburgh, Aberdeen Royal Infirmary and Ninewells Hospital in Dundee."),
                 p("For Golden Jubilee Hospital please be aware that they extended their catchment area 
                    during the lockdown period to cover Ayrshire & Arran, Dumfried & Galloway and southeast Glasgow,
                    areas which previously would have gone to Hairmyres Hospital.
                    Also their volume of cases from 2019 (used as a comparator) is 
                    artificially high as they had a mobile lab in 2019."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
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
    lab_chosen <- case_when(input$area_cardio_select == "All" ~ "Royal Infirmary of Edinburgh and Golden Jubilee Hospital",
                            TRUE ~ paste0(input$area_cardio_select))
    
      tagList( # Cath labs
        h3(paste0("Weekly visits to the cardiac catheterization labs at the ", lab_chosen)),
        actionButton("btn_cardio_modal", paste0("Data source: ", lab_chosen), icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "cath_overall"),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by sex", "cath_sex_var",
                     "Weekly number of cases by sex", "cath_sex_tot"),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by age group", "cath_age_var",
                     "Weekly number of cases by age group", "cath_age_tot"),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by type of intervention", "cath_type_var",
                     "Weekly number of cases by type of intervention", "cath_type_tot")
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
        h3(paste0("Weekly number of cardiovascular drug items prescribed in ", input$geoname_cardio)),
        actionButton("btn_cardio_modal", "Data source: ePrescribed Messages", icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "prescribing_all"),
        plot_cut_box(paste0("Percentage change in cardiovascular drug prescriptions in ", input$geoname_cardio, " compared with the corresponding
                     time in 2018-2019 by drug group"), "cardio_drugs_var",
                     paste0("Weekly number of cardiovascular drug prescriptions in ", input$geoname_cardio, " by drug group"), "cardio_drugs_tot")
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