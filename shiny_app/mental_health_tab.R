##Server script for MENTAL HEALTH tab

###############################################.
## Reactive controls ----
###############################################.
# Helper function
`%notin%` <- Negate(`%in%`)
# Reactive control for areatype
output$geotype_mh_ui <- renderUI({
if (input$measure_mh_select == "mhdrugs") {
  areas <- c("Scotland", "Health board", "HSC partnership")
} else if (input$measure_mh_select == "aye") {
  areas <- c("Scotland", "Health board")
} else if (input$measure_mh_select == "ooh") {
  areas <- c("Scotland", "Health board")
} 
  
selectizeInput("area_mh_select", "Step 2 - Select the area of interest",
               choices = areas, selected = "Scotland")
})

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
# observeEvent(input$measure_mh_select, {
#     mh_label = "Step 2 - Select geography level for mental health medicine prescriptions"
#     mh_choices = c("Scotland", "Health board", "HSC partnership")
#     shinyjs::show("geoname_mh_ui")
#     enable("area_mh_select")
#   
# })

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_mentalhealth_modal,
             if (input$measure_mh_select == "aye") { 
               showModal(modalDialog(# MH AYE MODAL
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
                           incomplete for a number of NHS Boards. Thus, the figures reported for mental health related 
                           attendances offer only a very approximate indication of attendances. 
                           Additionally, some NHS Boards have moved to a new recording standard which 
                           has not been fully consolidated in the A&E datamart as yet. As a result, figures for 2020, 
                           even prior to the introduction of lockdown measures, appear somehwat lower when compared to 
                           previous years.")),
                 # p("The table below shows the ICD-10 codes that were considered for the cardiovascular A&E data subset, 
                 #   where this information was available."),
                 # actionButton("toggleCodeTable", "Show / Hide Table"),
                 # shinyjs::onclick("toggleCodeTable",
                 #                  shinyjs::toggle(id = "CodeTable")),
                 # shinyjs::hidden(div(id = "CodeTable", br(), DT::dataTableOutput("ae_mh_codes_tbl"))),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_mh_select == "mhdrugs") {
             showModal(modalDialog(# MH DRUGS MODAL
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
               p("The ePrescribed messaging dataset is managed by Public Health Scotland (PHS)."),
          
                actionButton("toggle_mh_drug_codes", "Show / Hide Medicine Groups"),
                shinyjs::onclick("toggle_mh_drug_codes",
                                 shinyjs::toggle(id = "mh_drug_codes")),
                shinyjs::hidden(div(id="mh_drug_codes",
                  br(),
                  HTML({
                    "
                    <table style='width:100%'>
                    <tr>
                    <th colspan='1'>Anxiety Medicines</th>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Diazepam (excluding rectal preparations)</td>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Lorazepam</td>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Oxazepam</td>
                    </tr>
                    <tr>
                    <th colspan='1'>Insomnia Medicines</th>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Nitrazepam</td>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Loprazolam</td>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Lormetazepam</td>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Temazepam</td>
                    </tr>
                    <tr>
                   <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Zolpidem</td>
                    </tr>
                    <tr>
                   <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Zopiclone</td>
                    </tr>
                    <tr>
                    <th colspan='1'>Depression Medicines</th>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Citalopram</td>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Fluoxetine</td>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Fluvoxamine</td>
                    </tr>
                    <tr>
                    <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Paroxetine</td>
                    </tr>
                    <tr>
                   <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Sertraline</td>
                    </tr>
                    <tr>
                   <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Duloxetine</td>
                    </tr>
                    <tr>
                   <td colspan='1'>&nbsp;&nbsp;&nbsp;&nbsp;Venlafaxine</td>
                    </tr>
                    </table>
                    "
                    }),
               size = "1",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             ))
            } else if (input$measure_mh_select == "ooh") { 
              showModal(modalDialog(# MH OOH MODAL
                title = "What is the data source?",
                p("The Primary Care Out of Hours service provides urgent access to a nurse or doctor, 
                   when needed at times outside normal general practice hours, such as evenings, 
                   overnight or during the weekend. An appointment to the service is normally arranged 
                   following contact with NHS 24. The recent trend data is shown by age group, sex and 
                   broad deprivation category (SIMD)."),
                p("The charts provide a weekly summary of cases related to mental health in the recent past and 
                   historical trends for comparison purposes."),
                p("The figures presented include mental health out of hours consultations in areas relating to anxiety/stress, depression, self harm, 
                   mental health issues caused by substance misuse, psychotic conditions and mental disorders. The figures exclude cases related to dementia and 
                   learning disabilities."),
                p("The figures presented in this tool exclude cases within any of the COVID-19 
                   hubs or assessment centres and relate only to cases concerning non-COVID 
                   issues."),
                p("If required, more detailed analysis of the Primary Care Out of Hours service may 
                   be available on request to ",
                  tags$a(href="mailto:phs.isdunscheduledcare@nhs.net", "phs.isdunscheduledcare@nhs.net", 
                         class="externallink"), "."),
                p("General Practice Out of Hours service data is sourced from the",
                  tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=113", 
                         "GP Out of Hours Dataset (OOH).",class="externallink"), 
                  "The OOH dataset is managed by ", 
                  tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/GP-Out-of-Hours-Services/", 
                         "Public Health Scotland (PHS).", class="externallink")),
                p("Small counts, including zeroes, are not shown in order to protect patient confidentiality."),
                size = "m",
                easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
                })
               


###############################################.
## Reactive datasets ----
###############################################.
ae_mh_filt <- reactive({ae_mh %>% filter(area_type == input$area_mh_select &
                                           area_name == input$geoname_mh)
  })

mh_ooh_filt <- reactive({mh_ooh %>% filter(area_type == input$area_mh_select &
                                           area_name == input$geoname_mh)
})

###############################################.
## Charts ----
###############################################.

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
# MH A&E charts
output$ae_mh_overall <- renderPlotly({plot_overall_chart(ae_mh_filt(), data_name = "aye", area = "All")})
output$ae_mh_sex_var <- renderPlotly({plot_trend_chart(ae_mh_filt(), pal_sex, c("sex", "all"), data_name = "aye",tab = "mh")})
output$ae_mh_sex_tot <- renderPlotly({plot_trend_chart(ae_mh_filt(), pal_sex, c("sex", "all"), "total", "aye", tab = "mh")})
output$ae_mh_age_var <- renderPlotly({plot_trend_chart(ae_mh_filt(), pal_age, c("age", "all"), data_name = "aye",tab = "mh")})
output$ae_mh_age_tot <- renderPlotly({plot_trend_chart(ae_mh_filt(), pal_age, c("age", "all"), "total", "aye", tab = "mh")})
output$ae_mh_dep_var <- renderPlotly({plot_trend_chart(dataset = ae_mh_filt(), pal_chose = pal_depr, split = "dep", type = "variation", data_name = "aye", tab = "mh")})
output$ae_mh_dep_tot <- renderPlotly({plot_trend_chart(ae_mh_filt(), pal_depr, split = "dep", type = "total", data_name = "aye", tab = "mh")})

###############################################.
# MH OOH charts
output$mh_ooh_overall <- renderPlotly({plot_overall_chart(mh_ooh_filt(), data_name = "ooh", area = "All")})
output$mh_ooh_sex_var <- renderPlotly({plot_trend_chart(mh_ooh_filt(), pal_sex, c("sex", "all"), data_name = "ooh",tab = "mh")})
output$mh_ooh_sex_tot <- renderPlotly({plot_trend_chart(mh_ooh_filt(), pal_sex, c("sex", "all"), "total", "ooh", tab = "mh")})
output$mh_ooh_age_var <- renderPlotly({plot_trend_chart(mh_ooh_filt(), pal_age, c("age", "all"), data_name = "ooh",tab = "mh")})
output$mh_ooh_age_tot <- renderPlotly({plot_trend_chart(mh_ooh_filt(), pal_age, c("age", "all"), "total", "ooh", tab = "mh")})
output$mh_ooh_dep_var <- renderPlotly({plot_trend_chart(dataset = mh_ooh_filt(), pal_chose = pal_depr, split = "dep", type = "variation", data_name = "ooh", tab = "mh")})
output$mh_ooh_dep_tot <- renderPlotly({plot_trend_chart(mh_ooh_filt(), pal_depr, split = "dep", type = "total", data_name = "ooh", tab = "mh")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$mh_explorer <- renderUI({
  if (input$measure_mh_select == "mhdrugs") { 
    tagList(# Prescribing - items dispensed
      h3(paste0("Number of patients starting a new treatment course for selected mental health medicines in ", input$geoname_mh)),
      actionButton("btn_mentalhealth_modal", "Data source: ePrescribed Messages",
                   icon = icon('question-circle')),
      plot_box("2020 compared with 2018-2019 average", "mh_prescribing_all"),
      plot_cut_box(paste0("Percentage change in the number of patients starting a new treatment course for selected mental health medicines in ", input$geoname_mh, 
                          " compared with average of the corresponding time in 2018 and 2019 by medicine groupings"), "mh_drugs_var",
                   paste0("Weekly number of patients starting a new treatment course for selected mental health medicines in ", input$geoname_mh, " by medicine groupings"), "mh_drugs_tot"))
  } else if (input$measure_mh_select == "aye") {
    tagList(#A&E attendances
      tags$em("Please note that, due to limitations in diagnosis recording in the A&E datamart, the data are 
                 incomplete for a number of NHS Boards. Thus, the figures reported for mental health related 
                 attendances offer only a very approximate indication of attendances. 
                 Additionally, some NHS Boards have moved to a new recording standard which 
                 has not been fully consolidated in the A&E datamart as yet."),
      h3("Weekly mental health A&E attendances in Scotland"),
      actionButton("btn_mentalhealth_modal", "Data source: PHS AE2 Datamart", icon = icon('question-circle')),
      plot_box("2020 compared with 2018-2019 average", "ae_mh_overall"),
    if (input$geoname_mh == "Scotland") {
      tagList(
        plot_cut_box("Percentage change in mental health A&E attendances compared with the corresponding
                     time in 2018-2019 by sex", "ae_mh_sex_var",
                     "Weekly number of mental health A&E attendances by sex", "ae_mh_sex_tot"),
        plot_cut_box("Percentage change in mental health A&E attendances compared with the corresponding
                     time in 2018-2019 by age group", "ae_mh_age_var",
                     "Weekly number of mental health A&E attendances by age group", "ae_mh_age_tot"),
        plot_cut_box("Percentage change in mental health A&E attendances compared with the corresponding
                     time in 2018-2019 by SIMD quintile", "ae_mh_dep_var",
                     "Weekly number of mental health A&E attendances by SIMD quintile", "ae_mh_dep_tot",
                     extra_content = actionButton("btn_modal_simd_mh", "What is SIMD and deprivation?", 
                                                  icon = icon('question-circle')))
      ) #taglist bracket from if statement
      
    }
    )  #taglist bracket from aye section

    } else if (input$measure_mh_select == "ooh") {
      tagList(#OOH attendances
        h3("Weekly mental health out of hours consultations in Scotland"),
        actionButton("btn_mentalhealth_modal", "Data source: PHS GP OOH Datamart", icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "mh_ooh_overall"),
        if (input$geoname_mh == "Scotland") {
          tagList(
            plot_cut_box("Percentage change in mental health out of hours consultations compared with the corresponding
                     time in 2018-2019 by sex", "mh_ooh_sex_var",
                         "Weekly number of mental health out of hours consultations by sex", "mh_ooh_sex_tot"),
            plot_cut_box("Percentage change in mental health out of hours consultations compared with the corresponding
                     time in 2018-2019 by age group", "mh_ooh_age_var",
                         "Weekly number of mental health out of hours consultations by age group", "mh_ooh_age_tot"),
            plot_cut_box("Percentage change in mental health out of hours consultations compared with the corresponding
                     time in 2018-2019 by SIMD quintile", "mh_ooh_dep_var",
                         "Weekly number of mental health out of hours consultations by SIMD quintile", "mh_ooh_dep_tot",
                         extra_content = actionButton("btn_modal_simd_mh", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))
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
