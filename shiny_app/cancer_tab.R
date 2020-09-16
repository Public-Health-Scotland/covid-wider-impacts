# Server side for cancer tab

###############################################.
## Modals ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$btn_immune_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The information shown on the numbers of children eligible for, and receiving, routine preschool immunisations is taken from the ",
                 tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12",
                        "Scottish Immunisation and Recall System (SIRS).",class="externallink")),
               p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation 
                 of children when a scheduled immunisation is due. When a child receives an immunisation, relevant information 
                 is returned to administrative staff in the NHS Board child health department. The administrative staff then 
                 update the child’s SIRS record accordingly."),
               p("After a child becomes eligible for an immunisation, it takes some time for them to attend 
                 their appointment, and for a record of the immunisation provided to subsequently be entered 
                 into the SIRS system. We have allowed a 6-week window for this, therefore each release of 
                 this page will report on children becoming eligible for an immunisation up to 6 weeks before 
                 the date these data were extracted for analysis. Although children will generally have their 
                 immunisation, and their SIRS record updated accordingly, within 6 weeks of becoming eligible, 
                 the pandemic may have influenced not only how quickly eligible children receive their immunisations, 
                 but also how long it takes for children’s SIRS records to be updated once an immunisation has been given. 
                 Any disruption to SIRS data entry may vary across NHS Boards.  Data shown for the most recent cohorts of 
                 children will therefore not be fully complete in SIRS and should be viewed as provisional. The uptake 
                 rates for each cohort will be refreshed with more up-to-date data every 4 weeks, and rates for the most 
                 recent cohorts may increase slightly as relevant records are updated in SIRS."),
               p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink"),
                 " routinely receives quarterly data extracts from SIRS for the purpose of producing and ",
                 (tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/", 
                         "publishing",class="externallink")),
                 " immunisation uptake rates. To allow more rapid monitoring of the impact of Covid-19 on 
                 childhood immunisation uptake rates, PHS is also currently extracting a sub-set of
                 data from SIRS each month."),
               p("Data are reported by NHS Board of treatment as recorded on the monthly data extracts from SIRS.
                 Children receive their immunisations at clinics in the area where they live so this will usually 
                 be the same as the NHS Board of residence. Due to the reconfiguration of NHS Board boundaries a 
                 small proportion of records on SIRS do not reflect the current configuration of NHS Boards and 
                 in these instances children are assigned to an NHS Board of treatment based on their home postcode 
                 at the time of data extract. Uptake rates based on small numbers are prone to fluctuation. 
                 Therefore, in boards with small numbers of children eligible for immunisation each week, 
                 particularly NHS Borders and NHS Dumfries & Galloway, it is important to consider this when interpreting the rates."),
               p(paste0("Figures presented based on data extracted on ",immunisation_extract_date)),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Pop-up modal explaining source of data
observeEvent(input$imm_elig_defs, 
             showModal(modalDialog(
               title = "Immunisation eligibility definitions",
               p("Month of eligibility for each immunisation is defined based on complete weeks (Monday to Sunday)",
                 tags$sup("1"), ":"),
               month_elig_imm %>% autofit() %>% htmltools_value(), #showing month eligibility chart
               br(),
               p("6-in-1 immunisation uptake: Eligible age and uptake rates by 
                   age stage", tags$sup("2"), " shown in the tables."),
               age_defs_imm_6inone %>% autofit() %>% htmltools_value(),
               br(),
               p("MMR immunisation uptake: Eligible age and uptake rates by age stage", tags$sup("2"), 
                 " shown in the tables. Note that ages are defined in weeks but are 
                 labelled in years and/or months of age."),
               age_defs_imm_mmr %>% autofit() %>% htmltools_value(),
               p(tags$sup("1 "), "The immunisation indicators included in the tool are updated each month.
                  With each update an additional month will be added to the presentation."),
               p(tags$sup("2 "), "Uptake rates by a specified age refers to children who have 
                received the vaccine before turning the relevant age. For example, 
                 uptake of the second dose of MMR vaccine by 3 years 5  months is defined as 
                 children receiving the second dose before reaching 178 weeks of age."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive layout ----
###############################################.

# Immunisation reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_immun <- renderUI({
    #Lists areas available in   
  areas_summary_immun <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_immun])
  selectizeInput("geoname_immun", label = NULL, choices = areas_summary_immun, selected = "")
})




# The charts and text shown on the app will depend on what the user wants to see
output$data_explorer <- renderUI({
  
  # text for titles of cut charts
  dataset <- case_when(input$measure_select == "rapid" ~ "admissions",
                       input$measure_select == "aye" ~ "attendances",
                       input$measure_select == "nhs24" ~ "completed contacts",
                       input$measure_select == "ooh" ~ "consultations",
                       input$measure_select == "sas" ~ "incidents",
                       input$measure_select == "deaths" ~ "deaths")
  
  if (input$measure_select == "deaths"){
    variation_title <- paste0("Percentage change in ", dataset, 
                            " compared with the corresponding time in 2015-2019 by ")   #different averaging period for deaths
  } else {
    variation_title <- paste0("Percentage change in ", dataset, 
                              " compared with the corresponding time in 2018-2019 by ")
  }

  total_title <- paste0("Weekly number of ", dataset, " by ")
  
  # To make sure that both titles take the same space and are lined up doing
  # a bit of a hacky shortcut:
  diff_chars <- nchar(variation_title) - nchar(total_title) +10
  extra_chars <- paste0(c(rep("_", diff_chars), "."), collapse = '')

  # Function to create the standard layout for all the different charts/sections
  cut_charts <- function(title, source, data_name) {
    tagList(
      h3(title),
      actionButton("btn_dataset_modal", paste0("Data source: ", source), icon = icon('question-circle')),
      if (input$measure_select == "nhs24"){
        p("The data used in this chart are taken from the Unscheduled Care Datamart.  
          As mentioned in the", tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-statistical-report/", 
                                                                                                                   "COVID-19 weekly report for Scotland", class="externallink"), 
          "NHS 24 made changes to their service delivery to respond to COVID-19.  The data from March 2020 
          does not reflect the full extent of the demand and activity being undertaken by NHS 24 at this time. 
          Over the coming weeks PHS and NHS 24 are working to further enhance the data and intelligence that 
          can be shown in this publication.")
      },
      if (input$measure_select == "deaths"){
        tagList(
        p("The analyses below are derived from the National Records of Scotland (NRS) weekly deaths dataset (provisional numbers). 
          Numbers of deaths represent the total number of deaths (from any cause) that were registered in 
          Scotland in any particular week.  Comparing the number of deaths in the most recent weeks to the 
          average over the past 5 years allows estimation of the numbers of excess deaths.
          Volatility of the trends will be observed in some charts due to small counts."),
        plot_box(paste0("2020 compared with the 2015-2019 average"), paste0(data_name, "_overall"))) #different averaging period for deaths
        } else {
          plot_box(paste0("2020 compared with the 2018-2019 average"), paste0(data_name, "_overall"))
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
                                     choices = if (input$geotype == "Scotland") {spec_list} else {spec_list[c(1:8,11)]}, multiple = TRUE,
                                     selected = c("Medical (incl. Cardiology & Cancer)", "Surgery", "Paediatrics (medical & surgical)"))),
               column(6, actionButton("btn_spec_groups", "Specialties and their groups", icon = icon('question-circle')))),
      fluidRow(column(6, withSpinner(plotlyOutput("adm_spec_var"))),
               column(6, withSpinner(plotlyOutput("adm_spec_tot"))))
    )
  } else if (input$measure_select == "aye") { #A&E Attendances
    cut_charts(title= "Weekly attendances to A&E departments", 
               source = "PHS AE2 Datamart", data_name = "aye")
    
  } else if (input$measure_select == "nhs24") {# NHS 24 calls
    cut_charts(title= "Weekly completed contacts with NHS 24", 
               source = "PHS Unscheduled Care Datamart", data_name ="nhs24")
    
  } else if (input$measure_select == "ooh") { #Out of hours cases
    cut_charts(title= "Weekly cases in out of hours services", 
               source = "PHS GP OOH Datamart", data_name ="ooh")
    
  } else if (input$measure_select == "sas") { # SAS data
    cut_charts(title= "Weekly attended incidents by Scottish Ambulance Service", 
               source = "PHS Unscheduled Care Datamart", data_name ="sas")
    
  } else if (input$measure_select == "deaths") { # Deaths data
    cut_charts(title= "Weekly number of deaths", 
               source = "NRS Death Registrations", data_name ="deaths")
  }
}) 

###############################################.
## Charts ----
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

###############################################.
## Data downloads ----
###############################################.

###############################################.
## Commentary ----
###############################################.
output$cancer_commentary <- renderUI({
  tagList(
    bsButton("jump_to_cardio",label = "Go to data"), #this button can only be used once
    h2("Cancer - xxth xxx 2020"), 
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
