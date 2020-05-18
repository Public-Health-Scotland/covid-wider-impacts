###############################################.
#  # Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
output$geoname_ui <- renderUI({
  
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype])
  
  selectizeInput("geoname", label = NULL,  
                 choices = areas_summary, selected = "")
  
})

# Disabling  admissions type if no admissions to hospital selected and
# updating labels to say it's not available
observeEvent({input$measure_select}, {
  if (input$measure_select == "rapid") {
    enable("adm_type")
    
    updateSelectInput(session, "adm_type",
                      label = "Step 3. Select type of admission.")
    
  } else if (input$measure_select != "rapid") {
    disable("adm_type")
    
    updateSelectInput(session, "adm_type",
                      label = "Step 3. Select type of admission (not available).")
    
  }
  
})

###############################################.
## Modals ----
###############################################.
#modal to provide information on what specialties are included in each group
spec_modal <- modalDialog(
  h5("List of specialties and what group they correspond to"),
  renderTable(spec_lookup), # creating table based on specialty lookup
  size = "l", align= "center",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)
# Link action button click to modal launch 
observeEvent(input$btn_spec_groups, { showModal(spec_modal) }) 

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
                          "Rapid Preliminary Inpatient Data (RAPID)",class="externallink"),
                   "dataset. This dataset is submitted daily to PHS and relates mainly to general acute care. 
                   Exclusions from the RAPID dataset are day cases, neonatal, maternity and geriatric long stay admissions 
                   and admissions for psychiatric care.  Admissions to the Golden Jubilee National Hospital are 
                   also not included. Admissions related to COVID-19 will be included in totals."),
                 p("The normal source of information on hospital admissions is the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=5",
                          "SMR01 (general inpatient and day cases) return.",class="externallink"),
                   "However, there is generally time lag in the submission of SMR01 to PHS.  
                   Therefore, RAPID is being used for the immediate monitoring of the impact of 
                   COVID-19 on admissions to hospital and it provides broadly comparable figures to SMR01 on 
                   numbers of admissions."),
                 p("The RAPID dataset is managed by ", 
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Predicting-Hospital-Activity/", 
                          "Public Health Scotland (PHS).", class="externallink")),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_select == "aye") { #A&E ATTENDANCES MODAL
               showModal(modalDialog(
                 title = "What is the data source?",
                 p("This tool provides a weekly summary of people attending A&E departments (Emergency Departments 
                   and Nurse/GP led minor injury units) in the recent past, along with historical activity for 
                   comparison purposes. The recent trend data is shown by age group, sex
                   and broad deprivation category (SIMD)."),
                 p("Additional information relating to A&E activity is available from the ", 
                   tags$a(href="https://beta.isdscotland.org/find-publications-and-data/health-services/hospital-care/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/", 
                          "NHS Performs - weekly update of emergency department activity and waiting time statistics.", 
                          class="externallink")),
                 p("Numbers of A&E attendances will include both COVID-19 and non-COVID-19 related activity." ),                   
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
                 p("If required, more detailed analysis of NHS24 activity may be available on request to ",
                   tags$a(href="mailto:phs.isdunscheduledcare@nhs.net", "phs.isdunscheduledcare@nhs.net", 
                          class="externallink"), "."),
                 p("The NHS24 dataset is managed by ", 
                   tags$a(href="https://publichealthscotland.scot/", 
                          "Public Health Scotland", class="externallink"), "and ",
                   tags$a(href="https://www.nhs24.scot/", 
                          "NHS 24", class="externallink"), ".",
                   "This analysis is drawn from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=111", "Unscheduled Care Datamart (UCD).",class="externallink")
                 ),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
               
             } else if (input$measure_select == "ooh"){
               showModal(modalDialog(# OUT OF HOURS CONSULTATIONS  MODAL
                 title = "What is the data source?",
                 p("The Primary Care Out of Hours service provides urgent access to a nurse or doctor, 
                   when needed at times outside normal general practice hours, such as evenings, 
                   overnight or during the weekend. An appointment to the service is normally arranged 
                   following contact with NHS 24. The recent trend data is shown by age group, sex and 
                   broad deprivation category (SIMD)."),
                 p("The charts provide a weekly summary of consultations in the recent past and 
                   historical trends for comparison purposes."),
                 p("The figures presented in this tool exclude consultations within any of the COVID-19 
                   hubs or assessment centres and relate only to consultations concerning non-COVID 
                   issues. "),
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
                 p("If required, more detailed analysis of SAS activity may be available on request to ",
                   tags$a(href="mailto:phs.isdunscheduledcare@nhs.net", "phs.isdunscheduledcare@nhs.net", 
                          class="externallink"), "."),
                 p("The SAS dataset is managed by ", 
                   tags$a(href="https://publichealthscotland.scot/", 
                          "Public Health Scotland", class="externallink"), "and ",
                   tags$a(href="https://www.scottishambulance.com/", 
                          "Scottish Ambulance Service", class="externallink"), ".",
                   "This analysis is drawn from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=111", "Unscheduled Care Datamart (UCD).",class="externallink")
                 ),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
             
             )

###############################################.
# Modal to explain SIMD and deprivation
simd_modal <- modalDialog(
  h5("What is SIMD and deprivation?"),
  p("The", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD).",
                  class="externallink"), "is the Scottish Government's 
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

# Function to filter the datasets for the overall charts and download data based on user input
filter_data <- function(dataset) {
  dataset %>% filter(type == "sex") %>%
    filter(area_name == input$geoname &
             category == "All")
}

###############################################.
## Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$data_explorer <- renderUI({
  
  # text for titles of cut charts
  dataset <- case_when(input$measure_select == "rapid" ~ "admissions",
                       input$measure_select == "aye" ~ "attendances",
                       input$measure_select == "nhs24" ~ "completed contacts",
                       input$measure_select == "ooh" ~ "consultations",
                       input$measure_select == "sas" ~ "incidents")
  
  variation_title <- paste0("Percentage change in ", dataset, 
                            " compared with the corresponding time in 2018-2019 by ")
  
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
        p("The data used in this chart are taken from the Unscheduled Care Datamart.  As mentioned in the", tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-statistical-report/", 
                                                                                                                   "COVID-19 weekly report for Scotland", class="externallink"), "NHS 24 made changes to their service delivery to respond to COVID-19.  The data from March 2020 does not reflect the full extent of the demand and activity being undertaken by NHS 24 at this time. Over the coming weeks PHS and NHS 24 are working to further enhance the data and intelligence that can be shown in this publication.")
      },
      plot_box(paste0("2020 compared with the 2018-2019 average"), paste0(data_name, "_overall")),
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
                                     choices = spec_list, multiple = TRUE,
                                     selected = c("Medical (incl. Cardiology & Cancer)", "Surgery", "Paediatrics"))),
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
    
  } else if (input$measure_select == "ooh") { #Out of hours consultations
    cut_charts(title= "Weekly consultations to out of hours services", 
               source = "PHS GP OOH Datamart", data_name ="ooh")
    
  } else if (input$measure_select == "sas") { # SAS data
    cut_charts(title= "Weekly attended incidents by Scottish Ambulance Service", 
               source = "PHS Unscheduled Care Datamart", data_name ="sas")
  }
}) 

###############################################.
## Charts ----
###############################################.
###############################################.
# Creating plots for each cut and dataset
# A&E charts
output$aye_overall <- renderPlotly({plot_overall_chart(aye, "aye")})
output$aye_sex_var <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex")})
output$aye_age_var <- renderPlotly({plot_trend_chart(aye, pal_age, "age")})
output$aye_depr_var <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep")})
output$aye_sex_tot <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex", "total", "aye")})
output$aye_age_tot <- renderPlotly({plot_trend_chart(aye, pal_age, "age", "total", "aye")})
output$aye_depr_tot <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep", "total", "aye")})

# OOH charts
output$ooh_overall <- renderPlotly({plot_overall_chart(ooh, "ooh")})
output$ooh_sex_var <- renderPlotly({plot_trend_chart(ooh, pal_sex, "sex")})
output$ooh_age_var <- renderPlotly({plot_trend_chart(ooh, pal_age, "age")})
output$ooh_depr_var <- renderPlotly({plot_trend_chart(ooh, pal_depr, "dep")})
output$ooh_sex_tot <- renderPlotly({plot_trend_chart(ooh, pal_sex, "sex", "total", "ooh")})
output$ooh_age_tot <- renderPlotly({plot_trend_chart(ooh, pal_age, "age", "total", "ooh")})
output$ooh_depr_tot <- renderPlotly({plot_trend_chart(ooh, pal_depr, "dep", "total", "ooh")})

# NHS24 charts
output$nhs24_overall <- renderPlotly({plot_overall_chart(nhs24, "nhs24")})
output$nhs24_sex_var <- renderPlotly({plot_trend_chart(nhs24, pal_sex, "sex")})
output$nhs24_age_var <- renderPlotly({plot_trend_chart(nhs24, pal_age, "age")})
output$nhs24_depr_var <- renderPlotly({plot_trend_chart(nhs24, pal_depr, "dep")})
output$nhs24_sex_tot <- renderPlotly({plot_trend_chart(nhs24, pal_sex, "sex", "total", "nhs24")})
output$nhs24_age_tot <- renderPlotly({plot_trend_chart(nhs24, pal_age, "age", "total", "nhs24")})
output$nhs24_depr_tot <- renderPlotly({plot_trend_chart(nhs24, pal_depr, "dep", "total", "nhs24")})

# SAS charts
output$sas_overall <- renderPlotly({plot_overall_chart(sas, "sas")})
output$sas_sex_var <- renderPlotly({plot_trend_chart(sas, pal_sex, "sex")})
output$sas_age_var <- renderPlotly({plot_trend_chart(sas, pal_age, "age")})
output$sas_depr_var <- renderPlotly({plot_trend_chart(sas, pal_depr, "dep")})
output$sas_sex_tot <- renderPlotly({plot_trend_chart(sas, pal_sex, "sex", "total", "sas")})
output$sas_age_tot <- renderPlotly({plot_trend_chart(sas, pal_age, "age", "total", "sas")})
output$sas_depr_tot <- renderPlotly({plot_trend_chart(sas, pal_depr, "dep", "total", "sas")})

# Admissions to hospital charts
output$adm_overall <- renderPlotly({plot_overall_chart(rapid_filt(), "adm")})
output$adm_sex_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_sex, "sex")})
output$adm_age_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_age, "age")})
output$adm_depr_var <- renderPlotly({plot_trend_chart(rapid_filt(), pal_depr, "dep")})
output$adm_sex_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_sex, "sex", "total", "adm")})
output$adm_age_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_age, "age", "total", "adm")})
output$adm_depr_tot <- renderPlotly({plot_trend_chart(rapid_filt(), pal_depr, "dep", "total", "adm")})
output$adm_spec_var <- renderPlotly({plot_spec("variation")})
output$adm_spec_tot <- renderPlotly({plot_spec("total")})

# Palette for specialty
pal_spec <- reactive({
  #Creating palette of colors: colorblind proof
  #First obtaining length of each geography type, if more than 6, then 6,
  # this avoids issues. Extra selections will not be plotted
  trend_length <- length(input$adm_specialty)
  
  # First define the palette of colours used, then set a named vector, so each color
  # gets assigned to an area. I think is based on the order in the dataset.
  trend_palette <- c("#000000", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                     "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928")
  
  trend_scale <- c(setNames(trend_palette, unique(rapid_spec()$spec)[1:trend_length]))
  trend_col <- trend_scale[1:trend_length]
  
})

symbol_spec <- reactive({
  #Creating palette of colors: colorblind proof
  #First obtaining length of each geography type, if more than 6, then 6,
  # this avoids issues. Extra selections will not be plotted
  trend_length <- length(input$adm_specialty)
  
  # First define the palette of sybols used, then set a named vector, so each color
  # gets assigned to an area. I think is based on the order in the dataset.
  
  symbols_palette <-  c('circle', 'diamond', 'circle', 'diamond', 'circle', 'diamond',
                        'square','triangle-up', 'square','triangle-up', 'square','triangle-up')
  symbols_scale <- c(setNames(symbols_palette, unique(rapid_spec()$spec)[1:trend_length]))
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
    "rapid" = filter_data(rapid_filt()),
    "aye" = filter_data(aye),
    "nhs24" = filter_data(nhs24),
    "ooh" = filter_data(ooh),
    "sas" = filter_data(sas)
  ) %>% 
    select(area_name, week_ending, count, count_average) %>% 
    rename(average_pre2020 = count_average) %>% 
    mutate(week_ending = format(week_ending, "%d %b %y"))
})

output$download_chart_data <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    write_csv(overall_data_download(),
              file) } 
)

#END
