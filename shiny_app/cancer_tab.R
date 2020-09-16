# Server side for cancer tab

###############################################.
## Modals ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$btn_cancer_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The information shown on the numbers of ",
                 tags$a(href="https://www.",class="externallink")),
               p(""),
               p(""),
               p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink"),
                 " routinely receives quarterly data extracts ",
                 (tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/", 
                         "publishing",class="externallink")),
                 " "),
               p("Data are reported by NHS Board of treatment "),
               p(paste0("Figures presented based on data extracted on ",cancer_extract_date)),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Pop-up modal explaining source of data
observeEvent(input$cancer_elig_defs, 
             showModal(modalDialog(
               title = "Immunisation eligibility definitions",
               
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive layout ----
###############################################.

# Cancer reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_cancer <- renderUI({
    #Lists areas available in   
  areas_summary_cancer <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_cancer])
  selectizeInput("geoname_cancer", label = NULL, choices = areas_summary_cancer, selected = "")
})




# The charts and text shown on the app will depend on what the user wants to see
output$cancer_explorer <- renderUI({
  
  # text for titles of cut charts
  dataset <- case_when(input$cancer_type_select == "rapid" ~ "admissions",
                       input$cancer_type_select == "aye" ~ "attendances",
                       input$cancer_type_select == "nhs24" ~ "completed contacts",
                       input$cancer_type_select == "ooh" ~ "consultations",
                       input$cancer_type_select == "sas" ~ "incidents",
                       input$cancer_type_select == "deaths" ~ "deaths")
  
  if (input$cancer_type_select == "deaths"){
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
        p("The data used in this chart are ")
      },
      if (input$measure_select == "deaths"){
        tagList(
        p("The analyses below are "),
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
output$cancer_overall <- renderPlotly({plot_overall_chart(aye, data_name = "aye")})
output$cancer_type_select_sex_var <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex", data_name = "aye")})
output$cancer_type_select_age_var <- renderPlotly({plot_trend_chart(aye, pal_age, "age", data_name = "aye")})
output$cancer_type_select_depr_var <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep", data_name = "aye")})
output$cancer_type_select_sex_tot <- renderPlotly({plot_trend_chart(aye, pal_sex, "sex", "total", "aye")})
output$cancer_type_select_age_tot <- renderPlotly({plot_trend_chart(aye, pal_age, "age", "total", "aye")})
output$cancer_type_select_depr_tot <- renderPlotly({plot_trend_chart(aye, pal_depr, "dep", "total", "aye")})


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
    
          h3("Prescribing"),
          p(""),
          tags$ul(
            tags$li(""),
            tags$li("",
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
          p(""),
          tags$ul(
            tags$li("Overall there was a sharp drop in cardiovascular attendances at Accident and
                    Emergency Departments starting in early March 2020. Attendances were around 60%
                    lower compared to the 2018-2019 average. Levels rose again by the end of May, but
                    remain around 30% below the 2018-19 average."),
            tags$li("This drop in cardiovascular attendances was consistent across both males and
                    females, in younger and older patients and across deprivation quintiles.")),
          h3("Cardiac procedures"),
          p(""),
          tags$ul(
            tags$li("coronary angiography "),
            tags$li("cardiac devices"),
            tags$li("percutaneous coronary interventions")),
          p("The major observations are as follows:"),
          tags$ul(
            tags$li(""),
            tags$li(""),
            tags$li(""))
           )
})
