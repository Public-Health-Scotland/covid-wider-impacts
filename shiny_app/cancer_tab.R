# Server side for cancer tab

###############################################.
## Modals ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$btn_cancer_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("Cancer is not a statutorily notifiable disease, so cases are not reported prospectively. 
                  Instead reliant on a complex process involving record linkage and data processing of multiple 
                  records from multiple sources of potentially new cases of cancer, with the aim of maximising 
                  case ascertainment and data accuracy:  ",
                 tags$a(href="https://www.isdscotland.org/Health-Topics/Cancer/Scottish-Cancer-Registry/How-data-are-collected/",class="externallink")),
               p("In an effort to more rapidly assess the impact of COVID-19 on new diagnoses of cancer, opted 
                  to analyse pathology records, comparing trends in weekly numbers of cancer diagnoses 
                  (based on dates of pathology specimen receipt) for 2020 compared to 2019."),
               em("Advantages of using pathology records:"),
                p("- Pathology records are one of the main sources of new incident cases for the cancer registry."),
                 p("- For many anatomical sites of cancer, a pathology report is generated for the overwhelming majority of new cases "),
                  p("- •	In most cases, a pathology record is received by the cancer registry within weeks of the definitive diagnosis being made."),
              em("Limitations of using pathology records:"),
                p("- The SNOMED topography code assigned to a pathology record does not necessarily relate to the primary site of origin of a tumour,
                  especially if there has been localised spread to adjacent organs, or further afield (regional lymph nodes or distant metastases), or in 
                  the case of multi-organ cancers, eg, some lympho-haematopoietic neoplasms."),
                 p("- While the majority of pathology records relate to new incident cancers, some records relate to disease recurrences and/or metastatic disease. "),
                  p("- As with any system of data collection, there can be errors in the SNOMED code allocated, but these are relatively rare."),
                  p("- In some cases, multiple pathology records are generated for the same incident tumour, eg, breast needle biopsy, wide local excision specimen, 
                    axillary lymph node biopsy, etc. This can be mitigated to some extent by restricting to a single record per person (although some individuals will 
                    genuinely have multiple synchronous independent primary tumours), or to a single record per major tumour site (although some individuals will genuinely 
                    have multiple synchronous independent primary tumours at the same anatomical site and, as noted above, some records may relate to metastatic rather 
                    than primary disease, eg, secondary tumours of the liver)."),
                  p("- Full reporting of some pathology records may be delayed for several weeks, eg, if a second opinion and/or information on tumour biomarkers is 
                    being sought."),

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
  dataset <- case_when(input$cancer_type_select == "breast" ~ "breast",
                       input$cancer_type_select == "cervical" ~ "cervical",
                       input$cancer_type_select == "colorectal" ~ "colorectal",
                       input$cancer_type_select == "headandneck" ~ "headandneck",
                       input$cancer_type_select == "lung" ~ "lung",
                       input$cancer_type_select == "lymphoma" ~ "lymphoma",
                       input$cancer_type_select == "melanoma" ~ "melanoma",
                       input$cancer_type_select == "ovarian" ~ "ovarian",
                       input$cancer_type_select == "uppergi" ~ "uppergi",
                       input$cancer_type_select == "urological" ~ "urological")
  
 
  variation_title <- paste0("Percentage change in number of pathology referrals for ", dataset, 
                              " cancer compared with the corresponding time in 2018-2019")


  total_title <- paste0("Weekly change in pathology incidence for ", dataset, " cancer ")
  
  
  
  
  
  
  
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
  if (input$cancer_type_select == "all_types") {
    tagList(
      cut_charts(title= "Weekly pathology referrals", source = "",
                 data_name = "all_types"),
    )
  } else if (input$cancer_type_select == "breast") { 
    cut_charts(title= "Weekly pathology referrals", source = "", 
               data_name = "breast")
    
  } else if (input$cancer_type_select == "cervical") { 
    cut_charts(title= "Weekly pathology referrals", source = "", 
               data_name = "cervical")
    
  } else if (input$cancer_type_select == "colorectal") {
    cut_charts(title= "Weekly pathology referrals", 
               source = "", 
               data_name ="colorectal")
    
  } else if (input$cancer_type_select == "headandneck") { 
    cut_charts(title= "Weekly pathology referrals", 
               source = "", 
               data_name ="ooh")
    
  } else if (input$cancer_type_select == "lung") { 
    cut_charts(title= "Weekly pathology referrals", 
               source = "", 
               data_name ="lung")
    
  } else if (input$cancer_type == "lymphoma") { 
    cut_charts(title= "Weekly pathology referrals", 
               source = "", 
               data_name ="lymphoma")
    
  } else if (input$cancer_type == "melanoma") { 
    cut_charts(title= "Weekly pathology referrals", 
               source = "", 
               data_name ="melanoma")
    
  } else if (input$cancer_type == "ovarian") { 
    cut_charts(title= "Weekly pathology referrals", 
               source = "", 
               data_name ="ovarian")
    
  } else if (input$cancer_type == "uppergi") { # Deaths data
    cut_charts(title= "Weekly pathology referrals", 
               source = "", 
               data_name ="uppergi")
    
  } else if (input$cancer_type == "urological") { # Deaths data
    cut_charts(title= "Weekly pathology referrals", 
               source = "NRS Death Registrations", 
               data_name ="urological")
  } 
}) 

###############################################.
## Charts ----
###############################################.
# Creating plots for each cut and dataset
# A&E charts
output$cancer_overall <- renderPlotly({plot_overall_chart(aye, data_name = "all_types")})
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
