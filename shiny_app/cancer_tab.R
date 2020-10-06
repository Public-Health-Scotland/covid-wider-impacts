# Server side for cancer tab

###############################################.
## Modals ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$btn_cancer_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("",
                 tags$a(href="https://www.isdscotland.org/Health-Topics/Cancer/Scottish-Cancer-Registry/How-data-are-collected/",class="externallink")),
               p(""),
               p(""),
               p(""),
               
               p("Data are reported by NHS Board of treatment "),
               p(paste0("Figures presented based on data extracted on ",cancer_extract_date)), # need to define cancer_extract_date reactive value
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive datasets ----
###############################################.

cancer_data_hb <- reactive({
  cancer_data %>% filter(health_board_name == input$geoname_cancer) 
})

cancer_data_type <- reactive({
  cancer_data_hb() %>% filter(site_descript == input$cancer_type_select) 
})



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
  
  
  cancer_chart_title <- paste0("Percentage change in number of pathology referrals for ", dataset, 
                               " cancer compared with the corresponding time in 2018-2019 by ")
  
  tagList(
    h3("Weekly pathology referrals"),
    tagList(
      plot_box(paste0("2020 compared with the 2018-2019 average"), "cancer_overall"),
      
      plot_box(paste0(cancer_chart_title, "sex"), "cancer_sex_var"),
      
      plot_box(paste0(cancer_chart_title, "age group"), "cancer_age_var"),
      
      fluidRow(column(6, h4(paste0(cancer_chart_title, "SIMD quintile")))),
      
      fluidRow(actionButton("btn_modal_simd", "What is SIMD and deprivation?", 
                            icon = icon('question-circle'))),
      
      plot_box(paste0(cancer_chart_title, "SIMD quintile"), "cancer_depr_var")))
  
})

###############################################.
## Charts ----
###############################################.
# Creating plots for each cut and dataset

output$cancer_overall <- renderPlotly({plot_overall_chart(cancer_data_type(), data_name = "cancer")})
output$cancer_sex_var <- renderPlotly({plot_trend_chart(cancer_data_type(), pal_sex, "sex", tab = "cancer")})
output$cancer_age_var <- renderPlotly({plot_trend_chart(cancer_data_type(), pal_age, "age", tab = "cancer")})
output$cancer_depr_var <- renderPlotly({plot_trend_chart(cancer_data_type(), pal_depr, "dep", tab = "cancer")})


###############################################.
## Data downloads ----
###############################################.

# 

###############################################.
## Commentary ----
###############################################.
output$cancer_commentary <- renderUI({
  tagList(
    bsButton("jump_to_cancer",label = "Go to data"), #this button can only be used once
    h2("Cancer - xxth xxx 2020"), 
    
    h3("Commentary"),
    p(""),
    tags$ul(
      tags$li("Cancer is not a statutorily notifiable disease, so cases are not reported prospectively. 
              Instead reliant on a complex process involving record linkage and data processing of multiple 
              records from multiple sources of potentially new cases of cancer, with the aim of maximising 
              case ascertainment and data accuracy: "),
      tags$li("In an effort to more rapidly assess the impact of COVID-19 on new diagnoses of cancer, opted 
              to analyse pathology records, comparing trends in weekly numbers of cancer diagnoses 
              (based on dates of pathology specimen receipt) for 2020 compared to 2019.",
              tags$ul(
                tags$li("Advantages of using pathology records:"),
                tags$li("- Pathology records are one of the main sources of new incident cases for the cancer registry."),
                tags$li("- For many anatomical sites of cancer, a pathology report is generated for the overwhelming majority of new cases"),
                tags$li("-	In most cases, a pathology record is received by the cancer registry within weeks of the definitive diagnosis being made."),
                tags$li("Limitations of using pathology records:"),
                tags$li("- While the majority of pathology records relate to new incident cancers, some records relate to disease recurrences and/or metastatic disease."),
                tags$li("- As with any system of data collection, there can be errors in the SNOMED code allocated, but these are relatively rare."),
                tags$li("- In some cases, multiple pathology records are generated for the same incident tumour, eg, breast needle biopsy, wide local excision specimen, 
                        axillary lymph node biopsy, etc. This can be mitigated to some extent by restricting to a single record per person (although some individuals will 
                        genuinely have multiple synchronous independent primary tumours), or to a single record per major tumour site (although some individuals will genuinely 
                        have multiple synchronous independent primary tumours at the same anatomical site and, as noted above, some records may relate to metastatic rather 
                        than primary disease, eg, secondary tumours of the liver).")
                )
              )
              )
              )
})
