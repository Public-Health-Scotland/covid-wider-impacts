# Server side for cancer tab - tes

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


cancer_data_cum_main <- reactive({
  # if(input$gender != "All Persons") {
   cancer_data2 %>% filter(sex == input$gender, area == input$geoname_cancer, site == input$cancer_type)
  # } else {
  #  cancer_data2 %>% filter(site == input$cancer_type, hbres == input$geoname_cancer)
  # }
})

cancer_data_cum_split <- reactive({
  cancer_data %>% filter(category == input$split, area == input$geoname_cancer)
})



###############################################.
## Reactive layout ----
###############################################.

# Cancer reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_cancer <- renderUI({
  #Lists areas available in   
  areas_summary_cancer <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_cancer])
  if(input$geotype_cancer == "Health Board") {
  selectizeInput("geoname_cancer", label = NULL, choices = c("NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
                                                             "NHS Fife", "NHS Forth Valley", "NHS Grampian", 
                                                             "NHS Greater Glasgow & Clyde", "NHS Highland",
                                                             "NHS Lanarkshire", "NHS Lothian", "NHS Orkney",
                                                             "NHS Shetland", "NHS Tayside", "NHS Western Isles"), 
                                                              selected = "NHS Ayrshire & Arran")
  } else if (input$geotype_cancer == "Cancer Network") {
    selectizeInput("geoname_cancer", label = NULL, choices = c("NCA", "SCAN", "WOSCAN"), selected = "NCA")  
  } else if (input$geotype_cancer == "Scotland") {
    selectizeInput("geoname_cancer", label = NULL, choices = "Scotland", selected = "Scotland")  
  }  
})


# The charts and text shown on the app will depend on what the user wants to see
output$cancer_explorer <- renderUI({
  

  # text for titles of cut charts
  cancer_site <- case_when(input$cancer_type == "All Cancers" ~ "All Cancers",
                       input$cancer_type == "All Malignant Neoplasms (Excl. C44)" ~ "All Malignant Neoplasms (Excl. C44)",
                       input$cancer_type == "Bladder" ~ "Bladder",
                       input$cancer_type == "Bone and Connective Tissue" ~ "Bone and Connective Tissue",
                       input$cancer_type == "Breast" ~ "Breast",
                       input$cancer_type == "Colorectal" ~ "Colorectal",
                       input$cancer_type == "Head and Neck" ~ "Head and Neck",
                       input$cancer_type == "Hodgkin Lymphoma" ~ "Hodgkin Lymphoma",
                       input$cancer_type == "Kidney" ~ "Kidney",
                       input$cancer_type == "Leukaemias" ~ "Leukaemias",
                       input$cancer_type == "Liver and Intrahepatic Bile Ducts" ~ "Liver and Intrahepatic Bile Ducts",
                       input$cancer_type == "Malignant Brain Cancer" ~ "Malignant Brain Cancer",
                       input$cancer_type == "Malignant Melanoma of the Skin" ~ "Malignant Melanoma of the Skin",
                       input$cancer_type == "Mesothelioma" ~ "Mesothelioma",
                       input$cancer_type == "Multiple Myeloma and malignant plasma cell neoplasms" ~ "Multiple Myeloma and malignant plasma cell neoplasms",
                       input$cancer_type == "Non-Melanoma Skin Cancer" ~ "Non-Melanoma Skin Cancer",
                       input$cancer_type == "Oesophagus" ~ "Oesophagus",
                       input$cancer_type == "Other" ~ "Other",
                       input$cancer_type == "Ovary - Females Only" ~ "Ovary - Females only",
                       input$cancer_type == "Pancreas" ~ "Pancreas",
                       input$cancer_type == "Penis - Males Only" ~ "Penis - Males Only",
                       input$cancer_type == "Prostate - Males Only" ~ "Prostate - Males only",
                       input$cancer_type == "Stomach" ~ "Stomach",
                       input$cancer_type == "Testis - Males Only" ~ "Testis - Males only",
                       input$cancer_type == "Thyroid" ~ "Thyroid",
                       input$cancer_type == "Trachea, Bronchus and Lung" ~ "Trachea, Bronchus and Lung",
                       input$cancer_type == "Uterus - Females Only" ~ "Uterus - Females only",
                       input$cancer_type == "Vagina - Females Only" ~ "Vagina - Females only",
                       input$cancer_type == "Vulva - Females Only" ~ "Vulva - Females only"
                       )

  # if(input$geotype_cancer == "Scotland") {
    
    # enable("cancer_type")
    # enable("gender")
    tagList(
      h3("Weekly pathology referrals"),
      plot_box(paste0("2020 cumulative incidences of pathology referrals for cancer type:  ", cancer_site,
                      ", compared with the corresponding time in 2018-2019 by week"), "cancer_overall"),
      plot_box(paste0("Percentage change in number of pathology referrals for cancer type: ", cancer_site,
                      ", compared with the corresponding time in 2018-2019 by week"), "cancer_split"),
      plot_box(paste0("2020 incidences of pathology referrals for cancer type: ", cancer_site,
                      ", compared with the corresponding time in 2018-2019 by week"), "cancer_incidence"))
  # } else {
  #   
  #   disable("cancer_type")
  #   disable("gender")
  #   tagList(
  #     h3("Weekly pathology referrals"),
  #     plot_box(paste0("2020 cumulative incidences of pathology referrals for cancer of ", cancer_site,
  #                     ", compared with the corresponding time in 2018-2019 by week"), "cancer_overall"),
  #     plot_box(paste0("Percentage change in number of pathology referrals for cancer of ", cancer_site,
  #                     ", compared with the corresponding time in 2018-2019 by week"), "cancer_split"),
  #     plot_box(paste0("2020 incidences of pathology referrals for cancer of ", cancer_site,
  #                     ", compared with the corresponding time in 2018-2019 by week"), "cancer_incidence"))
  # }  
  #  
})

###############################################.
## Charts ----
###############################################.
# Creating plots for each cut and dataset

output$cancer_overall <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                              var1_chosen = "cum_count20", var2_chosen = "cum_count19", 
                                              data_name = "cum")})

output$cancer_split <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                                var1_chosen = "difference", var2_chosen = "difference",
                                                data_name = "dif")})

output$cancer_incidence <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                                                 var1_chosen = "count20", var2_chosen = "count19", 
                                                                 data_name = "inc")})

###############################################.
## Data downloads ----
###############################################.


output$download_cancer_data <- downloadHandler(
  filename ="cancer_extract.csv",
  content = function(file) {
    write_csv(cancer_data2,
              file) } 
) 

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
