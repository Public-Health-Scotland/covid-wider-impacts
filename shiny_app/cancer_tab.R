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
               p("The Scottish Cancer Register receives notifications of cancer from many data sources. 
                 Pathology records are one of the main sources, which are routinely transferred to us from 
                 the health board laboratories. These data are valuable to identify and maximise case 
                 ascertainment of potential new cancers."),
               p("The pathology records contain diagnosis information, which has been determined by examining the cells and tissues microscopically. 
                 The specimens used to determine diagnosis are received from various procedures such as simple diagnostic punch biopsies and lymph node 
                 biopsies to fuller wide local excisions and resections. Therefore, it is highly likely that there are numerous pathology reports for 
                 one individual. The reports received by the registry related to solid tissue and cytology specimens. The majority of pathology records 
                 will relate to new primary cancers, some records will relate to disease recurrence or known primary cancers and/or metastatic disease."),
               p(paste0("Figures presented based on data extracted on ",cancer_extract_date)), # need to define cancer_extract_date reactive value
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive datasets ----
###############################################.


cancer_data_cum_main <- reactive({

   cancer_data2 %>% filter(sex == input$gender, area == input$geoname_cancer, site == input$cancer_type)

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
  cancer_site <- case_when(input$cancer_type == "All Malignant Neoplasms (Excl. C44)" ~ "All Malignant Neoplasms (Excl. C44)",
                       input$cancer_type == "All Cancers" ~ "All Cancers",
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
                       input$cancer_type == "Ovary - Females only" ~ "Ovary - Females only",
                       input$cancer_type == "Pancreas" ~ "Pancreas",
                       input$cancer_type == "Penis - Males only" ~ "Penis - Males Only",
                       input$cancer_type == "Prostate - Males only" ~ "Prostate - Males only",
                       input$cancer_type == "Stomach" ~ "Stomach",
                       input$cancer_type == "Testis - Males only" ~ "Testis - Males only",
                       input$cancer_type == "Thyroid" ~ "Thyroid",
                       input$cancer_type == "Trachea, Bronchus and Lung" ~ "Trachea, Bronchus and Lung",
                       input$cancer_type == "Uterus - Females only" ~ "Uterus - Females only",
                       input$cancer_type == "Vagina - Females only" ~ "Vagina - Females only",
                       input$cancer_type == "Vulva - Females only" ~ "Vulva - Females only"
                       )

  
    tagList(
      # h3("Weekly pathology referrals"),
      plot_box(paste0("Total count of individuals having a cancer of type:  ", cancer_site,
                      " confirmed on a pathological specimen since January for 2019/2020"), "cancer_overall"),
      plot_box(paste0("Percentage change of individuals having a cancer of type: ", cancer_site,
                      "confirmed on a pathological specimen since January for 2019/2020"), "cancer_split"),
      plot_box(paste0("Weekly count of individuals having a cancer of type: ", cancer_site,
                      "confirmed on a pathological specimen since January for 2019/2020"), "cancer_incidence"))
 
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
    h3("Cancer in Scotland in 2020"),
    h4("Background"),
    p("COVID-19 has had a wide impact on cancer in Scotland. Some parts of this are better understood
      than others.   For example, cancer screening programmes were paused and urgent referrals for
      suspected cancer fell.  Any effects on patients being less likely to seek help, delays in
      investigations and treatment, or changes in usual treatment, are less clear."),
    p("The aim of this work is to describe the occurrence of new cancers in Scotland after COVID-19.
      Final high-quality cancer incidence data will not be published until 2022, when all information
      can be considered by the Scottish Cancer Registry.   As a proxy measure, the Registry is able to
      report pathology records where cancer was diagnosed."),
    h4("What these data do and do not show"),
    p("The numbers in this dashboard are individuals from whom a pathology sample found cancer in 2019
      and 2020 in Scotland.  Each individual was counted once the first time they appeared from 1st
      January; subsequent samples by the same individual were not counted (except when reporting cancer
      site-specific numbers, where an individual could contribute to more than one site)."),
    p("In most cases, these indicate a new diagnosis (incidence) of cancer but in some cases they are
      follow-up samples of cancers that were diagnosed previously."),
    p("Cancer is often diagnosed initially through clinical examination (including radiology) followed
      by pathological confirmation.  Not all cancers are diagnosed by pathology: some are better diagnosed
      through other methods (e.g. blood tests) and for some, the tumour is inaccessible for tissue sampling."),
    p("Any delays in receiving pathology records are likely to affect the most recent information.  For this
      reason, the penultimate week (week ending 21st June) rather than the final week of June is described
      in these comments."),
    p("It is not known whether any differences in numbers of pathologically confirmed cancers in 2020 compared
      to 2019 are due to changes in:"),
    tags$ul(
      tags$li("patients seeking or obtaining an initial medical consultation"),
      tags$li("availability of cancer screening"),
      tags$li("availability of diagnostic services"),
      tags$li("treatment (particularly surgery, which may provide the pathology sample)")),
    p("Differences may also be due to artefacts because of missing pathology data.  However, a quality
      assurance consultation with Scottish cancer clinicians suggests that the differences between 2020 and
      2019 correspond to the actual change in diagnoses."),
    h4("Overall trends in pathologically confirmed cancers"),
    p("In 2020, numbers were similar to 2019 until the end of March.  They fell by about 40% of comparable
      weeks until the end of June 2020, when the latest data were available."),
    p("By week ending 21st June 2020, the total number of individuals with a pathologically confirmed cancer
      (excluding non-melanoma skin cancers) was 16,902 in 2020 and 20,963 in 2019, an absolute difference of
      4,061. This suggests that about 4,000 fewer patients had a pathologically confirmed cancer diagnosis by
      the end of June 2020 than would have been expected. "),
    h4("Cancer sites"),
    tags$ul(
      tags$li("	Lung cancer numbers fell by about 40% of 2019 levels, with no evidence of recovery by the
              end of June.  The difference was -376 individuals by w/e 21/6/2020"),
      tags$li("	Breast cancer numbers in women fell by about 40% of 2019 levels, with no evidence of recovery
              by the end of June.  The difference was -799 individuals by w/e 21/6/2020 "),
      tags$li("	Colorectal cancers numbers initially fell by about 60% of the 2019 numbers with some evidence
              of recovery.  The difference was -677 individuals by w/e 21/6/2020."),
      tags$li("	Prostate cancer numbers fell with no evidence of recovery by the end of June.  The difference
              was -279 individuals by w/e 21/6/2020."),
      tags$li("	Malignant melanoma skin fell by -111 individuals by w/e 21/6/2020"),
      tags$li("	Non-melanoma skin cancers fell by about 80% of 2019 numbers with some recovery.  The difference
              was -3508 by w/e 21/6/2020."),
      tags$li("	Haematological cancers – important to note that a pathological sample is often not the basis
              of making a diagnosis.  There was little change in Hodgkin’s disease; leukaemias down -233 but were
              somewhat lower throughout 2020 comparing to 2019, probably reflecting the way data are gathered."),
      tags$li("	Malignant brain cancers fell.  By  w/e 21/6/2020 -53 individuals but it should be noted that
              the majority are not diagnosed from a pathology sample.")),
    h4("Sex (all cancers excluding non-melanoma skin cancers)"),
    tags$ul(
      # tags$li("	In general, numbers were higher with increasing age."),
      # tags$li("	Numbers were higher in people from the least deprived compared to the most deprived throughout
      #         2020.  Underlying this total are variations by cancer site, with, for example, breast and prostate
      #         cancers being more common in individuals from less deprived areas and lung and head and neck cancers
      #         being more common in more deprived areas.  By the week ending 29th March 2020, the numbers in the
      #         least and most deprived were 2175 and 1978, respectively.  By the week ending 21st June, these were
      #         3489 and 3070, respectively.  This indicates a widening of the difference in numbers between the least
      #         and most deprived after the end of March."),
      tags$li("	The decrease in 2020 numbers was similar in men and women.  By 21/3/2020, the difference in numbers
              between 2020 and 2019 was -2068 men and -1987 women."))
  )

})
