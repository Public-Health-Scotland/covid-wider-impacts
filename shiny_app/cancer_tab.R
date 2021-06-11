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
               p("The Scottish Cancer Registry receives notifications of cancer from many data sources. Pathology 
                 records are one of the main sources, these are routinely transferred to the registry from the health 
                 board laboratories. These data are valuable to identify and maximise case ascertainment of potential 
                 new cancers."),
               p("Pathology records contain diagnosis information, which has been determined by examining the
               cells and tissues microscopically.  Microscopic examination is generally considered as the most 
                 accurate method of diagnosis. The specimens used to determine diagnosis are received from various 
                 procedures such as smears and fluids, simple diagnostic punch biopsies, lymph node biopsies to 
                 fuller wide local excisions and resections. Therefore, it is highly likely that there are numerous 
                 pathology reports for one individual. The reports received by the registry related to solid tissue 
                 and cytology specimens. Peripheral blood smears are not included such as leukaemia diagnosed from 
                 peripheral blood film.  The majority of pathology records will relate to new primary cancers, some 
                 records will relate to disease recurrence or known primary cancers and/or metastatic disease."),
               p(paste0("Figures presented based on data extracted on ",cancer_extract_date)), # need to define cancer_extract_date reactive value
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive datasets ----
###############################################.


cancer_data_cum_main <- reactive({

   cancer_data2 %>% filter(sex == input$gender, area == input$geoname_cancer, site == input$cancer_type)

})

# cancer_data_cum_main2 <- reactive({
#   
#   cancer_data3 %>% filter(sex == input$gender, area == input$geoname_cancer, site == input$cancer_type) %>% 
#     mutate(dep = factor(dep))
#   
# })



cancer_data_dl <- reactive({
  
  cancer_data_cum_main() %>% 
    rename("Area name" = area, "Cancer type" = site,
           "Sex" = sex,
           "Week ending" = week_ending,
           "Count 2019" = count19,
           "Count 2020" = count20,
           "Count 2021" = count21,
           "Cumulative count 2019" = cum_count19,
           "Cumulative count 2020" = cum_count20,
           "Cumulative count 2021" = cum_count21,
           "Variation (%)" = difference)
  
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
                       input$cancer_type == "Brain Tumour" ~ "Brain Tumour",
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
      br(),
      plot_box(paste0("Total count of individuals having a cancer of type:  ", input$cancer_type, #cancer_site,
                      " confirmed on a pathological specimen since January for 2019/2020/2021"), 
                      "cancer_overall"),
      p("Data extract date: 20th May 2021"),
      br(),
      plot_box(paste0("Weekly count of individuals having a cancer of type: ", input$cancer_type, #cancer_site,
                      " confirmed on a pathological specimen since January for 2019/2020/2021"), 
                      "cancer_incidence"),
      p("Data extract date: 20th May 2021"),
      br(),
      plot_box(paste0("Percentage change of individuals having a cancer of type: ", input$cancer_type, #cancer_site,
                      " confirmed on a pathological specimen since January for 2019/2020 and 2019/2021"), 
                      "cancer_split"),
      p("Data extract date: 20th May 2021"),
      br(),
      p("Note: registrations for non-melanoma skin cancer (ICD-10 C44) are likely to be less complete and less accurate 
        than for other cancer sites. Such cancers are relatively common and usually non-fatal. There is a propensity 
        for multiple tumours to occur in one individual and cancer registries adopt different practices in recording 
        these. The tumours are most common in the elderly population and the completeness of registration in the very 
        elderly is likely to be less than for younger patients. Furthermore, increasing numbers of these cancers are 
        diagnosed and treated within GP surgeries and the registration scheme is not confident that all such cases 
        are notified. Because cancer registries across the world have different practices for recording non-melanoma 
        skin cancer (some do not record them at all), the category 'All Malignant Neoplasms (Excl. C44)' omits these tumours 
        in the interests of making international comparisons of cancer incidence more valid."))

 
})

################# POSSIBLE ADDITION MAY 21 TO RE-ARRANGE PAGE ##############

output$cancer_explorer2 <- renderUI({
  
  # text for titles of cut charts
  # cancer_site <- case_when(input$cancer_type == "All Malignant Neoplasms (Excl. C44)" ~ "All Malignant Neoplasms (Excl. C44)",
  #                          input$cancer_type == "All Cancers" ~ "All Cancers",
  #                          input$cancer_type == "Bladder" ~ "Bladder",
  #                          input$cancer_type == "Bone and Connective Tissue" ~ "Bone and Connective Tissue",
  #                          input$cancer_type == "Breast" ~ "Breast",
  #                          input$cancer_type == "Colorectal" ~ "Colorectal",
  #                          input$cancer_type == "Head and Neck" ~ "Head and Neck",
  #                          input$cancer_type == "Hodgkin Lymphoma" ~ "Hodgkin Lymphoma",
  #                          input$cancer_type == "Kidney" ~ "Kidney",
  #                          input$cancer_type == "Leukaemias" ~ "Leukaemias",
  #                          input$cancer_type == "Liver and Intrahepatic Bile Ducts" ~ "Liver and Intrahepatic Bile Ducts",
  #                          input$cancer_type == "Brain Tumour" ~ "Brain Tumour",
  #                          input$cancer_type == "Malignant Melanoma of the Skin" ~ "Malignant Melanoma of the Skin",
  #                          input$cancer_type == "Mesothelioma" ~ "Mesothelioma",
  #                          input$cancer_type == "Multiple Myeloma and malignant plasma cell neoplasms" ~ "Multiple Myeloma and malignant plasma cell neoplasms",
  #                          input$cancer_type == "Non-Melanoma Skin Cancer" ~ "Non-Melanoma Skin Cancer",
  #                          input$cancer_type == "Oesophagus" ~ "Oesophagus",
  #                          input$cancer_type == "Other" ~ "Other",
  #                          input$cancer_type == "Ovary - Females only" ~ "Ovary - Females only",
  #                          input$cancer_type == "Pancreas" ~ "Pancreas",
  #                          input$cancer_type == "Penis - Males only" ~ "Penis - Males Only",
  #                          input$cancer_type == "Prostate - Males only" ~ "Prostate - Males only",
  #                          input$cancer_type == "Stomach" ~ "Stomach",
  #                          input$cancer_type == "Testis - Males only" ~ "Testis - Males only",
  #                          input$cancer_type == "Thyroid" ~ "Thyroid",
  #                          input$cancer_type == "Trachea, Bronchus and Lung" ~ "Trachea, Bronchus and Lung",
  #                          input$cancer_type == "Uterus - Females only" ~ "Uterus - Females only",
  #                          input$cancer_type == "Vagina - Females only" ~ "Vagina - Females only",
  #                          input$cancer_type == "Vulva - Females only" ~ "Vulva - Females only"
  # )
  
  
  tagList(
    p("Cancer services in Scotland have been disrupted since late March 2020 as a result of the coronavirus
      pandemic.  It is important to understand whether fewer patients have been diagnosed with cancer as a
      result of these changes.  The Scottish Cancer Registry will publish its high quality figures on cancer
      incidence for 2020 in 2022.  As a rapid proxy measure of new cancer diagnoses, this dashboard presents
      numbers of individuals from whom a pathology sample found cancer in 2020/2021 and compares them to 2019."),
     p(strong("Note - this does not include all patients who have been newly diagnosed with cancer, and also will include some patients
      who are being followed-up from an earlier diagnosis of cancer. ")),
    # p("In 2020, the number of individuals was similar to 2019 until the end of March.  Weekly numbers then
    #   fell by about 40% of those in 2019.  By the week ending 30th August 2020, numbers had not increased again
    #   to 2019 levels for most cancers."),
    # p("In 2020, the number of individuals was similar to 2019 until the end of March.  Weekly numbers then
    #   fell by about 40% of those in 2019.  By the week ending 21st June 2020, numbers had not increased again
    #   to 2019 levels for most cancers."),
    p("By the end of 2020 (week ending 27th December), the total number of individuals in Scotland with a pathological confirmation of 
      cancer (excluding non-melanoma skin cancers) in Scotland was 37,706 in 2020 and 44,655 in 2019, an absolute difference 
      of 6,949 individuals (an overall cumulative difference of -16%).  That is to say, around 7,000 fewer 
      patients in Scotland had a pathologically confirmed cancer diagnosis by the end of 2020 than would have 
      been expected."),
    # p("By week ending 21st June 2020, the total number of individuals in Scotland with a pathologically confirmed
    #   cancer (excluding non-melanoma skin cancers) was 16,899 in 2020 and 20,962 in 2019, an absolute difference
    #   of 4,063 individuals (and an overall cumulative difference of 19%).  Thus, around 4,000 fewer
    #   patients in Scotland had a pathologically confirmed cancer diagnosis by the end of June 2020 than would have
    #   been expected."),
    p("The commonest cancers in Scotland are of the lung, breast (females), prostate (males) and colorectal.  By the week
      ending 27th December 2020, compared to the same week in 2019, there were 761 fewer lung cancers (a total fall of 21.2%);
      1591 fewer breast cancers (a total fall of 19.1%); 624 fewer prostate cancers (a total fall of 17.9%); and 1,111 fewer colorectal
      cancers (a total fall of 21.1%)."),
    # p("The commonest cancers in Scotland are of the lung, breast (females), prostate (males) and bowel.  By the week
    #   ending 21st June 2020, compared to the same week in 2019, there were 376 fewer lung cancers (a total fall of 23%);
    #   799 fewer breast cancers (a total fall of 20%); 279 prostate cancers (a total fall of 17%); and 677 fewer bowel
    #   cancers (a total fall of 27%)."),
    p("While these numbers are only proxy measures of new cancer diagnoses in Scotland, the size of the changes
      corresponds with those reported by cancer clinicians. "),
    strong("Note: as the information provided by this dashboard is updated, it will both add more recent data, and
           may also change historical data. This commentary refers to pathological specimens reported to the week
           ending 26th February 2021, which were available for inclusion in the analysis when the data were extracted
           on 20th May 2021; the dashboard may now reflect more recent information."),
    br(),
    br(),
    p(strong("First updated: - 04/11/2020 ;  date of extraction of data: - 16/9/2020, with pathological records to week ending
      21/06/2020.  ")),
    p(strong("Last updated: - 10/03/2021 ;  date of extraction of data: - 22/02/2021, with pathological records to week ending
      29/11/2020.  ")),
    p(strong("Last updated: - 16/06/2021 ;  date of extraction of data: - 20/05/2021, with pathological records to week ending
      26/02/2021.  ")),
    p("The three graphs show numbers of individuals from whom a pathology specimen confirmed cancer since the start of
      each of the years.  The Community Health Index (CHI) was used to count individuals.  If the same individual had
      a subsequent cancer specimen reported that year for the same type of cancer, they were not counted again; but they
      were counted twice or more for those with different types of cancer. "),
    p("The first chart shows the cumulative total, or a running sum of the individuals confirmed pathologically as having
      cancer.  The second and third charts show the weekly numbers and the weekly percent differences between 2020/21 and
      2019 of new individuals confirmed each week.  "),
    p("Drop-down menus allow further details for specific Health Boards, cancer types and sex to be selected"))
    # br(),
    # plot_box(paste0("Total count of individuals having a cancer of type:  ", cancer_site,
    #                 " confirmed on a pathological specimen since January for 2019/2020"), "cancer_overall"),
    # p("Data extract date: 21st May 2021"),
    # br(),
    # plot_box(paste0("Weekly count of individuals having a cancer of type: ", cancer_site,
    #                 " confirmed on a pathological specimen since January for 2019/2020"), "cancer_incidence"),
    # p("Data extract date: 21st May 2021"),
    # br(),
    # plot_box(paste0("Percentage change of individuals having a cancer of type: ", cancer_site,
    #                 " confirmed on a pathological specimen since January for 2019/2020"), "cancer_split"),
    # p("Data extract date: 21st May 2021"),
    # br(),
    # p("Note: registrations for non-melanoma skin cancer (ICD-10 C44) are likely to be less complete and less accurate 
    #     than for other cancer sites. Such cancers are relatively common and usually non-fatal. There is a propensity 
    #     for multiple tumours to occur in one individual and cancer registries adopt different practices in recording 
    #     these. The tumours are most common in the elderly population and the completeness of registration in the very 
    #     elderly is likely to be less than for younger patients. Furthermore, increasing numbers of these cancers are 
    #     diagnosed and treated within GP surgeries and the registration scheme is not confident that all such cases 
    #     are notified. Because cancer registries across the world have different practices for recording non-melanoma 
    #     skin cancer (some do not record them at all), the category 'All Malignant Neoplasms (Excl. C44)' omits these tumours 
    #     in the interests of making international comparisons of cancer incidence more valid."))
  
  
})

###############################################.
## Charts ----
###############################################.
# Creating plots for each cut and dataset

output$cancer_overall <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                              var1_chosen = "cum_count20", 
                                              var2_chosen = "cum_count19",
                                              var3_chosen = "cum_count21",
                                              data_name = "cum")})

output$cancer_split <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                                var1_chosen = "difference20", 
                                                var2_chosen = "difference20",
                                                var3_chosen = "difference21",
                                                data_name = "dif")})

output$cancer_incidence <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                                var1_chosen = "count20", 
                                                var2_chosen = "count19",
                                                var3_chosen = "count21",
                                                data_name = "inc")})

###############################################.
## Data downloads ----
###############################################.


output$download_cancer_data <- downloadHandler(
  filename ="cancer_extract.csv",
  content = function(file) {
    write_csv(cancer_data_dl(),
              file) } 
) 

###############################################.
## Commentary ----
###############################################.
output$cancer_commentary <- renderUI({
  tagList(
    bsButton("jump_to_cancer",label = "Go to data"), #this button can only be used once
    
    h3(strong("Cancer in Scotland in 2019/2020/2021")),
    p(strong("Note: as the information provided in this dashboard is updated, it will both add more recent 
       data and may also change historical data.  This commentary includes reference to pathological specimens 
       reported to the week ending 26th February 2021, which were available for inclusion in the analysis 
       when the data were extracted on 20th May 2021.")),

    h4(strong("Background")),
    p("COVID-19 has had a wide impact on cancer in Scotland since it led to widespread social disruption 
      from the end of March 2020. Some parts of this are better understood than others. For example, cancer 
      screening programmes were paused and urgent referrals for suspected cancer fell substantially. The 
      effects on patients being less likely to seek help, delays in investigations and treatment, or changes 
      in usual treatment, are less clear."),

    p("We explored how many patients had their cancers confirmed pathologically from 2020 onwards compared with how 
      many there were in 2019, as a proxy measure of changes in cancer incidence.  However, final high-quality cancer 
      incidence data will not be published until 2022, when all potential information sources for cancer 
      have been considered by the Scottish Cancer Registry. "),

    h4(strong("What these data do and do not show")),
    p("The numbers in this dashboard are individuals from whom a pathology sample found cancer in 2019 onwards 
      in Scotland.  Each individual was counted once the first time they appeared from 1st January; any subsequent 
      samples for the same individual were not counted (except when reporting cancer type-specific numbers, where 
      an individual could contribute to more than one cancer type)."),

    p("In most cases, these indicate a new diagnosis (incidence) of cancer but in some cases they are
      follow-up samples of cancers that were diagnosed previously."),

    p("Cancer is often diagnosed initially through clinical examination (including radiology) followed by pathological 
      confirmation.  However, not all cancers are diagnosed by pathology: some are better diagnosed through other methods 
      (e.g. blood tests) and for some, the tumour is inaccessible for tissue sampling.  On average, around 80% of cancers 
      have pathological confirmation, though this varies by the type of cancer."),

    p("There is generally a 2-3 month time lag between the pathology sample being reported on by the laboratory and 
      the complete data to have been received and processed by the Scottish Cancer Registry; as such the data shown 
      in the initial release of the dashboard are for pathological samples taken for patients to the week ending 29th 
      November."),

    p("Any observed differences in numbers of pathologically confirmed cancers in 2020 compared to 2019 could be due to changes in:"),
    tags$ul(
      tags$li("patients seeking or obtaining an initial medical consultation"),
      tags$li("availability of cancer screening"),
      tags$li("availability of diagnostic services"),
      tags$li("treatment (particularly surgery, which may provide the pathology sample)"),
      tags$li("completeness of pathology data")),

    p("However, a quality assurance consultation with Scottish cancer clinicians and cancer network managers 
      suggests that the differences between 2020 and 2019 correspond to their clinical experience."),
    

    h4(strong("Overall trends in pathologically confirmed cancers")),
    p("In 2020, numbers were similar to 2019 until towards the end of March.  After the first national “lockdown”, 
      the numbers fell by about 40% of those seen in comparable weeks in 2019. Numbers then rose from late April 2020. 
      Overall, the weekly numbers of patients with pathologically confirmed cancers were close to those before the 
      pandemic by 26th February 2021, when the latest data were available, although this varied by cancer type."),

    
    ###################################
    
    h4(strong("Update 16/6/2021: For pathology data to 26th February 2020 (extracted 20/5/2021)")),
    p("By the end of 2020 (week ending 27th December), the total number of individuals in Scotland with a pathological confirmation of 
      cancer (excluding non-melanoma skin cancers) in Scotland was 37,706 in 2020 and 44,655 in 2019, an absolute difference 
      of 6,949 individuals (an overall cumulative difference of -16%).  That is to say, around 7,000 fewer 
      patients in Scotland had a pathologically confirmed cancer diagnosis by the end of 2020 than would have 
      been expected."),

    p("By December 2020, weekly numbers of pathological cancer diagnoses had risen from an initial drop of 40% at the start of the pandemic  
      to around 3% lower than in the previous year. This meant that the gap was continuing to increase but by a small amount.  In the first 
      two months of 2021 (to week ending 23rd February), the total number of individuals with a pathologically confirmed cancer (excluding 
      non-melanoma skin cancers) was 6,589, compared with 6,409 in 2020 (before the impact of the pandemic).  This suggests that the overall 
      rate of cancer diagnoses in Scotland has returned to levels that are similar to, or higher than, pre-pandemic ones.  For some cancer types, 
      numbers of diagnoses in 2021 are higher than previously and for others, lower. "),

    p("Among the most common cancer types:"),
    tags$ul(
      tags$li(" Lung cancer: The cumulative difference between 2019 and 2020 was 715 individuals (-20%).  In 2021, the cumulative difference to 
              26th February 2021 compared with the same week in 2020 was 24 fewer individuals (-5%)."),

      tags$li(" Breast cancer (females).  The cumulative difference between 2019 and 2020 was 1,557 individuals (-19%).  In 2021, the cumulative 
              difference to 26th February 2021 compared with the same week in 2020 was 22 more individuals (+2%)."),

      tags$li("	Prostate cancer. The cumulative difference between 2019 and 2020 was 624 individuals (-18%).  In 2021, the cumulative difference 
              to 26th February 2021 compared with the same week in 2020 was 5 fewer individuals (less than 1% decrease)."),

      tags$li(" Colorectal (bowel) cancer. The cumulative difference between 2019 and 2020 was 1,111 individuals (-21%).  In 2021, the 
              cumulative difference to 26th February 2021 compared with the same week in 2020 was 65 more individuals (+8%)"),

      tags$li("	Malignant melanoma of the skin.  The cumulative difference between 2019 and 2020 was 345 individuals (-18%).  In 2021, the
              cumulative difference to 26th February 2021 compared with the same week in 2020 was 24 individuals (-13%).")),
    br(),
    
        
###################################
        
    h4(strong("Update 10/3/2021: For pathology data to 29/11/2020 (extracted 22/2/2021)")),
    p("By the week ending 29th November 2020, the total number of individuals in Scotland with a pathological confirmation of 
      cancer (excluding non-melanoma skin cancers) in Scotland was 40,343 in 2019 and 33,341 in 2020, an absolute difference 
      of 7,002 individuals (-17%).  That is to say, just over 7,000 fewer 
      patients in Scotland had a pathologically confirmed cancer diagnosis by the end of November 2020 than would have 
      been expected."),

    p("After the initial fall by 40% of 2019 figures in late March, weekly numbers of pathological cancer diagnoses increased to around 10% lower than the previous 
      year’s numbers by 29th November 2020.  While the total (cumulative) difference in cancer diagnoses between 2020 and 2019 
      was therefore not increasing as much as at the beginning of the pandemic, the gap was continuing to widen rather than close. However,
      for some types of cancer by the autumn of 2020, weekly pathological cancer diagnoses were the same or higher than in 2019. "),

    p("Among the most common cancer types, by 29th November 2020:"),
    tags$ul(
      tags$li(" Lung cancers: weekly numbers had risen such that in two weeks (ending 18th October and 15th November) they were higher than in corresponding
              weeks in 2019. However, they were typically around 20% lower than in 2019 in October and November. The cumulative difference from the start
              of the year was 726 individuals (-23%)."),

      tags$li(" Breast cancer (female only): weekly numbers had risen such that in one week (ending 22nd November) they were higher than 
              in the corresponding week of 2019 for the first time since March. The cumulative difference from the start of the year was
              1615 individuals(-21%)."),

      tags$li("	Prostate cancers (male only): weekly numbers had risen such that in three weeks (ending 11th and 25th October, and 8th November)
              they were higher than in corresponding weeks of 2019. The cumulative difference from the start of the year was 590 individuals (-19%)."),

      tags$li(" Colorectal cancer: weekly numbers rose to around 10% lower than in corresponding weeks of 2019. At no point did they exceed the previous
              year's. The cumulative difference from the start of the year was 1064 individuals (-23%)."),

      tags$li(" Non-melanoma skin cancer: from late March 2020, weekly numbers fell more steeply initially (to -80% of the 2019 figures) 
              than other cancers but in six weeks (in July, September, October and November), they were higher than in corresponding weeks of 2019.
              The cumulative difference from the start of the year was 5223 individuals (-27%).")),
    br(),
    
##############################################
    
    
    h4(strong("Update 23/12/2020: For pathology data to 30/8/2020 (extracted 27/11/2020)")),
    p("By the week ending 30th August 2020, the total number of individuals in Scotland with a pathologically confirmed 
      cancer (excluding non-melanoma skin cancers) in Scotland was 23,375 in 2020 and 29,364 in 2019, an absolute difference 
      of 5,989 individuals (and an overall cumulative difference of 26%).  That is to say, just under 6,000 fewer 
      patients in Scotland had a pathologically confirmed cancer diagnosis by the end of August 2020 than would have 
      been expected."),

    p("After the initial fall by 40% of 2019 figures in late March, weekly numbers increased to just under 20% of the previous 
      year’s numbers by 30th August 2020.  While the total (cumulative) difference in cancer diagnoses between 2020 and 2019 
      was therefore not increasing as much as at the beginning of the pandemic, it was still increasing, and there continued 
      to be 20% fewer confirmed cases of cancer in 2020 than in 2019. "),

    p("Among the most common cancer types, by 30th August:"),
    tags$ul(
      tags$li(" Lung cancers: weekly numbers were down a quarter of the previous year’s (-25%) taking the cumulative difference 
              from the start of the year to 577 individuals."),
      tags$li(" Breast cancer (female only): weekly numbers were down over a quarter of the previous year’s (-27%) taking the 
              cumulative difference from the start of the year to 1320 individuals."),
      tags$li("	Prostate caners (male only): weekly numbers were down nearly 40% of the previous year’s (-39%) taking the 
              cumulative difference from the start of the year to 482 individuals."),
      tags$li(" Colorectal cancer: weekly numbers were down over a quarter of the previous year’s (-27%) taking the cumulative 
              difference from the start of the year to 950 individuals."),
      tags$li(" Non-melanoma skin cancer. from late March 2020, weekly numbers fell more steeply initially (to -80% of the 2019 figures) 
              than other cancers but by the end of August, they were down by 18% of the previous year’s.  The cumulative difference 
              from the start of the year was 4,312 individuals.")),
    br(),
    

    h4(strong("Update 18/11/2020: For pathology data to 21/6/2020 (extracted 16/9/2020)")),
    p("By the week ending 21st June 2020, the total number of individuals with a pathologically confirmed cancer 
      (excluding non-melanoma skin cancers) was 16,899 in 2020 and 20,962 in 2019, an absolute difference of 
      4,063 individuals (and an overall cumulative difference of 19%).  That is to say, around 4,000 fewer 
      patients in Scotland had a pathologically confirmed cancer diagnosis by the end of June 2020 than would 
      have been expected."),

    p("Cancer sites: (note individuals can be counted in more than one cancer site for those who happen to have more than one type of cancer)"),
    tags$ul(
      tags$li("	Lung cancer numbers fell sharply by about 40% of 2019 levels after lockdown, with no evidence 
              of return to expected numbers by the end of June 2020.  There was a total of 376 fewer individuals 
              in 2020 by w/e 21/06/2020, a cumulative fall of 23%."),
      tags$li("	Breast cancer numbers in women fell by about 40% of 2019 levels after lockdown, with no evidence 
              of return to expected numbers by the end of June 2020.  The difference was -799 individuals by w/e 
              21/06/2020, a cumulative difference of 20% lower. Part of this fall in numbers will be due to the 
              National Breast Screening Programme being paused in March, with no new invitations sent out to eligible 
              women between March and August. "),
      tags$li("	Colorectal cancers numbers initially fell by about 60% of the 2019 numbers immediately after lockdown, 
              and although there was evidence of recovery towards the expected numbers, there were still a quarter 
              fewer patients having pathologically confirmed colorectal cancers each week.  Overall, there had been 
              a total of 677 fewer individuals, or 27% cumulatively lower by w/e 21/06/2020.  It is known that there 
              have been, and continue to be delays in patients accessing colonoscopies which may explain some of the 
              greater percentage drop for confirmed colorectal cancers. Additionally, the National Bowel Screening 
              Programme was also paused in March, which will account for some of the fall in numbers."),
      tags$li(" Prostate cancer numbers fell with no evidence of return to expected numbers by the end of June 2020. 
              There were 279 fewer men confirmed by w/e 21/06/2020, a fall of 17%"),
      tags$li("	Oesophageal cancer numbers fell by over 50% immediately after lockdown, but there is evidence that 
              these have returned to expected levels by the end of June.  Cumulatively, there remains 69 fewer individuals 
              by week ending 21/06/20."),
      tags$li("	Cancers of the stomach also fell sharply after lockdown, although there have been some signs of recovery 
              to the expected numbers by 21/06/20, with 70 fewer patients cumulatively with a pathologically confirmed 
              stomach cancer."),
      tags$li("	Initially there was not a sudden fall just after lockdown, however, the expected numbers each week started 
              to diverge in May and there were 111 fewer patients with malignant melanoma of the skin confirmed pathologically 
              by w/e 21st June in 2020 compared to 2019."),
      tags$li("	Haematological cancers are not often diagnosed on the basis of a pathological sample and therefore any 
              differences observed are likely to be random variation.  Other sources of information are needed to better 
              understand the incidence of these types of cancer."),
      tags$li("	Brain tumour numbers fell.  By w/e 21st June there were 53 fewer individuals in 2020 compared with 2019, 
              a cumulative fall of 29%.  However, it is important to note that a pathological sample is often not the basis 
              of making a diagnosis of a brain tumour and therefore other sources of information are needed to better 
              understand their incidence.")),
      p("Additionally, although not generally reported in the totals for all cancers, there was a fall in the numbers of 
        patients with pathologically confirmed non-melanoma skin cancer. Immediately after lockdown the numbers fell by 
        about 80% of 2019 numbers, and although there was  some return to expected numbers by the end of June 2020, there 
        were still about 40% fewer cases each week. In total, there had been 3508 fewer patients by w/e 21/6/2020, a cumulative 
        drop of 35%."),
    
      
    p(tags$a(href="https://www.isdscotland.org/Health-Topics/Cancer/FAQ/#15", "(Data Source)"))
  
  ) 

})
