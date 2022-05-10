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
               p("The three graphs show numbers of individuals from whom a pathology specimen confirmed cancer since the start of
                 each of the years.  The Community Health Index (CHI) was used to count individuals.  If the same individual had
                 a subsequent cancer specimen reported that year for the same type of cancer, they were not counted again; but they
                 were counted twice or more for those with different types of cancer. "),
               
               p(paste0("Figures presented based on data extracted on ",cancer_extract_date)), # need to define cancer_extract_date reactive value
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive datasets ----
###############################################.


cancer_data_cum_main <- reactive({
  
  cancer_data2 %>% filter(sex == input$gender, 
                          area == input$geoname_cancer, 
                          site == input$cancer_type,
                          breakdown == "None")
  
})

# Reactive datasets for the diff charts
cancer_data_diff <- reactive({
  cancer_data_quarters %>% filter(sex == input$gender, 
                              area == input$geoname_cancer, 
                              site == input$cancer_type,
                              breakdown == input$breakdown) %>%
    # Ordering factor so it shows correctly in Plotly
    mutate(quarter_no = factor(quarter_no, ordered = TRUE)) %>% 
    arrange(quarter_no)
})

cancer_data_diff_2yrs <- reactive({
  cancer_data_quarters_2 %>% filter(sex == input$gender, 
                                  area == input$geoname_cancer, 
                                  site == input$cancer_type,
                                  breakdown == input$breakdown) %>%
    # Ordering factor so it shows correctly in Plotly
    mutate(quarter_no = factor(quarter_no, labels = c("Jan-Mar 2020", "Apr-Jun 2020",
                                                      "Jul-Sep 2020", "Oct-Dec 2020",
                                                      "Jan-Mar 2021", "Apr-Jun 2021",
                                                      "Jul-Sep 2021", "Oct-Dec 2021"), ordered = TRUE)) %>% 
    arrange(quarter_no)
})

cancer_data_dl <- reactive({
  
  cancer_data2 %>%
    filter(sex == "All" & breakdown == "None") %>% 
    select(area, site, week_number, count19, count20, count21) %>% 
    rename("Area name" = area, "Cancer type" = site,
           # "Sex" = sex,
           # "Age Group" = age_group,
           # "Deprivation Quintile (0=unknown)" = dep,
           "Week Number" = week_number,
           "Count 2019" = count19,
           "Count 2020" = count20,
           "Count 2021" = count21) #,
           # "Breakdown" = breakdown)
  
})


###############################################.
## Reactive layout ----
###############################################.

# Cancer reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_cancer <- renderUI({
  #Lists areas available in   
  areas_summary_cancer <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_cancer])
  if(input$geotype_cancer == "Health Boards") {
    selectizeInput("geoname_cancer", label = NULL, choices = c("NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
                                                               "NHS Fife", "NHS Forth Valley", "NHS Grampian", 
                                                               "NHS Greater Glasgow & Clyde", "NHS Highland",
                                                               "NHS Lanarkshire", "NHS Lothian", "NHS Orkney",
                                                               "NHS Shetland", "NHS Tayside", "NHS Western Isles"), 
                   selected = "NHS Ayrshire & Arran")
  } else if (input$geotype_cancer == "Cancer Networks") {
    selectizeInput("geoname_cancer", label = NULL, choices = c("NCA", "SCAN", "WOSCAN"), selected = "NCA")  
  } else if (input$geotype_cancer == "Scotland") {
    selectizeInput("geoname_cancer", label = NULL, choices = "Scotland", selected = "Scotland")  
  }  
})


# The charts and text shown on the app will depend on what the user wants to see
output$cancer_explorer <- renderUI({
  
  #  text for titles of cut charts
  cancer_site <- case_when(input$cancer_type == "All Malignant Neoplasms (Excl. C44)" ~ "All Malignant Neoplasms (Excl. C44)",
                           input$cancer_type == "All Cancers" ~ "All Cancers",
                           input$cancer_type == "Bladder" ~ "Bladder",
                           input$cancer_type == "Bone and Connective Tissue" ~ "Bone and Connective Tissue",
                           input$cancer_type == "Breast" ~ "Breast",
                           input$cancer_type == "Cervical - Females only" ~ "Cervical - Females only",
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
    plot_box(paste0("Total count of individuals having a cancer of type:  ", input$cancer_type, #cancer_site,
                    " confirmed on a pathological specimen since January for 2019/2020/2021"), 
             "cancer_overall"),
    p(em(paste0(cancer_extract_date), style = "font-family: 'calibri'; font-si15pt")),
    br(),
    plot_box(paste0("Weekly count of individuals having a cancer of type: ", input$cancer_type, #cancer_site,
                    " confirmed on a pathological specimen since January for 2019/2020/2021"), 
             "cancer_incidence"),
    p(em(paste0(cancer_extract_date), style = "font-family: 'calibri'; font-si15pt"))
  )  
  
})

################## ADDITION 18-8-21 TO RE-ARRANGE PAGE #####################

output$cancer_explorer3 <- renderUI({
  
  tagList(
    if(input$breakdown == "Age Group") {
      plot_box(paste0("Percentage change of individuals having a cancer of type: ", input$cancer_type, #cancer_site,
                      " confirmed on a pathological specimen by quarter against equivalent quarter 2019 by Age Group - ", 
                      input$geoname_cancer), "cancer_split")
      
    } else if (input$breakdown == "Deprivation") {
      plot_box(paste0("Percentage change of individuals having a cancer of type: ", input$cancer_type, #cancer_site,
                      " confirmed on a pathological specimen by quarter against equivalent quarter 2019 by Deprivation quintile(1 = least deprived) - ",
                       input$geoname_cancer), "cancer_split")
    } else {
      plot_box(paste0("Percentage change of individuals having a cancer of type: ", input$cancer_type, #cancer_site,
                      " confirmed on a pathological specimen by quarter against equivalent quarter 2019 - ",
                      input$geoname_cancer), "cancer_split")
    },
    
    p(em(paste0(cancer_extract_date), style = "font-family: 'calibri'; font-si15pt")),
    br(),
    p(em("Note: registrations for non-melanoma skin cancer (ICD-10 C44) are likely to be less complete and less accurate 
         than for other cancer sites. Such cancers are relatively common and usually non-fatal. There is a propensity 
         for multiple tumours to occur in one individual and cancer registries adopt different practices in recording 
         these. The tumours are most common in the elderly population and the completeness of registration in the very 
         elderly is likely to be less than for younger patients. Furthermore, increasing numbers of these cancers are 
         diagnosed and treated within GP surgeries and the registration scheme is not confident that all such cases 
         are notified. Because cancer registries across the world have different practices for recording non-melanoma 
         skin cancer (some do not record them at all), the category 'All Malignant Neoplasms (Excl. C44)' omits these tumours 
         in the interests of making international comparisons of cancer incidence more valid.", style = "font-family: 'calibri'; font-si15pt")))
  
})

################# ADDITION MAY 21 TO RE-ARRANGE PAGE ##############

output$cancer_explorer2 <- renderUI({
  
  
  
  tagList(
    p("Cancer services in Scotland have been disrupted since late March 2020 as a result of the coronavirus
      pandemic.  It is important to understand whether fewer patients have been diagnosed with cancer as a
      result of these changes.  The Scottish Cancer Registry published its high quality figures on cancer
      incidence for 2020 on 12th April 2022.  As a proxy measure of new cancer diagnoses, this dashboard presents
      numbers of individuals from whom a pathology sample found cancer in 2020/2021 and compares them to 2019.
      While only proxy measures, the size of the changes corresponds approximately with those reported by cancer clinicians."),
    p(strong("Note - this does not include all patients who have been newly diagnosed with cancer (by other methods), 
              and will also include some patients who are being followed-up from a pre-2019 diagnosis of cancer.")),
    p(("By the end of 2021 (week ending 27th December), the total number of individuals in Scotland with a pathological confirmation of 
       cancer (excluding non-melanoma skin cancers) in Scotland was "),
      strong ("33,086 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("in 2021, "),
      strong ("28,481 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("in 2020 and"),
      strong ("33,345 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("in 2019, absolute differences of "), 
      strong ("4,864 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("individuals in 2020 (an overall cumulative difference of "),
      strong ("-14.7% ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("), and "),
      strong ("259 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("individuals in 2021 (an overall cumulative difference of "),
      strong ("-0.77% ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      (").  That is to say, "),
      strong ("4,864 / 259 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("fewer patients in Scotland had a pathologically confirmed cancer diagnosis by the end of 2020/2021 than would have 
       been expected.")),
    p("The commonest cancers in Scotland are of the lung, breast (females), prostate (males) and colorectal."),
    p("By the week ending 27th December 2021, compared to the same week in 2019, there were ",
      strong ("583 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
        "fewer lung cancers (a total fall of ",
      strong ("20.6%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "); ",
      strong ("848 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "fewer breast cancers (a total fall of ",
      strong ("15.8%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "); ",
      strong ("556 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "fewer prostate cancers (a total fall of ",
      strong ("17.7%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "); and ",
      strong ("854 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "fewer colorectal cancers (a total fall of ",
      strong ("21.1%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "). "),
    p("By the week ending 27th December 2021, compared to the same week in 2019, there were ",
      strong ("514 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "fewer lung cancers (a total fall of ",
      strong ("18.1%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "); ",
      strong ("4 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "fewer breast cancers (a total fall of less than",
      strong ("1%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "); ",
      strong ("503 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "more prostate cancers (a total increase of ",
      strong ("16%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "); and ",
      strong ("16 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "more colorectal cancers (a total increase of less than",
      strong ("1%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      "). "),
    strong("Note: as the information provided by this dashboard is updated, it will both add more recent data, and
           may also update historical data."),
    br(),
    p(strong(paste0("Last updated: - 20/04/2022 ;  date of extraction of data: ",cancer_extract_date, ", with pathological records to week ending
             31/12/2021."))))
  
  
})

###############################################.
## Charts ----
###############################################.
# Creating plots for each cut and dataset

output$cancer_overall <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                                                 var1_chosen = "cum_count20",
                                                                 if(input$baseline == "2019"){
                                                                   var2_chosen = "cum_count19"
                                                                 } else {
                                                                   var2_chosen = "cum_count_mean_17_19"  
                                                                 },
                                                                 var3_chosen = "cum_count21",
                                                                 data_name = "cum")})

output$cancer_incidence <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                                                   var1_chosen = "count20",
                                                                   if(input$baseline == "2019"){
                                                                     var2_chosen = "count19"
                                                                   } else {
                                                                     var2_chosen = "count_mean_17_19"  
                                                                   },
                                                                   var3_chosen = "count21",
                                                                   data_name = "inc")})

# Difference charts
output$cancer_split <- renderPlotly({plot_diff_cancer_chart(cancer_data_diff_2yrs(), periodvar = "quarter_no",
                                                            if(input$cum_baseline == "Standard"){
                                                              diffvar1 = "difference"
                                                              } else {
                                                              diffvar1 = "cum_difference"  
                                                              })})

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
             data and may also change historical data. This commentary includes reference to pathological specimens 
             reported to the week ending 31st December 2021, which were available for inclusion in the analysis 
             when the data were extracted on 20th April 2022.")),
    # p(strong("29/07/21 - Following a quality assurance exercise, a mistake was found in the methodology used to identify 
    #          unique patients; this has been corrected.  In addition, additional improvements were made in the identification 
    #          of non-residents of Scotland and in the identification of inappropriate cancer type/sex combinations. As such 
    #          there have been some revisions made to the numbers reported for the pathological specimens reported to the week 
    #          ending 21st February 2021, extracted on 20th May 2021. These revisions are shown in red.",
    #          style = "font-family: 'arial'; font-si20pt; color: #DC143C;")),
    
    h4(strong("Background")),
    p("COVID-19 has had a wide impact on cancer in Scotland since it led to widespread social disruption 
      from the end of March 2020. Some parts of this are better understood than others. For example, cancer 
      screening programmes were paused and urgent referrals for suspected cancer fell substantially. The 
      effects on patients being less likely to seek help, delays in investigations and treatment, or changes 
      in usual treatment, are less clear. We explored how many patients had their cancers confirmed pathologically from 2020 onwards compared with how 
      many there were in 2019, as a proxy measure of changes in cancer incidence. "),
    
    h4(strong("What these data do and do not show")),
    p("The numbers in this dashboard are individuals from whom a pathology sample found cancer in 2019 onwards 
      in Scotland. Each individual was counted once the first time they appeared from 1st January; any subsequent 
      samples for the same individual were not counted (except when reporting cancer type-specific numbers, where 
      an individual could contribute to more than one cancer type)."),
    
    p("In most cases, these indicate a new diagnosis (incidence) of cancer but in some cases they are
      follow-up samples of cancers that were diagnosed previously."),
    
    p("Cancer is often diagnosed initially through clinical examination (including radiology) followed by pathological 
      confirmation. However, not all cancers are diagnosed by pathology: some are better diagnosed through other methods 
      (e.g. blood tests) and for some, the tumour is inaccessible for tissue sampling. On average, around 80% of cancers 
      have pathological confirmation, though this varies by the type of cancer."),
    
    p("There is generally a 2-3 month time lag between the pathology sample being reported on by the laboratory and 
      the complete data to have been received and processed by the Scottish Cancer Registry; as such the data shown 
      in the dashboard are for pathological samples taken for patients to the week ending 31st December 2021."),
    
    p("Any observed differences in numbers of pathologically confirmed cancers in 2020 (or 2021) compared to 2019 could be due to changes in:"),
    tags$ul(
      tags$li("patients seeking or obtaining an initial medical consultation"),
      tags$li("availability of cancer screening"),
      tags$li("availability of diagnostic services"),
      tags$li("treatment (particularly surgery, which may provide the pathology sample)"),
      tags$li("completeness of pathology data")),
    
    p("However, a quality assurance consultation with Scottish cancer clinicians and cancer network managers 
      suggests that the differences between 2020 and 2019 correspond to their clinical experience."),
    
    
    h4(strong("Overall trends in pathologically confirmed cancers")),
    p("In 2020, numbers were similar to 2019 until towards the end of March. After the first national lockdown, 
      the numbers fell by about 40% of those seen in comparable weeks in 2019. Numbers then rose from late April 2020. 
      Overall, the weekly numbers of patients with pathologically confirmed cancers were close to those before the 
      pandemic by 26th February 2021, when the latest data were available, although this varied by cancer type."),
    p("In 2021, numbers had returned closer to those seen in 2019. Overall, the weekly numbers of patients with pathologically confirmed cancers were close to those before the 
      pandemic, although again, this varied by cancer type."),
 
    #################################################################################################################   .
    
    # UPDATES ----
    
    
    ###################################.
    
    h4(strong("**DRAFT** Update --/05/2022: For pathology data to 31st December 2021 (extracted 20/04/2022)")),
    p("In 2021 (weeks ending 05th January to 27th December), there was little difference in the total number
      of individuals with a pathological diagnosis of cancer (Excl. C44) compared with those in 2019 (33086 and 33345 in 2021 and 2019
      respectively, a difference of approximately 0.8%).  However, within cancer sites, some were higher and some lower than
      expected in 2021 compared with 2019."),
    
    p("Among the most common cancer types, comparing January to week ending 27th December in 2021 and 2019:"),
    tags$ul(
      tags$li("Lung cancer: 2,319 versus 2,833 pathological diagnoses; a decrease
              of 583 individuals, or 20.6% lower."),
      
      tags$li("Breast cancer: 5,362 versus 5,358 pathological diagnoses; 
              an increase of 4 individuals, or 0.7% higher"),
      
      tags$li("Prostate cancer: 3,646 versus 3,143 pathological diagnoses; an increase
              of 503 individuals, or 16% higher."),
      
      tags$li("Colorectal (bowel) cancer: 4,057 versus 4,041 pathological diagnoses; an
              increase of 16 individuals, or 0.4% higher."),
      
      tags$li("Liver and intrahepatic bile ducts: 379 versus 330 pathological diagnoses;
              an increase of 49 individuals, or 14.8% higher."),
      
      tags$li("Oesophagus: 1182 versus 1006 pathological diagnoses; an increase
              of 176 individuals, or 17.5% higher.")),
    br(),
    p("A new chart comparing 2019 quarterly totals to the same quarters in 2020 and 2021 shows that after initial falls in diagnoses, 
      there was some recovery or catching-up in the cumulative figures, repeating 2019 figures over two years for comparison with the 2020-2021 period:"),
    tags$ul(
      tags$li("Lung cancer: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 19.3% lower."),
      
      tags$li("Breast cancer: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 7.6% lower."),
      
      tags$li("Prostate cancer: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 0.4% lower."),
      
      tags$li("Colorectal(bowel) cancer: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 10% lower."),
      
      tags$li("Liver and intrahepatic bile ducts: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 6.5% higher."),
      
      tags$li("Oesophagus: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 6.3% higher."),
    
      tags$li("Cervical: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 10.2% lower.")),
      
    p(strong("New information on age and socio-economic deprivation has been added to the dashboard, reviewing the annual
             data to the end of December 2021 compared to 2019:")),
    
    h4(strong("Age")),
    p("For all cancers except non-melanoma skin cancers, the proportionate fall in pathologically confirmed cancers by December 2021 were
      much smaller than at the end of 2020, and there was less difference between age groups.
      The reduction in breast and colorectal cancer diagnoses, which were both affected by pauses in the screening
      programmes for people aged 50-69, were smaller by December 2021 than December 2020. For example, for breast cancer, 
      pathological cancer diagnoses in those aged 50-69 showed a shortfall of 9.1% 
      in those of screening age (50-69) by the end of Q4 2021, compared to a shortfall of 1.6% for those under 50.
      For colorectal cancers, numbers in 50-69 years olds showed a shortfall of
      11.2% by the end of Q4 2021; however, the total falls in under 50s had shown less improvement, a shortfall of 17.2%."),
    h4(strong("Socio-economic deprivation")),
    p("For deprivation, the least and most deprived quintiles are highlighted in colour.  For all cancers except non-melanomas
      skin cancers, these showed the largest decreases in diagnoses remained among people from the most deprived areas, showing 
      a shortfall of 12.9% by the end of Q4 2021.The least deprived were down 6% by the same period."),
    p("---------------------------------------------------------------------------------------------------------------------"),
    br(),
    
   
    
    ###################################.
    
    h4(strong("Update 22/9/2021: For pathology data to 14th June 2021 (extracted 19/8/2021)")),
    p("In the first half of 2021 (weeks ending 05 January to 14 June), there was little difference in the total number
      of individuals with a pathological diagnosis of cancer compared with those in 2019 (16455 and 16569 in 2021 and 2019,
      respectively, a difference of less than 1%).  However, within cancer sites, some were higher and some lower than
      expected in 2021 compared with 2019."),
    
    p("Among the most common cancer types, comparing January to week ending 14th June in 2021 and 2019:"),
    tags$ul(
      tags$li("Lung cancer: 1,102 versus 1,328 pathological diagnoses; a decrease
              of 226 individuals, or 17% lower."),
      
      tags$li("Breast cancer (females): 2,658 versus 2,774 pathological diagnoses; 
              a decrease of 116 individuals, or 4% lower."),
      
      tags$li("Prostate cancer: 1,581 versus 1,544 pathological diagnoses; an increase
              of 37 individuals, or 2% higher."),
      
      tags$li("Colorectal (bowel) cancer: 1,958 versus 2,011 pathological diagnoses; a
              decrease of 53 individuals, or 3% lower."),
      
      tags$li("Liver and intrahepatic bile ducts: 166 versus 147 pathological diagnoses;
              an increase of 19 individuals, or 11% higher."),
      
      tags$li("Oesophagus: 579 versus 473 pathological diagnoses; an increase
              of 106 individuals, or 22% higher.")),
    br(),
    p("A new quarterly chart of cumulative numbers shows that after initial falls in diagnoses, there was some recovery
      or catching-up.  For all cancers except non-melanoma skin cancers, there had been a drop to -14% of 2019 numbers
      by the end of Quarter 3 (Q3) but this increased to -12% by the end of 2020.   A similar pattern of maximum fall
      by the end of Q3 with a small recovery by the end of Q4 was seen for the commonest cancers – lung, breast, colorectal,
      and prostate."),
    p(strong("New information on age and socio-economic deprivation has been added to the dashboard, reviewing the annual
             data to the end of December in 2020 compared to 2019:")),
    h4(strong("Age")),
    p("For all cancers except non-melanoma skin cancers, the largest proportionate fall in
      pathologically confirmed cancers were among those aged 50-69 years and the smallest falls were in those under 50 years.
      This difference is more clearly seen in breast and colorectal cancers, which were both affected by pauses in the screening
      programmes for people aged 50-70 and 50-74, respectively.  For example, for breast cancer, there was little difference in
      pathological cancer diagnoses in those aged under 50; and a maximum fall of -24% in those of screening age (50-69) by the
      end of Q3, with some recovery (to -19%) by the end of the year.  For colorectal cancers, numbers in 50-69 years olds fell
      -20% by the end of Q3 with little recovery by the end of the year; while the total annual falls in under 50s and 70 and over
      were -17% and -14%, respectively."),
    br(),
    h4(strong("Socio-economic deprivation")),
    p("For deprivation, the least and most deprived quintiles are highlighted in colour.  For all cancers except non-melanomas
      skin cancers, these showed the largest decreases in diagnoses were among people from the most deprived areas (a maximum
      fall of -18% by the end of Q3).The smallest was among the least deprived (-11% by Q3). There was a greater recovery in the most
      deprived and some narrowing of the deprivation gap by the end of the year.  Nevertheless, the end-of-year differences in
      numbers of diagnoses were -9% among the least deprived and -14% in the most deprived.  This general pattern – that reductions in
      diagnoses were greater among people from more deprived areas – were seen across cancer types.  In lung cancer, the most
      deprived experienced a -24% reduction in diagnoses by the end of 2020 compared with -12% in the least deprived.  For
      breast cancer in women, the deprivation gap was wider: a fall of -20% in women from the most deprived quintile compared
      with a fall of -6% from those from the least deprived quintile.  For colorectal cancer, the pattern across socio-economic
      groups was a little less clear, although the reduction in diagnoses was smallest for those in the least deprived quintile
      (-11%) and greater for those in the most deprived quintile (-20%), but the greatest was for those in the second most deprived quintile
      (-25%).  For prostate cancer, the greatest reduction in diagnoses by the end of the year was -23% for those in the most
      deprived areas; -15% for those in the least deprived areas; and smallest for those in the middle quintile (-12%)."),
  
 
 
    ###################################.
    
    h4(strong("Update 29/7/2021: For pathology data to 26th February 2021 (extracted 20/5/2021)")),
    p(("By the end of 2020 (week ending 27th December), the total number of individuals in Scotland with a pathological confirmation of 
       cancer (excluding non-melanoma skin cancers) in Scotland was "),  
      strong ("28,474 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("in 2020 and"),
      strong ("33,343 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("in 2019, an absolute difference of "), 
      strong ("4,869 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("individuals (an overall cumulative difference of "),
      strong ("-14.6% ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      (").  That is to say, more than "),
      strong ("4,800 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
      ("fewer patients in Scotland had a pathologically confirmed cancer diagnosis by the 27th of December 2020 than would have 
       been expected.")),
    
    p("By December 2020, weekly numbers of pathological cancer diagnoses had risen from an initial drop of 40% at the start of the pandemic  
      to around 3% lower than in the previous year. This meant that the gap was continuing to increase but by a small amount.  In the first 
      two months of 2021 (to week ending 21st February), the total number of individuals with a pathologically confirmed cancer (excluding 
      non-melanoma skin cancers) was 5,922, compared with 5,844 in 2020 (before the impact of the pandemic).  This suggests that the overall  
      rate of cancer diagnoses in Scotland has returned to levels that are similar to, or higher than, pre-pandemic ones.  For some cancer types, 
      numbers of diagnoses in 2021 are higher than previously and for others, lower. "),
    
    p("Among the most common cancer types:"),
    tags$ul(
      tags$li("Lung cancer: The cumulative difference between 2019 and 2020 was",
              strong ("584 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "individuals (",
              strong ("-20.6%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "). In 2021, the cumulative difference to 
              21st February 2021 compared with the same week in 2020 was",
              strong ("40 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              " fewer individuals (",
              strong ("-9.6%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              ")."
      ),
      tags$li("Breast cancer: The cumulative difference between 2019 and 2020 was",
              strong ("849 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "individuals (",
              strong ("-15.8%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "). In 2021, the cumulative difference to 
              21st February 2021 compared with the same week in 2020 was",
              strong ("17 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              " more individuals (",
              strong ("1.6%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              ")."
      ),
      tags$li("Prostate cancer: The cumulative difference between 2019 and 2020 was",
              strong ("556 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "individuals (",
              strong ("-17.7%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "). In 2021, the cumulative difference to 
              21st February 2021 compared with the same week in 2020 was",
              strong ("6 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              " fewer individuals (",
              strong ("-1.1%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              ")."
      ),
      tags$li("Colorectal cancer: The cumulative difference between 2019 and 2020 was",
              strong ("851 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "individuals (",
              strong ("-21.1%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "). In 2021, the cumulative difference to 
              21st February 2021 compared with the same week in 2020 was",
              strong ("31 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              " fewer individuals (",
              strong ("-4.2%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              ")."
      ),
      tags$li("Malignant melanoma of the skin: The cumulative difference between 2019 and 2020 was",
              strong ("338 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "individuals (",
              strong ("-20.2%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              "). In 2021, the cumulative difference to 
              21st February 2021 compared with the same week in 2020 was",
              strong ("32 ", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              " fewer individuals (",
              strong ("-12.8%", style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
              ")."
      )
    ),
    br(),
    
    
    ###################################.
    
    h4(strong("Update 10/3/2021: For pathology data to 29/11/2020 (extracted 22/2/2021)")),
    p("By the week ending 29th November 2020, the total number of individuals in Scotland with a pathological confirmation of 
      cancer (excluding non-melanoma skin cancers) in Scotland was 40,343 in 2019 and 33,341 in 2020, an absolute difference 
      of 7,002 individuals (-17%).  That is to say, just over 7,000 fewer 
      patients in Scotland had a pathologically confirmed cancer diagnosis by the end of November 2020 than would have 
      been expected."),
    
    p("After the initial fall by 40% of 2019 figures in late March, weekly numbers of pathological cancer diagnoses increased to around 10% lower than the previous 
      year's numbers by 29th November 2020.  While the total (cumulative) difference in cancer diagnoses between 2020 and 2019 
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
    
    ##############################################.
    
    
    h4(strong("Update 23/12/2020: For pathology data to 30/8/2020 (extracted 27/11/2020)")),
    p("By the week ending 30th August 2020, the total number of individuals in Scotland with a pathologically confirmed 
      cancer (excluding non-melanoma skin cancers) in Scotland was 23,375 in 2020 and 29,364 in 2019, an absolute difference 
      of 5,989 individuals (and an overall cumulative difference of 26%).  That is to say, just under 6,000 fewer 
      patients in Scotland had a pathologically confirmed cancer diagnosis by the end of August 2020 than would have 
      been expected."),
    
    p("After the initial fall by 40% of 2019 figures in late March, weekly numbers increased to just under 20% of the previous 
      year's numbers by 30th August 2020.  While the total (cumulative) difference in cancer diagnoses between 2020 and 2019 
      was therefore not increasing as much as at the beginning of the pandemic, it was still increasing, and there continued 
      to be 20% fewer confirmed cases of cancer in 2020 than in 2019. "),
    
    p("Among the most common cancer types, by 30th August:"),
    tags$ul(
      tags$li(" Lung cancers: weekly numbers were down a quarter of the previous year's (-25%) taking the cumulative difference 
              from the start of the year to 577 individuals."),
      tags$li(" Breast cancer (female only): weekly numbers were down over a quarter of the previous year's (-27%) taking the 
              cumulative difference from the start of the year to 1320 individuals."),
      tags$li("	Prostate cancers (male only): weekly numbers were down nearly 40% of the previous year's (-39%) taking the 
              cumulative difference from the start of the year to 482 individuals."),
      tags$li(" Colorectal cancer: weekly numbers were down over a quarter of the previous year's (-27%) taking the cumulative 
              difference from the start of the year to 950 individuals."),
      tags$li(" Non-melanoma skin cancer. from late March 2020, weekly numbers fell more steeply initially (to -80% of the 2019 figures) 
              than other cancers but by the end of August, they were down by 18% of the previous year's.  The cumulative difference 
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