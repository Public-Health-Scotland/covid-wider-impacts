# Wider impacts dashboard - Cancer tab
# UI code


cancerpath_tab <- 
###############################################.
## Cancer pathology ----
###############################################.

    tabPanel(title = "Cancer pathology", icon = icon("microscope"), value = "cancer",
           wellPanel(uiOutput("cancer_explorer2"),
             column(3, selectInput("geotype_cancer", label = "Step 1. Select a geography level and then an area of interest.",
                                   choices= c("Scotland", "Cancer Networks", "Health Boards"),
                                   selected = "Scotland"),
                    uiOutput("geoname_ui_cancer")),
             column(3, selectInput("cancer_type", label = "Step 2. Select all or specific cancer type", choices = cancer_type_list,
                                selected = "All Malignant Neoplasms (Excl. C44)")),
             column(3,  radioButtons("baseline", "Step 3. Select baseline for comparison",
                                     list("2019", "Mean 2017-2019"), inline = TRUE,
                                     selected = "2019"),
                    radioButtons("gender", "Step 4. Select sex",
                                 list("All","Male","Female"), inline = TRUE,
                                 selected = "All")),
             column(3,
                    sourcemodal_ui("cancer"),
                    fluidRow(br()),
                    downloadButton('download_cancer_data', 'Download data'),
                    fluidRow(br()),
                    actionButton('jump_commentary_cancer','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("cancer_explorer"),
             column(6,
                    div(radioButtons("cum_baseline", "Select standard/cumulative baseline",
                                     list("Standard", "Cumulative"), inline = TRUE,
                                     selected = "Standard"))),
             column(6,
                    div(radioButtons("breakdown", "Select breakdown type",
                                     list("None","Age Group","Deprivation"), inline = TRUE,
                                     selected = "None"))),
                     uiOutput("cancer_explorer3")
             )#mainPanel bracket
  )  # tabpanel bracket


###############################################.
## SACT monthly tab ----
###############################################.

sact_tabm <- 
  tabPanel(title = "SACT (Chemotherapy) monthly patients ", icon = icon("syringe"), value = "sact",
           wellPanel(h4(strong("SACT treatment activity in Scotland - Monthly patient data")),
                     p("Systemic Anti-Cancer Treatments (SACT) is a collective term for drugs that are used in the treatment
                                  of cancer. The main type of drugs are cytotoxic chemotherapy drugs but there are other treatments
                                  such as targeted agents and immunotherapies."),
                     p("The weekly and monthly activity reports are generated from the SACT national MVP data platform
                                  held by PHS, which is updated weekly from the five instances of ChemoCare across Scotland.
                                  All SACT and non-SACT (e.g. other drugs used to treat cancer such as hormones and supportive medicines
                                  such as anti-sickness medicines and steroids) activity which is prescribed
                                  in secondary care settings and is recorded on ChemoCare is included. Paediatric patient activity
                                  and prescriptions not recorded on a ChemoCare system are not included."),
                     p("Local values have been used in the calculations, however, national mappings
                                  and derivations were applied to define tumour groups and identify the administration route."),
                     p("Due to differences in recording practice",
                       em(strong("it would be inappropriate to make direct comparisons between the cancer networks.")),
                       style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
                     
                     actionButton("btn_sact_modal", "FAQs", icon = icon('question-circle')),
                     downloadButton('download_sact_monthly_data', 'Download data')), # well panel
           
           wellPanel(column(4, selectInput("geotype_sact", label = "Step 1. Select a geography level and then an area of interest",
                                           choices= c("Scotland", "Cancer Network", "Health Board"),selected = "Scotland"),
                            uiOutput("geoname_ui_sact")),
                      column(3,   selectInput("sact_type", label = "Step 2. Select all or specific cancer type",
                                        choices = c("All", "Breast", "Cancer of Unknown Origin", "Central Nervous System",
                                                    "Germ Cell", "Gynaecology", "Haematology", "Head & Neck", "Lower GI",
                                                    "Lung & Chest", "Neuroendocrine", "Other", "Sarcoma", "Skin",
                                                    "Upper GI", "Urological"), selected = "All")),
                     column(5,  uiOutput("treatment_ui_sact"),
                            div(radioButtons("sact_plot_filter", "Step 4. Select data breakdown to display together:",
                                             list("Geographic area","Treatment administration", "Standard graph"), inline = TRUE,
                                             selected = "Standard graph")))
           ), #well panel
           
           mainPanel(width = 12,
                     uiOutput("sact_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket

###############################################.
## SACT weekly tab ----
###############################################.

sact_tabw <- 
  tabPanel(title = "SACT (Chemotherapy) weekly appointments", icon = icon("syringe"), value = "sact",
           wellPanel(h4(strong("SACT treatment activity in Scotland - Weekly appointment data")),
                     #p(strong("Data from the ChemoCare system in the North Cancer Alliance (NCA) Highland has not
                     #          refreshed on the 28th of June 2021. Data from this instance is therefore only deemed
                     #         complete up to the week beginning 7th of June and only presented in the graphs up to
                     #        that date. This affects Scotland level data, NCA network level data and NCA Highland
                     #       health boards: NHS Highland and NHS Western Isles."),
                     #style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
                     p("Systemic Anti-Cancer Treatments (SACT) is a collective term for drugs that are used in the treatment
                                  of cancer. The main type of drugs are cytotoxic chemotherapy drugs but there are other treatments
                                  such as targeted agents and immunotherapies."),
                     p("The weekly and monthly activity reports are generated from the SACT national MVP data platform
                                  held by PHS, which is updated weekly from the five instances of ChemoCare across Scotland.
                                  All SACT and non-SACT (e.g. other drugs used to treat cancer such as hormones and supportive medicines
                                  such as anti-sickness medicines and steroids) activity which is prescribed
                                  in secondary care settings and is recorded on ChemoCare is included. Paediatric patient activity
                                  and prescriptions not recorded on a ChemoCare system are not included."),
                     p("Local values have been used in the calculations, however, national mappings
                                  and derivations were applied to define tumour groups and identify the administration route."),
                     p("Due to differences in recording practice",
                       em(strong("it would be inappropriate to make direct comparisons between the cancer networks.")),
                       style = "font-family: 'arial'; font-si20pt; color: #DC143C;"),
                     p("Activity data is released two week in arrears. The latest data currently available in the
                                  dashboard is for the week beginning", strong(format(max(sact_weekly_data$week_beginning), "%d %B %Y"))),
                     
                     actionButton("btn_sact_wk_modal", "FAQs", icon = icon('question-circle')),
                     downloadButton('download_sact_weekly_data', 'Download data')), # well panel
           
           wellPanel(column(4, selectInput("geotype_wk_sact", label = "Step 1. Select a geography level and then an area of interest",
                                           choices= c("Scotland", "Cancer Network", "Health Board"),selected = "Scotland"),
                            uiOutput("geoname_ui_wk_sact")),
                     column(3, selectInput("sact_wk_type", label = "Step 2. Select all or specific cancer type",
                                        choices = c("All", "Breast", "Cancer of Unknown Origin", "Central Nervous System",
                                                    "Germ Cell", "Gynaecology", "Haematology", "Head & Neck", "Lower GI",
                                                    "Lung & Chest", "Neuroendocrine", "Other", "Sarcoma", "Skin",
                                                    "Upper GI", "Urological"), selected = "All")),
                     column(5, div(radioButtons("sact_wk_appt_reg", "Step 3. Select method of administration route derivation",
                                             list("Appointment level","Regimen level"), inline = TRUE,
                                             selected = "Appointment level")),
                            div(radioButtons("sact_plot_wk_filter", "Step 4. Select data breakdown to display together:",
                                             list("Geographic area","Treatment administration", "Standard graph"), inline = TRUE,
                                             selected = "Standard graph")),
                            uiOutput("treatment_ui_wk_sact"))
           ), #well panel
           
           mainPanel(width = 12,
                     uiOutput("sact_wk_explorer")
           )# mainPanel bracket
  ) #, # tabpanel bracket

###############################################.
## DCE - not in use ----
###############################################.

# dce_tab <-             
#   tabPanel(title = "Cancer Staging - DCE Data", icon = icon("clock"), value = "dce",
#            wellPanel(h4(strong("Cancer Staging - Detect Cancer Early Data (Breast, Colorectal & Lung)")),
#                      # p("Cancer is one of the major causes of death in Scotland. In 2018, 16,153 people died of cancer
#                      #    in Scotland and approximately 34,000 people were diagnosed with cancer, excluding non-melanoma
#                      #      skin cancer. The most common causes of cancer diagnosis are lung, breast, prostate and colorectal cancer."),
#                      # p("In February 2012 the Cabinet Secretary for Health and Wellbeing formally launched the Detect Cancer Early
#                      #    programme . One aim of the Detect Cancer Early programme was to increase the proportion of people who were
#                      #    diagnosed early in the disease process (with stage 1 disease). The programme concentrates on breast, colorectal
#                      #    and lung cancers, which collectively account for 42.6% of all cancers diagnosed in Scotland in 2018."),
#                      p("Cancer staging is the process of determining the extent to which a cancer has developed and spread.
#                                   For the majority of patients with cancer it is common practice to assign a number from 1 to 4 to a cancer,
#                                   with 1 indicating the cancer is confined to the original organ in which it occurred and 4 being a cancer
#                                   which has spread beyond the original organ and its local lymph glands (regional lymph nodes). Patients
#                                   diagnosed with stage 1 disease tend to have better outcomes and longer survival compared with patients
#                                   diagnosed with stage 4 disease."),
#                      p("This dashboard looks at breast, colorectal and lung cancer data separately to examine the
#                                   different changes on stage at diagnosis from the year the coronavirus pandemic began compared with 2019."),
#                      p("The proportions of patients with any given cancer stage may be affected by a number of things, including
#                                   changes in the proportions of other stages (including those that are not known).  These data can only
#                                   describe patients who were diagnosed with cancer and in a separate section of this dashboard, it is estimated
#                                   that total breast, colorectal and lung cancer diagnoses fell by 16%, 21% and 21%, respectively, in 2020
#                                   compared with 2019. Temporary pausing of the national screening programmes for breast and colorectal cancer
#                                   in 2020 is likely to have particularly reduced numbers of early stage cancers being diagnosed.  A full
#                                   understanding of the various determinants of any changes in stage of cancer when it was diagnosed after
#                                   the pandemic began, and of the status of the people who were not diagnosed with cancer as expected in 2020,
#                                   will take time to be reached."),
#                      # tags$ul(
#                      #   tags$li("For breast cancer, there were large falls numbers in stages 1 and 2 (35% and 15% respectively). In
#                      #           contrast, there were small increases in stages 3 and 4 (5% and 7%), with the biggest increase seen for
#                      #           those of unknown stage (34%)."),
#                      #
#                      #   tags$li("For Colorectal Cancer, there were substantial drops (30% and more) in the numbers diagnosed with
#                      #           stages 1, 2 or 3 colorectal cancer; whereas there was only a 4% drop for metastatic colorectal cancer."),
#                      #
#                      #   tags$li("For Lung Cancer, there were falls of 11%-13% for stages 1, 2 and 3; but only a fall of 4% for stage 4
#                      #           diagnoses, which was only lower than expected in April 2020.")),
#                      
#                      p(strong(paste0("Figures presented based on data extracted on ",dce_extract_date)))
#            ),
#            wellPanel(
#              column(5, selectInput("geotype_dce", label = "Select a geography level",
#                                    choices= c("Scotland", "Cancer Network"),
#                                    selected = "Scotland"),
#                     uiOutput("geoname_ui_dce")),
#              column(5,  selectInput("dce_type", label = "Select all or specific cancer type",
#                                     choices = c("Breast", "Colorectal", "Lung"), selected = "Breast")),
#              column(2,
#                     fluidRow(br()),
#                     actionButton("btn_dce_modal", "Data source: ", icon = icon('question-circle')),
#                     fluidRow(br()),
#                     downloadButton('download_dce_data', 'Download data')) #,
#            ) , #well panel
#            mainPanel(width = 12,
#                      uiOutput("dce_explorer1") ,
#                      div(radioButtons("dce_stage", "Select stage of cancer (NK - Not Known)",
#                                       list("1","2","3","4","NK"), inline = TRUE,
#                                       selected = "1")),
#                      uiOutput("dce_explorer2")
#            )# mainPanel bracket
#   ) # tabpanel bracket


###############################################.
## Commentary ----
###############################################.
cancer_commentary <- 
  tagList(
    fluidRow(
      column(8, h2("Cancer in Scotland in 2019 to 2022")), 
      column(4, div(bsButton("jump_to_cancer", label = "Go to data"), style="float:right"))),  #this button can only be used once
    p(strong("Note: as the information provided in this dashboard is updated, it will both add more recent 
             data and may also change historical data. This commentary includes reference to pathological specimens 
             reported to the week ending 2nd February 2022, which were available for inclusion in the analysis 
             when the data were extracted on 2nd June 2022.")),
    # p(strong("29/07/21 - Following a quality assurance exercise, a mistake was found in the methodology used to identify 
    #          unique patients; this has been corrected.  In addition, additional improvements were made in the identification 
    #          of non-residents of Scotland and in the identification of inappropriate cancer type/sex combinations. As such 
    #          there have been some revisions made to the numbers reported for the pathological specimens reported to the week 
    #          ending 21st February 2021, extracted on 20th May 2021. These revisions are shown in red.",
    #          style = "font-family: 'arial'; font-si20pt; color: #DC143C;")),
    
    h4("Background"),
    p("COVID-19 has had a wide impact on cancer in Scotland since it led to widespread social disruption 
      from the end of March 2020. Some parts of this are better understood than others. For example, cancer 
      screening programmes were paused and urgent referrals for suspected cancer fell substantially. The 
      effects on patients being less likely to seek help, delays in investigations and treatment, or changes 
      in usual treatment, are less clear. We explored how many patients had their cancers confirmed pathologically from 2020 onwards compared with how 
      many there were in 2019, as a proxy measure of changes in cancer incidence. "),
    
    h4("What these data do and do not show"),
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
      the complete data being received and processed by the Scottish Cancer Registry; as such the data shown 
      in the dashboard are for pathological samples taken for patients to the 2nd February 2022."),
    
    p("Any observed differences in numbers of pathologically confirmed cancers from 2020 onwards, compared to 2019, could be due to changes in:"),
    tags$ul(
      tags$li("patients seeking or obtaining an initial medical consultation"),
      tags$li("availability of cancer screening"),
      tags$li("availability of diagnostic services"),
      tags$li("treatment (particularly surgery, which may provide the pathology sample)"),
      tags$li("completeness of pathology data"),
      tags$li("true changes in cancer occurence (incidence)")),
    
    p("Since the definitive 2020 cancer incidence data were published in June 2022, quality assurance 
      work is being carried out to validate the pathology dashboard figures."),
    
    
    h4("Overall trends in pathologically confirmed cancers"),
    p("In 2020, numbers were similar to 2019 until towards the end of March. After the first national lockdown, 
      the numbers fell by about 40% of those seen in comparable weeks in 2019. Numbers then rose from late April 2020. 
      Overall, the weekly numbers of patients with pathologically confirmed cancers were close to those before the 
      pandemic by the end of September 2020. It should be noted that there were important variations in patterns between types of cancer."),
    p("In 2021, overall numbers remained close to those seen in 2019.  This varied by cancer type and there were some 
      notably higher and lower than expected numbers (see relevant updates)."),
    br(),
    
    #################################################################################################################   .
    
    # UPDATES ----
    
    ###################################
    ## AUGUST 2022 update ----
    ###################################.
    
    h3("3 August 2022 - Pathology data updated to 2 February 2022 (extracted 02/06/2022)"),
    p("In 2022 (weeks ending 5th January to 2nd February), there was little difference in the total number of individuals with a pathological
      diagnosis of cancer (Excl.C44) compared with those in 2019 over the same dates (4046 and 3986 in 2022 and 2019 respectively, a difference
      of approximately 1.5%). It should be noted that there are usually fewer cancer diagnoses in late December and early 
      January because of seasonal holidays, so the interpretation of results from the first few weeks of 2022 should be 
      made with caution."),
    p("For this update, we review patterns in the most common types of cancer for early 2022 as well as
      updating the results for the calendar year 2021."),
    
    h4("Early 2022 data"),
    p("Among the most common cancer types, comparing January to week ending 2nd February in 2022 and 2019, respectively:"),
    
    tags$ul(
      tags$li("Lung cancer: 230 versus 274 pathological diagnoses.  
              This indicates a continued deficit (-16%) which was also seen through 2021"),
      
      tags$li("Breast cancer (females only):  804 versus 739 pathological diagnoses. 
              This indicates an increase in diagnoses of 9% after the return to typical numbers of diagnoses in 2021."),
      
      tags$li("Prostate cancer:  476 versus 350 pathological diagnoses.
              This indicates a further increase in diagnoses of  36%, a continuation of the increase seen in 2021."),
      
      tags$li("Colorectal (bowel) cancer: 538 versus 486 pathological diagnoses.  This indicates an
              increase of 11% after the return to typical number of diagnoses in 2021."),
      
      tags$li("Liver and intrahepatic bile ducts: 38 versus 29 pathological diagnoses. 
              This indicates an increase of 31%, a continued increase in diagnoses also seen in 2021."),
      
      tags$li("Oesophagus: 112 versus 104 pathological diagnoses.  This indicates an increase of 8% and a
              continuation of the increase in diagnoses seen in 2021.")),
    
    p("These early results therefore show a mixture of some continued under-diagnoses and some “catching-up” 
      with the under-diagnoses seen earlier in the pandemic."),
    
    h4("Revisions of 2021 data"),
    p("As pathology data are continually updated, a revision of the January to week ending 27th December in 2021 and 2019, 
      in this latest extract is given below:"),
    tags$ul(
      tags$li("Lung cancer: 2,324 versus 2,833 pathological diagnoses; a decrease
              of 509 individuals, or 17.9% lower."),
      
      tags$li("Breast cancer (females only): 5,322 versus 5,315 pathological diagnoses; 
              a decrease of 7 individuals, or less than 1% lower"),
      
      tags$li("Prostate cancer: 3,651 versus 3,143 pathological diagnoses; an increase
              of 508 individuals, or 16.2% higher."),
      
      tags$li("Colorectal (bowel) cancer: 4,071 versus 4,041 pathological diagnoses; an
              increase of 30 individuals, or less than 1% higher."),
      
      tags$li("Liver and intrahepatic bile ducts: 380 versus 330 pathological diagnoses;
              an increase of 50 individuals, or 15.2% higher."),
      
      tags$li("Oesophagus: 1184 versus 1006 pathological diagnoses; an increase
              of 178 individuals, or 17.7% higher.")),
    br(),
    p("A new chart comparing 2019 quarterly totals to the same quarters in 2020 and 2021 shows that after initial falls in diagnoses, 
      there was some recovery or catching-up in the cumulative figures, repeating 2019 figures over two years for comparison with the 2020-2021 period:"),
    tags$ul(
      
      tags$li("All excl. NMSC: 33162 (2021) and 28784 (2020) compared with 33345 in 2019.  That is, down 4744 in total by December 2021.  
              No overall reduction in the number of “missing” patients although it is important to look at each cancer type to understand 
              what has happened."),
      tags$li("Lung: 2324 (2021) and 2263 (2020) compared with 2833 in 2019.  That is, 1079 down in total by December 2021.  
              So the continued under-diagnosis in 2021 added to the total “missing”."),
      
      tags$li("Colorectal: 4071 (2021) and 3216 (2020) compared with 4041 in 2019.  That is, 795 down in total by December 2021."),
      
      tags$li("Breast (females only): 5322 (2021) and 4529 (2020) compared with 5315 in 2019.  That is, down 779 in total by December 2021."),
      
      tags$li("Cervical: 358 (2021) and 309 (2020) compared with 371 in 2019.  That is, down 75 in total by December 2021."),
      
      tags$li("Prostate: 3651 (2021) and 2614 (2020) compared with 3143 in 2019.  That is, down only 21 patients in total by December 2021 – 
              suggesting that those missing in 2020 were largely identified in 2021 and there is little ongoing deficit.")),
    
    p(strong("New information on age and socio-economic deprivation has been added to the dashboard, reviewing the annual
             data to the end of December 2021 compared to 2019:")),
    
    h4("Age"),
    p("For all cancers except non-melanoma skin cancers, the proportionate fall in cumulative pathologically confirmed cancers by December 2021 were
      much smaller than at the end of 2020, and there was less difference between age groups.
      The reduction in breast and colorectal cancer diagnoses, which were both affected by pauses in the screening
      programmes for people aged 50-69, were smaller by December 2021 than December 2020. For example, for breast cancer, 
      pathological cancer diagnoses in those aged 50-69 showed a shortfall of 8.9% 
      in those of screening age (50-69) by the end of Q4 2021, compared to a shortfall of 1.7% for those under 50.
      For colorectal cancers, numbers in 50-69 years olds showed a shortfall of
      10.5% by the end of Q4 2021; however, the total falls in under 50s had shown less improvement, a shortfall of 18.0%."),
    h4("Socio-economic deprivation"),
    p("For deprivation, the least and most deprived quintiles are highlighted in colour.  For all cancers except non-melanomas
      skin cancers, these showed the largest decreases in diagnoses remained among people from the most deprived areas, showing 
      a shortfall of 9.8% by the end of Q4 2021.The least deprived were down 4.5% by the same period."),
    br(),
    
    ###################################.
    ## JUNE 2022 UPDATE ----
    ###################################.
    
    
    h3("15 June 2022 - Pathology data updated to 31 December 2021 (extracted 20/04/2022)"),
    p("In 2021 (weeks ending 05th January to 27th December), there was little difference in the total number
      of individuals with a pathological diagnosis of cancer (Excl. C44) compared with those in 2019 (33086 and 33345 in 2021 and 2019
      respectively, a difference of approximately 0.8%).  However, within cancer sites, some were higher and some lower than
      expected in 2021 compared with 2019."),
    
    p("Among the most common cancer types, comparing January to week ending 27th December in 2021 and 2019, respectively:"),
    tags$ul(
      tags$li("Lung cancer: 2,319 versus 2,833 pathological diagnoses; a decrease
              of 583 individuals, or 20.6% lower."),
      
      tags$li("Breast cancer (females only): 5,314 versus 5,315 pathological diagnoses; 
              a decrease of 1 individual, or less than 1% lower"),
      
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
      tags$li("All excl. NMSC: 33086 (2021) and 28481 (2020) compared with 33345 in 2019.  That is, down 5123 in total by December 2021.  
              No overall reduction in the number of “missing” patients although it is important to look at each cancer type to understand 
              what has happened."),
      tags$li("Lung: 2319 (2021) and 2250 (2020) compared with 2833 in 2019.  That is, 1097 down in total by December 2021.  
              So the continued under-diagnosis in 2021 added to the total “missing”."),
      
      tags$li("Colorectal: 4057 (2021) and 3187 (2020) compared with 4041 in 2019.  That is, 838 down in total by December 2021."),
      
      tags$li("Breast (females only): 5314 (2021) and 4488 (2020) compared with 5295 in 2019.  That is, down 788 in total by December 2021."),
      
      tags$li("Cervical: 307 (2021) and 357 (2020) compared with 371 in 2019.  That is, down 78 in total by December 2021."),
      
      tags$li("Prostate: 2587 (2021) and 3646 (2020) compared with 3143 in 2019.  That is, down only 53 patients in total by December 2021 – 
              suggesting that those missing in 2020 were largely identified in 2021 and there is little ongoing deficit.")),
    
    # tags$li("Lung cancer: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 19.3% lower."),
    
    # tags$li("Breast cancer: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 7.6% lower."),
    # 
    # tags$li("Prostate cancer: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 0.4% lower."),
    # 
    # tags$li("Colorectal(bowel) cancer: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 10% lower."),
    # 
    # tags$li("Liver and intrahepatic bile ducts: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 6.5% higher."),
    # 
    # tags$li("Oesophagus: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 6.3% higher."),
    # 
    # tags$li("Cervical: by December 27th 2021, total numbers of diagnoses since the start of 2020 were 10.2% lower.")),
    
    p(strong("New information on age and socio-economic deprivation has been added to the dashboard, reviewing the annual
             data to the end of December 2021 compared to 2019:")),
    
    h4("Age"),
    p("For all cancers except non-melanoma skin cancers, the proportionate fall in pathologically confirmed cancers by December 2021 were
      much smaller than at the end of 2020, and there was less difference between age groups.
      The reduction in breast and colorectal cancer diagnoses, which were both affected by pauses in the screening
      programmes for people aged 50-69, were smaller by December 2021 than December 2020. For example, for breast cancer, 
      pathological cancer diagnoses in those aged 50-69 showed a shortfall of 9.1% 
      in those of screening age (50-69) by the end of Q4 2021, compared to a shortfall of 1.6% for those under 50.
      For colorectal cancers, numbers in 50-69 years olds showed a shortfall of
      11.2% by the end of Q4 2021; however, the total falls in under 50s had shown less improvement, a shortfall of 17.2%."),
    h4("Socio-economic deprivation"),
    p("For deprivation, the least and most deprived quintiles are highlighted in colour.  For all cancers except non-melanomas
      skin cancers, these showed the largest decreases in diagnoses remained among people from the most deprived areas, showing 
      a shortfall of 12.9% by the end of Q4 2021.The least deprived were down 6% by the same period."),
    br(),
    
    
    
    ###################################.
    ## SEPTEMBER 2021 UPDATE ----
    ###################################.
    
    
    h3("22 September 2021 - Pathology data updated to 14 June 2021 (extracted 19/8/2021)"),
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
    h4("Age"),
    p("For all cancers except non-melanoma skin cancers, the largest proportionate fall in
      pathologically confirmed cancers were among those aged 50-69 years and the smallest falls were in those under 50 years.
      This difference is more clearly seen in breast and colorectal cancers, which were both affected by pauses in the screening
      programmes for people aged 50-70 and 50-74, respectively.  For example, for breast cancer, there was little difference in
      pathological cancer diagnoses in those aged under 50; and a maximum fall of -24% in those of screening age (50-69) by the
      end of Q3, with some recovery (to -19%) by the end of the year.  For colorectal cancers, numbers in 50-69 years olds fell
      -20% by the end of Q3 with little recovery by the end of the year; while the total annual falls in under 50s and 70 and over
      were -17% and -14%, respectively."),
    br(),
    h4("Socio-economic deprivation"),
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
    
    br(),
    
    ###################################.
    
    h3("29 July 2021 - Pathology data updated to 26 February 2021 (extracted 20/5/2021)"),
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
    
    h3("10 March 2021 - Pathology data updated to 29 November 2020 (extracted 22/2/2021)"),
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
    
    
    h3("23 December 2020 - Pathology data updated to 30 August 2020 (extracted 27/11/2020)"),
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
    
    
    h3("18 November 2020 - Pathology data updated to 21 June 2020 (extracted 16/9/2020)"),
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
    
    
    p(tags$a(href="https://www.isdscotland.org/Health-Topics/Cancer/FAQ/#15", "Data Source (external website)")),
    br()
      ) 
  

#END