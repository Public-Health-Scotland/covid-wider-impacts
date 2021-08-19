# Server side for SACT tab - 

###############################################.
## Modals ----
###############################################.
# Pop-up modal explaining source of data

observeEvent(input$btn_sact_modal, 
             showModal(modalDialog(
               title = "Frequently Asked Questions",
               h5(strong("What is SACT?")),
               p("SACT is an acronym for Systemic Anti-Cancer Therapies and is used as a collective 
                 term for drugs that are used in the treatment of cancer that affect the whole body. 
                 The main type of drugs are cytotoxic chemotherapy drugs but there are other treatments 
                 such as targeted agents and immunotherapies."),
               h5(strong("What is the SACT national MVP data platform?")),
               p("The SACT national data platform is a new system currently being developed by PHS 
                 and NSS (National Services Scotland) that allows PHS to access a data from all five 
                 instances of ChemoCare across Scotland effectively giving a unified view of SACT prescribing 
                 data. The data platform is currently available as a âMinimum Viable Productâ (MVP) allowing analysts to 
                 access the raw data for validation and development purposes until the final platform has been 
                 developed."),
               h5(strong("What is included in the data presented?")),
               p("The data include all SACT and non-SACT activity that is prescribed for adult patients on one 
                 of the five instances of ChemoCare. ChemoCare is an electronic prescribing system for SACT drugs 
                 that is used in secondary care. Patients will typically be prescribed SACT treatment through 
                 this system, however other treatments can be prescribed in this way, such as hormones and supportive 
                 treatments including anti sickness medication and supportive medicines."),
               h5(strong("What is not included in the data presented?")),
               p("The data does not include patients treated in a paediatric setting and patients who are younger 
                 than 16 years at the start of their treatment. Treatments that are not prescribed through a ChemoCare 
                 system, such as prescriptions in the community and paper prescriptions, are also not included in 
                 the data."),
               h5(strong("Why do you advise not to compare the data from different regions directly?")),
               p("ChemoCare is used as a prescribing system for drugs and different recording practices are used across 
                 Scotland. This may lead to different results when using a national approach to counting appointments. An 
                 example would be that a treatment consisting of multiple drugs is set up as one treatment on one instance 
                 of ChemoCare while on another the same treatment is set up as two different treatments used in conjunction, 
                 resulting in two appointments on the same day."),
               p("Therefore, we advise to look at general trends and trends over time."),
               h5(strong("What is the definition of an appointment?")),
               p("An appointment is the unique combination of patient CHI (identifier), appointment date and the treatment 
                 name, referred to as regimen, used locally. It is possible that a patient has multiple appointments in a 
                 week. Oral drugs are typically dispensed as a packet for the patient to take away and, in these cases, only one 
                 appointment is counted for the day that the patient was scheduled to have the drug dispensed."),
               h5(strong("How is the route of administration derived?")),
               p("At an appointment a patient will typically receive multiple drugs, for example a SACT drug and supportive 
                 treatments. Each drug can be administered in different ways. We have therefore implemented a hierarchy 
                 based on the invasiveness of the treatment to reflect the most invasive route for the appointment, at 
                 appointment level, or the regimen, at regimen level. The hierarchy is intrathecal, intravenous, subcutaneous, 
                 other and oral as the least invasive. Where patients receive SACT drugs the administration route is derived 
                 from these only but at appointments where only non-SACT drugs are administered the route is derived from these."),
               h5(strong("What are the geographies referring to?")),
               p("All data is presented by region (cancer network or health board) of treatment. This is derived from the ward 
                 that the treatment was scheduled to take place. In some cases, where there is no ward in the data, the location 
                 the prescription originated from is used as treatment location. There are a few wards that are serviced by 
                 prescribing locations in a different region, for example wards in Argyll are serviced by the Beatson. For these 
                 known cases, the prescribing location is used to reflect the origin of the prescription. "),
               
               # p(paste0("Figures presented based on data extracted on ",sact_extract_date)), 
               size = "l",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

observeEvent(input$btn_sact_wk_modal, 
             showModal(modalDialog(
               title = "Frequently Asked Questions",
               h5(strong("What is SACT?")),
               p("SACT is an acronym for Systemic Anti Cancer Therapies and is used as a collective 
                 term for drugs that are used in the treatment of cancer that affect the whole body. 
                 The main type of drugs are cytotoxic chemotherapy drugs but there are other treatments 
                 such as targeted agents and immunotherapies."),
               h5(strong("What is the SACT national MVP data platform?")),
               p("The SACT national MVP data platform is a new system currently being developed by PHS 
                 and NSS (National Services Scotland) that allows PHS to access a data from all five 
                 instances of ChemoCare across Scotland effectively giving a unified view of SACT prescribing 
                 data. The data platform is currently a âMinimum Viable Productâ (MVP) allowing analysts to 
                 access the raw data for validation and development purposes until the final platform has been 
                 developed."),
               h5(strong("What is included in the data presented?")),
               p("The data include all SACT and non-SACT activity that is prescribed for adult patients on one 
                 of the five instances of ChemoCare. ChemoCare is an electronic prescribing system for SACT drugs 
                 that is used in secondary care. Patients will typically be prescribed SACT treatment through 
                 this system, however other treatments can be prescribed in this way, such hormones and supportive 
                 treatments including anti sickness medication and supportive medicines."),
               h5(strong("What is not included in the data presented?")),
               p("The data does not include patients treated in a paediatric setting and patients that are younger 
                 than 16 years at the start of their treatment. Treatments that are not prescribed through a ChemoCare 
                 system, such as prescriptions in the community and paper prescriptions, are also not included in 
                 the data."),
               h5(strong("What is the definition of an appointment?")),
               p("An appointment is the unique combination of patient CHI (identifier), appointment date and the treatment 
                 name, referred to as regimen, used locally. It is possible that a patient has multiple appointments in a 
                 week. Oral drugs are typically dispensed as a packet for the patient to take away; in these cases only one 
                 appointment is counted for the day that the patient was scheduled to have the drug dispensed."),
               h5(strong("Why do you advise not to compare the data from different regions directly?")),
               p("ChemoCare is used as a prescribing system for drugs and different recording practices are used across 
                 Scotland. This may lead to different results when using a national approach to counting appointments. An 
                 example would be that a treatment consisting of multiple drugs is set up as one treatment on one instance 
                 of ChemoCare while on another the same treatment is set up as two different treatments used in conjunction 
                 resulting in the possibility that a patient has two appointments on the same day."),
               p("Therefore, we advise to look at general trends and trends over time."),
               h5(strong("How is the route of administration derived?")),
               p("At an appointment a patient will typically receive multiple drugs, for example a SACT drug and supportive 
                 treatments. Each drug can be administered in different ways. We have therefore implemented a hierarchy 
                 based on the invasiveness of the treatment to reflect the most invasive route for the appointment, at 
                 appointment level, or the regimen, at regimen level. The hierarchy is intrathecal, intravenous, subcutaneous, 
                 other and oral as the least invasive. Where patients receive SACT drugs the administration route is derived 
                 from these only but at appointments where only non-SACT drugs are administered the route is derived from these."),
               h5(strong("What are the geographies referring to?")),
               p("All data is presented by region (cancer network or health board) of treatment. This is derived from the ward 
                 that the treatment was scheduled to take place. In some cases, where there is no ward in the data, the location 
                 the prescription originated from is used as treatment location. There are a few wards that are serviced by 
                 prescribing locations in a different region, for example wards in Argyll are serviced by the Beatson. For these 
                 known cases, the prescribing location is used to reflect the origin of the prescription. "),
               
               # p(paste0("Figures presented based on data extracted on ",sact_extract_date)), 
               size = "l",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))


###############################################.
## Reactive datasets ----
###############################################.

############ 1. MONTHLY ########################

sact_data_main <- reactive({
  
  sact_data_inc %>%
    filter(area == input$geoname_sact, site == input$sact_type, 
           treatment == input$sact_treatment)
  
})


sact_data_main_area <- reactive({
  
  sact_data_inc %>%
    filter(as.character(region) != as.character(area)) %>% 
    filter(region == input$geoname_sact, site == input$sact_type, 
           treatment == input$sact_treatment)
  
})

sact_data_main_treatment <- reactive({
  
  sact_data_inc %>%
    filter(area == input$geoname_sact,
           site == input$sact_type)
  
})

############ 2. WEEKLY ########################

# INCIDENCE #
sact_data_wk_main <- reactive({
  
  sact_data_wk_inc %>%
    filter(area == input$geoname_wk_sact, site == input$sact_wk_type, 
           treatment == input$sact_wk_treatment, appt_reg == input$sact_wk_appt_reg)
  
})


sact_data_wk_main_area <- reactive({
  
  sact_data_wk_inc %>%
    filter(as.character(region) != as.character(area)) %>% 
    filter(region == input$geoname_wk_sact, site == input$sact_wk_type, 
           treatment == input$sact_wk_treatment, appt_reg == input$sact_wk_appt_reg)
  
})

sact_data_wk_main_treatment <- reactive({
  
  sact_data_wk_inc %>%
    filter(area == input$geoname_wk_sact,
           site == input$sact_wk_type,
           appt_reg == input$sact_wk_appt_reg)
  
})

# WEEKLY DIFFERENCE #

sact_data_wk_diff <- reactive({
  
  sact_data_wk_difference %>%
    filter(area == input$geoname_wk_sact, site == input$sact_wk_type, 
           treatment == input$sact_wk_treatment, appt_reg == input$sact_wk_appt_reg)
  
})


sact_data_wk_diff_area <- reactive({
  
  sact_data_wk_difference %>%
    # filter(as.character(region) != as.character(area)) %>% 
    filter(region == input$geoname_wk_sact, site == input$sact_wk_type, 
           treatment == input$sact_wk_treatment, appt_reg == input$sact_wk_appt_reg)
  
})

sact_data_wk_diff_treatment <- reactive({
  
  sact_data_wk_difference %>%
    filter(area == input$geoname_wk_sact,
           site == input$sact_wk_type,
           appt_reg == input$sact_wk_appt_reg)
  
})



###############################################.
## Reactive layout ---- MONTHLY
###############################################.

# sact reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_sact <- renderUI({
  if (input$geotype_sact == "Cancer Network") {
    selectizeInput("geoname_sact", label = NULL, choices = c("NCA", "SCAN", "WOSCAN"), selected = "NCA")
  } else if (input$geotype_sact == "Scotland") {
    selectizeInput("geoname_sact", label = NULL, choices = c("Scotland"), selected = "Scotland")
  } else if (input$geotype_sact == "Health Board") {
    selectizeInput("geoname_sact", label = NULL, choices = c("NHS Ayrshire & Arran",
                                                             "NHS Borders", "NHS Dumfries & Galloway",
                                                             "NHS Fife","NHS Forth Valley",
                                                             "NHS Grampian", "NHS Greater Glasgow & Clyde",
                                                             "NHS Highland", "NHS Lanarkshire",
                                                             "NHS Lothian", "NHS Orkney",
                                                             "NHS Shetland", "NHS Tayside",
                                                             "NHS Western Isles"), selected = "NHS Ayrshire & Arran")
  }
})


output$treatment_ui_sact <- renderUI({
  if (input$sact_plot_filter %in% c("Geographic area", "Standard graph")) {
    div(radioButtons("sact_treatment", "Select administration route",
                     list("All","Intravenous","Oral","Subcutaneous","Intrathecal", "Other"), inline = TRUE,
                     selected = "All"))
  } else  {
    div(radioButtons("sact_treatment", "Select administration route",
                     list("All","Intravenous","Oral","Subcutaneous","Intrathecal", "Other"), inline = TRUE,
                     selected = "All", disabled = TRUE))  
  } 
})

# The charts and text shown on the app will depend on what the user wants to see

output$sact_explorer <- renderUI({
  
  # text for titles of cut charts
  sact_site <- case_when(input$sact_type == "All" ~ "All",
                         input$sact_type == "Bone Sarcoma" ~ "Bone Sarcoma",
                         input$sact_type == "Breast" ~ "Breast",
                         input$sact_type == "Central Nervous System" ~ "Central Nervous System",
                         input$sact_type == "Cancer of Unknown Origin" ~ "Cancer of Unknown Origin",                         input$sact_type == "Germ Cell" ~ "Germ Cell",
                         input$sact_type == "Gynaecology" ~ "Gynaecology",
                         input$sact_type == "Haematology" ~ "Haematology",
                         input$sact_type == "Head & Neck" ~ "Head & Neck",
                         input$sact_type == "Lower GI" ~ "Lower GI",
                         input$sact_type == "Lung & Chest" ~ "Lung & Chest",
                         input$sact_type == "Neuroendocrine" ~ "Neuroendocrine",
                         input$sact_type == "Other" ~ "Other",
                         input$sact_type == "Skin" ~ "Skin",
                         input$sact_type == "Soft Tissue Sarcoma" ~ "Soft Tissue Sarcoma",
                         input$sact_type == "Upper GI" ~ "Upper GI",
                         input$sact_type == "Urological" ~ "Urological",
                         input$sact_type == "Unknown" ~ "Unknown"
  )
  
  tagList(
    
    if(input$sact_plot_filter == "Geographic area" & input$geotype_sact %in% c("Scotland", "Cancer Network")){
      plot_box(paste0("Monthly Number of Patients: ",  sact_site, " Cancers - ", input$geoname_sact), 
               "sact_incidence_area")
    } else if (input$sact_plot_filter == "Geographic area" & input$geotype_sact == "Health Board"){
      plot_box(paste0("Monthly Number of Patients: ",  sact_site, " Cancers - ", input$geoname_sact), 
               "sact_incidence")
    } else if (input$sact_plot_filter == "Treatment administration"){
      plot_box(paste0("Monthly Number of Patients: ",  sact_site, " Cancers - ", input$geoname_sact), 
               "sact_incidence_treatment")
    } else if (input$sact_plot_filter == "Standard graph"){
      plot_box(paste0("Monthly Number of Patients: ",  sact_site, " Cancers - ", input$geoname_sact), 
               "sact_incidence")
    },
    p(em("The monthly SACT activity shows the number of unique patients per full calendar month since January 2020. 
         Patients receiving more than one treatment are counted once overall but may appear in counts for multiple administration 
         routes. Monthly patient numbers are a good indication for long-term trends in SACT activity.", style = "font-family: 'calibri'; font-si15pt"))
    ) # tag list
  
})

###############################################.
## Reactive layout ---- WEEKLY
###############################################.

# sact reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_wk_sact <- renderUI({
  # areas_summary_sact <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_sact])
  if (input$geotype_wk_sact == "Cancer Network") {
    selectizeInput("geoname_wk_sact", label = NULL, choices = c("NCA", "SCAN", "WOSCAN"), selected = "NCA")
  } else if (input$geotype_wk_sact == "Scotland") {
    selectizeInput("geoname_wk_sact", label = NULL, choices = c("Scotland"), selected = "Scotland")
  } else if (input$geotype_wk_sact == "Health Board") {
    selectizeInput("geoname_wk_sact", label = NULL, choices = c("NHS Ayrshire & Arran",
                                                                "NHS Borders", "NHS Dumfries & Galloway",
                                                                "NHS Fife","NHS Forth Valley",
                                                                "NHS Grampian", "NHS Greater Glasgow & Clyde",
                                                                "NHS Highland", "NHS Lanarkshire",
                                                                "NHS Lothian", "NHS Orkney",
                                                                "NHS Shetland", "NHS Tayside",
                                                                "NHS Western Isles"), selected = "NHS Ayrshire & Arran")
  }
})

output$treatment_ui_wk_sact <- renderUI({
  # areas_summary_sact <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_sact])
  if (input$sact_plot_wk_filter %in% c("Geographic area", "Standard graph")) {
    div(radioButtons("sact_wk_treatment", "Select administration route",
                     list("All","Intravenous","Oral","Subcutaneous","Intrathecal", "Other"), inline = TRUE,
                     selected = "All"))
  } else  {
    div(radioButtons("sact_wk_treatment", "Select administration route",
                     list("All","Intravenous","Oral","Subcutaneous","Intrathecal", "Other"), inline = TRUE,
                     selected = "All", disabled = TRUE))  
  } 
})

output$sact_wk_explorer <- renderUI({
  
  # text for titles of cut charts
  sact_wk_site <- case_when(input$sact_wk_type == "All" ~ "All",
                            input$sact_wk_type == "Bone Sarcoma" ~ "Bone Sarcoma",
                            input$sact_wk_type == "Breast" ~ "Breast",
                            input$sact_wk_type == "Central Nervous System" ~ "Central Nervous System",
                            input$sact_wk_type == "Cancer of Unknown Origin" ~ "Cancer of Unknown Origin",
                            input$sact_wk_type == "Germ Cell" ~ "Germ Cell",
                            input$sact_wk_type == "Gynaecology" ~ "Gynaecology",
                            input$sact_wk_type == "Haematology" ~ "Haematology",
                            input$sact_wk_type == "Head & Neck" ~ "Head & Neck",
                            input$sact_wk_type == "Lower GI" ~ "Lower GI",
                            input$sact_wk_type == "Lung & Chest" ~ "Lung & Chest",
                            input$sact_wk_type == "Neuroendocrine" ~ "Neuroendocrine",
                            input$sact_wk_type == "Other" ~ "Other",
                            input$sact_wk_type == "Skin" ~ "Skin",
                            input$sact_wk_type == "Soft Tissue Sarcoma" ~ "Soft Tissue Sarcoma",
                            input$sact_wk_type == "Upper GI" ~ "Upper GI",
                            input$sact_wk_type == "Urological" ~ "Urological",
                            input$sact_wk_type == "Unknown" ~ "Unknown"
                            
  )
  
  tagList(
    
    if(input$sact_plot_wk_filter == "Geographic area" & input$geotype_wk_sact %in% c("Scotland", "Cancer Network")){
      plot_box(paste0("Weekly Number of Patients: ",  sact_wk_site, " Cancers - ", input$geoname_wk_sact), 
               "sact_wk_incidence_area")
    } else if (input$sact_plot_wk_filter == "Geographic area" & input$geotype_wk_sact == "Health Board"){
      plot_box(paste0("Weekly Number of Patients: ",  sact_wk_site, " Cancers - ", input$geoname_wk_sact), 
               "sact_wk_incidence")
    } else if (input$sact_plot_wk_filter == "Treatment administration"){
      plot_box(paste0("Weekly Number of Patients: ",  sact_wk_site, " Cancers - ", input$geoname_wk_sact), 
               "sact_wk_incidence_treatment")
    } else if (input$sact_plot_wk_filter == "Standard graph"){
      plot_box(paste0("Weekly Number of Patients: ",  sact_wk_site, " Cancers - ", input$geoname_wk_sact), 
               "sact_wk_incidence")
    },
    
    p(em("An appointment is defined as the unique combination of patient CHI, the local treatment name and the appointment date. 
         It is therefore possible that a single patient has more than one appointment per week or on a single day. The number of 
         weekly appointments gives an indication of activity and the workload within the areas. Data in this report is available 
         from the first calendar week 2020 (30-12-2019).", style = "font-family: 'calibri'; font-si14pt")) , 
    br(),
    
    if(input$sact_plot_wk_filter == "Geographic area" & input$geotype_wk_sact %in% c("Scotland", "Cancer Network")){
      plot_box(paste0("Weekly Percentage Difference from 6 Week Average Pre-COVID: ",  sact_wk_site, " Cancers - ", input$geoname_wk_sact),
               "sact_wk_difference_area")
    } else if (input$sact_plot_wk_filter == "Geographic area" & input$geotype_wk_sact == "Health Board"){
      plot_box(paste0("Weekly Percentage Difference from 6 Week Average Pre-COVID: ",  sact_wk_site, " Cancers - ", input$geoname_wk_sact),
               "sact_wk_difference")
    } else if (input$sact_plot_wk_filter == "Treatment administration"){
      plot_box(paste0("Weekly Percentage Difference from 6 Week Average Pre-COVID: ",  sact_wk_site, " Cancers - ", input$geoname_wk_sact),
               "sact_wk_difference_treatment")
    } else if (input$sact_plot_wk_filter == "Standard graph"){
      plot_box(paste0("Weekly Percentage Difference from 6 Week Average Pre-COVID: ",  sact_wk_site, " Cancers - ", input$geoname_wk_sact),
               "sact_wk_difference")
    },
    
    p(em("The percentage change of weekly appointments shows weekly activity against an average week in the pre-COVID period. The 
         average week (also called reference week) is defined as the average number of appointments occurring during the weeks 
         beginning 22/01/2020 up to the week beginning 26/02/2020. The percentage change is good measure to demonstrate the effect 
         of COVID-19 on SACT activity and how cancer services are recovering from the impact.", style = "font-family: 'calibri'; font-si15pt"))
    
    )
  
})

###############################################.
## Charts ----
###############################################.

# MONTHLY INCIDENCE

output$sact_incidence <- renderPlotly({plot_sact_incidence_chart(sact_dataset = sact_data_main())})

output$sact_incidence_area <- renderPlotly({plot_sact_incidence_chart_area(sact_dataset = sact_data_main_area())})

output$sact_incidence_treatment <- renderPlotly({plot_sact_incidence_chart_treatment(sact_dataset = sact_data_main_treatment())})

# WEEKLY INCIDENCE

output$sact_wk_incidence <- renderPlotly({plot_sact_wk_incidence_chart(sact_wk_dataset = sact_data_wk_main())})

output$sact_wk_incidence_area <- renderPlotly({plot_sact_wk_incidence_chart_area(sact_wk_dataset = sact_data_wk_main_area())})

output$sact_wk_incidence_treatment <- renderPlotly({plot_sact_wk_incidence_chart_treatment(sact_wk_dataset = sact_data_wk_main_treatment())})

# WEEKLY DIFFERENCE

output$sact_wk_difference <- renderPlotly({plot_sact_wk_difference_chart(sact_wk_dataset = sact_data_wk_diff())})

output$sact_wk_difference_area <- renderPlotly({plot_sact_wk_difference_chart_area(sact_wk_dataset = sact_data_wk_diff_area())})

output$sact_wk_difference_treatment <- renderPlotly({plot_sact_wk_difference_chart_treatment(sact_wk_dataset = sact_data_wk_diff_treatment())})




###############################################.
## Data downloads ----
###############################################.

output$download_sact_weekly_data <- downloadHandler(
  filename ="sact_weekly_extract.csv",
  content = function(file) {
    write_csv(sact_data_wk_main() %>% 
                select(week_beginning, region, area, site, appt_reg, treatment, count,
                       week_on_refweek_perc) %>% 
                rename("Week beginning" = week_beginning, "Region" = region, 
                       "Area name" = area, "Cancer type" = site,
                       "Administration route derivation" = appt_reg,
                       "Administration route" = treatment,
                       "Number of appointments" = count,
                       "Percentage change vs. average reference Week" = week_on_refweek_perc),
              file) } 
) 


output$download_sact_monthly_data <- downloadHandler(
  filename ="sact_monthly_extract.csv",
  content = function(file) {
    write_csv(sact_data_main() %>% 
                select(month, region, area, site, treatment, count) %>% 
                rename("Month" = month, "Region" = region, "Area name" = area, 
                       "Cancer type" = site, "Administration route" = treatment,
                       "Number of patients" = count),
              file) } 
) 

###############################################.
## Commentary ----
##################
