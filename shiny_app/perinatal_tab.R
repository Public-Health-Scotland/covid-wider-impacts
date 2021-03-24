##Server script for perinatal tab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_perinatal_modal,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p("Data source: NRS vital event registrations."),
               p("The stillbirth and infant mortality rates shown in this tool have been generated using data from the statutory registration of deaths and births provided by",
                 tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/deaths-background-information/stillbirths-and-infant-deaths",
                 "National Records for Scotland (NRS).", target="_blank"), 
                 tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)", target="_blank"),
                 " receives weekly data from NRS on deaths and stillbirths. 
                 PHS also normally receives weekly data on birth registrations from NRS, however birth registrations have been deferred during the pandemic period. So, to provide denominator information, 
                 birth numbers for January 2020 onwards have been taken from NHS notifications of births recorded in the national",
                 tags$a(href="https://www.isdscotland.org/Health-Topics/Child-Health/Child-Health-Programme/", "child health information systems.", target="_blank")),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SPC charts rules
observeEvent(input$btn_perinatal_rules,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "How do we identify patterns in the data?",
               p("Control charts follow a series of rules that help identify important changes in the data. 
                 These are the ones we used in this chart:"),
               tags$ul(tags$li("Outliers: Data points outside the limits marked by the control limits."),
                       tags$li("Shifts: Eight or more consecutive data points above or below the centreline."),
                       tags$li("Trends: Six or more consecutive data points which are increasing or decreasing."),
                       tags$li("Outer One – Third: Two out of three consecutive data points which sit between the control and warning limits."),
                       tags$li("Inner One -Third: 15 or more consecutive data points that lie close to the centreline.")),
               p("Different control charts are used depending on the type of data involved.
                 For the stillbirth, neonatal, and extended perinatal death rates P 
                 charts are presented. For the post-neonatal and infant death rates a U chart is presented. The type of chart used depends on the type of 
                 data involved (which statistical distribution we think it follows)."),
               p("Further information on these methods of presenting data can be found at the ",                      
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts.')),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive dataset ----
###############################################.
# Reactive dataset used 
peri_filt <- reactive({
  perinatal %>% filter(type == input$measure_select_perinatal & area_name == "Scotland") %>% 
    filter(month_of_year == max(month_of_year)) 
})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$perinatal_explorer <- renderUI({

  # text for titles of cut charts
  perinatal_title <- case_when(input$measure_select_perinatal == "stillbirths" ~ 
                                 paste0("Monthly rate of stillbirths per 1,000 total (live + still) births in Scotland", input$geoname_perinatal),
                               input$measure_select_perinatal == "pnnd" ~
                                 paste0("Monthly rate of post-neonatal deaths per 1,000 live births in Scotland", input$geoname_perinatal),
                               input$measure_select_perinatal == "nnd" ~ 
                                 paste0("Monthly rate of neonatal deaths per 1,000 live births in Scotland", input$geoname_perinatal),
                               input$measure_select_perinatal == "extperi" ~ 
                                 paste0("Monthly rate of extended perinatal deaths per 1,000 total (live + still) births in Scotland", input$geoname_perinatal),
                               input$measure_select_perinatal == "infantdeaths" ~ 
                                 paste0("Monthly rate of infant deaths per 1,000 live births in Scotland", input$geoname_perinatal))
  
  # Link used in intro text
  link_perinatal <- "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/deaths-background-information/stillbirths-and-infant-deaths"
  peri_common_intro <- "It is important to monitor the levels of stillbirth and infant mortality 
                    during the COVID-19 pandemic, as they may be influenced by maternal health 
  and well-being, by how maternity services are provided, and how people seek and interact with care.
  NHS Scotland and the Scottish Government"
  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies",
  "have produced guidelines for attending antenatal and postnatal care appointments during the pandemic.", target="_blank")
  
  # Text to be updated every month with updated dates
  last_month_peri <- "February 2021"
  cutdate_peri <- "21 March 2021"
  extractdate_peri <- "24 March 2021"
  nextup_peri <- "May 2021"
  nextdata_peri <- "March 2021"
  
  # Number of deaths and of births used in the text
  no_stillperi <- peri_filt() %>% pull(number_of_deaths_in_month)
  no_births <- peri_filt() %>% pull(sample_size) %>% format(big.mark=",")
  
  # Intro paragraph within perinatal tab
  if (input$measure_select_perinatal == "stillbirths") {
    intro_text <- p("Stillbirths refer to", tags$a(href=link_perinatal, 
                    "a child born after the 24th week of pregnancy which did not breathe or show any signs of life.", target="_blank"), 
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                    Whilst each stillbirth is clearly a tragedy for the family involved, stillbirths are uncommon events in Scotland: ", no_stillperi ," stillbirths (and ", no_births ," total 
                    [live and still] births) occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "pnnd") {
    intro_text <- p("Post-neonatal deaths refer to", tags$a(href=link_perinatal, 
                    "deaths occuring after the first 4 weeks but within the first year", target="_blank"), "of life.",
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                    Whilst each post-neonatal death is clearly a tragedy for the family involved, post-neonatal deaths are uncommon events in Scotland: ", no_stillperi ," post-neonatal deaths 
                    (and ", no_births ," live births) occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "nnd") {
    intro_text <- p("Neonatal deaths refer to", tags$a(href=link_perinatal, 
                  "deaths in the first four weeks", target="_blank"), "of life.",
                  peri_common_intro,
                  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                         "have produced guidelines", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                  Whilst each neonatal death is clearly a tragedy for the family involved, neonatal deaths are uncommon events in Scotland: ", no_stillperi ," neonatal deaths (and ", no_births ," live births) 
                  occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "extperi") {
    intro_text <- p("Extended perinatal deaths refer to", tags$a(href=link_perinatal, 
"the sum of stillbirths and neonatal mortality", target="_blank"), "(deaths within the first 4 weeks of life).",
peri_common_intro,
tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
       "have produced guidelines", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
Whilst each extended perinatal death is clearly a tragedy for the family involved, extended perinatal deaths are uncommon events in Scotland: ", no_stillperi ," extended perinatal deaths (and ", no_births ," total 
[live and still] births) occurring in Scotland ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "infantdeaths") {
    intro_text <- p("Infant deaths refer to", tags$a(href=link_perinatal, 
                    "all deaths in the first year", target="_blank"), "of life; 
                    this includes neonatal and post-neonatal deaths, but not stillbirths. ",
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                    Whilst each infant death is clearly a tragedy for the family involved, infant deaths are uncommon events in Scotland: ", no_stillperi ," infant deaths (and ", no_births ," live births) 
                    occurring in Scotland ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  }

  nrs_commentary <- p("It is important to note that chart data is based on month of occurence rather than month of registration used in NRS publications, and so
                      figures may differ slightly.")
  
  control_chart_commentary <- p("As stillbirths and infant deaths are relatively rare events in Scotland mortality rates tend to fluctuate over time just by chance.
                      We have therefore used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf', 
                      "‘control charts’", target="_blank"), "to present the rates above.", br(),
                      "Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation.  
                      Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’", br(),
                      "The dots joined by a solid line in the chart above show the monthly mortality rate for the measure selected from January 2017 onwards.", br(),  
                      "The other lines - centreline, and control and warning limits - are there to help show how unexpected any observed changes are. 
                      The centreline is an average (mean) over the time period. Control and warning limits take into consideration the random variation 
                      that would be expected by chance, and help us decide when values are unexpectedly low or high and require further investigation.")
  
  may_data_commentary <- p("By law, all stillbirths must be registered within 21 days of the baby being delivered, and all infant deaths must be registered within 8 days of the baby dying.  
                           The data extract used to produce the mortality numbers and rates for up to and including ", last_month_peri," presented here was taken on ", extractdate_peri, ", and included stillbirths and infant deaths registered up to and including ", cutdate_peri, ".", br(), 
                           "It is therefore possible that some stillbirths occurring in the last week of ", last_month_peri, " may not have been registered by the time the data extract was taken. The stillbirth and extended perinatal mortality rates for ", last_month_peri, " should 
                           therefore be taken as provisional, and they may increase when the data is refreshed (and new rates for ", nextdata_peri, " are added) in ", nextup_peri, ".", br(),  
                           "We would expect any increases to be small, as in previous years 95% of stillbirths have been registered within 14 days of the baby being delivered, despite the legal limit allowing up to 21 days.", br(),  
                           "This issue affects infant deaths (neonatal, post-neonatal, and infant death categories) less as the legal time limit for registration is 8 days.")        
  # Specify items to display in perinatal ui 
  tagList(
    fluidRow(column(12, 
                    intro_text,
                    h4(paste0(perinatal_title)))),
    actionButton("btn_perinatal_rules", "How do we identify patterns in the data?", 
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("perinatal_chart"))),
    fluidRow(column(12, renderUI(nrs_commentary))),
    fluidRow(column(12, renderUI(control_chart_commentary))),
    fluidRow(column(12, renderUI(may_data_commentary))))
  
}) #close perinatal_explorer function
  
  ###############################################.
  ## Charts ----
  ###############################################.
  
  #run chart function to generate spc charts
  output$perinatal_chart <- renderPlotly({
    trend_data <- perinatal %>% filter(type == input$measure_select_perinatal)
    
    yaxis_plots[["title"]] <- case_when(input$measure_select_perinatal %in% c("pnnd", "nnd", "infantdeaths") ~ "Rate per 1,000 live births", 
                                        input$measure_select_perinatal %in% c("extperi", "stillbirths") ~ "Rate per 1,000 (live + still) births")
    xaxis_plots[["title"]] <- "Month"
      
    # Tooltip
    measure_selected <- case_when(input$measure_select_perinatal == "stillbirths" ~ "Still births",
                                  input$measure_select_perinatal ==   "pnnd" ~ "Post-neonatal deaths",
                                  input$measure_select_perinatal ==    "nnd" ~ "Neonatal deaths",
                                  input$measure_select_perinatal ==   "extperi" ~ "Extended perinatal deaths",
                                  input$measure_select_perinatal ==    "infantdeaths" ~ "Infant deaths")
    
    tooltip_trend <- c(paste0(measure_selected, "<br>", 
                              format(trend_data$month_of_year, "%B %y"), "<br>", 
                              "Rate: ", round(trend_data$rate, 1)))
    
    plot_ly(data = trend_data, x = ~month_of_year) %>%
      add_lines(y = ~rate, line = list(color = "black"),
                text=tooltip_trend, hoverinfo="text",
                marker = list(color = "black"),
                name = "Rate") %>%
      add_lines(y = ~centreline, line = list(color = "blue", dash = "longdash"),
                 hoverinfo= "none", name = "Centreline") %>%
      add_lines(y = ~upper_cl_3_std_dev, line = list(color = "red", dash = "dash"),
                hoverinfo= "none", name = "Control limits") %>%
      add_lines(y = ~lower_cl_3_std_dev, line = list(color = "red", dash = "dash"),
                hoverinfo= "none", showlegend = FALSE) %>%
      add_lines(y = ~upper_wl_2_std_dev, line = list(color = "#33ccff", dash = "dot"),
                hoverinfo= "none", name = "Warning limits") %>%
      add_lines(y = ~lower_wl_2_std_dev, line = list(color = "#33ccff", dash = "dot"),
                hoverinfo= "none", showlegend = FALSE) %>%
      # adding outliers
      add_markers(data = trend_data %>% filter(outlier == T), y = ~ rate,
                  marker = list(color = "red", size = 10, symbol = "diamond"), name = "Outliers") %>% 
      # adding shifts
      add_markers(data = trend_data %>% filter(shift == T), y = ~ rate,
                  marker = list(color = "blue", size = 10, symbol = "circle"), name = "Shifts") %>% 
      # adding shifts
      add_markers(data = trend_data %>% filter(trend == T), y = ~ rate,
                  marker = list(color = "green", size = 10, symbol = "square"), name = "Trends") %>% 
      # adding inner third
      add_markers(data = trend_data %>% filter(inner == T), y = ~ rate,
                  marker = list(color = "gray", size = 10, symbol = "x"), name = "Inner one-third") %>% 
      # adding outer third
      add_markers(data = trend_data %>% filter(outer == T), y = ~ rate,
                  marker = list(color = "orange", size = 10, symbol = "star"), name = "Outer one-third") %>% 
      layout( #to avoid labels getting cut out
        yaxis = yaxis_plots, xaxis = xaxis_plots) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
    
  })


###############################################.
## Data downloads ----
###############################################.


perinatal_down_data <- reactive({
  perinatal %>% filter(type == input$measure_select_perinatal) 
  
  if (input$measure_select_perinatal %in% c("pnnd", "nnd", "infantdeaths")) { 
    perinatal <- perinatal %>% filter(type == input$measure_select_perinatal) %>%
      rename(live_births=sample_size) %>% 
      select(month_of_year:centreline, upper_cl_3_std_dev:area_name) %>%
      mutate(month_of_year = format(month_of_year, "%b %y")) } 
 
   else if (input$measure_select_perinatal %in% c("extperi", "stillbirths")) {
    perinatal <- perinatal %>% filter(type == input$measure_select_perinatal) %>%
      rename(total_births=sample_size) %>% 
      select(month_of_year:centreline, upper_cl_3_std_dev:area_name) %>%
      mutate(month_of_year = format(month_of_year, "%b %y")) }
})

output$download_perinatal_data <- downloadHandler(
  filename ="stillbirth_infantdeaths_extract.csv",
  content = function(file) {
    write_csv(perinatal_down_data(),
              file) } 
)
  

###############################################.
## Commentary ----
###############################################.
output$perinatal_commentary <- renderUI({
  tagList(
    bsButton("jump_to_perinatal_mortality",label = "Go to data"), #this button can only be used once
    h2("Stillbirths and infant deaths - 3rd March 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in January 2021. The rate of stillbirths, and all reported infant death measures, remained within the warning threshold limits this month. The stillbirth rate in January 2021 was 4.9 per 1,000 total births, and infant mortality rate was 3.6 per 1,000 live births. "),
    p("Last month we reported an issue involving a small number of infant deaths which were not included in the data files sent from NRS to PHS (affecting less than 3% of infant deaths). We now believe that this discrepancy has been resolved and any data affected have been retrospectively updated on the dashboard. "),
    h2("Stillbirths and infant deaths - 3rd February 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in December 2020. In the intervening months since previous reporting on deaths up to October 2020, the rate of stillbirths and infant deaths remained within the warning threshold limits. The stillbirth rate in December 2020 was 3.5 per 1,000 total births, the lowest rate since August 2020 (2.4 per 1,000 total births)."),
    p("Presenting rates for post-neonatal deaths (PNND) has been changed from a P chart to a U chart. Both types of chart are a means of identifying any important changes in the data (see the ‘How do we identify patterns in the data?’ box for more information). Changing to a U chart brings the reporting of the PNNDs into line with that for infant deaths. Neither the rates nor the control and warning limits are materially affected by this change. "),
    p("The data for these indicators are sourced from NRS, however, a recent issue has come to light whereby a small number of infant deaths are not included in the data files sent from NRS to PHS.  The numbers involved are thought to be very small (less than 3% of infant deaths) and affect data since July 2020.  Any impact on the overall rates included in the dashboard are likely to be minimal. We are working with NRS to resolve this discrepancy as soon as possible. Once resolved data relating to these deaths will be included retrospectively on the dashboard."),
    h2("Stillbirths and infant deaths - 2nd December 2020"),
    p("In this release of information on stillbirths and infant deaths (2 Dec 2020), data have been updated to include events that occurred in October 2020.
Last month it was noted that the rate of post-neonatal deaths in September breached the warning limit (though not the control limit). Continued monitoring shows that this rate has returned to a lower level in October, at 1.4 per 1,000 live births.
Whilst each of these events is a tragedy for those involved, in October the numbers remained small, and all stillbirth and infant death measures were within the warning limits."),
    h2("Stillbirths and infant deaths - 4th November 2020"),
          p("In this release of information on stillbirths and infant deaths (4 Nov 2020) data have been updated to include events that occurred in September 2020. 
In September the rate of stillbirths, neonatal deaths and extended perinatal deaths remained within control limits. Post-neonatal deaths are those which occur after 4 weeks of age, but within the first year of life. In September the rate of post-neonatal deaths was 2.2 per 1,000 live births. This is above the warning limit of 2.1 per 1,000, but below the control limit of 2.6 per 1,000. These thresholds are shown on the control charts, and are used to help differentiate between expected random variation and substantial changes which warrant further investigation. The overall infant mortality rate, which includes all deaths of children aged under 1 year (both below and above 4 weeks of age), remained within the warning limit. This pattern suggests that the higher rate of post-neonatal deaths in September reflects random variation in what is a tragic, but fortunately rare event. 
Monthly monitoring of these data will continue. 
"),
    h2("Stillbirths and infant deaths - 7th October 2020"),
          p("In this release of information on stillbirths and infant deaths (7 Oct 2020), data have been updated to include events that occurred in August 2020.", br(),
            "Last month it was noted that the rate of stillbirths in July breached the warning limit (though not the control limit). Continued monitoring shows that this rate has returned to a lower level in August, at 2.4 per 1,000 total births.", br(),
            "Neonatal, post-neonatal and infant deaths have remained within the expected range, and were relatively low in August 2020."),
    h2("Stillbirths and infant deaths - 2nd September 2020"),
          p("In this release of information on stillbirths and infant deaths (2 Sept 2020) data have been updated to include events that occurred in July 2020.", br(),
            "In July the rate of stillbirths was 6.0 per 1,000 total births, which was higher than the warning limit of 5.8, but below the control limit of 6.7. 
            Whilst each event is important and a tragedy for those involved, the numbers of stillbirths are small overall, and therefore rates fluctuate from month to month just by chance. 
            Control charts are used to help differentiate between expected random variation and substantial changes which warrant further investigation. The stillbirth rate in July was 
            within the control limit, indicating that this observation is within the range of expected random variation.", br(),
            "As there is a relationship between the rate of stillbirth and neonatal mortality, the extended perinatal mortality rate is used to present a combined figure for these two measures. 
            In July 2020, this was 7.4 per 1,000 total births, which is below the warning limit. Monthly monitoring of these data will continue.", br(),
            "Post-neonatal and infant deaths have remained within the expected range, and were relatively low in July 2020."),
    h2("Stillbirths and infant deaths - 5th August 2020"),
          p("In this second release of information on stillbirths and infant deaths (5 Aug 2020) data have been updated to include June 2020. Whilst each of these events is a tragedy for those involved, in June the numbers remained small, 
            and all stillbirth and infant death measures were within the warning limits.", br(),
            "Rates of stillbirths and extended perinatal deaths approached, but did not breach, the upper warning limit in May 2020. However, rates for both fell below the average in June 2020.", br(),
            "Further background information on the data sources used to monitor stillbirths and infant death rates, and how to interpret the control charts, is provided in the commentary for 1 July 2020 below."),
    h2("Stillbirths and infant deaths - 1st July 2020"),      
          p(h4("Background")),
          p("It is important to monitor the levels of stillbirth and infant mortality during the COVID-19 pandemic, 
            as they may be influenced by maternal health and well-being, by how maternity and child health services are provided, and 
            by how people seek and interact with care.", br(),
            "NHS Scotland and Scottish Government",
            tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies",
            "have produced guidelines", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.", br(),
            "The tool shows monthly data for stillbirths and infant deaths (those occurring under the age of one year). These are based on data 
            from National Records for Scotland (NRS), and are presented as rates per 1,000 live births for neonatal, post-neonatal and infant deaths and per 1,000 total (live and still) births for stillbirths and extended perinatal deaths."),
          p(h4("Control Charts")),
          p("Control charts have been used to support interpretation of these data. As numbers of deaths are relatively low, mortality rates tend to fluctuate from month to month just by chance: 
            control charts help differentiate between expected random variation and changes which warrant further investigation.", br(), 
            "In this first release of information on stillbirths and infant deaths (1 July 2020), data are shown for January 2017 to May 2020, with the most recent three months (March-May 2020) 
            being those when health and health services may have been affected by COVID-19.", br(), 
            "In this period the only observations which have reached a ‘warning limit’ as indicated by the relevant control chart were neonatal deaths in March 2020, 
            where the rate was just above the upper warning limit (3.7/1,000 compared to the UWL of 3.6/1,000), but did not breach the upper control limit 
            (the trigger for further investigation). In April and May there were fewer neonatal deaths, and the rate fell to below the upper warning limit. Rates of stillbirths and extended perinatal deaths are being closely 
            monitored, as these approached, but did not breach, the upper warning limit in May 2020."),
          p(h4("Data Sources")),
          p("NRS", tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2019", 
            "publishes", target="_blank"), "information on stillbirths and infant deaths registered in Scotland.", br(),  
            "Across the UK, surveillance of perinatal deaths is undertaken by MBRRACE-UK (Mothers and Babies: Reducing Risk through Audits and Confidential Enquiries across the UK). The latest MBRRACE-UK perinatal mortality", 
            tags$a(href="https://www.npeu.ox.ac.uk/mbrrace-uk#mbrrace-uk-perinatal-mortality-surveillance-report", "report",  target="_blank"),
            "(providing information on babies born in 2017) provides background information on factors that influence perinatal deaths.", br(),
            "Within Scotland, the", tags$a(href="https://ihub.scot/improvement-programmes/scottish-patient-safety-programme-spsp/maternity-and-children-quality-improvement-collaborative-mcqic/",
            "Maternal and Children Quality Improvement Collaborative (MCQIC)",  target="_blank"), "focuses on care quality to improve outcomes for babies, children and their mothers.  
            One of the key outcomes they track is stillbirths.")
            )
})

##END


