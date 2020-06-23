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
                 "National Records for Scotland (NRS).",class="externallink"), 
                 tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink"),
                 " receives weekly data from NRS on deaths and stillbirths. 
                 PHS also normally receives weekly data on birth registrations from NRS, however birth registrations have been deferred during the pandemic period. So, to provide denominator information, 
                 birth numbers for January 2020 onwards have been taken from NHS notifications of births recorded in the national",
                 tags$a(href="https://www.isdscotland.org/Health-Topics/Child-Health/Child-Health-Programme/", "child health information systems.",class="externallink")),
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
                 For the stillbirth, neonatal, post-neonatal and extended perinatal death rates P 
                 charts are presented. For the infant death rates a U chart is presented. The type of chart used depends on the type of 
                 data involved (which statistical distribution we think it follows)."),
               p("Further information on these methods of presenting data can be found at the ",                      
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts.')),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive controls  ----
###############################################.

# Perinatal reactive drop-down control showing list of area names depending on areatype selected 

#  FOR WHEN WE HAVE NHS BOARD DATA
# output$geoname_ui_perinatal <- renderUI({
# 
#   #Lists areas available in
#   areas_summary_perinatal <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_perinatal])
# 
#   selectizeInput("geoname_perinatal", label = NULL, choices = areas_summary_perinatal, selected = "")
# })

#  p_perinatal_table_data <- reactive({ # may add tables to tab
#    table <- p_perinatal_table 
# })
#  
#  u_perinatal_table_data <- reactive({ # may add tables to tab
#    table <- u_perinatal_table 
#  })
# 
# 
# 
###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$perinatal_explorer <- renderUI({

  # text for titles of cut charts
  perinatal_title <- paste0(case_when(input$measure_select_perinatal == "stillbirths" ~ 
                                        paste0("Monthly rate of stillbirths per 1,000 total births in Scotland", input$geoname_perinatal),
                                      input$measure_select_perinatal == "pnnd" ~
                                        paste0("Monthly rate of post-neonatal deaths per 1,000 live births in Scotland", input$geoname_perinatal),
                                      input$measure_select_perinatal == "nnd" ~ 
                                        paste0("Monthly rate of neonatal deaths per 1,000 live births in Scotland", input$geoname_perinatal),
                                      input$measure_select_perinatal == "extperi" ~ 
                                        paste0("Monthly rate of extended perinatal deaths per 1,000 total births in Scotland", input$geoname_perinatal),
                                      input$measure_select_perinatal == "infantdeaths" ~ 
                                        paste0("Monthly rate of infant deaths per 1,000 live births in Scotland", input$geoname_perinatal)))
  
  # Link used in intro text
  link_perinatal <- "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/deaths-background-information/stillbirths-and-infant-deaths"
  peri_common_intro <- "It is important to monitor the levels of stillbirth and infant mortality 
                    during the COVID-19 pandemic, as they may be influenced by maternal health 
  and well-being, by how maternity services are provided, and how people seek and interact with care.
  NHS Scotland and the Scottish Government"
  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies",
  "have produced guidelines for attending antenatal and postnatal care appointments during the pandemic.",class="externallink")
  # Intro paragraph within perinatal tab
  if (input$measure_select_perinatal == "stillbirths") {
    intro_text <- p("Stillbirths refer to", tags$a(href=link_perinatal, 
                    "a child born after the 24th week of pregnancy which did not breathe or show any signs of life.",class="externallink"), 
                    "The stillbirth rate in Scotland in 2018 ", 
                    tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2018/section-4-stillbirths-and-infant-deaths", 
                    "was 3.7 per 1,000 total births.",class="externallink"),
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "pnnd") {
    intro_text <- p("Post-neonatal deaths refer to", tags$a(href=link_perinatal, 
                    "deaths occuring after the first 4 weeks but within the first year",class="externallink"), "of life.",
                    "The post-neonatal death rate in Scotland in 2018 ", 
                    tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2018/section-4-stillbirths-and-infant-deaths", 
                           "was 1.2 per 1,000 total live births.",class="externallink"),
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "nnd") {
    intro_text <- p("Neonatal deaths refer to", tags$a(href=link_perinatal, 
                  "deaths in the first four weeks",class="externallink"), "of life.",
                  "The neonatal death rate in Scotland in 2018 ", 
                  tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2018/section-4-stillbirths-and-infant-deaths", 
                         "was 2.0 per 1,000 total live births.",class="externallink"),
                  peri_common_intro,
                  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                         "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "extperi") {
    intro_text <- p("Extended perinatal deaths refer to", tags$a(href=link_perinatal, 
"the sum of stillbirths and neonatal mortality",class="externallink"), "(deaths within the first 4 weeks of life).",
"The extended perinatal death rate in Scotland in 2018 ", 
tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2018/section-4-stillbirths-and-infant-deaths", 
       "was 5.7 per 1,000 total births.",class="externallink"),
peri_common_intro,
tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
       "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
    
  } else if (input$measure_select_perinatal == "infantdeaths") {
    intro_text <- p("Infant deaths refer to", tags$a(href=link_perinatal, 
                    "all deaths in the first year",class="externallink"), "of life; 
                    this includes neonatal and post-neonatal deaths, but not stillbirths. ",
                    "The infant death rate in Scotland in 2018 ", 
                    tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2018/section-4-stillbirths-and-infant-deaths", 
                           "was 3.2 per 1,000 total live births.",class="externallink"),
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
  }


  control_chart_commentary <- p("As stillbirths and infant deaths are relatively rare events in Scotland mortality rates tend to fluctuate over time just by chance.
                      We have therefore used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf', 
                      "‘control charts’",class="externallink"), "to present the rates above.", br(),
                      "Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation.  
                      Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’", br(),
                      "The dots joined by a solid line in the chart below show the monthly mortality rate for the measure selected from January 2017 onwards.", br(),  
                      "The other lines - centreline, and control and warning limits - are there to help show how unexpected any observed changes are. 
                      The centreline is an average (mean) over the time period.  Control and warning limits take into consideration the random variation 
                      that would be expected by chance, and help us decide when values are unexpectedly low or high and require further investigation.")
  
  may_data_commentary <- p("By law, all stillbirths must be registered within 21 days of the baby being delivered, and all infant deaths must be registered within 8 days of the baby dying.  
                           The data extract used to produce the mortality numbers and rates for up to and including May 2020 presented here was taken on 17 June 2020, and included stillbirths and infant deaths registered on up and including 14 June 2020.", br(), 
                           "It is therefore possible that some stillbirths occurring in the last week of May 2020 may not have been registered by the time the data extract was taken.  The stillbirth and extended perinatal mortality rates for May 2020 should 
                           therefore be taken as provisional, and they may increase when the data is refreshed (and new rates for June 2020 are added) in July 2020.", br(),  
                           "We would expect any increases to be small, as in previous years 95% of stillbirths have been registered within 14 days of the baby being delivered, despite the legal limit allowing up to 21 days.", br(),  
                           "This issue does not affect infant deaths, hence the neonatal, post-neonatal, and infant death rates for May 2020 should be final.")                  
  # Specify items to display in perinatal ui 
  tagList(
    fluidRow(column(12, 
                    intro_text,
                    h4(paste0(perinatal_title)))),
    actionButton("btn_perinatal_rules", "How do we identify patterns in the data?", 
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("perinatal_chart"))),
    fluidRow(column(12, renderUI(control_chart_commentary))),
    fluidRow(column(12, renderUI(may_data_commentary))))
  
}) #close perinatal_explorer function
  
  ###############################################.
  ## Charts ----
  ###############################################.
  
  #run chart function to generate spc charts
  output$perinatal_chart <- renderPlotly({
    trend_data <- perinatal %>% filter(type == input$measure_select_perinatal)
    
    yaxis_plots[["title"]] <- "Rate per 1,000 births"
    xaxis_plots[["title"]] <- "Month"
    
    # xaxis_plots[["dtick"]] <- 180
    # xaxis_plots[["tickmode"]] <- "array" 
    # xaxis_plots[["tickvals"]] <- c("2017-01-01", "2017-06-01", "2018-01-01", "2018-06-01",
    #                                "2019-01-01", "2019-06-01", "2020-01-01", "2020-05-01") %>% as.Date()
    # xaxis_plots[["ticktext"]] <- c("2017-01-01", "2017-06-01", "2018-01-01", "2018-06-01",
    #                                "2019-01-01", "2019-06-01", "2020-01-01", "2020-05-01")
      
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

output$download_perinatal_data <- downloadHandler(
  filename ="stillbirth_infantdeaths_extract.csv",
  content = function(file) {
    write_csv(perinatal %>% filter(type == input$measure_select_perinatal) %>% 
                select(month_of_year:centreline, upper_cl_3_std_dev:area_name),
              file) } 
)

###############################################.
## Commentary ----
###############################################.
output$perinatal_commentary <- renderUI({
  tagList(h2("Perinatal mortality - 1st July 2020"),
          p("It is important to monitor the levels of stillbirth and infant mortality during the COVID-19 pandemic, 
            as they may be influenced by maternal health and well-being, by how maternity and child health services are provided, and 
            by how people seek and interact with care.", br(),
            "NHS Scotland and Scottish Government",
            tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies",
            "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.", br(),
            "The dashboard shows monthly data for stillbirths and infant deaths (those occurring under the age of one year). These are based on data 
            from National Records for Scotland (NRS), and are presented as rates per 1,000 births.", br(),
            "Control charts have been used to support interpretation of these data. As numbers of deaths are relatively low, mortality rates tend to fluctuate from month to month just by chance: 
            control charts help differentiate between expected random variation and changes which warrant further investigation.", br(), 
            "In this first release of information on stillbirths and infant deaths (1 July 2020), data are shown for January 2017 to May 2020, with the most recent three months (March-May 2020) 
            being those when health and health services may have been affected by COVID-19.", br(), 
            "In this period the only observations which have reached a ‘warning limit’ as indicated by the relevant control chart were neonatal deaths in March 2020, 
            where the rate was just above the upper warning limit (3.7/1,000 compared to the UWL of 3.6/1,000), but did not breach the upper control limit 
            (the trigger for further investigation).", br(), 
            "In April and May there were fewer neonatal deaths, and the rate fell to below the upper warning limit. Rates of stillbirths and extended perinatal deaths are being closely 
            monitored, as these approached, but did not breach, the upper warning limit in May 2020.", br(),
            "NRS", tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2018/section-4-stillbirths-and-infant-deaths", 
            "publishes",class="externallink"), "information on stillbirths and infant deaths registered in Scotland.", br(),  
            "Across the UK, surveillance of perinatal deaths is undertaken by MBRRACE-UK (Mothers and Babies: Reducing Risk through Audits and Confidential Enquiries across the UK). The latest MBRRACE-UK perinatal mortality", 
            tags$a(href="https://www.npeu.ox.ac.uk/mbrrace-uk#mbrrace-uk-perinatal-mortality-surveillance-report", "report", class="externallink"),
            "(providing information on babies born in 2017) provides background information on factors that influence perinatal deaths.", br(),
            "Within Scotland, the", tags$a(href="https://ihub.scot/improvement-programmes/scottish-patient-safety-programme-spsp/maternity-and-children-quality-improvement-collaborative-mcqic/",
            "Maternal and Children Quality Improvement Collaborative (MCQIC)", class="externallink"), "focuses on care quality to improve outcomes for babies, children and their mothers.  
            One of the key outcomes they track is stillbirths.")
            )
})

##END


