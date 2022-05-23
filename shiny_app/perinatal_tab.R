##Server script for perinatal tab..

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
    filter(month_of_year == max(month_of_year, na.rm = T))
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
  last_month_peri <- "April 2022"
  cutdate_peri <- "15 May 2022"
  extractdate_peri <- "18 May 2022"
  nextup_peri <- "July 2022"
  nextdata_peri <- "May 2022"


  
  
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
[live and still] births) occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "infantdeaths") {
    intro_text <- p("Infant deaths refer to", tags$a(href=link_perinatal,
                    "all deaths in the first year", target="_blank"), "of life;
                    this includes neonatal and post-neonatal deaths, but not stillbirths. ",
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                    Whilst each infant death is clearly a tragedy for the family involved, infant deaths are uncommon events in Scotland: ", no_stillperi ," infant deaths (and ", no_births ," live births)
                    occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  }

  nrs_commentary <- p("It is important to note that chart data is based on month of occurence rather than month of registration used in NRS publications, and so
                      figures may differ slightly.
                      In August 2021, NRS published data on stillbirths and infant deaths registered in 2020. The data tables can be found in",
                      tags$a(href= 'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2020/list-of-data-tables#section4',
                      "Section 4: Stillbirths and infant deaths", target="_blank"), "on the NRS website.")

  control_chart_commentary <- p("As stillbirths and infant deaths are relatively rare events in Scotland mortality rates tend to fluctuate over time just by chance.
                      We have therefore used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                      "‘control charts’", target="_blank"), "to present the rates above.", br(),
                      "Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation.
                      Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’", br(),
                      "The dots joined by a solid black line in the chart above show the monthly mortality rate for the measure selected from July 2017 onwards.", br(),
                      "The other lines - centreline, and control and warning limits - are there to help show how unexpected any observed changes are.
                      The solid blue centreline is an average (mean) rate over the period July 2017 to December 2019. The dotted blue centreline continues that average through more recent time periods. Control and warning limits take into consideration the random variation
                      between months that would be expected by chance, and help us decide when values are unexpectedly low or high and require further investigation.
                      The use of a fixed centreline increases sensitivity of detection of signals in more recent data, since recent observations within the pandemic period do not contribute to this reference centreline.")

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
    
    xaxis_plots[["ticktext"]] <- list("Jul 2017", "Jan 2018", "Jul 2018", "Jan 2019", "Jul 2019", "Jan 2020",
                                      "Jul 2020", "Jan 2021", "Jul 2021", "Jan 2022")
    
    xaxis_plots[["tickvals"]] <- list("2017-07-01", "2018-01-01", "2018-07-01", "2019-01-01", "2019-07-01", "2020-01-01",
                                      "2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01")
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
      add_lines(data= trend_data %>% filter(month_of_year < as.Date("2020-02-01")),
                y = ~centreline, line = list(color = "blue"),
                 hoverinfo= "none", name = "Centreline") %>%
      add_lines(data= trend_data %>% filter(month_of_year >= as.Date("2020-01-01")),
                y = ~centreline, line = list(color = "blue", dash = "longdash"),
                hoverinfo= "none", showlegend = F) %>%
      add_lines(data = trend_data, y = ~upper_cl_3_std_dev, line = list(color = "red", dash = "dash"),
                hoverinfo= "none", name = "Control limits") %>%
      add_lines(y = ~lower_cl_3_std_dev, line = list(color = "red", dash = "dash"),
                hoverinfo= "none", showlegend = FALSE) %>%
      add_lines(y = ~upper_wl_2_std_dev, line = list(color = "#33ccff", dash = "dot"),
                hoverinfo= "none", name = "Warning limits") %>%
      add_lines(y = ~lower_wl_2_std_dev, line = list(color = "#33ccff", dash = "dot"),
                hoverinfo= "none", showlegend = FALSE) %>%
      # adding shifts
      add_markers(data = trend_data %>% filter(shift == T), y = ~ rate,
                  marker = list(color = "blue", size = 12, symbol = "circle"),
                  name = "Shifts", hoverinfo= "text") %>%
      # adding outliers
      add_markers(data = trend_data %>% filter(outlier == T), y = ~ rate,
                  marker = list(color = "red", size = 10, symbol = "diamond"),
                  name = "Outliers", hoverinfo= "text") %>%
    
      # adding shifts
      add_markers(data = trend_data %>% filter(trend == T), y = ~ rate,
                  marker = list(color = "green", size = 10, symbol = "square"),
                  name = "Trends", hoverinfo= "text") %>%
      # adding inner third
      add_markers(data = trend_data %>% filter(inner == T), y = ~ rate,
                  marker = list(color = "gray", size = 10, symbol = "x"),
                  name = "Inner one-third", hoverinfo= "text") %>%
      # adding outer third
      add_markers(data = trend_data %>% filter(outer == T), y = ~ rate,
                  marker = list(color = "orange", size = 10, symbol = "star"),
                  name = "Outer one-third", hoverinfo= "text") %>%
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
    
    h2("Stillbirths and infant deaths - 4th May 2022"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in March 2022."),
    p("In March 2022 the neonatal mortality rate (4.6 per 1,000 live births) exceeded the upper control limit of 4.4 per 1,000. The extended perinatal mortality rate, which captures both stillbirths and neonatal deaths was 9.1 per 1,000 live and stillbirths; this was above the upper warning limit of 8.4 per 1,000 but did not exceed the control limit of 9.6 per 1,000. Similarly, the overall infant mortality rate (5.9 per 1,000 live births) exceeded the warning limit (5.1 per 1,000), but not the upper control limit 6.0 per 1,000). Post-neonatal deaths (those that occur after 4 weeks of age) were within the expected range."),
    p("Each of the losses reflected in the information reported here is a tragedy for those involved. The review processes described below (2nd March 2022) will be important in understanding and learning from these events."),
    p("The effects of COVID-19 infection, and the safety and protection of COVID-19 vaccination in pregnancy, continue to be monitored in Scotland", tags$a(href = "https://academic.oup.com/ije/advance-article/doi/10.1093/ije/dyab243/6491903?login=true", "(https://academic.oup.com/ije/advance-article/doi/10.1093/ije/dyab243/6491903?login=true)", target = "_blank"), "and internationally", tags$a(href = "https://obgyn.onlinelibrary.wiley.com/doi/full/10.1002/uog.23619", "(https://obgyn.onlinelibrary.wiley.com/doi/full/10.1002/uog.23619).", target = "_blank"), "There is evidence that COVID-19 infection during pregnancy is associated with worse outcomes for mothers and babies. In Scotland it has been found that among babies born to mothers who had COVID-19 infection in the month prior to birth, the extended perinatal mortality rate was 13.4 per 1,000 live and stillbirths (95% confidence interval 8.1-21.9)", tags$a(href = "https://publichealthscotland.scot/media/12100/22-03-09-covid19-winter_publication_report.pdf", "(https://publichealthscotland.scot/media/12100/22-03-09-covid19-winter_publication_report.pdf)", target = "_blank"),"."),
    p("COVID-19 vaccination is a safe and effective way to reduce the risk of COVID-19 in pregnancy, and vaccination is strongly recommended. By mid-February 28,301 women had been vaccinated during pregnancy in Scotland, and 54% of women who gave birth in January 2022 had received at least two doses of vaccine, either during or prior to pregnancy. Among babies born to mothers who received a dose of COVID-19 vaccination in the month prior to birth, the extended perinatal mortality rate was 5.2 per 1,000 live and stillbirths (95% confidence interval 2.9-8.9)", tags$a(href = "https://publichealthscotland.scot/media/12100/22-03-09-covid19-winter_publication_report.pdf", "(https://publichealthscotland.scot/media/12100/22-03-09-covid19-winter_publication_report.pdf).", target = "_blank"), "More information and support with decision-making is available from the Royal College of Obstetricians and Gynaecologists", tags$a(href = "https://www.rcog.org.uk/guidance/coronavirus-covid-19-pregnancy-and-women-s-health/vaccination/covid-19-vaccines-pregnancy-and-breastfeeding-faqs/", "(https://www.rcog.org.uk/guidance/coronavirus-covid-19-pregnancy-and-women-s-health/vaccination/covid-19-vaccines-pregnancy-and-breastfeeding-faqs/)", target = "_blank"), "and NHS Inform", tags$a(href = "https://www.nhsinform.scot/covid-19-vaccine/the-vaccines/pregnancy-breastfeeding-and-the-coronavirus-vaccine", "(https://www.nhsinform.scot/covid-19-vaccine/the-vaccines/pregnancy-breastfeeding-and-the-coronavirus-vaccine)", target = "_blank"),"."),
    h2("Stillbirths and infant deaths - 2nd March 2022"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in January 2022, 
      when all reported measures of perinatal and infant mortality were within expected limits."),
    p("As described in the dashboard information box ‘How do we identify patterns in the data?’, control charts are used to provide an indication 
      of when changes in these data are less likely to be due to chance alone. In refreshed data now available for 2021, the months March to October 
      are identified as a ‘shift’ in the neonatal mortality rate, and the months April to November as a ‘shift’ in the infant mortality rate. A ‘shift’ 
      describes when there is a sequence of 8 or more months of data that are above (or below) the average level, which here is based on the pre-pandemic 
      mortality rates from 2017 to 2019. These ‘shifts’ are indicated by the blue markers on the chart for the relevant sequence of months. 
      This pattern suggests there was a sustained period in the middle part of 2021 when neonatal and infant mortality rates were higher than pre-pandemic 
      levels, rather than fluctuating around this level as would be expected with random variation. In the most recent months, the rates for both measures 
      have been below the average level. No shifts are noted for stillbirths, extended perinatal mortality or post-neonatal deaths."),
    
    p("As noted below, neonatal and infant deaths are subject to a number of review and learning processes to identify and mitigate any contributing factors. 
      Systematic information on deaths occurring in neonatal units is gathered via the", 
      tags$a(href = "https://www.npeu.ox.ac.uk/pmrt", "Perinatal Mortality Review Tool;", target = "_blank"), 
      "the UK-wide collaboration,", tags$a(href = "https://www.npeu.ox.ac.uk/mbrrace-uk", "MBRRACE-UK,", target = "_blank"), " provides surveillance and 
      investigation of maternal deaths, stillbirths and infant deaths, and there is a", 
      tags$a(href = "https://www.gov.scot/publications/maternity-neonatal-perinatal-adverse-event-review-process-scotland/", "standardised approach to review of perinatal adverse events in Scotland.", target = "_blank"),
      " All child deaths in Scotland are now reviewed to ensure that contributing factors are understood, and that learning is used in ", tags$a(href = "https://www.healthcareimprovementscotland.org/our_work/governance_and_assurance/deaths_of_children_reviews.aspx", "prevention and improving care quality.", target = "_blank"), " 
      A number of agencies consider the outcomes of these review processes, including the", tags$a(href = "https://www.perinatalnetwork.scot/neonatal/", "Scottish National Neonatal Network,", target = "_blank"), 
      "the ", tags$a(href = "https://ihub.scot/improvement-programmes/scottish-patient-safety-programme-spsp/spsp-programmes-of-work/maternity-and-children-quality-improvement-collaborative-mcqic/", "Maternity and Children Quality Improvement Collaborative", target = "_blank"), " and the Scottish Government."),
    
    p("Annual data on stillbirths and infant deaths are produced by ", tags$a(href = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables", "National Records of Scotland ", target = "_blank"), 
      "and information for 2021 is scheduled to be published in June 2022."),
    
    
    h2("Stillbirths and infant deaths - 2nd February 2022"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in ",
      "November and December 2021. In these months all reported measures of perinatal and infant mortality were within expected limits."),
    p("In addition, in this dashboard release information on gestation at delivery has been updated under the ‘Births and babies’ tab, ",
      "to include births in September and October 2021, which is of interest in follow up to the commentary below on neonatal mortality ",
      "in September 2021. These data show that in September 2021, 0.8% of singleton live births occurred at under 32 weeks gestation, and ",
      "5.5% at 32-36 weeks gestation. Both of these figures are close to the expected level based on the average over the period January ",
      "2018 to February 2020. As these data are based on singleton births only, data were also reviewed separately to assess the total number ",
      "and percentage of premature babies, including those from multiple births. This showed that the total number of babies born at under 32 ",
      "weeks gestation in September 2021 was relatively high, and in the upper quartile for monthly values in January to October 2021, however ",
      "it was not exceptional in comparison to the observed values in this period."),
    p("Further information on COVID-19 infection and vaccination in pregnancy in Scotland, including data on neonatal infections and extended ",
      "perinatal mortality rate have also been published recently (see ", tags$a(href="https://www.nature.com/articles/s41591-021-01666-2", 
      "SARS-CoV-2 infection and COVID-19 vaccination rates in pregnant women in Scotland", target="_blank"), " and ", 
      tags$a(href="https://www.publichealthscotland.scot/publications/show-all-releases?id=20580", "Public Health Scotland publications", 
      target="_blank"), "). The information in these publications provides further reassurance regarding the safety of vaccination in pregnancy, ",
      "and highlights the effective protection it provides for pregnant women and their babies."),
    
    
    h2("Stillbirths and infant deaths - 1st December 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in October 2021."),
    p("In October 2021 all reported measures of perinatal and infant mortality were within expected limits. ",
      "The neonatal mortality rate, which was raised in September 2021, was 3.3 per 1,000 live births in Oct 2021, ",
      "and returned to within the warning limit (3.6 per 1,000). The overall infant mortality rate (4.9 per 1,000 live births) ",
      "in Oct 2021 was close to, but did not breach the upper warning limit (5.0 per 1,000), whereas in September 2021 it was ",
      "above this (5.5 per 1,000). Stillbirths and post-neonatal deaths (those that occur after 4 weeks of age) were at expected levels in Oct 2021."),
    
    p("As referenced in the commentary on September 2021 data, all neonatal deaths are the subject of local and national review processes. ",
      "In addition to this, the higher than expected numbers that month prompted additional review of available data at national level, in ",
      "particular with respect to the role of prematurity, and to understand any relationship with COVID-19 infections. Findings from this ",
      "review are preliminary, as relevant information at national level on the total number of births and gestational age of babies in that ",
      "period is not yet fully complete, but will be by February 2022 (see below)."),
    
    p("Initial findings suggest that, overall, the number of births in September 2021 was at the expected level. Preliminary information ",
      "on prematurity suggests that the number of babies born at less than 32 weeks gestation in September 2021 was at the upper end of ", 
      "monthly numbers seen in 2021 to date. This may contribute to the neonatal mortality rate, as prematurity is associated with an ",
      "increased risk of neonatal death. The relevant dashboard indicators on live births and gestational age will be updated to include ",
      "September 2021 information, using the most complete data available, in the next dashboard update in February 2022."),
    
    p("There is no information at this stage to suggest that any of the neonatal deaths in September 2021 were due to COVID-19 infection ",
      "of the baby. Likewise, preliminary review does not indicate that maternal COVID-19 infection played a role in these events. Several ",
      "surveillance programmes focussing on direct impact of COVID-19 on pregnant women and babies are underway. The ",
      tags$a(href="https://www.ed.ac.uk/usher/eave-ii/covid-19-in-pregnancy-in-scotland", "COVID-19 in Pregnancy in Scotland study (COPS)", target="_blank"), 
      "has been established to provide population-level monitoring and analysis of the occurrence and outcomes of COVID-19 infection in pregnancy. ",
      "Monthly reporting of cases is available within the ", tags$a(href="https://publichealthscotland.scot/publications/covid-19-statistical-report/", 
                                                                    "PHS COVID-19 Statistical Report", target="_blank"),
      "and will next be updated on the 8th December. At UK-level, surveillance of any complications of COVID-19 among neonates is being undertaken ",
      "through the ", tags$a(href="https://www.rcpch.ac.uk/work-we-do/bpsu/study-neonatal-complications-coronavirus-disease-covid-19", 
                             "British Paediatric Surveillance Unit", target="_blank"), "."),
    
    p("Whilst COVID-19 does not appear to have played a role in the tragic deaths which occurred in September 2021, there is international ",
      "evidence which shows that COVID-19 infection during pregnancy is associated with a higher chance of problems for both mother and baby. ",
      "The Royal College of Obstetricians and Gynaecologists (RCOG) maintains a review of the literature on COVID-19 in pregnancy, with an update ",
      "published in ", tags$a(href="https://www.rcog.org.uk/en/guidelines-research-services/guidelines/coronavirus-pregnancy/", "November 2021", target="_blank"), ".",
      "COVID-19 infection during pregnancy has been linked to an increased risk of stillbirth. Symptomatic COVID-19 is associated with an increased ",
      "likelihood of premature birth due to a need to deliver the baby early for the health of mother or baby. "),
    
    p("In view of the small but important risks of COVID-19 infection in pregnancy, pregnant women are encouraged to take up the offer of COVID-19 ",
      "vaccination. Information on this is available from the ", tags$a(href="https://www.rcog.org.uk/globalassets/documents/guidelines/2021-02-24-combined-info-sheet-and-decision-aid.pdf", 
                                                                        "RCOG", target="_blank"), ", and from ",
      tags$a(href="https://www.nhsinform.scot/covid-19-vaccine/the-vaccines/pregnancy-breastfeeding-and-the-coronavirus-vaccine", 
             "NHS Inform", target="_blank"), ". There is good evidence that it is effective at preventing severe COVID-19 illness. In Scotland, in",
      tags$a(href="https://publichealthscotland.scot/publications/covid-19-statistical-report/covid-19-statistical-report-3-november-2021/", 
             "data available", target="_blank"), "to the end of September 2021, 99 women had been admitted to critical care within 21 days of ",
      " testing positive for COVID-19 during pregnancy, of whom 98 were unvaccinated. Vaccine safety monitoring takes place within ", 
      tags$a(href="https://publichealthscotland.scot/media/8413/covid-19-vaccine-surveillance-report-oct21-english.pdf", 
                                                                                                "Scotland", target="_blank"),
      " and internationally, with more than 200,000 women having received the vaccine during pregnancy across the UK and US, with no concerning 
      safety signals (see ", tags$a(href="https://www.rcog.org.uk/en/guidelines-research-services/guidelines/coronavirus-pregnancy/", 
                                      "Coronavirus (COVID-19) Infection in Pregnancy report", target="_blank"), ").", 
      tags$a(href="https://www.gov.uk/government/news/new-ukhsa-study-provides-more-safety-data-on-covid-19-vaccines-in-pregnancy", 
                                "Recently published data", target="_blank"), " from England provides further reassurance regarding birth ",
      "outcomes among vaccinated women."),
    
    
    h2("Stillbirths and infant deaths - 3rd November 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in September 2021."),
    p("In September 2021 both the neonatal mortality rate (4.9 per 1,000 live births) and the extended perinatal mortality rate (9.9 per 1,000 live and stillbirths) exceeded their upper control limits of 4.3 and 9.4, respectively. Extended perinatal death rate is a measure which combines stillbirths and neonatal deaths. Examining the data shows that the increase in extended perinatal mortality reflects the higher than expected neonatal deaths, and stillbirths that were at, but not lower than, their expected level. The overall infant mortality rate (5.5 per 1,000 live births) exceeded the warning limit (5.0), but not the upper control limit (5.9). This was due to the high number of neonatal deaths. Post-neonatal deaths (those that occur after 4 weeks of age) were not increased."),
    p("Each of these events is a tragedy for those involved. There are a number of existing processes through which these events, in common with all neonatal deaths, will be reviewed. All child deaths in Scotland are now reviewed to ensure that contributing factors are understood, and that learning is used in ",
      tags$a(href="https://www.healthcareimprovementscotland.org/our_work/governance_and_assurance/deaths_of_children_reviews.aspx",
             "prevention and improving care quality", target="_blank"), ". ",
      "Systematic information on deaths occurring in neonatal units is gathered via the ",
      tags$a(href="https://www.npeu.ox.ac.uk/pmrt", "Perinatal Mortality Review Tool"),
      " and the UK-wide collaboration, MBRRACE-UK, provides surveillance and investigation of maternal deaths, stillbirths and infant deaths. ",
      tags$a(href="https://www.gov.scot/publications/maternity-neonatal-perinatal-adverse-event-review-process-scotland/", "A standardised approach"),
      "to review of perinatal adverse events has also recently been adopted in Scotland"),
    p("As the overall number of deaths occurring each month is fortunately small, mortality rates tend to fluctuate from month to month just by chance. Control charts are a tool that help tell the difference between expected chance variation and changes which warrant further investigation. Exceeding the upper control limit indicates there is a higher likelihood that there are factors beyond random variation that may have contributed to the number of deaths that occurred. In view of this, in addition to the processes outlined above, Public Health Scotland is working with the ",
      tags$a(href="https://www.perinatalnetwork.scot/neonatal/" , "Scottish National Neonatal Network"), ", the ",
      tags$a(href="https://ihub.scot/improvement-programmes/scottish-patient-safety-programme-spsp/spsp-programmes-of-work/maternity-and-children-quality-improvement-collaborative-mcqic/" , "Maternity and Children Quality Improvement Collaborative"),
      " and the Scottish Government to understand any possible contributing factors to the most recent infant mortality patterns, and to incorporate findings into existing prevention and improvement work. Further information on the results of this work will be provided in future commentary."),
    h2("Stillbirths and infant deaths - 2nd June 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in April 2021. The rate of stillbirths, and all reported infant death measures, remained within the warning threshold limits this month. The stillbirth rate in April 2021 was 2.4 per 1,000 total births (baseline, pre-pandemic average 3.8 per 1,000 total births), the neonatal death rate was 2.7 per 1,000 live births (average 2.2 per 1,000 live births), and the infant mortality rate was 3.5 per 1,000 live births (average 3.3 per 1,000 live births). "),
    h2("Stillbirths and infant deaths - 5th May 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in March 2021. The rate of stillbirths, and all reported infant death measures, remained within the warning threshold limits this month. The stillbirth rate in March 2021 was 4.5 per 1,000 total births (average 3.8 per 1,000 total births), and the infant mortality rate was 3.3 per 1,000 live births (average 3.3 per 1,000 live births). "),
    p("All the stillbirth and infant death data have been revised in this latest release. Originally we reported these events from January 2017, and this has now been changed to July 2017. Also, a fixed centreline (average) has been recalculated for every chart using the data for the months July 2017 to December 2019.  The dotted centreline continues that average through the more recent time period to allow determination of whether the values seen in these months are unexpectedly low or high. The use of a fixed centreline increases sensitivity of detection of signals in more recent data, since recent observations within the pandemic period do not contribute to this reference centreline."),
    h2("Stillbirths and infant deaths - 7th April 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include events that occurred in February 2021. The rate of stillbirths, and all reported infant death measures, remained within the warning threshold limits this month. The stillbirth rate in February 2021 was 4.3 per 1,000 total births, and infant mortality rate was 3.2 per 1,000 live births."),
    p("Further background information is available within the commentary for July 2020."),
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


