# Wider impacts dashboard - Births and babies tab - Still births and infant deaths section
# Server code

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
                 "National Records for Scotland (NRS) (external website).", target="_blank"),
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
  "have produced guidelines for attending antenatal and postnatal care appointments during the pandemic (external website).", target="_blank")

  # Text to be updated every month with updated dates
  last_month_peri <- "June 2022"
  cutdate_peri <- "17 July 2022"
  extractdate_peri <- "20 July 2022"
  nextup_peri <- "September 2022"
  nextdata_peri <- "August 2022"




  # Number of deaths and of births used in the text
  no_stillperi <- peri_filt() %>% pull(number_of_deaths_in_month)
  no_births <- peri_filt() %>% pull(sample_size) %>% format(big.mark=",")

  # Intro paragraph within perinatal tab
  if (input$measure_select_perinatal == "stillbirths") {
    intro_text <- p("Stillbirths refer to", tags$a(href=link_perinatal,
                    "a child born after the 24th week of pregnancy which did not breathe or show any signs of life (external website).", target="_blank"),
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                    Whilst each stillbirth is clearly a tragedy for the family involved, stillbirths are uncommon events in Scotland: ", no_stillperi ," stillbirths (and ", no_births ," total
                    [live and still] births) occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "pnnd") {
    intro_text <- p("Post-neonatal deaths refer to", tags$a(href=link_perinatal,
                    "deaths occuring after the first 4 weeks but within the first year (external website)", target="_blank"), "of life.",
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines (external website)", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                    Whilst each post-neonatal death is clearly a tragedy for the family involved, post-neonatal deaths are uncommon events in Scotland: ", no_stillperi ," post-neonatal deaths
                    (and ", no_births ," live births) occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "nnd") {
    intro_text <- p("Neonatal deaths refer to", tags$a(href=link_perinatal,
                  "deaths in the first four weeks (external website)", target="_blank"), "of life.",
                  peri_common_intro,
                  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                         "have produced guidelines (external website)", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                  Whilst each neonatal death is clearly a tragedy for the family involved, neonatal deaths are uncommon events in Scotland: ", no_stillperi ," neonatal deaths (and ", no_births ," live births)
                  occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "extperi") {
    intro_text <- p("Extended perinatal deaths refer to", tags$a(href=link_perinatal,
"the sum of stillbirths and neonatal mortality (external website)", target="_blank"), "(deaths within the first 4 weeks of life).",
peri_common_intro,
tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
       "have produced guidelines (external website)", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
Whilst each extended perinatal death is clearly a tragedy for the family involved, extended perinatal deaths are uncommon events in Scotland: ", no_stillperi ," extended perinatal deaths (and ", no_births ," total
[live and still] births) occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  } else if (input$measure_select_perinatal == "infantdeaths") {
    intro_text <- p("Infant deaths refer to", tags$a(href=link_perinatal,
                    "all deaths in the first year (external website)", target="_blank"), "of life;
                    this includes neonatal and post-neonatal deaths, but not stillbirths. ",
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines (external website)", target="_blank"), "for attending antenatal and postnatal care appointments during the pandemic.
                    Whilst each infant death is clearly a tragedy for the family involved, infant deaths are uncommon events in Scotland: ", no_stillperi ," infant deaths (and ", no_births ," live births)
                    occurring in Scotland in ", last_month_peri, " had been registered by ", cutdate_peri, ".")
  }

  nrs_commentary <- p("It is important to note that chart data is based on month of occurence rather than month of registration used in NRS publications, and so
                      figures may differ slightly.
                      In August 2021, NRS published data on stillbirths and infant deaths registered in 2020. The data tables can be found in",
                      tags$a(href= 'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2020/list-of-data-tables#section4',
                      "Section 4: Stillbirths and infant deaths (external website)", target="_blank"), "on the NRS website.")

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


##END

