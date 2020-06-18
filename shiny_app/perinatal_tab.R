##Server script for perinatal tab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_perinatal_modal,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p("The information on stillbirths and infant mortality shown in this tool is derived 
                 from deaths and births data provided by ",
                 tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/deaths-background-information/stillbirths-and-infant-deaths",
                        "National Records for Scotland.",class="externallink")),
               p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink"),
                 " receives weekly data from NRS on deaths and stillbirths. 
                  PHS also normally receives weekly data on birth registrations from NRS. 
                 However birth registrations have been  suspended during the pandemic period. 
                 So, to provide denominator information, birth numbers from January 2020 are 
                 taken from notifications of births in the Child Health Programme System."),
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
  NHS Scotland and Scottish Government " 
  # Intro paragraph within perinatal tab
  if (input$measure_select_perinatal == "stillbirths") {
    intro_text <- p("Stillbirths refer to", tags$a(href=link_perinatal, 
                    "a child born after the 24th week of pregnancy which did not breathe or show any signs of life.",class="externallink"), 
                    "The stillbirth rate in Scotland in 2018 ", 
                    tags$a(href="https://www.isdscotland.org/Health-Topics/Maternity-and-Births/Births/", 
                    "was 3.7 per 1,000 total births.",class="externallink"),
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "pnnd") {
    intro_text <- p("Post-neonatal deaths refer to", tags$a(href=link_perinatal, 
                    "deaths occuring after the first 4 weeks but within the first year",class="externallink"), "of life.",
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "nnd") {
    intro_text <- p("Neonatal deaths refer to", tags$a(href=link_perinatal, 
                  "deaths in the first four weeks",class="externallink"), "of life.",
                  peri_common_intro,
                  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                         "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "extperi") {
    intro_text <- p("Extended perinatal deaths refer to", tags$a(href=link_perinatal, 
"the sum of stillbirths and neonatal mortality",class="externallink"), "(deaths within the first 28 days of life).",
peri_common_intro,
tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
       "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
    
  } else if (input$measure_select_perinatal == "infantdeaths") {
    intro_text <- p("Infant deaths refer to", tags$a(href=link_perinatal, 
                    "all deaths in the first year",class="externallink"), "of life; 
                    this includes neonatal and post-neonatal deaths, but not stillbirths. ",
                    peri_common_intro,
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal and postnatal care appointments during the pandemic.")
  }


  # Specify items to display in perinatal ui 
  tagList(
    fluidRow(column(12, 
                    intro_text,
                    p("Stillbirths and infant deaths are (happily) a comparatively rare occurrence in Scotland, 
                      and for this reason we use a particular methodology called ",
                      tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                             'control charts.'),
                      "These charts help us to identify unusual behaviour in data and indicate
                      areas that merit further investigation. Control charts follow a series of rules that help identify patterns 
                      in the data. Read more about the rules used in this chart by clicking the button below: ‘How do we identify patterns in the data?’. "),
                    p("The dots joined by a solid line in this chart show the rates over time for the measure selected.
                      The other lines - centreline, and control and warning limits - are there to help show how 
                      unexpected any observed changes are. The centreline is an average (mean) over the time period. 
                      Control and warning limits take into consideration the expected (random) variation of the data 
                      and help us decide when values are unexpectedly low or high and require further investigation."),
                    h4(paste0(perinatal_title)))),
    actionButton("btn_perinatal_rules", "How do we identify patterns in the data?", 
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("perinatal_chart"))))
  
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
                                  input$measure_select_perinatal ==   "pnnd" ~ "Post neonatal deaths",
                                  input$measure_select_perinatal ==    "nnd" ~ "Neonatal deaths",
                                  input$measure_select_perinatal ==   "extperi" ~ "Extended perinatal deaths",
                                  input$measure_select_perinatal ==    "infantdeaths" ~ "Infant deaths")
    
    tooltip_trend <- c(paste0(measure_selected, "<br>", 
                              format(trend_data$date, "%B %y"), "<br>", 
                              "Rate: ", round(trend_data$rate, 1)))
    
    plot_ly(data = trend_data, x = ~date) %>%
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
## Commentary ----
###############################################.
output$perinatal_commentary <- renderUI({
  tagList(h2("Perinatal mortality - 1st July 2020"),
          p("Placeholder")
            )
})

##END


