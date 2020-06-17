##Server script for perinatal tab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_perinatal_modal,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p("The information shown for rates of perinatal mortality is taken from the",
                 tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/deaths-background-information/stillbirths-and-infant-deaths",
                        "National Records for Scotland.",class="externallink")),
               p("This NRS page provides information which is specifically about the statistics of
                 perinatal deaths, neonatal deaths, post-neonatal deaths and infant deaths."),
               p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink"),
                 " routinely receives quarterly data extracts from the SMR02 for the purpose of producing and ",
                 (tags$a(href="https://www.isdscotland.org/Health-Topics/Maternity-and-Births/Publications/","publishing",class="externallink")),"
                 information on birth rates."),
               p("Rates based on small numbers are prone to fluctuation. Therefore in boards
                 with smaller numbers of births it is important to consider this when interpreting the rates."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SPC charts rules
observeEvent(input$btn_perinatal_rules,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "How do we identify patterns and trends in the data?",
               p("Statistical process charts follow a series a rules that help identify patterns and trends in the data. 
                 These are the ones we used in this chart:"),
               tags$ul(tags$li("Outliers: Data points outside the limits marked by the control limits."),
                       tags$li("Shifts: Eight or more consecutive data points above or below the centreline."),
                       tags$li("Trends: Six or more consecutive data points which are increasing or decreasing."),
                       tags$li("Outer One – Third: Two out of three consecutive data points which sit between the control and warning limits."),
                       tags$li("Inner One -Third: 15 or more consecutive data points that lie close to the centreline.")),
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
  perinatal_title <- paste0(case_when(input$measure_select_perinatal == "stillbirths" ~ paste0("Rate of stillbirths per 1000 births in Scotland",
                                                                                       input$geoname_perinatal),
                                      input$measure_select_perinatal == "pnnd" ~ paste0("Rate of post-neonatal deaths per 1000 births in Scotland",
                                                                                        input$geoname_perinatal),
                                      input$measure_select_perinatal == "nnd" ~ paste0("Rate of neonatal deaths per 1000 births in Scotland",
                                                                                    input$geoname_perinatal),
                                      input$measure_select_perinatal == "extperi" ~ paste0("Rate of extended perinatal deaths per 1000 births in Scotland",
                                                                                    input$geoname_perinatal),
                                      input$measure_select_perinatal == "infantdeaths" ~ paste0("Rate of infant deaths per 1000 births in Scotland",
                                                                                           input$geoname_perinatal)))
  
  # Link used in intro text
  link_perinatal <- "https://www.healthcareimprovementscotland.org/our_work/reproductive,_maternal__child/reproductive_health/spimmr_2012.aspx"
  
  # Intro paragraph within perinatal tab
  if (input$measure_select_perinatal == "stillbirths") {
    intro_text <- p("Stillbirths refer to", tags$a(href=link_perinatal, 
                    "a child born after the 24th week of pregnancy which did not breathe or show any signs of life.",class="externallink"), 
                    "The stillbirth rate in Scotland in 2018 ", 
                    tags$a(href="https://www.scotpho.org.uk/population-dynamics/pregnancy-births-and-maternity/key-points/", 
                    "was 3.7 per 1,000 total births.",class="externallink"),
                    " It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                    "NHS Scotland and Scottish Government", 
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "pnnd") {
    intro_text <- p("Post-neonatal deaths refer to", tags$a(href=link_perinatal, 
                    "deaths occuring after the first 4 weeks but within the first year",class="externallink"), "of life.",
                    " It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                    "NHS Scotland and Scottish Government", 
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "nnd") {
    intro_text <- p("Neonatal deaths refer to", tags$a(href=link_perinatal, 
                  "deaths in the first four weeks",class="externallink"), "of life.",
                  " It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                  "NHS Scotland and Scottish Government", 
                  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                         "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.")
  } else if (input$measure_select_perinatal == "extperi") {
    intro_text <- p("Extended perinatal deaths refer to", tags$a(href=link_perinatal, 
"the sum of stillbirths and neonatal mortality",class="externallink"), "(deaths within the first 28 days of life).",
" It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
"NHS Scotland and Scottish Government", 
tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
       "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.")
    
  } else if (input$measure_select_perinatal == "infantdeaths") {
    intro_text <- p("Infant deaths refer to", tags$a(href=link_perinatal, 
                    "all deaths in the first year",class="externallink"), "of life.",
                    " It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                    "NHS Scotland and Scottish Government", 
                    tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                           "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.")
  }


  # Specify items to display in perinatal ui 
  tagList(
    fluidRow(column(12, 
                    intro_text,
                    p("Perinatal mortality is a comparatively rare occurrence in Scotland and for 
                      this reason we use a particular methodology called ",
                      tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                             'statistical process charts.'), 
                      "These charts help us to understand if behind the normal fluctuation of these small numbers
                      there are trends and patterns we should pay attention to. Statistical process charts follow a series a rules that help identify patterns 
                      and trends in the data, read more about the ones used in this chart clicking the button below. "),
                    p("This chart shows the trend of rates for the measure selected, but it also incorporates
                      two other sets of lines: control and warning limits. Control and warning limits take in consideration the variation of the data and indicate when
                      values are unexpectedly low or high and require attention, as well as helping to detect trends. "),
                    h4(paste0(perinatal_title)))),
    actionButton("btn_perinatal_rules", "How do we identify patterns and trends in the data?", 
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("perinatal_chart"))))
  
}) #close perinatal_explorer function
  
  ###############################################.
  ## Charts ----
  ###############################################.
  
  #run chart function to generate spc charts
  output$perinatal_chart <- renderPlotly({
    trend_data <- perinatal %>% filter(type == input$measure_select_perinatal)
    
    yaxis_plots[["title"]] <- "Rate per 1000 births"
    xaxis_plots[["title"]] <- "Month"
    
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
                name = "Rate") %>%
      add_lines(y = ~centreline, line = list(color = "#33ccff"),
                 hoverinfo= "none",
                name = "Centreline") %>%
      add_lines(y = ~upper_cl_3_std_dev, line = list(color = "red", dash = "dot"),
                hoverinfo= "none",
                name = "Control limits") %>%
      add_lines(y = ~lower_cl_3_std_dev, line = list(color = "red", dash = "dot"),
                hoverinfo= "none",
                showlegend = FALSE) %>%
      add_lines(y = ~upper_wl_2_std_dev, line = list(color = "blue", dash = "dash"),
                hoverinfo= "none",
                name = "Warning limits") %>%
      add_lines(y = ~lower_wl_2_std_dev, line = list(color = "blue", dash = "dash"),
                hoverinfo= "none",
                showlegend = FALSE) %>%
      # adding outliers
      add_markers(data = trend_data %>% filter(outlier == T), y = ~ rate,
                  marker = list(color = "red", size = 10, symbol = "diamond"), name = "Outliers") %>% 
      # adding shifts
      add_markers(data = trend_data %>% filter(shift == T), y = ~ rate,
                  marker = list(color = "blue", size = 10, symbol = "circle"), name = "Shifts") %>% 
      # adding shifts
      add_markers(data = trend_data %>% filter(trend == T), y = ~ rate,
                  marker = list(color = "green", size = 10, symbol = "square"), name = "Trends") %>% 
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


