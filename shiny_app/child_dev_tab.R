##Server script for child development tab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_childdev_modal,
             showModal(modalDialog(
               title = "What is the data source?",
               p("Data source: CHSP Pre-School"),
               tags$b("Meaningful data"),
               p("TThis refers to records where a value of N (no concerns), C (concern newly suspected), or P (concern previously identified) 
                 has been recorded for all eight developmental domains assessed as part of the 13-15 month and 27-30 month child health reviews. See the ",
               tags$a(href = "https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/early-child-development/15-september-2020/dashboard/",
                      "Early Child Development", target="_blank"), " publication for further details."),
               tags$b("Denominators used in calculations"),
               p("The denominator used in the child development indicators is the total number of reviews, 
                 rather than the number of reviews with meaningful data recorded. This is because it is possible for 
                 children to have a developmental concern identified against one or more developmental domain 
                 without having meaningful data recorded for all domains. Analysis is based on NHS Board of Residence."), 
               p("The 13-15 month review has only been delivered in NHS Greater Glasgow & Clyde (NHS GG&C) from May 2019 onwards, hence no data are shown for this review for NHS GG&C for the period January to April 2019.  "),
               p("For this reason, the pre-pandemic average for Scotland and NHS GG&C (shown as the centreline in the charts) is based on reviews provided in May 2019 to February 2020.  The pre-pandemic average for all other Boards is based on reviews provided in January 2019 to February 2020. The average is calculated as the median value of the period specified."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SPC charts rules
observeEvent(input$btn_childdev_rules,
             showModal(modalDialog(
               title = "How do we identify patterns in the data?",
               p("Run charts use a series of rules to help identify important changes in the data. 
                 These are the ones we used for these charts:"),
               tags$ul(tags$li("Shifts: Six or more consecutive data points above or below the centreline. Points on the centreline neither break nor contribute to a shift (marked on chart)."),
                       tags$li("Trends: Five or more consecutive data points which are increasing or decreasing. An observation that is the same as the preceding value does not count towards a trend (marked on chart)."),
                       tags$li("Too many or too few runs: A run is a sequence of one or more consecutive observations on the same side of the centreline. Any observations falling directly on the centreline can be ignored. If there are too many or too few runs (i.e. the median is crossed too many or too few times) that’s a sign of something more than random chance."),
                       tags$li("Astronomical data point: A data point which is distinctly different from the rest. Different people looking at the same graph would be expected to recognise the same data point as astronomical (or not).")),
               p("Further information on these methods of presenting data can be found in the ",                      
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts', target="_blank"),"."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
output$geoname_childdev_ui <- renderUI({
  
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_childdev])
  
  selectizeInput("geoname_childdev", label = NULL,
                 choices = areas_summary, selected = "")
  
})

###############################################.
##  Reactive datasets  ----
###############################################.
child_dev_filt <- reactive({
  
  review_chosen <- case_when( input$measure_select_childdev == "13_15mnth" ~ "13-15 month",
                              input$measure_select_childdev == "27_30mnth" ~ "27-30 month")
  
  child_dev %>% filter(area_name == input$geoname_childdev &
                         area_type == input$geotype_childdev &
                         review == review_chosen)
})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$childdev_explorer <- renderUI({
  
  review_title <- case_when(input$measure_select_childdev == "13_15mnth" ~
                             "13-15 month" ,
                            input$measure_select_childdev == "27_30mnth" ~
                              "27-30 month")
  
  control_chart_commentary <-  p("We have used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf', 
                                                        "‘run charts’",class="externallink", target="_blank"), " to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’", br(),
                                "The dots joined by a solid black line in the chart above show the percentage of children receiving a child health review who had 1 or more developmental concern recorded on their review record.  
                                Data is shown for each month from January 2019 onwards. ", br(),  
                                "The blue line on the chart, the centreline, is there to help show how unexpected any observed changes are. 
                                  The centreline is an average (median) over the time period specified in the legend of the chart.")
  
  tagList(
    fluidRow(column(12, 
                    h4(paste0("Percentage of children with 1 or more developmental concerns recorded at the ",  
                              review_title, " review")))), 
    actionButton("btn_childdev_rules", "How do we identify patterns in the data?", 
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("childdev_no_concerns"))),
    control_chart_commentary,
    fluidRow(column(12, 
                    h4(paste0("Number of ", review_title,  
                              " reviews; reviews with full meaningful data on child development recorded; and children with 1 or more developmental concerns recorded")))),
    fluidRow(withSpinner(plotlyOutput("childdev_no_reviews")))
    )#tagLIst bracket
  
  }) #close perinatal_explorer function
###############################################.
## Charts ----
###############################################.
output$childdev_no_reviews <- renderPlotly({
  
  trend_data <- child_dev_filt() %>% mutate(dummy = 0)
  
    #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Number of reviews"
  
    tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),
                              "<br>", "Number of reviews: ", trend_data$no_reviews,
                              "<br>", "Number of reviews with meaningful data:  ", trend_data$no_meaningful_reviews,
                              "<br>", "Number of children with recorded concerns: ", trend_data$concerns_1_plus))
  
    #Creating time trend plot
    plot_ly(data=trend_data, x=~month_review) %>%
      add_lines(y = ~no_reviews, name = "Number of reviews", 
              line = list(color = "#bf812d"), text=tooltip_trend, hoverinfo="text") %>% 
      add_lines(y = ~no_meaningful_reviews, name = "Number of reviews with meaningful data",
              line = list(color = "#74add1"), text=tooltip_trend, hoverinfo="text") %>% 
      add_lines(y = ~concerns_1_plus, name = "Number of children with developmental concerns",
                line = list(color = "black"), text=tooltip_trend, hoverinfo="text") %>% 
      # Dummy line so Glasgow axis shows from January onwards
      add_lines(y = ~dummy, line = list(color = "white"), showlegend = F, hoverinfo = "skip") %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
    
  }
})

output$childdev_no_concerns <- renderPlotly({
  trend_data <- child_dev_filt()
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Percentage of all reviews"
  yaxis_plots[["range"]] <- c(0, 43)  # forcing range from 0 to 100%
  xaxis_plots[["range"]] <- c(min(trend_data$month_review), max(trend_data$month_review))
  
  tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                            "<br>", "% children with developmental concerns: ", trend_data$pc_1_plus, "%"))
  
  average_title <- case_when(input$geoname_childdev %in% c("Scotland", "NHS Greater Glasgow & Clyde") & 
                               input$measure_select_childdev == "13_15mnth" ~ "Average from May 19 to February 20",
                             T ~ "Average from January 19 to February 20")
  
  #Creating time trend plot
  run_plot <- plot_ly(data=trend_data, x=~month_review) %>%
    add_lines( y = ~pc_1_plus,  
              line = list(color = "black"), text=tooltip_trend, hoverinfo="text",
              marker = list(color = "black"), name = "% children with developmental concerns")
  
  # Dotted line for projected tails of centreline. It changes depending on area.
  if (input$geoname_childdev %in% c("Scotland", "NHS Greater Glasgow & Clyde") & input$measure_select_childdev == "13_15mnth") {
    run_plot %<>%     
      add_lines(data=trend_data %>% filter(as.Date(month_review) < as.Date("2020-03-01") &
                                             as.Date(month_review) >= as.Date("2019-05-01")), 
                                y = ~pc_1_plus_centreline, name = average_title,
                                line = list(color = "blue", dash = "solid"), hoverinfo="none") %>% 
      add_lines(data=trend_data %>% filter(as.Date(month_review) >= as.Date("2020-02-01") |
                                             as.Date(month_review) < as.Date("2019-05-01")), 
                y = ~pc_1_plus_centreline, showlegend = FALSE, 
                line = list(color = "blue", dash = "longdash"), hoverinfo="none")
  } else {
    run_plot %<>%     
      add_lines(data=trend_data %>% filter(as.Date(month_review) < as.Date("2020-03-01")), 
                y = ~pc_1_plus_centreline, name = average_title,
                line = list(color = "blue", dash = "solid"), hoverinfo="none") %>% 
      add_lines(data=trend_data %>% filter(as.Date(month_review) >= as.Date("2020-02-01")), 
                y = ~pc_1_plus_centreline, showlegend = FALSE, 
                line = list(color = "blue", dash = "longdash"), hoverinfo="none")
  }
  
  
 run_plot %>% 
   # adding shifts
   add_markers(data = trend_data %>% filter(shift == T), y = ~ pc_1_plus,
               marker = list(color = "orange", size = 10, symbol = "circle"), name = "Shifts") %>% 
   # adding trends
   add_markers(data = trend_data %>% filter(trend == T), y = ~ pc_1_plus,
               marker = list(color = "green", size = 10, symbol = "square"), name = "Trends") %>%  
   #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,  xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )}
})

###############################################.
## Data downloads ----
###############################################.
childdev_down <- reactive({
  
  review_chosen <- case_when( input$measure_select_childdev == "13_15mnth" ~ "13-15 month",
                              input$measure_select_childdev == "27_30mnth" ~ "27-30 month")
  
  child_dev %>% 
    filter(review == review_chosen) %>% 
    select(-hscp2019_code, -shift, -trend) %>% 
    mutate(month_review = format(month_review, "%b %y")) %>% 
    rename(no_reviews_meaningful_data = no_meaningful_reviews, pc_meaningful_data = pc_meaningful)
})


output$download_childdev_data <- downloadHandler(
  filename ="child_development_extract.csv",
  content = function(file) {
    write_csv(childdev_down(), file) } 
)

###############################################.
## Commentary ----
###############################################.
output$childdev_commentary <- renderUI({
  tagList(
    bsButton("jump_to_childdev",label = "Go to data"), #this button can only be used once
    h2("Child development - 7th April 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 7 April 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record. Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to January 2021, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic. For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development domains was substantially lower in April and May 2020, than the level seen in 2019. Data from more recent months show that this has improved, with full meaningful data recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one developmental concern documented remain similar to the levels observed in 2019, having been much lower in April 2020. This drop was associated with the reduction in complete data recording, and is likely to reflect changes in ascertainment of developmental concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Child development - 3rd March 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 3rd March 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record. Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to December 2020, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic. For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development domains was substantially lower in April and May 2020, than the level seen in 2019. Data from more recent months show that this has improved, with full meaningful data recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one developmental concern documented remain similar to the levels observed in 2019, having been much lower in April 2020. This drop was associated with the reduction in complete data recording, and is likely to reflect changes in ascertainment of developmental concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Child development - 3rd February 2021"),
    p("Information on child development has been updated on 3rd February 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old."),
    p("Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record. Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to November 2020, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic. For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only available through the data download function."),
    p("At Scotland level, the data show that the proportion of children having at least one developmental concern documented has returned towards almost the levels observed in 2019, having been much lower in April 2020. This supports the interpretation that the observed lower levels in April 2020 were most likely to be attributable to changes in ascertainment of developmental concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Child development - 23rd December 2020"),
    p("Information on child development has been updated on 23rd December 2020. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old."),
    p("Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record. Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to September 2020, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic. For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards. It should also be noted that NHS Greater Glasgow & Clyde have a backlog of 13-15 month and 27-30 month reviews to be entered into CHSP due to staffing shortages, so numbers will increase in subsequent updates."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only available through the data download function."),
    p("At Scotland level, the data show that the proportion of children having at least one developmental concern documented has returned towards the levels observed in 2019, having been much lower in April 2020. This supports the interpretation that the observed lower levels in April 2020 were most likely to be attributable to changes in ascertainment of developmental concerns (either identification or recording). "),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Child development - 4th November 2020"),
    p("Information on child development has been updated on 4th November 2020. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old."),
    p("Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record. Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to August 2020, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic. For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards. Data for NHS Grampian has now been included as their data entry issues have now been resolved."),
    p("Data has been added at Health & Social Care Partnership (HSCP) level, but this is only available through the data download function."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Child development - 30th September 2020"),
    p("Information on child development has been included in this tool for the first time on 30th September 2020. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old."),
    p("Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record.  Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to July 2020, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic.  For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards.  This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards.  ‘Scotland’ also excludes NHS Grampian for the whole time period shown, due to problems with entry of data into the CHSP-PS system in Grampian during the COVID-19 pandemic."),
    p("At Scotland level, the proportion of children recorded at their 13-15 month review as having at least one developmental concern fell in March and April 2020 before returning to pre-pandemic levels by May 2020. A similar, but more pronounced, pattern was seen for the 27-30 month review, with the proportion of children reviewed having at least one developmental concern recorded falling in March and April 2020 before returning to pre-pandemic levels by June 2020. In April 2020, 9% of children undergoing a 27-30 month review had a developmental concern identified compared to the pre-pandemic average of 16%."),
    p("The proportion of children receiving their 13-15 month or 27-30 month review (and having their review record entered into the CHSP-PS electronic system) is usually high (around 90% if sufficient follow up time allowed).  Delivery of these reviews was inevitably disrupted at the start of the COVID-19 pandemic, as shown by the dip in the number of reviews delivered in March to May 2020.  The number of reviews delivered per month recovered to pre-pandemic levels in June 2020.  Further information on review coverage is provided on the Child Health Reviews page of this tool."),
    p("When the number of 13-15 month and 27-30 month reviews delivered fell at the start of the COVID-19 pandemic, 
      so did the proportion that had full meaningful data recorded against each of the eight developmental domains 
      assessed during reviews. In April 2020, 77% of 13-15 month review records and 73% of 27-30 month review records 
      had full meaningful data on child development recorded, compared to previous averages of around 90%. ",
      tags$a(href ="https://www.gov.scot/publications/coronavirus-covid-19-nursing-and-community-health-staff-guidance/", "National guidance", target="_blank"), 
      " issued at the start of the pandemic recommended that the 13-15 month and 27-30 month reviews should be conducted remotely (by phone or video consultation) where feasible.  The fall in data completeness relating to child development on review records may therefore reflect difficulties in completing a full developmental assessment without face to face contact."),
    p("Given this, it is likely that the dip in March to May 2020 in the proportion of children undergoing 13-15 month and 27-30 month reviews who were identified as having a developmental concern reflects a fall in the ascertainment of developmental problems, rather than a genuine fall in the proportion of children with developmental delay."),
    p("The combined impact of fewer children having reviews, and a lower proportion of those reviewed having developmental concerns identified, means that across Scotland during the period March to July 2020 around 300 children fewer than would have been expected based on pre-pandemic levels were identified as having a developmental concern at 13-15 months, and 800 children fewer were identified as having a developmental concern at 27-30 months.  It is not currently clear to what extent these ‘missing’ children may be identified in coming months, either through ‘catch up’ reviews provided later than usual, or through their parents proactively raising concerns about their development with relevant services such as their Health Visitor, GP, or early learning and childcare staff.")
  ) #tagLIst bracket
})


##END