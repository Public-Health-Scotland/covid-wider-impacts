##Server script for breastfeeding tab..

###############################################.
## Modal ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$btn_breastfed_modal,
             showModal(modalDialog(
               title = "What is the data source?",
               p("Data source: CHSP Pre-School"),
               tags$b("Definitions"),
               tags$ul(
                 tags$li("Exclusively breastfed: children recorded as only being fed breastmilk in the previous 24 hour period"),
                 tags$li("Overall breastfed: children recorded as being fed breast and formula milk in the previous 24 hour period"),
                 tags$li("Ever breastfed: Has the child ever been breastfed? This is recorded at the Health Visitor First Visit.")
               ),
               tags$b("Denominators used in calculations"),
               p("The denominator for the breastfeeding indicators is the number of reviews
                 with valid data recorded (i.e. not ‘missing’ or ‘unknown’) for infant feeding status in the previous 24 hour period. Analysis is based on NHS Board of Residence."),
               p("The average is calculated as the median value of the period specified."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SPC charts rules
observeEvent(input$btn_breastfed_rules,
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
# Breastfeeding reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_bf <- renderUI({

  #Lists areas available in
  areas_summary_bf <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_bf])

  selectizeInput("geoname_bf", label = NULL, choices = areas_summary_bf, selected = "")
})

###############################################.
## Reactive data ----
###############################################.
# Reactive breastfeeding dataset
breastfeeding_filt <- reactive({

  breastfeeding %>% filter(area_type == input$geotype_bf &
                             area_name == input$geoname_bf &
                             review == input$measure_select_bf)
})

###############################################.
## Reactive layout  ----
###############################################.

# Breastfeeding explorer
output$breastfeeding_explorer <- renderUI({

  review_title <- case_when(input$measure_select_bf == "6-8 week" ~ "6-8 week reviews; reviews",
                            T ~ "health visitor first visits; visits")

  if (input$measure_select_bf == "First visit") {
    run_charts_bf <- tagList(
      fluidRow(
        column(4,
               h4(paste0("Percentage of children recorded as exclusively breastfed at health visitor first visit")),
               div(actionButton("btn_breastfed_rules", "How do we identify patterns in the data?",
                            icon = icon('question-circle')), style = "height:40px;"),
               withSpinner(plotlyOutput("bf_excl_pc",
                                        height = height_run_chart))),
        column(4,
               h4(paste0("Percentage of children recorded as overall breastfed at health visitor first visit")),
               div(style = "height:40px;"),
               withSpinner(plotlyOutput("bf_over_pc",
                                        height = height_run_chart))),
        column(4,
               h4(paste0("Percentage of children recorded as ever breastfed at health visitor first visit")),
               div(style = "height:40px;"),
               withSpinner(plotlyOutput("bf_ever_pc",
                                        height = height_run_chart))))
    )

  } else if (input$measure_select_bf == "6-8 week") {
    run_charts_bf <- tagList(
      fluidRow(
        column(6,
               h4(paste0("Percentage of children recorded as exclusively breastfed at the ", tolower(input$measure_select_bf), " review")),
               div(actionButton("btn_breastfed_rules", "How do we identify patterns in the data?",
                            icon = icon('question-circle')), style = "height:40px;"),
               withSpinner(plotlyOutput("bf_excl_pc",
                                        height = height_run_chart))),
        column(6,
               h4(paste0("Percentage of children recorded as overall breastfed at the ", tolower(input$measure_select_bf), " review")),
               div(style = "height:40px;"),
               withSpinner(plotlyOutput("bf_over_pc",
                                        height = height_run_chart))))
    )
  }

  control_chart_commentary <-
    tagList(
      p("We have used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                                                       "‘run charts’",class="externallink", target="_blank"), " to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
      p(run_chart_description(NULL,
                              "the percentage of children receiving a child
                              health review who were recorded as being breastfed
                              on their review record. Data is shown for each
                              month from January 2019 onwards",
                              "the average (median) percentage of children who
                              are recorded as breastfed over the time period
                              specified in the legend of each chart",
                              charts_plural = TRUE)))

  tagList(
    run_charts_bf,
    br(), #spacing
    fluidRow(control_chart_commentary),
    fluidRow(
      h4(paste0("Number of ", review_title, " with data on infant feeding recorded; and children recorded as being breastfed")),
      withSpinner(plotlyOutput("bf_types")))
  )
})

###############################################.
## Charts ----
###############################################.

output$bf_types <- renderPlotly({

  trend_data <- breastfeeding_filt()

  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {

    # Modifying standard layout
    yaxis_plots[["title"]] <- "Number of reviews"

    if (input$measure_select_bf == "First visit") {
    tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%B %y"),
                              "<br>", "Number of reviews: ", trend_data$no_reviews,
                              "<br>", "Number of reviews with infant feeding data recorded: ", trend_data$no_valid_reviews, " (", trend_data$pc_valid, "%)",
                              "<br>", "Number of children exclusively breastfed: ", trend_data$exclusive_bf,
                              " (", trend_data$pc_excl, "%)",
                              "<br>", "Number of children overall breastfed: ", trend_data$overall_bf,
                              " (", trend_data$pc_overall, "%)",
                              "<br>", "Number of children ever breastfed: ", trend_data$ever_bf,
                              " (", trend_data$pc_ever, "%)"))
    } else if (input$measure_select_bf == "6-8 week") {
      tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%B %y"),
                                "<br>", "Number of reviews: ", trend_data$no_reviews,
                                "<br>", "Number of reviews with infant feeding data recorded:  ", trend_data$no_valid_reviews, " (", trend_data$pc_valid, "%)",
                                "<br>", "Number of children exclusively breastfed: ", trend_data$exclusive_bf,
                                " (", trend_data$pc_excl, "%)",
                                "<br>", "Number of children overall breastfed: ", trend_data$overall_bf,
                                " (", trend_data$pc_overall, "%)"))
    }

    # Creating time trend plot
    plot_ly(data = trend_data, x = ~month_review) %>%
      add_lines(y = ~no_reviews, line = list(color = "black"), # Number of reviews
                text = tooltip_trend, hoverinfo="text", name = "Number of reviews") %>%
      add_lines(y = ~no_valid_reviews, line = list(color = "#74add1", dash = "dash"),
                text = tooltip_trend, hoverinfo = "text", name = "Number of reviews with infant feeding data recorded") %>%
      add_lines(y = ~ever_bf, line = list(color = "#313695"), #ever breastfed
                text = tooltip_trend, hoverinfo="text",  name = "Number of children ever breastfed") %>%
      add_lines(y = ~overall_bf, line = list(color = "2c7fb8"), #overall breastfed
                text = tooltip_trend, hoverinfo="text", name = "Number of children overall breastfed") %>%
      add_lines(y = ~exclusive_bf, line = list(color = "#bf812d"), # Exclusively breastfed
                text = tooltip_trend, hoverinfo="text", name = "Number of children exclusively breastfed") %>%
      # Layout
      layout(margin = list(b = 80, t=5),
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>%
      # Configure modebar buttons
      config(displaylogo = F, displayModeBar = T, modeBarButtonsToRemove = bttn_remove)
  }
})

plot_runchart_bf <- function(var_chosen, centreline, shift, trend) {
  trend_data <- breastfeeding_filt()

  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {

    #Modifying standard layout
    y_label <- "% of reviews with feeding data"
    yaxis_plots[["range"]] <- c(0, 100)  # forcing range from 0 to 100%

    measure_name <- case_when(var_chosen == "pc_excl" ~ "% exclusively breastfed",
                              var_chosen == "pc_overall" ~ "% overall breastfed",
                              var_chosen == "pc_ever" ~ "% ever breastfed")

    if (var_chosen == "pc_excl") {
      tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                                "<br>", measure_name, ": ", trend_data$pc_excl, "%"))
    } else  if (var_chosen == "pc_overall") {
      tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                                "<br>", measure_name, ": ", trend_data$pc_overall, "%"))
    } else if (var_chosen == "pc_ever") {
      tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%b %y"),
                                "<br>", measure_name, ": ", trend_data$pc_ever, "%"))
    }

    centreline_name = "Average up to February 2020"
    dottedline_name = "Projected average"
    centreline_end = ymd(20200301)
    centreline_data =
      trend_data %>%
      mutate(across(all_of(centreline),
                    ~case_when(month_review > centreline_end ~ NA_real_,
                               TRUE ~ .x))) %>%
      pull(centreline)
    dottedline_data =
            trend_data %>%
            mutate(across(all_of(centreline),
                          ~case_when(month_review < centreline_end ~ NA_real_,
                                     TRUE ~ .x))) %>%
      pull(centreline)

    x_dates = "month_review"

    plot_run_chart(trend_data, var_chosen, measure_name, y_label,
                   x_dates, shift, trend, tooltip_trend,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_buffer = 0)

  }
}

output$bf_excl_pc <- renderPlotly({
  plot_runchart_bf("pc_excl", "pc_excl_centreline", shift = "shift_excl", trend = "trend_excl")
})

output$bf_over_pc <- renderPlotly({
  plot_runchart_bf("pc_overall", "pc_overall_centreline", shift = "shift_over", trend = "trend_over")
})

output$bf_ever_pc <- renderPlotly({
  plot_runchart_bf("pc_ever", "pc_ever_centreline", shift = "shift_ever", trend = "trend_ever")
})

###############################################.
## Commentary ----
###############################################.
output$breastfeeding_commentary <- renderUI({
  tagList(
    bsButton("jump_to_breastfed",label = "Go to data"), #this button can only be used once
    h2("Breastfeeding - 1st September 2021"),
    p("Information on breastfeeding has been updated in this tool on 1 September 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old. Data is shown by month of review from January 2019 to June 2021. Background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    p("Please note that going forward the dashboard will continue to be updated on the first Wednesday of each month, but the commentary will only be updated in the case of exceptions."),
    h2("Breastfeeding - 4th August 2021"),
    p("Information on breastfeeding has been updated in this tool on 4 August 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old. Data is shown by month of review from January 2019 to May 2021. Background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 7th July 2021"),
    p("Information on breastfeeding has been updated in this tool on 7 July 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to April 2021, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information at Health & Social Care Partnership level is available in the data download function."),
    p("At Scotland level, the data show that there was a small increase in the overall proportion of babies recorded as having been breastfed at both the Heath Visitor first visit, and 6-8 week review in the early months of the pandemic, but this now appears to have fallen back to pre-pandemic levels."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 2nd June 2021"),
    p("Information on breastfeeding has been updated in this tool on 2 June 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to March 2021, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information at Health & Social Care Partnership level is available in the data download function."),
    p("At Scotland level, the data show that there was a small increase in the overall proportion of babies recorded as having been breastfed at both the Heath Visitor first visit, and 6-8 week review in the early months of the pandemic, but this now appears to have fallen back to pre-pandemic levels."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 5th May 2021"),
    p("Information on breastfeeding has been updated in this tool on 5 May 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to February 2021, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information at Health & Social Care Partnership level is available in the data download function."),
    p("At Scotland level, the data show that there was a small increase in the overall proportion of babies recorded as having been breastfed at both the Heath Visitor first visit, and 6-8 week review in the early months of the pandemic, but this now appears to have fallen back to pre-pandemic levels."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 7th April 2021"),
    p("Information on breastfeeding has been updated in this tool on 7 April 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to January 2021, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information at Health & Social Care Partnership level is available in the data download function."),
    p("At Scotland level, the data show that there was a small increase in the overall proportion of babies recorded as having been breastfed at both the Heath Visitor first visit, and 6-8 week review in the early months of the pandemic, but this now appears to have fallen back to pre-pandemic levels."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 3rd March 2021"),
    p("Information on breastfeeding has been updated in this tool on 3rd March 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to December 2020, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information is shown at Health & Social Care Partnership level, but this is only available in the data download function."),
    p("At Scotland level, the data show that the small increase in the overall proportion of babies recorded as having been breastfed has been maintained through to December 2020, for both the Heath Visitor first visit, and 6-8 week review."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 3rd February 2021"),
    p("Information on breastfeeding has been updated in this tool on 3rd February 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to November 2021, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information is shown at Health & Social Care Partnership level, but this is only available in the data download function."),
    p("At Scotland level, the data show that the small increase in the overall proportion of babies recorded as having been breastfed has been maintained through to November 2020, for both the Heath Visitor first visit, and 6-8 week review."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 23rd December 2020"),
    p("Information on breastfeeding has been updated in this tool on 23rd December 2020. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to September 2020, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information is shown at Health & Social Care Partnership level, but this is only available in the data download function."),
    p("At Scotland level, the data show that the small increase in the overall proportion of babies recorded as having been breastfed has been maintained through to September 2020, for both the Heath Visitor first visit, and 6-8 week review. "),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 4th November 2020"),
    p("Information on breastfeeding has been updated in this tool on 4th November 2020. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to August 2020, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information is now included for NHS Grampian, as their data recording issues have now been resolved. Information is also shown at Health & Social Care Partnership level, but this is only available in the data download function."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Breastfeeding - 30th September 2020"),
    p("Information on breastfeeding has been included in this tool for the first time on 30th September 2020. This is based on data recorded at child health reviews undertaken by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown on breastfeeding initiation (has the child ever been breastfed), and the child’s breastfeeding status over the 24 hours prior to their child health review (exclusive breastfeeding and overall breastfeeding [includes mixed breast and formula feeding])."),
    p("Data is shown by month of review from January 2019 to July 2020, so comparisons can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("At Scotland level, there was a small increase in the proportion of babies recorded at their HV First Visit as ever having been breastfed, and as still receiving some breastfeeding, in April and May 2020 (babies born March/April/May). For example, 56% of babies having their HV First Visit in April 2020 were recorded as overall breastfed, compared to the pre-pandemic average of 52%.  Similarly, there was a small increase in the proportion of babies recorded at their 6-8 week review as still receiving breastfeeding in May 2020 (babies born March/April).  Breastfeeding rates have returned to previous average levels for babies receiving their HV First Visit and 6-8 week review from June 2020 onwards."),
    p("The proportion of babies receiving a HV First Visit (and having their review record entered into the CHSP-PS electronic system) is usually very high (>95%) and this has been well maintained during the COVID-19 pandemic. The proportion of babies receiving a 6-8 week review is also usually high (>90% if sufficient follow up time allowed) and this has been reasonably well maintained during the COVID-19 pandemic in most, but not all, NHS Board areas.  This can be seen by examining the number of HV First Visits and 6-8 week reviews provided month by month on the Breastfeeding page of this tool, and through the more detailed data provided on review coverage on the Child Health Reviews page."),
    p("This means that the trends seen in the proportion of babies recorded as being breastfed are likely to be real, rather than the result of changes in data recording.  A temporary increase in breastfeeding rates for babies born during the first wave of the COVID-19 pandemic in Scotland may reflect women having more time to initiate and maintain breastfeeding during lockdown due to fewer competing demands on their time, for example through reduced visits from friends and family.")
  ) #tagLIst bracket
})

###############################################.
## Data downloads ----
###############################################.
breast_down <- reactive({
  breastfeeding %>% filter(review == input$measure_select_bf) %>%
    select(-hscp2019_code,
           -shift_excl, -trend_excl,
           -shift_ever, -trend_ever,
           -shift_over, -trend_over,
           -ends_with(".split")) %>%
    mutate(month_review = format(month_review, "%b %y")) %>%
    rename(no_reviews_with_data = no_valid_reviews, pc_with_data = pc_valid,
           pc_with_data_centreline = pc_valid_centreline)
})


output$download_bf_data <- downloadHandler(
  filename ="breastfeeding_extract.csv",
  content = function(file) {
    write_csv(breast_down(), file) }
)


#END

