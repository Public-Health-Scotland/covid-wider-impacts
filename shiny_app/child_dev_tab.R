##Server script for child development tab..

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_childdev_modal,
             showModal(modalDialog(
               title = "What is the data source?",
               p("Data source: CHSP Pre-School"),
               tags$b("Meaningful data"),
               p("This refers to records where a value of N (no concerns), C (concern newly suspected), or P (concern previously identified)
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
# Modal to explain SIMD and deprivation
child_dev_simd_modal <- modalDialog(
  h5("What is SIMD and deprivation?"),
  p("The", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD)",
                  target="_blank"), "is the Scottish Government's
    official tool for identifying areas in Scotland with concentrations of deprivation
    by incorporating several different aspects of deprivation (multiple-deprivations)
    and combining them into a single index. Concentrations of deprivation are identified
    in SIMD at Data Zone level and can be analysed using this small geographical unit.
    The use of data for such small areas helps to identify 'pockets' (or concentrations)
    of deprivation that may be missed in analyses based on larger areas such as council
    areas. By identifying small areas where there are concentrations of multiple deprivation,
    the SIMD can be used to target policies and resources at the places with the greatest need.
    The SIMD identifies deprived areas, not deprived individuals."),
  p("In this tool we use the concept of quintile, which refers to a fifth of the population.
    For example when we talk about the most deprived quintile, this means the 20% of the population
    living in the most deprived areas."),
  size = "l",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
# Link action button click to modal launch
observeEvent(input$btton_childdev_modal_simd, { showModal(child_dev_simd_modal) })

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

child_dev_depr_filt <- reactive({

  review_chosen <- case_when( input$measure_select_childdev == "13_15mnth" ~ "13-15 month",
                              input$measure_select_childdev == "27_30mnth" ~ "27-30 month")

  child_dev_depr %>% filter(area_name == input$geoname_childdev &
                         simd == input$simd_childdev &
                         review == review_chosen)
})

child_dev_domains_filt <- reactive({
  
  review_chosen <- case_when( input$measure_select_childdev == "13_15mnth" ~ "13-15 month",
                              input$measure_select_childdev == "27_30mnth" ~ "27-30 month")
  
  child_dev_domains %>% filter(area_name == input$geoname_childdev &
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

  control_chart_commentary <-
    tagList(
      p("We have used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                                                        "‘run charts’",class="externallink", target="_blank"), " to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
      p(run_chart_description(NULL,
                              "the percentage of children receiving a child
                              health review who had 1 or more developmental
                              concern recorded on their review record. Data is
                              shown for each month from January 2019 onwards",
                              "the average (median) percentage of children who
                              are recorded as having 1 or more developmental
                              concern over the time period specified in the
                              legend of the chart")))

  tagList(
    fluidRow(column(12,
                    h4(paste0("Percentage of children with 1 or more developmental concerns recorded at the ",
                              review_title, " review")))),
    actionButton("btn_childdev_rules", "How do we identify patterns in the data?",
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("childdev_no_concerns",
                                      height = height_run_chart))),
    br(), #spacing
    control_chart_commentary,
    fluidRow(column(12,
                    h4(paste0("Number of ", review_title,
                              " reviews; reviews with full meaningful data on child development recorded; and children with 1 or more developmental concerns recorded")))),
    fluidRow(withSpinner(plotlyOutput("childdev_no_reviews"))),
    br(), #spacing
    # Only give domain breakdown for Scotland
    if (input$geotype_childdev == "Scotland") {
      tagList(
        h4(paste0("Percentage of ", review_title,
                  " reviews with a new or previous concern recorded by developmental domain")),
      fluidRow(withSpinner(plotlyOutput("childdev_domains"))),
      br(), #spacing
    # Only give SIMD breakdown for Scotland
        h4(paste0("Percentage of children with 1 or more developmental concerns recorded at the ",
                  review_title, " review by SIMD deprivation quintile")),
        fluidRow(
          column(6, selectizeInput("simd_childdev",
                                   "Select SIMD deprivation quintile",
                                   choices =
                                     setNames(1:5, c("1 - most deprived",
                                                     "2", "3", "4",
                                                     "5 - least deprivation")))),
          column(6, actionButton("btton_childdev_modal_simd",
                                 "What is SIMD and deprivation?",
                                 icon = icon('question-circle')))),
        fluidRow(withSpinner(plotlyOutput("childdev_depr",
                                          height = height_run_chart)))

      ) # tagList from if statement
    }
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
    y_label <- "Percentage of all reviews"
    yaxis_plots[["range"]] <- c(0, 43)  # forcing range from 0 to 100%

    tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),
                          "<br>", "% children with developmental concerns: ", trend_data$pc_1_plus, "%"))

    # Dotted line for projected tails of centreline. It changes depending on area.
    if (input$geoname_childdev %in% c("Scotland", "NHS Greater Glasgow & Clyde") &
        input$measure_select_childdev == "13_15mnth") {

      centreline_name = "Average from May 19 to February 20"
      centreline_start = ymd(20190501)
      centreline_end = ymd(20200301)

      centreline_data =
        trend_data %>%
        mutate(across(pc_1_plus_centreline,
                      ~case_when(month_review < centreline_start ~ NA_real_,
                                 month_review > centreline_end ~ NA_real_,
                                 TRUE ~ .x))) %>%
        pull(pc_1_plus_centreline)

      dottedline_data =
        trend_data %>%
        mutate(across(pc_1_plus_centreline,
                      ~case_when((month_review > centreline_start) &
                                  (month_review < centreline_end) ~ NA_real_,
                                 TRUE ~ .x))) %>%
      pull(pc_1_plus_centreline)

    } else {

      centreline_name = "Average from January 19 to February 20"
      centreline_start = ymd(20190101)
      centreline_end = ymd(20200301)

      centreline_data =
        trend_data %>%
        mutate(across(pc_1_plus_centreline,
                      ~case_when(month_review < centreline_start ~ NA_real_,
                                 month_review > centreline_end ~ NA_real_,
                                 TRUE ~ .x))) %>%
        pull(pc_1_plus_centreline)

      dottedline_data =
        trend_data %>%
        mutate(across(pc_1_plus_centreline,
                      ~case_when((month_review > centreline_start) &
                                  (month_review < centreline_end) ~ NA_real_,
                                 TRUE ~ .x))) %>%
      pull(pc_1_plus_centreline)

    }

    dottedline_name = "Projected average"

    measure = "pc_1_plus"
    measure_name = "% children with developmental concerns"
    x_dates = "month_review"

    plot_run_chart(trend_data, measure, measure_name, y_label,
                   x_dates, "shift", "trend", tooltip_trend,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_buffer = 0)
  }

})

output$childdev_depr <- renderPlotly({
  trend_data <- child_dev_depr_filt()

  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {


    #Modifying standard layout
    y_label <- "Percentage of all reviews in quintile"
    yaxis_plots[["range"]] <- c(0, 43)  # forcing range from 0 to 100%

    tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),
                          "<br>", "% children with developmental concerns: ", trend_data$pc_1_plus, "%",
                          "<br>", "Number of reviews: ", trend_data$no_reviews,
                          "<br>", "Number of reviews with a concern: ", trend_data$concerns_1_plus,
                          "<br>", "% meaningful reviews: ", trend_data$pc_meaningful, "%"))

    # Dotted line for projected tails of centreline. It changes depending on area.
    if (input$geoname_childdev %in% c("Scotland", "NHS Greater Glasgow & Clyde") &
        input$measure_select_childdev == "13_15mnth") {

      centreline_name = "Average from May 19 to February 20"
      centreline_start = ymd(20190501)
      centreline_end = ymd(20200301)

      centreline_data =
        trend_data %>%
        mutate(across(pc_1_plus_centreline,
                      ~case_when(month_review < centreline_start ~ NA_real_,
                                 month_review > centreline_end ~ NA_real_,
                                 TRUE ~ .x))) %>%
        pull(pc_1_plus_centreline)

      dottedline_data =
        trend_data %>%
        mutate(across(pc_1_plus_centreline,
                      ~case_when((month_review > centreline_start) &
                                  (month_review < centreline_end) ~ NA_real_,
                                 TRUE ~ .x))) %>%
      pull(pc_1_plus_centreline)

    } else {

      centreline_name = "Average from January 19 to February 20"
      centreline_start = ymd(20190101)
      centreline_end = ymd(20200301)

      centreline_data =
        trend_data %>%
        mutate(across(pc_1_plus_centreline,
                      ~case_when(month_review < centreline_start ~ NA_real_,
                                 month_review > centreline_end ~ NA_real_,
                                 TRUE ~ .x))) %>%
        pull(pc_1_plus_centreline)

      dottedline_data =
        trend_data %>%
        mutate(across(pc_1_plus_centreline,
                      ~case_when((month_review > centreline_start) &
                                  (month_review < centreline_end) ~ NA_real_,
                                 TRUE ~ .x))) %>%
      pull(pc_1_plus_centreline)

    }

    dottedline_name = "Projected average"

    measure = "pc_1_plus"
    measure_name = "% children with developmental concerns"
    x_dates = "month_review"

    plot_run_chart(trend_data, measure, measure_name, y_label,
                   x_dates, "shift", "trend", tooltip_trend,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_buffer = 0)
  }
})


output$childdev_domains <- renderPlotly({
  
  trend_data <- child_dev_domains_filt() %>% mutate(dummy = 0)
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else {
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- "Percentage of children reviewed"
    
    tooltip_1 <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),
                              "<br>", "% with speech, language & communication concern: ", trend_data$slc_perc,
                                "<br>", "Number with speech, language & communication concern: ", trend_data$no_slc))
    tooltip_2 <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),               
                              "<br>", "% with problem solving concern:  ", trend_data$prob_solv_perc,
                                "<br>", "Number with problem solving concern:  ", trend_data$no_prob_solv))
    tooltip_3 <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),     
                              "<br>", "% with gross motor concern ", trend_data$gross_motor_perc,
                                "<br>", "Number with gross motor concern ", trend_data$no_gross_motor))
    tooltip_4 <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),     
                              "<br>", "% with personal/social concern: ", trend_data$per_soc_perc,
                                "<br>", "Number with personal/social concern: ", trend_data$no_per_soc))
    tooltip_5 <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),
                              "<br>", "% with fine motor concern:  ", trend_data$fine_motor_perc,
                                "<br>", "Number with fine motor concern:  ", trend_data$no_fine_motor))
    tooltip_6 <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),
                              "<br>", "% with emotional/behavioural concern ", trend_data$emot_beh_perc,
                                "<br>", "Number with emotional/behavioural concern ", trend_data$no_emot_beh))
    tooltip_7 <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),
                              "<br>", "% with vision concern:  ", trend_data$vision_perc,
                          "<br>", "Number with vision concern:  ", trend_data$no_vision))
    tooltip_8 <- c(paste0("Month: ", format(trend_data$month_review, "%b %y"),
                              "<br>", "% with hearing concern:  ", trend_data$hearing_perc,
                          "<br>", "Number with hearing concern:  ", trend_data$no_hearing))
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~month_review) %>%
      add_lines(y = ~slc_perc, name = "Speech, language & communication",
                line = list(color = "#2d2da1"), text=tooltip_1, hoverinfo="text") %>%
      add_lines(y = ~prob_solv_perc, name = "Problem solving",
                line = list(color = "#9999ff"), text=tooltip_2, hoverinfo="text") %>%
      add_lines(y = ~gross_motor_perc, name = "Gross motor",
                line = list(color = "#8e23a0"), text=tooltip_3, hoverinfo="text") %>%
      add_lines(y = ~per_soc_perc, name = "Personal/Social",
                line = list(color = "#a81141"), text=tooltip_4, hoverinfo="text") %>%
      add_lines(y = ~fine_motor_perc, name = "Fine motor",
                line = list(color = "#e3b419"), text=tooltip_5, hoverinfo="text") %>%
      add_lines(y = ~emot_beh_perc, name = "Emotional/Behavioural",
                line = list(color = "#1d91c0"), text=tooltip_6, hoverinfo="text") %>%
      add_lines(y = ~vision_perc, name = "Vision",
                line = list(color = "#edf8b1"), text=tooltip_7, hoverinfo="text") %>%
      add_lines(y = ~hearing_perc, name = "Hearing",
                line = list(color = "#7fcdbb"), text=tooltip_8, hoverinfo="text") %>%
      
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
    
  }
})


###############################################.
## Data downloads ----
###############################################.
childdev_down <- reactive({

  review_chosen <- case_when( input$measure_select_childdev == "13_15mnth" ~ "13-15 month",
                              input$measure_select_childdev == "27_30mnth" ~ "27-30 month")

  child_dev %>%
    filter(review == review_chosen) %>%
    select(-hscp2019_code, -shift, -trend, -ends_with(".split")) %>%
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
    h2("Child development - 1st December 2021"),
    p("Information on child development has been updated on 1st December 2021 to include information on reviews undertaken up to September 2021. This is based on child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    p("As reported last month, the percentage of children who are reported to have a concern in at least one developmental domain remains above the pre-pandemic centreline for both reviews. In September 2021 11.8% of children reviewed at 13-15 months of age had a concern documented, compared with a pre-pandemic baseline of 9.6%. At 27-30 months 18.7% of children reviewed had a concern documented, compared with a pre-pandemic baseline of 14.6%."),
    p("In this release, information is provided for the first time on the percentage of children with at least one developmental concern, by socioeconomic deprivation (as measured by Scottish Index of Multiple Deprivation (SIMD) of area of residence). ",
      tags$a(href = "https://publichealthscotland.scot/media/6578/2021-04-27-early-child-development-publication-report.pdf", "Annual reporting", target="_blank"), " of data on child development has previously demonstrated that a higher proportion of children living in more deprived areas are identified as having developmental concerns, than those in less deprived areas.
      The data in this release show that, at 27-30 months, an increase in the percentage of children with at least one developmental concern has been observed across all deprivation groups. There remains a steep socioeconomic gradient; in September 2021, 27.4% of children in the most deprived areas had a least one concern, compared with 13.7% in the least deprived areas.
      At 13-15 months the recent changes among deprivation groups are less clear. This is likely, in part, to be due to the adoption, in May 2019, of this review in NHS Greater Glasgow and Clyde, which contains a substantial proportion of children who live in more deprived areas in Scotland."),
    p("The commentary below includes potential reasons for the recent observed changes; PHS will continue to provide monthly monitoring of these data, and a full annual report with more detailed analysis by developmental domain and population group will be published in April 2022."),
    h2("Child development - 3rd November 2021"),
    p("Information on child development has been updated on 3rd November 2021 to include information on reviews undertaken in August 2021. This is based on child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    p("This release shows that there has been a recent increase in the percentage of children reviewed who are reported to have a concern in at least one developmental domain. In August 2021 12.6% of children reviewed at 13-15 months of age had a concern documented, compared with a pre-covid baseline of 9.6%. At 27-30 months 19.3% of children reviewed had a concern documented, compared with a pre-covid baseline of 14.6%. Both measures have been consistently above the expected level since February 2021, suggesting that these findings are less likely to be due to chance variation alone."),
    p("A number of factors may influence these data. They may reflect a true change in the proportion of children who are experiencing developmental delay or disorders in this period. Public Health Scotland is working to understand the impact of the COVID-19 pandemic on younger children and their families, through the COVID-19 early years resilience and impact survey (",
      tags$a(href ="https://publichealthscotland.scot/our-areas-of-work/covid-19/covid-19-data-and-intelligence/covid-19-and-children-research/covid-19-early-years-resilience-and-impact-survey-ceyris/", "CEYRIS", target="_blank"),
      "). The findings from CEYRIS help to identify areas for action to support the health, wellbeing and development of children. In addition, changes in the data may reflect changes in the way child health reviews are undertaken and reported. It appears that such changes may contribute to the findings in some health board areas. However, the sustained nature and consistency of the finding across several areas, in combination with a quite consistent proportion of reviews having full meaningful information recorded, suggests that there are likely to be common contributing factors."),
    p("PHS will continue to provide monthly monitoring of these data, and a full annual report with more detailed analysis by developmental domain and population group will be published in April 2022."),
    h2("Child development - 1st September 2021"),
    p("Information on child development has been updated on 1 September 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Data is shown by month of review from January 2019 to June 2021. Background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    p("Please note that going forward the dashboard will continue to be updated on the first Wednesday of each month, but the commentary will only be updated in the case of exceptions."),
    h2("Child development - 4th August 2021"),
    p("Information on child development has been updated on 4 August 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Data is shown by month of review from January 2019 to May 2021. Background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Child development - 7th July 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 7 July 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record. Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to April 2021, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic. For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development domains was substantially lower in April and May 2020, than the level seen in 2019. Data from more recent months show that this has improved, with full meaningful data recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one developmental concern documented remain similar to pre-pandemic levels, having been much lower in April 2020. This drop was associated with the reduction in complete data recording, and is likely to reflect changes in ascertainment of developmental concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Child development - 2nd June 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 2 June 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record. Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to February 2021, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic. For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development domains was substantially lower in April and May 2020, than the level seen in 2019. Data from more recent months show that this has improved, with full meaningful data recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one developmental concern documented remain similar to pre-pandemic levels, having been much lower in April 2020. This drop was associated with the reduction in complete data recording, and is likely to reflect changes in ascertainment of developmental concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
    h2("Child development - 5th May 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 5 May 2021. This is based on data recorded at child health reviews undertaken by health visiting teams when children are 13-15 months and 27-30 months old. Data is shown on the proportion of children with 1 or more developmental concern recorded on their child health review record. Data is also shown on the overall number of reviews provided, and on the number of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to February 2021, so comparisons can be made for children receiving their reviews before and during the COVID-19 pandemic. For the 13-15 month review specifically, no data is available for the period January to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development domains was substantially lower in April and May 2020, than the level seen in 2019. Data from more recent months show that this has improved, with full meaningful data recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one developmental concern documented remain similar to pre-pandemic levels, having been much lower in April 2020. This drop was associated with the reduction in complete data recording, and is likely to reflect changes in ascertainment of developmental concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary for 30 September 2020 below."),
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