# Wider impacts dashboard - Child health tab - Child development section
# Server code

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$`childdev-source-modal`,
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
observeEvent(input$childdev_rules, runchart_modal())

###############################################.
# Modal to explain SIMD and deprivation
observeEvent(input$childdev_modal_simd, simd_modal() )

###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("childdev")

###############################################.
##  Reactive datasets  ----
###############################################.

review_chosen <- reactive({

case_when( input$`childdev-measure` == "13_15mnth" ~ "13-15 month",
           input$`childdev-measure` == "27_30mnth" ~ "27-30 month")
})

child_dev_filt <- reactive({

  child_dev %>% filter(area_name == input$`childdev-geoname` &
                         area_type == input$`childdev-geotype` &
                         review == review_chosen())
})

child_dev_depr_filt <- reactive({

  child_dev_depr %>% filter(area_name == input$`childdev-geoname` &
                         simd == input$simd_childdev &
                         review == review_chosen())
})

child_dev_domains_filt <- reactive({
  
  child_dev_domains %>% filter(area_name == input$`childdev-geoname` &
                                 area_type == input$`childdev-geotype` &
                                 review == review_chosen())
})


###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$childdev_explorer <- renderUI({

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
                              review_chosen(), " review")))),
    actionButton("childdev_rules", "How do we identify patterns in the data?",
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("childdev_no_concerns",
                                      height = height_run_chart))),
    br(), #spacing
    control_chart_commentary,
    fluidRow(column(12,
                    h4(paste0("Number of ", review_chosen(),
                              " reviews; reviews with full meaningful data on child development recorded; and children with 1 or more developmental concerns recorded")))),
    fluidRow(withSpinner(plotlyOutput("childdev_no_reviews"))),
    br(), #spacing
    # Only give domain breakdown for Scotland
    if (input$`childdev-geotype` == "Scotland") {
      tagList(
        h4(paste0("Percentage of ", review_chosen(),
                  " reviews with a new or previous concern recorded by developmental domain")),
      fluidRow(withSpinner(plotlyOutput("childdev_domains"))),
      br(), #spacing
    # Only give SIMD breakdown for Scotland
        h4(paste0("Percentage of children with 1 or more developmental concerns recorded at the ",
                  review_chosen(), " review by SIMD deprivation quintile")),
        fluidRow(
          column(6, selectizeInput("simd_childdev",
                                   "Select SIMD deprivation quintile",
                                   choices =
                                     setNames(1:5, c("1 - most deprived",
                                                     "2", "3", "4",
                                                     "5 - least deprivation")))),
          column(6, actionButton("childdev_modal_simd",
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
    if (input$`childdev-geoname` %in% c("Scotland", "NHS Greater Glasgow & Clyde") &
        input$`childdev-measure` == "13_15mnth") {

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
    if (input$`childdev-geoname` %in% c("Scotland", "NHS Greater Glasgow & Clyde") &
        input$`childdev-measure` == "13_15mnth") {

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
                line = list(color = "#f28650"), text=tooltip_7, hoverinfo="text") %>%
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
  
  child_dev %>%
    filter(review == review_chosen()) %>%
    select(-hscp2019_code, -shift, -trend, -ends_with(".split")) %>%
    mutate(month_review = format(month_review, "%b %y")) %>%
    rename(no_reviews_meaningful_data = no_meaningful_reviews, pc_meaningful_data = pc_meaningful)
})


output$`childdev-download-data` <- downloadHandler(
  filename ="child_development_extract.csv",
  content = function(file) {
    write_csv(childdev_down(), file) }
)


##END