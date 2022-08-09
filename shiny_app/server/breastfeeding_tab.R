# Wider impacts dashboard - Child health tab - breastfeeding section
# Server code

###############################################.
## Modal ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$`bf-source-modal`,
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
observeEvent(input$btn_breastfed_rules, runchart_modal())


###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("bf")

###############################################.
## Reactive data ----
###############################################.
# Reactive breastfeeding dataset
breastfeeding_filt <- reactive({

  breastfeeding %>% filter(area_type == input$`bf-geotype` &
                             area_name == input$`bf-geoname` &
                             review == input$`bf-measure`)
})

###############################################.
## Reactive layout  ----
###############################################.

# Breastfeeding explorer
output$breastfeeding_explorer <- renderUI({

  review_title <- case_when(input$`bf-measure` == "6-8 week" ~ "6-8 week reviews; reviews",
                            T ~ "health visitor first visits; visits")

  if (input$`bf-measure` == "First visit") {
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

  } else if (input$`bf-measure` == "6-8 week") {
    run_charts_bf <- tagList(
      fluidRow(
        column(6,
               h4(paste0("Percentage of children recorded as exclusively breastfed at the ", tolower(input$`bf-measure`), " review")),
               div(actionButton("btn_breastfed_rules", "How do we identify patterns in the data?",
                            icon = icon('question-circle')), style = "height:40px;"),
               withSpinner(plotlyOutput("bf_excl_pc",
                                        height = height_run_chart))),
        column(6,
               h4(paste0("Percentage of children recorded as overall breastfed at the ", tolower(input$`bf-measure`), " review")),
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

    if (input$`bf-measure` == "First visit") {
    tooltip_trend <- c(paste0("Month: ", format(trend_data$month_review, "%B %y"),
                              "<br>", "Number of reviews: ", trend_data$no_reviews,
                              "<br>", "Number of reviews with infant feeding data recorded: ", trend_data$no_valid_reviews, " (", trend_data$pc_valid, "%)",
                              "<br>", "Number of children exclusively breastfed: ", trend_data$exclusive_bf,
                              " (", trend_data$pc_excl, "%)",
                              "<br>", "Number of children overall breastfed: ", trend_data$overall_bf,
                              " (", trend_data$pc_overall, "%)",
                              "<br>", "Number of children ever breastfed: ", trend_data$ever_bf,
                              " (", trend_data$pc_ever, "%)"))
    } else if (input$`bf-measure` == "6-8 week") {
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
## Data downloads ----
###############################################.
breast_down <- reactive({
  breastfeeding %>% filter(review == input$`bf-measure`) %>%
    select(-hscp2019_code,
           -shift_excl, -trend_excl,
           -shift_ever, -trend_ever,
           -shift_over, -trend_over,
           -ends_with(".split")) %>%
    mutate(month_review = format(month_review, "%b %y")) %>%
    rename(no_reviews_with_data = no_valid_reviews, pc_with_data = pc_valid,
           pc_with_data_centreline = pc_valid_centreline)
})


output$`bf-download-data` <- downloadHandler(
  filename ="breastfeeding_extract.csv",
  content = function(file) {
    write_csv(breast_down(), file) }
)


#END

