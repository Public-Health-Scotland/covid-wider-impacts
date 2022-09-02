##Server script for terminations tab..

# Pop-up modal explaining source of data
observeEvent(input$`top-source-modal`,
             showModal(modalDialog(#Maternal HEALTH MODAL
               title = "What is the data source?",
               p("These data are derived from the Notifications of Abortion to the Chief Medical Officer for Scotland (CMO) under the Abortion (Scotland) Regulations 1991."),
               p("Public Health Scotland (PHS) is responsible for the collation of data derived from notifications of terminations of pregnancy on behalf of the Chief Medical Officer (CMO) in Scotland. A termination of pregnancy (also referred to as a therapeutic or induced abortion) is carried out under the terms of the Abortion Act 1967, which applies to England, Wales and Scotland. Two doctors must agree that a termination of pregnancy is necessary under at least one of the grounds as specified in the 1991 Regulations. There is a legal requirement to notify the CMO in Scotland of all terminations carried out in Scotland within seven days of the termination of pregnancy."),
               p("Further information is available from the PHS ",
                 tags$a(href="https://www.publichealthscotland.scot/media/7976/2021-05-25-terminations-2020-report.pdf", "annual report on termination of pregnancy up to December 2020.",class="externallink"),
                 "The ",
                 tags$a(href="https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics", "data tables and charts",class="externallink"),
                 "are also available."),
               p("The number of terminations of pregnancy is shown for each month from January 2018 onwards.  Data is shown at all Scotland level and for each mainland NHS Board of residence.  Due to small numbers, data is not shown for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles, however the Island Boards are included in the Scotland total."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_top_rules, runchart_modal())
# Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd_top, simd_modal("Women"))

###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("top") 

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset behind trend plot (available at scotland and NHS board level)
top_filter <- function(){

  top %>% filter(area_name == input$`top-geoname` &
                       area_type == input$`top-geotype` &
                       type %in% c("Scotland","Health board"))
}

#Dataset behind deprivation/age plots (only available at scotland level)
top_filter_split <- function(split){

  top %>% filter(area_name == "Scotland" &
                       area_type == "Scotland" &
                       type==split) %>%
    droplevels()
}

###############################################.
## Charts ----
###############################################.

# chart outputs for trend
output$top_trend_n <- renderPlotly({plot_top_trend(measure="terminations", shift = "shift_top_no", trend = "trend_top_no")})
output$top_trend_g <- renderPlotly({plot_top_trend(measure="av_gest", shift = "shift_top_gest", trend = "trend_top_gest")})

output$top_age_n <- renderPlotly({plot_top_split(dataset=top_filter_split("age"), split="age", measure="top_number")})
output$top_age_g <- renderPlotly({plot_top_split(dataset=top_filter_split("age"), split="age", measure="top_gestation")})

output$top_dep_n <- renderPlotly({plot_top_split(dataset=top_filter_split("dep"), split="dep", measure="top_number")})
output$top_dep_g <- renderPlotly({plot_top_split(dataset=top_filter_split("dep"), split="dep", measure="top_gestation")})


###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$top_explorer <- renderUI({

  data_last_updated <- tagList(p("Last updated: 7 September 2022"))

  # text for titles of cut charts
  top_subtitle <-  paste0("Figures based on data extracted ",top_extract_date)
  top_trend_title <- paste0("Termination of pregnancy: ",input$`top-geoname`)
  top_title_n <-  paste0("Number of terminations of pregnancy")
  top_title_g <-   paste0("Average gestation at termination")
  top_title_g2 <-   paste0("(based on completed weeks of pregnancy)")

  chart_explanation <-
    tagList(p("We have used ",
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p(run_chart_description("Number of terminations of pregnancy",
                                    "the number of terminations of pregnancy in
                                    each month from January 2018 onwards",
                                    "the average (median) number of terminations
                                    of pregnancy over the period January 2018 to
                                    February 2020 inclusive (the period before
                                    the COVID-19 pandemic in Scotland)")),
            p(run_chart_description("Average gestation at termination",
                                    "the average (mean) gestation at which the
                                    terminations of pregnancy occurred (based on
                                    gestation at termination measured in
                                    completed weeks of pregnancy)",
                                    text_mode = "additional")))

  # Function to create common layout to all immunisation charts
  top_layout <- function(plot_trend_n,plot_trend_g, plot_age_n,plot_age_g,plot_dep_n,plot_dep_g){
    tagList(fluidRow(column(12,
                            h4(top_trend_title),
                            actionButton("btn_top_rules", "How do we identify patterns in the data?")),
                     column(6,
                            h4(paste0(top_title_n)), br(),p(" "),
                            #actionButton("btn_top_rules", "How do we identify patterns in the data?"),
                            withSpinner(plotlyOutput("top_trend_n",
                                                     height = height_run_chart))),
                     column(6,
                            h4(paste0(top_title_g)),
                            p(paste0(top_title_g2)),
                            withSpinner(plotlyOutput("top_trend_g",
                                                     height = height_run_chart))),
                     column(12,
                            br(), # spacing
                            p(top_subtitle),
                            p(chart_explanation))),
            #only if scotland selected display age and deprivation breakdowns
            if (input$`top-geotype` == "Scotland"){
              tagList(
                fluidRow(column(12,h4("Terminations of pregnancy, by age group: Scotland"))),
                fluidRow(column(6,
                                h4("Number of terminations of pregnancy"),br(),p(" "),
                                withSpinner(plotlyOutput("top_age_n"))),
                         column(6,
                                h4("Average gestation at termination"),
                                p("(based on completed weeks of pregnancy)"),
                                withSpinner(plotlyOutput("top_age_g")))),
                fluidRow(column(12,h4("Terminations of pregnancy, by deprivation: Scotland"),
                                actionButton("btn_modal_simd_top", "What is SIMD and deprivation?",
                                             icon = icon('question-circle')))),
                fluidRow(column(6,
                                h4("Number of terminations of pregnancy"),br(),p(" "),
                                withSpinner(plotlyOutput("top_dep_n"))),
                         column(6,
                                h4("Average gestation at termination"),
                                p("(based on completed weeks of pregnancy)"),
                                withSpinner(plotlyOutput("top_dep_g"))))
              )#tagList from if statement
            })
  }

  #link plot functions to layouts
  top_layout(plot_trend_n="top_trend_n", plot_trend_g="top_trend_g",
             plot_age_n="top_age_n", plot_age_g="top_age_g",
             plot_dep_n="top_dep_n", plot_dep_g="top_dep_g")
})

#############################################.
## Termination chart functions ----
############################################.

## Trend plot for monthly TOP numbers and average gestation
plot_top_trend <- function(measure, shift, trend){

  plot_data <- top_filter()


  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50,
                text_nodata = "Data not shown due to small numbers.
                               Data for the Island Boards is included in the
                               Scotland total")
  } else {

    # chart legend labels
    centreline_name <- paste0(input$`top-geoname`," average up to end Feb 2020")
    dottedline_name <- "Projected average"

    #switch y-axis according to which measure is selected
    if (measure == "terminations") {
      y_label <- "Number of terminations"
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Number of terminations: ",plot_data$terminations))
      dottedline_data <-  plot_data$dottedline_no
      centreline_data <-  plot_data$centreline_no
      measure_name <- "Number of terminations"
    } else if (measure  == "av_gest") {
      #yaxis_measure <- plot_data$av_gest
      y_label <- "Average gestation at termination"
      yaxis_plots[["range"]] <- c(0, 11.5)  # forcing range from 0 to 10 weeks
      tooltip_top <- c(paste0("Month: ",format(plot_data$month,"%B %Y"),"<br>",
                              "Average gestation at termination: ",format(plot_data$av_gest,digits = 1,nsmall=1)," weeks"))
      dottedline_data <-  plot_data$dottedline_g
      centreline_data <-  plot_data$centreline_g
      measure_name <- "Average gestation"
    }

    x_dates = "month"

    plot_run_chart(plot_data, measure, measure_name, y_label,
               x_dates, shift, trend, tooltip_top,
               xaxis_plots, yaxis_plots, bttn_remove,
               centreline_data, centreline_name,
               dottedline_data, dottedline_name,
               x_buffer = 20)
  }
}


## Trend plot for monthly termination numbers and average gestation at booking split by age group and simd quintile
plot_top_split <- function(dataset, split, measure){

  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))

  #switch y-axis according to which measure is selected
  if(measure == "top_number"){
    yaxis_measure <- dataset$terminations
    yaxis_plots[["title"]] <- "Number of terminations"
    tooltip_top <- c(paste0(tool_tip_split,dataset$category,"<br>",
                            "Month: ",format(dataset$month,"%B %y"),"<br>",
                            "Number of terminations: ",dataset$terminations))

  } else if (measure  == "top_gestation") {
    yaxis_measure <- dataset$av_gest
    yaxis_plots[["title"]] <- "Average gestation at termination"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10 weeks
    tooltip_top <- c(paste0(tool_tip_split,dataset$category,"<br>",
                            "Month: ",format(dataset$month,"%B %y"),"<br>",
                            "Average gestation at termination: ",format(dataset$av_gest,digits = 1,nsmall=1)," weeks"))
  }

  #adjust datasets accordig to which data split to be displayed
  if(split == "age"){
    dataset <- dataset %>%
      mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29","30-34","35-39", "40 and over")))
    pallette <- pal_age}

  if(split == "dep"){
    dataset <- dataset %>%
      mutate(category = factor(category, levels = c("1 - most deprived", "2", "3","4", "5 - least deprived")))
    pallette <- pal_depr}

  #Creating time trend plot
  plot_ly(data=dataset, x=~month, y = ~yaxis_measure) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category,
              colors = pallette,
              text= tooltip_top,
              hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,xaxis = xaxis_plots,
           legend = list(orientation = 'h')) %>% #position of legend underneath plot
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
}

###############################################.
## Data downloads ----
###############################################.

termination_down_data <- reactive({
     top_download
})

output$download_termination_data <- downloadHandler(
  filename ="terminations_extract.csv",
  content = function(file) {
    write_csv(termination_down_data(),
              file) }
)


#END
