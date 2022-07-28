
##Server script for pregnancy gestation at delivery tab..


# Pop-up modal explaining source of data
observeEvent(input$btn_gest_modal,
             showModal(modalDialog(
               title = "What is the data source?",
               p("The data used for the gestation at delivery page comes from the Scottish Morbidity Record 02 (SMR02) database.  An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care.  From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."),
               p("For the gestation at delivery page, SMR02 records for episodes of care involving the delivery of a singleton live birth (i.e. one baby, not twins or more) with known gestation (at 18-44 weeks inclusive) have been used.  The charts presented show the total number of singleton live births with known gestation, and the number and percentage that were delivered in the different gestation categories, in each month from January 2018 onwards.  Gestation at delivery has been categorised as under 32 weeks (very preterm); 32-36 weeks (moderately preterm); 37-41 weeks (term); and 42 weeks or over (post-term).  The month is based on the date the woman was discharged from hospital after delivery.  Data is shown at all Scotland level, and for women living in each mainland NHS Board area.  Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown.  However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
               p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete.  Data for the most recent months should be viewed as provisional.  Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high.  For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland.  In addition, the recording of gestation at delivery is very complete.  For the period 1 April 2019 to 31 March 2020, gestation was recorded on >99.9% of SMR02 records relating to singleton live births."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href="https://publichealthscotland.scot/publications/births-in-scottish-hospitals", "Births in Scottish Hospitals report",class="externallink",target="_blank"),"."),
               size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_gest_rules, runchart_modal())

#Modal to explain SIMD and deprivation
#Link action button click to modal launch
observeEvent(input$btn_modal_simd_gest, { showModal(
  modalDialog(
    h5("What is SIMD and deprivation?"),
    p("Women have been allocated to different levels of deprivation based on the small area (data zone) in which they live and the",
      tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD) (external website).",
             class="externallink"), "SIMD scores are based on data for local areas reflecting 38 indicators across 7 domains: income; employment; health; education, skills and training; housing; geographic access; and crime.
    In this tool we have presented results for women living in different SIMD ‘quintiles’. To produce quintiles, data zones are ranked by their SIMD score then the areas each containing a fifth (20%) of the overall population of Scotland are identified.
    Women living in the most and least deprived areas that each contain a fifth of the population are assigned to SIMD quintile 1 and 5 respectively."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  ))})


###############################################.
## Gestation at delivery Reactive controls  ----
###############################################.

# deliveries reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_gest <- renderUI({
  #Lists areas available in
  areas_summary_gest <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_gest])
  selectizeInput("geoname_gest", label = NULL, choices = areas_summary_gest, selected = "")
})

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
gest_filter <- function(){

  gestation_runchart %>% filter(area_name == input$geoname_gest &
                            area_type == input$geotype_gest &
                            type %in% c("Scotland","Health board"))
}

#Dataset 2: behind line charts for age and deprivation (available for scotland only)
gest_linechart_split <- function(split){

  gestation_scot  %>% filter(area_name == "Scotland" &
                         area_type == "Scotland" &
                         type==split)
}

#Dataset 3: behind line chart  (available at scotland and NHS board level)
gest_linechart_filter_one <- function(){

  gestation_linechart %>% filter(area_name == input$geoname_gest &
                             area_type == input$geotype_gest &
                             type %in% c("Scotland","Health board") &
                               gest %in% c("37 to 41 weeks", "All gestations (18-44 weeks)"))
}

gest_linechart_filter_two <- function(){

  gestation_linechart %>% filter(area_name == input$geoname_gest &
                                   area_type == input$geotype_gest &
                                   type %in% c("Scotland","Health board") &
                                   gest %in% c("Under 32 weeks", "32 to 36 weeks", "42 weeks plus"))
}

###############################################.
## Gestation at delivery Charts ----
###############################################.

# chart outputs for trend
output$gest_trend_u32 <- renderPlotly({plot_gest_trend(measure="perc_under32", shift = "gest_under32_shift", trend = "gest_under32_trend")})
output$gest_trend_u37 <- renderPlotly({plot_gest_trend(measure="perc_under37", shift = "gest_under37_shift", trend = "gest_under37_trend")})
output$gest_trend_32_36 <- renderPlotly({plot_gest_trend(measure="perc_32_36", shift = "gest_32_36_shift", trend = "gest_32_36_trend")})
output$gest_trend_42plus <- renderPlotly({plot_gest_trend(measure="perc_42plus", shift = "gest_42plus_shift", trend = "gest_42plus_trend")})

#chart outputs for line charts for NHS board and Scot
output$gest_linechart_number <- renderPlotly({plot_gest_linechart(measure="births")})
output$gest_linechart_percent <- renderPlotly({plot_gest_linechart(measure="percent_births")})

output$gest_linechart_number_37_41_all <- renderPlotly({plot_gest_linechart_one(measure="births")})
output$gest_linechart_number_32_3236_42plus <- renderPlotly({plot_gest_linechart_two(measure="births")})

#chart outputs for line charts for Scotland only age and deprivation line charts
output$gest_linechart_age_n <- renderPlotly({plot_gest_split(dataset=gest_linechart_split(split="age"),split="age", measure="births_under37")})
output$gest_linechart_age_p <- renderPlotly({plot_gest_split(dataset=gest_linechart_split(split="age"),split="age", measure="perc_under37")})
output$gest_linechart_dep_n <- renderPlotly({plot_gest_split(dataset=gest_linechart_split(split="dep"),split="dep", measure="births_under37")})
output$gest_linechart_dep_p <- renderPlotly({plot_gest_split(dataset=gest_linechart_split(split="dep"),split="dep", measure="perc_under37")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$gestation_explorer <- renderUI({

  # text for titles of cut charts
  gest_data_timeperiod <-  paste0("Figures based on data extracted ",gestation_extract_date)
  gest_title <- paste0("Percentage of singleton live births delivered at stated gestation: ",input$geoname_gest)

  chart_explanation <-
    tagList(p("We have used ",
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p(run_chart_description("Percentage of births at stated gestation",
                                    "the percentage of singleton live births
                                    with known gestation (18-44 weeks) that were
                                    delivered at the stated gestation in each
                                    month from January 2018 onwards",
                                    "the average (median) percentage of births
                                    in each gestation group over the period
                                    January 2018 to February 2020 inclusive (the
                                    period before the COVID-19 pandemic in
                                    Scotland)",
                                    charts_plural = TRUE)))

  # Function to create common layout to all immunisation charts
  gest_layout <- function(gest_trend_u32,gest_trend_u37,gest_trend_32_36,gest_trend_42plus,gest_linechart_age_n,gest_linechart_age_p,gest_linechart_dep_n,gest_linechart_dep_p,gest_linechart_number_37_41_all,gest_linechart_number_32_3236_42plus){
    tagList(fluidRow(column(12,
                            h4(gest_title),
                            actionButton("btn_gest_rules", "How do we identify patterns in the data?")),
                     column(6,
                            h4("Under 32 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_u32",
                                                     height = height_run_chart)),
                            h4("32-36 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_32_36",
                                                     height = height_run_chart))),
                     column(6,
                            h4("Under 37 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_u37",
                                                     height = height_run_chart)),
                     #column(4,
                            h4("At or over 42 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_42plus",
                                                     height = height_run_chart))),
                     column(12,
                            br(), # spacing
                            p(gest_data_timeperiod),
                            p(chart_explanation)),
                     column(12,
                            br(), #spacing
                            h4(paste0("Number of singleton live births delivered at stated gestation: ",input$geoname_gest))),
                     column(6,
                            h4("37-41 weeks gestation and all births"),
                            withSpinner(plotlyOutput("gest_linechart_number_37_41_all"))),
                     column(6,
                            h4("Under 32 weeks, 32-36 weeks, and at or over 42 weeks gestation"),
                            withSpinner(plotlyOutput("gest_linechart_number_32_3236_42plus"))),
                     #only if scotland selected display age and deprivation breakdowns
                     if (input$geotype_gest == "Scotland"){
                       tagList(
                         fluidRow(column(12,
                                         h4("Singleton live births delivered at under 37 weeks gestation by maternal age group: Scotland"))),
                         fluidRow(column(6,
                                         h4("Number of births at under 37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_age_n"))),
                                  column(6,
                                         h4("Percentage of births at under 37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_age_p")))),
                         fluidRow(column(12,
                                         br(), # spacing
                                         h4("Singleton live births delivered at under 37 weeks gestation by maternal deprivation level: Scotland"),
                                         actionButton("btn_modal_simd_gest", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         fluidRow(column(6,
                                         h4("Number of births at under 37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_dep_n"))),
                                  column(6,
                                         h4("Percentage of births at under 37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_dep_p"))))
                       )#tagList from if statement
                     }
    ))}


  # #link plot functions to layouts
  gest_layout(gest_trend_u32="gest_trend_u32",
             gest_trend_u37="gest_trend_u37",
             gest_trend_32_36="gest_trend_32_36",
             gest_trend_42plus="gest_trend_42plus",
             gest_linechart_age_n="gest_linechart_age_n",
             gest_linechart_age_p="gest_linechart_age_p",
             gest_linechart_dep_n="gest_linechart_dep_n",
             gest_linechart_dep_p="gest_linechart_dep_p",
             gest_linechart_number_37_41_all="gest_linechart_number_37_41_all",
             gest_linechart_number_32_3236_42plus="gest_linechart_number_32_3236_42plus")
})


#############################################.
## Gestation chart functions ----
############################################.

## RUNCHART:  trend chart for monthly c-section percentages : Scotland & NHS Board (except island boards)
## Rather than try and present all the modes of delivery we have opted just to produce a run chart
## showing rates of c-section (by type all, emergency, elective) as these are the modes of deliver that people most want to see

plot_gest_trend <- function(measure, shift, trend){

  plot_data <- gest_filter()

  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50,
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {

    # chart legend labels
    centreline_name <- paste0(input$geoname_gest," average up to end Feb 2020")
    dottedline_name = "Projected average"

    # format y axis
    measure_name <- "Percentage of births at stated gestation"
    yaxis_plots[["range"]] <- c(0, 15)  # forcing range from 0 to 8%
    y_label <- "Percentage of births (%)"



    #switch tooltip according to which measure is provided
    if(measure == "perc_under32"){
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_under32,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_under37") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_under37,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_32_36") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_32_36,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_42plus") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_42plus,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed
    }


    # Adjust the column used for median line according to which cut of chart to be shown
    centreline_data <- case_when(measure == "perc_under32" ~ plot_data$median_under32,
                                 measure == "perc_under37" ~ plot_data$median_under37,
                                 measure == "perc_32_36" ~ plot_data$median_32_36,
                                 measure == "perc_42plus" ~ plot_data$median_42plus)
    dottedline_data <- case_when(measure == "perc_under32" ~ plot_data$ext_under32,
                                 measure == "perc_under37" ~ plot_data$ext_under37,
                                 measure == "perc_32_36" ~ plot_data$ext_32_36,
                                 measure == "perc_42plus" ~ plot_data$ext_42plus)

    x_dates = "month"

    plot_run_chart(plot_data, measure, measure_name, y_label,
                   x_dates, shift, trend, tooltip_top,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_buffer = 20)


  }}


#####################################################################################################################
## LINECHART SCOTLAND: caesarean delivery by age group and deprivation, numbers and percentages - Scotland level only
plot_gest_split <- function(dataset, split, measure){

  plot_data <- dataset

  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))

  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Month: ", format(plot_data$month, "%B %Y"),"<br>",
                      "Number: ", plot_data$births_under37, "<br>", #number of csections has been removed from dataset
                      "Percentage: ", format(plot_data$perc_under37,digits=1,nsmall = 1),"%"))

  # adjust chart y axis according to what is being displayed
  if(measure == "perc_under37"){
     yaxis_plots[["title"]] <- "Percentage of births (%)"
    if(split == "age"){
      yaxis_plots[["range"]] <- c(0, 20)}  # forcing range from 0 to 20% for age group
    if(split == "dep"){
      yaxis_plots[["range"]] <- c(0, 20)}  # forcing range from 0 to 20% for dep
  }
  if(measure == "births_under37"){
        yaxis_plots[["title"]] <- "Number of births"
  }

  #adjust datasets according to which data split to be displayed
  if(split == "age"){
    plot_data<- plot_data %>%
      mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29","30-34","35-39", "40 and over")))
    pallette <- pal_age}

  if(split == "dep"){
    plot_data <- plot_data %>%
      mutate(category = factor(category, levels = c("1 - most deprived", "2", "3","4", "5 - least deprived")))
    pallette <- pal_depr}

  #Creating trend plot
  plot_ly(data=plot_data, x=~month,  y = ~get(measure)) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category, colors = pallette,
              text= tooltip, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,  xaxis = xaxis_plots,
           legend = list(orientation = 'h')) %>% #position of legend underneath plot
    #leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)

}

#####################################################################################################################
## LINECHART SCOTLAND & NHS BOARD: Births by gestation numbers and percentages

plot_gest_linechart_one <- function(measure){

  plot_data <- gest_linechart_filter_one()

  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(gest = factor(gest, levels = c("37 to 41 weeks", "All gestations (18-44 weeks)")))

  #pick a colour palette to apply
  pallette <- pal_age

  # adjust chart y axis according to what is being displayed
  if(measure == "percent_births"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    plot_data <- plot_data  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?

  }

  if(measure == "births"){
    yaxis_plots[["title"]] <- "Number of births"
    plot_data <- plot_data  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?

  }
  # Create tooltip for line chart
  tooltip <- c(paste0("Gestation at delivery: ", plot_data$gest,"<br>",
                      "Area: ",plot_data$area_name,"<br>",
                      "Month: ",  format(plot_data$month, "%B %Y"),"<br>",
                      "Number of births: ", plot_data$births,"<br>",
                      "Percentage of births: ", format(plot_data$percent_births,digits = 1,nsmall=1),"%"))

  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50,
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {

    #Creating trend plot
    plot_ly(data=plot_data, x=~month,  y = ~get(measure)) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~gest, colors = pallette,
                text= tooltip, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)}
}

#####################################################################################################################
## LINECHART SCOTLAND & NHS BOARD: Births by gestation numbers and percentages

plot_gest_linechart_two <- function(measure){

  plot_data <- gest_linechart_filter_two()

  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(gest = factor(gest, levels = c("Under 32 weeks", "32 to 36 weeks", "42 weeks plus")))

  #pick a colour palette to apply
  pallette <- c('#543005', '#8c510a', '#74add1') # specified to ensure grouping of pre-term deliveries using brown lines

  # adjust chart y axis according to what is being displayed
  if(measure == "percent_births"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    plot_data <- plot_data %>%  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
      filter(gest!="18 to 44 weeks")
  }

  if(measure == "births"){
    yaxis_plots[["title"]] <- "Number of births"
    plot_data <- plot_data %>% #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
      filter(gest!="18 to 44 weeks")
  }
  # Create tooltip for line chart
  tooltip <- c(paste0("Gestation at delivery: ", plot_data$gest,"<br>",
                      "Area: ",plot_data$area_name,"<br>",
                      "Month: ",  format(plot_data$month, "%B %Y"),"<br>",
                      "Number of births: ", plot_data$births,"<br>",
                      "Percentage of births: ", format(plot_data$percent_births,digits = 1,nsmall=1),"%"))

  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50,
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {

    #Creating trend plot
    plot_ly(data=plot_data, x=~month,  y = ~get(measure)) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~gest, colors = pallette,
                text= tooltip, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)}
}

###############################################.
## Data downloads ----
###############################################.

gest_download_data <- reactive({
  gestation_download
})

output$download_gest_data <- downloadHandler(
  filename ="gestation_at_delivery_extract.csv",
  content = function(file) {
    write_csv(gest_download_data(),
              file) }
)

###############################################.
## Commentary tab content  ----
###############################################.

#action associated with action links within commentary text - this observe event linked to an actionLink within the TOP commentary which will take the user from TOP commentary to ANB commentary easily.
observeEvent(input$switch_to_gestation,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Gestation at delivery")
})


output$gestation_commentary <- renderUI({
  tagList(
    bsButton("jump_to_gestation",label = "Go to data"), #this button can only be used once
    h2("Gestation at delivery - 6th July 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley in March 2022 and for NHS Fife in February 2022, so the proportion of births that are delivered pre-term and post-term in these months are likely to change in future releases of the dashboard."),
    h2("Gestation at delivery - 1st June 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley and for NHS Fife in February 2022,  so the proportions of births that are delivered pre-term and post-term in February 2022 are likely to change in future releases of the dashboard."),
    h2("Gestation at delivery - 4th May 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 and January 2022 so the proportions of births that are delivered pre-term and post-term for these months are likely to change in future releases of the dashboard."),
    h2("Gestation at delivery - 6th April 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 so the proportion of births that are preterm, which appears lower than that for most other NHS Boards, is likely to change in future releases of the dashboard."),
    h2("Gestation at Delivery – 2nd March 2022"),
    p("The percentage of singleton live births delivered at or over 42 weeks (‘post-term’) in NHS Grampian has been increasing and has remained above the pre-pandemic average for the last 8 consecutive months, although it should be noted that numbers of post-term births delivered each month are very small.  NHS Grampian have been made aware of the data and PHS will continue to monitor."),
    h2("Gestation at delivery - 1st December 2021"),
    p("NHS Tayside have shown a recent drop in their proportion of births delivered at 32-36 weeks gestation, however, numbers involved are very small and so are likely to fluctuate from month to month. "),
    h2("Gestation at delivery - 3rd November 2021"),
    p("Following 11 months (October 2019 to September 2020) where the percentage of singleton live births in NHS Lothian delivered at or over 42 weeks (‘post-term’) was below the pre-pandemic average of 1.9%, the proportion of post-term births has now been above the pre-pandemic average for 10 consecutive months (October 2020 to July 2021). We are working with the board in order to investigate this further."),
    h2("Gestation at delivery - 2nd June 2021"),
    p("In this release of information on gestation at delivery (2nd June 2021) data have been updated to include women discharged after delivery up to and including February 2021. The data at all Scotland level show that the preterm proportion (the percentage of singleton live births delivered at under 37 weeks gestation) has decreased in February 2021 to a level of 5.5%, although the previous two months were either side of the pre-pandemic average. The data by NHS Board vary. In February 2021, NHS Borders and NHS Ayrshire & Arran have recorded a preterm proportion lower than their pre-pandemic average for at least 6 consecutive months. In the last 5 consecutive months, NHS Fife have shown a sequential month-on-month increasing trend in the proportion of singleton live births delivered at under 32 weeks gestation. Including February 2021, NHS Borders have shown 8 consecutive months where the proportion of births delivered between 32-36 weeks gestation was lower than their pre-pandemic average."),
    p("In February 2021, the percentage of singleton live births in Scotland delivered at or over 42 weeks (‘post-term’) has continued to be below its usual historical level (for a 16th consecutive month). In February 2021, NHS Greater Glasgow & Clyde recorded a lower than average post-term proportion for the 17th consecutive month."),
    h2("Gestation at delivery - 5th May 2021"),
    p("In this release of information on gestation at delivery (5th May 2021) data have been updated to include women discharged after delivery up to and including January 2021. The data at all Scotland level show that the preterm proportion (the percentage of singleton live births delivered at under 37 weeks gestation) in January 2021 is 6.5%, a level similar to the pre-pandemic average. The data by NHS Board vary. In January 2021, NHS Borders has recorded a preterm proportion lower than their pre-pandemic average for the 7th consecutive month albeit based on very small numbers. NHS Fife has recorded unusually high preterm proportions in December 2020 and January 2021. However, data are thought to be incomplete for NHS Fife for these two periods so these proportions could change in future releases of the dashboard. PHS are working with NHS Fife to clarify this situation. Including Jan 21, NHS Tayside have shown 6 consecutive months where the proportion of births delivered between 32-36 weeks gestation was lower than their pre-pandemic average."),
    p("In January 2021, the percentage of singleton live births in Scotland delivered at or over 42 weeks (‘post-term’) has continued to be below its usual historical level (for a 15th consecutive month). In January 2021, NHS Greater Glasgow & Clyde recorded a lower than average post-term proportion for the 16th consecutive month. Including Jan 21, NHS Highland have shown 5 consecutive months where a sequential month-on-month decrease in the proportion of post-term births has occurred."),
    h2("Gestation at delivery - 7th April 2021"),
    p("In this third release of information on gestation at delivery (7th April 2021) data have been updated to include women discharged after delivery up to and including December 2020. The data at all Scotland level show that the preterm proportion (the percentage of singleton live births delivered at under 37 weeks gestation) in December 2020 is at a level very similar to the pre-pandemic average at 7.0%. The data by NHS Board vary.  In December 2020, NHS Borders has recorded a preterm proportion lower than their pre-pandemic average for the sixth consecutive month albeit based on very small numbers. NHS Fife has recorded an unusually high preterm proportion in December 2020. However, data are thought to be incomplete for NHS Fife for December 2020 so this proportion could change in future releases of the dashboard."),
    p("In December 2020, the percentage of singleton live births in Scotland delivered at or over 42 weeks (‘post-term’) has continued to be below its usual historical level (for a 14th consecutive month). In December 2020, NHS Greater Glasgow & Clyde recorded a lower than average post-term proportion for the 15th consecutive month."),
    h2("Gestation at delivery - 3rd March 2021"),
    p("In this third release of information on gestation at delivery (3rd March 2021) data have been updated to include women discharged after delivery up to and including November 2020. The data at all Scotland level show that the preterm proportion (the percentage of singleton live births delivered at under 37 weeks gestation) still remains fractionally below the pre-pandemic average in November 2020. The data by NHS Board vary but there are no notable changes in the preterm proportion for November 2020."),
    p("In November 2020, the percentage of singleton live births in Scotland delivered at or over 42 weeks (‘post-term’) has continued to be below its usual historical level. NHS Greater Glasgow & Clyde have continued their run of consecutive months showing a lower than average post-term proportion."),
    h2("Gestation at delivery - 3rd February 2021"),
    p("In this second release of information on gestation at delivery (3rd February 2021) data have been updated to include women discharged after delivery up to and including October 2020.  The data at all Scotland level show that the preterm proportion (the percentage of singleton live births delivered at under 37 weeks gestation), having been lower than the pre-pandemic average during the period March to July 2020, has increased slightly but still remains fractionally below the pre-pandemic average at 6.7% in October 2020."),
    p("The data by NHS Board of residence show more varied patterns.  NHS Forth Valley has shown quite low proportions of preterm births in recent months compared to their pre-pandemic average with 4.9% in October 2020.  NHS Ayrshire & Arran, NHS Dumfries & Galloway and NHS Lothian also show periods where the percentage of preterm births is lower than the long-term average. However these periods start before the pandemic period. No NHS Boards are showing particularly high preterm numbers. Data are thought to be incomplete for NHS Fife for October 2020 so the proportion of births that are preterm, which is higher than that for most other NHS Boards, is likely to change in future releases of the dashboard."),
    p("The percentage of singleton live births in Scotland delivered at or over 42 weeks ( ‘post-term’) has continued to be fractionally below its usual historical level. This pattern began in November 2019, and no specific change in the post-term proportion has been seen during the COVID-19 pandemic. The pattern at board level is more variable. NHS Greater Glasgow & Clyde has shown a decrease over recent months with the percentage for October 2020 being the lowest in the period shown at 0.7%."),
    h2("Gestation at delivery - 16th December 2020"),
    p("Information on gestation at delivery was included in this tool for the first time on 16 December 2020."),
    p("‘Gestation at delivery’ refers to the number of completed weeks pregnant a woman is when she delivers her baby. Babies are ‘due’ at 40 completed weeks gestation. Those born between 37 and 41 weeks inclusive are considered to be born ‘at term’. Babies born at under 37 weeks (more than three weeks before their due date) are considered ",
      tags$a(href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/after-the-birth/premature-babies", "preterm or premature (external website)",  target="_blank"),
      ", with those born at under 32 weeks considered very preterm and those born at 32 to 36 weeks inclusive considered moderately preterm. Babies born at or over 42 weeks (more than two weeks after their due date) are considered post-term or over-due. Babies born preterm are at increased risk of both short and long term health and developmental problems, with the ",
      tags$a(href = "https://www.tommys.org/pregnancy-information/premature-birth/how-long-do-you-stay-in-hospital-after-birth/gestational-age-and-medical-needs", "risk increasing the earlier a baby is born (external website)",  target="_blank"),
      ". Babies are also at increased risk when pregnancies extend post-term, in particular the ",
      tags$a(href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/getting-ready-for-the-birth/induced-labour", "risk of stillbirth (external website)",  target="_blank"),
      " increases from 42 weeks gestation onwards."),
    p("Care for women and babies around the time they are giving birth/being born is an essential, time critical service that cannot be deferred.  As such, it has been provided throughout the COVID-19 pandemic, and maternity and neonatal staff have not been redeployed to support other services.  The way that some elements of this care are provided has changed in response to COVID-19 however, to minimise the risk of infection and to allow services to continue to provide safe care during times when a high number of staff may be off work, for example due to needing to isolate.  Relevant guidance has been issued by the ",
      tags$a(href = "https://www.gov.scot/collections/coronavirus-covid-19-guidance/#health,careandsocialwork", "Scottish Government (external website)",  target="_blank"),
      ", the ",
      tags$a(href = "https://www.rcog.org.uk/coronavirus-pregnancy", "Royal College of Obstetricians and Gynaecologists (external website)",  target="_blank"),
      ", and the ",
      tags$a(href = "https://www.bapm.org/pages/182-perinatal-covid-19-resources", "British Association for Perinatal Medicine (external website)",  target="_blank"),
      "."),
    p("The current evidence suggests that ",
      tags$a(href = "https://www.birmingham.ac.uk/research/who-collaborating-centre/pregcov/about/publications.aspx", "women with COVID-19 are at increased risk of preterm delivery (external website)",  target="_blank"),
      ". Conversely, several studies (for example from the ",
      tags$a(href = "https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(20)30223-1/fulltext", "Netherlands (external website)",  target="_blank"),
      ", ",
      tags$a(href = "https://www.medrxiv.org/content/10.1101/2020.05.22.20109793v1", "Denmark (external website)",  target="_blank"),
      ",  and ",
      tags$a(href = "https://gh.bmj.com/content/5/9/e003075", "Ireland (external website)",  target="_blank"),
      ") have reported that the overall number or proportion of babies born preterm or with low birthweight fell during the ‘lockdown’ period implemented in response to COVID-19.  The reasons for this finding are currently unclear, but may reflect the combined impact of factors such as a reduction in infections other than COVID-19, improved air quality, and changes to antenatal care, which together more than outweigh any direct effect of COVID-19.",
      p("The information on gestation at delivery presented through this tool is taken from hospital discharge records, specifically records relating to the care of women delivering a singleton live birth (i.e. one baby, not twins or more) at a known gestation (between 18-44 weeks inclusive).  Further technical information is available through the ‘Data source’ button on the dashboard page."),
      p("The data shows that, at all Scotland level, the percentage of singleton live births delivered at under 37 weeks gestation (the ‘preterm rate’) was slightly lower than usual over the period March to July 2020 (at just over 6% compared to the more usual 6.8%).  This was driven by a dip in the percentage of births delivered at 32-36 weeks (the ‘moderately preterm rate’): there has been no change in the percentage of births delivered at under 32 weeks (the ‘very preterm rate’).  The percentage of singleton live births delivered at or over 42 weeks (the ‘post-term rate’) has been fractionally below its usual historical level since November 2019, but no specific change in the post-term rate has been seen during the COVID-19 pandemic."),
      p("Prior to the COVID-19 pandemic, the preterm rate was somewhat variable between NHS Board areas of residence.  There is also some variation between areas in how the preterm rate has changed around the time of the pandemic.  The preterm rate for women living in NHS Ayrshire & Arran and NHS Lothian has shown a particularly pronounced fall during the pandemic.  No area has seen a sustained increase in the preterm rate during the pandemic.  The hospital delivery discharge records returned to Public Health Scotland that are used in this tool are incomplete for NHS Fife for September 2020: this is likely to account for the unusually high very preterm rate seen for women living in NHS Fife in this month specifically.  We expect this unusually high rate to change when more records are received and this page of the dashboard is refreshed."),
      p("The preterm rate tends to be highest among mothers in the youngest (<20 years) and oldest (40+ years) age groups, however differences between age groups are not pronounced.  There is a clear gradient in the preterm rate by deprivation, with the rate being highest among mothers living in the most deprived areas of Scotland.  These patterns have persisted during the COVID-19 pandemic.")
    )
  )
})




#END