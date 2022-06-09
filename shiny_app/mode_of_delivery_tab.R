
##Server script for pregnancy mode of delivery tab..


# Pop-up modal explaining source of data
observeEvent(input$btn_mod_modal,
             showModal(modalDialog(
               title = "What is the data source?",
               p("The data used for the method of delivery page comes from the Scottish Morbidity Record 02 (SMR02) database.  An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care.  From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."),
               p("For the method of delivery page, SMR02 records for episodes of care involving the delivery of a singleton live birth (i.e. one baby, not twins or more) at any gestation have been used.  The charts presented show the total number of singleton live births, and the number and percentage with the different methods of delivery, in each month from January 2018 onwards.  Method of delivery has been categorised as spontaneous vaginal delivery; assisted vaginal delivery (including forceps, ventouse, and vaginal breech deliveries); elective (i.e. planned) caesarean section; and emergency caesarean section.  The month is based on the date the woman was discharged from hospital after delivery.  Data is shown at all Scotland level, and for women living in each mainland NHS Board area.  Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown.  However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
               p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete.  Data for the most recent months should be viewed as provisional.  Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high.  For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland.  In addition, the recording of method of delivery is very complete.  For the period 1 April 2019 to 31 March 2020, method of delivery was recorded on 99.9% of SMR02 records relating to singleton live births."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/births-and-maternity/births-in-scottish-hospitals/", "Births in Scottish Hospitals report",class="externallink",target="_blank"),"."),
               size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_mod_rules,
             showModal(modalDialog(
               title = "How do we identify patterns in the data?",
               p("Run charts use a series of rules to help identify important changes in the data. These are the ones we used for these charts:"),
               tags$ul(tags$li("Shifts: Six or more consecutive data points above or below the centreline. Points on the centreline neither break nor contribute to a shift (marked on chart)."),
                       tags$li("Trends: Five or more consecutive data points which are increasing or decreasing. An observation that is the same as the preceding value does not count towards a trend (marked on chart)."),
                       tags$li("Too many or too few runs: A run is a sequence of one or more consecutive observations on the same side of the centreline. Any observations falling directly on the centreline can be ignored. If there are too many or too few runs (i.e. the median is crossed too many or too few times) that’s a sign of something more than random chance."),
                       tags$li("Astronomical data point: A data point which is distinctly different from the rest. Different people looking at the same graph would be expected to recognise the same data point as astronomical (or not).")),
               p("Further information on these methods of presenting data can be found in the ",
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts', target="_blank"),"."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

#Modal to explain SIMD and deprivation
#Link action button click to modal launch
observeEvent(input$btn_modal_simd_mod, { showModal(
  modalDialog(
    h5("What is SIMD and deprivation?"),
    p("Women have been allocated to different levels of deprivation based on the small area (data zone) in which they live and the",
      tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD).",
             class="externallink"), "SIMD scores are based on data for local areas reflecting 38 indicators across 7 domains: income; employment; health; education, skills and training; housing; geographic access; and crime.
    In this tool we have presented results for women living in different SIMD ‘quintiles’. To produce quintiles, data zones are ranked by their SIMD score then the areas each containing a fifth (20%) of the overall population of Scotland are identified.
    Women living in the most and least deprived areas that each contain a fifth of the population are assigned to SIMD quintile 1 and 5 respectively."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  ))})

###############################################.
## Deliveries Reactive controls  ----
###############################################.

# deliveries reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_mod <- renderUI({
  #Lists areas available in
  areas_summary_mod <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_mod])
  selectizeInput("geoname_mod", label = NULL, choices = areas_summary_mod, selected = "")
})

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
mod_filter <- function(){

  mod_runchart %>% filter(area_name == input$geoname_mod &
                            area_type == input$geotype_mod &
                            type %in% c("Scotland","Health board"))
}

#Dataset 2: behind line charts for age and deprivation (available for scotland only)
mod_linechart_split <- function(split){

  mod_scot  %>% filter(area_name == "Scotland" &
                    area_type == "Scotland" &
                    type==split)
}

#Dataset 3: behind line chart (available at scotland and NHS board level)
mod_linechart_filter <- function(){

  mod_linechart %>% filter(area_name == input$geoname_mod &
                   area_type == input$geotype_mod &
                   type %in% c("Scotland","Health board"))
}

###############################################.
## Mode of delivery Charts ----
###############################################.

# chart outputs for trend runcharts
output$mod_trend_csection_all <- renderPlotly({plot_mod_trend(measure="perc_csection_all", shift = "csection_all_shift", trend = "csection_all_trend")})
output$mod_trend_csection_elec <- renderPlotly({plot_mod_trend(measure="perc_csection_elec", shift = "csection_elec_shift", trend = "csection_elec_trend")})
output$mod_trend_csection_emer <- renderPlotly({plot_mod_trend(measure="perc_csection_emer", shift = "csection_emer_shift", trend = "csection_emer_trend")})

#chart outputs for line charts for NHS board and Scot
output$mod_linechart_number <- renderPlotly({plot_mod_linechart(measure="births")})
output$mod_linechart_percent <- renderPlotly({plot_mod_linechart(measure="percent_births")})

#chart outputs for line charts for Scotland only age and deprivation line charts
output$mod_linechart_age_n <- renderPlotly({plot_mod_split(dataset=mod_linechart_split(split="age"),split="age", measure="csection_all")})
output$mod_linechart_age_p <- renderPlotly({plot_mod_split(dataset=mod_linechart_split(split="age"),split="age", measure="perc_csection_all")})
output$mod_linechart_dep_n <- renderPlotly({plot_mod_split(dataset=mod_linechart_split(split="dep"),split="dep", measure="csection_all")})
output$mod_linechart_dep_p <- renderPlotly({plot_mod_split(dataset=mod_linechart_split(split="dep"),split="dep", measure="perc_csection_all")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$mod_explorer <- renderUI({

  # text for titles of cut charts
  mod_data_timeperiod <-  paste0("Figures based on data extracted ",mod_extract_date)
  mod_title <- paste0("Percentage of singleton live births delivered by caesarean section: ",input$geoname_mod)

  chart_explanation <-
    tagList(p("We have used ",
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p(run_chart_description("Percentage of births by caesarean section",
                                    "the percentage of singleton live births
                                    (at all gestations) that were delivered by
                                    caesarean section in each month from January
                                    2018 onwards",
                                    "the average (median) percentage of births
                                    that were by caesarean section over the
                                    period January 2018 to February 2020
                                    inclusive (the period before the COVID-19
                                    pandemic in Scotland)",
                                    charts_plural = TRUE)))

  # Charts if data is available and one sign for the three charts if not
  if (input$geoname_mod %in% c("NHS Shetland", "NHS Orkney", "NHS Western Isles") ){
    charts_mod <-  tagList(fluidRow(br()),
                            column(12,
                                   br(),
                                   h5("Charts not shown as unstable due to small numbers.
                                      Data for the Island Boards is included in the data download.")))
  } else {
    charts_mod <-  tagList(
      column(4,
             h4("All caesarean sections"),
             withSpinner(plotlyOutput("mod_trend_csection_all",
                                      height = height_run_chart))),
      column(4,
             h4("Elective caesarean sections"),
             withSpinner(plotlyOutput("mod_trend_csection_elec",
                                      height = height_run_chart))),
      column(4,
             h4("Emergency caesarean sections"),
             withSpinner(plotlyOutput("mod_trend_csection_emer",
                                      height = height_run_chart))),
      column(12,
             br(), # spacing
             p(mod_data_timeperiod),
             p(chart_explanation)),
      column(12,
             br(), #spacing
             h4(paste0("Number of singleton live births by method of delivery: ",input$geoname_mod))),
      column(12,
             withSpinner(plotlyOutput("mod_linechart_number")))
    )
  }


  # Function to create common layout to all immunisation charts
  tagList(fluidRow(column(12,
                          h4(mod_title),
                          actionButton("btn_mod_rules", "How do we identify patterns in the data?")),
                   charts_mod,
                   #only if scotland selected display age and deprivation breakdowns
                   if (input$geotype_mod == "Scotland"){
                     tagList(
                       fluidRow(column(12,
                                       h4("Singleton live births delivered by caesarean section by maternal age group: Scotland"))),
                       fluidRow(column(6,
                                       h4("Number of births delivered by caesarean section"),
                                       withSpinner(plotlyOutput("mod_linechart_age_n"))),
                                column(6,
                                       h4("Percentage of births delivered by caesarean section"),
                                       withSpinner(plotlyOutput("mod_linechart_age_p")))),
                       fluidRow(column(12,
                                       br(), # spacing
                                       h4("Singleton live births delivered by caesarean section by maternal deprivation level: Scotland"),
                                       actionButton("btn_modal_simd_mod", "What is SIMD and deprivation?",
                                                    icon = icon('question-circle')))),
                       fluidRow(column(6,
                                       h4("Number of births delivered by caesarean section"),
                                       withSpinner(plotlyOutput("mod_linechart_dep_n"))),
                                column(6,
                                       h4("Percentage of births delivered by caesarean section"),
                                       withSpinner(plotlyOutput("mod_linechart_dep_p"))))
                     )#tagList from if statement
                   }

  ))
})

#############################################.
## Method of delivery chart functions ----
############################################.

## RUNCHART trend chart for monthly c-section percentages : Scotland & NHS Board (except island boards)
## Rather than try and present all the modes of delivery we have opted just to produce a run chart
## showing rates of c-section (by type all, emergency, elective) as these are the modes of deliver that people most want to see

plot_mod_trend <- function(measure, shift, trend){

  plot_data <- mod_filter()

  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50,
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {


    # centrelines
    centreline_name <- paste0(input$geoname_mod," average up to end Feb 2020")
    dottedline_name = "Projected average"

    # format y axis
    measure_name <- "Percentage of births delivered by caesarean (%)"
    yaxis_plots[["range"]] <- c(0, 50)  # forcing range from 0 to 40%
    y_label <- "Percentage of births (%)"

    #switch tooltip according to which measure is provided
    if(measure == "perc_csection_all"){
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_csection_all,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_csection_emer") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_csection_emer,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_csection_elec") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_csection_elec,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed
    }

    # Adjust the column used for median line according to which cut of chart to be shown
    centreline_data <- case_when(measure == "perc_csection_all" ~ plot_data$median_csection_all,
                                 measure == "perc_csection_elec" ~ plot_data$median_csection_elec,
                                 measure == "perc_csection_emer" ~ plot_data$median_csection_emer)
    dottedline_data <- case_when(measure == "perc_csection_all" ~ plot_data$ext_csection_all,
                                 measure == "perc_csection_elec" ~ plot_data$ext_csection_elec,
                                 measure == "perc_csection_emer" ~ plot_data$ext_csection_emer)

    x_dates = "month"

    plot_run_chart(plot_data, measure, measure_name, y_label,
                   x_dates, shift, trend, tooltip_top,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_buffer = 20, width_mode = "narrow")

  }}


#####################################################################################################################.
## LINECHART SCOTLAND: caesarean delivery by age group and deprivation, numbers and percentages - Scotland level only ----
plot_mod_split <- function(dataset, split, measure){

  plot_data <- dataset

  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))

  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Month: ", format(plot_data$month, "%B %Y"),"<br>",
                      "Number: ", plot_data$csection_all, "<br>",
                      "Percentage: ", format(plot_data$perc_csection_all,digits=1,nsmall = 1),"%"))

  # adjust chart y axis according to what is being displayed
  if(measure == "perc_csection_all"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    if(split == "age"){
      yaxis_plots[["range"]] <- c(0, 70)}  # forcing range from 0 to 70% for age group
    if(split == "dep"){
      yaxis_plots[["range"]] <- c(0, 50)}  # forcing range from 0 to 40% for dep
  }
  if(measure == "csection_all"){
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

#####################################################################################################################.
#### LINECHART SCOTLAND & NHS BOARD: Births by mode of delivery numbers and percentages ----

plot_mod_linechart <- function(measure){

plot_data <- mod_linechart_filter()

pallette <- pal_age

# adjust chart y axis according to what is being displayed
if(measure == "percent_births"){
  yaxis_plots[["title"]] <- "Percentage of births (%)"
  plot_data <- plot_data %>%  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
    filter(mode!="All births")
}

if(measure == "births"){
  yaxis_plots[["title"]] <- "Number of births"
  plot_data <- plot_data #%>% #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?

}

# Create tooltip for line chart
tooltip <- c(paste0("Mode of delivery: ", plot_data$mode,"<br>",
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
              color = ~mode, colors = pallette,
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

mod_download_data <- reactive({
  mod_download %>%
    rename(assisted_vaginal = assisted_vaginal_inc_breech, spontaneous = spontaneous_vaginal,
           perc_assisted_vaginal = perc_assisted_vaginal_inc_breech,
           perc_spontaneous = perc_spontaneous_vaginal)
})

output$download_mod_data <- downloadHandler(
  filename ="mode_of_delivery_extract.csv",
  content = function(file) {
    write_csv(mod_download_data(),
              file) }
)

###############################################.
## Commentary tab content  ----
###############################################.

#action associated with action links within commentary text - this observe event linked to an actionLink within the TOP commentary which will take the user from TOP commentary to ANB commentary easily.
observeEvent(input$switch_to_mod,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Method of delivery")
})


output$mod_commentary <- renderUI({
  tagList(
    bsButton("jump_to_mod",label = "Go to data"), #this button can only be used once
    h2("Method of delivery - 1st June 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley and for NHS Fife in February 2022, so the proportion of births that are delivered by caesarean section in February 2022 is likely to change in future releases of the dashboard."),
    h2("Method of delivery - 4th May 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 and January 2022 so the proportion of births that are delivered by caesarean section for these months is likely to change in future releases of the dashboard."),
    h2("Method of delivery - 6th April 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 so the proportion of births that are delivered by elective caesarean section, which appears lower than that for most other NHS Boards, is likely to change in future releases of the dashboard"),
    h2("Method of delivery - 6th October 2021"),
    p("The proportion of all caesarean sections in Scotland has risen and remains higher than the pre-pandemic average for a number of consecutive months with the proportion approaching 40%.
      Over the last five months the proportions of births recorded as caesarean sections in NHS Greater Glasgow & Clyde have been consistently above 40%. We are linking with NHS Greater Glasgow & Clyde to investigate this further."),
    h2("Method of delivery - 1st September 2021"),
    p("In this release of information on method of delivery (1st September 2021) data have been updated to include women discharged after delivery up to and including May 2021. The data for all Scotland show that the proportion of all caesarean sections has risen, they remain higher than the pre-pandemic average in the last 12 consecutive months and are now approaching 40%.  In recent months the proportions recorded for NHS Grampian have been particularly high, approaching 50%. We are linking with NHS Grampian to investigate this further."),
    h2("Method of delivery - 7th July 2021"),
    p("In this release of information on method of delivery (7th July 2021) data have been updated to include women discharged after delivery up to and including March 2021. The data for all Scotland show that the proportion of all caesarean sections has risen, they remain higher than the pre-pandemic average in the last 12 consecutive months and are now approaching 40%."),
    p("A sharp increase was noted for NHS Fife in the most recent month. We are linking with NHS Fife to explore possible reasons for this."),
    h2("Method of delivery - 2nd June 2021"),
    p("In this release of information on method of delivery (2nd June 2021) data have been updated to include women discharged after delivery up to and including February 2021. The data for all Scotland show that the proportion of both emergency caesarean sections and elective caesarean sections have remained higher than the pre-pandemic average in recent months (at least the last 11 consecutive months). However, the shift in the proportion of elective caesarean sections (and of all caesarean sections) predates the COVID-19 pandemic. Including February 2021, NHS Grampian, NHS Greater Glasgow & Clyde and NHS Highland recorded a higher proportion of elective sections than their pre-pandemic average for at least the last 6 consecutive months. In NHS Ayrshire & Arran there has been a sequential month-on-month decrease in the proportion of emergency caesarean sections in the last 5 consecutive months. NHS Dumfries & Galloway, NHS Fife, NHS Grampian, NHS Greater Glasgow & Clyde, and NHS Lothian have continued, in February 2021, to show at least 6 consecutive months with a higher proportion of emergency sections than their pre-pandemic average. Including February 2021, NHS Dumfries & Galloway, NHS Fife, NHS Grampian, NHS Greater Glasgow & Clyde and NHS Lothian have continued to show at least 6 consecutive months with a higher proportion of all caesarean sections than their pre-pandemic average. However there has been a sequential month-on-month decrease in the proportion of all caesarean sections in the last 5 consecutive months in NHS Lothian."),
    h2("Method of delivery - 5th May 2021"),
    p("In this release of information on method of delivery (5th May 2021) data have been updated to include women discharged after delivery up to and including January 2021. The data for all Scotland show that the proportion of both emergency caesarean sections and elective caesarean sections have remained higher than the pre-pandemic average in recent months (at least the last 10 consecutive months). However, the shift in the proportion of elective caesarean sections predates the COVID-19 pandemic. Including January 2021, NHS Grampian and NHS Highland recorded a higher proportion of elective sections than their pre-pandemic average for at least the last 6 consecutive months. NHS Dumfries & Galloway, NHS Greater Glasgow & Clyde and NHS Lothian have continued, in January 2021, to show at least 6 consecutive months with a higher proportion of emergency sections than their pre-pandemic average. In January 2021, NHS Dumfries & Galloway and NHS Lothian have continued to show at least 6 consecutive months with a higher proportion of all caesarean sections than their pre-pandemic average. Including Jan 2021, NHS Greater Glasgow & Clyde have shown 5 consecutive months where a sequential month-on-month decrease in the proportion of all caesarean sections has occurred."),
    h2("Method of delivery - 7th April 2021"),
    p("In this third release of information on method of delivery (7th April 2021) data have been updated to include women discharged after delivery up to and including December 2020. The data for all Scotland show that the proportion of both emergency caesarean sections and elective caesarean sections have remained higher than the pre-pandemic average in recent months (at least the last nine consecutive months). However, the shift in the proportion of elective caesarean sections predates the COVID-19 pandemic. In December 2020, NHS Grampian recorded a higher proportion of elective sections than their pre-pandemic average for the 14th consecutive month. NHS Dumfries & Galloway and NHS Greater Glasgow & Clyde have continued in December 2020 to show at least 6 consecutive months with a higher proportion of emergency sections than their pre-pandemic average. In December 2020, NHS Fife and NHS Lothian have continued to show at least 6 consecutive months with a higher proportion of all caesarean sections than their pre-pandemic average. However, data are thought to be incomplete for NHS Fife for December 2020 so this proportion could change in future releases of the dashboard."),
    h2("Method of delivery - 3rd March 2021"),
    p("In this third release of information on method of delivery (3rd March 2021) data have been updated to include women discharged after delivery up to and including November 2020.  The data for all Scotland show that both the proportion of emergency caesarean sections and elective caesarean sections have remained higher than the pre-pandemic average for a number of consecutive months.  However, the shift in the proportion of elective caesarean sections predates the COVID-19 pandemic.  In November 2020, NHS Ayrshire & Arran and NHS Grampian continue their run of consecutive months showing a higher proportion of elective sections than their pre-pandemic average. NHS Dumfries & Galloway, NHS Greater Glasgow & Clyde and NHS Fife have continued to show consecutive months with a higher proportion of emergency sections than their pre-pandemic average.  However, data are thought to be incomplete for NHS Fife for November 2020 so this proportion could change in future releases of the dashboard."),
    h2("Method of delivery - 3rd February 2021"),
    p("In this second release of information on method of delivery (3rd February 2021) data have been updated to include women discharged after delivery up to and including October 2020.  Both the proportion of emergency caesarean sections and elective caesarean sections in Scotland as a whole have increased over this time. The rise appears to be driven by the increase in elective caesarean proportions with a notable increase in deliveries by elective caesarean from 16.4% to 18.4% between August and October 2020. The data by NHS Board of residence show more varied patterns. An increase in the proportion of elective caesareans is evident in some, but not all, NHS Board areas. For example NHS Greater Glasgow & Clyde (GGC), NHS Tayside and NHS Ayrshire and Arran, have a higher proportion of elective sections, although the shift precedes the pandemic period in GGC and the proportion of emergency sections in GGC is also elevated from April to October 2020. NHS Lothian and NHS Grampian also show an increase in both categories of caesarean section during 2020. The percentage of emergency and all caesarean sections are up in NHS Dumfries & Galloway from May to October 2020.  The percentage of emergency caesarean sections is down in NHS Lanarkshire from February to September 2020.  NHS Fife shows a sharper increase in emergency caesarean section proportions than elective caesareans, however, data are thought to be incomplete for NHS Fife for October 2020 so this proportion is likely to change in future releases of the dashboard."),
    h2("Method of delivery - 16th December 2020"),
    p("Information on method of delivery was included in this tool for the first time on 16 December 2020."),
    p("The ‘",
      tags$a(href="https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/assisted-birth", "method of delivery",class="externallink",target="_blank"),
      "' refers to the way a baby is born.  Different methods of delivery include spontaneous vaginal delivery (a natural birth); assisted vaginal delivery (including vaginal delivery by forceps or ventouse, or vaginal delivery of a breech baby); or a caesarean section (an operation to deliver the baby through a cut in the mother’s abdomen).  A caesarean section can be elective (planned in advance and provided before labour has started) or emergency (unplanned, and usually but not always provided after labour has started)."),
    p("Care for women around the time they are giving birth is an essential, time critical service that cannot be deferred.  As such, it has been provided throughout the COVID-19 pandemic, and maternity staff have not been redeployed to support other services.  The way that some elements of this care are provided has changed in response to COVID-19 however, to minimise the risk of infection and to allow services to continue to provide safe care during times when a high number of staff may be off work, for example due to needing to isolate."),
    p("Guidance issued by the ",
      tags$a(href="https://www.gov.scot/collections/coronavirus-covid-19-guidance/#health,careandsocialwork", "Scottish Government",class="externallink",target="_blank"),
      " and ",
      tags$a(href="https://www.rcog.org.uk/coronavirus-pregnancy", "Royal College of Obstetricians and Gynaecologists",class="externallink",target="_blank"),
      "to maternity services at the height of the first wave of the pandemic noted that:"),
    tags$ul(
      tags$li("It may be necessary for services to temporarily suspend the option for women to deliver at home or in midwife led units, and to concentrate delivery care within obstetric units"),
      tags$li("Additional restrictions on the use of water births were recommended"),
      tags$li("Care pathways for women requiring induction of labour should be amended to ensure the early stages of the induction process were delivered on an outpatient basis wherever possible"),
      tags$li("Services should consider deferring a planned induction of labour or elective caesarean section if a woman was isolating due to having COVID-19, or having been in contact with a case, if it was safe to do so"),
      tags$li("Services should support low risk women in the early latent phase of labour to remain at home wherever possible"),
      tags$li("In general, strict restrictions on visitors for patients in hospital were advised, however women giving birth could still be accompanied by their chosen birth partner")
    ),
    p("The information on method of delivery presented through this tool is taken from hospital discharge records, specifically records relating to the care of women delivering a singleton live birth (i.e. one baby, not twins or more) at any stage of pregnancy.  Further technical information is available through the ‘Data source’ button on the dashboard page."),
    p("The data shows that, at all Scotland level, the percentage of singleton live births delivered by caesarean section (the ‘caesarean section rate’) has gradually increased from January 2018 (when the data shown starts) to end September 2020 (the latest point for which data is currently available).  The increase is particularly seen in the elective caesarean section rate, but is also evident in the emergency caesarean section rate.  The upward trend in the elective and emergency caesarean section rates predates the COVID-19 pandemic, and it has continued during the pandemic. Whilst caesarean section can be a lifesaving operation for mothers and babies, the high and rising caesarean section rate seen in many countries over recent years is a ",
      tags$a(href="https://obgyn.onlinelibrary.wiley.com/doi/full/10.1111/1471-0528.13526", "cause for concern",class="externallink",target="_blank"),
      ". Excessive use of caesarean sections can carry unnecessary risks for mothers and babies."),
    p("Prior to the COVID-19 pandemic, the caesarean section rate was somewhat variable between NHS Board areas of residence.  There is also some variation between areas in how the caesarean section rate has changed around the time of the pandemic, for example the emergency caesarean section rate has increased noticeably for women living in NHS Fife, whereas the elective and emergency caesarean section rates have decreased for women living in NHS Lanarkshire."),
    p("There is a very clear gradient in the caesarean section rate by maternal age, with the rate being lowest among mothers in the youngest (<20 years) age group and highest among mothers in the oldest (40+ years) age group. These patterns have persisted during the COVID-19 pandemic. As women from the least deprived areas of Scotland tend to have their children at older ages than women from more deprived areas, this means that the caesarean section rate tends to be highest among mothers living in the least deprived areas.")
    )
})


##END
