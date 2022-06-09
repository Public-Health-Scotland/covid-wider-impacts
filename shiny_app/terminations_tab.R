##Server script for terminations tab..

# Pop-up modal explaining source of data
observeEvent(input$btn_top_modal,
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
observeEvent(input$btn_top_rules,
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

# Modal to explain SIMD and deprivation
# Link action button click to modal launch
observeEvent(input$btn_modal_simd_top, { showModal(
  modalDialog(
    h5("What is SIMD and deprivation?"),
    p("Women have been allocated to different levels of deprivation based on the small area (data zone)
      in which they live and the", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD) (external website).",
                                          class="externallink"), "score for that area.
      SIMD scores are based on data for local areas reflecting 38 indicators across 7 domains:
      income; employment; health; education, skills and training; housing; geographic access; and crime.
      In this tool we have presented results for women living in different SIMD ‘quintiles’.
      To produce quintiles, data zones are ranked by their SIMD score then the areas each containing a fifth (20%)
      of the overall population of Scotland are identified. Women living in the most and least deprived areas
      that each contain a fifth of the population are assigned to SIMD quintile 1 and 5 respectively."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
) })

###############################################.
## ToP Reactive controls  ----
###############################################.

# Pregnancy reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_top <- renderUI({
  #Lists areas available in
  areas_summary_top <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_top])
  selectizeInput("geoname_top", label = NULL, choices = areas_summary_top, selected = "")
})

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset behind trend plot (available at scotland and NHS board level)
top_filter <- function(){

  top %>% filter(area_name == input$geoname_top &
                       area_type == input$geotype_top &
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
## Termination Charts ----
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

  data_last_updated <- tagList(p("Last updated: 1 June 2022"))

  # text for titles of cut charts
  top_subtitle <-  paste0("Figures based on data extracted ",top_extract_date)
  top_trend_title <- paste0("Termination of pregnancy: ",input$geoname_top)
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
            if (input$geotype_top == "Scotland"){
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
    centreline_name <- paste0(input$geoname_top," average up to end Feb 2020")
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

###############################################.
## Commentary tab content  ----
###############################################.

#action associated with action links within commentary text - this observe event linked to an actionLink within the TOP commentary which will take the user from TOP commentary to ANB commentary easily.
observeEvent(input$switch_to_anb,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Antenatal bookings")
})


output$top_commentary <- renderUI({
  tagList(
    bsButton("jump_to_top",label = "Go to data"), #this button can only be used once
    h2("Termination of pregnancy - 2nd June 2021"),
    p("This latest release reports on the number of terminations of pregnancy in Scotland up to February 2021. In this latest month 981 terminations were provisionally notified and numbers may be updated in subsequent releases. This is the lowest monthly figure reported since January 2018: Previous years have reported 1,093 in February 2018; 1,174 in February 2019; 1,230 in February 2020. In February 2021 two mainland boards notified their lowest number of terminations since January 2018: Grampian (82) and Lothian (176). Average gestation at termination in Scotland for February 2021 (6.9 weeks) remained similar to that reported in recent months and ranged between 6 weeks in Grampian to 8.3 weeks in Fife."),
    p("The shift of average gestations below the centreline (average gestation between January 2018 to February 2020) started in March 2020. To a greater or lesser extent this shift has been mirrored across all the mainland boards except in Ayrshire and Arran, Borders and Fife. Also of note, the average gestation in Highland has been trending upwards (but remains below board average), and the lowest average gestations since January 2018 were reported in this release in Ayrshire and Arran (6.7 weeks), Dumfries and Galloway (6.5 weeks) and Grampian (6.0 weeks)."),
    p("There continued to be little variation in average gestation by either age group or deprivation category in February 2021. The drop in numbers (referred to above) in February 2021 was reflected across all age groups and deprivation categories. The range in average gestation by age group was 6.8 weeks (25 to 29 and 40 and over) to 7.2 weeks (35 to 39). For deprivation category the range was from 6.8 week (SIMD 3 and 4) to 7.1 weeks (SIMD 1 - most deprived)."),
    h2("Termination of pregnancy - 5th May 2021"),
    p("This latest release reports on the number of terminations of pregnancy in Scotland up to January 2021. In this latest month 1,108 terminations were provisionally notified, which is comparable with the January average for 2018, 2019 and 2020 of 1,191."),
    p("Observing these figures are provisional and may be updated in subsequent reports, we note that Dumfries and Galloway’s terminations for January 2021 are not shown this month as their total numbers fell below the threshold that we can safely report numbers of terminations without potentially compromising patient confidentiality. Ayrshire and Arran notified their lowest number of terminations (42) since January 2018. We noted in the last release a downward trend in Tayside, which reversed in January 2021."),
    p("Average gestation at termination in Scotland for January 2021 (6.8 weeks) remained similar to that reported in recent months. It is the eleventh consecutive month where the average gestation was below the Scotland average gestation (to end February 2020). The average gestation ranged from 6.1 weeks in Grampian (the lowest notified by Grampian since January 2018) to 8 weeks in Highland."),
    p("As in previous releases, we see little variation in average gestation by deprivation (most deprived - 7 weeks and least deprived - 6.6 weeks). The average gestation was slightly higher in the under 20 age group (7.3 weeks) and the 40 and over age group (7.1 weeks) compared with 6.6 weeks in the 25 to 29 age group."),
    h2("Termination of pregnancy - 7th April 2021"),
    p("This latest release reports on the number of terminations of pregnancy in Scotland up to December 2020. In this latest month 1,053 terminations were provisionally notified, which is comparable with the same months in 2019 and 2018 (1,076 and 1,092 respectively)."),
    p("We reported in last month’s release that terminations in Lothian had been steadily rising, reaching a 7-month high in November 2020 (248 terminations). This trend reversed in December 2020 with notifications dropping to 193. This is close to the number of terminations notified in December 2019 (192). We also reported that Forth Valley recorded their lowest number of terminations (44) since January 2018. This increased to 76 in December 2020 and was the highest number of terminations recorded by Forth Valley since October 2019. We note a downward trend in numbers of terminations reported by Tayside from 107 in August 2020 to 79 in December 2020."),
    p("Average gestation at termination in Scotland for December (6.7 weeks) remained similar to that reported in recent months. The average gestational range in mainland Boards in December was between 6.3 weeks (Lothian and Greater Glasgow & Clyde) and 7.9 weeks (Highland). All areas remained under their Board averages except Borders. This is probably a reflection of the small numbers of terminations carried out by this Board. "),
    p("Latest data also continues to show little variation in average gestation by age or deprivation."),
    h2("Termination of pregnancy - 3rd March 2021"),
    p("This latest release reports on the number of terminations of pregnancy in Scotland up to November 2020. In this latest month 1,060 terminations were provisionally notified and although this is below the Scotland average, it is comparable with the number of terminations reported in November 2019 (1,068) and November 2018 (1,111)."),
    p("There was some variation in numbers reported across the Boards. Of note, Lothian reported a seven month high in November (245 terminations), above the Board average of 218. For the first time since March 2020, Ayrshire and Arran also reported terminations above the board average, the third consecutive monthly increase in the number of terminations. In contrast, Forth Valley recorded their lowest number of terminations since January 2018.  Dumfries and Galloway’s terminations for November 2020 are not shown this month as their total numbers fell below the threshold that we can safely report numbers of terminations without potentially compromising patient confidentiality."),
    p("Average gestation at termination in Scotland for November (6.8 weeks) remained similar to that reported in recent months. The pattern varied by Board, but all areas remained under their Board averages except Borders, their variation is probably related to the small numbers of terminations carried out by this Board. The combination of telemedicine (a remote consultation by telephone or video call) and early medical abortion at home (where both drugs are taken at home) continued to affect gestation at termination."),
    p("The numbers of terminations between age groups and between the most and least deprived areas continues to show little clear variation this has remained the case since July 2020. There is even less variation between age groups and between the most and least deprived areas in respect of the average gestation at termination [range 6.7 to 6.9 weeks]."),
    h2("Termination of pregnancy - 3rd February 2021"),
    p("In this latest release of information on terminations of pregnancy up to September 2020, the provisional numbers reported in Scotland showed numbers of terminations rising gradually over the last four months. With numbers of terminations at 1072 in September 2020, since May 2020 the number of terminations in Scotland has remained below the Scotland average from January 2018. The majority of Boards recorded an increase in terminations between August and September 2020."),
    p("Average gestation at termination in Scotland decreased in the period February to August 2020 from 8 weeks to 6.6 weeks. This probably reflects changes in the configuration of termination care services in response to COVID-19 across Scotland. For Scotland as a whole, average gestation of terminations in Scotland has remained below 7 weeks since May 2020.  This is most clearly seen in Health Board returns from NHS Greater Glasgow and Clyde and NHS Tayside. Overall, the variation in gestation at termination between Board areas in September was minor (range 7.7 to 6.4 weeks)."),
    p("The numbers of terminations between age groups and between most and least deprived areas in Scotland have showed little clear variation since July 2020."),
    p("In Scotland in September 2020, there was a slight widening in the gap of average gestation at time of termination between the most and least deprived areas. In the most deprived areas the average gestation was 7 weeks compared to 6.3 weeks in the least deprived areas."),
    p("There was little variation across the six age groups at time of termination (range 6.9 to 6.6 weeks)."),
    h2("Termination of pregnancy - 2nd December 2020"),
    p("In this second release of information on terminations of pregnancy, the provisional numbers of terminations reported in Scotland in August 2020 fell to the lowest level reported since January 2018. This continues the trend of a fall in numbers from May 2020 onwards and is consistent across some but not all Board areas. The decrease in the average gestation at termination (6.6 weeks in the last month) has also continued in Scotland as a whole and any variation between Boards probably reflects minor variation in service provision between Boards."),
    p("The impact of reductions in numbers of terminations in younger women continues to be evident. The observation of an increase in the average gestation reported in women aged over 40 (to 7.5 weeks in August) may simply represent the impact of variation within small numbers of terminations in this age group. Overall, there remains no substantial variation in average gestation at termination by maternal age group or deprivation level."),
    h2("Termination of pregnancy - 28th October 2020"),
    p("Information on the number of terminations of pregnancy carried out in Scotland, and the average gestation (stage of pregnancy) at which they occurred, was included in this tool for the first time on 28 October 2020."),
    p("Termination of pregnancy (also referred to as a therapeutic or induced abortion) is provided under the terms of the Abortion Act 1967 and subsequent regulations.  When a healthcare practitioner provides a termination of pregnancy, there is a legal requirement for them to notify the Chief Medical Officer of the termination within seven days of it taking place.  Public Health Scotland is responsible for the collation of data derived from notifications of terminations of pregnancy on behalf of the Chief Medical Officer.  This notification data has been used in this tool (see Data source button on the dashboard page).  Detailed information on terminations is published each year by Public Health Scotland.  The ",
      tags$a(href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics", "most recent report", class="externallink",target="_blank"),
      " covers the year to December 2019."),
    p("As an essential service, ",
      tags$a(href = "https://www.rcog.org.uk/en/guidelines-research-services/guidelines/coronavirus-abortion/", "care relating to termination of pregnancy has been provided throughout the COVID-19 pandemic (external website)", class="externallink", target="_blank"),
      ".   Termination of pregnancy can be carried out as a medical procedure or, less commonly, a surgical procedure.  Medical terminations involve the woman taking two different medicines 24-48 hours apart to end her pregnancy. Prior to Oct 2017, women having a medical termination were required to attend a clinic or hospital on two occasions to take the first and then, separately, the second medicine. From ",
      tags$a(href = "https://www.sehd.scot.nhs.uk/cmo/CMO(2017)14.pdf", "October 2017 (external website)", class="externallink",target="_blank"),
      ", women requiring an early medical termination (at up to 9 weeks and 6 days gestation) were able to take the second medicine away with them at the end of their first appointment, and subsequently take that at home.  From ",
      tags$a(href = "https://www.sehd.scot.nhs.uk/cmo/CMO(2020)09.pdf", "31 March 2020 (external website)", class="externallink", target="_blank"),
      ", in response to the COVID-19 pandemic, women requiring an early medical termination (at up to 11 weeks and 6 days gestation) have been able to have an initial remote consultation, by telephone or video call, then take both medicines at home."),
    p("Since 31 March 2020, there has been variation between NHS Boards across Scotland in exactly how care relating to termination of pregnancy has been provided.  Almost all Boards have provided some remote consultations.  After their initial consultation, some women have been required to attend for an ultrasound scan (for example to confirm how far along their pregnancy is if there is some doubt about that, or to see if they have an ectopic pregnancy) before medicines are provided.  Once a woman has been confirmed as eligible for an early medical termination, in some areas both sets of medicine have been delivered to the woman’s home, whereas in other areas women have been required to pick up their medicine from a clinic reception.  On 30 September 2020, the Scottish Government issued a ",
      tags$a(href = "http://www.gov.scot/publications/consultation-future-arrangements-early-medical-abortion-home/", "consultation (external website)", class="externallink", target="_blank"),
      " on whether the recent changes extending women’s access to early medical termination at home should be retained after the COVID-19 pandemic.  The consultation will be open until 5 January 2021."),
    p("The data shows that, at all Scotland level, the number of terminations of pregnancy provided month by month remained broadly constant from January 2018 (when the data shown starts) to February 2020 inclusive. The number of terminations was then higher than usual in March and April 2020, before falling to lower than usual levels in May, June, and July 2020 (with July 2020 being the latest month for which data are currently available).  Over March and April 2020, around 500 more terminations than would have been expected based on pre-pandemic average levels were provided in Scotland. This is likely to reflect a higher proportion than usual of women who found they were pregnant at the start of the COVID-19 pandemic in Scotland choosing not to continue with their pregnancy. As discussed in the ",
      actionLink("switch_to_anb","Commentary on the Antenatal booking data"),
   " provided through this tool, it is likely that the lower than usual numbers of termination provided from May 2020 onwards reflects a reduction in the number of women becoming pregnant from April 2020 onwards.  Further analysis is required to accurately examine trends in the number of women becoming pregnant during the COVID-19 pandemic, their subsequent choices to continue with or terminate their pregnancy, and what this means for future trends in the number of births in Scotland."),
    p("Looking at the data for women living in different NHS Board areas across Scotland, the pattern of an increase in the number of terminations of pregnancy in March and April 2020, then a subsequent fall from May 2020 onwards is evident in some but not all areas."),
    p("At all Scotland level, prior to COVID-19, the average gestation at which terminations of pregnancy took place was around 7 and a half weeks of pregnancy. This fell slightly to around 7 weeks in April 2020, then fell further to around 6 and a half weeks in May to July 2020.  This decrease in the average gestation at termination means that the recent reduction seen in the number of terminations is unlikely to be due to women deferring, or being unable to access, termination until later in their pregnancy."),
    p("Looking at the data for women living in different NHS Board areas across Scotland, the pattern of a reduction in average gestation at termination from April 2020 onwards is evident in some but not all areas. This probably reflects the fact that the detail of how termination care was reconfigured in response to COVID-19 varied across Scotland."),
    p("At all Scotland level, the recent reduction in the number of terminations of pregnancy has been more evident in younger (compared to older) women. The reduction has been seen in women living in areas with all levels of deprivation. In general, there is no substantial variation in average gestation at termination by maternal age group or deprivation level. The reduction in average gestation at termination from April 2020 onwards has been seen in women from all age groups and from all deprivation levels.")
    )
})




