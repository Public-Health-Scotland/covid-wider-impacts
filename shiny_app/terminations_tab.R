##Server script for terminations tab

# Pop-up modal explaining source of data
observeEvent(input$btn_top_modal, 
             showModal(modalDialog(#Maternal HEALTH MODAL
               title = "What is the data source?",
               p("These data are derived from the Notifications of Abortion to the Chief Medical Officer for Scotland (CMO) under the Abortion (Scotland) Regulations 1991."),
               p("Public Health Scotland (PHS) is responsible for the collation of data derived from notifications of terminations of pregnancy on behalf of the Chief Medical Officer (CMO) in Scotland. A termination of pregnancy (also referred to as a therapeutic or induced abortion) is carried out under the terms of the Abortion Act 1967, which applies to England, Wales and Scotland. Two doctors must agree that a termination of pregnancy is necessary under at least one of the grounds as specified in the 1991 Regulations. There is a legal requirement to notify the CMO in Scotland of all terminations carried out in Scotland within seven days of the termination of pregnancy."),
               p("Further information is available from the PHS ",
                 tags$a(href="https://beta.isdscotland.org/media/5320/2020-08-25-terminations-2019-report.pdf", "annual report on termination of pregnancy up to December 2019.",class="externallink"),
                 "The ",
                 tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/sexual-health/termination-of-pregnancy-statistics/", "data tables and charts",class="externallink"),
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
      in which they live and the", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD).",
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

  data_last_updated <- tagList(p("Last updated: 3rd February 2021"))
  
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
            p("On the ‘Number of terminations of pregnancy’ chart above, the dots joined by a solid black line show the number of terminations of pregnancy in each month from January 2018 onwards.  The solid blue centreline on the chart shows the average (median) number of terminations of pregnancy over the period January 2018 to February 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The dotted blue centreline continues that average to allow determination of whether there has subsequently been a change in the number of terminations of pregnancy."),
            p("The ‘Average gestation at termination’ chart follows a similar format.  In this chart, the dots joined by a solid black line show the average (mean) gestation at which the terminations of pregnancy occurred (based on gestation at termination measured in completed weeks of pregnancy)."))
  
  # Function to create common layout to all immunisation charts
  top_layout <- function(plot_trend_n,plot_trend_g, plot_age_n,plot_age_g,plot_dep_n,plot_dep_g){
    tagList(fluidRow(column(12,
                            h4(top_trend_title),
                            actionButton("btn_top_rules", "How do we identify patterns in the data?")),
                     column(6,
                            h4(paste0(top_title_n)), br(),p(" "),
                            #actionButton("btn_top_rules", "How do we identify patterns in the data?"),
                            withSpinner(plotlyOutput("top_trend_n"))),
                     column(6,
                            h4(paste0(top_title_g)),
                            p(paste0(top_title_g2)),
                            withSpinner(plotlyOutput("top_trend_g"))),
                     column(12,
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
              text_nodata = "Data not shown due to small numbers. Data for the Island Boards is included in the Scotland total")
  } else {
    
  # chart legend labels  
  centreline_name <- paste0(input$geoname_top," average up to end Feb 2020")    

  # chart x-axis range with some extra spacing so that markers are not cut in half at start and end of chart  
  xaxis_plots[["range"]] <- c(min(plot_data$month)-20, max(plot_data$month)+20)
    
  #switch y-axis according to which measure is selected
  if(measure == "terminations"){
    yaxis_plots[["title"]] <- "Number of terminations"
    
    tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                            "Number of terminations: ",plot_data$terminations))
    dotted_line <-  plot_data$dottedline_no
    centre_line <-  plot_data$centreline_no
    yname <- "Number of terminations"
  } else if (measure  == "av_gest") {
    #yaxis_measure <- plot_data$av_gest
    yaxis_plots[["title"]] <- "Average gestation at termination"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10 weeks
    tooltip_top <- c(paste0("Month: ",format(plot_data$month,"%B %Y"),"<br>",
                            "Average gestation at termination: ",format(plot_data$av_gest,digits = 1,nsmall=1)," weeks"))                           
        dotted_line <-  plot_data$dottedline_g
    centre_line <-  plot_data$centreline_g
    yname <- "Average gestation"
  }
  
  #Creating time trend plot
  plot_ly(data=plot_data, x=~month) %>%
    add_lines(y = ~get(measure),  
              line = list(color = "black"), text=tooltip_top, hoverinfo="text",
              marker = list(color = "black"), name = yname ) %>% 
    add_lines(y = ~dotted_line, name = FALSE,
              line = list(color = "blue", dash = "longdash"), hoverinfo="none",
              name = "Centreline", showlegend = FALSE) %>%
    add_lines(y = ~centre_line, name = centreline_name,
              line = list(color = "blue"), hoverinfo="none",
              name = "Centreline") %>%
    # adding trends
    add_markers(data = plot_data %>% filter_at(trend, all_vars(. == T)), y = ~get(measure),
                marker = list(color = "green", size = 10, symbol = "square"), name = "Trends", hoverinfo="none") %>%
    # adding shifts - add these last so that shifts are always visible on top of trends
    add_markers(data = plot_data %>% filter_at(shift, all_vars(. == T)), y = ~get(measure),
                marker = list(color = "orange", size = 10, symbol = "circle"), name = "Shifts", hoverinfo="none") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,  xaxis = xaxis_plots,
           legend = list(orientation = 'h')) %>% #position of legend underneath plot
           #legend = list(x = 0.1, y = 0.1)) %>% #position of legend
    #leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }}

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
    h2("Termination of pregnancy - 3rd February 2021"),
    p("COMMENTARY PLACEHOLDER"),
    h2("Termination of pregnancy - 2nd December 2020"),
    p("In this second release of information on terminations of pregnancy, the provisional numbers of terminations reported in Scotland in August 2020 fell to the lowest level reported since January 2018. This continues the trend of a fall in numbers from May 2020 onwards and is consistent across some but not all Board areas. The decrease in the average gestation at termination (6.6 weeks in the last month) has also continued in Scotland as a whole and any variation between Boards probably reflects minor variation in service provision between Boards."),
    p("The impact of reductions in numbers of terminations in younger women continues to be evident. The observation of an increase in the average gestation reported in women aged over 40 (to 7.5 weeks in August) may simply represent the impact of variation within small numbers of terminations in this age group. Overall, there remains no substantial variation in average gestation at termination by maternal age group or deprivation level."),
    h2("Termination of pregnancy - 28th October 2020"),
    p("Information on the number of terminations of pregnancy carried out in Scotland, and the average gestation (stage of pregnancy) at which they occurred, was included in this tool for the first time on 28 October 2020."),
    p("Termination of pregnancy (also referred to as a therapeutic or induced abortion) is provided under the terms of the Abortion Act 1967 and subsequent regulations.  When a healthcare practitioner provides a termination of pregnancy, there is a legal requirement for them to notify the Chief Medical Officer of the termination within seven days of it taking place.  Public Health Scotland is responsible for the collation of data derived from notifications of terminations of pregnancy on behalf of the Chief Medical Officer.  This notification data has been used in this tool (see Data source button on the dashboard page).  Detailed information on terminations is published each year by Public Health Scotland.  The ",
      tags$a(href = "https://beta.isdscotland.org/find-publications-and-data/population-health/sexual-health/termination-of-pregnancy-statistics/", "most recent report", class="externallink",target="_blank"),
      " covers the year to December 2019."),
    p("As an essential service, ",
      tags$a(href = "https://www.rcog.org.uk/en/guidelines-research-services/guidelines/coronavirus-abortion/", "care relating to termination of pregnancy has been provided throughout the COVID-19 pandemic", class="externallink", target="_blank"),
      ".   Termination of pregnancy can be carried out as a medical procedure or, less commonly, a surgical procedure.  Medical terminations involve the woman taking two different medicines 24-48 hours apart to end her pregnancy. Prior to Oct 2017, women having a medical termination were required to attend a clinic or hospital on two occasions to take the first and then, separately, the second medicine. From ",
      tags$a(href = "https://www.sehd.scot.nhs.uk/cmo/CMO(2017)14.pdf", "October 2017", class="externallink",target="_blank"),
      ", women requiring an early medical termination (at up to 9 weeks and 6 days gestation) were able to take the second medicine away with them at the end of their first appointment, and subsequently take that at home.  From ",
      tags$a(href = "https://www.sehd.scot.nhs.uk/cmo/CMO(2020)09.pdf", "31 March 2020", class="externallink", target="_blank"),
      ", in response to the COVID-19 pandemic, women requiring an early medical termination (at up to 11 weeks and 6 days gestation) have been able to have an initial remote consultation, by telephone or video call, then take both medicines at home."),
    p("Since 31 March 2020, there has been variation between NHS Boards across Scotland in exactly how care relating to termination of pregnancy has been provided.  Almost all Boards have provided some remote consultations.  After their initial consultation, some women have been required to attend for an ultrasound scan (for example to confirm how far along their pregnancy is if there is some doubt about that, or to see if they have an ectopic pregnancy) before medicines are provided.  Once a woman has been confirmed as eligible for an early medical termination, in some areas both sets of medicine have been delivered to the woman’s home, whereas in other areas women have been required to pick up their medicine from a clinic reception.  On 30 September 2020, the Scottish Government issued a ",
      tags$a(href = "http://www.gov.scot/publications/consultation-future-arrangements-early-medical-abortion-home/", "consultation", class="externallink", target="_blank"),
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




