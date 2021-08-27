
##Server script for births and babies - apgar tab.


###############################################.
## Modals ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_apgar_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The data used for the Apgar scores page comes from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended home births."),
               p("For the Apgar scores page, SMR02 records for episodes of care involving the delivery of a singleton live birth (i.e. one baby, not twins or more) at 37-42 weeks gestation inclusive have been used. The charts presented show the total number of singleton live births at 37-42 weeks with known Apgar score at 5 minutes following delivery, and the number and percentage of babies that had a score of <7. Data is presented for January 2018 onwards, based on the date the woman was discharged from hospital after delivery."),
               p("Apgar scores range from 0 to 10, with higher scores indicating that the baby is in a healthy condition following delivery."),
               p("Data is presented for individual months at Scotland level, and for sequential quarters (Jan-Mar 2018, Apr-Jun 2018, etc) for individual NHS Boards. Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown. However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
               p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete. Data for the most recent months should be viewed as provisional. Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland. In addition, the recording of specific data items allowing identification of singleton live births at 37-42 weeks gestation, and of babies’ 5 minute Apgar score, is very complete. For the period 1 April 2019 to 31 March 2020, a 5 minute Apgar score was recorded on 99% of SMR02 records relating to singleton live births at 37-42 weeks gestation."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href= "https://beta.isdscotland.org/find-publications-and-data/population-health/births-and-maternity/births-in-scottish-hospitals/",
                        "Births in Scottish Hospitals report." , target="_blank")),

               size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_apgar_rules,
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
observeEvent(input$btn_modal_simd_apgar, { showModal(
  modalDialog(
    h5("What is SIMD and deprivation?"),
    p("Babies have been allocated to different levels of deprivation based on the 
      small area (data zone) in which their mothers live and the Scottish Index of 
      Multiple Deprivation (SIMD). SIMD scores are based on data for local areas 
      reflecting 38 indicators across 7 domains: income; employment; health; education, 
      skills and training; housing; geographic access; and crime. In this tool we have 
      presented results for babies living in different SIMD ‘quintiles’. To produce 
      quintiles, data zones are ranked by their SIMD score then the areas each containing 
      a fifth (20%) of the overall population of Scotland are identified. Babies living 
      in the most and least deprived areas that each contain a fifth of the population are 
      assigned to SIMD quintile 1 and 5 respectively."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  ))})


###############################################.
## Reactive controls  ----
###############################################.

# deliveries reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_apgar <- renderUI({
  #Lists areas available in   
  areas_summary_apgar <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_apgar])
  selectizeInput("geoname_apgar", label = NULL, choices = areas_summary_apgar, selected = "")
})

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
apgar_filter <- function(){
  
apgar_filt <- apgar_runchart %>% filter(area_name == input$geoname_apgar &
                               area_type == input$geotype_apgar &
                               type %in% c("Scotland", "Health board")) 
apgar_filt %>% 
    # Sorting levels based on date
    mutate(date_label = factor(date_label, levels = as.character(apgar_filt[order(apgar_filt$date),]$date_label))) # to allow sorting in x axis later on
}

#Dataset 2: behind line charts for age and deprivation (available for scotland only)
apgar_linechart_split <- function(split){
  
  apgar_scot  %>% filter(area_name == "Scotland" &
                            area_type == "Scotland" &
                            type==split)
}

#Dataset 3: behind line chart  (available at scotland and NHS board level)
apgar_linechart_filter <- function(){
  
  apgar_linechart %>% filter(area_name == input$geoname_apgar &
                                area_type == input$geotype_apgar &
                                type %in% c("Scotland","Health board"))
}

###############################################.
## Induction Chart calls to chart function ----
###############################################.

# chart outputs for trend
output$apgar_trend <- renderPlotly({plot_apgar_trend(measure="perc_low_apgar5_37plus",shift = "apgar_shift", trend = "apgar_trend")})

#chart outputs for line charts for NHS board and Scot
output$apgar_linechart_number <- renderPlotly({plot_apgar_linechart(measure="apgar5")})
output$apgar_linechart_percent <- renderPlotly({plot_apgar_linechart(measure="percent_apgar")})

#chart outputs for line charts for Scotland only age and deprivation line charts
output$apgar_linechart_age_n <- renderPlotly({plot_apgar_split(dataset=apgar_linechart_split(split="age"),split="age", measure="tot_apgar5_37plus")})
output$apgar_linechart_age_p <- renderPlotly({plot_apgar_split(dataset=apgar_linechart_split(split="age"),split="age", measure="perc_low_apgar5_37plus")})
output$apgar_linechart_dep_n <- renderPlotly({plot_apgar_split(dataset=apgar_linechart_split(split="dep"),split="dep", measure="tot_apgar5_37plus")})
output$apgar_linechart_dep_p <- renderPlotly({plot_apgar_split(dataset=apgar_linechart_split(split="dep"),split="dep", measure="perc_low_apgar5_37plus")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$apgar_explorer <- renderUI({
  
  # text for titles of cut charts
  apgar_data_timeperiod <-  paste0("Figures based on data extracted ",apgar_extract_date)
  apgar_title <- paste0("of singleton live births at 37-42 weeks gestation that have a 5 minute Apgar score of <7: ",input$geoname_apgar)
  
  chart_explanation <- 
    tagList(#p("We have used ",                      
              # tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
              #        'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p("On the ‘Percentage of births that have a 5 minute Apgar score of <7’ chart above, the dots joined by a solid black line show the percentage of singleton live births at 37-42 weeks gestation with known 5 minute Apgar score that had a score of <7, in each month from January 2018 onwards. The solid blue centreline on the chart shows the average (median) percentage of births with 5 minute Apgar score of <7 over the period January 2018 to February 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The dotted blue centreline continues that average to allow determination of whether there has subsequently been a change in the percentage of births with a 5 minute Apgar score of <7."))
  
  # Layout depending if Scotland or HB selected
    tagList(fluidRow(column(12,
                            h4(paste0("Percentage ", apgar_title)),
                            actionButton("btn_apgar_rules", "How do we identify patterns in the data?",
                                         icon = icon('question-circle')),
                            withSpinner(plotlyOutput("apgar_trend"))),
                     column(12,
                            p(apgar_data_timeperiod),
                            p(chart_explanation)),
                     column(12,
                            br(), #spacing
                            h4(paste0("Number of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7: ",input$geoname_apgar))),
                     column(12,
                            withSpinner(plotlyOutput("apgar_linechart_number"))),
                     #only if scotland selected display age and deprivation breakdowns
                     if (input$geotype_apgar == "Scotland"){
                       tagList(
                         fluidRow(column(12,
                                         h4("Singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7 by maternal age group: Scotland"))),
                         fluidRow(column(6,
                                         h4("Number of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7"),
                                         withSpinner(plotlyOutput("apgar_linechart_age_n"))),
                                  column(6,
                                         h4("Percentage of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7"),
                                         withSpinner(plotlyOutput("apgar_linechart_age_p")))),
                         fluidRow(column(12,
                                         br(), # spacing
                                         h4("Singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7 by maternal deprivation level: Scotland"),
                                         actionButton("btn_modal_simd_apgar", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         fluidRow(column(6,
                                         h4("Number of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7"),
                                         withSpinner(plotlyOutput("apgar_linechart_dep_n"))),
                                  column(6,
                                         h4("Percentage of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7"),
                                         withSpinner(plotlyOutput("apgar_linechart_dep_p"))))
                       )#tagList from if statement
                     }
    ))
})

#############################################.
## apgar chart functions ----
############################################.

## RUNCHART trend chart for monthly inductions percentages: Scotland & NHS Board (except island boards) 
## Function could be simplified to run without parameters but copied logic from other pregnancy tabs therefore easier to keep same structure.

plot_apgar_trend <- function(measure, shift, trend){  
  plot_data <- apgar_filter()
  
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 65, 
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {
    
    # chart legend labels  
    centreline_name <- paste0(input$geoname_apgar," average up to end Feb 2020") 
    
    # format y axis
    yname <- "Percentage of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7 (%)"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10%
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    
    # chart x-axis range with some extra spacing so that markers are not cut in half at start and end of chart  
    xaxis_plots[["range"]] <- c(min(plot_data$date)-20, max(plot_data$date)+20)
    xaxis_plots[["title"]] <- c("")
    tick_freq <- case_when(input$geotype_apgar == "Scotland" ~ 6, T ~ 2)
    xaxis_plots[["dtick"]] <- tick_freq
    xaxis_plots[["tickangle"]] <- 0
    
    #specify tool tip
    tooltip_top <- c(paste0(format(plot_data$date_type),": ",format(plot_data$date_label),"<br>",
                            "Percentage: ",format(plot_data$perc_low_apgar5_37plus, digits = 1,nsmall=1),"%", "<br>"))
    
    #Creating time trend plot
    plot_ly(data=plot_data, x=~date_label) %>%
      add_lines(y = ~perc_low_apgar5_37plus,  
                line = list(color = "black"), text=tooltip_top, hoverinfo="text",
                marker = list(color = "black"), name = yname ) %>% 
      add_lines(y = ~ext_median_apgar5_37plus, name = FALSE,
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline", showlegend = FALSE) %>%
      add_lines(y = ~median_apgar5_37plus, name = centreline_name,
                line = list(color = "blue"), hoverinfo="none",
                name = "Centreline") %>%
      # adding trends
      add_markers(data = plot_data %>% filter_at(trend, all_vars(. == T)), y = ~get(measure),
                  marker = list(color = "green", size = 10, symbol = "square"), name = "Trends", hoverinfo="none") %>%  
      # adding shifts
      add_markers(data = plot_data %>% filter_at(shift, all_vars(. == T)), y = ~get(measure),
                  marker = list(color = "orange", size = 10, symbol = "circle"), name = "Shifts", hoverinfo="none") %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,
             xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }}

#####################################################################################################################.
## LINECHART SCOTLAND & NHS BOARD: births (37-42 weeks gestation) where delivery induced, numbers and percentages - Scotland level only
plot_apgar_linechart <- function(measure){  
  
  plot_data <- apgar_linechart_filter() 
  
  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(ind = factor(ind, levels = c("Babies with Apgar 5 < 7", "Babies with known Apgar 5")))
  #pick a colour palette to apply
  pallette <- pal_age
  
  # adjust chart y axis according to what is being displayed
  if(measure == "percent_apgar"){
    yaxis_plots[["title"]] <- "Percentage of births (%)" 
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10%

    plot_data <- plot_data %>%  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
      filter(ind!="Babies with known Apgar 5")
    
  }
  
  if(measure == "apgar5"){
    yaxis_plots[["title"]] <- "Number of births"

  }
  # Create tooltip for line chart
  tooltip <- c(paste0( plot_data$ind,"<br>",
                       "Area: ",plot_data$area_name,"<br>",
                       format(plot_data$date_type),": ",  format(plot_data$date_label),"<br>",
                       "Number of births: ", plot_data$apgar5,"<br>",
                       "Percentage of births: ", format(plot_data$percent_apgar,digits = 1,nsmall=1),"%"))
  
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50, 
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {
    
    tick_freq <- case_when(input$geotype_apgar == "Scotland" ~ 6, T ~ 2)
    
    xaxis_plots <- c(xaxis_plots,
                     dtick =tick_freq, tickangle = 0,
                     categoryorder = "array", categoryarray = ~date)
    
    #Creating trend plot
    plot_ly(data=plot_data, x=~date_label,  y = ~get(measure)) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~ind, colors = pallette,
                text= tooltip, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)}
}


#####################################################################################################################.
## LINECHART SCOTLAND: inductced deliveries by age group and deprivation, numbers and percentages - Scotland level only
plot_apgar_split <- function(dataset, split, measure){  
  
  plot_data <- dataset
  
  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))
  
  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Quarter: ", format(plot_data$quarter_label),"<br>",
                      "Number: ", plot_data$tot_apgar5_37plus, "<br>",
                      "Percentage: ", format(plot_data$perc_low_apgar5_37plus,digits=1,nsmall = 1),"%"))
  
  # adjust chart y axis according to what is being displayed
  if(measure == "perc_low_apgar5_37plus"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    if(split == "age"){
      yaxis_plots[["range"]] <- c(0, 10)}  # forcing range from 0 to 10% for age group
    if(split == "dep"){
      yaxis_plots[["range"]] <- c(0, 10)}  # forcing range from 0 to 10% for dep
  }
  if(measure == "tot_apgar5_37plus"){
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
  
  xaxis_plots <- c(xaxis_plots,
                   dtick = 3, tickangle = 0,
                   categoryorder = "array", categoryarray = ~quarter)
  
  #Creating trend plot
  plot_ly(data=plot_data, x=~quarter_label,  y=~get(measure)) %>%
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

###############################################.
## Data downloads ----
###############################################.

apgar_download_data <- reactive({
  apgar_download
})

output$download_apgar_data <- downloadHandler(
  filename ="apgar5_extract.csv",
  content = function(file) {
    write_csv(apgar_download_data(),
              file) } 
)

###############################################.
## Commentary tab content  ----
###############################################.

#action associated with action links within commentary text - this observe event linked 
# to an actionLink within the commentary which will take the user to commentary easily.
observeEvent(input$switch_to_apgar,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Apgar 5")
})


output$apgar_commentary <- renderUI({
  tagList(
    bsButton("jump_to_apgar",label = "Go to data"), #this button can only be used once
    h2("Apgar scores - 2nd June 2021"),
    p("In this release of information on Apgar scores (2nd June 2021) data have been updated to include women discharged after delivery up to and including February 2021. The data show that, at all Scotland level, the percentage of singleton live born babies delivered at 37-42 weeks gestation which have a low 5 minute Apgar score (less than 7) in February 2021 was at a very similar level to the pre-pandemic average at 1.8%."),
    p("The Apgar score data by NHS Board are presented quarterly and this information will next be updated on 7th July 2021."),
    h2("Apgar scores - 14th April 2021"),
    p("Information on Apgar scores was included in this tool for the first time on 14 April 2021."),
    p("The Apgar score measures the condition of newborn babies.  It was developed to allow health professionals to quickly identify babies needing resuscitation after delivery.  Babies are scored 0, 1, or 2 for each of their heart rate; respiratory effort; muscle tone; response to stimulation; and colour.  Scores therefore range from 0 to 10, with higher scores indicating a better condition.  Scores of 7 or over are generally interpreted as ‘reassuring’, with scores of 4-6 considered moderately low, and scores of 0-3 considered very low.  The Apgar score is measured at 1 and 5 minutes after delivery for all babies in Scotland."),
    p("Low Apgar scores at 5 minutes after delivery are associated with a higher risk of neonatal death, neonatal morbidity, and longer term problems with babies’ development.  Babies born preterm can have lower scores due to their overall immaturity rather than a specific problem such as lack of oxygen during delivery.  Due to this, the association between low Apgar scores and poor outcomes is generally stronger for babies born at term (at 37-41 weeks gestation) or post-term (at ≥42 weeks gestation) compared to those born preterm (at <37 weeks gestation)."),
    p("The information on Apgar scores presented through this tool is taken from hospital discharge records, specifically records relating to the care of women delivering a singleton live birth (i.e. one baby, not twins or more) at 37-42 weeks gestation inclusive.  Further technical information is available through the ‘Data source’ button on the dashboard page."),
    p("The data shows that, at all Scotland level, just under 2% of singleton, live born babies delivered at 37-42 weeks gestation have a low 5 minute Apgar score (less than 7).  The percentage of babies with a low score has been broadly similar over the whole period examined (January 2018 to, currently, December 2020).  In particular, no increase in the percentage of babies with a low score has been seen during the COVID-19 pandemic. In fact the percentage of babies born with a 5 minute Apgar score of less than 7 was consistently slightly lower from Jan to Sep 2020 than the Jan 2018 to Feb 2020 average."),
    p("Prior to the COVID-19 pandemic, the percentage of babies with a low 5 minute Apgar score was similar across mainland NHS Boards, ranging from around 1% of babies born to mothers living in NHS Highland to around 2.5% of babies born to mothers living in NHS Lanarkshire.  Within each Board, the percentage of babies with a low score fluctuates over time, as would be expected by chance.  No increase in the percentage of babies with a low score has been seen during the COVID-19 pandemic in any Board."),
    p("The percentage of babies with a low 5 minute Apgar score is similar for babies born to mothers from different age groups, and for babies born to mothers living in areas with different levels of deprivation.  No changes to these patterns have been seen during the COVID-19 pandemic."))

})


##END
