##Server script for births and babies - apgar tab

###############################################.
## Modals ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_tears_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
      p("The data used for the perineal tears page comes from the Scottish 
        Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by 
        maternity hospitals to Public Health Scotland (PHS) whenever a woman is 
        discharged from an episode of day case or inpatient maternity care. From 
        October 2019, maternity hospitals have also been asked to submit SMR02 
        records following attended home births."),
      p("For the perineal tears page, SMR02 records for episodes of care 
        involving the delivery of a singleton live or stillborn baby (i.e. 
        one baby, not twins or more) at 37-42 weeks gestation inclusive have
        been used. The charts presented show the total number of singleton 
        live births at 37-42 weeks with known perineal tear status, and the 
        percentage of women giving birth vaginally to a singleton live or 
        stillborn baby with a cephalic presentation between 37-42 weeks 
        gestation who have a third or fourth degree perineal tear. Data is 
        presented for January 2018 onwards, based on the date the woman was 
        discharged from hospital after delivery."),
      p("Perineal tear scores range from 0 to 4, with 0 indicating no perineal 
        tear, and higher scores indicating the degree of tear. 8 indicates a 
        tear with unspecified degree, and 9 indicates that it is not known if 
        there was a tear."),
      p("Data is presented for individual months at Scotland level, and for 
        sequential quarters (Jan-Mar 2018, Apr-Jun 2018, etc) for individual NHS 
        Boards. Due to small numbers, the charts for individual Island Boards of 
        residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable 
        so these have not been shown. However, the Island Boards are included in 
        the Scotland total, and data for the Island Boards is available in the 
        spreadsheet provided through the ‘Download data’ button."),
      p("Data is shown for up to and including the most recent month for which 
        SMR02 records are considered near complete. Data for the most recent 
        months should be viewed as provisional. Data for all months will be 
        refreshed every time the dashboard page is updated, and data for the 
        most recent months is likely to change slightly as additional SMR02 
        records are submitted to PHS."),
      p("Although there is no legal requirement to submit SMR02 records to PHS, 
        data completeness is very high. For example, for the period 1 April 2019 
        to 31 March 2020, live births recorded on SMR02 represented 98.8% of the 
        live births registered by law with National Records of Scotland (NRS) 
        and stillbirths on SMR02 represented 91.1% of stillbirths registered 
        with NRS. In addition, the recording of specific data items allowing 
        identification of singleton live births at 37-42 weeks gestation is very 
        complete. However, identification of babies with a cephalic presentation 
        is less robust. The data included in this dashboard use the data item 
        ‘Mode of Delivery’ to identify cephalic presentation, which is regarded 
        as being more accurately captured than ‘Presentation at Delivery’. For 
        the period 1 April 2018 to 31 December 2020 ‘Mode of Delivery’ was 
        recorded on 99.9% of SMR02 records relating to a singleton live or 
        stillbirth and information on perineal tears was recorded on 99.6% of 
        records."),
      p("Further information based on SMR02 data is also available from the annual ",
        tags$a(href= "https://beta.isdscotland.org/find-publications-and-data/population-health/births-and-maternity/births-in-scottish-hospitals/",
              "Births in Scottish Hospitals report." , target="_blank")),

      size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_tears_rules,
             showModal(modalDialog(
               title = "How do we identify patterns in the data?",
               p("Run charts use a series of rules to help identify important 
                 changes in the data. These are the ones we used for these charts:"),
               tags$ul(tags$li("Shifts: Six or more consecutive data points 
                               above or below the centreline. Points on the 
                               centreline neither break nor contribute to a 
                               shift (marked on chart)."),
                       tags$li("Trends: Five or more consecutive data points 
                               which are increasing or decreasing. An 
                               observation that is the same as the preceding 
                               value does not count towards a trend (marked on 
                               chart)."),
                       tags$li("Too many or too few runs: A run is a sequence of
                               one or more consecutive observations on the same 
                               side of the centreline. Any observations falling 
                               directly on the centreline can be ignored. If 
                               there are too many or too few runs (i.e. the 
                               median is crossed too many or too few times) 
                               that’s a sign of something more than random chance."),
                       tags$li("Astronomical data point: A data point which is 
                               distinctly different from the rest. Different 
                               people looking at the same graph would be 
                               expected to recognise the same data point as 
                               astronomical (or not).")),
               p("Further information on these methods of presenting data can be found in the ",                      
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts', target="_blank"),"."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

#Modal to explain SIMD and deprivation
#Link action button click to modal launch
observeEvent(input$btn_modal_simd_tears, { showModal(
  modalDialog(
    h5("What is SIMD and deprivation?"),
    p("Women have been allocated to different levels of deprivation based on the 
      small area (data zone) in which they live and the Scottish Index of 
      Multiple Deprivation (SIMD). SIMD scores are based on data for local areas 
      reflecting 38 indicators across 7 domains: income; employment; health; 
      education, skills and training; housing; geographic access; and crime. 
      In this tool we have presented results for women living in different SIMD 
      ‘quintiles’. To produce quintiles, data zones are ranked by their SIMD score 
      then the areas each containing a fifth (20%) of the overall population of 
      Scotland are identified. Women living in the most and least deprived areas 
      that each contain a fifth of the population are assigned to SIMD quintile 
      1 and 5 respectively."),
    size = "l",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  ))})


###############################################.
## Reactive controls  ----
###############################################.

# deliveries reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_tears <- renderUI({
  #Lists areas available in   
  areas_summary_tears <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_tears])
  selectizeInput("geoname_tears", label = NULL, choices = areas_summary_tears, selected = "")
})

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
tears_filter <- function(){
  
tears_filt <- tears_runchart %>% filter(area_name == input$geoname_tears &
                               area_type == input$geotype_tears &
                               type %in% c("Scotland", "Health board")) 
tears_filt %>% 
    # Sorting levels based on date
    mutate(date_label = factor(date_label, levels = as.character(tears_filt[order(tears_filt$date),]$date_label))) # to allow sorting in x axis later on
}

#Dataset 2: behind line charts for age and deprivation (available for scotland only)
tears_linechart_split <- function(split){
  
  tears_scot  %>% filter(area_name == "Scotland" &
                            area_type == "Scotland" &
                            type==split)
}

#Dataset 3: behind line chart  (available at scotland and NHS board level)
tears_linechart_filter <- function(){
  
  tears_linechart %>% filter(area_name == input$geoname_tears &
                                area_type == input$geotype_tears &
                                type %in% c("Scotland","Health board"))
}

###############################################.
## Induction Chart calls to chart function ----
###############################################.

# chart outputs for trend
output$tears_trend <- renderPlotly({plot_tears_trend(measure="perc_3rd4th_tears_37plus",shift = "tears_shift", trend = "tears_trend")})

#chart outputs for line charts for NHS board and Scot
output$tears_linechart_number <- renderPlotly({plot_tears_linechart(measure="tears")})
output$tears_linechart_percent <- renderPlotly({plot_tears_linechart(measure="percent_tears")})

#chart outputs for line charts for Scotland only age and deprivation line charts
output$tears_linechart_age_n <- renderPlotly({plot_tears_split(dataset=tears_linechart_split(split="age"),split="age", measure="tot_tears_37plus")})
output$tears_linechart_age_p <- renderPlotly({plot_tears_split(dataset=tears_linechart_split(split="age"),split="age", measure="perc_3rd4th_tears_37plus")})
output$tears_linechart_dep_n <- renderPlotly({plot_tears_split(dataset=tears_linechart_split(split="dep"),split="dep", measure="tot_tears_37plus")})
output$tears_linechart_dep_p <- renderPlotly({plot_tears_split(dataset=tears_linechart_split(split="dep"),split="dep", measure="perc_3rd4th_tears_37plus")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$tears_explorer <- renderUI({
  
  # text for titles of cut charts
  tears_data_timeperiod <-  paste0("Figures based on data extracted ",tears_extract_date)
  tears_title <- paste0("of women giving birth vaginally to a singleton live or 
                        stillborn baby with a cephalic presentation between 37-42 
                        weeks gestation who have a third or fourth degree 
                        perineal tear: ",input$geoname_tears)
  
  chart_explanation <- 
    tagList(#p("We have used ",                      
              # tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
              #        'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p("On the ‘Percentage of women who have a third or fourth degree 
              perineal tear’ chart above, the dots joined by a solid black line 
              show the percentage of women giving birth vaginally to a singleton 
              live or stillborn baby with a cephalic presentation between 37-42 
              weeks gestation who have a third or fourth degree perineal tear, 
              in each month from January 2018 onwards. The solid blue centreline 
              on the chart shows the average (median) percentage of women who 
              have a third or fourth degree perineal tear over the period 
              January 2018 to February 2020 inclusive (the period before the 
              COVID-19 pandemic in Scotland). The dotted blue centreline 
              continues that average to allow determination of whether there has 
              subsequently been a change in the percentage of women who have a 
              third or fourth degree perineal tear."))
  
  # Layout depending if Scotland or HB selected
    tagList(fluidRow(column(12,
                            h4(paste0("Percentage ", tears_title)),
                            actionButton("btn_tears_rules", "How do we identify patterns in the data?",
                                         icon = icon('question-circle')),
                            withSpinner(plotlyOutput("tears_trend"))),
                     column(12,
                            p(tears_data_timeperiod),
                            p(chart_explanation)),
                     column(12,
                            br(), #spacing
                            h4(paste0("Number of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a third or fourth degree perineal tear: ",input$geoname_tears))),
                     column(12,
                            withSpinner(plotlyOutput("tears_linechart_number"))),
                     #only if scotland selected display age and deprivation breakdowns
                     if (input$geotype_tears == "Scotland"){
                       tagList(
                         fluidRow(column(12,
                                         h4("Women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a third or fourth degree perineal tear by maternal age group: Scotland"))),
                         fluidRow(column(6,
                                         h4("Number of women who have a third or fourth degree perineal tear"),
                                         withSpinner(plotlyOutput("tears_linechart_age_n"))),
                                  column(6,
                                         h4("Percentage of women who have a third or fourth degree perineal tear"),
                                         withSpinner(plotlyOutput("tears_linechart_age_p")))),
                         fluidRow(column(12,
                                         br(), # spacing
                                         h4("Women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a third or fourth degree perineal tear by maternal deprivation level: Scotland"),
                                         actionButton("btn_modal_simd_tears", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         fluidRow(column(6,
                                         h4("Number of women who have a third or fourth degree perineal tear"),
                                         withSpinner(plotlyOutput("tears_linechart_dep_n"))),
                                  column(6,
                                         h4("Percentage of women who have a third or fourth degree perineal tear"),
                                         withSpinner(plotlyOutput("tears_linechart_dep_p"))))
                       )#tagList from if statement
                     }
    ))
})

#############################################.
## tears chart functions ----
############################################.

## RUNCHART trend chart for monthly inductions percentages: Scotland & NHS Board (except island boards) 
## Function could be simplified to run without parameters but copied logic from other pregnancy tabs therefore easier to keep same structure.

plot_tears_trend <- function(measure, shift, trend){  
  plot_data <- tears_filter()
  
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 65, 
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {
    
    # chart legend labels  
    centreline_name <- paste0(input$geoname_tears," average up to end Feb 2020") 
    
    # format y axis
    yname <- "Percentage of women who have a third or fourth degree perineal tear (%)"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10%
    yaxis_plots[["title"]] <- "Percentage of women (%)"
    
    # chart x-axis range with some extra spacing so that markers are not cut in half at start and end of chart  
    xaxis_plots[["range"]] <- c(min(plot_data$date)-20, max(plot_data$date)+20)
    xaxis_plots[["title"]] <- c("")
    tick_freq <- case_when(input$geotype_tears == "Scotland" ~ 6, T ~ 2)
    xaxis_plots[["dtick"]] <- tick_freq
    xaxis_plots[["tickangle"]] <- 0
    
    #specify tool tip
    tooltip_top <- c(paste0(format(plot_data$date_type),": ",format(plot_data$date_label),"<br>",
                            "Percentage: ",format(plot_data$perc_3rd4th_tears_37plus, digits = 1,nsmall=1),"%", "<br>"))
    
    #Creating time trend plot
    plot_ly(data=plot_data, x=~date_label) %>%
      add_lines(y = ~perc_3rd4th_tears_37plus,  
                line = list(color = "black"), text=tooltip_top, hoverinfo="text",
                marker = list(color = "black"), name = yname ) %>% 
      add_lines(y = ~ext_median_tears_37plus, name = FALSE,
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline", showlegend = FALSE) %>%
      add_lines(y = ~median_tears_37plus, name = centreline_name,
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
plot_tears_linechart <- function(measure){  
  
  plot_data <- tears_linechart_filter() 
  
  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(ind = factor(ind, levels = c("Women who have a 3rd or 4th degree perineal tear", "Women with known perineal tear status")))
  #pick a colour palette to apply
  pallette <- pal_age
  
  # adjust chart y axis according to what is being displayed
  if(measure == "percent_tears"){
    yaxis_plots[["title"]] <- "Percentage of births (%)" 
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10%

    plot_data <- plot_data %>%  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
      filter(ind!="Women with known perineal tear status")
    
  }
  
  if(measure == "tears"){
    yaxis_plots[["title"]] <- "Number of women"

  }
  # Create tooltip for line chart
  tooltip <- c(paste0( plot_data$ind,"<br>",
                       "Area: ",plot_data$area_name,"<br>",
                       format(plot_data$date_type),": ",  format(plot_data$date_label),"<br>",
                       "Number of women: ", plot_data$tears,"<br>",
                       "Percentage of women: ", format(plot_data$percent_tears,digits = 1,nsmall=1),"%"))
  
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50, 
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {
    
    tick_freq <- case_when(input$geotype_tears == "Scotland" ~ 6, T ~ 2)
    
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
plot_tears_split <- function(dataset, split, measure){  
  
  plot_data <- dataset
  
  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group: "), split=="dep" ~ paste0("Deprivation group: "))
  
  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Quarter: ", format(plot_data$quarter_label),"<br>",
                      "Number: ", plot_data$tot_tears_37plus, "<br>",
                      "Percentage: ", format(plot_data$perc_3rd4th_tears_37plus,digits=1,nsmall = 1),"%"))
  
  # adjust chart y axis according to what is being displayed
  if(measure == "perc_3rd4th_tears_37plus"){
    yaxis_plots[["title"]] <- "Percentage of women (%)"
    if(split == "age"){
      yaxis_plots[["range"]] <- c(0, 10)}  # forcing range from 0 to 10% for age group
    if(split == "dep"){
      yaxis_plots[["range"]] <- c(0, 10)}  # forcing range from 0 to 10% for dep
  }
  if(measure == "tot_tears_37plus"){
    yaxis_plots[["title"]] <- "Number of women"
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

tears_download_data <- reactive({
  tears_download
})

output$download_tears_data <- downloadHandler(
  filename ="tears_extract.csv",
  content = function(file) {
    write_csv(tears_download_data(),
              file) } 
)

###############################################.
## Commentary tab content  ----
###############################################.

#action associated with action links within commentary text - this observe event linked 
# to an actionLink within the commentary which will take the user to commentary easily.
observeEvent(input$switch_to_tears,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Perineal Tears")
})


output$tears_commentary <- renderUI({
  tagList(
    bsButton("jump_to_tears",label = "Go to data"), #this button can only be used once
    h2("Perineal tears - June 2021"),
    p("Commentary required"))
    
})


##END
