##Server script for pregnancy mode of delivery tab

# Pop-up modal explaining source of data
observeEvent(input$btn_mod_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The Antenatal Booking Data presented is based on a new data collection established as a rapid response to COVID-19. Data is collected each week, from the clinical information system (BadgerNet Maternity (most NHS boards), TrakCare Maternity (Lothian) or Eclipse (A&A) used by the midwives who ‘book’ the pregnant woman for maternity care."),br(),
               p("Historic data from April 2019 was also collected as a ‘catch-up’ extract in order to identify all women who were currently pregnant during the COVID-19 period. This was either from the same source or - in Tayside and Highland - from the systems in use before the introduction of BadgerNet Maternity."),br(),
               p("The charts presented on this page show the number of women booking for antenatal care in each week from the week beginning 1 April 2019 onwards.  Data is shown at all Scotland level and for each mainland NHS Board of residence.  Due to small numbers, weekly data is not shown for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles), however the Island Boards are included in the Scotland total.  In addition to the weekly data, the ‘Download data’ button provides monthly data (based on exact month of booking rather than summation of sequential weeks) for each NHS Board of residence, including the Island Boards."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

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

#Dataset behind trend run chart  (available at scotland and NHS board level)
mod_filter <- function(){
  
  mod_runchart %>% filter(area_name == input$geoname_mod &
                   area_type == input$geotype_mod &
                   type %in% c("Scotland","Health board"))
}

#Dataset behind line charts for age and deprivation (available for scotland only)
mod_linechart_split <- function(split){
  
  mod_scot  %>% filter(area_name == "Scotland" &
                    area_type == "Scotland" &
                    type==split)
}

#Dataset behind line chart  (available at scotland and NHS board level)
mod_linechart_filter <- function(){
  
  mod_linechart %>% filter(area_name == input$geoname_mod &
                   area_type == input$geotype_mod &
                   type %in% c("Scotland","Health board"))
}


###############################################.
## Mode of delivery Charts ----
###############################################.

# chart outputs for trend
output$mod_trend_csection_all <- renderPlotly({plot_mod_trend(measure="perc_csection_all", shift = "csection_all_shift", trend = "csection_all_trend")})
output$mod_trend_csection_elec <- renderPlotly({plot_mod_trend(measure="perc_csection_elec", shift = "csection_elec_shift", trend = "csection_elec_trend")})
output$mod_trend_csection_emer <- renderPlotly({plot_mod_trend(measure="perc_csection_emer", shift = "csection_emer_shift", trend = "csection_emer_trend")})

output$mod_linechart_number <- renderPlotly({plot_mod_linechart(measure="births")})
output$mod_linechart_percent <- renderPlotly({plot_mod_linechart(measure="percent_births")})

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
  mod_title <- paste0("Percentage of births by caesarian section: ",input$geoname_mod)
  mod_title_detail <-  paste0("Singleton live births only, all gestations")

  
  chart_explanation <- 
    tagList(p("We have used ",                      
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p("On the ‘Number of caesarian sections’ chart above, the dots joined by a solid black line show the number of terminations of pregnancy in each month from January 2018 onwards.  The solid blue centreline on the chart shows the average (median) number of terminations of pregnancy over the period January 2018 to February 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The dotted blue centreline continues that average to allow determination of whether there has subsequently been a change in the number of terminations of pregnancy."),
            p("The ‘Average gestation at termination’ chart follows a similar format.  In this chart, the dots joined by a solid black line show the average (mean) gestation at which the terminations of pregnancy occurred (based on gestation at termination measured in completed weeks of pregnancy)."))
  
  # Function to create common layout to all immunisation charts
  mod_layout <- function(mod_trend_csection_all,mod_trend_csection_elec,mod_trend_csection_emer,mod_linechart_age_n, mod_linechart_age_p,mod_linechart_dep_n, mod_linechart_dep_p,mod_linechart_number,mod_linechart_percent){
    tagList(fluidRow(column(12,
                            h4(mod_title),
                            p(mod_title_detail),
                            actionButton("btn_mod_rules", "How do we identify patterns in the data?")),
                     column(4,
                            h4("All caesarian sections"),
                            withSpinner(plotlyOutput("mod_trend_csection_all"))),
                     column(4,
                            h4("Elective caesarian sections"),
                            withSpinner(plotlyOutput("mod_trend_csection_elec"))),
                     column(4,
                            h4("Emergency caesarian sections"),
                            withSpinner(plotlyOutput("mod_trend_csection_emer"))),
                     #only if scotland selected display age and deprivation breakdowns
                     if (input$geotype_mod == "Scotland"){
                       tagList(
                         fluidRow(column(12,h4("splits for scot only"))),
                         fluidRow(column(6,
                                         h4("Numbers"),
                                         withSpinner(plotlyOutput("mod_linechart_age_n"))),
                                  column(6,
                                         h4("percentage"),
                                         withSpinner(plotlyOutput("mod_linechart_age_p")))),
                         fluidRow(column(12,h4("Caesarian sections by deprivation: Scotland"),
                                         actionButton("btn_modal_simd_mod", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         fluidRow(column(6,
                                         h4("c section number"),
                                        withSpinner(plotlyOutput("mod_linechart_dep_n"))),
                                  column(6,
                                         h4("csection %"),
                                         withSpinner(plotlyOutput("mod_linechart_dep_p"))))
                       )#tagList from if statement
                     },
                     column(6,
                            p("Line - number"),
                            withSpinner(plotlyOutput("mod_linechart_number"))),
                     column(6,
                            p("line - %"),
                            withSpinner(plotlyOutput("mod_linechart_percent"))),
                     column(12,
                            p(chart_explanation))))}
  
  # #link plot functions to layouts
  mod_layout(mod_trend_csection_all="mod_trend_csection_all",
             mod_trend_csection_elec="mod_trend_csection_elec",
             mod_trend_csection_emer="mod_trend_csection_emer",
             mod_linechart_age_n="mod_linechart_age_n",
             mod_linechart_age_p="mod_linechart_age_p",
             mod_linechart_dep_n="mod_linechart_dep_n",
             mod_linechart_dep_p="mod_linechart_dep_p",
             mod_linechart_number="mod_linechart_number",
             mod_linechart_percent="mod_linechart_percent")
})


#############################################.
## Mode of delivery chart functions ----
############################################.

## Runchart trend chart for monthly c-section percentages 
## Rather than try and present all the modes of delivery we have opted just to produce a run chart
## showing rates of c-section (by type all, emergency, elective) as these are the modes of deliver that people most want to see

plot_mod_trend <- function(measure, shift, trend){  
  
  plot_data <- mod_filter()
  
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50, 
                text_nodata = "Data not shown due to small numbers. Data for the Island Boards is included in the Scotland total")
  } else {
    
    # chart legend labels  
    centreline_name <- paste0(input$geoname_mod," average up to end XXX 2020")    
    yname <- "Percentage (%)"
    yaxis_plots[["range"]] <- c(0, 40)  # forcing range from 0 to60
    
    # chart x-axis range with some extra spacing so that markers are not cut in half at start and end of chart  
    xaxis_plots[["range"]] <- c(min(plot_data$month)-20, max(plot_data$month)+20)
    
    
    yaxis_plots[["title"]] <- "Percentage (%)"
    
    tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                            "Percentage of all live births: ",format(plot_data$perc_csection_all,digits = 1,nsmall=1)))
    
    centre_line <- case_when(measure == "perc_csection_all" ~ plot_data$median_csection_all,
                             measure == "perc_csection_elec" ~ plot_data$median_csection_elec,
                             measure == "perc_csection_emer" ~ plot_data$median_csection_emer)
    dotted_line <- case_when(measure == "perc_csection_all" ~ plot_data$ext_csection_all,
                             measure == "perc_csection_elec" ~ plot_data$ext_csection_elec,
                             measure == "perc_csection_emer" ~ plot_data$ext_csection_emer)                        
    
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
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }}


##
## Line chart showing modes of delivery by age group and deprivation, numbers and percentages - Scotland level only
plot_mod_split <- function(dataset, split, measure){  

  plot_data <- dataset
  
  # Create tooltip for line chart
  tooltip <- c(paste0("Caesarian sections",
                      "Number: ", plot_data$csection))
  
  #adjust datasets accordig to which data split to be displayed
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



##
## Line chart showing modes of delivery numbers and percentages - NHS board and Scotland
plot_mod_linechart <- function(measure){  
  
plot_data <- mod_linechart_filter()

# Create tooltip for line chart
tooltip <- c(paste0("Mode of delivery: ", plot_data$mode,"<br>",
                    "Number of births: ", plot_data$births,"<br>",
                    plot_data$area_name))

if (is.data.frame(plot_data) && nrow(plot_data) == 0)
{ plot_nodata(height = 50, 
              text_nodata = "Data not shown due to small numbers. Data for the Island Boards is included in the Scotland total")
} else {

  #Creating trend plot
  plot_ly(data=plot_data, x=~month,  y = ~get(measure)) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~mode,
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
  top_download
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
  updateCollapse(session, "collapse_commentary", open = "Mode of delivery")
})


output$mod_commentary <- renderUI({
  tagList(
    bsButton("jump_to_mod",label = "Go to data"), #this button can only be used once
    h2("Mode of delivery - 16th December 2020"))
})



