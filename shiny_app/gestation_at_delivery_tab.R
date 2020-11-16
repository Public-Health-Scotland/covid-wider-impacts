##Server script for pregnancy gestation at delivery tab


# Pop-up modal explaining source of data
observeEvent(input$btn_gest_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("need some details about SMR02"),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_gest_rules,
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
observeEvent(input$btn_modal_simd_gest, { showModal(
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

#Dataset behind trend run chart  (available at scotland and NHS board level)
gest_filter <- function(){
  
  gestation_runchart %>% filter(area_name == input$geoname_gest &
                            area_type == input$geotype_gest &
                            type %in% c("Scotland","Health board")) 
}

#Dataset behind line charts for age and deprivation (available for scotland only)
gest_linechart_split <- function(split){
  
  gestation_scot  %>% filter(area_name == "Scotland" &
                         area_type == "Scotland" &
                         type==split)
}
# 
# #Dataset behind line chart  (available at scotland and NHS board level)
# gest_linechart_filter <- function(){
#   
#   mod_linechart %>% filter(area_name == input$geoname_mod &
#                              area_type == input$geotype_mod &
#                              type %in% c("Scotland","Health board"))
# }

###############################################.
## Gestation at delivery Charts ----
###############################################.

# chart outputs for trend
output$gest_trend_u32 <- renderPlotly({plot_gest_trend(measure="perc_under32", shift = "gest_under32_shift", trend = "gest_under32_trend")})
output$gest_trend_u37 <- renderPlotly({plot_gest_trend(measure="perc_under37", shift = "gest_under37_shift", trend = "gest_under37_trend")})
output$gest_trend_32_36 <- renderPlotly({plot_gest_trend(measure="perc_32_36", shift = "gest_32_36_shift", trend = "gest_32_36_trend")})
output$gest_trend_42plus <- renderPlotly({plot_gest_trend(measure="perc_42plus", shift = "gest_42plus_shift", trend = "gest_42plus_trend")})
# 
# output$mod_linechart_number <- renderPlotly({plot_mod_linechart(measure="births")})
# output$mod_linechart_percent <- renderPlotly({plot_mod_linechart(measure="percent_births")})

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
  gest_title <- paste0("Percentage of singleton live births by gestation at delivery: ",input$geoname_gest)

  chart_explanation <- 
    tagList(p("We have used ",                      
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p("On the ‘Percentage of births by gestation at delivery’ charts above, the dots joined by a solid black line show the percentage of births by gestation at delivery in each month from January 2018 onwards.  The solid blue centreline on the chart shows the average (median) number of deliveries by over the period January 2018 to February 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The dotted blue centreline continues that average to allow determination of whether there has subsequently been a change in the number of caesarean sections."))
  
  # Function to create common layout to all immunisation charts
  gest_layout <- function(gest_trend_u32,gest_trend_u37,gest_trend_32_36,gest_trend_42plus,gest_linechart_age_n,gest_linechart_age_p,gest_linechart_dep_n,gest_linechart_dep_p ){
    tagList(fluidRow(column(12,
                            h4(gest_title),
                            actionButton("btn_gest_rules", "How do we identify patterns in the data?")),
                     column(6,
                            h4("Percentage of singleton live births delivered at <32 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_u32")),
                            h4("Percentage of singleton live births delivered at 32-36 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_32_36"))),
                     column(6,
                            h4("Percentage of singleton live births delivered at <37 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_u37")),
                     #column(4,
                            h4("Percentage of singleton live births delivered at >41 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_42plus"))),
                     column(12,
                            p(chart_explanation)),
                     #only if scotland selected display age and deprivation breakdowns
                     if (input$geotype_gest == "Scotland"){
                       tagList(
                         fluidRow(column(12,
                                         h4("Singleton live births delivered at <37 weeks gestation by maternal age: Scotland"))),
                         fluidRow(column(6,
                                         h4("Number of births at <37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_age_n"))),
                                  column(6,
                                         h4("Percentage of births at <37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_age_p")))),
                         fluidRow(column(12,
                                         br(), # spacing
                                         h4("Singleton live births delivered at <37 weeks gestation by deprivation: Scotland"),
                                         actionButton("btn_modal_simd_gest", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         fluidRow(column(6,
                                         h4("Number of births at <37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_dep_n"))),
                                  column(6,
                                         h4("Percentage of births at <37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_dep_p"))))
                       )#tagList from if statement
                     }
                     # column(12,
                     #        br(), #sapcing
                     #        h4(paste0("Singleton live births by mode of delivery: ",input$geoname_mod))),
                     # column(6,
                     #        p("Number of births"),
                     #        withSpinner(plotlyOutput("mod_linechart_number"))),
                     # column(6,
                     #        p("Percentage of births"),
                     #        withSpinner(plotlyOutput("mod_linechart_percent")))
    ))}
  
  # #link plot functions to layouts
  gest_layout(gest_trend_u32="gest_trend_u32",
             gest_trend_u37="gest_trend_u37",
             gest_trend_32_36="gest_trend_32_36",
             gest_trend_42plus="gest_trend_42plus",
             gest_linechart_age_n="gest_linechart_age_n",
             gest_linechart_age_p="gest_linechart_age_p",
             gest_linechart_dep_n="gest_linechart_dep_n",
             gest_linechart_dep_p="gest_linechart_dep_p")})
             # mod_linechart_number="mod_linechart_number",
             # mod_linechart_percent="mod_linechart_percent")


#############################################.
## Gestation chart functions ----
############################################.

## Runchart trend chart for monthly c-section percentages : Scotland & NHS Board (except island boards) 
## Rather than try and present all the modes of delivery we have opted just to produce a run chart
## showing rates of c-section (by type all, emergency, elective) as these are the modes of deliver that people most want to see

plot_gest_trend <- function(measure, shift, trend){  
  
  plot_data <- gest_filter()
  
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50, 
                text_nodata = "Data not shown due to small numbers. Data for the Island Boards is included in the Scotland total")
  } else {
    
    # chart legend labels  
    centreline_name <- paste0(input$geoname_gest," average up to end Feb 2020") 
    
    # format y axis
    yname <- "Percentage births by gestation at delivery"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 8%
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    
    # chart x-axis range with some extra spacing so that markers are not cut in half at start and end of chart  
    xaxis_plots[["range"]] <- c(min(plot_data$month)-20, max(plot_data$month)+20)
    
    tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                            "Percentage: ",format(measure,digits = 1,nsmall=1),"%", "<br>"))
    #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed
    
    # Adjust the column used for median line according to which cut of chart to be shown
    centre_line <- case_when(measure == "perc_under32" ~ plot_data$median_under32,
                             measure == "perc_under37" ~ plot_data$median_under37,
                             measure == "perc_32_36" ~ plot_data$median_32_36,
                             measure == "perc_42plus" ~ plot_data$median_42plus)
    dotted_line <- case_when(measure == "perc_under32" ~ plot_data$ext_under32,
                              measure == "perc_under37" ~ plot_data$ext_under37,
                              measure == "perc_32_36" ~ plot_data$ext_32_36,
                              measure == "perc_42plus" ~ plot_data$ext_42plus)                      
    
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
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }}

## LINECHART SCOTLAND: caesarean delivery by age group and deprivation, numbers and percentages - Scotland level only
plot_gest_split <- function(dataset, split, measure){  
  
  plot_data <- dataset

  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))
  
  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Month: ", format(plot_data$month, "%B %Y"),"<br>",
                      "Number: ", plot_data$csection_all, "<br>",
                      "Percentage: ", format(plot_data$perc_under37,digits=1,nsmall = 1),"%"))
  
  # adjust chart y axis according to what is being displayed
  if(measure == "perc_under37"){
     yaxis_plots[["title"]] <- "Percentage births (%)"  
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
observeEvent(input$switch_to_gest,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Gestation at delivery")
})


output$gest_commentary <- renderUI({
  tagList(
    bsButton("gest_at_delivery",label = "Go to data"), #this button can only be used once
    h2("Gestation at delivery - 16th December 2020"))
})

