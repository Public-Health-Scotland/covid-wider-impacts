##Server script for pregnancy mode of delivery tab

# Pop-up modal explaining source of data
observeEvent(input$btn_mod_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("These data are derived from the Scottish Morbidity Record 02 (SMR02). An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient obstetric care, mainly categorised as an antenatal, a postnatal or a delivery episode. The data used for these indicators are from the delivery episode and are based on month of discharge from hospital of the woman after the delivery episode. Only singleton live births are included. From October 2019 the guidance for reporting on homebirths was updated, enabling maternity units to submit an SMR02 record for a homebirth."),
               p("Although there is no legal requirement to submit these data to PHS, the level of submission falls only slightly short of the National Records for Scotland (NRS) statutory birth registrations. For the period 1 April 2018 to 31 March 2019, live births recorded on SMR02 represented 98.4% of the live births registered with NRS. Further information based on SMR02 data is also available from the annual ",
                 tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/births-and-maternity/", "Births in Scottish Hospitals report",class="externallink",target="_blank"),"."),
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
            p("On the ‘Percentage of births by caesarean section’ charts above, the dots joined by a solid black line show the percentage of singleton live births (at all gestations) that were delivered by caesarean section in each month from January 2018 onwards. The solid blue centreline on the chart shows the average (median) percentage of births that were by caesarean section over the period January 2018 to February 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The dotted blue centreline continues that average to allow determination of whether there has subsequently been a change in the percentage of births by caesarean section."))
  
  # Function to create common layout to all immunisation charts
  mod_layout <- function(mod_trend_csection_all,mod_trend_csection_elec,mod_trend_csection_emer,mod_linechart_number,mod_linechart_age_n, mod_linechart_age_p,mod_linechart_dep_n, mod_linechart_dep_p){
    tagList(fluidRow(column(12,
                            h4(mod_title),
                            actionButton("btn_mod_rules", "How do we identify patterns in the data?")),
                     column(4,
                            h4("All caesarean sections"),
                            withSpinner(plotlyOutput("mod_trend_csection_all"))),
                     column(4,
                            h4("Elective caesarean sections"),
                            withSpinner(plotlyOutput("mod_trend_csection_elec"))),
                     column(4,
                            h4("Emergency caesarean sections"),
                            withSpinner(plotlyOutput("mod_trend_csection_emer"))),
                     column(12,
                            p(mod_data_timeperiod),
                            p(chart_explanation)),
                     column(12,
                            br(), #spacing
                            h4(paste0("Number of singleton live births by method of delivery: ",input$geoname_mod))),
                     column(12,
                            withSpinner(plotlyOutput("mod_linechart_number"))),
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

                     ))}
  
  # #link plot functions to layouts
  mod_layout(mod_trend_csection_all="mod_trend_csection_all",
             mod_trend_csection_elec="mod_trend_csection_elec",
             mod_trend_csection_emer="mod_trend_csection_emer",
             mod_linechart_age_n="mod_linechart_age_n",
             mod_linechart_age_p="mod_linechart_age_p",
             mod_linechart_dep_n="mod_linechart_dep_n",
             mod_linechart_dep_p="mod_linechart_dep_p",
             mod_linechart_number="mod_linechart_number")
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
    
    # chart legend labels  
    centreline_name <- paste0(input$geoname_mod," average up to end Feb 2020") 
    
    # format y axis
    yname <- "Percentage of births delivered by caesarean (%)"
    yaxis_plots[["range"]] <- c(0, 50)  # forcing range from 0 to 40%
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    
    # chart x-axis range with some extra spacing so that markers are not cut in half at start and end of chart  
    xaxis_plots[["range"]] <- c(min(plot_data$month)-20, max(plot_data$month)+20)
    
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
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }}


#####################################################################################################################
## LINECHART SCOTLAND: caesarean delivery by age group and deprivation, numbers and percentages - Scotland level only
plot_mod_split <- function(dataset, split, measure){  

  plot_data <- dataset
  
  # Create tooltip for line chart
  tooltip <- c(paste0("Month: ", format(plot_data$month, "%B %Y"),"<br>",
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

#####################################################################################################################
## LINECHART SCOTLAND & NHS BOARD: Births by mode of delivery numbers and percentages

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
    rename(assisted_vaginal_in_breech = assisted_vaginal, spontaneous_vaginal = spontaneous)
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


##END
