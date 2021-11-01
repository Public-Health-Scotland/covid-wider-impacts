
##Server script for births and babies - apgar tab..


###############################################.
## Modals ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_tears_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
      p("The data used for the perineal tears page comes from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended home births."),
      p("For the perineal tears page, SMR02 records for episodes of care involving the delivery of a singleton live or stillborn baby (i.e. one baby, not twins or more) at 37-42 weeks gestation inclusive have been used. The charts presented show the total number of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation with known perineal tear status, and the percentage of women who have a third or fourth degree perineal tear. Data is presented for January 2018 onwards, based on the date the woman was discharged from hospital after delivery."),
      p("Data is presented for individual months at Scotland level, and for sequential quarters (Jan-Mar 2018, Apr-Jun 2018, etc) for individual NHS Boards. Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown. However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
      p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete. Data for the most recent months should be viewed as provisional. Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
      p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland (NRS) and stillbirths on SMR02 represented 91.1% of stillbirths registered with NRS. In addition, the recording of specific data items allowing identification of delivery of a singleton live or stillborn baby at 37-42 weeks gestation is very complete. "),
      p("SMR02 delivery records allow the recording of both mode of delivery (i.e. how the baby was delivered: spontaneous or assisted vaginal delivery, Caesarean section, etc) and the presentation of the baby at delivery prior to any operative manipulation (i.e. which part of the baby was lowest in the maternal pelvis: cephalic [‘head first’], breech, etc).  The completeness and accuracy of mode of delivery recording is very high whereas the completeness and accuracy of presentation recording is slightly less good, in particular from some hospitals.  For the period 1 April 2018 to 31 December 2020, 99.9% of records relating to a singleton live or stillbirth had a meaningful value (not unknown or missing) recorded for  ‘Mode of Delivery’.  When producing this indicator, we have therefore relied on the mode of delivery variable alone to identify vaginal deliveries of babies with a cephalic presentation.  Records with mode of delivery coded as spontaneous cephalic or assisted vaginal (forceps or ventouse) have been assumed to identify vaginal deliveries of babies with a cephalic presentation.  Babies with other (breech delivery, Caesarean section) or unknown mode of delivery recorded have been excluded.  As this indicator only includes deliveries of singleton babies at 37-42 weeks gestation, this is considered a reasonable assumption.  It is unlikely that these babies will have been manipulated from a non-cephalic to a cephalic presentation prior to assisted vaginal delivery, as may happen for preterm babies or second twins."),
      p("Perineal tears are classified as 1st to 4th degree, with 4th degree tears being the most serious.  On SMR02 delivery records, a specific data item captures information on whether a woman had a perineal tear.  This item is coded as 0 (no tear), 1-4 (1st to 4th degree tear respectively), 8 (tear of unspecified degree), or 9 (unknown whether there was a tear or not).  The completeness of tear recording is very high.  For the period 1 April 2018 to 31 December 2020, 99.6% of records relating to a singleton live or stillbirth had a meaningful value (not unknown or missing) recorded for  ‘Perineal Tears’.  Other forms of injury to the birth canal, for example isolated high vaginal tears, or cervical tears, are less common than perineal tears.  These other injuries are recorded separately on SMR02 and have not been considered as part of this indicator."),
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
    tagList(
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
  
  chart_explanation_quarter <- 
    tagList(
      p("On the ‘Percentage of women who have a third or fourth degree 
              perineal tear’ chart above, the dots joined by a solid black line 
              show the percentage of women giving birth vaginally to a singleton 
              live or stillborn baby with a cephalic presentation between 37-42 
              weeks gestation who have a third or fourth degree perineal tear, 
              in each quarter from January 2018 onwards. The solid blue centreline 
              on the chart shows the average (median) percentage of women who 
              have a third or fourth degree perineal tear over the period 
              January 2018 to February 2020 inclusive (the period before the 
              COVID-19 pandemic in Scotland). The dotted blue centreline 
              continues that average to allow determination of whether there has 
              subsequently been a change in the percentage of women who have a 
              third or fourth degree perineal tear."))
  
  # Layout depending if Scotland or HB selected
   if (input$geotype_tears == "Health board"){
     tagList(fluidRow(column(12,
                             h4(paste0("Percentage ", tears_title)),
                             actionButton("btn_tears_rules", "How do we identify patterns in the data?",
                                          icon = icon('question-circle')),
                             withSpinner(plotlyOutput("tears_trend"))),
                      column(12,
                             p(tears_data_timeperiod),
                             p(chart_explanation_quarter)),
                      column(12,
                             br(), #spacing
                             h4(paste0("Number of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a third or fourth degree perineal tear: ",input$geoname_tears))),
                      column(12,
                             withSpinner(plotlyOutput("tears_linechart_number")))))

    } else if (input$geotype_tears == "Scotland"){
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
                                   withSpinner(plotlyOutput("tears_linechart_number")))),
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
    h2("Perineal tears - 3rd November 2021"),
    p("The percentage of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a third or fourth degree perineal tear has been above the pre-pandemic average for seven successive quarters (including April-June 2021) in both NHS Dumfries & Galloway and NHS Greater Glasgow & Clyde. We are working with these two boards in order to investigate this further. NHS Dumfries & Galloway have indicated that their overall numbers of third and fourth degree tears are small. They routinely review all women who have had a third or fourth degree tear at their Clinical Incident Review Group and they have not so far identified any common themes."),
    h2("Perineal tears - 16th June 2021"),
    p("Information on perineal tears was included in this tool for the first time on 16 June 2021."),
    p("When a woman is giving birth, the baby stretches the mother’s vagina and perineum.  Occasionally, the tissues cannot stretch enough, and a tear (called a ",                      
                 tags$a(href= 'https://www.rcog.org.uk/en/patients/patient-leaflets/third--or-fourth-degree-tear-during-childbirth/',
                        "'perineal tear'", target="_blank"),") occurs.  The perineum is the area between a woman’s vagina and anus."),
    p("Perineal tears are classified as 1st to 4th degree, with 4th degree tears being the most serious.  First degree tears just involve the skin of the perineum or lining of the lower vagina.  Second degree tears also involve the muscles of the perineum.  Third degree tears extend further back and also involve the muscles surrounding the anus.  Fourth degree tears extend further into the lining of the anus or rectum (lower bowel)."),
    p("Third and 4th degree tears are also known as obstetric anal sphincter injury.  These tears require surgical repair immediately after delivery.  Most women recover completely following a 3rd or 4th degree tear, however some are left with persistent problems controlling their bowels (anal incontinence)."),
    p("Most tears are unexpected and it’s hard to predict which women will have a tear, although tears are more common during a woman’s first vaginal delivery, if the baby is big (over 4kg birthweight), or if the second stage of labour goes on for a long time."),
    p("An ",                      
      tags$a(href= 'https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/assisted-birth/perineal-tears-and-episiotomy',
             'episiotomy', target="_blank")," may be offered if a woman is thought to be at risk of a tear.  An episiotomy is a controlled cut made by a healthcare professional through the vaginal wall and perineum that is repaired with stitches after delivery.  An episiotomy does not guarantee that a tear will not happen, as the episiotomy cut may extend and become a tear.  Women requiring assisted vaginal delivery (with forceps or ventouse) are at high risk of a tear so would generally be offered an episiotomy."),
    p("Care for women around the time they are giving birth is an essential, time critical service that cannot be deferred.  As such, it has been provided throughout the COVID-19 pandemic, and maternity staff have not been redeployed to support other services."),
    p("However, there have been some changes to how delivery care is provided in response to COVID-19, to minimise the risk of infection and to allow services to continue to provide safe care during times when a high number of staff may be off work, for example due to needing to isolate.  These changes have varied over time and between areas.  For example, guidance issued by the  ",                      
      tags$a(href= 'https://www.gov.scot/collections/coronavirus-covid-19-guidance/#health,careandsocialwork',
             'Scottish Government', target="_blank")," and ",
      tags$a(href= 'https://www.rcog.org.uk/coronavirus-pregnancy',
             'Royal College of Obstetricians and Gynaecologists', target="_blank")," to maternity services at the height of the first wave of the pandemic (spring 2020) noted that it may be necessary for services to temporarily suspend the option for women to deliver at home or in midwife led units, and to concentrate delivery care within obstetric units.  This tool allows us to monitor whether changes to care provision associated with COVID-19 have led to any changes in the outcomes of women or babies."),
    p("The information on perineal tears presented through this tool is taken from hospital discharge records, specifically records relating to the care of women undergoing spontaneous or assisted vaginal delivery of a singleton live or stillborn baby (i.e. one baby, not twins or more) with cephalic (i.e. ‘head first’) presentation at 37-42 weeks gestation (i.e. up to 3 weeks before or after their due date).  Further technical information is available through the ‘Data source’ button on the dashboard page."),
    p("The data shows that, at all Scotland level, the percentage of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a 3rd or 4th degree perineal tear (the ‘tear rate’) has remained broadly constant at around 3.5% from January 2018 (when the data shown starts) to end February 2021 (the latest point for which data is currently available)."),
    p("The tear rate varies somewhat between NHS Board areas of residence: for example the average rates in mainland Boards in the period prior to the COVID-19 pandemic ranged from 1.5% for women in NHS Dumfries & Galloway to 4.6% in NHS Lothian.  No areas have shown a clear change in the tear rate since the start of the COVID-19 pandemic."),
    p("The tear rate does not show any clear relationship to maternal age.  The tear rate tends to be highest among mothers living in the least deprived areas of Scotland, and this pattern has not changed during the COVID-19 pandemic.")
    )
    
})


##END
