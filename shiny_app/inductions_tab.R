##Server script for pregnancy - inductions tab

# Pop-up modal explaining source of data
observeEvent(input$btn_induct_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The data used for the induction of labour page comes from the Scottish Morbidity Record 02 (SMR02) database.  An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care.  From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."),
               p("For the induction of labour page, SMR02 records for episodes of care involving the delivery of a singleton live birth (i.e. one baby, not twins or more) at 37 to 42 weeks gestation inclusive have been used.  The charts presented show the total number of singleton live births at 37-42 weeks gestation, and the number and percentage that followed induction of labour, in each month from January 2018 onwards.  The month is based on the date the woman was discharged from hospital after delivery.  Data is shown at all Scotland level, and for women living in each mainland NHS Board area.  Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown.  However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
               p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete.  Data for the most recent months should be viewed as provisional.  Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high.  For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland.  In addition, the recording of gestation at delivery, and of whether the delivery followed induction of labour, is very complete.  For the period 1 April 2019 to 31 March 2020, gestation was recorded on >99.9% of SMR02 records relating to singleton live births, and whether the delivery followed induction of labour was recorded on 99.4% of records relating to singleton live births at 37-42 weeks gestation."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/births-and-maternity/births-in-scottish-hospitals/", "Births in Scottish Hospitals report",class="externallink",target="_blank"),"."),
               size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_induct_rules,
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
observeEvent(input$btn_modal_simd_induct, { showModal(
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
## Induction Reactive controls  ----
###############################################.

# deliveries reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_induct <- renderUI({
  #Lists areas available in   
  areas_summary_induct <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_induct])
  selectizeInput("geoname_induct", label = NULL, choices = areas_summary_induct, selected = "")
})

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
induct_filter <- function(){
  
  induct_runchart %>% filter(area_name == input$geoname_induct &
                            area_type == input$geotype_induct &
                            type %in% c("Scotland","Health board")) 
}

#Dataset 2: behind line charts for age and deprivation (available for scotland only)
induct_linechart_split <- function(split){
  
  induct_scot  %>% filter(area_name == "Scotland" &
                         area_type == "Scotland" &
                         type==split)
}

#Dataset 3: behind line chart  (available at scotland and NHS board level)
induct_linechart_filter <- function(){
  
  induct_linechart %>% filter(area_name == input$geoname_induct &
                                area_type == input$geotype_induct &
                                type %in% c("Scotland","Health board"))
}

###############################################.
## Induction Chart calls to chart function ----
###############################################.

# chart outputs for trend
output$induct_trend <- renderPlotly({plot_induct_trend(measure="perc_ind_37_42",shift = "induction_shift", trend = "induction_trend")})

#chart outputs for line charts for NHS board and Scot
output$induct_linechart_number <- renderPlotly({plot_induct_linechart(measure="births")})
output$induct_linechart_percent <- renderPlotly({plot_induct_linechart(measure="percent_births")})

#chart outputs for line charts for Scotland only age and deprivation line charts
output$induct_linechart_age_n <- renderPlotly({plot_induct_split(dataset=induct_linechart_split(split="age"),split="age", measure="ind_37_42")})
output$induct_linechart_age_p <- renderPlotly({plot_induct_split(dataset=induct_linechart_split(split="age"),split="age", measure="perc_ind_37_42")})
output$induct_linechart_dep_n <- renderPlotly({plot_induct_split(dataset=induct_linechart_split(split="dep"),split="dep", measure="ind_37_42")})
output$induct_linechart_dep_p <- renderPlotly({plot_induct_split(dataset=induct_linechart_split(split="dep"),split="dep", measure="perc_ind_37_42")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$induct_explorer <- renderUI({
  
  # text for titles of cut charts
  induct_data_timeperiod <-  paste0("Figures based on data extracted ",induct_extract_date)
  induct_title <- paste0("of singleton live births at 37-42 weeks gestation that followed induction of labour: ",input$geoname_induct)
  
  chart_explanation <- 
    tagList(p("We have used ",                      
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p("On the ‘Percentage of births that followed induction of labour’ chart above, the dots joined by a solid black line show the percentage of singleton live births at 37-42 weeks gestation that followed induction of labour in each month from January 2018 onwards. The solid blue centreline on the chart shows the average (median) percentage of births that followed induction of labour over the period January 2018 to February 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The dotted blue centreline continues that average to allow determination of whether there has subsequently been a change in the percentage of deliveries following induction of labour."))
  
  # Function to create common layout to all immunisation charts
  induct_layout <- function(induct_trend,induct_linechart_number,induct_linechart_age_n,induct_linechart_age_p,induct_linechart_dep_n,induct_linechart_dep_p){
    tagList(fluidRow(column(12,
                            h4(paste0("Percentage ", induct_title)),
                            actionButton("btn_induct_rules", "How do we identify patterns in the data?"),
                            withSpinner(plotlyOutput("induct_trend"))),
                     column(12,
                            p(induct_data_timeperiod),
                            p(chart_explanation)),
                     column(12,
                            br(), #spacing
                            h4(paste0("Number of singleton live births at 37-42 weeks gestation that followed induction of labour: ",input$geoname_induct))),
                     column(12,
                            withSpinner(plotlyOutput("induct_linechart_number"))),
                     #only if scotland selected display age and deprivation breakdowns
                     if (input$geotype_induct == "Scotland"){
                       tagList(
                         fluidRow(column(12,
                                         h4("Singleton live births at 37-42 weeks gestation that followed induction of labour by maternal age group: Scotland"))),
                         fluidRow(column(6,
                                         h4("Number of births that followed induction of labour"),
                                         withSpinner(plotlyOutput("induct_linechart_age_n"))),
                                  column(6,
                                         h4("Percentage of births that followed induction of labour"),
                                         withSpinner(plotlyOutput("induct_linechart_age_p")))),
                         fluidRow(column(12,
                                         br(), # spacing
                                         h4("Singleton live births at 37-42 weeks gestation that followed induction of labour by maternal deprivation level: Scotland"),
                                         actionButton("btn_modal_simd_induct", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         fluidRow(column(6,
                                         h4("Number of births that followed induction of labour "),
                                         withSpinner(plotlyOutput("induct_linechart_dep_n"))),
                                  column(6,
                                         h4("Percentage of births that followed induction of labour"),
                                         withSpinner(plotlyOutput("induct_linechart_dep_p"))))
                       )#tagList from if statement
                     }

                     ))}

                     
           
  # #link plot functions to layouts
  induct_layout(induct_trend="induct_trend",
             induct_linechart_number="induct_linechart_number",  
             induct_linechart_age_n="induct_linechart_age_n",
             induct_linechart_age_p="induct_linechart_age_p",
             induct_linechart_dep_n="induct_linechart_dep_n",
             induct_linechart_dep_p="induct_linechart_dep_p")
})


#############################################.
## Induction chart functions ----
############################################.

## RUNCHART trend chart for monthly inductions percentages: Scotland & NHS Board (except island boards) 
## Function could be simplified to run without parameters but copied logic from other pregnancy tabs therefore easier to keep same structure.

plot_induct_trend <- function(measure, shift, trend){  
  plot_data <- induct_filter()
  
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 65, 
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {
    
    # chart legend labels  
    centreline_name <- paste0(input$geoname_induct," average up to end Feb 2020") 
    
    # format y axis
    yname <- "Percentage of births that followed induction (%)"
    yaxis_plots[["range"]] <- c(0, 60)  # forcing range from 0 to 60%
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    
    # chart x-axis range with some extra spacing so that markers are not cut in half at start and end of chart  
    xaxis_plots[["range"]] <- c(min(plot_data$month)-20, max(plot_data$month)+20)

    #specify tool tip
    tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                            "Percentage: ",format(plot_data$perc_ind_37_42,digits = 1,nsmall=1),"%", "<br>"))
    
    #Creating time trend plot
    plot_ly(data=plot_data, x=~month) %>%
      add_lines(y = ~perc_ind_37_42,  
                line = list(color = "black"), text=tooltip_top, hoverinfo="text",
                marker = list(color = "black"), name = yname ) %>% 
      add_lines(y = ~ext_ind_37_42, name = FALSE,
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline", showlegend = FALSE) %>%
      add_lines(y = ~median_ind_37_42, name = centreline_name,
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
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }}

#####################################################################################################################
## LINECHART SCOTLAND & NHS BOARD: births (37-42 weeks gestation) where delivery induced, numbers and percentages - Scotland level only
plot_induct_linechart <- function(measure){  
  
  plot_data <- induct_linechart_filter() 
  
  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(ind = factor(ind, levels = c("Births that followed induction", "All births")))
  #pick a colour palette to apply
  pallette <- pal_age
  
  # adjust chart y axis according to what is being displayed
  if(measure == "percent_births"){
    yaxis_plots[["title"]] <- "Percentage of births (%)" 
    yaxis_plots[["range"]] <- c(0, 60)  # forcing range from 0 to 60%

        plot_data <- plot_data %>%  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
      filter(ind!="Total births (37-42 weeks)")
   
  }
  
  if(measure == "births"){
    yaxis_plots[["title"]] <- "Number of births"
    # plot_data <- plot_data %>% #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
    #   filter(ind!="37 to 42 weeks")
  }
  # Create tooltip for line chart
  tooltip <- c(paste0( plot_data$ind,"<br>",
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
                color = ~ind, colors = pallette,
                text= tooltip, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)}
}


#####################################################################################################################
## LINECHART SCOTLAND: inductced deliveries by age group and deprivation, numbers and percentages - Scotland level only
plot_induct_split <- function(dataset, split, measure){  
  
  plot_data <- dataset
  
  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))
  
  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Month: ", format(plot_data$month, "%B %Y"),"<br>",
                      "Number: ", plot_data$ind_37_42, "<br>",
                      "Percentage: ", format(plot_data$perc_ind_37_42,digits=1,nsmall = 1),"%"))
  
  # adjust chart y axis according to what is being displayed
  if(measure == "perc_ind_37_42"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    if(split == "age"){
      yaxis_plots[["range"]] <- c(0, 60)}  # forcing range from 0 to 70% for age group
    if(split == "dep"){
      yaxis_plots[["range"]] <- c(0, 50)}  # forcing range from 0 to 40% for dep
  }
  if(measure == "ind_37_42"){
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

induct_download_data <- reactive({
  induct_download
})

output$download_induct_data <- downloadHandler(
  filename ="induced_deliveries_extract.csv",
  content = function(file) {
    write_csv(induct_download_data(),
              file) } 
)

###############################################.
## Commentary tab content  ----
###############################################.

#action associated with action links within commentary text - this observe event linked to an actionLink within the TOP commentary which will take the user from TOP commentary to ANB commentary easily.
observeEvent(input$switch_to_induction,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Induction of labour")
})


output$induction_commentary <- renderUI({
  tagList(
    bsButton("jump_to_induction",label = "Go to data"), #this button can only be used once
    h2("Induction of labour - 16th December 2020"),
    p("Information on induction of labour was included in this tool for the first time on 16 December 2020."),
    p("'",
      tags$a(href="https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/getting-ready-for-the-birth/induced-labour", "Induction of labour",class="externallink",target="_blank"),
      "' is when a woman is given a medical intervention to start her labour, rather than waiting for labour to start spontaneously.  It is offered because there are medical reasons meaning it is considered safer (for the mother or baby) for the baby to be born, or because a woman is past her ‘due date’.  There are different approaches to inducing labour, for example using medicines, a medical ‘balloon’ device that sits at the neck of the womb, and/or breaking the woman’s waters."),
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
    p("The information on induction of labour presented through this tool is taken from hospital discharge records, specifically records relating to the care of women delivering a singleton live birth (i.e. one baby, not twins or more) at 37-42 weeks gestation (i.e. up to 3 weeks before or after their due date).  Further technical information is available through the ‘Data source’ button on the dashboard page."),
    p("The data shows that, at all Scotland level, the percentage of singleton live births at 37-42 weeks gestation that followed induction of labour (the ‘induction rate’) has remained broadly constant (at around 34%) from January 2018 (when the data shown starts) to end September 2020 (the latest point for which data is currently available).  Prior to the COVID-19 pandemic, the induction rate was somewhat variable between NHS Board areas of residence.  There is also variation between areas in how the induction rate has changed around the time of the pandemic, with some areas (for example NHS Ayrshire & Arran and NHS Greater Glasgow & Clyde) showing a small increase and other areas (for example NHS Dumfries & Galloway, NHS Forth Valley, and NHS Lanarkshire) showing a small decrease."),
    p("The induction rate tends to be highest among mothers in the youngest (<20 years) and oldest (40+ years) age groups, and among mothers living in the most deprived areas of Scotland.  These patterns have persisted during the COVID-19 pandemic.")
     )
})


##END
