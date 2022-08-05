# Wider impacts dashboard - Births and babies tab - Gestation at delivery section
# Server code

###############################################.
## Modals ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$`gest-source-modal`,
             showModal(modalDialog(
               title = "What is the data source?",
               p("The data used for the gestation at delivery page comes from the Scottish Morbidity Record 02 (SMR02) database.  An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care.  From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."),
               p("For the gestation at delivery page, SMR02 records for episodes of care involving the delivery of a singleton live birth (i.e. one baby, not twins or more) with known gestation (at 18-44 weeks inclusive) have been used.  The charts presented show the total number of singleton live births with known gestation, and the number and percentage that were delivered in the different gestation categories, in each month from January 2018 onwards.  Gestation at delivery has been categorised as under 32 weeks (very preterm); 32-36 weeks (moderately preterm); 37-41 weeks (term); and 42 weeks or over (post-term).  The month is based on the date the woman was discharged from hospital after delivery.  Data is shown at all Scotland level, and for women living in each mainland NHS Board area.  Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown.  However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
               p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete.  Data for the most recent months should be viewed as provisional.  Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high.  For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland.  In addition, the recording of gestation at delivery is very complete.  For the period 1 April 2019 to 31 March 2020, gestation was recorded on >99.9% of SMR02 records relating to singleton live births."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href="https://publichealthscotland.scot/publications/births-in-scottish-hospitals", "Births in Scottish Hospitals report",class="externallink",target="_blank"),"."),
               size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_gest_rules, runchart_modal())
#Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd_gest, simd_modal("Women"))

###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("gest")

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
gest_filter <- function(){

  gestation_runchart %>% filter(area_name == input$`gest-geoname` &
                            area_type == input$`gest-geotype` &
                            type %in% c("Scotland","Health board"))
}

#Dataset 2: behind line charts for age and deprivation (available for scotland only)
gest_linechart_split <- function(split){

  gestation_scot  %>% filter(area_name == "Scotland" &
                         area_type == "Scotland" &
                         type==split)
}

#Dataset 3: behind line chart  (available at scotland and NHS board level)
gest_linechart_filter_one <- function(){

  gestation_linechart %>% filter(area_name == input$`gest-geoname` &
                             area_type == input$`gest-geotype` &
                             type %in% c("Scotland","Health board") &
                               gest %in% c("37 to 41 weeks", "All gestations (18-44 weeks)"))
}

gest_linechart_filter_two <- function(){

  gestation_linechart %>% filter(area_name == input$`gest-geoname` &
                                   area_type == input$`gest-geotype` &
                                   type %in% c("Scotland","Health board") &
                                   gest %in% c("Under 32 weeks", "32 to 36 weeks", "42 weeks plus"))
}

###############################################.
## Charts ----
###############################################.

# chart outputs for trend
output$gest_trend_u32 <- renderPlotly({plot_gest_trend(measure="perc_under32", shift = "gest_under32_shift", trend = "gest_under32_trend")})
output$gest_trend_u37 <- renderPlotly({plot_gest_trend(measure="perc_under37", shift = "gest_under37_shift", trend = "gest_under37_trend")})
output$gest_trend_32_36 <- renderPlotly({plot_gest_trend(measure="perc_32_36", shift = "gest_32_36_shift", trend = "gest_32_36_trend")})
output$gest_trend_42plus <- renderPlotly({plot_gest_trend(measure="perc_42plus", shift = "gest_42plus_shift", trend = "gest_42plus_trend")})

#chart outputs for line charts for NHS board and Scot
output$gest_linechart_number <- renderPlotly({plot_gest_linechart(measure="births")})
output$gest_linechart_percent <- renderPlotly({plot_gest_linechart(measure="percent_births")})

output$gest_linechart_number_37_41_all <- renderPlotly({plot_gest_linechart_one(measure="births")})
output$gest_linechart_number_32_3236_42plus <- renderPlotly({plot_gest_linechart_two(measure="births")})

#chart outputs for line charts for Scotland only age and deprivation line charts
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
  gest_title <- paste0("Percentage of singleton live births delivered at stated gestation: ",input$`gest-geoname`)

  chart_explanation <-
    tagList(p("We have used ",
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p(run_chart_description("Percentage of births at stated gestation",
                                    "the percentage of singleton live births
                                    with known gestation (18-44 weeks) that were
                                    delivered at the stated gestation in each
                                    month from January 2018 onwards",
                                    "the average (median) percentage of births
                                    in each gestation group over the period
                                    January 2018 to February 2020 inclusive (the
                                    period before the COVID-19 pandemic in
                                    Scotland)",
                                    charts_plural = TRUE)))

  # Function to create common layout to all immunisation charts
  gest_layout <- function(gest_trend_u32,gest_trend_u37,gest_trend_32_36,gest_trend_42plus,gest_linechart_age_n,gest_linechart_age_p,gest_linechart_dep_n,gest_linechart_dep_p,gest_linechart_number_37_41_all,gest_linechart_number_32_3236_42plus){
    tagList(fluidRow(column(12,
                            h4(gest_title),
                            actionButton("btn_gest_rules", "How do we identify patterns in the data?")),
                     column(6,
                            h4("Under 32 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_u32",
                                                     height = height_run_chart)),
                            h4("32-36 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_32_36",
                                                     height = height_run_chart))),
                     column(6,
                            h4("Under 37 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_u37",
                                                     height = height_run_chart)),
                     #column(4,
                            h4("At or over 42 weeks gestation"),
                            withSpinner(plotlyOutput("gest_trend_42plus",
                                                     height = height_run_chart))),
                     column(12,
                            br(), # spacing
                            p(gest_data_timeperiod),
                            p(chart_explanation)),
                     column(12,
                            br(), #spacing
                            h4(paste0("Number of singleton live births delivered at stated gestation: ",input$`gest-geoname`))),
                     column(6,
                            h4("37-41 weeks gestation and all births"),
                            withSpinner(plotlyOutput("gest_linechart_number_37_41_all"))),
                     column(6,
                            h4("Under 32 weeks, 32-36 weeks, and at or over 42 weeks gestation"),
                            withSpinner(plotlyOutput("gest_linechart_number_32_3236_42plus"))),
                     #only if scotland selected display age and deprivation breakdowns
                     if (input$`gest-geotype` == "Scotland"){
                       tagList(
                         fluidRow(column(12,
                                         h4("Singleton live births delivered at under 37 weeks gestation by maternal age group: Scotland"))),
                         fluidRow(column(6,
                                         h4("Number of births at under 37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_age_n"))),
                                  column(6,
                                         h4("Percentage of births at under 37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_age_p")))),
                         fluidRow(column(12,
                                         br(), # spacing
                                         h4("Singleton live births delivered at under 37 weeks gestation by maternal deprivation level: Scotland"),
                                         actionButton("btn_modal_simd_gest", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         fluidRow(column(6,
                                         h4("Number of births at under 37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_dep_n"))),
                                  column(6,
                                         h4("Percentage of births at under 37 weeks gestation"),
                                         withSpinner(plotlyOutput("gest_linechart_dep_p"))))
                       )#tagList from if statement
                     }
    ))}


  # #link plot functions to layouts
  gest_layout(gest_trend_u32="gest_trend_u32",
             gest_trend_u37="gest_trend_u37",
             gest_trend_32_36="gest_trend_32_36",
             gest_trend_42plus="gest_trend_42plus",
             gest_linechart_age_n="gest_linechart_age_n",
             gest_linechart_age_p="gest_linechart_age_p",
             gest_linechart_dep_n="gest_linechart_dep_n",
             gest_linechart_dep_p="gest_linechart_dep_p",
             gest_linechart_number_37_41_all="gest_linechart_number_37_41_all",
             gest_linechart_number_32_3236_42plus="gest_linechart_number_32_3236_42plus")
})


#############################################.
## functions ----
############################################.

## RUNCHART:  trend chart for monthly c-section percentages : Scotland & NHS Board (except island boards)
## Rather than try and present all the modes of delivery we have opted just to produce a run chart
## showing rates of c-section (by type all, emergency, elective) as these are the modes of deliver that people most want to see

plot_gest_trend <- function(measure, shift, trend){

  plot_data <- gest_filter()

  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50,
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {

    # chart legend labels
    centreline_name <- paste0(input$`gest-geoname`," average up to end Feb 2020")
    dottedline_name = "Projected average"

    # format y axis
    measure_name <- "Percentage of births at stated gestation"
    yaxis_plots[["range"]] <- c(0, 15)  # forcing range from 0 to 8%
    y_label <- "Percentage of births (%)"



    #switch tooltip according to which measure is provided
    if(measure == "perc_under32"){
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_under32,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_under37") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_under37,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_32_36") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_32_36,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_42plus") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_42plus,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed
    }


    # Adjust the column used for median line according to which cut of chart to be shown
    centreline_data <- case_when(measure == "perc_under32" ~ plot_data$median_under32,
                                 measure == "perc_under37" ~ plot_data$median_under37,
                                 measure == "perc_32_36" ~ plot_data$median_32_36,
                                 measure == "perc_42plus" ~ plot_data$median_42plus)
    dottedline_data <- case_when(measure == "perc_under32" ~ plot_data$ext_under32,
                                 measure == "perc_under37" ~ plot_data$ext_under37,
                                 measure == "perc_32_36" ~ plot_data$ext_32_36,
                                 measure == "perc_42plus" ~ plot_data$ext_42plus)

    x_dates = "month"

    plot_run_chart(plot_data, measure, measure_name, y_label,
                   x_dates, shift, trend, tooltip_top,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_buffer = 20)


  }}


#####################################################################################################################.
## LINECHART SCOTLAND: caesarean delivery by age group and deprivation, numbers and percentages - Scotland level only
plot_gest_split <- function(dataset, split, measure){

  plot_data <- dataset

  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))

  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Month: ", format(plot_data$month, "%B %Y"),"<br>",
                      "Number: ", plot_data$births_under37, "<br>", #number of csections has been removed from dataset
                      "Percentage: ", format(plot_data$perc_under37,digits=1,nsmall = 1),"%"))

  # adjust chart y axis according to what is being displayed
  if(measure == "perc_under37"){
     yaxis_plots[["title"]] <- "Percentage of births (%)"
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

#####################################################################################################################.
## LINECHART SCOTLAND & NHS BOARD: Births by gestation numbers and percentages

plot_gest_linechart_one <- function(measure){

  plot_data <- gest_linechart_filter_one()

  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(gest = factor(gest, levels = c("37 to 41 weeks", "All gestations (18-44 weeks)")))

  #pick a colour palette to apply
  pallette <- pal_age

  # adjust chart y axis according to what is being displayed
  if(measure == "percent_births"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    plot_data <- plot_data  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?

  }

  if(measure == "births"){
    yaxis_plots[["title"]] <- "Number of births"
    plot_data <- plot_data  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?

  }
  # Create tooltip for line chart
  tooltip <- c(paste0("Gestation at delivery: ", plot_data$gest,"<br>",
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
                color = ~gest, colors = pallette,
                text= tooltip, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)}
}

#####################################################################################################################.
## LINECHART SCOTLAND & NHS BOARD: Births by gestation numbers and percentages

plot_gest_linechart_two <- function(measure){

  plot_data <- gest_linechart_filter_two()

  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(gest = factor(gest, levels = c("Under 32 weeks", "32 to 36 weeks", "42 weeks plus")))

  #pick a colour palette to apply
  pallette <- c('#543005', '#8c510a', '#74add1') # specified to ensure grouping of pre-term deliveries using brown lines

  # adjust chart y axis according to what is being displayed
  if(measure == "percent_births"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    plot_data <- plot_data %>%  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
      filter(gest!="18 to 44 weeks")
  }

  if(measure == "births"){
    yaxis_plots[["title"]] <- "Number of births"
    plot_data <- plot_data %>% #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
      filter(gest!="18 to 44 weeks")
  }
  # Create tooltip for line chart
  tooltip <- c(paste0("Gestation at delivery: ", plot_data$gest,"<br>",
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
                color = ~gest, colors = pallette,
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

gest_download_data <- reactive({
  gestation_download
})

output$download_gest_data <- downloadHandler(
  filename ="gestation_at_delivery_extract.csv",
  content = function(file) {
    write_csv(gest_download_data(),
              file) }
)



#END