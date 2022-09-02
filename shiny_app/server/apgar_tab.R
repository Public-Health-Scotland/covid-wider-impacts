# Wider impacts dashboard - Births and babies tab - apgar section
# Server code

###############################################.
## Modals ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$`apgar-source-modal`,
             showModal(modalDialog(
               title = "What is the data source?",
               p("The data used for the Apgar scores page comes from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended home births."),
               p("For the Apgar scores page, SMR02 records for episodes of care involving the delivery of a singleton live birth (i.e. one baby, not twins or more) at 37-42 weeks gestation inclusive have been used. The charts presented show the total number of singleton live births at 37-42 weeks with known Apgar score at 5 minutes following delivery, and the number and percentage of babies that had a score of <7. Data is presented for January 2018 onwards, based on the date the woman was discharged from hospital after delivery."),
               p("Apgar scores range from 0 to 10, with higher scores indicating that the baby is in a healthy condition following delivery."),
               p("Data is presented for individual months at Scotland level, and for sequential quarters (Jan-Mar 2018, Apr-Jun 2018, etc) for individual NHS Boards. Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown. However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
               p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete. Data for the most recent months should be viewed as provisional. Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland. In addition, the recording of specific data items allowing identification of singleton live births at 37-42 weeks gestation, and of babies’ 5 minute Apgar score, is very complete. For the period 1 April 2019 to 31 March 2020, a 5 minute Apgar score was recorded on 99% of SMR02 records relating to singleton live births at 37-42 weeks gestation."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href= "https://publichealthscotland.scot/publications/births-in-scottish-hospitals",
                        "Births in Scottish Hospitals report." , target="_blank")),

               size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_apgar_rules, runchart_modal())
#Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd_apgar, simd_modal("Babies"))

###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("apgar")

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
apgar_filter <- function(){

apgar_filt <- apgar_runchart %>% filter(area_name == input$`apgar-geoname` &
                               area_type == input$`apgar-geotype` &
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

  apgar_linechart %>% filter(area_name == input$`apgar-geoname` &
                                area_type == input$`apgar-geotype` &
                                type %in% c("Scotland","Health board"))
}

###############################################.
## Chart calls to chart function ----
###############################################.

# chart outputs for trend
output$apgar_trend <- renderPlotly({plot_apgar_trend(measure="perc_low_apgar5_37plus",shift = "apgar_shift", trend = "apgar_trend")})

#chart outputs for line charts for NHS board and Scot
output$apgar_linechart_number <- renderPlotly({plot_apgar_linechart(measure="apgar5")})
output$apgar_linechart_percent <- renderPlotly({plot_apgar_linechart(measure="percent_apgar")})

#chart outputs for line charts for Scotland only age and deprivation line charts
output$apgar_linechart_age_n <- renderPlotly({plot_apgar_split(dataset=apgar_linechart_split(split="age"),split="age", measure="low_apgar5_37plus")})
output$apgar_linechart_age_p <- renderPlotly({plot_apgar_split(dataset=apgar_linechart_split(split="age"),split="age", measure="perc_low_apgar5_37plus")})
output$apgar_linechart_dep_n <- renderPlotly({plot_apgar_split(dataset=apgar_linechart_split(split="dep"),split="dep", measure="low_apgar5_37plus")})
output$apgar_linechart_dep_p <- renderPlotly({plot_apgar_split(dataset=apgar_linechart_split(split="dep"),split="dep", measure="perc_low_apgar5_37plus")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$apgar_explorer <- renderUI({

  # text for titles of cut charts
  apgar_data_timeperiod <-  paste0("Figures based on data extracted ",apgar_extract_date)
  apgar_title <- paste0("of singleton live births at 37-42 weeks gestation that have a 5 minute Apgar score of <7: ",input$`apgar-geoname`)

  chart_explanation <-
    tagList(p(run_chart_description("Percentage of births that have a 5 minute
                                    Apgar score of <7",
                                    "the percentage of singleton live births at
                                    37-42 weeks gestation with known 5 minute
                                    Apgar score that had a score of <7, in each
                                    month from January 2018 onwards",
                                    "the average (median) percentage of births
                                    with 5 minute Apgar score of <7 over the
                                    period January 2018 to February 2020
                                    inclusive (the period before the COVID-19
                                    pandemic in Scotland)")))

  chart_explanation_quarter <-
    tagList(
      p(run_chart_description("Percentage of births that have a 5 minute Apgar score of <7",
                              "the percentage of singleton live births at
                              37-42 weeks gestation with known 5 minute
                              Apgar score that had a score of <7, in each
                              quarter from January 2018 onwards",
                              "the average (median) percentage of births
                              with 5 minute Apgar score of <7 over the
                              period January 2018 to February 2020
                              inclusive (the period before the COVID-19
                              pandemic in Scotland)")))

  # Layout depending if Scotland or HB selected
  if (input$`apgar-geotype` == "Health board"){
    tagList(fluidRow(column(12,
                            h4(paste0("Percentage ", apgar_title)),
                            div(actionButton("btn_apgar_rules",
                                             "How do we identify patterns in the data?",
                                             icon = icon('question-circle')),
                                style = "height:40px;"),
                            withSpinner(plotlyOutput("apgar_trend",
                                        height = height_run_chart))),
                     column(12,
                            br(), # spacing
                            p(apgar_data_timeperiod),
                            p(chart_explanation_quarter)),
                     column(12,
                            br(), #spacing
                            h4(paste0("Number of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7: ",input$`apgar-geoname`))),
                     column(12,
                            withSpinner(plotlyOutput("apgar_linechart_number")))))

  } else if (input$`apgar-geotype` == "Scotland"){ #only if scotland selected display age and deprivation breakdowns
                       tagList(fluidRow(column(12,
                                               h4(paste0("Percentage ", apgar_title)),
                                               div(actionButton("btn_apgar_rules",
                                                                "How do we identify patterns in the data?",
                                                                icon = icon('question-circle')),
                                                   style = "height:40px;"),
                                               withSpinner(plotlyOutput("apgar_trend",
                                               height = height_run_chart))),
                                        column(12,
                                               br(), # spacing
                                               p(apgar_data_timeperiod),
                                               p(chart_explanation)),
                                        column(12,
                                               br(), #spacing
                                               h4(paste0("Number of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7: ",input$`apgar-geoname`))),
                                        column(12,
                                               withSpinner(plotlyOutput("apgar_linechart_number")))),
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

    # centrelines
    centreline_name <- paste0(input$`apgar-geoname`," average up to end Feb 2020")
    dottedline_name = "Projected average"
    centreline_data = plot_data$median_apgar5_37plus
    dottedline_data = plot_data$ext_median_apgar5_37plus

    # format y axis
    measure_name <- "Percentage of singleton live births at 37-42 weeks gestation with 5 minute Apgar score of <7 (%)"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10%
    y_label <- "Percentage of births (%)"

    tick_freq <- case_when(input$`apgar-geotype` == "Scotland" ~ 6, T ~ 2)

    #specify tool tip
    tooltip_top <- c(paste0(format(plot_data$date_type),": ",format(plot_data$date_label),"<br>",
                            "Percentage: ",format(plot_data$perc_low_apgar5_37plus, digits = 1,nsmall=1),"%", "<br>"))

    x_dates = "date_label"

    plot_run_chart(plot_data, measure, measure_name, y_label,
                   x_dates, shift, trend, tooltip_top,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_factor_space = tick_freq)

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

    tick_freq <- case_when(input$`apgar-geotype` == "Scotland" ~ 6, T ~ 2)

    xaxis_plots <- c(xaxis_plots,
                     dtick =tick_freq, tickangle = 0,
                     categoryorder = "array", categoryarray = ~date_label)

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
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group: "), split=="dep" ~ paste0("Deprivation group: "))

  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Quarter: ", format(plot_data$quarter_label),"<br>",
                      "Number: ", plot_data$low_apgar5_37plus, "<br>",
                      "Percentage: ", format(plot_data$perc_low_apgar5_37plus,digits=1,nsmall = 1),"%"))

  # adjust chart y axis according to what is being displayed
  if(measure == "perc_low_apgar5_37plus"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    if(split == "age"){
      yaxis_plots[["range"]] <- c(0, 10)}  # forcing range from 0 to 10% for age group
    if(split == "dep"){
      yaxis_plots[["range"]] <- c(0, 10)}  # forcing range from 0 to 10% for dep
  }
  if(measure == "low_apgar5_37plus"){
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
                   dtick = 4, tickangle = 0,
                   categoryorder = "array", categoryarray = ~quarter_label)

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



##END
