# Wider impacts dashboard - Induction of labour tab
# Server code

# Pop-up modal explaining source of data
observeEvent(input$`induct-source-modal`,
             showModal(modalDialog(
               title = "What is the data source?",
               p("The data used for the induction of labour page comes from the Scottish Morbidity Record 02 (SMR02) database.  An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care.  From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."),
               p("For the induction of labour page, SMR02 records for episodes of care involving the delivery of a singleton live birth (i.e. one baby, not twins or more) at 37 to 42 weeks gestation inclusive have been used.  The charts presented show the total number of singleton live births at 37-42 weeks gestation, and the number and percentage that followed induction of labour, in each month from January 2018 onwards.  The month is based on the date the woman was discharged from hospital after delivery.  Data is shown at all Scotland level, and for women living in each mainland NHS Board area.  Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown.  However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
               p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete.  Data for the most recent months should be viewed as provisional.  Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high.  For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland.  In addition, the recording of gestation at delivery, and of whether the delivery followed induction of labour, is very complete.  For the period 1 April 2019 to 31 March 2020, gestation was recorded on >99.9% of SMR02 records relating to singleton live births, and whether the delivery followed induction of labour was recorded on 99.4% of records relating to singleton live births at 37-42 weeks gestation."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href="https://publichealthscotland.scot/publications/births-in-scottish-hospitals", "Births in Scottish Hospitals report",class="externallink",target="_blank"),"."),
               size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_induct_rules, runchart_modal())
#Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd_induct, simd_modal("Women"))

###############################################.
## Induction Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("induct")

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
induct_filter <- function(){

  induct_runchart %>% filter(area_name == input$`induct-geoname` &
                            area_type == input$`induct-geotype` &
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

  induct_linechart %>% filter(area_name == input$`induct-geoname` &
                                area_type == input$`induct-geotype` &
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
  induct_title <- paste0("of singleton live births at 37-42 weeks gestation that followed induction of labour: ",input$`induct-geoname`)

  chart_explanation <-
    tagList(p("We have used ",
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p(run_chart_description("Percentage of births that followed
                                    induction of labour",
                                    "the percentage of singleton live births at
                                    37-42 weeks gestation that followed
                                    induction of labour in each month from
                                    January 2018 onwards",
                                    "the average (median) percentage of births
                                    that followed induction of labour over the
                                    period January 2018 to February 2020
                                    inclusive (the period before the COVID-19
                                    pandemic in Scotland)")))

  # Function to create common layout to all immunisation charts
  induct_layout <- function(induct_trend,induct_linechart_number,induct_linechart_age_n,induct_linechart_age_p,induct_linechart_dep_n,induct_linechart_dep_p){
    tagList(fluidRow(column(12,
                            h4(paste0("Percentage ", induct_title)),
                            div(actionButton("btn_induct_rules",
                                             "How do we identify patterns in the data?"),
                                style = "height:40px;"),
                            withSpinner(plotlyOutput("induct_trend",
                                                     height = height_run_chart))),
                     column(12,
                            br(), # spacing
                            p(induct_data_timeperiod),
                            p(chart_explanation)),
                     column(12,
                            br(), #spacing
                            h4(paste0("Number of singleton live births at 37-42 weeks gestation that followed induction of labour: ",input$`induct-geoname`))),
                     column(12,
                            withSpinner(plotlyOutput("induct_linechart_number"))),
                     #only if scotland selected display age and deprivation breakdowns
                     if (input$`induct-geotype` == "Scotland"){
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

    # centrelines
    centreline_name <- paste0(input$`induct-geoname`," average up to end Feb 2020")
    dottedline_name <- "Projected average"
    centreline_data = plot_data$median_ind_37_42
    dottedline_data = plot_data$ext_ind_37_42

    # format y axis
    measure_name <- "Percentage of births that followed induction (%)"
    yaxis_plots[["range"]] <- c(0, 60)  # forcing range from 0 to 60%
    y_label <- "Percentage of births (%)"

    #specify tool tip
    tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                            "Percentage: ",format(plot_data$perc_ind_37_42,digits = 1,nsmall=1),"%", "<br>"))

    x_dates = "month"

    plot_run_chart(plot_data, measure, measure_name, y_label,
                   x_dates, shift, trend, tooltip_top,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_buffer = 20)

  }}

#####################################################################################################################.
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


#####################################################################################################################.
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


##END
