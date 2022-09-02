# Wider impacts dashboard - Births and babies tab - perineal tears section
# Server code

###############################################.
## Modals ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$`tears-source-modal`,
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
        tags$a(href= "https://publichealthscotland.scot/publications/births-in-scottish-hospitals",
              "Births in Scottish Hospitals report." , target="_blank")),

      size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_tears_rules, runchart_modal())
#Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd_tears, simd_modal("Women"))

###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("tears") 

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
tears_filter <- function(){

tears_filt <- tears_runchart %>% filter(area_name == input$`tears-geoname` &
                               area_type == input$`tears-geotype` &
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

  tears_linechart %>% filter(area_name == input$`tears-geoname` &
                                area_type == input$`tears-geotype` &
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
                        perineal tear: ",input$`tears-geoname`)

  chart_explanation <-
    tagList(
      p(run_chart_description("Percentage of women who have a third or fourth
                              degree perineal tear",
                              "the percentage of women giving birth vaginally
                              to a singleton live or stillborn baby with a
                              cephalic presentation between 37-42 weeks
                              gestation who have a third or fourth degree
                              perineal tear, in each month from January 2018
                              onwards",
                              "the average (median) percentage of women who have
                              a third or fourth degree perineal tear over the
                              period January 2018 to February 2020 inclusive
                              (the period before the COVID-19 pandemic in
                              Scotland)")))


  chart_explanation_quarter <-
    tagList(
      p(run_chart_description("Percentage of women who have a third or fourth
                              degree perineal tear",
                              "the percentage of women giving birth vaginally
                              to a singleton live or stillborn baby with a
                              cephalic presentation between 37-42 weeks
                              gestation who have a third or fourth degree
                              perineal tear, in each quarter from January 2018
                              onwards",
                              "the average (median) percentage of women who have
                              a third or fourth degree perineal tear over the
                              period January 2018 to February 2020 inclusive
                              (the period before the COVID-19 pandemic in
                              Scotland)")))

   layout_tags =
     tagList(fluidRow(column(12,
                    h4(paste0("Percentage ", tears_title)),
                    div(actionButton("btn_tears_rules",
                                     "How do we identify patterns in the data?",
                                     icon = icon('question-circle')),
                        style = "height:40px;"),
                    withSpinner(plotlyOutput("tears_trend",
                                             height = height_run_chart))),
             column(12,
                    br(), # spacing
                    p(tears_data_timeperiod),
                    p(chart_explanation_quarter)),
             column(12,
                    br(), #spacing
                    h4(paste0("Number of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a third or fourth degree perineal tear: ",input$`tears-geoname`))),
             column(12,
                    withSpinner(plotlyOutput("tears_linechart_number")))))

   # Only include some plots if Scotland selected
    if (input$`tears-geotype` == "Scotland") {
      layout_tags =
        tagList(layout_tags,
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
                                  withSpinner(plotlyOutput("tears_linechart_dep_p")))))
      )#tagList from if statement
    }
   return(layout_tags)
})

#############################################.
## tears chart functions ----
############################################.

## RUNCHART trend chart for monthly inductions percentages: Scotland & NHS Board (except island boards)

plot_tears_trend <- function(measure, shift, trend){
  plot_data <- tears_filter()

  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 65,
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {


    # chart legend labels
    centreline_name <- paste0(input$`tears-geoname`," average up to end Feb 2020")
    dottedline_name <- "Projected average"
    centreline_data = plot_data$median_tears_37plus
    dottedline_data = plot_data$ext_median_tears_37plus

    measure_name <- "Percentage of women who have a third or fourth degree perineal tear (%)"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10%
    y_label <- "Percentage of women (%)"

    tick_freq <- case_when(input$`tears-geotype` == "Scotland" ~ 6, T ~ 2)

    #specify tool tip
    tooltip_top <- c(paste0(format(plot_data$date_type),": ",format(plot_data$date_label),"<br>",
                            "Percentage: ",format(plot_data$perc_3rd4th_tears_37plus, digits = 1,nsmall=1),"%", "<br>"))

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

    tick_freq <- case_when(input$`tears-geotype` == "Scotland" ~ 6, T ~ 2)

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

tears_download_data <- reactive({
  tears_download
})

output$download_tears_data <- downloadHandler(
  filename ="tears_extract.csv",
  content = function(file) {
    write_csv(tears_download_data(),
              file) }
)


##END
