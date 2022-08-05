# Wider impacts dashboard - Antenatal booking tab
# Server code

# Pop-up modal explaining source of data
observeEvent(input$`booking-source-modal`,
             showModal(modalDialog(
               title = "What is the data source?",
               p("The Antenatal Booking Data presented is based on a new data collection established as a rapid response to COVID-19. Data is collected each week, from the clinical information system - BadgerNet Maternity (most NHS boards) or TrakCare Maternity (Lothian) - used by the midwives who ‘book’ the pregnant woman for maternity care."),br(),
               p("Historic data from April 2019 was also collected as a ‘catch-up’ extract in order to identify all women who were currently pregnant during the COVID-19 period. This was either from the same source or - in Ayrshire and Arran, Tayside and Highland - from the systems in use before the introduction of BadgerNet Maternity."),br(),
               p("The charts presented on this page show the number of women booking for antenatal care in each week from the week beginning 1 April 2019 onwards.  Data is shown at all Scotland level and for each mainland NHS Board of residence.  Due to small numbers, weekly data is not shown for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles), however the Island Boards are included in the Scotland total.  In addition to the weekly data, the ‘Download data’ button provides monthly data (based on exact month of booking rather than summation of sequential weeks) for each NHS Board of residence, including the Island Boards."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_booking_rules, runchart_modal())
# Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd_booking, simd_modal("Women"))


###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("booking")

###############################################.
##  Reactive datasets  ----
###############################################.
#Dataset behind trend plot (available at scotland and NHS board level)
ante_booking_filter <- function(){

  booking %>%
    select(-g_u10wks,-g_10to12wks,-g_13pluswks) %>%
    filter(area_name == input$`booking-geoname` &
                       area_type == input$`booking-geotype` &
                       type %in% c("Scotland","Health board"))# %>%
    # temp fix for june 2022 update
    # filter(case_when(area_name == "NHS Greater Glasgow & Clyde" | area_name == "Scotland" ~ week_book_starting < "2022-04-19",
    #   area_name != "NHS Greater Glasgow & Clyde" | area_name != "Scotland" ~ week_book_starting < "2022-05-15"))
    
  }

#Dataset behind deprivation/age plots (only available at scotland level)
ante_booking_filter_split <- function(split){

  booking %>%
    select(-g_u10wks,-g_10to12wks,-g_13pluswks) %>%
    filter(area_name == "Scotland" &
                       area_type == "Scotland" &
                       type==split) %>%
    # filter(week_book_starting < "2022-04-19") %>% #temporary fix for june 2022 update
    droplevels()
}

###############################################.
## Antenatal Booking Charts ----
###############################################.

# chart outputs for Scotland/NHS board trends
output$booking_trend_n <- renderPlotly({
  plot_booking_trend(measure="booked_no", shift = "shift_booked_no", trend = "trend_booked_no")})
output$booking_trend_g <- renderPlotly({
  plot_booking_trend(measure="ave_gest", shift = "shift_booked_gest", trend = "trend_booked_gest")})

# chart outputs for age split numbers and average gestation
output$booking_age_n <- renderPlotly({plot_booking_split(dataset=ante_booking_filter_split("age"), split="age", measure="booking_number")})
output$booking_age_g <- renderPlotly({plot_booking_split(dataset=ante_booking_filter_split("age"), split="age", measure="booking_gestation")})

# chart outputs for age split numbers and average gestation
output$booking_dep_n <- renderPlotly({plot_booking_split(dataset=ante_booking_filter_split("dep"), split="dep", measure="booking_number")})
output$booking_dep_g <- renderPlotly({plot_booking_split(dataset=ante_booking_filter_split("dep"), split="dep", measure="booking_gestation")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$booking_explorer <- renderUI({

  # text for titles of trend charts
  booking_subtitle <-  paste0("Figures based on data extracted ",booking_extract_date)
  booking_trend_title <- paste0("Women booking for antenatal care: ",input$`booking-geoname`)
  booking_title_n <-  paste0("Number of women booking for antenatal care")
  booking_title_g <-  paste0("Average gestation at booking")
  booking_title_g2 <-  paste0("(based on completed weeks of pregnancy)")

  #chart_explanation <- paste0("The black line on the ‘antenatal booking numbers’ charts for Scotland, and each Health Board, shows a weekly time series of data. The solid blue centreline is the average number of bookings over the period 1st April 2019 to 29th February 2020. The dotted line continues that average to allow determination of whether there has been a change.  The ‘average gestation at antenatal booking’ charts follow a similar format.")
  chart_explanation <-
    tagList(p("We have used ",
      tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
             'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
      p(run_chart_description("Number of women booking for antenatal care",
                              "the number of women booking for antenatal care
                              in each week from the week beginning 1 April 2019
                              onwards",
                              "the average (median) number of bookings per week
                              over the period April 2019 to February 2020
                              inclusive (the period before the COVID-19 pandemic
                              in Scotland)")),
      p(run_chart_description("Average gestation at booking",
                              "the average (mean) gestation at which women
                              booked for their antenatal care (based on
                              gestation at booking measured in completed weeks
                              of pregnancy)",
                              text_mode = "additional")))

  # Function to create common layout to all immunisation charts
  booking_layout <- function(plot_trend_n,plot_trend_g, plot_age_n, plot_age_g, plot_dep_n, plot_dep_g){
    tagList(if (input$`booking-geoname` == "NHS Tayside"){
            fluidRow(column(12,
                            h4(booking_trend_title),
                            actionButton("btn_booking_rules", "How do we identify patterns in the data?")),
                     column(6,
                            h4(paste0(booking_title_n)), br(), p(" "),
                            #actionButton("btn_booking_rules", "How do we identify patterns in the data?"),
                            withSpinner(plotlyOutput("booking_trend_n",
                                                     height = height_run_chart))),
                     column(6,
                            h4(paste0(booking_title_g)),
                            p(paste0(booking_title_g2)),
                            withSpinner(plotlyOutput("booking_trend_g",
                                                     height = height_run_chart))),
                     column(12,
                            br(), # spacing
                            p(booking_subtitle),
                            p(chart_explanation)))
    } else if (input$`booking-geoname` == "NHS Forth Valley"){
      fluidRow(column(12,
                      h4(booking_trend_title),
                      actionButton("btn_booking_rules", "How do we identify patterns in the data?")),
               column(6,
                      h4(paste0(booking_title_n)), br(), p(" "),
                      #actionButton("btn_booking_rules", "How do we identify patterns in the data?"),
                      withSpinner(plotlyOutput("booking_trend_n",
                                               height = height_run_chart))),
               column(6,
                      h4(paste0(booking_title_g)),
                      p(paste0(booking_title_g2)),
                      withSpinner(plotlyOutput("booking_trend_g",
                                               height = height_run_chart))),
               column(12,
                      br(), # spacing
                      p(booking_subtitle),
                      p(chart_explanation)))

      } else {
      fluidRow(column(12,
                      h4(booking_trend_title),
                      actionButton("btn_booking_rules", "How do we identify patterns in the data?")),
               column(6,
                      h4(paste0(booking_title_n)), br(), p(" "),
                      #actionButton("btn_booking_rules", "How do we identify patterns in the data?"),
                      withSpinner(plotlyOutput("booking_trend_n",
                                               height = height_run_chart))),
               column(6,
                      h4(paste0(booking_title_g)),
                      p(paste0(booking_title_g2)),
                      withSpinner(plotlyOutput("booking_trend_g",
                                               height = height_run_chart))),
               column(12,
                      br(), # spacing
                      p(booking_subtitle),
                      p(chart_explanation)))
      },
            # only if scotland selected display age and deprivation breakdowns
            if (input$`booking-geotype` == "Scotland"){
              tagList(
                fluidRow(column(12,h4("Women booking for antenatal care, by age group: Scotland"))),
                fluidRow(column(6,
                                h4("Number of women booking for antenatal care"),
                                br(),p(" "), #required to balance out alignment of charts in columns
                                withSpinner(plotlyOutput("booking_age_n"))),
                         column(6,
                                h4("Average gestation at booking"),
                                p("(based on completed weeks of pregnancy)"),
                                withSpinner(plotlyOutput("booking_age_g"))),
                         fluidRow(column(12,h4("Women booking for antenatal care, by deprivation: Scotland"),
                                         actionButton("btn_modal_simd_booking", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         column(6,
                                h4("Number of women booking for antenatal care"),
                                br(),p(" "), #required to balance out alignment of charts in columns
                                withSpinner(plotlyOutput("booking_dep_n"))),
                         column(6,
                                h4("Average gestation at booking"),
                                p("(based on completed weeks of pregnancy)"),
                                withSpinner(plotlyOutput("booking_dep_g"))))
              )#tagList from if statement
            })#close top taglist
  } #close layout function

  #link plot functions to layouts
  booking_layout(plot_trend_n="booking_trend_n", plot_trend_g="booking_trend_g",
                 plot_age_n="booking_age_n", plot_age_g="booking_age_g",
                 plot_dep_n="booking_dep_n", plot_dep_g="booking_dep_g")
}) #close booking explorer


#############################################.
## Antenatal booking chart functions ----
############################################.

## Trend plot for weekly bookings numbers and average gestation at booking
plot_booking_trend <- function(measure, shift, trend){

  plot_data <- ante_booking_filter()

  # Display message if island/small board is supplied and no chart available
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50,
                text_nodata = "Weekly data not shown due to small numbers. Monthly data is available through the Download data button above")

  } else {
    # chart legend labels
    centreline_name <- paste0(input$`booking-geoname`," average up to end Feb 2020")
    dottedline_name <- paste0(input$`booking-geoname`," projected average from Mar 2020")

    #switch y-axis according to which measure is selected
    if(measure == "booked_no"){
      tooltip_booking <- c(paste0("Week commencing: ",format(plot_data$week_book_starting,"%d %b %y"),"<br>",
                                  "Number of women booking: ",plot_data$booked_no))

      dottedline_data <-  plot_data$dottedline_no
      centreline_data <-  plot_data$centreline_no

      y_label <- "Number of women booking"
      measure_name <- "Number of women booking"

    } else if (measure  == "ave_gest") {
      # chart legend labels, for most boards and the different centrelines of Tayside and FV
      centreline_name <- paste0(input$`booking-geoname`," average up to end Feb 2020")
      centreline_name_t <- paste0(input$`booking-geoname`, " average from Aug 2020 to end Dec 2020")
      centreline_name_v <- paste0(input$`booking-geoname`, " average from Mar 2021 to Jun 2021")
      dottedline_name <- paste0(input$`booking-geoname`," projected average from Mar 2020")
      dottedline_name_t <- paste0(input$`booking-geoname`," projected average from Jan 2021")
      dottedline_name_v <- paste0(input$`booking-geoname`," projected average from Jul 2021")
      dottedline_name_ts <- paste0(input$`booking-geoname`," projected average from Mar 2020 to end Jul 2020")
      dottedline_name_fv <- paste0(input$`booking-geoname`," projected average from Mar 2020 to end Feb 2021")

      yaxis_plots[["range"]] <- c(0, 16)  # forcing range from 0 to 16 weeks to ensure doesn't change when NHS board selected
      tooltip_booking <- c(paste0("Week commencing: ",format(plot_data$week_book_starting,"%d %b %y"),"<br>",
                                  "Average gestation: ",format(plot_data$ave_gest,digits = 1,nsmall=1)," weeks"))

      dottedline_data <-  plot_data$dottedline_g
      centreline_data <-  plot_data$centreline_g
      dotted_line_t <- plot_data$dottedline_g_t
      centre_line_t <- plot_data$centreline_g_t
      centre_line_v <- plot_data$centreline_g_v
      dotted_line_v <- plot_data$dottedline_g_v

      y_label <- "Average gestation at booking"
      measure_name <- "Average gestation"
    }


    # Adding a couple of different centrelines for average gestation in FV and Tayside
    if (measure == "ave_gest" & input$`booking-geoname` == "NHS Tayside") {

      main_dottedline_name = dottedline_name_ts

      centreline_2_data = centre_line_t
      centreline_2_name = centreline_name_t
      dottedline_2_data = dotted_line_t
      dottedline_2_name = dottedline_name_t

    } else if (measure == "ave_gest" & input$`booking-geoname` == "NHS Forth Valley") {

      main_dottedline_name = dottedline_name_fv

      centreline_2_data = centre_line_v
      centreline_2_name = centreline_name_v
      dottedline_2_data = dotted_line_v
      dottedline_2_name = dottedline_name_v

    } else {

      main_dottedline_name = dottedline_name

      centreline_2_data = NULL
      centreline_2_name = NULL
      dottedline_2_data = NULL
      dottedline_2_name = NULL

    }

    x_dates = "week_book_starting"

    ante_plot = plot_run_chart(plot_data, measure, measure_name, y_label,
                               x_dates, shift, trend, tooltip_booking,
                               xaxis_plots, yaxis_plots, bttn_remove,
                               centreline_data, centreline_name,
                               dottedline_data, main_dottedline_name,
                               centreline_2_data, centreline_2_name,
                               dottedline_2_data, dottedline_2_name)
  }
}


## Trend plot for weekly bookings numbers and average gestation at booking split by age group and simd quintile
plot_booking_split <- function(dataset, split, measure){

  plot_data <- dataset

  #label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"),
                              split=="dep" ~ paste0("Deprivation group:"))

  #switch y-axis according to which measure is selected
  if(measure == "booking_number"){
    yaxis_measure <- dataset$booked_no
    yaxis_plots[["title"]] <- "Number of women booking"
    tooltip_booking <- c(paste0(tool_tip_split,dataset$category,"<br>",
                                "Week commencing: ",format(plot_data$week_book_starting,"%d %b %y"),"<br>",
                                "Number of women booking: ",plot_data$booked_no))
    xaxis_plots[["range"]] <- c(min(plot_data$week_book_starting), max(plot_data$week_book_starting))

  } else if (measure  == "booking_gestation") {
    yaxis_measure <- dataset$ave_gest
    yaxis_plots[["title"]] <- "Average gestation at booking"
    yaxis_plots[["range"]] <- c(0, 16)  # forcing range from 0 to 16 weeks
    tooltip_booking <- c(paste0(tool_tip_split,dataset$category,"<br>",
                                "Week commencing: ",format(dataset$week_book_starting,"%d %b %y"),"<br>",
                                "Average gestation: ",format(dataset$ave_gest,digits = 1,nsmall=1)," weeks"))
    xaxis_plots[["range"]] <- c(min(plot_data$week_book_starting), max(plot_data$week_book_starting))
    }

  #adjust datasets accordig to which data split to be displayed
  if(split == "age"){
    plot_data <- plot_data %>%
      droplevels() %>%
      mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29","30-34", "35-39","40 and over")))
    pallette <- pal_age}

  if(split == "dep"){
    plot_data <- plot_data %>%
      mutate(category = factor(category, levels = c("1 - most deprived", "2", "3","4", "5 - least deprived")))
    pallette <- pal_depr}

  #Creating time trend plot
  plot_ly(data=plot_data, x=~week_book_starting, y = ~yaxis_measure) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category,
              colors = pallette,
              text= tooltip_booking,
              hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,xaxis = xaxis_plots,
           legend = list(orientation = "h",   # show entries horizontally
                                xanchor = "center",  # use center of legend as anchor
                                x = 0.5)) %>%             # put legend in center of x-axis
           #legend = list(x = 100, y = 0.5),
           #legend = list(orientation = 'h')) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
}

###############################################.
## Data downloads ----
###############################################.

antebooking_down_data <- reactive({
  booking_download
})

output$download_ante_booking_data <- downloadHandler(
  filename ="antenatal_booking_extract.csv",
  content = function(file) {
    write_csv(antebooking_down_data(),
              file) }
)


#END