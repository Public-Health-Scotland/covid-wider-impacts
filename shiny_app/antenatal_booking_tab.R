##Server script for antenatal booking tab.


# Pop-up modal explaining source of data
observeEvent(input$btn_booking_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The Antenatal Booking Data presented is based on a new data collection established as a rapid response to COVID-19. Data is collected each week, from the clinical information system (BadgerNet Maternity (most NHS boards), TrakCare Maternity (Lothian) or Eclipse (A&A) used by the midwives who ‘book’ the pregnant woman for maternity care."),br(),
               p("Historic data from April 2019 was also collected as a ‘catch-up’ extract in order to identify all women who were currently pregnant during the COVID-19 period. This was either from the same source or - in Tayside and Highland - from the systems in use before the introduction of BadgerNet Maternity."),br(),
               p("The charts presented on this page show the number of women booking for antenatal care in each week from the week beginning 1 April 2019 onwards.  Data is shown at all Scotland level and for each mainland NHS Board of residence.  Due to small numbers, weekly data is not shown for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles), however the Island Boards are included in the Scotland total.  In addition to the weekly data, the ‘Download data’ button provides monthly data (based on exact month of booking rather than summation of sequential weeks) for each NHS Board of residence, including the Island Boards."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_booking_rules,
             showModal(modalDialog(
               title = "How do we identify patterns in the data?",
               p("Run charts use a series of rules to help identify important changes in the data. These are the ones we used for these charts:"),
               tags$ul(tags$li("Shifts: Six or more consecutive data points above or below the centreline. Points on the centreline neither break nor contribute to a shift (marked on chart)."),
                       tags$li("Trends: Five or more consecutive data points which are increasing or decreasing. An observation that is the same as the preceding value does not count towards a trend (marked on chart)."),
                       tags$li("Too many or too few runs: A run is a sequence of one or more consecutive observations on the same side of the centreline. Any observations falling directly on the centreline can be ignored. If there are too many or too few runs (i.e. the median is crossed too many or too few times) that’s a sign of something more than random chance.")),
               p("Further information on these methods of presenting data can be found in the ",                      
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts', target="_blank"),"."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SIMD and deprivation
# Link action button click to modal launch 
observeEvent(input$btn_modal_simd_booking, { showModal(
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

# Pop-up modal explaining source of data
observeEvent(input$btn_tayside_modal, 
             showModal(modalDialog(
               title = "Why is there a second centreline?",
               p("Insert text explaining second centre line here."),br(),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Pregnancy Reactive controls  ----
###############################################.

# Pregnancy reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_booking <- renderUI({
  #Lists areas available in   
  areas_summary_booking <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_booking])
  selectizeInput("geoname_booking", label = NULL, choices = areas_summary_booking, selected = "")
})

###############################################.
##  Reactive datasets  ----
###############################################.
#Dataset behind trend plot (available at scotland and NHS board level)
ante_booking_filter <- function(){
  
  booking %>% 
    select(-g_u10wks,-g_10to12wks,-g_13pluswks) %>%
    filter(area_name == input$geoname_booking &
                       area_type == input$geotype_booking &
                       type %in% c("Scotland","Health board"))
}

#Dataset behind deprivation/age plots (only available at scotland level)
ante_booking_filter_split <- function(split){
  
  booking %>% 
    select(-g_u10wks,-g_10to12wks,-g_13pluswks) %>%
    filter(area_name == "Scotland" &
                       area_type == "Scotland" &
                       type==split) %>%
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
  booking_trend_title <- paste0("Women booking for antenatal care: ",input$geoname_booking)
  booking_title_n <-  paste0("Number of women booking for antenatal care")
  booking_title_g <-  paste0("Average gestation at booking")
  booking_title_g2 <-  paste0("(based on completed weeks of pregnancy)")
  
  #chart_explanation <- paste0("The black line on the ‘antenatal booking numbers’ charts for Scotland, and each Health Board, shows a weekly time series of data. The solid blue centreline is the average number of bookings over the period 1st April 2019 to 29th February 2020. The dotted line continues that average to allow determination of whether there has been a change.  The ‘average gestation at antenatal booking’ charts follow a similar format.")
  chart_explanation <- 
    tagList(p("We have used ",                      
      tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
             'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
      p("On the ‘Number of women booking for antenatal care’ chart above, the dots joined by a solid black line show the number of women booking for antenatal care in each week from the week beginning 1 April 2019 onwards. The solid blue centreline on the chart shows the average (median) number of bookings per week over the period April 2019 to February 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The dotted blue centreline continues that average to allow determination of whether there has subsequently been a change in the number of women booking."),
      p("The ‘Average gestation at booking’ chart follows a similar format. In this chart, the dots joined by a solid black line show the average (mean) gestation at which women booked for their antenatal care (based on gestation at booking measured in completed weeks of pregnancy)."))
  
  # Function to create common layout to all immunisation charts
  booking_layout <- function(plot_trend_n,plot_trend_g, plot_age_n, plot_age_g, plot_dep_n, plot_dep_g){
    tagList(if (input$geoname_booking == "NHS Tayside"){
            fluidRow(column(12,
                            h4(booking_trend_title),
                            actionButton("btn_booking_rules", "How do we identify patterns in the data?")),
                     column(6,
                            h4(paste0(booking_title_n)), br(), p(" "),
                            #actionButton("btn_booking_rules", "How do we identify patterns in the data?"),
                            withSpinner(plotlyOutput("booking_trend_n"))),
                     column(6,
                            h4(paste0(booking_title_g)),
                            p(paste0(booking_title_g2)),
                            withSpinner(plotlyOutput("booking_trend_g"))),
                     column(12,
                            p(booking_subtitle),
                            p(chart_explanation)))
    } else if (input$geoname_booking == "NHS Forth Valley"){
      fluidRow(column(12,
                      h4(booking_trend_title),
                      actionButton("btn_booking_rules", "How do we identify patterns in the data?")),
               column(6,
                      h4(paste0(booking_title_n)), br(), p(" "),
                      #actionButton("btn_booking_rules", "How do we identify patterns in the data?"),
                      withSpinner(plotlyOutput("booking_trend_n"))),
               column(6,
                      h4(paste0(booking_title_g)),
                      p(paste0(booking_title_g2)),
                      withSpinner(plotlyOutput("booking_trend_g"))),
               column(12,
                      p(booking_subtitle),
                      p(chart_explanation)))
    
      } else {
      fluidRow(column(12,
                      h4(booking_trend_title),
                      actionButton("btn_booking_rules", "How do we identify patterns in the data?")),
               column(6,
                      h4(paste0(booking_title_n)), br(), p(" "),
                      #actionButton("btn_booking_rules", "How do we identify patterns in the data?"),
                      withSpinner(plotlyOutput("booking_trend_n"))),
               column(6,
                      h4(paste0(booking_title_g)),
                      p(paste0(booking_title_g2)),
                      withSpinner(plotlyOutput("booking_trend_g"))),
               column(12,
                      p(booking_subtitle),
                      p(chart_explanation)))
      },
            #only if scotland selected display age and deprivation breakdowns
            if (input$geotype_booking == "Scotland"){
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
    centreline_name <- paste0(input$geoname_booking," average up to end Feb 2020")    
    dottedline_name <- paste0(input$geoname_booking," projected average from Mar 2020")
    # format max and min x-axis to show initial time period and to add padding so markers aren't cut in half at start and end of chart
    xaxis_plots[["range"]] <- c(min(plot_data$week_book_starting)-7, max(plot_data$week_book_starting)+7) #force x-axis to display first week of data
    
    #switch y-axis according to which measure is selected
    if(measure == "booked_no"){
      yaxis_plots[["title"]] <- "Number of women booking"
      tooltip_booking <- c(paste0("Week commencing: ",format(plot_data$week_book_starting,"%d %b %y"),"<br>",
                                  "Number of women booking: ",plot_data$booked_no))
      dotted_line <-  plot_data$dottedline_no
      centre_line <-  plot_data$centreline_no
      yname <- "Number of women booking"
      
    } else if (measure  == "ave_gest") {
      # chart legend labels, for most boards and the different centrelines of Tayside and FV  
      centreline_name <- paste0(input$geoname_booking," average up to end Feb 2020")   
      centreline_name_t <- paste0(input$geoname_booking, " average from Aug 2020 to end Dec 2020")
      centreline_name_v <- paste0(input$geoname_booking, " average from Mar 2021 to Jun 2021")
      dottedline_name <- paste0(input$geoname_booking," projected average from Mar 2020") 
      dottedline_name_t <- paste0(input$geoname_booking," projected average from Jan 2021") 
      dottedline_name_v <- paste0(input$geoname_booking," projected average from Jul 2021")
      dottedline_name_ts <- paste0(input$geoname_booking," projected average from Mar 2020 to end Jul 2020")
      dottedline_name_fv <- paste0(input$geoname_booking," projected average from Mar 2020 to end Feb 2021")
      # format max and min x-axis to show initial time period and to add padding so markers aren't cut in half at start and end of chart
      xaxis_plots[["range"]] <- c(min(plot_data$week_book_starting)-7, max(plot_data$week_book_starting)+7) #force x-axis to display first week of data
      
      yaxis_plots[["title"]] <- "Average gestation at booking"
      yaxis_plots[["range"]] <- c(0, 16)  # forcing range from 0 to 16 weeks to ensure doesn't change when NHS board selected
      tooltip_booking <- c(paste0("Week commencing: ",format(plot_data$week_book_starting,"%d %b %y"),"<br>",
                                  "Average gestation: ",format(plot_data$ave_gest,digits = 1,nsmall=1)," weeks"))
      
      dotted_line <-  plot_data$dottedline_g
      centre_line <-  plot_data$centreline_g
      dotted_line_t <- plot_data$dottedline_g_t
      centre_line_t <- plot_data$centreline_g_t
      centre_line_v <- plot_data$centreline_g_v
      dotted_line_v <- plot_data$dottedline_g_v
      
      yname <- "Average gestation"
    } 
    
    
    #Creating time trend plot
    ante_plot <- plot_ly(data=plot_data, x=~week_book_starting)
    
    # Adding a couple of different centrelines for average gestation in FV and Tayside
    if(measure == "ave_gest" & input$geoname_booking == "NHS Tayside"){
      ante_plot <- ante_plot %>% 
        # For Tayside
        add_lines(y = ~get(measure),  
                  line = list(color = "black"), text=tooltip_booking, hoverinfo="text",
                  marker = list(color = "black"), name = yname) %>% 
        add_lines(y = ~centre_line, name = centreline_name,
                  line = list(color = "blue"), hoverinfo="none") %>%        
        add_lines(y = ~dotted_line,
                  line = list(color = "blue", dash = "dash"), hoverinfo="none",
                  name = dottedline_name_ts) %>%
        add_lines(y = ~centre_line_t, name = centreline_name_t,
                line = list(color = "limegreen"), hoverinfo="none") %>%
        add_lines(y = ~dotted_line_t,
                  line = list(color = "limegreen", dash = "dash"), hoverinfo="none",
                  name = dottedline_name_t) 
    
    } else if(measure == "ave_gest" & input$geoname_booking == "NHS Forth Valley") {
      ante_plot <- ante_plot %>% 
        # For Tayside
        add_lines(y = ~get(measure),  
                  line = list(color = "black"), text=tooltip_booking, hoverinfo="text",
                  marker = list(color = "black"), name = yname) %>% 
        add_lines(y = ~centre_line, name = centreline_name,
                  line = list(color = "blue"), hoverinfo="none") %>%        
        add_lines(y = ~dotted_line,
                  line = list(color = "blue", dash = "dash"), hoverinfo="none",
                  name = dottedline_name_fv) %>%
      add_lines(y = ~centre_line_v, name = centreline_name_v,
                line = list(color = "limegreen"), hoverinfo="none") %>%
        add_lines(y = ~dotted_line_v,
                  line = list(color = "limegreen", dash = "dash"), hoverinfo="none",
                  name = dottedline_name_v) 
      
    } else {
      ante_plot <- ante_plot %>%
        add_lines(y = ~get(measure),  
                  line = list(color = "black"), text=tooltip_booking, hoverinfo="text",
                  marker = list(color = "black"), name = yname) %>% 
        add_lines(y = ~centre_line, name = centreline_name,
                  line = list(color = "blue"), hoverinfo="none") %>%        
        add_lines(y = ~dotted_line,
                  line = list(color = "blue", dash = "dash"), hoverinfo="none",
                  name = dottedline_name) }
      
    ante_plot %>%
      # adding trends
      add_markers(data = plot_data %>% filter_at(trend, all_vars(. == T)), y = ~get(measure),
                  marker = list(color = "green", size = 10, symbol = "square"), name = "Trends", hoverinfo="none") %>%
      # adding shifts - add these last so that shifts are always visible on top of trends
      add_markers(data = plot_data %>% filter_at(shift, all_vars(. == T)), y = ~get(measure),
                  marker = list(color = "orange", size = 10, symbol = "circle"), name = "Shifts", hoverinfo="none") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             #legend = list(x = 0.1, y = 0.1)) %>% #position of legend inside plot
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
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

###############################################.
## Commentary tab content  ----
###############################################.

observeEvent(input$switch_to_top,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Termination of pregnancy")
})

output$booking_commentary <- renderUI({
  tagList(
    bsButton("jump_to_booking",label = "Go to data"), #this button can only be used once
    h2("Antenatal bookings - 7th July 2021"),
    p("In this release of information on antenatal booking data (7th July 2021) data have been updated to include women booking for antenatal care up to the week beginning 7th June 2021. 
       A new centreline line for average gestation has been included for NHS Forth Valley because a technical change to the way their data are recorded is thought to have resulted in data which more accurately represent the timing of when women book for antenatal care in NHS Forth Valley. The new centreline starts from the week beginning 1st March 2021 and will be calculated over the period 1st March - 12th July 2021 after which a projected centreline will be presented on the average gestation chart for NHS Forth Valley."),
    h2("Antenatal bookings - 2nd June 2021"),
    p("In this release of information on antenatal booking data (2nd June 2021) data have been updated to include women booking for antenatal care up to the week beginning 3rd May 2021. Since the previous release, which showed data up until the week beginning 5th April 2021, numbers of women booking for antenatal care in Scotland have reduced (to 869 in week of 3rd May). This is likely to be as a result of fewer women booking over the May public holiday. This reduction is also reflected in the numbers of bookings by NHS Board with NHS Borders, NHS Lothian and NHS Greater Glasgow & Clyde showing notable decreases for the week beginning 3rd May 2021. NHS Forth Valley have recorded six consecutive data points below their average number of bookings."),
    p("The updated (all-Scotland) data in this release show that the average gestation at which women booked for antenatal care in recent weeks is around the average based on the pre-pandemic period. Recent data on average gestation by NHS Board are more varied. Lower than average gestation at booking has been observed over at least six consecutive data points in the most recent NHS Ayrshire & Arran, NHS Dumfries & Galloway, NHS Highland, NHS Lanarkshire and NHS Lothian data. NHS Forth Valley continues to show an increased average gestation at booking in recent weeks compared to their pre-pandemic average. This is believed to be as a result of a technical change in data recording and we are continuing to work with the Health Board to clarify this."),
    h2("Antenatal bookings - 5th May 2021"),
    p("In this release of information on antenatal booking data (5th May 2021) data have been updated to include women booking for antenatal care up to the week beginning 5th April 2021. Since the previous release, which showed data up until the week beginning 8th March 2021, numbers of women booking for antenatal care in Scotland have reduced slightly (to 971 in week of 5 April) but are still at a level which is very similar to the average numbers seen pre-pandemic. Numbers of bookings in different NHS Boards vary. NHS Borders, NHS Grampian and NHS Highland are all showing runs of at least six consecutive data points above their average number of bookings per week, that continue into April. NHS Ayrshire & Arran have recorded six consecutive data points below their average number of bookings."),
    p("The updated (all-Scotland) data in this release show that the average gestation at which women booked for antenatal care is at a very similar level to the average based on the pre-pandemic period: at 9.3 weeks. A higher than average gestation at booking for women aged under 20 is evident in six out of the last seven time points (average gestation of 11.2 weeks, in week of 5 April). Recent data on average gestation by NHS Board are more varied. Lower than average gestation at booking has been observed over at least six consecutive data points in the most recent NHS Ayrshire & Arran, NHS Lanarkshire and NHS Lothian data. NHS Forth Valley has shown a sharp rise in average gestation at booking in recent weeks. This is believed to be as a result of a technical change in data recording and we are continuing to work with the Health Board to clarify this."),
    h2("Antenatal bookings - 7th April 2021"),
    p("In this release of information on antenatal booking data (7th April 2021) data have been updated to include women booking for antenatal care up to the week beginning 8th March 2021. Since the previous release, which showed data up until the week beginning 1st February 2021, numbers of women booking for antenatal care in Scotland 
      have returned to a level which is very similar to the average numbers seen pre-pandemic at just over 1,000 women per week. Numbers of bookings in different NHS Boards vary. NHS Fife, NHS Greater Glasgow & Clyde, NHS Highland and NHS Lanarkshire are all showing runs of at least six consecutive data points above their average number 
      of bookings per week, that continue into March. NHS Forth Valley have shown a marked decrease over the last five weeks in their recorded data for the number of women booking for antenatal care. This is thought to be as a result of a data recording issue and does not represent the true number of women booking in NHS Forth Valley.  
      We are working with NHS Forth Valley to rectify this."),
    p("The updated (all-Scotland) data in this release show that the average gestation at which women booked for antenatal care is at a very similar level to the average based on the pre-pandemic period: at 9.3 weeks. Recent data on average gestation by NHS Board are more varied. Lower than average gestation at booking has been observed 
      over at least six consecutive data points in the most recent NHS Ayrshire & Arran, NHS Dumfries & Galloway, NHS Highland, NHS Lanarkshire and NHS Lothian data.  NHS Forth Valley has shown a sharp rise in average gestation at booking in recent weeks, but as noted above, this is thought to be as a result of a data recording issue 
      which we are working to rectify with the Health Board."),
    h2("Antenatal bookings - 3rd March 2021"),
    p("In this release of information on antenatal booking data (3rd March 2021) data have been updated to include women booking for antenatal care up to the week beginning 1st February 2021. Since the previous release, which showed data up until the week beginning 4th January 2021, numbers of women booking for antenatal care in 
      Scotland reached a peak during the week beginning 11th January 2021 and have since decreased but still remained high, well above the average numbers seen pre-pandemic.  Much of this increase in numbers is likely to be due to women delaying booking until after the Christmas and new year holidays. A similar increase can be seen 
      over the same period last year.  The extent of the Christmas and new year reduction on numbers and subsequent increase in January is more prominent in the larger NHS Boards such as NHS Greater Glasgow & Clyde, NHS Lothian and NHS Lanarkshire."),
    p("The updated (all-Scotland) data in this release show that the average gestation at which women booked for antenatal care remains just below the average based on the pre-pandemic period. The recent data on average gestation by NHS Board are more varied.  Lower than average gestation at booking is observed in recent weeks in NHS
      Ayrshire & Arran, NHS Dumfries and Galloway, NHS Forth Valley and NHS Lanarkshire."),
    p("A new average line has been included for NHS Tayside because the data sourced from their Badgernet Maternity information system (introduced in August 2020) are thought to more accurately represent the timing of when women book for antenatal care in NHS Tayside than the earlier (pre-August 2020) data sourced from their Protos 
      information system. Further detail on this is included in the commentary dated 3rd February 2021."),
    h2("Antenatal bookings - 3rd February 2021"),
    p("In this third release of information on antenatal booking data (3rd February 2021) data have been updated to include women booking for antenatal care up to the week beginning 4th January 2021. Previous releases of data have shown that from mid-May to end September the number of women booking for antenatal care had been 
      consistently lower than expected based on pre-pandemic average levels. At the end of September numbers started to rise and have been increasing throughout October, November and most of December.  Although this increase may be partly explained by some women planning their pregnancies during these months, having previously 
      delayed their pregnancy during the first Coronavirus lockdown, the increase is also consistent with a seasonal pattern of increasing numbers of bookings that we usually see each year during the Autumn months."),
    p("The sudden drop in numbers of women booking for antenatal care during the weeks starting 21st December and 28th December 2020 is thought to be as a result of the Christmas and new year public holidays. A similar decrease can be seen during the previous year’s Christmas and new year period and the extent of the decrease 
      is likely to depend on whether the four public holidays fall across a two or three week period. All NHS Boards showed some level of reduction in their numbers of women booked over this period. "),
    p("The updated (all-Scotland) data in this release (for November and December 2020) show that the average gestation at which women booked for antenatal care continues to be just below the average based on the pre-pandemic period. The recent data on average gestation by NHS Board are more varied, most notably NHS Tayside have 
      shown higher average gestations of women booking compared to their pre-pandemic average since August 2020. This change reflects a number of factors: transition of local care pathways to accommodate changes resulting from the impact of the Covid-19 pandemic; contemporaneous local transition to the Badgernet Maternity information 
      system, and reinforced compliance with local care pathways to ensure booking of women between 8 to 10 weeks gestation. Data for more recent months for NHS Tayside, which show average gestations of between 8 to 10 weeks, are thought to more accurately represent the timing of when women book for antenatal care in NHS Tayside 
      than the earlier (pre-August 2020) data sourced from the Protos information system."),
    h2("Antenatal bookings - 2nd December 2020"),
    p("In this second release of information on antenatal booking data (2 Dec 2020) data have been updated to include women booking for antenatal care up to the week beginning 26th October 2020.  The initial release of data on 28th October 2020 showed that from mid-May to end September the number of women booking for antenatal 
      care had been consistently lower than expected based on previous average levels. During October numbers have increased and the most recent data show numbers are at a similar level to the pre-pandemic period. The average gestation at which women booked for antenatal care fell slightly from the end of March 2020, 
      before increasing back to previous levels around August 2020. The most recent data show that the average gestation at booking during September and October continues to be just below the average based on the pre-pandemic period. Looking at the data for women living in different NHS Board areas across Scotland, 
      the pattern of a temporary dip in gestation at booking coinciding with the first wave of the COVID-19 pandemic in Scotland is evident in some but not all areas. This probably reflects the fact that the detail of how maternity services were reconfigured in response to COVID-19 varied across Scotland. From August 2020 onwards, 
      the recorded gestation at booking has remained higher than usual for women living in NHS Tayside. Public Health Scotland is working with NHS Tayside to explore this issue."),
    h2("Antenatal bookings - 28th October 2020"),
    p("Information on the number of women booking for antenatal care, and the average gestation (stage of pregnancy) at which they booked, was included in this tool for the first time on 28 October 2020."),
    p("The ",
      tags$a(href = "https://www.nhsinform.scot/ready-steady-baby/pregnancy/your-antenatal-care/your-booking-appointment-booking-visit", "‘booking’ appointment", class="externallink",target="_blank"),
      " is the first main appointment a woman has with her local maternity service once she knows she is pregnant. At the booking appointment, women are assessed by a midwife who can then tailor the subsequent care they receive during their pregnancy to their particular preferences and needs.  Women are encouraged to book before they are 13 weeks pregnant, and ideally before they are 10 weeks pregnant."),
    p("As an essential service, maternity care including ‘booking’ has been provided throughout the COVID-19 pandemic, and ",
      tags$a(href = "https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies", "women have been encouraged to attend all their scheduled antenatal appointments", class="externallink"),
      ".  However, ",
      tags$a(href = "https://www.rcog.org.uk/globalassets/documents/guidelines/2020-07-10-guidance-for-antenatal-and-postnatal.pdf", "how some elements of maternity care are delivered has changed", class="externallink",target="_blank"),
      ", to minimise the number of visits women need to make to clinics and hospitals."),
    p("In general, prior to COVID-19, women were offered an initial in-person booking appointment (including various face to face tests such as blood tests and blood pressure monitoring) then a follow up appointment for their early pregnancy ultrasound scan. Since March 2020, in many areas women have been offered an initial remote consultation, for example using the Near Me video consultation system, then an in-person ‘one stop’ follow up appointment for all their face to face tests and their scan."),
    p("At the start of the COVID-19 pandemic, Public Health Scotland worked with NHS Boards to set up a new national data return providing information on women booking for antenatal care (see the Data source button on the dashboard page). This provides the information required to monitor in a timely way both the direct impact of COVID-19 on pregnant women, and the wider impacts of changes to maternity services and how women interact with services.  The data return is based on an extract of data recorded by midwives in local clinical information systems.  The information relates to the first main appointment a woman has with her maternity service: as noted above, during COVID-19 this will have changed from an in-person to a remote consultation in many areas."),
    p("The data shows that, at all Scotland level, the number of women booking for antenatal care week by week remained broadly constant from April 2019 (when the data starts) to the end of 2019. As would be expected there was then a dip reflecting the Christmas holidays, with higher numbers of women booking just before and just after the holidays.  The number of women booking then returned to previous levels until mid-May 2020.  From mid-May to end September (the latest point for which data is currently available), the number of women booking has been consistently lower than expected based on previous average levels.  Over the 19 weeks from week beginning 18 May 2020 to week beginning 21 September 2020, around 1,400 fewer women than would have been expected based on pre-pandemic levels have booked for antenatal care in Scotland.  As women most commonly book at around 9 weeks gestation, women booking from mid-May onwards will broadly reflect women getting pregnant from late March 2020 onwards, i.e. the point at which the initial UK wide lockdown was implemented in response to COVID-19."),
    p("Looking at the data for women living in different NHS Board areas across Scotland, the pattern of a recent fall in the number of women booking for antenatal care is evident in some but not all areas."),
    p("Fewer women booking for antenatal care could reflect fewer women who become pregnant choosing to continue with their pregnancy and/or fewer women becoming pregnant. It is therefore helpful to consider the data on antenatal booking alongside the data on terminations of pregnancy provided through this tool",
      actionLink("switch_to_top","(see the Commentary on Terminations of pregnancy  for more information)"),
      ".  Considering both sets of data, it seems likely that both reasons apply. It is likely that the higher than usual number of terminations of pregnancy provided in March and April 2020 at least partially contributed to the initial fall in the number of women booking for antenatal care from mid-May. Conversely, the subsequent sustained reduction seen in both the number of terminations and the number of women booking for antenatal care is likely to reflect a reduction in the number of women becoming pregnant from April 2020 onwards. Further analysis is required to accurately examine trends in the number of women becoming pregnant during the COVID-19 pandemic, their subsequent choices to continue with or terminate their pregnancy, and what this means for future trends in the number of births in Scotland."),      
    p("At all Scotland level, prior to COVID-19, the average gestation at which women booked for antenatal care was around 9 and a half weeks of pregnancy.  This fell slightly from the end of March 2020, reaching around 8 and a half weeks by end June 2020 before increasing back to previous levels from August 2020 onwards. This temporary reduction in the average gestation at booking means that the recent fall seen in the number of women booking is unlikely to be due to women deferring, or being unable to access, booking until later in their pregnancy.  This further confirms that it is likely that the number of women becoming pregnant has been lower than usual from April 2020 onwards."),
    p("Looking at the data for women living in different NHS Board areas across Scotland, the pattern of a temporary dip in gestation at booking coinciding with the first wave of the COVID-19 pandemic in Scotland is evident in some but not all areas.  This probably reflects the fact that the detail of how maternity services were reconfigured in response to COVID-19 varied across Scotland.  From August 2020 onwards, the recorded gestation at booking has been higher than usual for women living in NHS Tayside.  This is due to a temporary data recording issue following implementation of a new clinical information system in NHS Tayside at that time.  Public Health Scotland is working with NHS Tayside to resolve this and we expect that future releases of the antenatal booking data through this tool will see the average gestation return to a level which is more typical for NHS Tayside."),
    p("At all Scotland level, the recent reduction in the number of women booking for antenatal care has been more evident in younger (compared to older) women, and in women living in more (compared to less) deprived areas.  In general, there is no substantial variation in average gestation at booking by maternal age group or deprivation level.  The temporary dip in average gestation at booking associated with the first wave of COVID-19 in Scotland has been seen in women from all age groups and from all deprivation levels.")
  )
})


