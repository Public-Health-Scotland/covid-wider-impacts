##Server script for antenatal booking tab

# Pop-up modal explaining source of data
observeEvent(input$btn_booking_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The Antenatal Booking Data presented is based on a new data collection established as a rapid response to COVID-19. Data is collected each week, from the clinical information system (BadgerNet Maternity (most NHS boards), TrakCare Maternity (Lothian) or Eclipse (A&A) used by the midwives who ‘book’ the pregnant woman for maternity care."),
               p("Historic data from APRIL 2019 was also collected as a ‘catch-up’ extract in order to identify all women who were currently pregnant during the COVID-19 period. This was either from the same source or - in Tayside and Highland - from the systems in use before the introduction of BadgerNet Maternity."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))


# Modal to explain SIMD and deprivation
# Link action button click to modal launch 
observeEvent(input$btn_modal_simd_preg, { showModal(
  modalDialog(
    h5("What is SIMD and deprivation?"),
    p("Children have been allocated to different levels of deprivation based on the small area (data zone) 
      in which they live and the", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD).",
                                          class="externallink"), "score for that area. 
      SIMD scores are based on data for local areas reflecting 38 indicators across 7 domains: 
      income; employment; health; education, skills and training; housing; geographic access; and crime. 
      In this tool we have presented results for children living in different SIMD ‘quintiles’. 
      To produce quintiles, data zones are ranked by their SIMD score then the areas each containing a fifth (20%) 
      of the overall population of Scotland are identified. Children living in the most and least deprived areas 
      that each contain a fifth of the population are assigned to SIMD quintile 1 and 5 respectively."),
    size = "l", 
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  ))}) 


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
output$booking_trend_n <- renderPlotly({plot_booking_trend(measure="booking_number")})
output$booking_trend_g <- renderPlotly({plot_booking_trend(measure="booking_gestation")})

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
  booking_trend_title <- paste0("Antenatal bookings: ",input$geoname_booking)
  booking_title_n <-  paste0("Number of bookings")
  booking_title_g <-  paste0("Average gestation at booking (completed weeks)")
  
  chart_explanation <- paste0("The black line on the ‘antenatal booking numbers’ charts for Scotland, and each Health Board, shows a weekly time series of data. The solid blue centreline is the average number of bookings over the period 1st Mar 2019 to 28 Feb 2020. The dotted line continues that average to allow determination of whether there has been a change.  The ‘average gestation at antenatal booking’ charts follow a similar format.")
  
  #Additional commentart/meta data to appear on booking tab
  commentary_booking <-  tagList(p("Space for any meta-data/commentary about booking"))
  
  # Function to create common layout to all immunisation charts
  booking_layout <- function(plot_trend_n,plot_trend_g, plot_age_n, plot_age_g, plot_dep_n, plot_dep_g){
    tagList(fluidRow(column(12,
                            h4(booking_trend_title),
                            p(booking_subtitle),
                            p(chart_explanation)),
                     column(6,
                            h4(paste0(booking_title_n)),
                            withSpinner(plotlyOutput("booking_trend_n"))),
                     column(6,
                            h4(paste0(booking_title_g)),
                            withSpinner(plotlyOutput("booking_trend_g")))),
                     # column(12,
                     #        p(chart_explanation))),
            #only if scotland selected display age and deprivation breakdowns
            if (input$geotype_booking == "Scotland"){
              tagList(
                fluidRow(column(12,h4("Antenatal bookings by age group: Scotland"))),
                fluidRow(column(6,
                                h4("Number of bookings"),br(),
                                withSpinner(plotlyOutput("booking_age_n"))),
                         column(6,
                                h4("Average gestation at booking (completed weeks)"),br(),
                                withSpinner(plotlyOutput("booking_age_g"))),
                         fluidRow(column(12,h4("Antenatal booking by deprivation: Scotland"),
                                         actionButton("btn_modal_simd_booking", "What is SIMD and deprivation?",
                                                      icon = icon('question-circle')))),
                         column(6,
                                h4("Number of bookings"),br(),
                                withSpinner(plotlyOutput("booking_dep_n"))),
                         column(6,
                                h4("Average gestation at booking (completed weeks)"),br(),
                                withSpinner(plotlyOutput("booking_dep_g"))))
              )#tagList from if statement
            },
            fluidRow(column(12, renderUI(commentary_booking))))
  }
  
  #link plot functions to layouts
  booking_layout(plot_trend_n="booking_trend_n", plot_trend_g="booking_trend_g",
                 plot_age_n="booking_age_n", plot_age_g="booking_age_g",
                 plot_dep_n="booking_dep_n", plot_dep_g="booking_dep_g")
}) #close booking explorer


#############################################.
## Antenatal booking chart functions ----
############################################.

## Trend plot for weekly bookings numbers and average gestation at booking
plot_booking_trend <- function(measure){
  
  plot_data <- ante_booking_filter()
  
  # Display message if island/small board is supplied and no chart available
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50, text_nodata = "No data shown for small island boards")
  } else {
    
    # legend label should respond when dataset updates  
    centreline <- paste0("Scotland centre line up to ","xxxx")
    
    #switch y-axis according to which measure is selected
    if(measure == "booking_number"){
      yaxis_measure <- plot_data$booked_no
      yaxis_plots[["title"]] <- "Number of bookings"
      tooltip_booking <- c(paste0("Week commencing: ",format(plot_data$week_book_starting,"%d %b %y"),"<br>",
                                  "Number of antenatal bookings: ",plot_data$booked_no))
      dotted_line <-  plot_data$dottedline_no
      centre_line <-  plot_data$centreline_no
      yname <- "# booking"
      
    } else if (measure  == "booking_gestation") {
      yaxis_measure <- plot_data$ave_gest
      yaxis_plots[["title"]] <- "Average gestation at booking (completed weeks)"
      tooltip_booking <- c(paste0("Week commencing: ",format(plot_data$week_book_starting,"%d %b %y"),"<br>",
                                  "Average gestation: ",format(plot_data$week_book_starting,"%d %b %y")," weeks"))
      dotted_line <-  plot_data$dottedline_g
      centre_line <-  plot_data$centreline_g
      yname <- "Average gestation"
    }
    
    #Creating time trend plot
    plot_ly(data=plot_data, x=~week_book_starting) %>%
      add_lines(y = ~yaxis_measure,  
                line = list(color = "black"), text=tooltip_booking, hoverinfo="text",
                marker = list(color = "black"), name = yname) %>% 
      add_lines(y = ~dotted_line, name = "Scotland projected",
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline") %>%
      add_lines(y = ~centre_line, name = centreline,
                line = list(color = "blue"), hoverinfo="none",
                name = "Centreline") %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(x = 0.1, y = 0.1)) %>% #position of legend
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }
  
}


## Trend plot for weekly bookings numbers and average gestation at booking split by age group and simd quintile 
plot_booking_split <- function(dataset, split, measure){
  
  plot_data <- dataset
  
  #label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"),split=="dep" ~ paste0("Deprivation group:"))
  
  #switch y-axis according to which measure is selected
  if(measure == "booking_number"){
    yaxis_measure <- dataset$booked_no
    yaxis_plots[["title"]] <- "Number of bookings"
    tooltip_booking <- c(paste0(tool_tip_split,dataset$category,"<br>",
                                "Week commencing: ",format(plot_data$week_book_starting,"%d %b %y"),"<br>",
                                "Number of antenatal bookings: ",plot_data$booked_no))
    
  } else if (measure  == "booking_gestation") {
    yaxis_measure <- dataset$ave_gest
    yaxis_plots[["title"]] <- "Average gestation at booking (completed weeks)"
    tooltip_booking <- c(paste0(tool_tip_split,dataset$category,"<br>",
                                "Week commencing: ",format(dataset$week_book_starting,"%d %b %y"),"<br>",
                                "Average gestation: ",dataset$ave_gest," weeks"))
  }
  
  #adjust datasets accordig to which data split to be displayed
  if(split == "age"){
    dataset <- dataset %>%
      mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29","30-34", "35-39", "40 plus")))
    pallette <- pal_age}
  
  if(split == "dep"){
    dataset <- dataset %>% 
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
           legend = list(x = 100, y = 0.5)) %>% #position of legend
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
output$booking_commentary <- renderUI({
  tagList(
    bsButton("jump_to_booking",label = "Go to data"), #this button can only be used once
    h2("Antenatal bookings - 28th October 2020"))
})


