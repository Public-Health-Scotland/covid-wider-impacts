##Server script for antenatal booking tab

# Pop-up modal explaining source of data
observeEvent(input$btn_booking_modal, 
             showModal(modalDialog(#Maternal HEALTH MODAL
               title = "What is the data source?",
               p("The information shown on the numbers of children eligible for routine preschool reviews is taken from the",
                 tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12", 
                        "Scottish Immunisation and Recall System (SIRS)", class="externallink"),
                 ". The information recorded at each review is taken from the",
                 tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=10",
                        "Child Health Systems Programme-PreSchool (CHSP-PS)", class="externallink"),
                 "."),
               p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation of children when a scheduled vaccination is due."),
               p("CHSP-PS is an electronic system used by all NHS Boards in Scotland. The CHSP Pre-School system supports the delivery of the child health programme by facilitating the automated call and recall of children for the agreed schedule of child health reviews for pre-school children. Child health reviews incorporate assessment of children's health, development, and wider wellbeing alongside provision of health promotion advice and parenting support."),
               p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink")," routinely receives quarterly data extracts from SIRS and CHSP-PS for the purpose of producing and ",
                 (tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/child-health-pre-school-review-coverage/","publishing",class="externallink"))," coverage rates for child health reviews. To allow more rapid monitoring of the impact of Covid-19 on child health review coverage rates, PHS is also currently extracting a sub-set of data from SIRS & CHSP-PS each month."),
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
  )
) }) 

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

    booking %>% filter(area_name == input$geoname_booking &
                         area_type == input$geotype_booking &
                         type %in% c("Scotland","Health board"))
}

#Dataset behind deprivation/age plots (only available at scotland level)
ante_booking_filter_split <- function(split){
  
  booking %>% filter(area_name == "Scotland" &
                       area_type == "Scotland" &
                       type==split) %>%
  droplevels()
  
}


###############################################.
## Antenatal Booking Charts ----
###############################################.

# Creating plots for each dataset
output$booking_trend <- renderPlotly({
  plot_data <- ante_booking_filter()
  
  # chart when numbers selected
  if(input$measure_select_booking == "booking_number"){ 
    
    yaxis_plots[["title"]] <- "Number of bookings"
    tooltip_booking <- c(paste0("Month:"))  
    
    #Creating time trend plot
    plot_ly(data=plot_data, x=~week_book_starting) %>%
      add_lines(y = ~booked,  
                line = list(color = "black"), text=tooltip_booking, hoverinfo="text",
                marker = list(color = "black"), name = "# booking") %>% 
      add_lines(y = ~dottedline, name = "Scotland projected",
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline") %>%
      add_lines(y = ~centreline, name = "Scotland centre line up to 23rd March 2020",
                line = list(color = "blue"), hoverinfo="none",
                name = "Centreline") %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  } else if (input$measure_select_booking  == "booking_gestation") {
    
    yaxis_plots[["title"]] <- "Average gestation at booking (weeks)"
    tooltip_booking <- c(paste0("Month:"))
    
    #Creating time trend plot
    plot_ly(data=plot_data, x=~week_book_starting) %>%
      add_lines(y = ~booked_g,  
                line = list(color = "black"), text=tooltip_booking, hoverinfo="text",
                marker = list(color = "black"), name = "# booking") %>% 
      add_lines(y = ~dottedline_g, name = "Scotland projected",
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline") %>%
      add_lines(y = ~centreline_g, name = "Scotland centre line up to 23rd March 2020",
                line = list(color = "blue"), hoverinfo="none",
                name = "Centreline") %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }
})

output$booking_age <- renderPlotly({plot_booking_split(dataset=ante_booking_filter_split("age"), split="age")})
output$booking_dep <- renderPlotly({plot_booking_split(dataset=ante_booking_filter_split("dep"), split="dep")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$booking_explorer <- renderUI({
  
  # text for titles of cut charts
  booking_title <- case_when(input$measure_select_booking == "booking_number" ~ paste0("Antenatal booking numbers: ", input$geoname_booking),
                             input$measure_select_booking == "booking_gestation" ~ paste0("Average gestation at antenatal booking: ", input$geoname_booking))
  
  booking_subtitle <-  paste0("Figures based on data extracted from XXXX on XXXX ")
  
  #Additional commentart/meta data to appear on immunisation tab
  commentary_booking <-  tagList(p("Space for any meta-data/commentary about booking"))
  
  # Function to create common layout to all immunisation charts
  booking_layout <- function(plot_trend, plot_age, plot_dep){
    tagList(fluidRow(column(12,
                            h4(paste0(booking_title))),
                     p(booking_subtitle),
                     withSpinner(plotlyOutput("booking_trend"))),
            #only if scotland selected display age and deprivation breakdowns
            if (input$geotype_booking == "Scotland"){
              fluidRow(column(6,br(), br(),
                              #h4(paste0(preg_age_title)),
                              br(), br(),
                              withSpinner(plotlyOutput("booking_age")),
                              p("test1")),
                       column(6, br(), br(),
                              #h4(paste0(preg_dep_title)),
                              actionButton("btn_modal_simd_preg", "What is SIMD and deprivation?",
                                           icon = icon('question-circle')),
                              withSpinner(plotlyOutput("booking_dep")),
                              p("test2")
                       ))},
            fluidRow(column(12, renderUI(commentary_booking))))
  }
  
  #link plot functions to layouts
  if (input$measure_select_booking == "booking_number") {
    booking_layout(plot_trend="booking_trend", plot_age="plot_booking_age", plot_dep="plot_booking_dep")
  }  else if (input$measure_select_booking == "booking_gestation"){
    booking_layout(plot_trend="booking_trend")
    # booking_layout(plot_trend="booking_trend", plot_dep="preg_booking_dep", plot_age="preg_booking_age")
  }
  
})



#############################################.
## Antenatal booking chart functions ----
############################################.

#function to draw trend chart

plot_booking_split <- function(dataset, split){
  
  #dataset <- ante_booking_filter_split()
  plot_data <- dataset
  
  if(split == "age"){
    dataset <- dataset %>%
      mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29",
                                                    "30-34", "35-39", "40 plus")))}
  if(split == "dep"){
    dataset <- dataset %>% 
      mutate(category = factor(category, levels = c("1 - most deprived", "2", "3","4", "5 - least deprived")))} 
  
  tooltip_booking <- c(paste0("Month:"))
  
  #Creating time trend plot
  plot_ly(data=plot_data, x=~week_book_starting, y = ~booked) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category, 
              #colors = pallette
              text= tooltip_booking, 
              hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,  xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
}