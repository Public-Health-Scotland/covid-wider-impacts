##Server script for maternal health tab

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

# Reactive dataset for flextable filter on geographical area and dose
# booking_data_filter_trend <- function(dataset){
#   
#   #filter dataset according to whether NHS board or Scotland data selected
#   if(input$geotype_booking=="Scotland"){
#     #mat_chosen %>% filter(area_type== input$geotype_mat & category=="All")
#     booking_chosen <- dataset %>% 
#       filter(area_type== "Scotland" & category=="All")
#   } else if (input$geotype_booking=="Health board"){
#     booking_chosen <- dataset %>%
#       filter(area_type== input$geotype_booking & area_name==input$geoname_booking)
#   }}


#filter data for maternal health trend chart
booking_data_filter_trend <- reactive({

  #change dataset that will be filtered and used in chart construction
  booking_chosen <-  case_when(input$measure_select_booking == "preg_number" ~ booking_no,
                           input$measure_select_booking == "preg_gestation" ~ booking_gest)
  #booking_chosen <- booking_no

  #filter dataset according to whether NHS board or Scotland data selected
  if(input$geotype_booking=="Scotland"){
    #mat_chosen %>% filter(area_type== input$geotype_mat & category=="All")
    booking_chosen %>% filter(area_type== "Scotland" & category=="All")
  } else if (input$geotype_booking=="Health board"){
    booking_chosen %>% filter(area_type== input$geotype_booking & area_name==input$geoname_booking)
  }
})

#filter data for antenatal booking deprivation chart
booking_data_filter_split <- reactive({
 #mat_chosen <-  case_when(input$measure_select_mat == "ante_booking" ~ booking,
 #                        input$measure_select_mat == "top" ~ top1)
   booking_chosen <-  case_when(input$measure_select_booking == "preg_number" ~ booking_no,
                               input$measure_select_booking == "preg_gestation" ~ booking_gest)
  
   booking_chosen %>%
     filter(area_type== "Scotland")

  })


###############################################.
## Preganancy Charts ----
###############################################.

# Creating plots for each dataset

output$preg_booking_trend <- renderPlotly({plot_preg_trend(dataset=booking_data_filter_trend())})
output$preg_booking_dep <- renderPlotly({plot_preg_split(dataset=booking_data_filter_split(), split="dep")})
output$preg_booking_age <- renderPlotly({plot_preg_split(dataset=booking_data_filter_split(), split="age")})
#output$mat_booking_dep <- renderPlotly({plot_mat_split(dataset=booking,split="age")})

output$pregnancy_explorer <- renderUI({
 
   # text for titles of cut charts
   preg_title <- case_when(input$measure_select_booking == "preg_number" ~ paste0("Antenatal booking numbers: ", input$geoname_booking),
                             input$measure_select_booking == "preg_gestation" ~ paste0("Average gestation at antenatal booking: ", input$geoname_booking))
   
   preg_subtitle <-  paste0("Figures based on data extracted from XXXX on XXXX ")
   
   preg_dep_title <- case_when(input$measure_select_booking == "preg_number" ~ paste0("Antenatal booking numbers by deprivation: ", input$geoname_booking),
                          input$measure_select_booking == "preg_gestation" ~ paste0("Average gestation at antenatal booking by deprivation: ", input$geoname_booking))
   
   preg_age_title <- case_when(input$measure_select_booking == "preg_number" ~ paste0("Antenatal booking numbers by age group: ", input$geoname_booking),
                               input$measure_select_booking == "preg_gestation" ~ paste0("Average gestation at antenatal booking by age group: ", input$geoname_booking))
   
   
   #Additional commentart/meta data to appear on immunisation tab
   commentary_booking <-  tagList(p("Space for any meta-data/commentary about booking"))
   
   # Function to create common layout to all immunisation charts
   preg_layout <- function(plot_trend, plot_dep, plot_age) {
       tagList(fluidRow(column(12,
                             h4(paste0(preg_title)),
                             p(preg_subtitle),
                             withSpinner(plotlyOutput(plot_trend)))),
             if (input$geotype_booking == "Scotland"){
             fluidRow(column(6,br(), br(),
                             h4(paste0(preg_age_title)),
                             br(), br(),
                      #withSpinner(plotlyOutput(plot_age)),
                      p("test1")),
                      column(6, br(), br(),
                             h4(paste0(preg_dep_title)),
                             actionButton("btn_modal_simd_preg", "What is SIMD and deprivation?",
                                          icon = icon('question-circle')),
                            # withSpinner(plotlyOutput(plot_dep)),
                             p("test2")
                             ))},
             fluidRow(column(12, renderUI(commentary_booking)))
             )}

  #link plot functions to layouts
   if (input$measure_select_booking == "preg_number") {
     preg_layout(plot_trend="preg_booking_trend", plot_dep="preg_booking_dep", plot_age="preg_booking_age")
   }  else if (input$measure_select_booking == "preg_gestation"){
     preg_layout(plot_trend="preg_booking_trend", plot_dep="preg_booking_dep", plot_age="preg_booking_age")
   }

    }) # close maternal explorer function

 #############################################
 ## Maternal health chart functions----
 ############################################
 
plot_preg_trend <- function(dataset) {
  
  dataset_name <- deparse(substitute(dataset)) # character name of the data

  
  # chart axis for maternal health
  if(dataset_name == "booking_no"){ #
    
    dataset
    yaxis_plots[["title"]] <- "Number of bookings"}
    #xaxis_plots[["title"]] <- "Age of children in weeks"
    #xaxis_plots[["tickvals"]] <- c(0, seq(56, 308, by = 28))
    #xaxis_plots[["ticktext"]] <- c(0, seq(8, 44, by = 4))
    #xaxis_plots[["range"]] <- c((7*(as.numeric(age_week)-4)),((as.numeric(age_week)+16))*7) # To adjust x-axis min and max depending on which dose selected
    #age_unit <- paste0(age_week, " weeks:") #string for legend label
  ##chart axis for MMR dose 1 scurve
  else if(dataset_name == "booking_gest"){ #s
    yaxis_plots[["title"]] <- "Average gestation at booking"}

  tooltip_booking <- c(paste0("Month:"))
  
  #Creating time trend plot
  plot_ly(data=dataset, x=~week_book_starting) %>%
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
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
  }

plot_preg_split <- function(dataset, split) {
  
  dataset_name <- deparse(substitute(dataset)) # character name of the data
  split_name <- deparse(substitute(split)) # character name of the data
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Number of bookings"
  
  # chart axis for maternal health
   if(dataset_name == "booking_no"){ #
     yaxis_plots[["title"]] <- "Number of bookings"
     dataset <- dataset %>%
       filter(area_type== "Scotland" & type==split_name) %>%
       droplevels()
   }
   ##chart axis for top scurve
   else if(dataset_name == "booking_gest"){ #s
     yaxis_plots[["title"]] <- "Average gestation at booking"
     dataset <- dataset %>%
       filter(area_type== "Scotland" & type==split_name) %>%
       droplevels()
   }
  
  if(split == "age"){
   dataset <- dataset %>%
     mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29",
                                                   "30-34", "35-39", "40 plus")))}
  if(split == "dep"){
    dataset <- dataset %>% 
      mutate(category = factor(category, levels = c("1 - most deprived", "2", "3","4", "5 - least deprived")))} 
   
   dataset <- dataset %>%
     filter(area_type== "Scotland" & type== split) %>%
     droplevels()
  
    
  tooltip_booking <- c(paste0("Month:"))
  
  #Creating time trend plot
  plot_ly(data=dataset, x=~week_book_starting, y = ~booked) %>%
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
     config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
}

