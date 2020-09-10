##Server script for maternal health tab

# Pop-up modal explaining source of data
observeEvent(input$btn_mat_modal, 
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
observeEvent(input$btn_modal_simd_mat, { showModal(
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
## Maternal Health Reactive controls  ----
###############################################.

# maternal reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_mat <- renderUI({
  
  #Lists areas available in   
  areas_summary_mat <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_mat])
  
  selectizeInput("geoname_mat", label = NULL, choices = areas_summary_mat, selected = "")
})


###############################################.
##  Reactive datasets  ----
###############################################.

#filter data for maternal health trend chart 
mat_data_filter <- reactive({
  
  #mat_chosen <-  case_when(input$measure_select_mat == "ante_booking" ~ booking,
   #                        input$measure_select_mat == "top" ~ top1)
  mat_chosen <- booking
  
  if(input$geotype_mat=="Scotland"){
    #mat_chosen %>% filter(area_type== input$geotype_mat & category=="All")
    mat_chosen %>% filter(area_type== "Scotland" & category=="All")
  } else if (input$geotype_mat=="Health board"){
    mat_chosen %>% filter(area_type== input$geotype_mat & area_name==input$geoname_mat)
  }
})

#filter data for maternal health dep chart

mat_data_filter_split <- reactive({
 #mat_chosen <-  case_when(input$measure_select_mat == "ante_booking" ~ booking,
 #                        input$measure_select_mat == "top" ~ top1)
 
   mat_chosen <- booking %>%  filter(area_type== "Scotland")

  })



###############################################.
## Maternal health Charts ----
###############################################.

# Creating plots for each dataset

output$mat_booking_trend <- renderPlotly({plot_mat_trend(dataset=mat_data_filter())})
output$mat_booking_dep <- renderPlotly({plot_mat_split(dataset=mat_data_filter_split(), split="dep")})
output$mat_booking_age <- renderPlotly({plot_mat_split(dataset=mat_data_filter_split(), split="age")})
#output$mat_booking_dep <- renderPlotly({plot_mat_split(dataset=booking,split="age")})



 output$maternal_explorer <- renderUI({
 
   # text for titles of cut charts
   mat_title <- case_when(input$measure_select_mat == "ante_booking" ~ paste0("Antenatal booking numbers: ", input$geoname_mat),
                             input$measure_select_mat == "top" ~ paste0("Terminations: ", input$geoname_mat))
   
   mat_subtitle <-  paste0("Figures based on data extracted from XXXX on XXXX ")
   
   mat_dep_title <- case_when(input$measure_select_mat == "ante_booking" ~ paste0("Antenatal booking numbers by deprivation: ", input$geoname_mat),
                          input$measure_select_mat == "top" ~ paste0("Terminations by deprivation: ", input$geoname_mat))
   
   mat_age_title <- case_when(input$measure_select_mat == "ante_booking" ~ paste0("Antenatal booking numbers by age group: ", input$geoname_mat),
                               input$measure_select_mat == "top" ~ paste0("Terminations by age group: ", input$geoname_mat))
   
   
   #Additional commentart/meta data to appear on immunisation tab
   commentary_booking <-  tagList(p("Space for any meta-data/commentary about booking"))
   
   # Function to create common layout to all immunisation charts
   mat_layout <- function(plot_trend, plot_dep, plot_age) {
       tagList(fluidRow(column(12,
                             h4(paste0(mat_title)),
                             p(mat_subtitle),
                             withSpinner(plotlyOutput(plot_trend)))),
             if (input$geotype_mat == "Scotland"){
             fluidRow(column(6,br(), br(),
                             h4(paste0(mat_age_title)),
                             br(), br(),
                      withSpinner(plotlyOutput(plot_age))),
                      column(6, br(), br(),
                             h4(paste0(mat_dep_title)),
                             actionButton("btn_modal_simd_mat", "What is SIMD and deprivation?",
                                          icon = icon('question-circle')),
                             withSpinner(plotlyOutput(plot_dep))))},
             fluidRow(column(12, renderUI(commentary_booking)))
             )}
             
  #link plot functions to layouts   
   if (input$measure_select_mat == "ante_booking") {
     mat_layout(plot_trend="mat_booking_trend", plot_dep="mat_booking_dep", plot_age="mat_booking_age")
   }  else if (input$measure_select_mat == "top"){
     mat_layout()
   }
   
    }) # close maternal explorer function

 #############################################
 ## Maternal health chart functions----
 ############################################
 
plot_mat_trend <- function(dataset) {
  
  dataset_name <- deparse(substitute(dataset)) # character name of the data

  # chart axis for maternal health
  if(dataset_name == "ante_booking"){ #
    yaxis_plots[["title"]] <- "Number of bookings"}
    #xaxis_plots[["title"]] <- "Age of children in weeks"
    #xaxis_plots[["tickvals"]] <- c(0, seq(56, 308, by = 28))
    #xaxis_plots[["ticktext"]] <- c(0, seq(8, 44, by = 4))
    #xaxis_plots[["range"]] <- c((7*(as.numeric(age_week)-4)),((as.numeric(age_week)+16))*7) # To adjust x-axis min and max depending on which dose selected
    #age_unit <- paste0(age_week, " weeks:") #string for legend label
  ##chart axis for MMR dose 1 scurve
  else if(dataset_name == "top"){ #s
    yaxis_plots[["title"]] <- "Number of terminations"}

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

plot_mat_split <- function(dataset, split) {
  
  dataset_name <- deparse(substitute(dataset)) # character name of the data
  split_name <- deparse(substitute(split)) # character name of the data
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Number of bookings"
  
  # chart axis for maternal health
   if(dataset_name == "ante_booking"){ #
     yaxis_plots[["title"]] <- "Number of bookings"
     dataset <- dataset %>%
       filter(area_type== "Scotland" & type==split_name) %>%
       droplevels()
   }
   ##chart axis for top scurve
   else if(dataset_name == "top"){ #s
     yaxis_plots[["title"]] <- "Number of terminations"
   }
  
  if(split == "age"){
   dataset <- dataset %>%
     mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29",
                                                   "30-34", "35-39", "40 plus")))}
  if(split == "dep"){
    dataset <- dataset %>% 
      mutate(category = factor(category, levels = c("SIMD 1", "SIMD 2", "SIMD 3",  
                                                    "SIMD 4", "SIMD 5")))} 
   
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

