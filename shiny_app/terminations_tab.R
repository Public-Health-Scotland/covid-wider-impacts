
# Pop-up modal explaining source of data
observeEvent(input$btn_top_modal, 
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
observeEvent(input$btn_modal_simd_top, { showModal(
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
## ToP Reactive controls  ----
###############################################.

# Pregnancy reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_top <- renderUI({
  #Lists areas available in   
  areas_summary_top <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_top])
  selectizeInput("geoname_top", label = NULL, choices = areas_summary_top, selected = "")
})


###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset behind trend plot (available at scotland and NHS board level)
top_filter <- function(){
  
  top %>% filter(area_name == input$geoname_top &
                       area_type == input$geotype_top &
                       type %in% c("Scotland","Health board"))
}

#Dataset behind deprivation/age plots (only available at scotland level)
top_filter_split <- function(split){
  
  top %>% filter(area_name == "Scotland" &
                       area_type == "Scotland" &
                       type==split) %>%
    droplevels()
  
}


###############################################.
## Termination Charts ----
###############################################.

# Creating plots for each dataset
output$top_trend <- renderPlotly({
  plot_data <- top_filter()
  
  # chart when numbers selected
  if(input$measure_select_top == "top_number"){ 
    
    yaxis_plots[["title"]] <- "Number of terminations"
    
    tooltip_top <- c(paste0("Month: ",plot_data$month,"<br>",
                                "Number of terminations: ",plot_data$terminations))
    
    #Creating time trend plot
    plot_ly(data=plot_data, x=~month) %>%
      add_lines(y = ~terminations,  
                line = list(color = "black"), text=tooltip_top, hoverinfo="text",
                marker = list(color = "black"), name = "Number of terminations") %>% 
      add_lines(y = ~dottedline_no, name = "Scotland projected",
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline") %>%
      add_lines(y = ~centreline_no, name = "Scotland centre line up to XXX need date?",
                line = list(color = "blue"), hoverinfo="none",
                name = "Centreline") %>% 
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  } else if (input$measure_select_top  == "top_gestation") {
    yaxis_plots[["title"]] <- "Average gestation at termination (weeks)"

    tooltip_top <- c(paste0("Month: ",plot_data$month,"<br>",
                                "Average gestation at termination: ",plot_data$av_gest," weeks"))                           
    
    #Creating time trend plot
    plot_ly(data=plot_data, x=~month) %>%
      add_lines(y = ~av_gest,  
                line = list(color = "black"), text=tooltip_top, hoverinfo="text",
                marker = list(color = "black"), name = "Average gestation") %>% 
      add_lines(y = ~dottedline_g, name = "Scotland projected",
                line = list(color = "blue", dash = "longdash"), hoverinfo="none",
                name = "Centreline") %>%
      add_lines(y = ~centreline_g, name = "Scotland centre line up to XXX need date",
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


output$top_age <- renderPlotly({plot_top_split(dataset=top_filter_split("age"), split="age")})
output$top_dep <- renderPlotly({plot_top_split(dataset=top_filter_split("dep"), split="dep")})


###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$top_explorer <- renderUI({
  
  # text for titles of cut charts
  top_title <- case_when(input$measure_select_top == "top_number" ~ paste0("Number of terminations: ", input$geoname_top),
                             input$measure_select_top == "top_gestation" ~ paste0("Average gestation at at termination: ", input$geoname_top))
  
  top_subtitle <-  paste0("Figures based on data extracted from XXXX on XXXX ")
  
  
  top_age_title <- case_when(input$measure_select_top == "top_number" ~ paste0("Number of terminations by age group: ", input$geoname_top),
                                 input$measure_select_top == "top_gestation" ~ paste0("Average gestation at termination by age group: ", input$geoname_top))
  
  top_dep_title <- case_when(input$measure_select_top == "top_number" ~ paste0("Numbers of terminations by deprivation: ", input$geoname_top),
                                 input$measure_select_top == "top_gestation" ~ paste0("Average gestation at termination by deprivation: ", input$geoname_top))
  
  
  #Additional commentart/meta data to appear on immunisation tab
  commentary_top <-  tagList(p("Space for any meta-data/commentary about terminations"))
  
  # Function to create common layout to all immunisation charts
  top_layout <- function(plot_trend, plot_age, plot_dep){
    tagList(fluidRow(column(12,
                            h4(paste0(top_title))),
                     p(top_subtitle),
                     withSpinner(plotlyOutput("top_trend"))),
            #only if scotland selected display age and deprivation breakdowns
            if (input$geotype_top == "Scotland"){
              fluidRow(column(6,br(), br(),
                              h4(paste0(top_age_title)),
                              br(), br(),
                              withSpinner(plotlyOutput("top_age"))),
                       column(6, br(), br(),
                              h4(paste0(top_dep_title)),
                              actionButton("btn_modal_simd_preg", "What is SIMD and deprivation?",
                                           icon = icon('question-circle')),
                              withSpinner(plotlyOutput("top_dep"))))},
            fluidRow(column(12, renderUI(commentary_top))))
  }
  
  #link plot functions to layouts
  if (input$measure_select_top == "top_number") {
    top_layout(plot_trend="top_trend", plot_age="plot_top_age", plot_dep="plot_top_dep")
  }  else if (input$measure_select_top == "top_gestation"){
    top_layout(plot_trend="top_trend")
  }
  
})




#############################################.
## Termination chart functions ----
############################################.

#function to draw trend chart

plot_top_split <- function(dataset, split){
  
  plot_data <- dataset
  
  #switch y-axis according to which measure is selected
  if(input$measure_select_top == "top_number"){
    yaxis_measure <- dataset$terminations
    yaxis_plots[["title"]] <- "Number of terminations"
    tooltip_top <- c(paste0("Week commencing: ",dataset$month,"<br>",
                                "Number of terminations: ",dataset$terminations))
    

  } else if (input$measure_select_top  == "top_gestation") {
    yaxis_measure <- dataset$av_gest
    yaxis_plots[["title"]] <- "Average gestation at top (weeks)"
    tooltip_top <- c(paste0("Week commencing: ",dataset$month,"<br>",
                                "Average gestation at termination: ",dataset$av_gest," weeks"))
  }
  
  
  #adjust datasets accordig to which data split to be displayed
  if(split == "age"){
    dataset <- dataset %>%
      mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29","30-34", "35-39", "40+")))
    pallette <- pal_age}
  
  if(split == "dep"){
    dataset <- dataset %>% 
      mutate(category = factor(category, levels = c("1 - most deprived", "2", "3","4", "5 - least deprived")))
    pallette <- pal_depr}
  
  #Creating time trend plot
  plot_ly(data=plot_data, x=~month, y = ~yaxis_measure) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category, 
              colors = pallette,
              text= tooltip_top, 
              hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
}
