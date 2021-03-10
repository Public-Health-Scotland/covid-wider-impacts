##Server script for preterm tab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_preterm_modal,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p("Data source: SMR02"),
               p("The data source ..."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SPC charts rules
observeEvent(input$btn_preterm_rules,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "How do we identify patterns in the data?",
               p("Control charts follow a series of rules that help identify important changes in the data. 
                 These are the ones we used in this chart:"),
               tags$ul(tags$li("Outliers: Data points outside the limits marked by the control limits."),
                       tags$li("Shifts: Eight or more consecutive data points above or below the centreline."),
                       tags$li("Trends: Six or more consecutive data points which are increasing or decreasing."),
                       tags$li("Outer One – Third: Two out of three consecutive data points which sit between the control and warning limits."),
                       tags$li("Inner One -Third: 15 or more consecutive data points that lie close to the centreline.")),
               p("The type of chart used depends on the type of 
                 data involved (which statistical distribution we think it follows).
                 For the extremely preterm deliveries in a hospital with a NICU indicator P 
                 charts are presented."),
               p("Further information on these methods of presenting data can be found at the ",                      
                 tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                        'PHS guide to statistical process control charts.')),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# ###############################################.
# ## Induction Chart calls to chart function ----
# ###############################################.
# 
# #chart outputs for line charts for NHS board and Scot
# output$preterm_linechart <- renderPlotly({plot_preterm_linechart(measure="nicu")})
# output$apgar_linechart_percent <- renderPlotly({plot_apgar_linechart(measure="percent_nicu")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$preterm_explorer <- renderUI({
  
  # text for titles of cut charts
  preterm_title <- paste0("Proportion of deliveries at 23-26 weeks gestation resulting in a live born baby that occur in a hospital with a neonatal intensive care unit on site: Scotland")
  
  
  # Intro paragraph within preterm tab
  intro_text <- p("Preterm intro ...")
  
  
  control_chart_commentary <- p("As extremely preterm deliveries are relatively rare events in Scotland rates tend to fluctuate over time just by chance.
                      We have therefore used", tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf', 
                                                      "‘control charts’", target="_blank"), "to present the rates above.", br(),
                                "Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation.  
                      Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’", br(),
                                "The dots joined by a solid line in the chart above show the quarterly proportion of deliveries at 23-26 weeks gestation that occur in a hospital with a neonatal intensive care unit on site from January 2018 onwards.", br(),  
                                "The other lines - centreline, and control and warning limits - are there to help show how unexpected any observed changes are. 
                      The centreline is an average (mean) over the time period. Control and warning limits take into consideration the random variation 
                      that would be expected by chance, and help us decide when values are unexpectedly low or high and require further investigation.")
  
  # Specify items to display in preterm ui 
  tagList(
    fluidRow(column(12, 
                    intro_text,
                    h4(paste0(preterm_title)))),
    actionButton("btn_preterm_rules", "How do we identify patterns in the data?", 
                 icon = icon('question-circle')),
    fluidRow(withSpinner(plotlyOutput("preterm_chart"))),
    fluidRow(column(12, renderUI(control_chart_commentary))),
    fluidRow(column(12, h4(paste0("Number of deliveries at 23-26 weeks gestation resulting in a live born baby that occur in a hospital with a neonatal intensive care unit on site: Scotland")))),
    fluidRow(withSpinner(plotlyOutput("preterm_linechart"))))
  
}) #close perinatal_explorer function

###############################################.
## Charts ----
###############################################.

## run chart function to generate spc charts
output$preterm_chart <- renderPlotly({
  trend_data <- preterm_chart
  
  yaxis_plots[["title"]] <- "Proportion of deliveries at 23-26 weeks gestation resulting in a live born baby that occur in a hospital with a neonatal intensive care unit on site" 
                                      
  xaxis_plots[["title"]] <- "Quarter"
  
  # Tooltip
  tooltip_trend <- c(paste0(format(trend_data$quarter_of_year, "%B %y"), "<br>", 
                            "Proportion: ", round(trend_data$rate, 1)))
  
  plot_ly(data = trend_data, x = ~quarter_of_year) %>%
    add_lines(y = ~rate, line = list(color = "black"),
              text=tooltip_trend, hoverinfo="text",
              marker = list(color = "black"),
              name = "Rate") %>%
    add_lines(y = ~centreline, line = list(color = "blue", dash = "longdash"),
              hoverinfo= "none", name = "Centreline") %>%
    add_lines(y = ~upper_cl_3_std_dev, line = list(color = "red", dash = "dash"),
              hoverinfo= "none", name = "Control limits") %>%
    add_lines(y = ~lower_cl_3_std_dev, line = list(color = "red", dash = "dash"),
              hoverinfo= "none", showlegend = FALSE) %>%
    add_lines(y = ~upper_wl_2_std_dev, line = list(color = "#33ccff", dash = "dot"),
              hoverinfo= "none", name = "Warning limits") %>%
    add_lines(y = ~lower_wl_2_std_dev, line = list(color = "#33ccff", dash = "dot"),
              hoverinfo= "none", showlegend = FALSE) %>%
    # adding outliers
    add_markers(data = trend_data %>% filter(outlier == T), y = ~ rate,
                marker = list(color = "red", size = 10, symbol = "diamond"), name = "Outliers") %>% 
    # adding shifts
    add_markers(data = trend_data %>% filter(shift == T), y = ~ rate,
                marker = list(color = "blue", size = 10, symbol = "circle"), name = "Shifts") %>% 
    # adding shifts
    add_markers(data = trend_data %>% filter(trend == T), y = ~ rate,
                marker = list(color = "green", size = 10, symbol = "square"), name = "Trends") %>% 
    # adding inner third
    add_markers(data = trend_data %>% filter(inner == T), y = ~ rate,
                marker = list(color = "gray", size = 10, symbol = "x"), name = "Inner one-third") %>% 
    # adding outer third
    add_markers(data = trend_data %>% filter(outer == T), y = ~ rate,
                marker = list(color = "orange", size = 10, symbol = "star"), name = "Outer one-third") %>% 
    layout( #to avoid labels getting cut out
      yaxis = yaxis_plots, xaxis = xaxis_plots) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
})

#####################################################################################################################.
## LINECHART SCOTLAND
output$preterm_linechart <- renderPlotly({  
  
  plot_data <- preterm_linechart
  
  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(ind = factor(ind, levels = c("Neonate deliveries in NICU", "All neonate deliveries")))
  #pick a colour palette to apply
  pallette <- pal_age
  
  # # adjust chart y axis according to what is being displayed
  # if(measure == "percent_nicu"){
  #   yaxis_plots[["title"]] <- "Percentage of deliveries (%)" 
  #   yaxis_plots[["range"]] <- c(0, 100)  # forcing range from 0 to 10%
  #   
  #   plot_data <- plot_data %>%  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
  #     filter(ind!="All neonate deliveries")
  #   
  # }
  # 
  # if(measure == "nicu"){
    yaxis_plots[["title"]] <- "Number of deliveries"
    # plot_data <- plot_data %>% #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
    #   filter(ind!="37 to 42 weeks")
  # }
  # Create tooltip for line chart
  tooltip <- c(paste0( plot_data$ind,"<br>",
                       "Area: ",plot_data$area_name,"<br>",
                       "Quarter: ",  format(plot_data$quarter, "%B %Y"),"<br>",
                       "Number of births: ", plot_data$mats,"<br>",
                       "Percentage of births: ", format(plot_data$percent_nicu,digits = 1,nsmall=1),"%"))
  
  # if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  # { plot_nodata(height = 50, 
  #               text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  # } else {
    
    #Creating trend plot
    plot_ly(data=plot_data, x=~quarter,  y = ~mats) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~ind, colors = pallette,
                text= tooltip, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
})

###############################################.
## Data downloads ----
###############################################.

preterm_download_data <- reactive({
  preterm_download
})

output$download_preterm_data <- downloadHandler(
  filename ="preterm_extract.csv",
  content = function(file) {
    write_csv(preterm_download_data(),
              file) } 
)

###############################################.
## Commentary tab content  ----
###############################################.

#action associated with action links within commentary text - this observe event linked to an actionLink within the TOP commentary which will take the user from TOP commentary to ANB commentary easily.
observeEvent(input$switch_to_preterm,{
  updateTabsetPanel(session, "intabset", selected = "comment")
  updateCollapse(session, "collapse_commentary", open = "Extremely preterm deliveries in a hospital with a NICU")
})


output$preterm_commentary <- renderUI({
  tagList(
    bsButton("jump_to_preterm",label = "Go to data"), #this button can only be used once
    h2("Extremely preterm deliveries in a hospital with a NICU - April 2021"))
  
})


##END
