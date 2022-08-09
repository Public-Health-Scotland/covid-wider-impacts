# Wider impacts dashboard - Births and babies tab - location of extremely preterm deliveries section
# Server code

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$`preterm-source-modal`,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p("Data source: SMR02"),
               p("The data used for the Location of extremely preterm deliveries page comes from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended home births."),
               p("For the Location of extremely preterm deliveries page, SMR02 records for deliveries at 23-26 weeks gestation that resulted in the birth of one or more live born babies have been used. The charts presented show the total number of deliveries at 23-26 weeks inclusive that resulted in the birth of one or more live born babies, and the number and percentage of these deliveries that occurred in a hospital that had a neonatal intensive care unit (NICU) on site at the time of the delivery.  Data is presented for sequential quarters from January-March 2018 onwards, based on the date the woman was discharged from hospital after delivery.  Due to the small number of deliveries at this very early gestation, data is only shown at all Scotland level."),
               p("For the purpose of this measure, since January 2018, the following hospitals in Scotland have been considered to have a NICU on site for the period specified:"),
               tags$ul(tags$li("A111H University Hospital Crosshouse (up until end September 2019 only) (Best Start early adopter since Oct 2019)"),
                       tags$li("F704H Victoria Hospital, Kirkcaldy (up until end September 2019 only) (Best Start early adopter since Oct 2019)"),
                       tags$li("F705H Victoria Maternity Unit, Kirkcaldy (up until end September 2019 only) (same location as F704H)"),
                       tags$li("G108H The Princess Royal Maternity Unit"),
                       tags$li("G405H Queen Elizabeth University Hospital"),
                       tags$li("G513H Royal Hospital for Children (same location as G405H)"),
                       tags$li("L308H University Hospital Wishaw"),
                       tags$li("N101H Aberdeen Royal Infirmary"),
                       tags$li("N161H Aberdeen Maternity Hospital (same location as N101H)"),
                       tags$li("S314H Royal Infirmary of Edinburgh at Little France"),
                       tags$li("T101H Ninewells Hospital")),
               p("The following approach has been used to classify deliveries as occurring in a hospital with a NICU on site. The location of delivery as recorded on SMR02 was compared to the above list of hospitals to determine whether the delivery occurred at a hospital with a NICU on site.   Admissions following home births or any deliveries occurring en route to hospital were not counted as a delivery in a hospital with a NICU on site. In the calculation of the percentage of deliveries occurring in a hospital with a NICU on site admissions following home births and deliveries occurring en route to hospital are excluded from the numerator but are included in the denominator.  There were no deliveries where location of delivery was recorded as unknown or missing."),
               p("Data is shown for up to and including the most recent quarter for which SMR02 records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland. In addition, the recording of specific data items allowing identification of deliveries at 23-26 weeks gestation, and the location of these deliveries, is very complete. Since January 2018 recording of gestation at delivery on SMR02 was known and within the range 18-44 in 99.7% of deliveries. Location of delivery was recorded in 100% of deliveries. The latest ",
                 tags$a(href= 'https://www.isdscotland.org/Products-and-Services/Data-Quality/docs/20191023-Assessment-of-SMR02-Data-Scotland-2017-2018.pdf',
                        'Data Quality Assessment Report', target="_blank"),
                 " found gestation to be accurately recorded in 95% of cases although there was no information provided on the accuracy of the location of delivery data item."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href= "https://publichealthscotland.scot/publications/births-in-scottish-hospitals",
                        "Births in Scottish Hospitals report." , target="_blank")),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SPC charts rules
observeEvent(input$btn_preterm_rules,
             showModal(modalDialog(
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

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$preterm_explorer <- renderUI({
  
  # text for titles of cut charts
  preterm_title <- paste0("Percentage of deliveries at 23-26 weeks gestation resulting in a live born baby that occur in a hospital with a neonatal intensive care unit on site: Scotland")

  control_chart_commentary <- p("As deliveries at 23-26 weeks gestation are relatively rare events in Scotland, the percentage of these deliveries that occurs in a hospital with a neonatal intensive care unit on site will fluctuate over time just by chance. We have therefore used ‘control charts’ to present the percentages above.", br(),
                                "Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’", br(),
                                "The dots joined by a solid black line in the chart above show the percentage of deliveries at 23-26 weeks gestation inclusive that occurred in a hospital with a neonatal intensive care unit on site, for sequential quarters from January-March 2018 onwards.", br(),  
                                "The other lines - centreline, and control and warning limits - are there to help show how unexpected any observed changes are. The centreline is an average (mean) over the time period. Control and warning limits take into consideration the random variation that would be expected by chance, and help us decide when values are unexpectedly low or high and require further investigation.")
  
  # Specify items to display in preterm ui 
  tagList(
    fluidRow(column(12, 
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

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
preterm_filter <- function(){

  preterm_chart %>%
  # Sorting levels based on date
    mutate(quarter =
             factor(quarter,
                    levels = as.character(preterm_chart[order(preterm_chart$quarter_of_year),]$quarter))) # to allow sorting in x axis later on
}

## run chart function to generate spc charts
output$preterm_chart <- renderPlotly({
  trend_data <- preterm_filter()

  yaxis_plots[["title"]] <- "Percentage of deliveries" 
                                      
  xaxis_plots[["title"]] <- "Quarter"
  
  # Tooltip
  tooltip_trend <- c(paste0(format(trend_data$quarter), "<br>", 
                            "Percentage: ", round(trend_data$percentage_NICU_site, 1)))
  
  xaxis_plots <- c(xaxis_plots, dtick = 2, tickangle = 0) #adding parameters to axis layout
  
  plot_ly(data = trend_data, x = ~quarter) %>%
    add_lines(y = ~percentage_NICU_site, line = list(color = "black"),
              text=tooltip_trend, hoverinfo="text",
              marker = list(color = "black"),
              name = "Percentage") %>%
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
    add_markers(data = trend_data %>% filter(outlier == T), y = ~ percentage_NICU_site,
                marker = list(color = "red", size = 10, symbol = "diamond"), name = "Outliers") %>% 
    # adding shifts
    add_markers(data = trend_data %>% filter(shift == T), y = ~ percentage_NICU_site,
                marker = list(color = "blue", size = 10, symbol = "circle"), name = "Shifts") %>% 
    # adding shifts
    add_markers(data = trend_data %>% filter(trend == T), y = ~ percentage_NICU_site,
                marker = list(color = "green", size = 10, symbol = "square"), name = "Trends") %>% 
    # adding inner third
    add_markers(data = trend_data %>% filter(inner == T), y = ~ percentage_NICU_site,
                marker = list(color = "gray", size = 10, symbol = "x"), name = "Inner one-third") %>% 
    # adding outer third
    add_markers(data = trend_data %>% filter(outer == T), y = ~ percentage_NICU_site,
                marker = list(color = "orange", size = 10, symbol = "star"), name = "Outer one-third") %>% 
    layout( #to avoid labels getting cut out
      yaxis = yaxis_plots, xaxis = xaxis_plots,
      legend = list(orientation = 'h')) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
})

#####################################################################################################################.
## LINECHART SCOTLAND
output$preterm_linechart <- renderPlotly({  
  
  plot_data <- preterm_linechart
  
  #arrange sort order for gestation categories
  plot_data <- plot_data %>%
    mutate(ind = factor(ind, levels = c("Deliveries 23-26w in hosp with NICU", "All deliveries 23-26w")))
  #pick a colour palette to apply
  pallette <- pal_age
  
  yaxis_plots[["title"]] <- "Number of deliveries"

  # Create tooltip for line chart
  tooltip <- c(paste0( plot_data$ind,"<br>",
                       "Area: ",plot_data$area_name,"<br>",
                       "Quarter: ",  format(plot_data$quarter_label),"<br>",
                       "Number of births: ", plot_data$mats,"<br>",
                       "Percentage of births: ", format(plot_data$percent_nicu,digits = 1,nsmall=1),"%"))
  
  xaxis_plots <- c(xaxis_plots,
                   dtick = 2, tickangle = 0,
                   categoryorder = "array", categoryarray = ~quarter)
    
    #Creating trend plot
    plot_ly(data=plot_data, x=~quarter_label,  y = ~mats) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~ind, colors = pallette,
                text= tooltip, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots,  
             xaxis = xaxis_plots,
             legend = list(orientation = 'h')) %>% #position of legend underneath plot
      #leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
})

###############################################.
## Data downloads ----
###############################################.

preterm_download_data <- reactive({
  preterm_chart %>% 
    select(quarter, N_deliveries_23_26_NICU_site, N_deliveries_23_26,
           percentage_NICU_site, centreline, stdev,
           upper_cl_3_std_dev, lower_cl_3_std_dev,
           upper_wl_2_std_dev, lower_wl_2_std_dev,
           area_name, outlier, shift, trend, outer, inner)
})

output$download_preterm_data <- downloadHandler(
  filename ="preterm_extract.csv",
  content = function(file) {
    write_csv(preterm_download_data(),
              file) } 
)



##END
