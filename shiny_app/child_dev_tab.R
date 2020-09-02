##Server script for child development tab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_childdev_modal,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p("Data source: xxxx."),
               p("Placeholder"),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
output$geoname_childdev_ui <- renderUI({
  
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_childdev])
  
  selectizeInput("geoname_childdev", label = NULL,
                 choices = areas_summary, selected = "")
  
})

###############################################.
##  Reactive datasets  ----
###############################################.
child_dev_filt <- reactive({
  
  review_chosen <- case_when( input$measure_select_childdev == "13_15mnth" ~ "13-15 months",
                              input$measure_select_childdev == "27_30mnth" ~ "27-30 months")
  
  child_dev %>% filter(area_name == input$geoname_childdev &
                         area_type == input$geotype_childdev &
                         review == review_chosen)
})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$childdev_explorer <- renderUI({
  
  tagList(
    fluidRow(column(12, 
                    p("Placeholder for intro text if required"),
                    h4(paste0("Child development reviews at ", input$measure_select_childdev)))),
    fluidRow(withSpinner(plotlyOutput("childdev_concern")))
    )#tagLIst bracket
  
  }) #close perinatal_explorer function

###############################################.
## Charts ----
###############################################.
# output$childdev_concerns_chart <- renderPlotly({
#   # Filtering dataset to include only overall figures
#   trend_data <- filter_data(child_dev, area = F)
#   
#   ###############################################.
#   # Creating objects that change depending on dataset
#   yaxis_title <- case_when(data_name == "adm" ~ "Number of admissions",
#                            data_name == "aye" ~ "Number of attendances",
#                            data_name == "ooh" ~ "Number of consultations",
#                            data_name == "nhs24" ~ "Number of completed contacts",
#                            data_name == "sas" ~ "Number of incidents",
#                            data_name == "cath" ~ "Number of cases",
#                            data_name == "drug_presc" ~ "Number of items prescribed",
#                            data_name == "deaths" ~ "Number of deaths")
#   
#   
#   #Modifying standard layout
#   yaxis_plots[["title"]] <- yaxis_title
#   
#   hist_legend <- case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", "sas", "drug_presc", "cath") ~ "Average 2018-2019",
#                            data_name == "deaths" ~ "Average 2015-2019")
#   
#   measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
#                             data_name == "aye" ~ "Attendances: ",
#                             data_name == "ooh" ~ "Consultations: ",
#                             data_name == "nhs24" ~ "Completed contacts: ",
#                             data_name == "sas" ~ "Incidents: ",
#                             data_name == "cath" ~ "Cases: ",
#                             data_name == "drug_presc" ~ "Items prescribed: ",
#                             data_name == "deaths" ~ "Deaths: ")
#   
#   #Text for tooltip
#   tooltip_trend <- c(paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y"),
#                             "<br>", measure_name, trend_data$count,
#                             "<br>", "Historic average: ", trend_data$count_average))
#   
#   #Creating time trend plot
#   plot_ly(data=trend_data, x=~week_ending) %>%
#     # 2020 line
#     add_lines(y = ~pc_1_plus, line = list(color = pal_overall[1]),
#               text=tooltip_trend, hoverinfo="text",
#               name = "2020") %>%
#     # Average of previous years line
#     add_lines(y = ~concerns_1_plus_aver, line = list(color = pal_overall[2], dash = 'dash'),
#               text=tooltip_trend, hoverinfo="text",
#               name = hist_legend) %>%
#     #Layout
#     layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
#            yaxis = yaxis_plots, xaxis = xaxis_plots,
#            legend = list(x = 100, y = 0.5)) %>% #position of legend
#     # leaving only save plot button
#     config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
# })

# output$childdev_concern <- renderPlotly({
#   plot_overall_chart(dataset = child_dev_filt(), data_name = "childdev", area = "All",
#                      var2020 = "pc_1_plus", var_aver = "pc_1_plus_aver",
#                      xvar = "month_review", filtering = F)})

output$childdev_concern <- renderPlotly({

    trend_data <- child_dev_filt()
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "% of children with 1+ developmental concerns recorded"
  xaxis_plots[["dtick"]] <- 1
  
  hist_legend <- "Average 2018-2019"
  
  xaxis_plots[["tickvals"]] <- unique(trend_data$month_review)
  xaxis_plots[["ticktext"]] <- format(unique(trend_data$month_review), "%B %Y")
  
    tooltip_trend <- c(paste0("Month:", format(trend_data$month_review, "%B %y"),
                              "<br>", "Children with recorded concerns", trend_data$concerns_1_plus,
                              "<br>", "% children with recorded concerns", trend_data$pc_1_plus,
                              "<br>", "Historic average: ", trend_data$pc_1_plus_aver))
    
  #Creating time trend plot
  plot_ly(data=trend_data, x=~month_review) %>%
    # 2020 line
    add_lines(y = ~pc_1_plus, line = list(color = pal_overall[1]),
              text=tooltip_trend, hoverinfo="text",
              name = "2020") %>%
    # Average of previous years line
    add_lines(y = ~pc_1_plus_aver, line = list(color = pal_overall[2], dash = 'dash'),
              text=tooltip_trend, hoverinfo="text",
              name = hist_legend) %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
})

###############################################.
## Data downloads ----
###############################################.

output$download_childdev_data <- downloadHandler(
  filename ="stillbirth_infantdeaths_extract.csv",
  content = function(file) {
    write_csv(perinatal_down_data(),
              file) } 
)


###############################################.
## Commentary ----
###############################################.
output$childdev_commentary <- renderUI({
  tagList(
    bsButton("jump_to_childdev",label = "Go to data"), #this button can only be used once
    h2("Child development reviews -  2020"),
    p("Placeholder")
  ) #tagLIst bracket
})
