# Functions for server side

###############################################.
# Function that creates line trend charts in Plotly for different splits: age, sex, depr
# THree parameters: pal_chose - what palette of colours you want
# dataset - what data to use for the chart formatted as required
# split - age, sex, or dep (simd deprivation)
plot_trend_chart <- function(dataset, pal_chose, split, type = "variation", data_name = NULL) {
  
  trend_data <- dataset %>% # filtering data by cut and area name
    filter(type == split &
             area_name == input$geoname )
  
  # Formatting age groups as factor so they appear in the correct order in the legend
  if (split == "age") {
    trend_data <- trend_data %>% 
      mutate(category = factor(category, levels = c("Under 5", "5 - 14", "Under 15", "15 - 44", "15 - 64", 
                                                    "45 - 64", "65 - 74", "65 and over", 
                                                    "75 - 84", "85 and over"))) 
    }

  
  if (type == "variation") {
    
    aver_period <- paste0(case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", "sas") ~ "2018-2019",
                             data_name == "deaths" ~ "2015-2019"))
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                              "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", "Change from ", aver_period, " average: ", round(trend_data$variation, 1), "%"))

      #Modifying standard layout
    yaxis_plots[["title"]] <- paste0("% change from ", aver_period, " average")
    
      #Creating time trend plot
      trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~variation) 
      
  } else if (type == "total") {
    
    ###############################################.
    # Creating objects that change depending on dataset
    yaxis_title <- case_when(data_name == "adm" ~ "Number of admissions",
                             data_name == "aye" ~ "Number of attendances",
                             data_name == "ooh" ~ "Number of consultations",
                             data_name == "nhs24" ~ "Number of completed contacts",
                             data_name == "sas" ~ "Number of incidents",
                             data_name == "deaths" ~ "Number of deaths")
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- yaxis_title
    
    measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                              data_name == "aye" ~ "Attendances: ",
                              data_name == "ooh" ~ "Consultations: ",
                              data_name == "nhs24" ~ "Completed contacts: ",
                              data_name == "sas" ~ "Incidents: ",
                              data_name == "deaths" ~ "Deaths: ")
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$category, "<br>",
                              "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", measure_name, trend_data$count,
                              "<br>", "Historic average: ", trend_data$count_average))
    
    #Creating time trend plot
    trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~count) 
    
  }
  
  #Creating time trend plot
  trend_plot %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category, colors = pal_chose,
              text=tooltip_trend, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  
}

plot_overall_chart <- function(dataset, data_name, yaxis_title) {
  
  # Filtering dataset to include only overall figures
  trend_data <- filter_data(dataset)
  
  ###############################################.
  # Creating objects that change depending on dataset
  yaxis_title <- case_when(data_name == "adm" ~ "Number of admissions",
                           data_name == "aye" ~ "Number of attendances",
                           data_name == "ooh" ~ "Number of consultations",
                           data_name == "nhs24" ~ "Number of completed contacts",
                           data_name == "sas" ~ "Number of incidents",
                           data_name == "deaths" ~ "Number of deaths")
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
  hist_legend <- ifelse(data_name == "deaths", "Average 2015-2019", "Average 2018-2019")
  
  measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                            data_name == "aye" ~ "Attendances: ",
                            data_name == "ooh" ~ "Consultations: ",
                            data_name == "nhs24" ~ "Completed contacts: ",
                            data_name == "sas" ~ "Incidents: ",
                            data_name == "deaths" ~ "Deaths: ")
  
  #Text for tooltip
  tooltip_trend <- c(paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                            "<br>", measure_name, trend_data$count,
                            "<br>", "Historic average: ", trend_data$count_average))
  
  #Creating time trend plot
  plot_ly(data=trend_data, x=~week_ending) %>%
    # 2020 line
    add_lines(y = ~count, line = list(color = pal_overall[1]),
              text=tooltip_trend, hoverinfo="text",
              name = "2020") %>%
    # Average of previous years line
    add_lines(y = ~count_average, line = list(color = pal_overall[2], dash = 'dash'),
              text=tooltip_trend, hoverinfo="text",
              name = hist_legend) %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  
}

###############################################.
# Function that creates specialty charts. Potentially could be merge with tren one 

plot_spec <- function(type) {
  trend_data <- rapid_spec()
  
  if (type == "variation") {
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$spec, "<br>", 
                              "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", "Change from 2018 - 2019 average: ", trend_data$variation, "%"))
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- "% change from 2018-19 average"
    
    #Creating time trend plot
    trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~variation) 
    
    
  } else if (type == "total") {
    
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- "Number of admissions"
    
    measure_name <- "Admissions: "
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$spec, "<br>",
                              "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", measure_name, trend_data$count,
                              "<br>", "Historic average: ", trend_data$count_average))
    
    #Creating time trend plot
    trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~count) 
    
  }
  
  
  #Creating time trend plot
  trend_plot %>% 
    add_trace(type = 'scatter', mode = 'lines+markers',
              color = ~spec, colors = pal_spec(), marker = list(size = 8),
              symbol = ~spec, symbols = symbol_spec(),
              text=tooltip_trend, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
           showlegend = TRUE, # in case only one spec selected, it still shows
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% # position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
}

### END