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
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50)
  } else {
    
  # Formatting age groups as factor so they appear in the correct order in the legend
  if (split == "age") {
    trend_data <- trend_data %>% 
      mutate(category = factor(category, levels = c("Under 5", "5 - 14", "Under 65", "15 - 44",  
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
           showlegend = TRUE, # always show legen
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% # position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
}

######################################################################.
#Function to create plot when no data available
plot_nodata <- function(height_plot = 450, text_nodata = "No data available") {
  text_na <- list(x = 5, y = 5, text = text_nodata , size = 20,
                  xref = "x", yref = "y",  showarrow = FALSE)
  
  plot_ly(height = height_plot) %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')) %>% 
    config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
} 

#####################################################################################.
## Function for drawing S-Curve charts used in immunisation and health visitor tabs.

plot_scurve <- function(dataset) {
  
  scurve_data <- dataset %>% filter(area_name == input$geoname_immun) 
  # %>%
  # droplevels() # might be needed if sort order in legend is to change
  
  if (is.data.frame(scurve_data) && nrow(scurve_data) == 0)
  { plot_nodata(height = 50)
  } else {
  
  #Create tooltip for scurve
  tooltip_scurve <- c(paste0("Cohort: ", scurve_data$time_period_eligible))
  
  #Modifying standard yaxis layout
  yaxis_plots[["title"]] <- "% of children who have received their vaccine"
  xaxis_plots[["title"]] <- "Age of children in weeks"
  # For custom tick labels
  xaxis_plots[["tickvals"]] <- c(0, seq(56, 308, by = 28))
  xaxis_plots[["ticktext"]] <- c(0, seq(8, 44, by = 4))
  
  #Creating time trend plot
    plot_ly(data=scurve_data, x=~interv,  y = ~surv) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~time_period_eligible, colors = pal_immun,
              text= tooltip_scurve, hoverinfo="text") %>%
      # Adding legend title
      add_annotations( text="Children turning 8 weeks in:", xref="paper", yref="paper",
                       x=1.02, xanchor="left",
                       y=0.8, yanchor="bottom",    # Same y as legend below
                       legendtitle=TRUE, showarrow=FALSE ) %>% 
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
         yaxis = yaxis_plots, xaxis = xaxis_plots,
         legend = list(x = 100, y = 0.8, yanchor="top")) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }}

#####################################################################################.
## Function for generating flextable summary of immunisation data being displayed in s curve.

immune_table <- function() {
  format_col <- c("denominator","uptake_12weeks_num","uptake_24weeks_num","uptake_tot_num")
  no_24_row_id <- with(table_data(), (substr(time_period_eligible,1,3) == "W/B"|substr(time_period_eligible,1,3) == "FEB"))
  
  table_data() %>%
    select (time_period_eligible, denominator,uptake_12weeks_num,uptake_12weeks_percent,uptake_24weeks_num, uptake_24weeks_percent,uptake_tot_num,uptake_tot_percent) %>%
    flextable() %>%
    set_header_labels(time_period_eligible="Children turning 8 weeks in:",
                      denominator="Total number of children",
                      uptake_12weeks_num="Children recorded as receiving their vaccine by 12 weeks of age",
                      uptake_12weeks_percent="Children recorded as receiving their vaccine by 12 weeks of age",
                      uptake_24weeks_num="Children recorded as receiving their vaccine by 24 weeks of age (or younger if children have not reached 24 weeks of age by the date data was extracted for analysis)",
                      uptake_24weeks_percent="Children recorded as receiving their vaccine by 24 weeks of age (or younger if children have not reached 24 weeks of age by the date data was extracted for analysis)",
                      uptake_tot_num="Children recorded as receiving their vaccine by the date information was extracted for analysis (25-May-2020)",
                      uptake_tot_percent="Children recorded as receiving their vaccine by the date information was extracted for analysis (25-May-2020)") %>%
    footnote(i = 1, j = c(1,2,4), 
             value = as_paragraph(c("W/B : Week beginning",
                                    "Cohort sizes are dependent on time periods whether, annual, monthly (4 or 5 weeks) or weekly",
                                    "Blue cells indicate cohorts that have not reached 24 weeks of age")),
             part = "header") %>%
    merge_at(i = 1, j = 3:4, part = "header") %>%
    merge_at(i = 1, j = 5:6, part = "header") %>%
    merge_at(i = 1, j = 7:8, part = "header") %>%
    add_header_row(values=c("","","N","%","N","%","N","%"), top = FALSE ) %>%
    font(fontname="Helvetica", part = "all") %>%
    colformat_num(j=format_col,big.mark = ",", digits=0) %>%
    # Italics and colour if not 24 weeks
    color(i = no_24_row_id, j = c("uptake_24weeks_num", "uptake_24weeks_percent"), color="#0033cc")  %>% 
    italic(i = no_24_row_id, j = c("uptake_24weeks_num", "uptake_24weeks_percent")) %>% 
    theme_box() %>%
    autofit() %>%
    htmltools_value()
  
}

### END