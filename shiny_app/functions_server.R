# Functions for server side

###############################################.
# Function that creates line trend charts in Plotly for different splits: age, sex, depr, condition
# THree parameters: pal_chose - what palette of colours you want
# dataset - what data to use for the chart formatted as required

# split - age, sex, condition, or dep (simd deprivation)
plot_trend_chart <- function(dataset, pal_chose, split = F, type = "variation", 
                             data_name = NULL, tab = "summary", period = "weekly") {
  
  period_data <- case_when(period == "weekly" ~ "Week ending: ",
                           period == "monthly" ~ "Month: ")
  
  if (split != FALSE) {
    if (tab == "summary") {
      trend_data <- dataset %>% # filtering data by cut and area name
        filter(type == split & area_name == input$geoname)
    } else if (tab == "cardio") {
      trend_data <- dataset %>% # filtering data by cut and area name
        filter(type %in% split)
    }
 # if (tab == "summary") {area_name == input$geoname} else if (tab == "cardio") {area_name == input$geoname_cardio})
  } else { # for cases outside summary tab
    trend_data <- dataset
  }
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50)
  } else {
    
  # Formatting age groups as factor so they appear in the correct order in the legend
  if ( split == "age") {
    if (tab == "summary") {
    trend_data <- trend_data %>% 
      mutate(category = factor(category, levels = c("Under 5", "5 - 14", "Under 65", "15 - 44",  
                                                    "45 - 64", "65 - 74", "65 and over", 
                                                    "75 - 84", "85 and over"))) 

    } else if (tab == "cardio") {
      trend_data <- trend_data %>% 
        mutate(category = factor(category, levels = c("All", "<65", "65+")))
    }
  } else if (split == "condition") {
      if (tab == "cardio") {
        trend_data <- dataset %>% 
          filter(type %in% split & area_name == input$geoname_cardio,
                 category != "All") %>% 
          # Wrapping long legend names
          mutate(category = case_when(
            category == "Antihypertensive, anti-anginal, anti-arrhythmic and heart failure drugs" ~ "Antihypertensive, \nanti-anginal, anti-arrhythmic \nand heart failure drugs",
            TRUE ~ category
          ))
      }
  } else {
    trend_data <- trend_data 
  }
  
  # If variation selected different values
  if (type == "variation") {
    
    aver_period <- paste0(case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", "sas", "drug_presc", "cath") ~ "2018-2019",
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
                             data_name == "cath" ~ "Number of cases",
                             data_name == "drug_presc" ~ "Number of items prescribed",
                             data_name == "deaths" ~ "Number of deaths")
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- yaxis_title
    
    measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                              data_name == "aye" ~ "Attendances: ",
                              data_name == "ooh" ~ "Consultations: ",
                              data_name == "nhs24" ~ "Completed contacts: ",
                              data_name == "sas" ~ "Incidents: ",
                              data_name == "cath" ~ "Cases: ",
                              data_name == "drug_presc" ~ "Items prescribed: ",
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

###############################################.
## Function for overall charts ----
###############################################.

plot_overall_chart <- function(dataset, data_name, yaxis_title, area = T) {
  
  # Filtering dataset to include only overall figures
  trend_data <- filter_data(dataset, area = area)
  
  ###############################################.
  # Creating objects that change depending on dataset
  yaxis_title <- case_when(data_name == "adm" ~ "Number of admissions",
                           data_name == "aye" ~ "Number of attendances",
                           data_name == "ooh" ~ "Number of consultations",
                           data_name == "nhs24" ~ "Number of completed contacts",
                           data_name == "sas" ~ "Number of incidents",
                           data_name == "cath" ~ "Number of cases",
                           data_name == "drug_presc" ~ "Number of items prescribed",
                           data_name == "deaths" ~ "Number of deaths")

  
  #Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
  hist_legend <- case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", "sas", "drug_presc", "cath") ~ "Average 2018-2019",
                          data_name == "deaths" ~ "Average 2015-2019")
  
  measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                            data_name == "aye" ~ "Attendances: ",
                            data_name == "ooh" ~ "Consultations: ",
                            data_name == "nhs24" ~ "Completed contacts: ",
                            data_name == "sas" ~ "Incidents: ",
                            data_name == "cath" ~ "Cases: ",
                            data_name == "drug_presc" ~ "Items prescribed: ",
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
## # Function that creates specialty charts.   ----
###############################################.
# Potentially could be merge with trend one
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

###############################################.
## Function for filtering ----
###############################################.
# Function to filter the datasets for the overall charts and download data based on user input
filter_data <- function(dataset, area = T) {
  if (area == T) {
    dataset %>% filter(type == "sex") %>%
      filter(area_name == input$geoname &
               category == "All")
  } else { #this works for cath data
    dataset %>% 
      filter(category == "All")
  }
}

#####################################################################################.
## Function for drawing S-Curve charts used in immunisation tabs.

plot_scurve <- function(dataset, age_week) {
 
  scurve_data <- dataset %>% filter(area_name == input$geoname_immun) 

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
  # To adjust x-axis min and max depending on which dose selected 
  xaxis_plots[["range"]] <- c((7*(as.numeric(age_week)-4)),((as.numeric(age_week)+16))*7) 
  # enforcing range from 0 to 100%
  yaxis_plots[["range"]] <- c(0, 100) 
  
  
  #Creating time trend plot
    plot_ly(data=scurve_data, x=~interv,  y = ~surv) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~time_period_eligible, colors = pal_immun,
              text= tooltip_scurve, hoverinfo="text") %>%

      # Adding legend title
      add_annotations( text= paste0("Children turning ", age_week, " weeks:"), xref="paper", yref="paper",
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


######################################################################.
#Function to create plot when no data available
plot_nodata <- function(height_plot = 450, text_nodata = "Data not available due to small numbers") {
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
## Function for generating flextable summary of immunisation data being displayed in s curve.

immune_table <- function(dataset, age_week) {

  table_data <- filter_table_data_immun(dataset)

  no_complete_row <- with(table_data, (substr(time_period_eligible,1,3) == "W/B"|substr(time_period_eligible,1,3) == "FEB"))
  
  if (age_week == 8) {
    #Apply different column names and formatting according to which dataset selected
    format_col <- c("denominator","uptake_12weeks_num","uptake_24weeks_num","uptake_tot_num")
    
    imm_table <- table_data %>%
      select (time_period_eligible, denominator,uptake_12weeks_num,uptake_12weeks_percent,uptake_24weeks_num, 
              uptake_24weeks_percent,uptake_tot_num,uptake_tot_percent) %>%
      flextable() %>%
      set_header_labels(uptake_12weeks_num="Children recorded as receiving their vaccine by 12 weeks of age",
                        uptake_12weeks_percent="Children recorded as receiving their vaccine by 12 weeks of age",
                        uptake_24weeks_num="Children recorded as receiving their vaccine by 24 weeks of age (or younger if children have not reached 24 weeks of age by the date data was extracted for analysis)",
                        uptake_24weeks_percent="Children recorded as receiving their vaccine by 24 weeks of age (or younger if children have not reached 24 weeks of age by the date data was extracted for analysis)") %>%
      # Italics and colour if not 24 weeks
      color(i = no_complete_row, j = c("uptake_24weeks_num", "uptake_24weeks_percent"), color="#0033cc")  %>% 
      italic(i = no_complete_row, j = c("uptake_24weeks_num", "uptake_24weeks_percent"))
    
  } else if (age_week == 12) {
    #Apply different column names and formatting according to which dataset selected
    format_col <- c("denominator","uptake_16weeks_num","uptake_28weeks_num","uptake_tot_num")

    imm_table <- table_data %>%
      select (time_period_eligible, denominator,uptake_16weeks_num,uptake_16weeks_percent,uptake_28weeks_num, 
              uptake_28weeks_percent,uptake_tot_num,uptake_tot_percent) %>%
      flextable() %>%
      set_header_labels(uptake_16weeks_num="Children recorded as receiving their vaccine by 16 weeks of age",
                        uptake_16weeks_percent="Children recorded as receiving their vaccine by 16 weeks of age ",
                        uptake_28weeks_num="Children recorded as receiving their vaccine by 28 weeks of age (or younger if children have not reached 28 weeks of age by the date data was extracted for analysis)",
                        uptake_28weeks_percent="Children recorded as receiving their vaccine by 28 weeks of age (or younger if children have not reached 28 weeks of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not 24 weeks
      color(i = no_complete_row, j = c("uptake_28weeks_num", "uptake_28weeks_percent"), color="#0033cc")  %>% 
      italic(i = no_complete_row, j = c("uptake_28weeks_num", "uptake_28weeks_percent")) 

  }else if (age_week == 16) {
    #Apply different column names and formatting according to which dataset selected
    format_col <- c("denominator","uptake_20weeks_num","uptake_32weeks_num","uptake_tot_num")
    
    imm_table <- table_data %>%
      select (time_period_eligible, denominator,uptake_20weeks_num,uptake_20weeks_percent,uptake_32weeks_num, 
              uptake_32weeks_percent,uptake_tot_num,uptake_tot_percent) %>%
      flextable() %>%
      set_header_labels(uptake_20weeks_num="Children recorded as receiving their vaccine by 20 weeks of age",
                        uptake_20weeks_percent="Children recorded as receiving their vaccine by 20 weeks of age ",
                        uptake_32weeks_num="Children recorded as receiving their vaccine by 32 weeks of age (or younger if children have not reached 32 weeks of age by the date data was extracted for analysis)",
                        uptake_32weeks_percent="Children recorded as receiving their vaccine by 32 weeks of age (or younger if children have not reached 32 weeks of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not  weeks
      color(i = no_complete_row, j = c("uptake_32weeks_num", "uptake_32weeks_percent"), color="#0033cc")  %>% 
      italic(i = no_complete_row, j = c("uptake_32weeks_num", "uptake_32weeks_percent")) 
    
  }
 imm_table %>% 
   set_header_labels(time_period_eligible= paste0("Children turning ", age_week, " weeks in:"),
                     denominator="Total number of children",
                     uptake_tot_num="Children recorded as receiving their vaccine by the date information was extracted for analysis (25-May-2020)",
                     uptake_tot_percent="Children recorded as receiving their vaccine by the date information was extracted for analysis (25-May-2020)") %>% 
   footnote(i = 1, j = c(1,2,4), 
            value = as_paragraph(c("W/B : Week beginning",
                                   "Cohort sizes are dependent on time periods whether, annual, monthly (4 or 5 weeks) or weekly",
                                   paste0("Blue cells indicate cohorts that have not reached ", age_week + 16," weeks of age"))),
            part = "header") %>%
   merge_at(i = 1, j = 3:4, part = "header") %>%
   merge_at(i = 1, j = 5:6, part = "header") %>%
   merge_at(i = 1, j = 7:8, part = "header") %>%
   add_header_row(values=c("","","N","%","N","%","N","%"), top = FALSE ) %>%
   font(fontname="Helvetica", part = "all") %>%
   colformat_num(j=format_col,big.mark = ",", digits=0) %>%
   theme_box() %>%
   autofit() %>%
   htmltools_value()
}

#####################################################################################.
## Function for drawing S-Curve charts used in health visitor tabs.

plot_scurve_child <- function(dataset, age_week) {
  
  scurve_data <- dataset %>% filter(area_name == input$geoname_child) 
  # %>%
  # droplevels() # might be needed if sort order in legend is to change
  
  if (is.data.frame(scurve_data) && nrow(scurve_data) == 0 && input$geoname_child == "NHS Grampian")
  { plot_nodata(height = 50, text_nodata = "Data not available due to data quality issues")
  } else if (is.data.frame(scurve_data) && nrow(scurve_data) == 0)
  { plot_nodata(height = 50)
  } else {
    
    #Create tooltip for scurve
    tooltip_scurve <- c(paste0("Cohort: ", scurve_data$time_period_eligible))
    
  if (age_week == "2 weeks")  {
    
    #Modifying standard yaxis layout
    yaxis_plots[["title"]] <- "% of children who have received their review"
    xaxis_plots[["title"]] <- "Age of children in weeks"
    # For custom tick labels
    xaxis_plots[["tickvals"]] <- c(0, seq(14, 126, by = 28))
    xaxis_plots[["ticktext"]] <- c(0, seq(2, 18, by = 4))
    # enforcing range from 0 to 100%
    yaxis_plots[["range"]] <- c(0, 100)
    
  } else if (age_week == "6 weeks") {
    
    #Modifying standard yaxis layout
    yaxis_plots[["title"]] <- "% of children who have received their review"
    xaxis_plots[["title"]] <- "Age of children in weeks"
    # For custom tick labels
    xaxis_plots[["tickvals"]] <- c(0, seq(42, 154, by = 28))
    xaxis_plots[["ticktext"]] <- c(0, seq(6, 22, by = 4))
    # enforcing range from 0 to 100%
    yaxis_plots[["range"]] <- c(0, 100)
    
  } else if (age_week == "13 months") {
    
    #Modifying standard yaxis layout
    yaxis_plots[["title"]] <- "% of children who have received their review"
    xaxis_plots[["title"]] <- "Age of children in months"
    # For custom tick labels
    xaxis_plots[["tickvals"]] <- c(0, seq(371, 518, by = 28))
    xaxis_plots[["ticktext"]] <- c(0, seq(12, 17, by = 1))
    # enforcing range from 0 to 100%
    xaxis_plots[["range"]] <- c(371, 518)
    yaxis_plots[["range"]] <- c(0, 100)
    
  } else if (age_week == "27 months") {
    
    #Modifying standard yaxis layout
    yaxis_plots[["title"]] <- "% of children who have received their review"
    xaxis_plots[["title"]] <- "Age of children in months"
    # For custom tick labels
    xaxis_plots[["tickvals"]] <- c(0, seq(791, 945, by = 28))
    xaxis_plots[["ticktext"]] <- c(0, seq(26, 31, by = 1))
    # enforcing range from 0 to 100%
    xaxis_plots[["range"]] <- c(791, 945)
    yaxis_plots[["range"]] <- c(0, 100)
    
  } 
    
    #Creating time trend plot
    plot_ly(data=scurve_data, x=~interv,  y = ~surv) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~time_period_eligible, colors = pal_child,
                text= tooltip_scurve, hoverinfo="text") %>%
      # Adding legend title
      add_annotations( text=paste0("Children turning ", age_week, " in:"), xref="paper", yref="paper",
                       x=1.02, xanchor="left",
                       y=0.8, yanchor="bottom",    # Same y as legend below
                       legendtitle=TRUE, showarrow=FALSE ) %>% 
      #Layout
      layout(margin = list(b = 80, t=12), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.8, yanchor="top")) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }}

#####################################################################################.
## Function for generating flextable summary of child health data being displayed in s curve.

child_table <- function(dataset, age_week, age_not_reached) {
  
  table_data <- filter_table_data_child(dataset)
  
  if (age_week == "2 weeks") {
    format_col <- c("denominator","coverage_6weeks_num","coverage_18weeks_num","coverage_tot_num")
    no_complete_row <- with(table_data, (substr(time_period_eligible,1,3) == "W/B" |
                                           time_period_eligible == "MAR 2020"))
    
    child_table <- table_data %>%
    select (time_period_eligible, denominator, coverage_6weeks_num, 
            coverage_6weeks_percent, coverage_18weeks_num, coverage_18weeks_percent, 
            coverage_tot_num, coverage_tot_percent) %>%
    flextable() %>%
    set_header_labels(coverage_6weeks_num="Children recorded as receiving their health visitor first visit by 6 weeks of age",
                      coverage_6weeks_percent="Children recorded as receiving their health visitor first visit by 6 weeks of age",
                      coverage_18weeks_num="Children recorded as receiving their health visitor first visit by 18 weeks of age (or younger if children have not reached 18 weeks of age by the date data was extracted for analysis)",
                      coverage_18weeks_percent="Children recorded as receiving their health visitor first visit by 18 weeks of age (or younger if children have not reached 18 weeks of age by the date data was extracted for analysis)") %>% 
    # Italics and colour if not 12 weeks
    color(i = no_complete_row, j = c("coverage_18weeks_num", "coverage_18weeks_percent"), color="#0033cc")  %>%
    italic(i = no_complete_row, j = c("coverage_18weeks_num", "coverage_18weeks_percent"))
  } 
  else if (age_week == "6 weeks") {
    format_col <- c("denominator","coverage_12weeks_num","coverage_24weeks_num","coverage_tot_num")
    no_complete_row <- with(table_data, (substr(time_period_eligible,1,3) == "W/B"|
                                           substr(time_period_eligible,1,3) == "FEB" |
                                           substr(time_period_eligible,1,3) == "MAR"))
    
    child_table <- table_data %>%
      select (time_period_eligible, denominator, coverage_12weeks_num, 
              coverage_12weeks_percent, coverage_24weeks_num, coverage_24weeks_percent, 
              coverage_tot_num, coverage_tot_percent) %>%
      flextable() %>%
      set_header_labels(coverage_12weeks_num="Children recorded as receiving their 6-8 week review by 12 weeks of age",
                        coverage_12weeks_percent="Children recorded as receiving their 6-8 week review by 12 weeks of age",
                        coverage_24weeks_num="Children recorded as receiving their 6-8 week review by 24 weeks of age (or younger if children have not reached 24 weeks of age by the date data was extracted for analysis)",
                        coverage_24weeks_percent="Children recorded as receiving their 6-8 week review by 24 weeks of age (or younger if children have not reached 24 weeks of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not 12 weeks
      color(i = no_complete_row, j = c("coverage_24weeks_num", "coverage_24weeks_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row, j = c("coverage_24weeks_num", "coverage_24weeks_percent"))
  }
  else if (age_week == "13 months") {
    format_col <- c("denominator","coverage_14months_num","coverage_17months_num","coverage_tot_num")
    no_complete_row <- with(table_data, (substr(time_period_eligible,1,3) == "W/B"|
                                           substr(time_period_eligible,1,3) == "FEB" |
                                           substr(time_period_eligible,1,3) == "MAR"))
    
    child_table <- table_data %>%
      select (time_period_eligible, denominator, coverage_14months_num, 
              coverage_14months_percent, coverage_17months_num, coverage_17months_percent, 
              coverage_tot_num, coverage_tot_percent) %>%
      flextable() %>%
      set_header_labels(coverage_14months_num="Children recorded as receiving their 13-15 month review by 14 months of age",
                        coverage_14months_percent="Children recorded as receiving their 13-15 month review by 14 months of age",
                        coverage_17months_num="Children recorded as receiving their 13-15 month review by 17 months of age (or younger if children have not reached 17 months of age by the date data was extracted for analysis)",
                        coverage_17months_percent="Children recorded as receiving their 13-15 month review by 17 months of age (or younger if children have not reached 17 months of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not 17 months
      color(i = no_complete_row, j = c("coverage_17months_num", "coverage_17months_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row, j = c("coverage_17months_num", "coverage_17months_percent"))
  }
  else if (age_week == "27 months") {
    format_col <- c("denominator","coverage_28months_num","coverage_31months_num","coverage_tot_num")
    no_complete_row <- with(table_data, (substr(time_period_eligible,1,3) == "W/B"|
                                           substr(time_period_eligible,1,3) == "FEB"|
                                           substr(time_period_eligible,1,3) == "MAR"))
    
    child_table <- table_data %>%
      select (time_period_eligible, denominator, coverage_28months_num, 
              coverage_28months_percent, coverage_31months_num, coverage_31months_percent, 
              coverage_tot_num, coverage_tot_percent) %>%
      flextable() %>%
      set_header_labels(coverage_28months_num="Children recorded as receiving their 27-30 month review by 28 months of age",
                        coverage_28months_percent="Children recorded as receiving their 27-30 month review by 28 months of age",
                        coverage_31months_num="Children recorded as receiving their 27-30 month review by 31 months of age (or younger if children have not reached 31 months of age by the date data was extracted for analysis)",
                        coverage_31months_percent="Children recorded as receiving their 27-30 month review by 31 months of age (or younger if children have not reached 31 months of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not 17 months
      color(i = no_complete_row, j = c("coverage_31months_num", "coverage_31months_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row, j = c("coverage_31months_num", "coverage_31months_percent"))
  }
  
  child_table %>% 
    set_header_labels(time_period_eligible=paste0("Children turning ", age_week, " in:"),
                      denominator="Total number of children",
                      coverage_tot_num="Children recorded as receiving their review by the date information was extracted for analysis (22-June-2020)",
                      coverage_tot_percent="Children recorded as receiving their review by the date information was extracted for analysis (22-June-2020)") %>%
    footnote(i = 1, j = c(1:2, 4),
             value = as_paragraph(c("W/B : Week beginning",
                                    "Cohort sizes are dependent on time periods whether, annual, monthly (4 or 5 weeks) or weekly",
                                    paste0("Blue cells indicate cohorts that have not reached ", age_not_reached, " of age"))),
             part = "header") %>%
    merge_at(i = 1, j = 3:4, part = "header") %>%
    merge_at(i = 1, j = 5:6, part = "header") %>%
    merge_at(i = 1, j = 7:8, part = "header") %>%
    add_header_row(values=c("","","N","%","N","%","N","%"), top = FALSE ) %>%
    font(fontname="Helvetica", part = "all") %>%
    colformat_num(j=format_col,big.mark = ",", digits=0) %>%
    theme_box() %>%
    autofit() %>%
    htmltools_value()
  
}

### END