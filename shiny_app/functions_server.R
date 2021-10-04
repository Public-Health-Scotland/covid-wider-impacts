# Functions for server side

# Helper function
`%notin%` <- Negate(`%in%`)

###############################################.
# Function that creates line trend charts in Plotly for different splits: age, sex, depr, condition
# THree parameters: pal_chose - what palette of colours you want
# dataset - what data to use for the chart formatted as required

# split - age, sex, condition, or dep (simd deprivation)
plot_trend_chart <- function(dataset, pal_chose, split = F, type = "variation", 
                             data_name = NULL, tab = "summary", period = "weekly",
                             aver_week = F) {
  
  if (split != FALSE) {
    if (tab == "summary") {
      if (input$measure_select != "outpats") {
        trend_data <- dataset %>% # filtering data by cut and area name
          filter(type == split & area_name == input$geoname)
      } else { #for outpatients data
        trend_data <- dataset %>% # filtering data by cut and area name
          filter(type == split & area_name == input$geoname_op)
      }

    } else if (tab %in% c("cardio", "mh", "injuries")) {

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
          mutate(category = factor(category, levels = c("All", "Under 5", "5 - 14", "Under 65", "15 - 24", "15 - 44", "25 - 44",
                                                        "45 - 64", "<65", "65 - 74", "65+","65 and over", 
                                                        "75 - 84", "85 and over"))) 
        
      } else if (tab == "cancer") {
        trend_data <- trend_data %>% 
          mutate(category = factor(category, levels = c("Under 5", "5-9", "10-14", "15-19",  
                                                        "20-24", "25-29", "30-34", 
                                                        "35-39", "40-44", "45-49", 
                                                        "50-54", "55-59", "60-64", 
                                                        "65-69", "70-74", "75-79","80 and over")))
           } else if (tab == "injuries") {
        trend_data <- trend_data %>% 
            mutate(category = factor(category, levels = c("0-4", "5-11", "12-17",  
                                                    "18-24", "25-44", "45-64", 
                                                    "65-79", "80 and over")))
        
      }
    } else if (split == "condition") {
      if (tab == "cardio") {
        trend_data <- trend_data %>% 
          filter(type %in% split & area_name == input$geoname_cardio,
                 category != "All") %>% 
          # Wrapping long legend names
          mutate(category = case_when(
            category == "Antihypertensive, anti-anginal, anti-arrhythmic and heart failure drugs" ~ "Antihypertensive, \nanti-anginal, anti-arrhythmic \nand heart failure drugs",
            TRUE ~ category
          ))
      } else if (tab == "mh") {
        trend_data <- trend_data %>% 
          filter(type %in% split & area_name == input$geoname_mh,
                 category != "All") %>%
          mutate(category = case_when(
            category == "SSRI SNRI" ~ "Depression medicine",
            category == "Anxiolytic" ~ "Anxiety medicine",
            category == "Hypnotic" ~ "Insomnia medicine",
            TRUE ~ category
          ))

      } else if (tab == "injuries") {
        trend_data <- trend_data %>% 
          filter(type %in% split & area_name == input$geoname_injuries,
                 category != "all")
    } else {
    trend_data <- trend_data 
  }
    }
    
    
    period_data <- case_when(period == "weekly" ~ paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y")),
                             period == "monthly" ~ paste0("Month: ", format(trend_data$week_ending, "%b %y")))  
    
  # If variation selected different values
  if (type == "variation") {
    
    aver_period <- paste0(case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", 
                                                     "sas", "drug_presc", "cath", 
                                                     "mentalhealth_drugs", "mh_ooh",
                                                     "ooh_cardiac", "sas_cardiac", "ui_smr01_all", "ui_smr01_assaults",
                                                     "ui_smr01_falls", "ui_smr01_other", "ui_smr01_poison",
                                                     "ui_smr01_rta","op") ~ "2018-2019",
                             data_name == "deaths" ~ "2015-2019"))
      
    if (aver_week == T) {
      #Text for tooltip
      tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                                "Average of weeks ending on ", 
                                format(trend_data$week_ending - 7, "%d %b %y"), ", ",
                                format(trend_data$week_ending, "%d %b %y"), " and ", 
                                format(trend_data$week_ending + 7, "%d %b %y"),
                                "<br>", "Change from ", aver_period, " average: ", 
                                round(trend_data$variation, 1), "%"))
      
    } else {
      #Text for tooltip
      tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                                period_data, 
                                "<br>", "Change from ", aver_period, " average: ", 
                                round(trend_data$variation, 1), "%"))
      
    }
      
      #Modifying standard layout
      yaxis_plots[["title"]] <- paste0("% change from ", aver_period, " average")
      
      #Creating time trend plot
      trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~variation) 
      
    } else if (type == "total") {
      
      ###############################################.
      # Creating objects that change depending on dataset
      yaxis_title <- case_when(data_name == "adm" ~ "Number of admissions",
                               data_name == "aye" ~ "Number of attendances",
                               substr(data_name, 1, 3) == "ooh" ~ "Number of cases",
                               data_name == "nhs24" ~ "Number of completed contacts",
                               substr(data_name, 1, 3) == "sas" ~ "Number of incidents",
                               data_name == "cath" ~ "Number of cases",
                               data_name == "drug_presc" ~ "Number of items prescribed",
                               data_name == "deaths" ~ "Number of deaths",
                               data_name == "mentalhealth_drugs" ~ "Number of patients",
                               data_name == "mh_ooh" ~ "Number of consultations",
                               data_name == "op" ~ "Number of appointments",
                               substr(data_name, 1, 6) == "ui_smr" ~ "Number of admissions")
      
      #Modifying standard layout
      yaxis_plots[["title"]] <- yaxis_title
      
      measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                                data_name == "aye" ~ "Attendances: ",
                                substr(data_name, 1, 3) == "ooh" ~ "Cases: ",
                                data_name == "nhs24" ~ "Completed contacts: ",
                                substr(data_name, 1, 3) == "sas" ~ "Incidents: ",
                                data_name == "cath" ~ "Cases: ",
                                data_name == "drug_presc" ~ "Items prescribed: ",
                                data_name == "cancer" ~ "Referrals: ",
                                data_name == "deaths" ~ "Deaths: ",
                                data_name == "mentalhealth_drugs" ~ "Patients prescribed medicine: ",
                                data_name == "mh_ooh" ~ "Consultations: ",
                                data_name == "op" ~ "Appointments: ",
                                substr(data_name, 1, 6) == "ui_smr" ~ "Admissions: ")
      
      #Text for tooltip
      if (aver_week == T) {
        #Text for tooltip
        tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                                  "Average of weeks ending on ", 
                                  format(trend_data$week_ending - 7, "%d %b %y"), ", ",
                                  format(trend_data$week_ending, "%d %b %y"), " and ", 
                                  format(trend_data$week_ending + 7, "%d %b %y"),
                                  "<br>", measure_name, trend_data$count,
                                  "<br>", "Historic average: ", 
                                  trend_data$count_average))
        
      } else {
        tooltip_trend <- c(paste0(trend_data$category, "<br>",
                                  period_data, 
                                  "<br>", measure_name, trend_data$count,
                                  "<br>", "Historic average: ", trend_data$count_average))
        
      }
      
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

plot_overall_chart <- function(dataset, data_name, yaxis_title, area = T,
                               var2020 = "count", var_aver = "count_average",
                               xvar = "week_ending", filtering = T, op = F) {
  
  if (filtering == T) {
    # Filtering dataset to include only overall figures
    trend_data <- filter_data(dataset, area = area, op = op)
  } else {
    trend_data <- dataset
  }
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data) && nrow(trend_data) == 0)
  {
    plot_nodata(height = 50)
  } else {
    
    ###############################################.
    # Creating objects that change depending on dataset
    yaxis_title <- case_when(data_name == "adm" ~ "Number of admissions",
                             data_name == "aye" ~ "Number of attendances",
                             data_name == "ooh" ~ "Number of cases",
                             data_name == "nhs24" ~ "Number of completed contacts",
                             data_name == "sas" ~ "Number of incidents",
                             data_name == "cath" ~ "Number of cases",
                             data_name == "drug_presc" ~ "Number of items prescribed",
                             data_name == "ooh_cardiac" ~ "Number of cases",
                             data_name == "sas_cardiac" ~ "Number of incidents",
                             data_name == "deaths" ~ "Number of deaths",
                             data_name == "cancer" ~ "Number of referrals",
                             data_name == "mentalhealth_drugs" ~ "Number of patients",
                             data_name == "mh_ooh" ~ "Number of consultations",
                             data_name == "op" ~ "Number of appointments",
                             data_name == "ui_smr01_all" ~ "Number of admissions",
                             data_name == "ui_smr01_assaults" ~ "Number of admissions",
                             data_name == "ui_smr01_falls" ~ "Number of admissions",
                             data_name == "ui_smr01_other" ~ "Number of admissions",
                             data_name == "ui_smr01_poison" ~ "Number of admissions",
                             data_name == "ui_smr01_rta" ~ "Number of admissions")
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- yaxis_title
    
    hist_legend_previous <- case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", "sas", "drug_presc", 
                                                       "ooh_cardiac", "sas_cardiac",
                                                       "cath", "mentalhealth_drugs", "mh_ooh",
                                                       "op") ~ "Average 2018-2019",
                                      data_name == "deaths" ~ "Average 2015-2019")
    
    hist_legend_covid <- case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", "sas", "drug_presc", 
                                                    "ooh_cardiac", "sas_cardiac",
                                                    "mentalhealth_drugs", "mh_ooh", "deaths") ~ "2020 & 2021",
                                   data_name %in% c("cath", "op")  ~ "2020")
    
    measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                              data_name == "aye" ~ "Attendances: ",
                              data_name == "ooh" ~ "Cases: ",
                              data_name == "nhs24" ~ "Completed contacts: ",
                              data_name == "sas" ~ "Incidents: ",
                              data_name == "cath" ~ "Cases: ",
                              data_name == "drug_presc" ~ "Items prescribed: ",
                              data_name == "ooh_cardiac" ~ "Cases: ",
                              data_name == "sas_cardiac" ~ "Incidents: ",
                              data_name == "deaths" ~ "Deaths: ",
                              data_name == "mentalhealth_drugs" ~ "Patients prescribed medicine: ",
                              data_name == "mh_ooh" ~ "Consultations: ",
                              data_name == "op" ~ "Appointments: ")
    
    #Text for tooltip
    tooltip_trend <- c(paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", measure_name, trend_data$count,
                              "<br>", "Historic average: ", trend_data$count_average))
    
    #Creating time trend plot
    plot_ly(data=trend_data, x=~get(xvar)) %>%
      # 2020/21 line
      add_lines(y = ~get(var2020), line = list(color = pal_overall[1]),
                text=tooltip_trend, hoverinfo="text",
                name = hist_legend_covid) %>%
      # Average of previous years line
      add_lines(y = ~get(var_aver), line = list(color = pal_overall[2], dash = 'dash'),
                text=tooltip_trend, hoverinfo="text",
                name = hist_legend_previous) %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(x = 100, y = 0.5)) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
    
  }
  
}

###############################################.
## Function for overall cancer charts ----
###############################################.

plot_overall_cancer_chart <- function(dataset, var1_chosen, var2_chosen, var3_chosen, data_name) {
  
  # set plot display if no data  
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available, no referrals recorded")
  } else {
    
    
    # Set y axis label
    yaxis_title <- case_when(data_name == "cum" ~ "Cumulative Total of Individuals",
                             data_name == "inc" ~ "Weekly Total of Individuals")
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips  
    
    measure_name <- case_when(data_name == "cum" ~ "Cumulative Total of Individuals: ",
                              data_name == "inc" ~ "Weekly Total of Individuals: ")
    
    denom_period <- case_when(var2_chosen %in% c("count19", "cum_count19")  ~ "2019",
                              var2_chosen %in% c("count_mean_17_19", "cum_count_mean_17_19") ~ "Mean 2017-2019")
    
    
    value1 <- dataset[[var1_chosen]]
    
    value2 <- dataset[[var2_chosen]]
    
    value3 <- dataset[[var3_chosen]]
    
    
    tooltip_1 <- c(paste0("Year:2020", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value1))
    
    tooltip_2 <- c(paste0("Year:", denom_period, "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value2))
    
    tooltip_3 <- c(paste0("Year:2021", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value3))
    
    tooltip_4 <- c(paste0("Year:2020", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    tooltip_5 <- c(paste0("Year:2021", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, paste0(format(round(value3, 2), nsmall = 2), "%")))
    
    # Function for verical line at start of lockdown
    vline <- function(x = 0, color = "lightgrey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    
    
    #Creating time trend plot for cumulative totals and incidence
    plot_ly(data=dataset, x=~week_ending) %>%
      
      # 2021 line
      add_lines(y = ~get(var3_chosen), line = list(color = "blue", opacity = 0.3, width = 3), 
                text=tooltip_3, hoverinfo="text", name = "2021") %>%
      
      # 2020 line
      add_lines(y = ~get(var1_chosen), line = list(color = "green", opacity = 0.6, width = 2),
                text=tooltip_1, hoverinfo="text", name = "2020") %>%
      # 2019 line
      add_lines(y = ~get(var2_chosen), line = list(color = "black", dash = 'dash', opacity = 3, width = 1),
                text=tooltip_2, hoverinfo="text", name = "2019") %>%
      
      add_annotations(x = "2020-04-05",
                      y = max(var1_chosen),
                      text = "1st lockdown",
                      xref = "1",
                      yref = "1",
                      showarrow = FALSE) %>% 
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             shapes = list(vline("2020-03-22")),
             yaxis = yaxis_plots, xaxis = list(title = "Week Ending", tickfont = list(size = 13), tick0 = "2020-01-05", dtick = 60*60*24*7*1000),
             legend = list(x = 100, y = 0.5)) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### DIFF PLOT - NO BREAKDOWN ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_diff_cancer_chart <- function(dataset, diffvar1) {
  
  # set plot display if no data  
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available, no referrals recorded")
  } else {
    
    
    # Set y axis label
    yaxis_title <-  "% Change"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips  
    
    measure_name <- "% Change from 2019 "
    
    denom_period <- case_when(input$baseline == "2019" ~ "2019",
                              input$baseline == "Mean 2017-2019" ~ "Mean 2017-2019")
    
    measure_name <- case_when(diffvar1 == "difference20"  ~ "Percentage(%) Change:",
                              diffvar1 == "difference20_cum"  ~ "Cumulative Percentage(%) Change:") # ,
    # diffvar2 == "difference21"  ~ "Percentage(%) Change:",
    # diffvar2 == "difference21_cum"  ~ "Cumulative Percentage(%) Change:")
    
    value1 <- dataset[[diffvar1]]
    
    # value2 <- dataset[[diffvar2]]
    
    tooltip_1 <- c(paste0("Year: ", denom_period, "<br>", "Quarter: ", dataset$quarter, "<br>", 
                          "Age Group: ", dataset$age_group, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    # tooltip_2 <- c(paste0("Year: ", denom_period, "<br>", "Quarter: ", dataset$quarter, "<br>", 
    #                       "Age Group: ", dataset$age_group, "<br>",
    #                       measure_name, " ", paste0(format(round(value2, 2), nsmall = 2), "%")))
    
    # Function for vertical line at start of lockdown
    vline <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    #Creating time trend plot for difference
    
    
    plot_ly(data=dataset) %>%
      
      add_trace(x=~quarter, 
                y = ~get(diffvar1),
                type = 'scatter', 
                mode = 'line',
                color = 'purple',
                text=tooltip_1, 
                hoverinfo="text") %>%
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             xaxis = xaxis_plots, yaxis = yaxis_plots,
             legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'reversed')) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### DIFF PLOT - AGE BREAKDOWN ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_diff_cancer_chart_age <- function(dataset, diffvar1) {
  
  # set plot display if no data  
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available, no referrals recorded")
  } else {
    
    
    # Set y axis label
    yaxis_title <-  "% Change"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips  
    
    measure_name <- "% Change from 2019 "
    
    denom_period <- case_when(input$baseline == "2019" ~ "2019",
                              input$baseline == "Mean 2017-2019" ~ "Mean 2017-2019")
    
    measure_name <- case_when(diffvar1 == "difference20"  ~ "Percentage(%) Change:",
                              diffvar1 == "difference20_cum"  ~ "Cumulative Percentage(%) Change:") # ,
    
    value1 <- dataset[[diffvar1]]
    
    tooltip_1 <- c(paste0("Year: ", denom_period, "<br>", "Quarter: ", dataset$quarter, "<br>", 
                          "Age Group: ", dataset$age_group, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    
    # Function for verical line at start of lockdown
    vline <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    ####Creating time trend plot for difference
    
    plot_ly(data=dataset) %>%
      
      add_trace(x=~quarter, 
                y = ~get(diffvar1),
                type = 'scatter', 
                mode = 'line',
                color = ~age_group,
                colors = pal_sact,
                text=tooltip_1, 
                hoverinfo="text") %>%
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             xaxis = xaxis_plots, yaxis = yaxis_plots,
             legend = list(orientation = 'h', x = 0, y = 1.1)) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### DIFF PLOT - DEPRIVATION BREAKDOWN ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_diff_cancer_chart_dep <- function(dataset, diffvar1) {
  
  # set plot display if no data  
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available, no referrals recorded")
  } else {
    
    
    # Set y axis label
    yaxis_title <-  "% Change"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips  
    
    measure_name <- "% Change from 2019 "
    
    denom_period <- case_when(input$baseline == "2019" ~ "2019",
                              input$baseline == "Mean 2017-2019" ~ "Mean 2017-2019")
    
    measure_name <- case_when(diffvar1 == "difference20"  ~ "Percentage(%) Change:",
                              diffvar1 == "difference20_cum"  ~ "Cumulative Percentage(%) Change:") # ,
    
    value1 <- dataset[[diffvar1]]
    
    # value2 <- dataset[[diffvar2]]
    
    tooltip_1 <- c(paste0("Year: ", denom_period, "<br>", "Quarter: ", dataset$quarter, "<br>", 
                          "Age Group: ", dataset$age_group, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    
    # Function for verical line at start of lockdown
    vline <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    #Creating time trend plot for difference
    
    plot_ly(data=dataset) %>%
      
      add_trace(x=~quarter, 
                y = ~get(diffvar1),
                type = 'scatter', 
                mode = 'line',
                name = paste0(dataset$depdesc),
                color = ~dep,
                colors = pal_cancer_diff,
                text=tooltip_1, 
                hoverinfo="text") %>%
      
      #Layout
      layout(margin = list(b = 80, t=5),
             xaxis = xaxis_plots, yaxis = yaxis_plots,
             legend = list(orientation = 'h', x = 0, y = 1.1)) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}




###############################################.
## Functions for SACT charts ----
###############################################.

## 1. MONTHLY INCIDENCE 

plot_sact_incidence_chart <- function(sact_dataset) {
  
  # set plot display if no data
  if (is.data.frame(sact_dataset) && nrow(sact_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    
    # Set axis labelS
    yaxis_title <- "Number of Patients"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    xaxis_title <- "Month"
    
    xaxis_plots[["title"]] <- xaxis_title
    
    
    #Text for tooltips  
    
    sact_tooltip_mon <- c(paste0("Month: ", sact_dataset$month,
                                 "<br>", "Number of Patients: ", sact_dataset$count,
                                 "<br>", "Area: ", sact_dataset$area))
    
    # #Creating time trend plot for incidence
    
    plot_ly(data=sact_dataset) %>%
      
      
      add_trace(x=~month, 
                y = ~count,
                type = 'scatter', 
                mode = 'line',
                colors = pal_sact,
                text = sact_tooltip_mon, 
                hoverinfo = "text") %>%  
      
      #Layout
      layout(margin = list(b = 80, t=5),
             xaxis = xaxis_plots, yaxis = yaxis_plots,
             legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'reversed')) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}


## 2. MONTHLY INCIDENCE - AREA

plot_sact_incidence_chart_area <- function(sact_dataset) {
  
  # set plot display if no data
  if (is.data.frame(sact_dataset) && nrow(sact_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {    
    
    # Set axis labelS
    yaxis_title <- "Number of Patients"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    xaxis_title <- "Month"
    
    xaxis_plots[["title"]] <- xaxis_title
    
    
    #Text for tooltips  
    
    sact_tooltip_mon <- c(paste0("Month: ", sact_dataset$month,
                                 "<br>", "Number of Patients: ", sact_dataset$count,
                                 "<br>", "Area: ", sact_dataset$area))
    
    # #Creating time trend plot for incidence
    
    plot_ly(data=sact_dataset) %>%
      
      
      add_trace(x=~month, 
                y = ~count,
                type = 'scatter', 
                mode = 'line',
                color = ~area,
                colors = pal_sact,
                text = sact_tooltip_mon, 
                hoverinfo = "text") %>%  
      
      #Layout
      layout(margin = list(b = 80, t=5),
             xaxis = xaxis_plots, yaxis = yaxis_plots,
             legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'reversed')) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}

## 3. MONTHLY INCIDENCE - TREATMENTS

plot_sact_incidence_chart_treatment <- function(sact_dataset) {
  
  # set plot display if no data
  if (is.data.frame(sact_dataset) && nrow(sact_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    
    # Set axis labelS
    yaxis_title <- "Number of Patients"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    xaxis_title <- "Month"
    
    xaxis_plots[["title"]] <- xaxis_title
    
    
    #Text for tooltips  
    
    sact_tooltip_mon <- c(paste0("Month: ", sact_dataset$month,
                                 "<br>", "Number of Patients: ", sact_dataset$count,
                                 "<br>", "Administration Route: ", sact_dataset$treatment))
    
    # #Creating time trend plot for incidence
    
    plot_ly(data=sact_dataset ) %>%
      
      
      add_trace(x=~month, 
                y = ~count,
                type = 'scatter', 
                mode = 'line',
                color = ~treatment,
                colors = pal_sact,
                text = sact_tooltip_mon, 
                hoverinfo = "text") %>%  
      
      #Layout
      layout(margin = list(b = 80, t=5),
             xaxis = xaxis_plots, yaxis = yaxis_plots,
             legend = list(orientation = 'h', x = 0, y = 1.1)) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}


## 2. WEEKLY INCIDENCE

plot_sact_wk_incidence_chart <- function(sact_wk_dataset) {
  
  # set plot display if no data  
  if (is.data.frame(sact_wk_dataset) && nrow(sact_wk_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    
    # Set y axis label
    yaxis_title <- "Number of Appointments"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    #Text for tooltips  
    
    sact_tooltip_wk_inc <- c(paste0("Week beginning: ", format(sact_wk_dataset$week_beginning, "%d %b"),
                                    "<br>", "Number of Appointments: ", sact_wk_dataset$count,
                                    "<br>", "Area: ", sact_wk_dataset$area))
    
    sact_tooltip_lockdown <- c(paste0("Start of 1st lockdown"))
    
    
    # Function for vertical line at start of lockdown
    
    vline1 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    # Function for verical line at 2nd lockdown
    vline2 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    # #Creating time trend plot for weekly incidence
    
    plot_ly(data=sact_wk_dataset) %>%
      
      
      add_trace(x=~week_beginning, 
                y = ~count, 
                type = 'scatter', 
                mode = 'line',
                colors = pal_sact,
                text=sact_tooltip_wk_inc, 
                hoverinfo="text") %>%
      
      add_annotations(x = "2020-03-23",
                      y = 1,
                      text = "1st lockdown",
                      xanchor = 'left',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      add_annotations(x = "2020-12-26",
                      y = 1,
                      text = "2nd lockdown",
                      xanchor = 'right',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             shapes = list(vline1("2020-03-23"), vline2("2020-12-26")),
             yaxis = yaxis_plots, xaxis = list(title = "Week Beginning", tickfont = list(size = 13), 
                                               tick0 = "2019-12-30", dtick = 60*60*24*7*1000*4),
             # shapes=list(type='line', x0= 13, x1= 13, y0=min(sact_wk_var), y1=max(sact_wk_var), line=list(dash='dot', width=1)),
             legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'reversed')) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}

## 2. WEEKLY INCIDENCE - AREA

plot_sact_wk_incidence_chart_area <- function(sact_wk_dataset) {
  
  # set plot display if no data  
  if (is.data.frame(sact_wk_dataset) && nrow(sact_wk_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    
    # Set y axis label
    yaxis_title <- "Number of Appointments"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    #Text for tooltips  
    
    sact_tooltip_wk_inc <- c(paste0("Week beginning: ", format(sact_wk_dataset$week_beginning, "%d %b"),
                                    "<br>", "Number of Appointments: ", sact_wk_dataset$count,
                                    "<br>", "Area: ", sact_wk_dataset$area))
    
    sact_tooltip_lockdown <- c(paste0("Start of 1st lockdown"))
    
    
    # Function for vertical line at start of lockdown
    
    vline1 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    # Function for verical line at 2nd lockdown
    vline2 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    # #Creating time trend plot for weekly incidence
    
    plot_ly(data=sact_wk_dataset) %>%
      
      
      add_trace(x=~week_beginning, 
                y = ~count, 
                type = 'scatter', 
                mode = 'line',
                color = ~area, 
                colors = pal_sact,
                text=sact_tooltip_wk_inc, 
                hoverinfo="text") %>%
      
      add_annotations(x = "2020-03-23",
                      y = 1,
                      text = "1st lockdown",
                      xanchor = 'left',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      add_annotations(x = "2020-12-26",
                      y = 1,
                      text = "2nd lockdown",
                      xanchor = 'right',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             shapes = list(vline1("2020-03-23"), vline2("2020-12-26")),
             yaxis = yaxis_plots, xaxis = list(title = "Week Beginning", tickfont = list(size = 13), 
                                               tick0 = "2019-12-30", dtick = 60*60*24*7*1000*4),
             # shapes=list(type='line', x0= 13, x1= 13, y0=min(sact_wk_var), y1=max(sact_wk_var), line=list(dash='dot', width=1)),
             legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'reversed')) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}

## 2. WEEKLY INCIDENCE - TREATMENT

plot_sact_wk_incidence_chart_treatment <- function(sact_wk_dataset) {
  
  # set plot display if no data  
  if (is.data.frame(sact_wk_dataset) && nrow(sact_wk_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    
    # Set y axis label
    yaxis_title <- "Number of Appointments"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    #Text for tooltips  
    
    sact_tooltip_wk_inc <- c(paste0("Week beginning: ", format(sact_wk_dataset$week_beginning, "%d %b"),
                                    "<br>", "Number of Appointments: ", sact_wk_dataset$count,
                                    "<br>", "Administration Route: ", sact_wk_dataset$treatment))
    
    sact_tooltip_lockdown <- c(paste0("Start of 1st lockdown"))
    
    
    # Function for vertical line at start of lockdown
    
    vline1 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    # Function for verical line at 2nd lockdown
    vline2 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    # #Creating time trend plot for weekly incidence
    
    plot_ly(data=sact_wk_dataset) %>%
      
      
      add_trace(x=~week_beginning, 
                y = ~count, 
                type = 'scatter', 
                mode = 'line',
                color = ~treatment,
                colors = pal_sact,
                text=sact_tooltip_wk_inc, 
                hoverinfo="text") %>%
      
      add_annotations(x = "2020-03-23",
                      y = 1,
                      text = "1st lockdown",
                      xanchor = 'left',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      add_annotations(x = "2020-12-26",
                      y = 1,
                      text = "2nd lockdown",
                      xanchor = 'right',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             shapes = list(vline1("2020-03-23"), vline2("2020-12-26")),
             yaxis = yaxis_plots, xaxis = list(title = "Week Beginning", tickfont = list(size = 13), 
                                               tick0 = "2019-12-30", dtick = 60*60*24*7*1000*4),
             # shapes=list(type='line', x0= 13, x1= 13, y0=min(sact_wk_var), y1=max(sact_wk_var), line=list(dash='dot', width=1)),
             legend = list(orientation = 'h', x = 0, y = 1.1)) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}


################################################################################

## 3. WEEKLY DIFFERENCE

plot_sact_wk_difference_chart <- function(sact_wk_dataset) {
  
  # set plot display if no data  
  if (is.data.frame(sact_wk_dataset) && nrow(sact_wk_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    
    # Set y axis label
    yaxis_title <- "Percentage Change (%)"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips  
    
    sact_tooltip_wk_dif <- c(paste0("Week beginning: ", format(sact_wk_dataset$week_beginning, "%d %b"),
                                    "<br>", 
                                    "Percentage change (%):", paste0(format(round(sact_wk_dataset$week_on_refweek_perc, 2), nsmall = 2), "%"),
                                    "<br>", 
                                    "Area: ", sact_wk_dataset$area))
    
    # Function for verical line at start of lockdown
    vline1 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    # Function for verical line at 2nd lockdown
    vline2 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    # #Creating time trend plot for difference
    
    plot_ly(data=sact_wk_dataset) %>%
      
      
      add_trace(x=~week_beginning, y = ~week_on_refweek_perc, type = 'scatter', 
                mode = 'line',
                text=sact_tooltip_wk_dif, hoverinfo="text") %>%
      
      add_annotations(x = "2020-03-23",
                      y = 1,
                      text = "1st lockdown",
                      xanchor = 'left',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      add_annotations(x = "2020-12-26",
                      y = 1,
                      text = "2nd lockdown",
                      xanchor = 'right',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             shapes = list(vline1("2020-03-23"), vline2("2020-12-26")),
             yaxis = yaxis_plots, xaxis = list(title = "Week Beginning", tickfont = list(size = 13), tick0 = "2019-12-30", dtick = 60*60*24*7*1000*4),
             legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'reversed')) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}

## 3. WEEKLY DIFFERENCE - AREA

plot_sact_wk_difference_chart_area <- function(sact_wk_dataset) {
  
  # set plot display if no data  
  if (is.data.frame(sact_wk_dataset) && nrow(sact_wk_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    
    # Set y axis label
    yaxis_title <- "Percentage Change (%)"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips  
    
    sact_tooltip_wk_dif <- c(paste0("Week beginning: ", format(sact_wk_dataset$week_beginning, "%d %b"),
                                    "<br>", 
                                    "Percentage change (%):", paste0(format(round(sact_wk_dataset$week_on_refweek_perc, 2), nsmall = 2), "%"),
                                    "<br>", 
                                    "Area: ", sact_wk_dataset$area))
    
    # Function for verical line at start of lockdown
    vline1 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    # Function for verical line at 2nd lockdown
    vline2 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    # #Creating time trend plot for difference
    
    plot_ly(data=sact_wk_dataset) %>%
      
      
      add_trace(x=~week_beginning, y = ~week_on_refweek_perc, type = 'scatter', 
                mode = 'line',
                color = ~area, 
                colors = pal_sact,
                text=sact_tooltip_wk_dif, 
                hoverinfo="text") %>%
      
      add_annotations(x = "2020-03-23",
                      y = 1,
                      text = "1st lockdown",
                      xanchor = 'left',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      add_annotations(x = "2020-12-26",
                      y = 1,
                      text = "2nd lockdown",
                      xanchor = 'right',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             shapes = list(vline1("2020-03-23"), vline2("2020-12-26")),
             yaxis = yaxis_plots, xaxis = list(title = "Week Beginning", tickfont = list(size = 13), tick0 = "2019-12-30", dtick = 60*60*24*7*1000*4),
             legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'reversed')) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}

## 3. WEEKLY DIFFERENCE - TREATMENT

plot_sact_wk_difference_chart_treatment <- function(sact_wk_dataset) {
  
  # set plot display if no data  
  if (is.data.frame(sact_wk_dataset) && nrow(sact_wk_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    
    # Set y axis label
    yaxis_title <- "Percentage Change (%)"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips  
    
    sact_tooltip_wk_dif <- c(paste0("Week beginning: ", format(sact_wk_dataset$week_beginning, "%d %b"),
                                    "<br>", 
                                    "Percentage change (%):", paste0(format(round(sact_wk_dataset$week_on_refweek_perc, 2), nsmall = 2), "%"),
                                    "<br>", 
                                    "Administration Route: ", sact_wk_dataset$treatment))
    
    # Function for verical line at start of lockdown
    vline1 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    # Function for verical line at 2nd lockdown
    vline2 <- function(x = 0, color = "grey") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    # #Creating time trend plot for difference
    
    plot_ly(data=sact_wk_dataset) %>%
      
      
      add_trace(x=~week_beginning, 
                y = ~week_on_refweek_perc, 
                type = 'scatter', 
                mode = 'line',
                color = ~treatment,
                colors = pal_sact,
                text=sact_tooltip_wk_dif, 
                hoverinfo="text") %>%
      
      add_annotations(x = "2020-03-23",
                      y = 1,
                      text = "1st lockdown",
                      xanchor = 'left',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      add_annotations(x = "2020-12-26",
                      y = 1,
                      text = "2nd lockdown",
                      xanchor = 'right',
                      xref = "1",
                      yref = "paper",
                      yanchor = 'top',
                      showarrow = FALSE) %>%
      
      
      #Layout
      layout(margin = list(b = 80, t=5),
             shapes = list(vline1("2020-03-23"), vline2("2020-12-26")),
             yaxis = yaxis_plots, xaxis = list(title = "Week Beginning", tickfont = list(size = 13), tick0 = "2019-12-30", dtick = 60*60*24*7*1000*4),
             legend = list(orientation = 'h', x = 0, y = 1.1)) %>% 
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}
###############################################.
## Function for overall injury charts ----
###############################################.

plot_overall_injury_chart <- function(dataset, var1_chosen, var2_chosen, data_name) {
  
  # set plot display if no data  
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Data not avaiable due to small numbers")
  } else {
    
    
    # Set y axis label
    yaxis_title <- case_when(data_name == "ui_smr01_all" ~ "Number of Admissions",
                             data_name == "ui_smr01_rta" ~ "Number of Admissions",
                             data_name == "ui_smr01_falls" ~ "Number of Admissions",
                             data_name == "ui_smr01_other" ~ "Number of Admissions",
                             data_name == "ui_smr01_poison" ~ "Number of Admissions",
                             data_name == "ui_smr01_assaults" ~ "Number of Admissions")
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips  
    
    measure_name <- case_when(data_name == "ui_smr01_all" ~ "All unintentional injuries",
                              data_name == "ui_smr01_rta" ~ "RTAs",
                              data_name == "ui_smr01_falls" ~ "Falls",
                              data_name == "ui_smr01_other" ~ "Other",
                              data_name == "ui_smr01_poison" ~ "Poisoning",
                              data_name == "ui_smr01_assaults" ~ "Assaults")
    
    value1 <- dataset[[var1_chosen]]
    
    value2 <- dataset[[var2_chosen]]
    
    
    tooltip_1 <- c(paste0("Month: ", format(dataset$week_ending, "%b %y"),
                          "<br>", "Admissions from ",measure_name,": ", value1))
    tooltip_2 <- c(paste0("Month: ", format(dataset$week_ending, "%b %y"),
                          "<br>", "Admissions from ",measure_name,": ", value2))
    
    tooltip_3 <- c(paste0("Month: ", format(dataset$week_ending, "%b %y"),
                          "<br>", "Admissions: ", paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    if(data_name != "dif") { 
      
      #Creating time trend plot for cumulative totals and incidence
      plot_ly(data=dataset, x=~week_ending) %>%
        
        # 2020 line
        add_lines(y = ~get(var1_chosen), line = list(color = pal_overall[1]),text=tooltip_1, hoverinfo="text",
                  name = "2020 & 2021") %>%
        # 2019 line
        add_lines(y = ~get(var2_chosen), line = list(color = pal_overall[2], dash = 'dash'),text=tooltip_2, 
                  hoverinfo="text", name = "Average 2018 & 2019") %>%
        
        #Layout
        layout(margin = list(b = 80, t=5), 
               yaxis = yaxis_plots, xaxis = xaxis_plots,
               legend = list(x = 100, y = 0.5)) %>% 
        
        # leaving only save plot button
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
      
    } else {
      
      #Creating time trend plot for difference
      plot_ly(data=dataset, x=~week_ending) %>%
        
        # 2020 line
        add_lines(y = ~get(var1_chosen), line = list(color = pal_overall[1]),text=tooltip_3, hoverinfo="text",
                  name = "2020") %>%
        
        #Layout
        layout(margin = list(b = 80, t=5), 
               yaxis = yaxis_plots, xaxis = list(title = "Month", tickfont = list(size = 13), tick0 = "2020-01-05", dtick = 60*60*24*7*1000),
               legend = list(x = 100, y = 0.5)) %>% 
        
        # leaving only save plot button
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)}
  }
}

# END OF SACT CHARTS
###############################################.


###############################################.
## # Function that creates specialty charts.   ----
###############################################.
# Potentially could be merge with trend one
plot_spec <- function(type, dataset, marg = 160) {
  trend_data <- dataset
  
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
    layout(margin = list(b = marg, t=5), #to avoid labels getting cut out
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
filter_data <- function(dataset, area = T, op = F) {
  if (area == T & op == T) {
    dataset %>% filter(type == "sex") %>%
      filter(area_name == input$geoname_op &
               category == "All")
  } else if (area == T & op == F) {
    dataset %>% filter(type == "sex") %>%
      filter(area_name == input$geoname &
               category == "All")
  } else { #this works for cath data
    dataset %>% 
      filter(category == "All")
  }
}

#####################################################################################.
##Immunisations-functions ----

######################################################################.
#Function to create bar-plot for Scotland immunisation data by SIMD 
plot_imm_simd <- function(dataset, age_week, dose, 
                          var_plot, base_var = F) {
  
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_immun
  
  imm_simd_data <- dataset %>% filter(exclude == 0,
                                      # filter to selected time periods, but don't re-execute each time input changes
                                      time_period_eligible %in% isolate(input$dates_immun)) 
  
  #count the number of distinct months in the dataset - used later to correctly adjust chart
  month_count <- length(unique(imm_simd_data$time_period_eligible))
  
  dataset_name <- deparse(substitute(dataset)) # character name of the data
  
  elig <- case_when(dataset_name == "six_simd_dose1" ~ "12 weeks",
                    dataset_name == "six_simd_dose2" ~ "16 weeks",
                    dataset_name == "six_simd_dose3" ~ "20 weeks",
                    dataset_name == "mmr_simd_dose1" ~ "13 months",
                    dataset_name == "mmr_simd_dose2" ~ "3y 5 months")
  
  # Create tooltip for scurve
  tooltip_scurve <- c(paste0("Cohort: ", imm_simd_data$time_period_eligible))
  tooltip_2019 <- c(paste0("Cohort: 2019"))
  
  ## String text for legend title label
  age_unit <- case_when(substr(dataset_name,1,3) == "six" ~ paste0(age_week, " weeks:"),
                        dataset_name == "mmr_simd_dose1" ~ paste0("12 months:"),
                        dataset_name == "mmr_simd_dose2" ~ paste0("3y 4months:"))
  
  #Modifying standard yaxis name applies to all curves
  xaxis_plots[["title"]] <- "SIMD quintile"
  xaxis_plots[["tickangle"]] <- 315
  
  if (base_var != F) {
    yaxis_plots[["range"]] <- c(0, 100) # enforcing range from 0 to 100%
    yaxis_plots[["title"]] <- paste0("% uptake by ", elig)
    
  } else {
    yaxis_plots[["range"]] <- c(-10, 30) 
    yaxis_plots[["title"]] <- paste0("Change in % uptake by ", elig)
    
  }
  
  #Creating bar plot
  simd_plot <- plot_ly(data=imm_simd_data, x = ~simdq) %>% 
    add_trace(type = 'bar', y = ~get(var_plot), split = ~time_period_eligible,
              color=~time_period_eligible,
              colors = pal_immun,
              text= tooltip_scurve, hoverinfo="text")
  
  if (base_var != F) {
    simd_plot <- simd_plot %>% 
      add_trace(type = 'bar', y = ~get(base_var)/month_count, 
                name = "2019", marker = list(color = "black"),
                text= tooltip_2019, hoverinfo="text") 
  }
  
  simd_plot %>% #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.8, yanchor="top", #position of legend
                         title=list(text=paste0("Children turning ", age_unit))), 
           showlegend = T) %>% 
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
  
}

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
## HV S-curve----
## Function for drawing S-Curve charts used in health visitor tabs.

plot_scurve_child <- function(dataset, age_week) {
  
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_child
  
  scurve_data <- dataset %>% filter(area_name == input$geoname_child,
                                    # filter to selected time periods, but don't re-execute each time input changes
                                    time_period_eligible %in% isolate(input$dates_child)) 
  # %>%
  # droplevels() # might be needed if sort order in legend is to change
  
  if (is.data.frame(scurve_data) && nrow(scurve_data) == 0)
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
      xaxis_plots[["tickvals"]] <- c(0, seq(371, 518, by = 29.4))
      xaxis_plots[["ticktext"]] <- c(0, seq(12, 17, by = 1))
      # enforcing range from 0 to 100%
      yaxis_plots[["range"]] <- c(0, 100)
      
    } else if (age_week == "27 months") {
      
      #Modifying standard yaxis layout
      yaxis_plots[["title"]] <- "% of children who have received their review"
      xaxis_plots[["title"]] <- "Age of children in months"
      # For custom tick labels
      xaxis_plots[["tickvals"]] <- c(0, seq(791, 945, by = 30.8))
      xaxis_plots[["ticktext"]] <- c(0, seq(26, 31, by = 1))
      # enforcing range from 0 to 100%
      yaxis_plots[["range"]] <- c(0, 100)
      
    } else if (age_week == "4 years") {
      
      #Modifying standard yaxis layout
      yaxis_plots[["title"]] <- "% of children who have received their review"
      xaxis_plots[["title"]] <- "Age of children in months"
      # For custom tick labels
      xaxis_plots[["tickvals"]] <- c(0, seq(1428, 1582, by = 30.8))
      xaxis_plots[["ticktext"]] <- c(0, seq(47, 52, by = 1))
      # enforcing range from 0 to 100%
      yaxis_plots[["range"]] <- c(0, 100)
      
    }     
    
    #Creating time trend plot
    plot_ly(data=scurve_data, x=~interv,  y = ~surv) %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~time_period_eligible, colors = pal_child,
                text= tooltip_scurve, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=12), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(title=list(text=paste0("Children turning ", age_week, " in:")),
                           x = 100, y = 0.8, yanchor="top")) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }}


#####################################################################################.
## Function for generating flextable summary of child health data being displayed in s curve.

child_table <- function(dataset, age_week, age_not_reached) {
  
  table_data <- filter_table_data_child(dataset) 
  
  table_data <- table_data %>%
    filter(substr(time_period_eligible,1,3) != "W/B") #filter child health table to exclude weekly cohorts that should only be downloadable
  
  no_complete_row <- with(table_data, (shade_cells == 1))
  
  if (age_week == "2 weeks") {
    format_col <- c("denominator","coverage_6weeks_num","coverage_18weeks_num","coverage_tot_num")
    
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
    format_col <- c("denominator","coverage_10weeks_num","coverage_22weeks_num","coverage_tot_num")
    
    child_table <- table_data %>%
      select (time_period_eligible, denominator, coverage_10weeks_num, 
              coverage_10weeks_percent, coverage_22weeks_num, coverage_22weeks_percent, 
              coverage_tot_num, coverage_tot_percent) %>%
      flextable() %>%
      set_header_labels(coverage_10weeks_num="Children recorded as receiving their 6-8 week review by 10 weeks of age",
                        coverage_10weeks_percent="Children recorded as receiving their 6-8 week review by 10 weeks of age",
                        coverage_22weeks_num="Children recorded as receiving their 6-8 week review by 22 weeks of age (or younger if children have not reached 22 weeks of age by the date data was extracted for analysis)",
                        coverage_22weeks_percent="Children recorded as receiving their 6-8 week review by 22 weeks of age (or younger if children have not reached 22 weeks of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not 12 weeks
      color(i = no_complete_row, j = c("coverage_22weeks_num", "coverage_22weeks_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row, j = c("coverage_22weeks_num", "coverage_22weeks_percent"))
  }
  else if (age_week == "13 months") {
    format_col <- c("denominator","coverage_14months_num","coverage_17months_num","coverage_tot_num")
    
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
  else if (age_week == "4 years") {
    format_col <- c("denominator","coverage_49months_num","coverage_52months_num","coverage_tot_num")
    
    child_table <- table_data %>%
      select (time_period_eligible, denominator, coverage_49months_num, 
              coverage_49months_percent, coverage_52months_num, coverage_52months_percent, 
              coverage_tot_num, coverage_tot_percent) %>%
      flextable() %>%
      set_header_labels(coverage_49months_num="Children recorded as receiving their 4-5 year review by 49 months of age",
                        coverage_49months_percent="Children recorded as receiving their 4-5 year review by 49 months of age",
                        coverage_52months_num="Children recorded as receiving their 4-5 year review by 52 months of age (or younger if children have not reached 52 months of age by the date data was extracted for analysis)",
                        coverage_52months_percent="Children recorded as receiving their 4-5 year review by 52 months of age (or younger if children have not reached 52 months of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not 17 months
      color(i = no_complete_row, j = c("coverage_52months_num", "coverage_52months_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row, j = c("coverage_52months_num", "coverage_52months_percent"))
  }
  
  child_table %>% 
    set_header_labels(time_period_eligible=paste0("Children turning ", age_week, " in:"),
                      denominator="Total number of children",
                      coverage_tot_num=paste0("Children recorded as receiving their review by the date information was extracted for analysis (", child_extract_date,")"),
                      coverage_tot_percent=paste0("Children recorded as receiving their review by the date information was extracted for analysis (", child_extract_date,")")) %>%
    footnote(i = 1, j = c(2, 5),
             value = as_paragraph(c("Cohort sizes are dependent on time periods whether, annual, monthly (4 or 5 weeks) or weekly",
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
