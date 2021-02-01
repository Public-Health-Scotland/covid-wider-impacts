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
  
  period_data <- case_when(period == "weekly" ~ "Week ending: ",
                           period == "monthly" ~ "Month: ")
  
  if (split != FALSE) {
    if (tab == "summary") {
      trend_data <- dataset %>% # filtering data by cut and area name
        filter(type == split & area_name == input$geoname)
    } else if (tab %in% c("cardio", "mh")) {
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
      }
  } else {
    trend_data <- trend_data 
  }
    
  # If variation selected different values
  if (type == "variation") {
    
    aver_period <- paste0(case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", 
                                                     "sas", "drug_presc", "cath", 
                                                     "mentalhealth_drugs", "mh_ooh",
                                                     "ooh_cardiac", "sas_cardiac") ~ "2018-2019",
                             data_name == "deaths" ~ "2015-2019"))
    
    if (aver_week == T) {
      #Text for tooltip
      tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                                "Average of weeks ending on ", format(trend_data$week_ending - 7, "%d %b %y"), ", ",
                                format(trend_data$week_ending, "%d %b %y"), " and ", format(trend_data$week_ending + 7, "%d %b %y"),
                                "<br>", "Change from ", aver_period, " average: ", round(trend_data$variation, 1), "%"))
      
    } else {
      #Text for tooltip
      tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                                "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                                "<br>", "Change from ", aver_period, " average: ", round(trend_data$variation, 1), "%"))
      
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
                             data_name == "ooh" ~ "Number of cases",
                             data_name == "nhs24" ~ "Number of completed contacts",
                             data_name == "sas" ~ "Number of incidents",
                             data_name == "cath" ~ "Number of cases",
                             data_name == "drug_presc" ~ "Number of items prescribed",
                             data_name == "ooh_cardiac" ~ "Number of cases",
                             data_name == "sas_cardiac" ~ "Number of incidents",
                             data_name == "deaths" ~ "Number of deaths",
                             data_name == "mentalhealth_drugs" ~ "Number of patients",
                             data_name == "mh_ooh" ~ "Number of consultations")
    
    #Modifying standard layout
    yaxis_plots[["title"]] <- yaxis_title
    
    measure_name <- case_when(data_name == "adm" ~ "Admissions: ",
                              data_name == "aye" ~ "Attendances: ",
                              data_name == "ooh" ~ "Cases: ",
                              data_name == "nhs24" ~ "Completed contacts: ",
                              data_name == "sas" ~ "Incidents: ",
                              data_name == "cath" ~ "Cases: ",
                              data_name == "drug_presc" ~ "Items prescribed: ",
                              data_name == "ooh_cardiac" ~ "Cases: ",
                              data_name == "sas_cardiac" ~ "Incidents: ",
                              data_name == "cancer" ~ "Referrals: ",
                              data_name == "deaths" ~ "Deaths: ",
                              data_name == "mentalhealth_drugs" ~ "Patients prescribed medicine: ",
                              data_name == "mh_ooh" ~ "Consultations: ")

    #Text for tooltip
    if (aver_week == T) {
      #Text for tooltip
      tooltip_trend <- c(paste0(trend_data$category, "<br>", 
                                "Average of weeks ending on ", format(trend_data$week_ending - 7, "%d %b %y"), ", ",
                                format(trend_data$week_ending, "%d %b %y"), " and ", format(trend_data$week_ending + 7, "%d %b %y"),
                                "<br>", measure_name, trend_data$count,
                                "<br>", "Historic average: ", trend_data$count_average))
      
    } else {
      tooltip_trend <- c(paste0(trend_data$category, "<br>",
                                "Week ending: ", format(trend_data$week_ending, "%d %b %y"),
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
                               xvar = "week_ending", filtering = T) {
  
  if (filtering == T) {
    # Filtering dataset to include only overall figures
    trend_data <- filter_data(dataset, area = area)
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
                           data_name == "mh_ooh" ~ "Number of consultations")

  #Modifying standard layout
  yaxis_plots[["title"]] <- yaxis_title
  
  hist_legend <- case_when(data_name %in% c("adm", "aye", "ooh", "nhs24", "sas", "drug_presc", 
                                            "ooh_cardiac", "sas_cardiac",
                                            "cath", "mentalhealth_drugs", "mh_ooh") ~ "Average 2018-2019",
                          data_name == "deaths" ~ "Average 2015-2019")
  
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
                            data_name == "mh_ooh" ~ "Consultations: ")
  
  #Text for tooltip
    tooltip_trend <- c(paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y"),
                              "<br>", measure_name, trend_data$count,
                              "<br>", "Historic average: ", trend_data$count_average))

  #Creating time trend plot
  plot_ly(data=trend_data, x=~get(xvar)) %>%
    # 2020 line
    add_lines(y = ~get(var2020), line = list(color = pal_overall[1]),
              text=tooltip_trend, hoverinfo="text",
              name = "2020") %>%
    # Average of previous years line
    add_lines(y = ~get(var_aver), line = list(color = pal_overall[2], dash = 'dash'),
              text=tooltip_trend, hoverinfo="text",
              name = hist_legend) %>%
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

plot_overall_cancer_chart <- function(dataset, var1_chosen, var2_chosen, data_name) {
  
# set plot display if no data  
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available, no referrals recorded")
  } else {
  
  
# Set y axis label
  yaxis_title <- case_when(data_name == "cum" ~ "Cumulative Total of Individuals",
                           data_name == "dif" ~ "% Change from 2019 to 2020 ",
                           data_name == "inc" ~ "Weekly Total of Individuals")
  
  yaxis_plots[["title"]] <- yaxis_title
  

#Text for tooltips  
  
  measure_name <- case_when(data_name == "cum" ~ "Cumulative Total of Individuals: ",
                            data_name == "dif" ~ "% Change from 2019 to 2020: ",
                            data_name == "inc" ~ "Weekly Total of Individuals: ")
 
  value1 <- dataset[[var1_chosen]]
  
  value2 <- dataset[[var2_chosen]]
  
  
  tooltip_1 <- c(paste0("Week ending: ", format(dataset$week_ending, "%d %b"),
                            "<br>", measure_name, value1))
  
  tooltip_2 <- c(paste0("Week ending: ", format(dataset$week_ending, "%d %b"),
                        "<br>", measure_name, value2))
  
  tooltip_3 <- c(paste0("Week ending: ", format(dataset$week_ending, "%d %b"),
                              "<br>", measure_name, paste0(format(round(value1, 2), nsmall = 2), "%")))

if(data_name != "dif") { 
  
  #Creating time trend plot for cumulative totals and incidence
  plot_ly(data=dataset, x=~week_ending) %>%
    
    # 2020 line
    add_lines(y = ~get(var1_chosen), line = list(color = pal_overall[1]),text=tooltip_1, hoverinfo="text",
              name = "2020") %>%
    # 2019 line
    add_lines(y = ~get(var2_chosen), line = list(color = pal_overall[2], dash = 'dash'),text=tooltip_2, 
              hoverinfo="text", name = "2019") %>%
    
    #Layout
    layout(margin = list(b = 80, t=5), 
           yaxis = yaxis_plots, xaxis = list(title = "Week Ending", tickfont = list(size = 13), tick0 = "2020-01-05", dtick = 60*60*24*7*1000),
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
           yaxis = yaxis_plots, xaxis = list(title = "Week Ending", tickfont = list(size = 13), tick0 = "2020-01-05", dtick = 60*60*24*7*1000),
           legend = list(x = 100, y = 0.5)) %>% 
    
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)}
  }
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
##Immunistions-curve ----
## Function for drawing S-Curve charts used in immunisation tabs.

plot_scurve <- function(dataset, age_week, dose) {
 
  scurve_data <- dataset %>% filter(area_name == input$geoname_immun & #filter to correct geography
                                    str_detect(immunisation,dose),
                                    exclude !=1) #filter immunisation scurve data on dose

  if (is.data.frame(scurve_data) && nrow(scurve_data) == 0 && input$geoname_immun == "NHS Grampian"  && dataset == mmr_alldose && dose== "dose 2")
  { plot_nodata(height = 50, text_nodata = "Chart not available, NHS Grampian offer 2nd dose of MMR vaccine at 4 years of age. 
                Data is available from the data download option.")
  } else if (is.data.frame(scurve_data) && nrow(scurve_data) == 0)
  { plot_nodata(height = 50)
  } else {     
     
# Create tooltip for scurve
tooltip_scurve <- c(paste0("Cohort: ", scurve_data$time_period_eligible))

#if( any(c(six,six_dose2,six_dose3) %in% dataset)){ #original logic prior to data file change
#if(dataset == six_alldose){ #throws up error which prevents mmr chart working

#Modifying standard yaxis name applies to all curves
yaxis_plots[["title"]] <- "% of children who have received their vaccine"
yaxis_plots[["range"]] <- c(0, 100)  # forcing range from 0 to 100%
xaxis_plots[["tickmode"]] <- "array"  # For custom tick labels

## chart axis for all 6-in-1 scurves
if( any(c(six_alldose) %in% dataset)){ # this doesn't seem like very efficient logic but it works
  
  xaxis_plots[["title"]] <- "Age of children in weeks"
  xaxis_plots[["tickvals"]] <- c(0, seq(56, 308, by = 28))
  xaxis_plots[["ticktext"]] <- c(0, seq(8, 44, by = 4))
  xaxis_plots[["range"]] <- c((7*(as.numeric(age_week)-4)),((as.numeric(age_week)+16))*7) # To adjust x-axis min and max depending on which dose selected

  age_unit <- paste0(age_week, " weeks:") #string for legend label
}
##chart axis for MMR dose 1 scurve
else if(dataset == mmr_alldose && dose== "dose 1" ){ #set chart parameters for mmr dose 1

  xaxis_plots[["title"]] <- "Age of children in months"
  xaxis_plots[["tickvals"]] <- c(0, seq(343, 459, by = 29), 490) # xaxis days 343 (49 weeks) to 490 (70 weeks)
  xaxis_plots[["ticktext"]] <- c(0, seq(11, 16, by = 1))  # xaxis labels 11 months (49 weeks) to 16 months (70 weeks)
  xaxis_plots[["range"]] <- c((7*49),(7*70))  # To adjust x-axis min and max depending on which dose selected

  age_unit <- paste0("12 months:") #string for legend label
}

##chart axis for MMR dose 2 scurve
else if(dataset == mmr_alldose && dose== "dose 2" ){ #set chart parameters for mmr dose 2

  xaxis_plots[["title"]] <- "Age of children in years and months"
  xaxis_plots[["tickvals"]] <- c(0, seq(1190, 1306, by = 29), 1337) #xaxis 1190 days (170 week) to 1337 days (191 weeks)
  xaxis_plots[["ticktext"]] <- c(0, seq(3.3,3.8 , by = 0.1))  # xaxis labels in years and months (works even though months are not decimals because we only show part of a year?)
  xaxis_plots[["range"]] <- c((7*170),(7*191))  # To adjust x-axis min and max depending on which dose selected
  
  age_unit <- paste0("3y 4months:") #string for legend label
}


#Creating time trend plot
  plot_ly(data=scurve_data, x=~interv,  y = ~surv) %>%
  add_trace(type = 'scatter', mode = 'lines',
            color = ~time_period_eligible, colors = pal_immun,
            text= tooltip_scurve, hoverinfo="text") %>%

    # Adding legend title
    add_annotations( text= paste0("Children turning ", age_unit), xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.8, yanchor="bottom",    # Same y as legend below
                     legendtitle=TRUE, showarrow=FALSE ) %>%

    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.8, yanchor="top")) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
}
}

######################################################################.
#Function to create bar-plot for Scotland immunisation data by SIMD 
plot_imm_simd <- function(dataset, age_week, dose, 
                          var_plot, base_var = F) {
  
  imm_simd_data <- dataset %>% filter(exclude == 0) 
  
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
## Function for generating flextable summary of immunisation data.

immune_table <- function(dataset, dose, age_week) {

  table_data <- filter_table_data_immun(dataset, dose)
    
  table_data <- table_data %>%
    filter(exclude_from_table !=1) #filter immunisation table to exclude weekly cohorts that should only be downloadable
  
  no_complete_row <- with(table_data, (substr(time_period_eligible,1,3) == "W/B"|substr(time_period_eligible,1,3) == c("OCT", "NOV")))
  
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
    age_unit <- "8 weeks" #text inserted into 
    age_max <- "24 weeks" #test inserted into note #3 under summary table
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
    age_unit <- "12 weeks"
    age_max <- "28 weeks" #test inserted into note #3 under summary table
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
    age_unit <- "16 weeks"
    age_max <- "32 weeks" #test inserted into note #3 under summary table
  }else if (age_week == 1) {
    #Apply different column names and formatting according to which dataset selected
    format_col <- c("denominator","uptake_13m_num","uptake_16m_num","uptake_tot_num")
    
    imm_table <- table_data %>%
      select (time_period_eligible, denominator,uptake_13m_num,uptake_13m_percent,uptake_16m_num, 
              uptake_16m_percent,uptake_tot_num,uptake_tot_percent) %>%
      flextable() %>%
      set_header_labels(uptake_13m_num="Children recorded as receiving their vaccine by 13 months of age",
                        uptake_13m_percent="Children recorded as receiving their vaccine by 13 months of age ",
                        uptake_16m_num="Children recorded as receiving their vaccine by 16 months of age (or younger if children have not reached 16 months of age by the date data was extracted for analysis)",
                        uptake_16m_percent="Children recorded as receiving their vaccine by 16 months of age (or younger if children have not reached 16 months of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not  weeks
      color(i = no_complete_row, j = c("uptake_16m_num", "uptake_16m_percent"), color="#0033cc")  %>% 
      italic(i = no_complete_row, j = c("uptake_16m_num", "uptake_16m_percent")) 
    age_unit <- "12 months"
    age_max <- "16 months" #test inserted into note #3 under summary table
  }else if (age_week == 3) {
    #Apply different column names and formatting according to which dataset selected
    format_col <- c("denominator","uptake_3y5m_num","uptake_3y8m_num","uptake_tot_num")
    
    imm_table <- table_data %>%
      select (time_period_eligible, denominator,uptake_3y5m_num,uptake_3y5m_percent,uptake_3y8m_num, 
              uptake_3y8m_percent,uptake_tot_num,uptake_tot_percent) %>%
      flextable() %>%
      set_header_labels(uptake_3y5m_num="Children recorded as receiving their vaccine by 3 years and 5 months of age",
                        uptake_3y5m_percent="Children recorded as receiving their vaccine by 3 years and 5 months of age ",
                        uptake_3y8m_num="Children recorded as receiving their vaccine by 3 years and 8 months of age (or younger if children have not reached 3 years and 8 months of age by the date data was extracted for analysis)",
                        uptake_3y8m_percent="Children recorded as receiving their vaccine by 3 years and 8 months of age (or younger if children have not reached 3 years and 8 months of age by the date data was extracted for analysis)") %>% 
      # Italics and colour if not  weeks
      color(i = no_complete_row, j = c("uptake_3y8m_num", "uptake_3y8m_percent"), color="#0033cc")  %>% 
      italic(i = no_complete_row, j = c("uptake_3y8m_num", "uptake_3y8m_percent")) 
    age_unit <- "3 years and 4 months"
    age_max <- "3 years and 8 months" #test inserted into note #3 under summary tabl
  }
  
 imm_table %>% 
   set_header_labels(time_period_eligible= paste0("Children turning ", age_unit," in:"),
                     denominator="Total number of children",
                     uptake_tot_num=paste0("Children recorded as receiving their vaccine by the date information was extracted for analysis (", immunisation_extract_date ,")"),
                     uptake_tot_percent=paste0("Children recorded as receiving their vaccine by the date information was extracted for analysis (", immunisation_extract_date ,")")) %>% 
   footnote(i = 1, j = c(2,5),
            value = as_paragraph(c(
                                   "Cohort sizes are dependent on time periods, whether annual or monthly (4 or 5 weeks)",
                                   paste0("Blue cells indicate cohorts that have not reached ", age_max," of age"))),
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
## HV S-curve----
## Function for drawing S-Curve charts used in health visitor tabs.

plot_scurve_child <- function(dataset, age_week) {
  
  scurve_data <- dataset %>% filter(area_name == input$geoname_child) 
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
  
  table_data <- table_data %>%
    filter(substr(time_period_eligible,1,3) != "W/B") #filter child health table to exclude weekly cohorts that should only be downloadable
  
  if (age_week == "2 weeks") {
    format_col <- c("denominator","coverage_6weeks_num","coverage_18weeks_num","coverage_tot_num")
    
    no_complete_row <- with(table_data, (time_period_eligible == "SEP 2020"))

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

    no_complete_row <- with(table_data, (time_period_eligible == "SEP 2020"))

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

    no_complete_row <- with(table_data, (time_period_eligible == "SEP 2020"))

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

    no_complete_row <- with(table_data, (time_period_eligible == "SEP 2020"))

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

    no_complete_row <- with(table_data, (time_period_eligible == "SEP 2020"))

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
