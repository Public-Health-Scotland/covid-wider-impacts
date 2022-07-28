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
      if (input$`summary-measure` != "outpats") {
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
  if (split == "eth" & is.data.frame(trend_data) && nrow(trend_data) == 0) 
  {plot_nodata(text_nodata = "Data is only available at Scotland level") }
  
  else if (split != "eth" & is.data.frame(trend_data) && nrow(trend_data) == 0)
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
          mutate(category = factor(category, levels = c("All", "Under 5", "5 - 14", "Under 65", "Under 75", "15 - 24", "15 - 44", "25 - 44",
                                                        "45 - 64", "<65", "65 - 74", "65+","65 and over", 
                                                        "75 - 84", "75 and over", "85 and over"))) 
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
                             period == "monthly" ~ paste0("Month: ", format(trend_data$week_ending, "%b %y")),
                             period == "quarterly" ~ paste0("Quarter: ", format(trend_data$week_ending, "%b %y")))  

  # If variation selected different values
  if (type == "variation") {

    aver_period <- paste0(case_when(data_name %in% c("adm", "aye", "ooh", "nhs24",
                                                     "sas", "drug_presc", "cath",
                                                     "mentalhealth_drugs", "mh_ooh",
                                                     "ooh_cardiac", "sas_cardiac", "ui_smr01_all", "ui_smr01_assaults",
                                                     "ui_smr01_falls", "ui_smr01_other", "ui_smr01_poison",
                                                     "ui_smr01_rta","op","cardio_admissions") ~ "2018-2019",
                             data_name %in% c("deaths","cardio_deaths") ~ "2015-2019"))

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
      yaxis_title <- case_when(data_name == "adm" | substr(data_name, 1, 6) == "ui_smr" ~ "Number of admissions",
                               data_name == "aye" ~ "Number of attendances",
                               substr(data_name, 1, 3) == "ooh" | data_name == "cath" ~ "Number of cases",
                               data_name == "nhs24" ~ "Number of completed contacts",
                               substr(data_name, 1, 3) == "sas" ~ "Number of incidents",
                               data_name == "drug_presc" ~ "Number of items prescribed",
                               data_name == "deaths" ~ "Number of deaths",
                               data_name == "mentalhealth_drugs" ~ "Number of patients",
                               data_name == "op" ~ "Number of appointments")

      #Modifying standard layout
      yaxis_plots[["title"]] <- yaxis_title

      measure_name <- case_when(data_name %in% c("adm", "cardio_admissions") | 
                                  substr(data_name, 1, 6) == "ui_smr" ~ "Admissions: ",
                                data_name == "aye" ~ "Attendances: ",
                                substr(data_name, 1, 3) == "ooh" | data_name == "cath" ~ "Cases: ",
                                data_name == "nhs24" ~ "Completed contacts: ",
                                substr(data_name, 1, 3) == "sas" ~ "Incidents: ",
                                data_name == "drug_presc" ~ "Items prescribed: ",
                                data_name == "cancer" ~ "Referrals: ",
                                data_name %in% c("cardio_deaths", "deaths") ~ "Deaths: ",
                                data_name == "mentalhealth_drugs" ~ "Patients prescribed medicine: ",
                                data_name == "op" ~ "Appointments: ")

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

        
      } else if (split == "eth"){
        tooltip_trend <- c(paste0(trend_data$category, "<br>",
                                  period_data, 
                                  "<br>", measure_name, trend_data$count))

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
                               xvar = "week_ending", filtering = T, op = F, period = "weekly") {

  if (filtering == T) {
    # Filtering dataset to include only overall figures
    trend_data <- filter_data(dataset, area = area, op = op, data_name = data_name)
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
    yaxis_title <- case_when(data_name %in% c("adm", "cardio_admissions") 
                             | substr(data_name, 1, 6) == "ui_smr" ~ "Number of admissions",
                             data_name == "aye" ~ "Number of attendances",
                             substr(data_name, 1, 3) == "ooh" | data_name == "cath"  ~ "Number of cases",
                             data_name == "ooh_cons" ~ "Number of consultations",
                             data_name == "nhs24" ~ "Number of completed contacts",
                             substr(data_name, 1, 3) == "sas" ~ "Number of incidents",
                             data_name == "drug_presc" ~ "Number of items prescribed",
                             data_name %in% c("deaths", "cardio_deaths") ~ "Number of deaths",
                             data_name == "cancer" ~ "Number of referrals",
                             data_name == "mentalhealth_drugs" ~ "Number of patients",
                             data_name == "op" ~ "Number of appointments")

    #Modifying standard layout
    yaxis_plots[["title"]] <- yaxis_title

    hist_legend_previous <- case_when(data_name == "deaths" ~ "Average 2015-2019",
                                      TRUE ~ "Average 2018-2019")

    hist_legend_covid <- case_when(data_name %in% c("cath")  ~ "2020", TRUE ~ "2020 - 2022")

    measure_name <- case_when(data_name %in% c("adm", "cardio_admissions") 
                              | substr(data_name, 1, 6) == "ui_smr" ~ "Admissions: ",
                              data_name == "aye" ~ "Attendances: ",
                              substr(data_name, 1, 3) == "ooh" | data_name == "cath"  ~ "Cases: ",
                              data_name == "ooh_cons" ~ "Consultations: ",
                              data_name == "nhs24" ~ "Completed contacts: ",
                              substr(data_name, 1, 3) == "sas" ~ "Incidents: ",
                              data_name == "cath" ~ "Cases: ",
                              data_name == "drug_presc" ~ "Items prescribed: ",
                              data_name %in% c("deaths", "cardio_deaths") ~ "Deaths: ",
                              data_name == "mentalhealth_drugs" ~ "Patients prescribed medicine: ",
                              data_name == "op" ~ "Appointments: ")
    
    # Input for tooltip based on weekly/monthly
    period_data <- case_when(period == "weekly" ~ paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y")),
                             period == "monthly" ~ paste0("Month: ", format(trend_data$week_ending, "%b %y")),
                             period == "quarterly" ~ paste0("Quarter: ", format(trend_data$week_ending, "%b %y")))
    
    #Text for tooltip
    tooltip_trend <- c(paste0(period_data,
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


    tooltip_1 <- c(paste0("Year: 2020", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value1))

    tooltip_2 <- c(paste0("Year: ", denom_period, "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value2))

    tooltip_3 <- c(paste0("Year: 2021", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value3))

    tooltip_4 <- c(paste0("Year: 2020", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, paste0(format(round(value1, 2), nsmall = 2), "%")))

    tooltip_5 <- c(paste0("Year: 2021", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
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
                text=tooltip_2, hoverinfo="text", name = denom_period) %>%

      add_annotations(x = "2020-04-05",
                      y = max(var1_chosen),
                      text = "1st lockdown",
                      xref = "1",
                      yref = "1",
                      showarrow = FALSE) %>%


      #Layout
      layout(margin = list(b = 80, t=5),
             shapes = list(vline("2020-03-22")),
             yaxis = yaxis_plots, xaxis = list(title = "Week Ending", tickfont = list(size = 13), tick0 = "2020-01-05", dtick = 4*60*60*24*7*1000),
             legend = list(x = 100, y = 0.5)) %>%

      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)

  }
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### CANCER DIFF PLOT - NO BREAKDOWN ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_diff_cancer_chart <- function(dataset, periodvar, diffvar1) {
  
  # set plot display if no data  
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available, no referrals recorded")
  } else {
    
    # Set x axis label
    xaxis_title <-  "Quarter"
    
    xaxis_plots[["title"]] <- xaxis_title
    
    # Set y axis label
    yaxis_title <-  "% Change from 2019 Quarter"

    yaxis_plots[["title"]] <- yaxis_title 
    
    #Text for tooltips  
    measure_name <- "Percentage(%) Change:"

    value1 <- dataset[[diffvar1]]

    tooltip_1 <- c(paste0("Quarter: ", dataset$quarter_no, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))
        
    tooltip_2 <- c(paste0("Quarter: ", dataset$quarter_no, "<br>", 
                          "Age group: ", dataset$age_group, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))

    tooltip_3 <- c(paste0("Quarter: ", dataset$quarter_no, "<br>", 
                          "Deprivation Quintile: ", dataset$dep, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    
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
    
   diff_plot <- plot_ly(data=dataset, x = ~get(periodvar)) 
    
   if (input$breakdown == "None"){ # DIFF PLOT - No breakdown
     
     diff_plot <- diff_plot %>%
       
       add_trace(y = ~get(diffvar1),
                type = 'scatter', 
                mode = 'line',
                color = 'purple',
                text = tooltip_1,
                hoverinfo="text")
     
   } else if (input$breakdown == "Age Group") { # DIFF PLOT - AGE BREAKDOWN
     
     diff_plot <- diff_plot %>%
       
       add_trace(y = ~get(diffvar1),
                 type = 'scatter', 
                 mode = 'line',
                 color = ~age_group,
                 colors = pal_sact,
                 text = tooltip_2, 
                 hoverinfo="text")
     
   } else if (input$breakdown == "Deprivation") { #DIFF PLOT - Deprivation BREAKDOWN
    
     diff_plot <- diff_plot %>% 
       
      add_trace(y = ~get(diffvar1),
                type = 'scatter', 
                mode = 'line',
                color = ~dep,
                colors = pal_cancer_diff,
                text = tooltip_3, 
                hoverinfo="text") 
       
   }
      
      #Layout
       diff_plot %>%  layout(margin = list(b = 80, t=5),
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
## # Function that creates specialty charts.   ----
###############################################.
# Potentially could be merge with trend one
plot_spec <- function(type, dataset, marg = 160, period = "weekly", op = F) {
  trend_data <- dataset

  if (type == "variation") {

    # Input for tooltip based on weekly/monthly
    period_data <- case_when(period == "weekly" ~ paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y")),
                             period == "monthly" ~ paste0("Month: ", format(trend_data$week_ending, "%b %y")))  
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$spec, "<br>",
                              period_data,
                              "<br>", "Change from 2018 - 2019 average: ", trend_data$variation, "%"))

    #Modifying standard layout
    yaxis_plots[["title"]] <- "% change from 2018-19 average"

    #Creating time trend plot
    trend_plot <- plot_ly(data=trend_data, x=~week_ending,  y = ~variation)


  } else if (type == "total") {

    #Modifying standard layout
    if (op == T){
      yaxis_plots[["title"]] <- "Number of appointments"
      
      measure_name <- "Appointments: "
    } else{
    yaxis_plots[["title"]] <- "Number of admissions"

    measure_name <- "Admissions: "
    }
    
    # Input for tooltip based on weekly/monthly
    period_data <- case_when(period == "weekly" ~ paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y")),
                             period == "monthly" ~ paste0("Month: ", format(trend_data$week_ending, "%b %y")))  

    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$spec, "<br>",
                              period_data,
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
filter_data <- function(dataset, data_name = "other", area = T, op = F) {
  if (data_name == "ooh_cons"){
    dataset %>% filter(area_name == input$`summary-geoname` &
                         type == input$ooh_appt_type)
  } else if (area == T & op == T) {
    dataset %>% filter(type == "sex") %>%
      filter(area_name == input$`op-geoname` &
               category == "All")
  } else if (area == T & op == F) {
    dataset %>% filter(type == "sex") %>%
      filter(area_name == input$`summary-geoname` &
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
    yaxis_plots[["range"]] <- c(-30, 30) # some baseline changes are lower than -10
    yaxis_plots[["title"]] <- paste0("Change in % uptake by ", elig)

  }

  #Creating bar plot
  simd_plot <- plot_ly(data=imm_simd_data, x = ~simdq) %>%
    add_trace(type = 'bar', y = ~get(var_plot), split = ~time_period_eligible,
              color=~time_period_eligible,
              colors = pal_immun, textposition="none",
              text= tooltip_scurve, hoverinfo="text")

  if (base_var != F) {
    simd_plot <- simd_plot %>%
      add_trace(type = 'bar', y = ~get(base_var)/month_count,
                name = "2019", marker = list(color = "black"),
                text= tooltip_2019, hoverinfo="text", textposition="none")
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
    add_trace(x = 0, y = 0, visible = FALSE, type = "scatter", mode = "lines") %>%
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
###############################################.
## Function for drug charts ----
###############################################.

lockdown<-function(x,col){
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = col, dash = 'dash')
  )
}
annote<-function(loc, x,y){
  list(x = loc,
       y =max(max(x,na.rm = T),max(y,na.rm=T)) ,
       text = "1st lockdown",
       xref = "1",
       yref = "1",
       showarrow = F)
}



###############################################.
## Function for run charts ----
###############################################.

# Get a description of a run chart
#
# Arguments
# chart - chart name (set to NULL to use "chart"/"charts")
# measure - description of measure
# centreline - description of centreline (required if text_mode == "main")
# text_mode - "main" for full description,
#             "additional" for description of another chart to be shown after
#               main description
# charts_plural - TRUE if describing multiple charts, FALSE if not
#
# Returns character vector with description
run_chart_description =
  function(chart, measure, centreline = NULL,
           text_mode = "main", charts_plural = FALSE) {

  if (isTRUE(charts_plural)) {
    pl_on = "s"
    pl_off = ""
  } else {
    pl_on = ""
    pl_off = "s"
  }

  if (is.null(chart)) {
    chart_text = paste0("chart", pl_on)
  } else {
    chart_text = paste0("‘", chart, "’ chart", pl_on)
  }

  if (text_mode == "main") {
    description_text =
      paste0("To provide a basis for identifying patterns in the data, the ",
              chart_text, " above use", pl_off, " a blue line to show ", centreline,
              ". The blue line is dashed where the average is projected outside
              that time range. A black line shows ", measure, ". The line becomes
              yellow where there are 6 or more consecutive  points above or
              below the average, and is highlighted in green where there are 5
             or more consecutively increasing or decreasing points.")
  } else if (text_mode == "additional") {
    description_text =
      paste0("The ", chart_text, " follow", pl_off, " a similar format, and
             show", pl_off, " the ", measure, ".")
  } else {
    stop("text_mode not recognised")
  }

  return(description_text)


}


# Plots a run chart
#
# Arguments
# plot_data - dataframe containing the columns to plot
# measure - name for column with measure data (character vector)
# measure_name - text to show in legend for measure
# y_label - text to show as y-axis label
# x_dates - name for column with date data (character vector)
#            Column should have a date or factor type.
# shift - name for column with shift data (character vector)
# trend - name for column with trend data (character vector)
# tooltip_text - character vector of text for tooltip at each data point
# xaxis_plots, yaxis_plots - lists of options for x and y axes
# bttn_remove - list of standard buttons to remove from chart
# centreline_data, dottedline_data - vector of data for centrelines
#                                     (same length as plot_data)
# centreline_name, dottedline_name - text to show on legend
# centreline_2_data, dottedline_2_data - for optional extra centrelines
# centreline_2_name, dottedline_2_name - for optional extra centrelines
# width_mode - optional, set to "narrow" when the legend is too wide
# x_buffer - optional, how much space to add at left and right of plot area
#             If x_dates column contains dates, x_buffer is days (default 7)
#             If x_dates column contains factors, x_buffer is a multiple of
#               the x-spacing between data points (default 0.25)
# x_factor_space - optional, how often to have a tick on x-axis when x_dates
#                    column is factor. Defaults to every 6 points.
#
# Returns plotly object
plot_run_chart =
  function(plot_data, measure, measure_name, y_label,
           x_dates, shift, trend, tooltip_text,
           xaxis_plots, yaxis_plots, bttn_remove,
           centreline_data, centreline_name,
           dottedline_data, dottedline_name,
           centreline_2_data = NULL, centreline_2_name = NULL,
           dottedline_2_data = NULL, dottedline_2_name = NULL,
           width_mode = "standard", x_buffer = NULL, x_factor_space = 6) {



  # Prep

  # Approach needs to change depending on the type of x-data supplied
  use_string_xticks = ifelse(is.factor(plot_data[[x_dates]]),
                             TRUE, FALSE)

  # Different units means this has to be set differently depending on mode
  if (is.null(x_buffer)) {
    x_buffer = if (use_string_xticks) 0.25 else 7
  }

  if (use_string_xticks) {

    # We need the column name for the labels later
    x_date_labels = x_dates

    # We need to use numerical values for the x-data to avoid an odd issue
    # when using the factors directly - the x-axis was starting too late when
    # there were no shifts, and the early measure data plottled in the wrong
    # place
    plot_data["x_vals"] = c(1:nrow(plot_data))
    x_dates = "x_vals"

    # We need to just assume the data is in the right order
    range_x = c(1 - x_buffer,
                nrow(plot_data) + x_buffer)

  } else {

    range_x = c(min(plot_data[[x_dates]], na.rm = TRUE) - x_buffer,
                max(plot_data[[x_dates]], na.rm = TRUE) + x_buffer)

  }

  # Need to tweak actions later if we're showing extra centrelines
  if (is.null(centreline_2_data)) {
    plot_centreline_2 = FALSE
  } else {
    plot_centreline_2 = TRUE
  }

  # Text to put in legend for trends and shifts
  trend_text = "Trends: 5+ increasing or decreasing points in a row"
  shift_text = "Shifts: 6+ points above or below average in a row"

  # After changes to layout there's not much difference between modes,
  # but kept for future use
  if (width_mode == "narrow") {
    leg_wrap_char = 52
    leg_pos_y = -0.48
    button_pos_y = -0.35
  } else {
    leg_wrap_char = 60
    leg_pos_y = -0.48
    button_pos_y = -0.35
  }

  # Need to adjust spacing when plotting centreline 2 due to taller legend
  if (plot_centreline_2) {
    button_pos_y = button_pos_y - 0.03
    leg_pos_y = leg_pos_y - 0.03
  }



  # Set up data


  # There can be adjacent shifts and trends that should not be connected.
  # These were marked in the data during prep, now we just need to add
  # a row with measure = NA between the shifts/trends.
  #
  # Arguments
  # dataset - dataframe of shift or trend data
  # measure - string with measure column name
  # split_col_prefix - string prefix for column that identifies split locations
  add_split_gaps = function(dataset, measure, split_col_prefix){
    dataset =
      dataset %>%
      # How many times should each row be included? Splits should be included
      # twice, and FALSE + 1 = 1, TRUE + 1 = 2
      mutate(across(all_of(paste0(split_col_prefix, ".split")),
                    ~.x + 1, .names = "num_rows")) %>%
      # If a row should be included more than once, duplicate it
      uncount(weights = num_rows, .id = "dup_row", .remove = FALSE) %>%
      # We want measure to be NA for the first row in each split
      mutate(across(all_of(measure),
                    ~if_else((num_rows == 2) & (dup_row == 1), NA_real_, .x)))
  }

  # Set up data for trend trace
  # We don't want to use this data to plot anything that is not part of a
  # trend, so just set non-trend data to NA
  trend_data = plot_data
  trend_data[!trend_data[[trend]], measure] = NA
  trend_data = add_split_gaps(trend_data, measure, trend)

  # Set up data for shift trace
  # We don't want to use this data to plot anything that is not part of a
  # shift, so just set non-shift data to NA
  shift_data =
    plot_data %>%
    # So it is the same length if add_split_gaps duplicates any rows
    mutate(shift_tooltip = tooltip_text)
  shift_data[!shift_data[[shift]], measure] = NA
  shift_data = add_split_gaps(shift_data, measure, shift)

  # Set up data for dummy traces, that will be used to set legend order
  # Plot outside axis limits so that the data doesn't show when markers on
  dummy_x_buffer = if (use_string_xticks) 100 else years(5)
  dummy_data_leg = list(x = range_x[1] - dummy_x_buffer,
                        y = min(plot_data[[measure]], na.rm = TRUE))



  # Set options for each trace


  # Solid centreline
  trace_args_centreline =
    list(data = plot_data, y = ~centreline_data,
         name = str_wrap(centreline_name, leg_wrap_char), legendgroup = "centreline",
         type = "scatter", mode = "lines",
         line = list(color = "blue"), hoverinfo = "none")

  # Dotted centreline
  trace_args_dottedline =
    list(data = plot_data, y = ~dottedline_data,
         name = str_wrap(dottedline_name, leg_wrap_char), legendgroup = "dottedline",
         type = "scatter", mode = "lines",
         line = list(color = "blue", dash = "dash"), hoverinfo = "none")

  # Additional solid centreline
  trace_args_centreline_additional =
    list(data = plot_data, y = ~centreline_2_data,
         name = str_wrap(centreline_2_name, leg_wrap_char), legendgroup = "centreline_additional",
         type = "scatter", mode = "lines",
         line = list(color = "#60609f"), hoverinfo = "none")

  # Additional dotted centreline
  trace_args_dottedline_additional =
    list(data = plot_data, y = ~dottedline_2_data,
         name = str_wrap(dottedline_2_name, leg_wrap_char), legendgroup = "dottedline_additional",
         type = "scatter", mode = "lines",
         line = list(color = "#60609f", dash = "dash"), hoverinfo = "none")

  # Measure data
  trace_args_measure =
    list(data = plot_data, y = ~get(measure),
         name = str_wrap(measure_name, leg_wrap_char), legendgroup = "measure",
         type = "scatter", mode = "lines",
         line = list(color = "black"), text = tooltip_text, hoverinfo = "text")

  # We can only set the marker options when they are on, just store for now
  measure_marker_opt = list(color = "black", size = 5)

  # Trends
  trace_args_trends =
    list(data = trend_data, y = ~get(measure),
         name = str_wrap(trend_text, leg_wrap_char), legendgroup = "trends",
         type = "scatter", mode = "lines",
         line = list(color = "lightgreen", width = 12), hoverinfo = "none")

  # Shifts
  trace_args_shifts =
    list(data = shift_data, y = ~get(measure),
         name = str_wrap(shift_text, leg_wrap_char), legendgroup = "shifts",
         type = "scatter", mode = "lines",
         line = list(color = "orange", width = 2), text = ~shift_tooltip,
         hoverinfo = "text", hoverlabel = list(bgcolor = "black"))



  # Add traces to plot


  # Each trace will be added twice, so that the order on the plot and on the
  # legend can be controlled separately (plotly limitation).
  #
  # Setting legendgroup and showlegend in add_trace arguments means only one
  # trace will show on the legend and it will control the real trace when
  # clicked. Using dummy data for the trace to be shown on the legend means it
  # will not show up in the plot area.

  # We're going to add a lot of traces, and each trace will be added twice
  # Helpful to have a function
  #
  # Arguments:
  # fig - plotly object for the plot
  # trace args - named list of arguments to pass to add_trace
  # add_trace - optionally set to FALSE to avoid actually adding the trace
  # dummy_data - pass in named list of dummy data (names x and y) for traces
  #               to be shown on the legend
  add_run_trace = function(fig, trace_args,
                           add_trace = TRUE, dummy_data = NULL){

    if (is.null(dummy_data)) {
      trace_args[["showlegend"]] = FALSE
    } else {
      trace_args[["showlegend"]] = TRUE
      trace_args[["x"]] = dummy_data$x
      trace_args[["y"]] = dummy_data$y
    }

    trace_args[["p"]] = fig

    if (isTRUE(add_trace)) {
      fig = do.call("add_trace", trace_args)
    }

    return(fig)

  }

  # Initialise plot
  run_chart = plot_ly(data = plot_data, x = ~get(x_dates))

  # Add real traces
  # The order here is the order traces will be on the plot, bottom to top
  run_chart =
    run_chart %>%
    add_run_trace(trace_args_trends) %>%
    add_run_trace(trace_args_centreline) %>%
    add_run_trace(trace_args_dottedline) %>%
    add_run_trace(trace_args_centreline_additional,
                  add_trace = plot_centreline_2) %>%
    add_run_trace(trace_args_dottedline_additional,
                  add_trace = plot_centreline_2) %>%
    add_run_trace(trace_args_measure) %>%
    add_run_trace(trace_args_shifts)

  # Add dummy traces
  # The order here is the order traces will be on the legend, top to bottom
  run_chart =
    run_chart %>%
    add_run_trace(trace_args_measure,
                  dummy_data = dummy_data_leg) %>%
    add_run_trace(trace_args_centreline,
                  dummy_data = dummy_data_leg) %>%
    add_run_trace(trace_args_dottedline,
                  dummy_data = dummy_data_leg) %>%
    add_run_trace(trace_args_centreline_additional,
                  add_trace = plot_centreline_2,
                  dummy_data = dummy_data_leg) %>%
    add_run_trace(trace_args_dottedline_additional,
                  add_trace = plot_centreline_2,
                  dummy_data = dummy_data_leg) %>%
    add_run_trace(trace_args_trends,
                  dummy_data = dummy_data_leg) %>%
    add_run_trace(trace_args_shifts,
                  dummy_data = dummy_data_leg)



  # Set up marker button


  # Set how many traces there are, and which of them are the measure trace
  if (isTRUE(plot_centreline_2)) {
    num_traces = 14
    measure_trace_pos = c(6, 8)
  } else {
    num_traces = 10
    measure_trace_pos = c(4, 6)
  }

  # When marker button is off, markers should be off
  marker_button_mode_off = rep(list("lines"), num_traces)
  marker_button_marker_off = rep(list(NULL), num_traces)

  # When marker button is on, markers should be on for the measure trace
  marker_button_mode_on = marker_button_mode_off
  marker_button_marker_on = marker_button_marker_off
  marker_button_mode_on[measure_trace_pos] = "lines+markers"
  marker_button_marker_on[measure_trace_pos] = list(measure_marker_opt)

  # Add the button
  run_chart =
    layout(run_chart,
           updatemenus =
             list(
               list(type = "buttons",
                    yanchor = "top",
                    y = button_pos_y,
                    xanchor = "right",
                    x = 1,
                    bordercolor = "#ccc",
                    borderwidth = 1,
                    active = -1, # no buttons start active
                    buttons = list(
                      list(method = "restyle",
                           # attributes to set when button activated
                           args = list(list(mode = marker_button_mode_on,
                                            marker = marker_button_marker_on)),
                           # attributes to set when button deactivated
                           args2 = list(list(mode = marker_button_mode_off,
                                            marker = marker_button_marker_off)),
                           label = "Show/Hide data points")))))



  # Chart Formatting


  yaxis_plots[["title"]] = y_label

  # Setting a specific range stops chart jumping when turning markers on and off
  if (is.null(yaxis_plots[["range"]])) {
    yaxis_plots[["range"]] = c(0, 1.05*max(plot_data[[measure]]))
  }

  # reduces chart jumping around vertically when turning markers on and off
  # (more noticable in some circumstances).
  # Downside: won't automatically make space for wide tick labels, but plotly
  # automatically uses k etc to shorten numbers
  yaxis_plots[["automargin"]] = FALSE

  # stops chart jumping around vertically when moving range slider
  # (would only happen under some circumstances e.g. when I set range 0 - 16 on y axis)
  xaxis_plots[["automargin"]] = FALSE

  # Different approaches needed to x tick labels depending on axis type
  if (use_string_xticks) {

    xaxis_plots[["tickvals"]] = seq(1, nrow(plot_data), x_factor_space)
    # Slightly convoluted indexing here to get as.character to return string
    # instead of factor keys
    xaxis_plots[["ticktext"]] =
      as.character(plot_data[[x_date_labels]][xaxis_plots[["tickvals"]]])

  } else {

    # Left to its own devices, plotly will swap between one line and two line
    # labels. So sometimes the rangeslider is pushed down and overlaps the
    # legend. Better to make it always 2 lines and add sufficient space.
    #
    # dtickrange sets the distance between the ticks where the specified
    # format applies. M1 and M12 mean 1 month and 12 months.
    xaxis_plots[["tickformatstops"]] =
      list(
        # Put this one first so it takes priority over the other range with M1
        list(dtickrange = list("M1", "M12"),
             value = "%b<br>%Y"),
        list(dtickrange = list(NULL, "M1"),
             value = "%-d %b<br>%Y"),
        # A range of M1 to NULL doesn't seem to work for me, so this is here
        # so I can have a range of M1 to M12. No idea if M12 to NULL will work.
        list(dtickrange = list("M12", NULL),
             value = "%b<br>%Y")
    )

  }

  # Otherwise the angle will sometimes change and text will be cut off
  xaxis_plots[["tickangle"]] = 0

  # Prevents this line at zero when dates column is factor
  xaxis_plots[["zeroline"]] = FALSE

  xaxis_plots[["rangeslider"]] =
    list(type = "date",
         thickness = 0.075,
         # Without this, range will change when adding/removing markers
         range = range_x)

  # Set starting range for x axis
  xaxis_plots[["range"]] = range_x


  # For annotation
  zoom_hover_text =
    "Drag the markers at either end of<br>the bar to view specific time periods"

  # We need an annotation to show user how to use the rangeslider
  zoom_annotation =
    list(text = "Drag to zoom", borderpad = 2,
         hovertext = zoom_hover_text,
         showarrow = TRUE, ax = 0, ay = 18,
         x = 0, xref = "paper", xanchor = "left",
         y = button_pos_y, yref = "paper", yanchor = "middle")

  # Set the options
  run_chart %>%
    #Layout
    layout(margin = list(b = 80, t = 5, r = 25), # to avoid labels getting cut off
           xaxis = xaxis_plots, yaxis = yaxis_plots,
           legend = list(traceorder = "normal", # avoids different spacing of grouped legend
                         yanchor = "top", y = leg_pos_y, x = 0),
           annotations = zoom_annotation) %>%
    #leaving only save plot button
    config(displaylogo = FALSE, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)

  }

###############################################.
## Select geography ----
###############################################.
geotype_value <- function(input, output, session) {
  geotype <- reactive({input$geotype})
  return(geotype)
}

geoname_server <- function(id, lookup = geo_lookup) {

    lookup <- lookup
    output$geoname <- renderUI({
      
      areas_summary <- sort(lookup$areaname[lookup$areatype == input$geotype])
      selectizeInput(NS(id, "geoname"), label = NULL,
                     choices = areas_summary, selected = "")
      
    })
}

### END
