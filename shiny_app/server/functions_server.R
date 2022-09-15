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
                             aver_week = F, fix_x_range = FALSE) {

  if (split != FALSE) {
    if (tab == "summary") {
        trend_data <- dataset %>% # filtering data by cut and area name
          filter(type == split & area_name == input$`summary-geoname`)

    } else if (tab %in% c("cardio", "mh", "injuries")) {

      trend_data <- dataset %>% # filtering data by cut and area name
        filter(type %in% split)
    }
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
          filter(type %in% split & area_name == input$`cardio-geoname`,
                 category != "All") %>%
          # Wrapping long legend names
          mutate(category = case_when(
            category == "Antihypertensive, anti-anginal, anti-arrhythmic and heart failure drugs" ~ "Antihypertensive, \nanti-anginal, anti-arrhythmic \nand heart failure drugs",
            TRUE ~ category
          ))
      } else if (tab == "mh") {
        trend_data <- trend_data %>%
          filter(type %in% split & area_name == input$`mh-geoname`,
                 category != "All") %>%
          mutate(category = case_when(
            category == "SSRI SNRI" ~ "Depression medicine",
            category == "Anxiolytic" ~ "Anxiety medicine",
            category == "Hypnotic" ~ "Insomnia medicine",
            TRUE ~ category
          ))

      } else if (tab == "injuries") {
        trend_data <- trend_data %>%
          filter(type %in% split & area_name == input$`injury-geoname`,
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

    aver_period <- paste0(case_when(data_name %in% c("rapid", "aye", "ooh", "nhs24",
                                                     "sas", "drug_presc", "cath",
                                                     "mhdrugs", "mh_ooh",
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
      yaxis_title <- case_when(data_name == "rapid" | substr(data_name, 1, 6) == "ui_smr" ~ "Number of admissions",
                               data_name == "aye" ~ "Number of attendances",
                               substr(data_name, 1, 3) == "ooh" | data_name == "cath" ~ "Number of cases",
                               data_name == "nhs24" ~ "Number of completed contacts",
                               substr(data_name, 1, 3) == "sas" ~ "Number of incidents",
                               data_name == "drug_presc" ~ "Number of items prescribed",
                               data_name == "deaths" ~ "Number of deaths",
                               data_name == "mhdrugs" ~ "Number of patients",
                               data_name == "op" ~ "Number of appointments")

      #Modifying standard layout
      yaxis_plots[["title"]] <- yaxis_title

      measure_name <- case_when(data_name %in% c("rapid", "cardio_admissions") | 
                                  substr(data_name, 1, 6) == "ui_smr" ~ "Admissions: ",
                                data_name == "aye" ~ "Attendances: ",
                                substr(data_name, 1, 3) == "ooh" | data_name == "cath" ~ "Cases: ",
                                data_name == "nhs24" ~ "Completed contacts: ",
                                substr(data_name, 1, 3) == "sas" ~ "Incidents: ",
                                data_name == "drug_presc" ~ "Items prescribed: ",
                                data_name == "cancer" ~ "Referrals: ",
                                data_name %in% c("cardio_deaths", "deaths") ~ "Deaths: ",
                                data_name == "mhdrugs" ~ "Patients prescribed medicine: ",
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
    
    # Add a range slider to filter the time period in the chart
    xaxis_plots[["rangeslider"]] <- list(range="week_ending", visible = TRUE, thickness = 0.05, bgcolor = "#ECEBF3")
    
    
    # For annotation
    zoom_hover_text =
      "Drag the markers at either end of<br>the bar to view specific time periods"
    
    # We need an annotation to show user how to use the rangeslider
    zoom_annotation =
      list(text = "Drag to zoom", borderpad = 2,
           hovertext = zoom_hover_text,
           showarrow = TRUE, ax = 0, ay = 18,
           x = 0, xref = "paper", xanchor = "left",
           y = -0.26,
           yref = "paper", yanchor = "middle")

    if (isTRUE(fix_x_range)) {
      xaxis_plots[["range"]] = c(min(trend_data$week_ending, na.rm = TRUE),
                                 max(trend_data$week_ending, na.rm = TRUE))
    }

    #Creating time trend plot
    trend_plot %>%
      add_trace(type = 'scatter', mode = 'lines',
                color = ~category, colors = pal_chose,
                text=tooltip_trend, hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(orientation = "h", x=0, y=1.2),
             annotations = zoom_annotation) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
    
    
  }
}

###############################################.
## Function for overall charts ----
###############################################.

plot_overall_chart <- function(dataset, data_name, yaxis_title, area = T,
                               var2020 = "count", var_aver = "count_average",
                               xvar = "week_ending", filtering = T, op = F,
                               period = "weekly", fix_x_range = FALSE) {

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
    yaxis_title <- case_when(data_name %in% c("rapid", "cardio_admissions") 
                             | substr(data_name, 1, 6) == "ui_smr" ~ "Number of admissions",
                             data_name == "aye" ~ "Number of attendances",
                             substr(data_name, 1, 3) == "ooh" | data_name == "cath"  ~ "Number of cases",
                             data_name == "ooh_cons" ~ "Number of consultations",
                             data_name == "nhs24" ~ "Number of completed contacts",
                             substr(data_name, 1, 3) == "sas" ~ "Number of incidents",
                             data_name == "drug_presc" ~ "Number of items prescribed",
                             data_name %in% c("deaths", "cardio_deaths") ~ "Number of deaths",
                             data_name == "cancer" ~ "Number of referrals",
                             data_name == "mhdrugs" ~ "Number of patients",
                             data_name == "op" ~ "Number of appointments")

    #Modifying standard layout
    yaxis_plots[["title"]] <- yaxis_title
    
    # Add a range slider to filter the time period in the chart
    xaxis_plots[["rangeslider"]] <- list(range="xvar", visible = TRUE, thickness = 0.05, bgcolor = "#ECEBF3")
      
    hist_legend_previous <- case_when(data_name %in% c("deaths", "cardio_deaths") ~ "Average 2015-2019",
                                      TRUE ~ "Average 2018-2019")

    hist_legend_covid <- case_when(data_name %in% c("cath")  ~ "2020", TRUE ~ "2020 - 2022")

    measure_name <- case_when(data_name %in% c("rapid", "cardio_admissions") 
                              | substr(data_name, 1, 6) == "ui_smr" ~ "Admissions: ",
                              data_name == "aye" ~ "Attendances: ",
                              substr(data_name, 1, 3) == "ooh" | data_name == "cath"  ~ "Cases: ",
                              data_name == "ooh_cons" ~ "Consultations: ",
                              data_name == "nhs24" ~ "Completed contacts: ",
                              substr(data_name, 1, 3) == "sas" ~ "Incidents: ",
                              data_name == "cath" ~ "Cases: ",
                              data_name == "drug_presc" ~ "Items prescribed: ",
                              data_name %in% c("deaths", "cardio_deaths") ~ "Deaths: ",
                              data_name == "mhdrugs" ~ "Patients prescribed medicine: ",
                              data_name == "op" ~ "Appointments: ")
    
    # Input for tooltip based on weekly/monthly
    period_data <- case_when(period == "weekly" ~ paste0("Week ending: ", format(trend_data$week_ending, "%d %b %y")),
                             period == "monthly" ~ paste0("Month: ", format(trend_data$week_ending, "%b %y")),
                             period == "quarterly" ~ paste0("Quarter: ", format(trend_data$week_ending, "%b %y")))

    #Text for tooltip
    tooltip_trend <- c(paste0(period_data,
                              "<br>", measure_name, trend_data$count,
                              "<br>", "Historic average: ", trend_data$count_average))
    
    # For annotation
    zoom_hover_text =
      "Drag the markers at either end of<br>the bar to view specific time periods"
    
    # We need an annotation to show user how to use the rangeslider
    zoom_annotation =
      list(text = "Drag to zoom", borderpad = 2,
           hovertext = zoom_hover_text,
           showarrow = TRUE, ax = 0, ay = 18,
           x = 0, xref = "paper", xanchor = "left",
           y = -0.26,
           yref = "paper", yanchor = "middle")

    if (isTRUE(fix_x_range)) {
      xaxis_plots[["range"]] = c(min(trend_data[[xvar]], na.rm = TRUE),
                                 max(trend_data[[xvar]], na.rm = TRUE))
    }

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
             yaxis = yaxis_plots, 
             xaxis = xaxis_plots,
             legend = list(orientation = "h", x=0, y=1.2), #position of legend
             annotations = zoom_annotation) %>% 
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  }
}


###############################################.
## # Function that creates specialty charts.   ----
###############################################.
# Potentially could be merge with trend one
plot_spec <- function(type, dataset, marg = NULL, period = "weekly", op = F) {
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

  # Add a range slider to filter the time period in the chart
  xaxis_plots[["rangeslider"]] <- list(range="week_ending", visible = TRUE, thickness = 0.05, bgcolor = "#ECEBF3")
  
  # For annotation
  zoom_hover_text =
    "Drag the markers at either end of<br>the bar to view specific time periods"
  
  # We need an annotation to show user how to use the rangeslider
  zoom_annotation =
    list(text = "Drag to zoom", borderpad = 2,
         hovertext = zoom_hover_text,
         showarrow = TRUE, ax = 0, ay = 18,
         x = 0, xref = "paper", xanchor = "left",
         y = -0.36,
         yref = "paper", yanchor = "middle")

  #Creating time trend plot
  trend_plot %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~spec, colors = pal_spec(), 
              #marker = list(size = 8),
              #symbol = ~spec, 
              #symbols = symbol_spec(),
              text=tooltip_trend, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = marg, t=5), #to avoid labels getting cut out
           showlegend = TRUE, # always show legend
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(orientation = "h", x=0, y=1.2), # position of legend
           annotations = zoom_annotation) %>% 
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
  } else if (area == T) {
    dataset %>% filter(type == "sex") %>%
      filter(area_name == input$`summary-geoname` &
               category == "All")
  } else { #this works for cath data
    dataset %>%
      filter(category == "All")
  }
}


######################################################################.
#Function to create plot when no data available
plot_nodata <- function(height_plot = 450, text_nodata = "Data not available due to small numbers") {
  text_na <- list(x = 5, y = 5, text = text_nodata , size = 20,
                  xref = "x", yref = "y",  showarrow = FALSE,
                  name = "_no_data_plot_marker_") # so can check later if plot object has no data

  plot_ly(height = height_plot) %>%
    add_trace(x = 0, y = 0, visible = FALSE, type = "scatter", mode = "lines") %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')) %>%
    config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
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
    button_pos_y = -0.30
  } else {
    leg_wrap_char = 60
    leg_pos_y = -0.48
    button_pos_y = -0.30
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
         thickness = 0.05,
         bgcolor = "#ECEBF3",
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
geoname_server <- function(id, lookup = geo_lookup) {
  moduleServer(
    id,
    function(input, output, session) {

          output$geoname <- renderUI({
            ns <- session$ns #obtaining namespace
            
            areas_choices <- sort(lookup$areaname[lookup$areatype == input$geotype])
            areas_choices <- areas_choices[areas_choices != "NHS Golden Jubilee"]

          geoname <-  selectizeInput(ns("geoname"), label = NULL,
                         choices = areas_choices, selected = "")

    })
}

  )
}

###############################################.
## Run chart modal ----
###############################################.
runchart_modal <-  function() {
  showModal(modalDialog(
    title = "How do we identify patterns in the data?",
    p("Run charts use a series of rules to help identify important changes in the data.
                 These are the ones we used for these charts:"),
    tags$ul(tags$li("Shifts: Six or more consecutive data points above or below the centreline. Points on the centreline neither break nor contribute to a shift (marked on chart)."),
            tags$li("Trends: Five or more consecutive data points which are increasing or decreasing. An observation that is the same as the preceding value does not count towards a trend (marked on chart)."),
            tags$li("Too many or too few runs: A run is a sequence of one or more consecutive observations on the same side of the centreline. Any observations falling directly on the centreline can be ignored. If there are too many or too few runs (i.e. the median is crossed too many or too few times) that’s a sign of something more than random chance."),
            tags$li("Astronomical data point: A data point which is distinctly different from the rest. Different people looking at the same graph would be expected to recognise the same data point as astronomical (or not).")),
    p("Further information on these methods of presenting data can be found in the ",
      tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
             'PHS guide to statistical process control charts', target="_blank"),"."),
    size = "m",
    easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
}

###############################################.
## SIMD modal ----
###############################################.
simd_modal <- function(group = "People") {
  showModal(modalDialog(
    title = "What is SIMD and deprivation?",
    p(group, " have been allocated to different levels of deprivation based on the small area (data zone)
      in which they live and the", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD) (external website).",
                                          class="externallink"), "score for that area.
      SIMD scores are based on data for 38 indicators covering seven topic areas:
      income, employment, health, education, skills and training, housing, geographic access, and crime."),
    p("By identifying small areas where there are concentrations of multiple deprivation,
    the SIMD can be used to target policies and resources at the places with the greatest need.
    The SIMD identifies deprived areas, not deprived individuals."),
    p("In this tool we have presented results for ", tolower(group), " living in different SIMD ‘quintiles’.
      To produce quintiles, data zones are ranked by their SIMD score then the areas each containing a fifth (20%)
      of the overall population of Scotland are identified. ", group, " living in the most and least deprived areas
      that each contain a fifth of the population are assigned to SIMD quintile 1 and 5 respectively."),
    size = "m",
    easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
}


###############################################.
## Functions to modify charts ----
###############################################.

# Adds a single vertical line with an annotation to an existing chart
#
# Warning: will remove any existing shapes from chart  and only works with a 
# maximum of two lines on the plot
#
# Note: works better when plot's x-axis range is fixed, otherwise the range
# adjusts to fit the annotation in the plot area even if it could extend out
# over legend.
#
# Arguments
# fig - a plotly chart
# x - position on x-axis to draw vertical line at
# x2 - position on x-axis to draw a second vertical line at
# text_align - which side of text should be aligned with the line?
#               "left", "right", or "center"
# text_y - y position of annotation's anchor (as specified by text_yanchor)
#         (0 to 1 = chart area, can be outside that)
# text_yanchor - "auto", "top", "middle", "bottom"
# margin - named list (t, l, r, b), plot margin in number of pixels.
#           May need to adjust to get annotation to fit
# line_settings - named list (color, dash, width)
# hovertext - text to display on hover (optional)
# line_number - number of lines to include
#
# Returns
# plotly chart
add_vline = function(fig, x, x2 = NULL, text, text2, text_align = "center", text_y = 1,
                     text_yanchor = "bottom", margin = list(t = 15),
                     line_settings = list(color = "grey", dash = "dash"),
                     hovertext = NULL, line_number = 1) {

  # This check works locally but not when deployed, not sure why! JV 26/7/22
  # plot_dodata() inserts the string _no_data_plot_marker_ as the name of the
  # annotation. So if that's included in the JSON data plotly generates, we
  # know it's an empty plot saying there's no data
  # no_data_plot =
  #   str_detect(plotly_json(fig)[["x"]][["data"]], "_no_data_plot_marker_")

  # Don't add the annotation to an empty plot
  # if (isFALSE(no_data_plot)) {
  # Dealing with two lines on the plot
  if (line_number == 1) {
  shape_list <- list(type = "line", line = line_settings,
                     y0 = 0, y1 = 1, yref = "paper", x0 = x, x1 = x)
  
  text_list <- list(text = text, align = text_align, showarrow = FALSE,
                    x = x, xanchor = text_align, hovertext = hovertext,
                    y = text_y, yref = "paper", yanchor = text_yanchor)
  
  } else if (line_number == 2) {
    shape_list <- list(
      list(type = "line", line = line_settings, y0 = 0, y1 = 1, yref = "paper", 
           x0 = x, x1 = x),
      list(type = "line", line = line_settings, y0 = 0, y1 = 1, yref = "paper", 
           x0 = x2, x1 = x2))
    
    text_list <- list(
      list(text = text, align = text_align, showarrow = FALSE,
           x = x, xanchor = text_align, hovertext = hovertext,
           y = text_y, yref = "paper", yanchor = text_yanchor),
      list(text = text2, align = text_align, showarrow = FALSE,
           x = x2, xanchor = text_align, hovertext = hovertext,
           y = text_y, yref = "paper", yanchor = text_yanchor))
  }
  
    fig =
      layout(fig,
             shapes = shape_list,
             annotations = text_list,
             margin = margin)
  # }

  return(fig)

}

### END