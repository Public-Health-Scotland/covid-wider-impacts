# Wider impacts dashboard - Child health tab - Immunisations section
# Server code

###############################################.
## Modals  ----
###############################################.
# Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd_imm, simd_modal("Children")) 

# Pop-up modal explaining source of data
observeEvent(input$`immun-source-modal`, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The information shown on the numbers of children eligible for, and receiving, routine 
                 preschool immunisations is taken from the ",
                 tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12",
                        "Scottish Immunisation and Recall System (SIRS) (external website).", target="_blank")),
               p(tags$a(href="https://publichealthscotland.scot/",
                        "Public Health Scotland (PHS)", target="_blank"),
                 " routinely receives quarterly data extracts from SIRS for the purpose of producing and ",
                 tags$a(href="https://publichealthscotland.scot/publications/childhood-immunisation-statistics-scotland/",
                        "publishing", target="_blank"),
                 " immunisation uptake rates. To allow the more rapid monitoring of the impact of 
                 Covid-19 on childhood immunisation uptake rates presented here, PHS is also currently 
                 extracting a sub-set of data from SIRS each month."),
               p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates 
                 the invitation of children when a scheduled immunisation is due. When a child receives an 
                 immunisation, relevant information is returned to administrative staff in the NHS Board 
                 child health department. The administrative staff then update the child’s SIRS record 
                 accordingly."),
               p("After a child becomes eligible for an immunisation, it takes some time for them to 
                 attend their appointment, and for a record of the immunisation provided to subsequently 
                 be entered into the SIRS system. We have allowed a 6-week window for this, therefore 
                 each release of this page will report on children becoming eligible for an immunisation 
                 up to 6 weeks before the date the data were extracted for analysis."),
               p("Although children will generally have their immunisation, and their SIRS record updated 
                 accordingly, within 6 weeks of becoming eligible, the pandemic may have influenced not 
                 only how quickly eligible children receive their immunisations, but also how long it 
                 takes for children’s SIRS records to be updated once an immunisation has been given. 
                 Any disruption to SIRS data entry may vary across NHS Boards. Data provided for the 
                 most recent cohorts of children will therefore not be fully complete in SIRS and should 
                 be viewed as provisional. The uptake rates for each cohort will be refreshed with more 
                 up-to-date data every 4 to 5 weeks, and rates for the most recent cohorts may increase 
                 slightly as relevant records are updated in SIRS."),
               p("Through this tool, data on immunisation uptake are provided for individual NHS Boards 
                 and Health and Social Care Partnerships (HSCPs).  Data by Board are reported by NHS 
                 Board of treatment as recorded on SIRS. Due to the reconfiguration of NHS Board 
                 boundaries, a small proportion of records on SIRS do not reflect the current configuration 
                 of NHS Boards.  In these instances, children have been assigned to an NHS Board of 
                 treatment based on their home postcode. Data by HSCP (available through the data 
                 download button) are reported by HSCP of residence, derived from home postcode recorded 
                 on SIRS. As children may receive their immunisations outwith their Board of residence, 
                 or have missing postcode information recorded on SIRS, this means that there are some 
                 small differences in figures for specific NHS Boards and their corresponding HSCPs."),
               p("Some NHS Boards and HSCPs have small numbers of children eligible for immunisation 
                 each week or month. Uptake rates based on these small numbers are prone to fluctuation, 
                 and it is important to bear this in mind when interpreting uptake rates."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Pop-up modal explaining source of data
observeEvent(input$imm_elig_defs,
             showModal(modalDialog(
               title = "Immunisation eligibility definitions",
               p("Month of eligibility for each immunisation is defined based on complete weeks (Monday to Sunday)",
                 tags$sup("1"), ":"),
               month_elig_imm %>% autofit() %>% htmltools_value(), #showing month eligibility chart
               br(),
               p("6-in-1 immunisation uptake: Eligible age and uptake rates by
                 age stage", tags$sup("2"), " shown in the tables."),
               age_defs_imm_6inone %>% autofit() %>% htmltools_value(),
               br(),
               p("MMR immunisation uptake: Eligible age and uptake rates by age stage", tags$sup("2"),
                 " shown in the tables. Note that ages are defined in weeks but are
                 labelled in years and/or months of age."),
               age_defs_imm_mmr %>% autofit() %>% htmltools_value(),
               p(tags$sup("1 "), "The immunisation indicators included in the tool are updated each month.
                 With each update an additional month will be added to the presentation."),
               p(tags$sup("2 "), "Uptake rates by a specified age refers to children who have
                 received the vaccine before turning the relevant age. For example,
                 uptake of the second dose of MMR vaccine by 3 years 5  months is defined as
                 children receiving the second dose before reaching 178 weeks of age."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
## Functions ----
###############################################.
plot_immun_simd <- function(imm_simd_data){
  
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_immun
  
  dataset_name <- deparse(substitute(imm_simd_data)) # character name of the data
  
  title <- case_when(dataset_name == "six_simd_dose1" ~ "12 weeks",
                     dataset_name == "six_simd_dose2" ~ "16 weeks",
                     dataset_name == "six_simd_dose3" ~ "20 weeks",
                     dataset_name == "mmr_simd_dose1" ~ "13 months",
                     dataset_name == "mmr_simd_dose2" ~ "3y 5 months")
  
  elig <- case_when(dataset_name == "six_simd_dose1" ~ "12 weeks",
                    dataset_name == "six_simd_dose2" ~ "16 weeks",
                    dataset_name == "six_simd_dose3" ~ "20 weeks",
                    dataset_name== "mmr_simd_dose1" ~ "13 months",
                    dataset_name== "mmr_simd_dose2" ~ "3y 5 months")
  
  imm_simd_data %<>% dplyr::rename("percent_uptake" = names(select(imm_simd_data, ends_with("_percent"))))
  
  percent_name <- case_when(dataset_name == "six_simd_dose1" ~ "12weeks",
                            dataset_name == "six_simd_dose2" ~ "16weeks",
                            dataset_name == "six_simd_dose3" ~ "20weeks",
                            dataset_name == "mmr_simd_dose1" ~ "57weeks",
                            dataset_name == "mmr_simd_dose2" ~ "178weeks")
  
  graph_data <- imm_simd_data %>%
    filter(cohort == "monthly") %>%
    mutate(time_period_eligible = paste0("01", time_period_eligible)) %>%
    mutate(time_period_eligible = dmy(time_period_eligible)) %>%
    select(time_period_eligible, simdq, percent_uptake) %>%
    pivot_wider(names_from = simdq,
                values_from = percent_uptake)
  
  #Modifying standard xaxis name applies to all curves
  xaxis_plots[["title"]] <- "Month"
  
  yaxis_plots[["range"]] <- c(0, 100) # enforcing range from 0 to 100%
  yaxis_plots[["title"]] <- paste0("% uptake by ", elig) # elig isn't working
  
  #count the number of distinct months in the dataset - used later to correctly adjust chart
  month_count <- length(unique(graph_data$time_period_eligible))
  
  tooltip_trend <- c(paste0("Month: ", format(graph_data$time_period_eligible, "%B %Y"),"<br>",
                            "1 (most deprived): ", format(as.numeric(graph_data$`1 - most deprived`)), "%", "<br>",
                            "2: ", format(as.numeric(graph_data$`2`)), "%", "<br>",
                            "3: ", format(as.numeric(graph_data$`3`)), "%", "<br>",
                            "4: ", format(as.numeric(graph_data$`4`)), "%", "<br>",
                            "5 (least deprived): ", format(as.numeric(graph_data$`5 - least deprived`)), "%"))
  
  
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
         y = -0.28, yref = "paper", yanchor = "middle")
  
  
  p <- graph_data %>%
    plot_ly( x = ~`time_period_eligible`) %>%
    add_lines(
      y = ~`1 - most deprived`, line = list(color="#045a8d"), name = '1 - most deprived', mode = 'lines',
      text = tooltip_trend, hoverinfo = "text"
    ) %>%
    add_lines(
      y = ~`2`, line = list(color="#2b8cbe"), name = '2', mode = 'lines',
      text = tooltip_trend, hoverinfo = "text"
    ) %>%
    add_lines(
      y = ~`3`, line = list(color="#74a9cf"), name = '3', mode = 'lines',
      text = tooltip_trend, hoverinfo = "text"
    ) %>%
    add_lines(
      y = ~`4`, line = list(color="#a6bddb"), name = '4', mode = 'lines',
      text = tooltip_trend, hoverinfo = "text"
    ) %>%
    add_lines(
      y = ~`5 - least deprived`, line = list(color="#d0d1e6"), name = '5 - least deprived', mode = 'lines',
      text = tooltip_trend, hoverinfo = "text"
    ) %>%
    
    layout(margin = list(b = 80, t = 5),
           yaxis = yaxis_plots,
           xaxis = xaxis_plots,
           legend = list(orientation = "h", x=0, y=1.2),
           annotations = zoom_annotation
    ) %>%
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
  return(p)
  
}


plot_imm_simd_bar <- function(imm_simd_data){
  
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_immun
  
  dataset_name <- deparse(substitute(imm_simd_data)) # character name of the data
  
  # define changing y axis depending on dataset
  elig <- case_when(dataset_name == "six_simd_dose1" ~ "12 weeks",
                    dataset_name == "six_simd_dose2" ~ "16 weeks",
                    dataset_name == "six_simd_dose3" ~ "20 weeks",
                    dataset_name == "mmr_simd_dose1" ~ "13 months",
                    dataset_name == "mmr_simd_dose2" ~ "3years 5 months")
  
  imm_simd_data %<>% dplyr::rename("percent_var" = names(select(imm_simd_data, ends_with("_percent"))))
  imm_simd_data %<>% dplyr::rename("baseline_var" = names(select(imm_simd_data, ends_with("weeks"))))
  
  #create recent year variable
  recent_year <- imm_simd_data %>%
    filter(cohort == "monthly") %>%
    group_by(simdq) %>%
    mutate(percent_var = mean(percent_var)) %>%
    ungroup() %>%
    select(simdq, percent_var) %>%
    mutate(time_period_eligible = "2022*") %>%
    distinct()
  
  #seperate out 2019 from main dataset and combine with recent year variable
  additional_bars <- imm_simd_data %>%
    filter(cohort == "yearly") %>%
    select(simdq, baseline_var) %>%
    mutate(time_period_eligible = "2019") %>%
    distinct() %>%
    rename(percent_var = baseline_var) %>%
    rbind(recent_year)
  
  graph_data <- imm_simd_data %>%
    filter(cohort == "yearly") %>%
    select(time_period_eligible, simdq, percent_var) %>%
    rbind(additional_bars) %>%
    group_by(simdq) %>%
    mutate(time_period_eligible = as.character(time_period_eligible))
  
  #Modifying standard xaxis name applies to all curves
  xaxis_plots[["title"]] <- "SIMD Quintile"
  
  yaxis_plots[["range"]] <- c(0, 100) # enforcing range from 0 to 100%
  yaxis_plots[["title"]] <- paste0("% uptake by ", elig)
  
  #count the number of distinct months in the dataset - used later to correctly adjust chart
  year_count <- length(unique(graph_data$time_period_eligible))
  
  tooltip_bars <- c(paste0("Cohort: ", graph_data$time_period_eligible, "<br>",
                           "Deprivation quintile: ", graph_data$simdq, "<br>",
                           "Percentage uptake: ", round(graph_data$percent_var,1), "%"))
  
  #Creating bar plot
  simd_plot <- plot_ly(data=graph_data, x = ~simdq) %>%
    add_bars(y = ~percent_var,
             color = ~time_period_eligible,
             colors = pal_yr_immun,
             text = tooltip_bars,
             hoverinfo = "text",
             name = ~time_period_eligible) %>%
    
    layout(margin = list(b = 80, t = 5),
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.8, yanchor = "top"), showlegend = T, barmode="group") %>%
    
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
  return(simd_plot)
  
}

plot_imm_simd_change <- function(imm_simd_data){
  
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_immun
  
  dataset_name <- deparse(substitute(imm_simd_data)) # character name of the data
  
  # define changing y axis depending on dataset
  elig <- case_when(dataset_name == "six_simd_dose1" ~ "12 weeks",
                    dataset_name == "six_simd_dose2" ~ "16 weeks",
                    dataset_name == "six_simd_dose3" ~ "20 weeks",
                    dataset_name == "mmr_simd_dose1" ~ "13 months",
                    dataset_name == "mmr_simd_dose2" ~ "3 years 5 months")
  
  # define changing baseline column name depending on dataset
  baseline <- case_when(dataset_name == "six_simd_dose1" ~ "week12",
                        dataset_name == "six_simd_dose2" ~ "week16",
                        dataset_name == "six_simd_dose3" ~ "week20",
                        dataset_name == "mmr_simd_dose1" ~ "week57",
                        dataset_name == "mmr_simd_dose2" ~ "week178")
  
  abs_change <- imm_simd_data %>%
    filter(cohort == "monthly") %>%
    mutate(simdq = as.factor(simdq)) %>%
    mutate(time_period_eligible = paste0("01", time_period_eligible)) %>%
    mutate(time_period_eligible = dmy(time_period_eligible)) %>%
    select(time_period_eligible, simdq, paste0(baseline, "_abs_diff"))
  
  abs_change %<>% dplyr::rename("abs_diff" = names(select(imm_simd_data, ends_with("_abs_diff"))))
  
  #Modifying standard xaxis name applies to all curves
  xaxis_plots[["title"]] <- "Month"
  xaxis_plots[["tickangle"]] <- 315
  # rangeslider doesn't work so well with subplot
  #xaxis_plots[["rangeslider"]] <- list(range="week_ending", visible = TRUE, thickness = 0.05, bgcolor = "#ECEBF3")
  
  yaxis_plots[["range"]] <- c(-30, 30) # enforcing range from -30 to 30%
  yaxis_plots[["title"]] <- paste0("% uptake by ", elig)
  yaxis_plots[["zerolinecolor"]] <- "#C73918"
  yaxis_plots[["zerolinewidth"]] <- 2
  
  abs_change_1 <- abs_change %>%
    filter(simdq == "1 - most deprived")
  
  abs_change_plot_1 <- abs_change_1 %>%
    plot_ly( x = ~`time_period_eligible`, y = ~`abs_diff`, name = "1 - most deprived",
             type = 'scatter', mode = 'lines', line = list(color="black"),
             text = paste0("Cohort: ", format(abs_change_1$time_period_eligible, "%B %Y"), "<br>",
                           "Deprivation quintile: 1 (most deprived) <br>",
                           "Percentage uptake: ", round(abs_change_1$abs_diff,1), "%"),
             hoverinfo = 'text') %>%
    layout(xaxis = xaxis_plots, yaxis = yaxis_plots, 
           annotations = list( x = 0.5, y = 1.0, text = "1-most deprived", xref = "paper",  yref = "paper",  
                               xanchor = "center", yanchor = "bottom", showarrow = FALSE ))
  
  abs_change_2 <- abs_change %>%
    filter(simdq == "2")
  
  abs_change_plot_2 <- abs_change_2 %>%
    plot_ly( x = ~`time_period_eligible`, y = ~`abs_diff`, name = "2",
             type = 'scatter', mode = 'lines' , line = list(color="black"),
             text = paste0("Cohort: ", format(abs_change_2$time_period_eligible, "%B %Y"), "<br>",
                           "Deprivation quintile: 2 <br>",
                           "Percentage uptake: ", round(abs_change_2$abs_diff,1), "%"),
             hoverinfo = 'text') %>%
    layout(xaxis = xaxis_plots, yaxis = yaxis_plots,
           annotations = list( x = 0.5, y = 1.0, text = "Quintile 2", xref = "paper",  yref = "paper",  
                               xanchor = "center", yanchor = "bottom", showarrow = FALSE ))
  
  abs_change_3 <- abs_change %>%
    filter(simdq == "3")
  
  abs_change_plot_3 <- abs_change_3 %>%
    plot_ly( x = ~`time_period_eligible`, y=~`abs_diff`, name = "3",
             type = 'scatter', mode = 'lines' , line = list(color="black"),
             text = paste0("Cohort: ", format(abs_change_3$time_period_eligible, "%B %Y"), "<br>",
                           "Deprivation quintile: 3 <br>",
                           "Percentage uptake: ", round(abs_change_3$abs_diff,1), "%"),
             hoverinfo = 'text')%>%
    layout(xaxis = xaxis_plots, yaxis = yaxis_plots,
           annotations = list( x = 0.5, y = 1.0, text = "Quintile 3", xref = "paper",  yref = "paper",  
                               xanchor = "center", yanchor = "bottom", showarrow = FALSE ))
  
  
  abs_change_4 <- abs_change %>%
    filter(simdq == "4")
  
  abs_change_plot_4 <- abs_change_4 %>%
    plot_ly( x = ~`time_period_eligible`, y = ~`abs_diff`, name = "4",
             type = 'scatter', mode = 'lines' , line = list(color="black"),
             text = paste0("Cohort: ", format(abs_change_4$time_period_eligible, "%B %Y"), "<br>",
                           "Deprivation quintile: 4 <br>",
                           "Percentage uptake: ", round(abs_change_4$abs_diff,1), "%"),
             hoverinfo = 'text')%>%
    layout(xaxis = xaxis_plots, yaxis = yaxis_plots,
           annotations = list( x = 0.5, y = 1.0, text = "Quintile 4", xref = "paper",  yref = "paper",  
                               xanchor = "center", yanchor = "bottom", showarrow = FALSE ))
  
  abs_change_5 <- abs_change %>%
    filter(simdq == "5 - least deprived")
  
  abs_change_plot_5 <- abs_change_5 %>%
    plot_ly( x = ~`time_period_eligible`, y = ~`abs_diff`, name = "5 - least deprived",
             type = 'scatter', mode = 'lines' , line = list(color="black"),
             text = paste0("Cohort: ", format(abs_change_5$time_period_eligible, "%B %Y"), "<br>",
                           "Deprivation quintile: 5 (least deprived) <br>",
                           "Percentage uptake: ", round(abs_change_5$abs_diff,1), "%"),
             hoverinfo = 'text') %>%
    layout(xaxis = xaxis_plots,  yaxis = yaxis_plots,
           annotations = list( x = 0.5, y = 1.0, text = "5-least deprived", xref = "paper",  yref = "paper",  
                               xanchor = "center", yanchor = "bottom", showarrow = FALSE ))
  
  # Number of rows for subplot depending on dimensions
  react_rows <- case_when(between(input$dimension[1], 600, 1000) ~ 2,
                          input$dimension[1] < 600 ~ 5,
                          TRUE ~ 1)
  # Also height of plot depending on number of rows
  react_height <- case_when(react_rows == 5 ~ 1200,
                            react_rows == 2 ~ 700,
                            T ~ 400)
  
  
  abs_change_plot <- subplot(abs_change_plot_1, abs_change_plot_2, abs_change_plot_3, abs_change_plot_4, abs_change_plot_5,
                             shareY = TRUE, shareX = TRUE, nrows = react_rows) %>%
    layout(legend = list(x = 100, y = 0.8, yanchor = "top"), showlegend = F,
           height = react_height) %>%
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
  return(abs_change_plot)
  
}

###############################################...
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("immun")

# Get list of available time periods for plotting
# Assumes that the time periods available are the same for all data
available_time_periods_immun =
  six_alldose %>%
  filter(substr(time_period_eligible,1,3)!="W/B") %>%
  # using pull to get a vector rather than select because the selectizeInput didn't work otherwise
  pull(time_period_eligible) %>%
  unique()

# Set the default time periods for plotting
# Assumes that the months are listed in ascending order in available_time_periods_immun, followed by the years
default_time_periods_immun = tail(available_time_periods_immun, 7)

# Immunisation reactive drop-down control showing list of time periods
output$dates_ui_immun <- renderUI({
  selectizeInput("dates_immun", label = NULL, choices = available_time_periods_immun,
                 selected = default_time_periods_immun, multiple = TRUE,
                 options = list(placeholder = 'Select time periods',
                                plugins = c('remove_button')))
})

# Reactive dataset for flextable filter on geographical area, dose, and time period
filter_table_data_immun <- function(dataset){

  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_immun

  dataset %>% filter(area_name == input$`immun-geoname` & 
                       str_detect(immunisation, #filter immunisation scurve data on dose
                                  substr(input$`immun-measure`, 
                                         nchar(input$`immun-measure`), 
                                         nchar(input$`immun-measure`))),
                       # we don't want this function to re-execute every time dates_immun changes, so isolate()
                       time_period_eligible %in% isolate(input$dates_immun))
}

filter_chart_data_immun <- function(dataset){

  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_immun

  dataset %>% filter(area_name == input$`immun-geoname` &
                       str_detect(immunisation, #filter immunisation scurve data on dose
                                  substr(input$`immun-measure`,
                                         nchar(input$`immun-measure`),
                                         nchar(input$`immun-measure`))),
                     !str_detect(time_period_eligible, "W/B")) %>%
    mutate(colour_flag = case_when(time_period_eligible %in% isolate(input$dates_immun) ~ 1,
                                   TRUE ~ 0))
}


###############################################.
## Reactive data ----
###############################################.
# These two reactive datasets are used for the s-chart
six_alldose_filt <- reactive({
  filter_chart_data_immun(six_alldose)
})

mmr_alldose_filt <- reactive({
  filter_chart_data_immun(mmr_alldose)
})

###############################################.
## Charts ----
###############################################.

# Creating plots for each dataset
#run chart function to generate s curves
output$immun_scurve <- renderPlotly({
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_immun
  if (substr(input$`immun-measure`, 1, 3) == "six") {
    scurve_data <-  six_alldose_filt()
  } else if (substr(input$`immun-measure`, 1, 3) == "mmr") {
    scurve_data <-  mmr_alldose_filt()
  }

  dose <- paste("dose", #extracting dose from input
                substr(input$`immun-measure`, nchar(input$`immun-measure`),
                       nchar(input$`immun-measure`)))
  
    imm_type <- substr(unique(scurve_data$immunisation),1,3)

  # Age week starting for each dose
  age_week <- case_when(imm_type == "six" & dose == "dose 1" ~ "8",
                        imm_type == "six" & dose == "dose 2" ~ "12",
                        imm_type == "six" & dose == "dose 3" ~ "16",
                        imm_type == "mmr" & dose == "dose 1" ~ "1",
                        imm_type == "mmr" & dose == "dose 2" ~ "3")

  if (is.data.frame(scurve_data) && nrow(scurve_data) == 0 && input$`immun-geoname` == "NHS Grampian" && dose== "dose 2")
  { plot_nodata(height = 50, text_nodata = "Chart not available, NHS Grampian offer 2nd dose of MMR vaccine at 4 years of age. 
                Data is available from the data download option.")
  } else if (is.data.frame(scurve_data) && nrow(scurve_data) == 0)
  { plot_nodata(height = 50)
  } else {

    #Modifying standard yaxis name applies to all curves
    yaxis_plots[["title"]] <- "% of children who have received their vaccine"
    yaxis_plots[["range"]] <- c(0, 100)  # forcing range from 0 to 100%
    xaxis_plots[["tickmode"]] <- "array"  # For custom tick labels

    ## chart axis for all 6-in-1 scurves
    if(imm_type == "six"){ # this doesn't seem like very efficient logic but it works

      xaxis_plots[["title"]] <- "Age of children in weeks"
      xaxis_plots[["tickvals"]] <- c(0, seq(56, 308, by = 28))
      xaxis_plots[["ticktext"]] <- c(0, seq(8, 44, by = 4))
      xaxis_plots[["range"]] <- c((7*(as.numeric(age_week)-4)),((as.numeric(age_week)+16))*7) # To adjust x-axis min and max depending on which dose selected

      age_unit <- paste0(age_week, " weeks") #string for legend label
    }
    ##chart axis for MMR dose 1 scurve
    else if(imm_type == "mmr" && dose== "dose 1" ){ #set chart parameters for mmr dose 1

      xaxis_plots[["title"]] <- "Age of children in months"
      xaxis_plots[["tickvals"]] <- c(0, seq(343, 459, by = 29), 490) # xaxis days 343 (49 weeks) to 490 (70 weeks)
      xaxis_plots[["ticktext"]] <- c(0, seq(11, 16, by = 1))  # xaxis labels 11 months (49 weeks) to 16 months (70 weeks)
      xaxis_plots[["range"]] <- c((7*49),(7*70))  # To adjust x-axis min and max depending on which dose selected

      age_unit <- paste0("12 months") #string for legend label
    }

    ##chart axis for MMR dose 2 scurve
    else if(imm_type == "mmr" && dose== "dose 2" ){ #set chart parameters for mmr dose 2

      xaxis_plots[["title"]] <- "Age of children in years and months"
      xaxis_plots[["tickvals"]] <- c(0, seq(1190, 1306, by = 29), 1337) #xaxis 1190 days (170 week) to 1337 days (191 weeks)
      xaxis_plots[["ticktext"]] <- c(0, seq(3.3,3.8 , by = 0.1))  # xaxis labels in years and months (works even though months are not decimals because we only show part of a year?)
      xaxis_plots[["range"]] <- c((7*170),(7*191))  # To adjust x-axis min and max depending on which dose selected

      age_unit <- paste0("3y 4months") #string for legend label
    }

    #filter dataset to just time periods to be shown in grey
    grey_lines <- scurve_data %>%
      filter(colour_flag == 0)

    #filter dataset to just time periods to be shown in colour
    coloured_lines <- scurve_data %>%
      filter(colour_flag == 1)
    
    # First define the palette of colours used, then set a named vector, so each color
    # gets assigned to an time period. But it has to be done for grays and coloured lines 
    trend_length <- length(unique(coloured_lines$time_period_eligible))
    grey_length <-  length(unique(grey_lines$time_period_eligible))

    trend_scale <- c(setNames(pal_immun, unique(coloured_lines$time_period_eligible)[1:trend_length]))
    trend_scale <- trend_scale[1:trend_length]
    grey_scale <- c(setNames(rep("lightgrey", grey_length), 
                             unique(grey_lines$time_period_eligible)[1:grey_length]))
    all_scale <- c(grey_scale, trend_scale)

    # Create tooltip for scurves
    tooltip_grey <- c(paste0("Cohort: ", grey_lines$time_period_eligible))
    tooltip_col <- c(paste0("Cohort: ", coloured_lines$time_period_eligible))

    #Creating time trend plot
    plot_ly() %>%
      # commented out because add_trace() breaks colour palette
      add_lines(data = grey_lines, name = "Other time periods", showlegend = FALSE,
                x=~interv,  y = ~surv, 
                colors = all_scale, color = ~time_period_eligible,
                text= tooltip_grey, hoverinfo="text") %>%
      add_lines(data = coloured_lines,
                x=~interv,  y = ~surv,
                color = ~time_period_eligible, colors = all_scale, 
                text= tooltip_col,hoverinfo="text") %>%
      #Layout
      layout(margin = list(b = 80, t=12), #to avoid labels getting cut out
             yaxis = yaxis_plots, xaxis = xaxis_plots,
             legend = list(title=list(text=paste0("Children turning ", age_unit, " in:")),
                           x = 100, y = 0.8, yanchor="top")) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  }
})

#run function to generate data tables linked to s-curves
output$immun_table <- renderUI({
  if (substr(input$`immun-measure`, 1, 3) == "six") {
    dataset <-  sixtable
  } else if (substr(input$`immun-measure`, 1, 3) == "mmr") {
    dataset <-  mmrtable
  }

  dose <- paste("dose", #extracting dose from input
                substr(input$`immun-measure`, nchar(input$`immun-measure`),
                       nchar(input$`immun-measure`)))
  
  table_data <- filter_table_data_immun(dataset)

  imm_type <- substr(unique(table_data$immunisation),1,3)

  # Age week starting for each dose
  age_week <- case_when(imm_type == "six" & dose == "dose 1" ~ 8,
                        imm_type == "six" & dose == "dose 2" ~ 12,
                        imm_type == "six" & dose == "dose 3" ~ 16,
                        imm_type == "mmr" & dose == "dose 1" ~ 1,
                        imm_type == "mmr" & dose == "dose 2" ~ 3)

  #add data completeness depending on whether six in one or mmr is being looked at (sometimes will cover different time periods)
  no_complete_row_six1 <- with(table_data, time_period_eligible %in% c("MAY 2022", "JUN 2022"))
  no_complete_row_mmr <- with(table_data, time_period_eligible %in% c("MAY 2022", "JUN 2022"))
  
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
      color(i = no_complete_row_six1, j = c("uptake_24weeks_num", "uptake_24weeks_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row_six1, j = c("uptake_24weeks_num", "uptake_24weeks_percent"))
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
      color(i = no_complete_row_six1, j = c("uptake_28weeks_num", "uptake_28weeks_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row_six1, j = c("uptake_28weeks_num", "uptake_28weeks_percent"))
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
      color(i = no_complete_row_six1, j = c("uptake_32weeks_num", "uptake_32weeks_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row_six1, j = c("uptake_32weeks_num", "uptake_32weeks_percent"))
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
      color(i = no_complete_row_mmr, j = c("uptake_16m_num", "uptake_16m_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row_mmr, j = c("uptake_16m_num", "uptake_16m_percent"))
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
      color(i = no_complete_row_mmr, j = c("uptake_3y8m_num", "uptake_3y8m_percent"), color="#0033cc")  %>%
      italic(i = no_complete_row_mmr, j = c("uptake_3y8m_num", "uptake_3y8m_percent"))
    age_unit <- "3 years and 4 months"
    age_max <- "3 years and 8 months" #test inserted into note #3 under summary tabl
  }

  imm_table %>%
    set_header_labels(time_period_eligible= paste0("Children turning ", age_unit," in:"),
                      denominator="Total number of children",
                      uptake_tot_num=paste0("Children recorded as receiving their vaccine by the 
                                            date information was extracted for analysis 
                                            (", immunisation_extract_date ,")"),
                      uptake_tot_percent=paste0("Children recorded as receiving their vaccine by 
                                                the date information was extracted for analysis 
                                                (", immunisation_extract_date ,")")) %>%
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
})

# # Specify items to display in immunisation ui based on step 2 selection
plot_choice <- reactive({switch(input$`immun-measure`,
                                "sixin_dose1" = c("plot_six1_simd", "plot_six1_simd_bar", "plot_six1_simd_change"),
                                "sixin_dose2" = c("plot_six2_simd", "plot_six2_simd_bar", "plot_six2_simd_change"),
                                "sixin_dose3" = c("plot_six3_simd", "plot_six3_simd_bar", "plot_six3_simd_change"),
                                "mmr_dose1" = c("plot_mmr1_simd", "plot_mmr1_simd_bar", "plot_mmr1_simd_change"),
                                "mmr_dose2" = c("plot_mmr2_simd", "plot_mmr2_simd_bar", "plot_mmr2_simd_change")
                                ) # switch bracket
  })

noplot_choice <- reactive({switch(input$`immun-geotype`,
                                  "Health board" = "empty_plot")})

## Deprivation graphs outputs

# outputs - monthly line graphs
output$plot_six1_simd <- renderPlotly({ plot_immun_simd(six_simd_dose1) })
output$plot_six2_simd <- renderPlotly({ plot_immun_simd(six_simd_dose2) })
output$plot_six3_simd <- renderPlotly({ plot_immun_simd(six_simd_dose3) })
output$plot_mmr1_simd <- renderPlotly({ plot_immun_simd(mmr_simd_dose1) })
output$plot_mmr2_simd <- renderPlotly({ plot_immun_simd(mmr_simd_dose2) })

# # outputs - yearly bar charts
output$plot_six1_simd_bar <- renderPlotly({ plot_imm_simd_bar(six_simd_dose1) })
output$plot_six2_simd_bar <- renderPlotly({ plot_imm_simd_bar(six_simd_dose2) })
output$plot_six3_simd_bar <- renderPlotly({ plot_imm_simd_bar(six_simd_dose3) })
output$plot_mmr1_simd_bar <- renderPlotly({ plot_imm_simd_bar(mmr_simd_dose1) })
output$plot_mmr2_simd_bar <- renderPlotly({ plot_imm_simd_bar(mmr_simd_dose2) })

# outputs - absolute change graphs
output$plot_six1_simd_change <- renderPlotly({ plot_imm_simd_change(six_simd_dose1)})
output$plot_six2_simd_change <- renderPlotly({ plot_imm_simd_change(six_simd_dose2)})
output$plot_six3_simd_change <- renderPlotly({ plot_imm_simd_change(six_simd_dose3)})
output$plot_mmr1_simd_change <- renderPlotly({ plot_imm_simd_change(mmr_simd_dose1)})
output$plot_mmr2_simd_change <- renderPlotly({ plot_imm_simd_change(mmr_simd_dose2)})

# no graph outputted
output$empty_plot <- renderPlotly({ plot_nodata() })

###############################################.
## Layout ----
###############################################.

output$immunisation_explorer <- renderUI({

  imm_trends_title <- case_when(input$`immun-measure` == "sixin_dose1" ~ "Uptake of first dose of 6-in-1 vaccine by 12 weeks of age by deprivation: Scotland",
                                input$`immun-measure` == "sixin_dose2" ~ "Uptake of second dose of 6-in-1 vaccine by 16 weeks of age by deprivation: Scotland",
                                input$`immun-measure` == "sixin_dose3" ~ "Uptake of third dose of 6-in-1 vaccine by 20 weeks of age by deprivation: Scotland",
                                input$`immun-measure` == "mmr_dose1" ~ "Uptake of first dose of MMR vaccine by 13 months of age by deprivation: Scotland",
                                input$`immun-measure` == "mmr_dose2" ~ "Uptake of second dose of MMR vaccine by 3 years 5 months of age by deprivation: Scotland")

  imm_change_title <- case_when(input$`immun-measure` == "sixin_dose1" ~ "Change in uptake of first dose of 6-in-1 vaccine by 12 weeks of age by deprivation: Scotland (compared to baseline of children turning 8 weeks in 2019)",
                                input$`immun-measure` == "sixin_dose2" ~ "Change in uptake of second dose of 6-in-1 vaccine by 16 weeks of age by deprivation: Scotland (compared to baseline of children turning 12 weeks in 2019)",
                                input$`immun-measure` == "sixin_dose3" ~ "Change in uptake of third dose of 6-in-1 vaccine by 20 weeks of age by deprivation: Scotland (compared to baseline of children turning 16 weeks in 2019)",
                                input$`immun-measure` == "mmr_dose1" ~ "Change in uptake of first dose of MMR vaccine by 13 months of age by deprivation: Scotland (compared to baseline of children turning 12-13 months in 2019)",
                                input$`immun-measure` == "mmr_dose2" ~ "Change in uptake of second dose of MMR vaccine by 3 years 5 months of age by deprivation: Scotland (compared to baseline of children turning 3 years 4 months in 2019)")


  if(input$`immun-geotype` == "Scotland"){

  immune_title <- case_when(input$`immun-measure` == "sixin_dose1" ~ paste0("Uptake of first dose of 6-in-1 vaccine (offered to children at 8 weeks of age): ",
                                                                                 input$`immun-geoname`),
                            input$`immun-measure` == "sixin_dose2" ~ paste0("Uptake of second dose 6-in-1 vaccine (offered to children at 12 weeks of age): ", input$`immun-geoname`),
                            input$`immun-measure` == "sixin_dose3" ~ paste0("Uptake of third dose 6-in-1 vaccine (offered to children at 16 weeks of age): ", input$`immun-geoname`),
                            input$`immun-measure` == "mmr_dose1" ~ paste0("Uptake of first dose MMR vaccine (offered to children at 12-13 months of age): ", input$`immun-geoname`),
                            input$`immun-measure` == "mmr_dose2" ~ paste0("Uptake of second dose MMR vaccine (offered to children at 3 years 4 months of age): ", input$`immun-geoname`))

  immune_subtitle <-  paste0("Figures based on data extracted from SIRS on ",immunisation_extract_date)

  tagList(br(),
          fluidRow(column(12,
                          p("Immunisation protects children against certain serious infections. Public Health Scotland and Scottish Government have produced a range of communications reminding parents that the NHS is still open for childhood immunisations, signposting parents to up to date advice via ",
                                  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
                                         "NHS inform (external website)", target="_blank"),"."),
                          h4(paste0(immune_title)),
                          p(immune_subtitle))),
          fluidRow(column(6,
                          p("Grey lines represent previous month's data not selected in step 3 filter."),
                          withSpinner(plotlyOutput("immun_scurve"))),
          column(6, uiOutput("immun_table"))),

          tagList(p("All preschool children are offered a total of five immunisation appointments 
                    as they reach the following ages: 8, 12, and 16 weeks; 12-13 months; and 3 
                    years and 4 months of age. Multiple immunisations are offered at each appointment. 
                    Here, for simplicity, we have just shown the uptake of one of the immunisations 
                    offered at each appointment. The charts show the progression of uptake of the 
                    relevant immunisation as children age and the data tables provide the uptake 
                    rates at three specific time-points.  Data is provided  on children who have 
                    become eligible for immunisation during the pandemic (from March 2020 onwards) 
                    and for children who became eligible for immunisation before the pandemic (in 2019 
                    and in January and February 2020) for comparison."),
                  p("After a child becomes eligible for an immunisation, it takes time for them to 
                    attend their appointment, and for a record of the immunisation provided to 
                    subsequently be entered into the SIRS system. We have allowed a 6-week window 
                    for this, therefore each release of this page will report on children becoming 
                    eligible for an immunisation up to 6 weeks before the date the data were extracted 
                    for analysis. Although children will generally have their immunisation, and their 
                    SIRS record updated accordingly, within 6 weeks of becoming eligible, the pandemic 
                    may have influenced not only how quickly eligible children receive their 
                    immunisations, but also how long it takes for children’s SIRS records to be updated 
                    once an immunisation has been given. Any disruption to SIRS data entry may vary 
                    across NHS Boards. Data provided for the most recent cohorts of children will 
                    therefore not be fully complete in SIRS and should be viewed as provisional. The 
                    uptake rates for each cohort will be refreshed with more up-to-date data every 4 
                    to 5 weeks, and rates for the most recent cohorts may increase slightly as relevant 
                    records are updated in SIRS."),
                  p("On this page, data for yearly and monthly cohorts are shown for Scotland and for 
                    NHS Board areas. For the second dose of MMR vaccine at the 3 year 4 months appointment 
                    specifically, results for NHS Grampian are not shown separately, and NHS Grampian 
                    is excluded from the ‘Scotland’ totals. This is because children in NHS Grampian 
                    are offered the second dose of MMR vaccine at 4 years of age rather than 3 years 
                    4 months. Separate figures on uptake of the second dose of MMR vaccine from age 4 
                    years are available for NHS Grampian only through the data download button at the 
                    top of the page. "),
                  p("The data downloads also include information by Health and Social Care Partnerships, 
                    as well as weekly cohorts. Note that due to small numbers of children in the Island 
                    Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are provided for 
                    monthly and yearly cohorts only."),
                  p("Some NHS Boards and HSCPs have small numbers of children eligible for immunisation. 
                    Uptake rates based on these small numbers are prone to fluctuation, and it is 
                    important to bear this in mind when interpreting uptake rates.")),


          h3("Deprivation"),

          p(em("Click on legend to select or deselect categories - single click on an item to remove 
               it from the plot and double click on an item to view only that line.")),

          fluidRow(

            column(4, actionButton("btn_modal_simd_imm", "What is SIMD and deprivation?",
                                    icon = icon('question-circle'))),
            column(8,
                   div(downloadButton('download_imm_simd_data', 'Download deprivation data'), style = "float:right")),
            br(), br(),

            column(12, h4(paste0(imm_trends_title))),

            br(),

            column(6, withSpinner(plotlyOutput(plot_choice()[[1]]))),


            column(6, withSpinner(plotlyOutput(plot_choice()[[2]]))),

            br(),

            tagList(
              br(), br(), br(),
              p(em("* Please note that data for the most recent year are incomplete, calculated 
                   as the mean uptake of the months of the year so far.")),
              br(),
              p("The deprivation charts above show the immunisation uptake for children becoming 
                eligible for their immunisation during the Covid-19 pandemic. The graph on the 
                left shows this uptake by month, and the graph on the right compared to those 
                who became eligible in 2019, at all Scotland level. Early uptake achieved by 
                4 weeks after the children became eligible for their immunisation is considered, 
                as this indicator is available for the most recent cohorts of children as well as 
                the baseline 2019 cohort. The early uptake rates are shown for children living in 
                areas with different levels of deprivation."),
              p("The deprivation chart below shows the change in early uptake for children becoming 
                eligible for their immunisation during the Covid-19 pandemic, compared to those who 
                became eligible in 2019. Again, results are shown for children living in areas with 
                different levels of deprivation. So, for example, if early uptake for children 
                becoming eligible for an immunisation in 2019 and in March 2020 was 80% and 84% 
                respectively, this would be shown on the ‘change’ chart as a 4% absolute increase in 
                early uptake for children becoming eligible in March 2020. The deprivation data 
                download (available through the button above the ‘change’ chart) also provides the 
                relative change (5% in this example) as this allows an easier comparison across 
                deprivation groups if the baseline level of uptake varies between groups."),

              br()),

            column(12, h4(paste0(imm_change_title))),
            br(),
            column(12, withSpinner(plotlyOutput(plot_choice()[[3]]))),
            br()) # fluid row bracket
          )# taglist bracket
  }
  else{

    immune_title <- case_when(input$`immun-measure` == "sixin_dose1" ~ paste0("Uptake of first dose of 6-in-1 vaccine (offered to children at 8 weeks of age): ",
                                                                                   input$`immun-geoname`),
                              input$`immun-measure` == "sixin_dose2" ~ paste0("Uptake of second dose 6-in-1 vaccine (offered to children at 12 weeks of age): ", input$`immun-geoname`),
                              input$`immun-measure` == "sixin_dose3" ~ paste0("Uptake of third dose 6-in-1 vaccine (offered to children at 16 weeks of age): ", input$`immun-geoname`),
                              input$`immun-measure` == "mmr_dose1" ~ paste0("Uptake of first dose MMR vaccine (offered to children at 12-13 months of age): ", input$`immun-geoname`),
                              input$`immun-measure` == "mmr_dose2" ~ paste0("Uptake of second dose MMR vaccine (offered to children at 3 years 4 months of age): ", input$`immun-geoname`))

    immune_subtitle <-  paste0("Figures based on data extracted from SIRS on ",immunisation_extract_date)

    tagList(fluidRow(br(),
                     column(12,
                            p("Immunisation protects children against certain serious infections. Public Health Scotland and Scottish Government have produced a range of communications reminding parents that the NHS is still open for childhood immunisations, signposting parents to up to date advice via ",
                              tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
                                     "NHS inform (external website)", target="_blank"),"."),
                            h4(paste0(immune_title)),
                            p(immune_subtitle))),
            fluidRow(column(6,br(), br(),
                            withSpinner(plotlyOutput("immun_scurve"))),
            column(6, uiOutput("immun_table"))),

            tagList(p("All preschool children are offered a total of five immunisation 
                      appointments as they reach the following ages: 8, 12, and 16 weeks; 
                      12-13 months; and 3 years and 4 months of age. Multiple immunisations 
                      are offered at each appointment. Here, for simplicity, we have just 
                      shown the uptake of one of the immunisations offered at each appointment. 
                      The charts show the progression of uptake of the relevant immunisation as 
                      children age and the data tables provide the uptake rates at three 
                      specific time-points.  Data is provided  on children who have become 
                      eligible for immunisation during the pandemic (from March 2020 onwards) 
                      and for children who became eligible for immunisation before the pandemic 
                      (in 2019 and in January and February 2020) for comparison."),
                    p("After a child becomes eligible for an immunisation, it takes time for 
                      them to attend their appointment, and for a record of the immunisation 
                      provided to subsequently be entered into the SIRS system. We have allowed 
                      a 6-week window for this, therefore each release of this page will report 
                      on children becoming eligible for an immunisation up to 6 weeks before the 
                      date the data were extracted for analysis. Although children will generally 
                      have their immunisation, and their SIRS record updated accordingly, within 
                      6 weeks of becoming eligible, the pandemic may have influenced not only how 
                      quickly eligible children receive their immunisations, but also how long it 
                      takes for children’s SIRS records to be updated once an immunisation has 
                      been given. Any disruption to SIRS data entry may vary across NHS Boards. 
                      Data provided for the most recent cohorts of children will therefore not be 
                      fully complete in SIRS and should be viewed as provisional. The uptake rates 
                      for each cohort will be refreshed with more up-to-date data every 4 to 5 weeks, 
                      and rates for the most recent cohorts may increase slightly as relevant records 
                      are updated in SIRS."),
                    p("On this page, data for yearly and monthly cohorts are shown for Scotland and 
                      for NHS Board areas. For the second dose of MMR vaccine at the 3 year 4 months 
                      appointment specifically, results for NHS Grampian are not shown separately, 
                      and NHS Grampian is excluded from the ‘Scotland’ totals. This is because children 
                      in NHS Grampian are offered the second dose of MMR vaccine at 4 years of age rather 
                      than 3 years 4 months. Separate figures on uptake of the second dose of MMR vaccine 
                      from age 4 years are available for NHS Grampian only through the data download button 
                      at the top of the page. "),
                    p("The data downloads also include information by Health and Social Care Partnerships, 
                      as well as weekly cohorts. Note that due to small numbers of children in the Island 
                      Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are provided for 
                      monthly and yearly cohorts only."),
                    p("Some NHS Boards and HSCPs have small numbers of children eligible for immunisation. 
                      Uptake rates based on these small numbers are prone to fluctuation, and it is 
                      important to bear this in mind when interpreting uptake rates.")))



  }
})


###############################################.
## Data downloads ----
###############################################.

# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
imm_data_download <- reactive({
  if (input$`immun-measure` == "mmr_dose2" & input$`immun-geoname` == "NHS Grampian") {
    mmrtable_dose2_gramp %>%
      select(immunisation, area_name, time_period_eligible, denominator, starts_with("uptake"))  %>%
      rename(cohort = time_period_eligible)
  } else {

    data_down <- switch(
      input$`immun-measure`,
      # for data download filter on dose for table appearing in the app
      "sixin_dose1" = filter(sixtable,str_detect(immunisation,"dose 1")),
      "sixin_dose2" = filter(sixtable,str_detect(immunisation,"dose 2")),
      "sixin_dose3" = filter(sixtable,str_detect(immunisation,"dose 3")),
      "mmr_dose1" = filter(mmrtable,str_detect(immunisation,"dose 1")),
      "mmr_dose2"= filter(mmrtable,str_detect(immunisation,"dose 2"))) %>%
      select(-cohort) %>%
      rename(cohort = time_period_eligible) %>%
      mutate_at(vars(contains("percent")), ~format(., digits=1, nsmall=1))#forcing variables to show one decimal digit.

    if (input$`immun-measure` %in% "sixin_dose1") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_12weeks_num, uptake_12weeks_percent,
               uptake_24weeks_num, uptake_24weeks_percent, uptake_tot_num, uptake_tot_percent)
    } else if (input$`immun-measure` %in% "sixin_dose2") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_16weeks_num, uptake_16weeks_percent,
               uptake_28weeks_num, uptake_28weeks_percent, uptake_tot_num, uptake_tot_percent)
    } else if (input$`immun-measure` %in% "sixin_dose3") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_20weeks_num, uptake_20weeks_percent,
               uptake_32weeks_num, uptake_32weeks_percent,uptake_tot_num, uptake_tot_percent)
    } else if (input$`immun-measure` %in% "mmr_dose1") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_13m_num, uptake_13m_percent,
               uptake_16m_num, uptake_16m_percent, uptake_tot_num, uptake_tot_percent)
    } else if (input$`immun-measure` %in% "mmr_dose2") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_3y5m_num, uptake_3y5m_percent,
               uptake_3y8m_num, uptake_3y8m_percent, uptake_tot_num, uptake_tot_percent)
    }

    data_down %>% #forcing variables to show one decimal digit.
      mutate_at(vars(contains("percent")), ~format(., digits=1, nsmall=1))#forcing variables to show one decimal digit.
  }
})

output$download_imm_data <- downloadHandler(
  filename ="immunisation_extract.csv",
  content = function(file) {
    write_csv(imm_data_download(),
              file) }
)

##download immunisation SIMD data
imm_simd_data_download <- reactive ({

  data_down <- switch(
    input$`immun-measure`,
    "sixin_dose1" = six_simd_dose1,
    "sixin_dose2" = six_simd_dose2,
    "sixin_dose3" = six_simd_dose3,
    "mmr_dose1" = mmr_simd_dose1,
    "mmr_dose2"= mmr_simd_dose2) %>%
    select(-cohort) %>%
    rename(cohort = time_period_eligible, deprivation_quintile = simdq)

  if (input$`immun-measure` %in% "sixin_dose1") {
    data_down <- data_down %>%
      select(immunisation, area_name, cohort, deprivation_quintile,
             children_turn_8weeks_num = denominator,
             children_rec_imm_12weeks_num = uptake_12weeks_num,
             uptake_12weeks_percent,
             children_turn_8weeks_2019_num = baseline_denominator,
             children_rec_imm_12weeks_2019_num = baseline_numerator,
             uptake_12weeks_2019_percent = baseline_12weeks,
             absolute_change_from_baseline_percent = week12_abs_diff,
             relative_change_from_baseline_percent = week12_rel_diff)

  } else   if (input$`immun-measure` %in% "sixin_dose2") {
    data_down <- data_down %>%
      select(immunisation, area_name, cohort, deprivation_quintile,
             children_turn_12weeks_num = denominator,
             children_rec_imm_16weeks_num = uptake_16weeks_num,
             uptake_16weeks_percent,
             children_turn_12weeks_2019_num = baseline_denominator,
             children_rec_imm_16weeks_2019_num = baseline_numerator,
             uptake_16weeks_2019_percent = baseline_16weeks,
             absolute_change_from_baseline_percent = week16_abs_diff,
             relative_change_from_baseline_percent = week16_rel_diff)

  } else   if (input$`immun-measure` %in% "sixin_dose3") {
    data_down <- data_down %>%
      select(immunisation, area_name, cohort, deprivation_quintile,
             children_turn_16weeks_num = denominator,
             children_rec_imm_20weeks_num = uptake_20weeks_num,
             uptake_20weeks_percent,
             children_turn_16weeks_2019_num = baseline_denominator,
             children_rec_imm_20weeks_2019_num = baseline_numerator,
             uptake_20weeks_2019_percent = baseline_20weeks,
             absolute_change_from_baseline_percent = week20_abs_diff,
             relative_change_from_baseline_percent = week20_rel_diff)
  } else   if (input$`immun-measure` %in% "mmr_dose1") {
    data_down <- data_down %>%
      select(immunisation, area_name, cohort, deprivation_quintile,
             children_turn_12months_num = denominator,
             children_rec_imm_13months_num = uptake_57weeks_num,
             uptake_13months_percent = uptake_57weeks_percent,
             children_turn_12months_2019_num = baseline_denominator,
             children_rec_imm_13months_2019_num = baseline_numerator,
             uptake_13months_2019_percent = baseline_57weeks,
             absolute_change_from_baseline_percent = week57_abs_diff,
             relative_change_from_baseline_percent = week57_rel_diff)

  } else   if (input$`immun-measure` %in% "mmr_dose2") {
    data_down <- data_down %>%
      select(immunisation, area_name, cohort, deprivation_quintile,
             children_turn_3y4months_num = denominator,
             children_rec_imm_3y5months_num = uptake_178weeks_num,
             uptake_3y5months_percent = uptake_178weeks_percent,
             children_turn_3y4months_2019_num = baseline_denominator,
             children_rec_imm_3y5months_2019_num = baseline_numerator,
             uptake_3y5months_2019_percent = baseline_178weeks,
             absolute_change_from_baseline_percent = week178_abs_diff,
             relative_change_from_baseline_percent = week178_rel_diff)
  }

  data_down %>% #forcing variables to show one decimal digit.
    mutate_at(vars(starts_with("uptake"), absolute_change_from_baseline_percent,
                   relative_change_from_baseline_percent),
              ~format(., digits=1, nsmall=1))

})


output$download_imm_simd_data <- downloadHandler(
  filename ="immunisation_extract_by_deprivation.csv",
  content = function(file) {
    write_csv(imm_simd_data_download(), file) }
)

#END

