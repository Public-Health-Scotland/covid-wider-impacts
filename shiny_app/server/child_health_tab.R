# Wider impacts dashboard - Child health tab - Child health reviews section
# Server code


# Pop-up modal explaining source of data
observeEvent(input$`childr-source-modal`, 
                 showModal(modalDialog(#CHILD HEALTH MODAL
                 title = "What is the data source?",
                 p("The information shown on the numbers of children eligible for routine preschool reviews is taken from the",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12", 
                          "Scottish Immunisation and Recall System (SIRS) (external website)",  target="_blank"),
                    ". The information recorded at each review is taken from the",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=10",
                          "Child Health Systems Programme-PreSchool (CHSP-PS) (external website)",  target="_blank"),
                    "."),
                 p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation of children when a scheduled vaccination is due."),
                 p("CHSP-PS is an electronic system used by all NHS Boards in Scotland. The CHSP Pre-School system supports the delivery of the child health programme by facilitating the automated call and recall of children for the agreed schedule of child health reviews for pre-school children. Child health reviews incorporate assessment of children's health, development, and wider wellbeing alongside provision of health promotion advice and parenting support."),
                 p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)", target="_blank")," routinely receives quarterly data extracts from SIRS and CHSP-PS for the purpose of producing and ",
                   (tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/child-health-pre-school-review-coverage/","publishing", target="_blank"))," coverage rates for child health reviews. To allow more rapid monitoring of the impact of Covid-19 on child health review coverage rates, PHS is also currently extracting a sub-set of data from SIRS & CHSP-PS each month."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))
    
###############################################.
## Functions ----
###############################################.
#####################################################################################.
## HV S-curve----
## Function for drawing S-Curve charts used in health visitor tabs.

plot_scurve_child <- function(dataset, age_week) {
  
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_child
  
  scurve_data <- dataset %>% filter(area_name == input$`childr-geoname`) %>% 
    mutate(colour_flag = case_when(time_period_eligible %in% isolate(input$dates_child) ~ 1,
                                   TRUE ~ 0))
                                    # filter to selected time periods, but don't re-execute each time input changes
                                    # time_period_eligible %in% isolate(input$dates_child))
  
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
    
    # Creating time trend plot
    plot_ly() %>%
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
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("childr")

# Get list of available time periods for plotting
# Assumes that the time periods available are the same for all data
available_time_periods_child = 
  first %>%
  # using pull to get a vector rather than select because the selectizeInput didn't work otherwise
  pull(time_period_eligible) %>%
  unique()

# Set the default time periods for plotting
# Assumes that the months are listed in ascending order in first, followed by the years
default_time_periods_child = tail(available_time_periods_child, 8)

# Child reactive drop-down control showing list of time periods
output$dates_ui_child <- renderUI({
  selectizeInput("dates_child", label = NULL, choices = available_time_periods_child, 
                 selected = default_time_periods_child, multiple = TRUE,
                 options = list(placeholder = 'Select time periods',
                                plugins = c('remove_button')))
})

# Reactive dataset for flextable filter on geographical area
filter_table_data_child <- function(dataset){
  
  # We want shiny to re-execute this function whenever the button is pressed, so create a dependency here
  input$btn_update_time_child
  
  dataset %>% filter(area_name == input$`childr-geoname` &
                       # we don't want this function to re-execute every time dates_immun changes, so isolate()
                       time_period_eligible %in% isolate(input$dates_child))
}

###############################################.
## Child Health Tab Reactive layout  ----
###############################################.

#run chart function to generate s curve  
output$child_first_scurve <- renderPlotly({plot_scurve_child(first, "2 weeks")})
output$child_first_table <- renderUI({child_table(firsttable, "2 weeks", "18 weeks")})

output$child_sixtoeight_scurve <- renderPlotly({plot_scurve_child(sixtoeight, "6 weeks")})
output$child_sixtoeight_table <- renderUI({child_table(sixtoeighttable, "6 weeks", "22 weeks")})

output$child_thirteen_scurve <- renderPlotly({plot_scurve_child(thirteen, "13 months")})
output$child_thirteen_table <- renderUI({child_table(thirteentable, "13 months", "17 months")})

output$child_twentyseven_scurve <- renderPlotly({plot_scurve_child(twentyseven, "27 months")})
output$child_twentyseven_table <- renderUI({child_table(twentyseventable, "27 months", "31 months")})

output$child_fourtofive_scurve <- renderPlotly({plot_scurve_child(fourtofive, "4 years")})
output$child_fourtofive_table <- renderUI({child_table(fourtofivetable, "4 years", "52 months")})

# The charts and text shown on the app will depend on what the user wants to see
output$child_health_explorer <- renderUI({

  # text for titles of cut charts
  child_title <- paste0(case_when(input$`childr-measure` == "first_visit" ~ paste0("Coverage of health visitor first visit (offered to children at 2 weeks of age): ",
                                                                                             input$`childr-geoname`),
                            input$`childr-measure` == "six_eightwks" ~ paste0("Coverage of child health review offered at 6-8 weeks of age: ", input$`childr-geoname`),
                            input$`childr-measure` == "13_15mnth" ~ paste0("Coverage of child health review offered at 13-15 months of age: ", input$`childr-geoname`),
                            input$`childr-measure` == "27_30mnth" ~ paste0("Coverage of child health review offered at 27-30 months of age: ", input$`childr-geoname`),
                            input$`childr-measure` == "4_5yr" ~ paste0("Coverage of child health review offered at 4-5 years of age: ", input$`childr-geoname`)))
  child_subtitle <-  paste0("Figures based on data extracted from SIRS and CHSP-PS on ",child_extract_date)

  #commentary to appear in child health tab
  commentary_first <-
    if (input$`childr-measure` == "4_5yr") {
      p("All preschool children should be offered the following health reviews: health visitor first visit, 6-8 week review, 13-15 month review, 27-30 month review, and 4-5 year review. Although the 4-5 year review only became mandated by government policy for children turning 4 from April 2020 onwards.", br(),
        "The charts show the progression of coverage of the relevant review as children age. The data tables provide the coverage rates at three specific time-points. Data is shown for children who have become eligible for review during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for review before the pandemic (in 2019 and in January and February 2020) for comparison.", br(), 
        "After a child becomes eligible for a review, it takes time for them to attend their appointment, and for a record of the review provided to subsequently be entered into the CHSP-PS system. We have allowed a 6-week window for this, therefore each release of this page will report on children becoming eligible for a review up to 6 weeks before the date these data were extracted for analysis. Although children will generally have their review, and their CHSP-PS record updated accordingly, within 6 weeks of becoming eligible, the pandemic may have influenced not only how quickly eligible children receive their reviews, but also how long it takes for children’s CHSP-PS records to be updated once a review has been given. Any disruption to CHSP-PS data entry may vary across NHS Boards. Data shown for the most recent cohorts of children will therefore not be fully complete in CHSP-PS and should be viewed as provisional. The coverage rates for each cohort will be refreshed with more up-to-date data every 4 weeks, and rates for the most recent cohorts may increase slightly as relevant records are updated in CHSP-PS.", br(), 
        "Data is shown for Scotland and for all NHS Board areas. Data for Health & Social Care Partnerships is also available in the data download. Weekly data is no longer shown in the charts as these were becoming difficult to interpret due to the number of lines, however the weekly data is available in the data download for all areas except the island boards, which have been excluded due to small numbers.", br(),
        "Coverage rates based on small numbers are prone to fluctuation. Therefore, in Boards with small numbers of children eligible for review, it is important to consider this when interpreting the rates.
        ")
    } else { 
      p("All preschool children should be offered the following health reviews: health visitor first visit, 6-8 week review, 13-15 month review, 27-30 month review, and 4-5 year review. Although the 4-5 year review only became mandated by government policy for children turning 4 from April 2020 onwards.", br(),
        "The charts show the progression of coverage of the relevant review as children age. The data tables provide the coverage rates at three specific time-points. Data is shown for children who have become eligible for review during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for review before the pandemic (in 2019 and in January and February 2020) for comparison.", br(), 
        "After a child becomes eligible for a review, it takes time for them to attend their appointment, and for a record of the review provided to subsequently be entered into the CHSP-PS system. We have allowed a 6-week window for this, therefore each release of this page will report on children becoming eligible for a review up to 6 weeks before the date these data were extracted for analysis. Although children will generally have their review, and their CHSP-PS record updated accordingly, within 6 weeks of becoming eligible, the pandemic may have influenced not only how quickly eligible children receive their reviews, but also how long it takes for children’s CHSP-PS records to be updated once a review has been given. Any disruption to CHSP-PS data entry may vary across NHS Boards. Data shown for the most recent cohorts of children will therefore not be fully complete in CHSP-PS and should be viewed as provisional. The coverage rates for each cohort will be refreshed with more up-to-date data every 4 weeks, and rates for the most recent cohorts may increase slightly as relevant records are updated in CHSP-PS.", br(), 
        "Data is shown for Scotland and for all NHS Board areas. Data for Health & Social Care Partnerships is also available in the data download. Weekly data is no longer shown in the charts as these were becoming difficult to interpret due to the number of lines, however the weekly data is available in the data download for all areas except the island boards, which have been excluded due to small numbers.", br(),
        "Coverage rates based on small numbers are prone to fluctuation. Therefore, in Boards with small numbers of children eligible for review, it is important to consider this when interpreting the rates.
        ")
    }
  
  explorer_child <-     tagList(
    fluidRow(column(12, h4(paste0(child_title)),
                    p(child_subtitle))))
  
  # Specify items to display in child health ui based on step 2 selection 
  if (input$`childr-measure` == "first_visit") {
    tagList(explorer_child,
      fluidRow(column(6,br(), 
                      p("Grey lines represent previous month's data not selected in step 3.")
                      withSpinner(plotlyOutput("child_first_scurve"))),
               column(6, uiOutput("child_first_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  }  else if (input$`childr-measure` == "six_eightwks"){
    tagList(explorer_child,
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_sixtoeight_scurve"))),
               column(6, uiOutput("child_sixtoeight_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else if (input$`childr-measure` == "13_15mnth") {
    tagList(explorer_child,
      fluidRow(column(12, em("13 months defined as 57 weeks"))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_thirteen_scurve"))),
               column(6, uiOutput("child_thirteen_table"))),

      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else if (input$`childr-measure` == "27_30mnth") {
    tagList(explorer_child,
      fluidRow(column(12, em("27 months defined as 117 weeks"))),
      fluidRow(column(6,br(), br(),
                      withSpinner(plotlyOutput("child_twentyseven_scurve"))),
               column(6, uiOutput("child_twentyseven_table"))),
      fluidRow(column(12, renderUI(commentary_first)))
    )
  } else {
    tagList(explorer_child,
            fluidRow(column(12, em("4 years defined as 209 weeks"))),
            fluidRow(column(6,br(), br(),
                            withSpinner(plotlyOutput("child_fourtofive_scurve"))),
                     column(6, uiOutput("child_fourtofive_table"))),
            fluidRow(column(12, renderUI(commentary_first)))
    )
  }
  
}) #close child_health_explorer function


###############################################.
## Data downloads ----
###############################################.

# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
visit_data_download <- reactive({
  switch(
    input$`childr-measure`,
    "first_visit" = firstdata,
    "six_eightwks" = sixtoeightdata,
    "13_15mnth" = thirteendata,
    "27_30mnth" = twentysevendata,
    "4_5yr" = fourtofivedata
  ) %>% 
    select(area_name, time_period_eligible, denominator, starts_with("coverage"), cohort) %>% 
    mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
    arrange(desc(cohort)) %>% 
    select(-cohort) %>% 
    rename(cohort = time_period_eligible) 
})

output$download_visit_data <- downloadHandler(
  filename ="child_visits_extract.csv",
  content = function(file) {
    write_csv(visit_data_download(),
              file) } 
)

#END

