
###############################################.
## Reactive data ----
###############################################.

##reactive data to show in app
data_table <- reactive({
  # Change dataset depending on what user selected
  table_data <- switch(input$data_select,
        "rapid" = rapid %>% rename(specialty = spec, average_2018_2019 = count_average),
         "aye" = aye %>% rename(average_2018_2019 = count_average),
         "nhs24" = nhs24 %>% rename(average_2018_2019 = count_average),
         "ooh" = ooh %>% rename(average_2018_2019 = count_average),
         "sas" = sas %>% rename(average_2018_2019 = count_average),
         "deaths" = deaths %>% rename(average_2015_2019 = count_average),
         "sixin_8wks" = six,
         "first_visit" = first) %>% 
    # Note: character variables are converted to factors in each
    # dataset for use in the table
    # This is because dropdown prompts on the table filters only
    # appear for factors
    mutate_if(is.character, as.factor) 
  
  if (input$data_select %in% c("rapid", "aye", "nhs24", "ooh", "sas", "deaths")) {
    table_data <- table_data %>% 
    # Formatting to a "nicer" style
    select(-type) %>% 
    rename("Variation (%)" = variation) %>% 
    mutate(category = recode_factor(category, "All" = "All", "Female" = "Female", "Male" = "Male",
                                    "1 - most deprived" = "Quintile 1 - most deprived",
                                    "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4",
                                    "5 - least deprived" = "Quintile 5 - least deprived",
                                    "Under 5" = "Aged under 5", "5 - 14"= "Aged 5 to 14",
                                    "15 - 44" = "Aged 15 to 44","45 - 64" = "Aged 45 to 64",
                                    "65 - 74" = "Aged 65 to 74", "75 - 84" = "Aged 75 to 84", 
                                    "85 and over" = "Aged 85 and over",
                                    "Under 65" = "Aged under 65",
                                    "65 and over" = "Aged 65 and over"),
           week_ending = format(week_ending, "%d %b %y"))
  } else if (input$data_select %in% "first_visit") {
    table_data <- table_data %>%
      select(-extract_date, -tabno, -week_no, -review, -cohort) %>% 
      rename(week_starting = week_2_start, children_due_visit_in = time_period_eligible,
             age_in_days = interv, percentage_of_children_who_have_received_review = surv)
  } else if (input$data_select %in% "sixin_8wks") {
    table_data <- table_data %>%
      select(-extract_date, -tabno, -week_no, -immunisation, -cohort) %>% 
      rename(week_starting = week_8_start, children_due_immunisation_in = time_period_eligible,
             age_in_days = interv, percentage_of_children_who_have_received_immunisation = surv)
  }
  
  table_data %>% 
    rename_all(list(~str_to_sentence(.))) %>% # initial capital letter
    select(sort(current_vars())) # order columns alphabetically
})

###############################################.
## Table ----
###############################################.

output$table_filtered <- DT::renderDataTable({
  
  # Remove the underscore from column names in the table
  table_colnames  <-  gsub("_", " ", colnames(data_table()))
  
  DT::datatable(data_table(), style = 'bootstrap',
                class = 'table-bordered table-condensed',
                rownames = FALSE,
                options = list(pageLength = 20,
                               dom = 'tip',
                               autoWidth = TRUE),
                filter = "top",
                colnames = table_colnames)
})

###############################################.
## Data downloads ----
###############################################.
# Data download of data table. 
output$download_table_csv <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    # This downloads only the data the user has selected using the table filters
    write_csv(data_table()[input[["table_filtered_rows_all"]], ], file) 
  } 
)