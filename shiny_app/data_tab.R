
###############################################.
## Reactive data ----
###############################################.

##reactive data to show in app
data_table <- reactive({
  # Change dataset depending on what user selected
  table_data <- switch(input$data_select,
        "rapid" = rapid %>% rename(specialty = spec, average_2018_2019 = count_average),
         "aye" = aye %>% rename(average_2018_2019 = count_average),
         "ae_cardio" = ae_cardio %>% rename(average_2018_2019 = count_average),
         "nhs24" = nhs24 %>% rename(average_2018_2019 = count_average),
         "ooh" = ooh %>% rename(average_2018_2019 = count_average),
         "sas" = sas %>% rename(average_2018_2019 = count_average),
         "deaths" = deaths %>% rename(average_2015_2019 = count_average),
         "cardio_drugs" = cardio_drugs %>% rename(average_2018_2019 = count_average),
         "cath_lab" = cath_lab %>% rename(average_2018_2019 = count_average),
         "sixin_8wks" = bind_rows(sixtable,six_hscp_dose1),
         "sixin_8wks_second" = bind_rows(sixtable_dose2,six_hscp_dose2),
         "sixin_8wks_third" = bind_rows(sixtable_dose3,six_hscp_dose3),
        "mmr_1dose" = bind_rows(mmrtable_dose1,mmr_hscp_dose1) ,
        "mmr_2dose" = bind_rows(mmrtable_dose2, mmr_hscp_dose2),
         "first_visit" = firsttable,
         "sixtoeight_visit" = sixtoeighttable,
         "thirteen_visit" = thirteentable,
         "twentyseven_visit" = twentyseventable,
         "fourtofive_visit" = fourtofivetable,
         "perinatal" = perinatal,
         "top" = top_download ,
         "ante_booking" = booking_download 
        ) %>% 
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
      select(area_name, time_period_eligible, denominator, starts_with("coverage")) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 6 weeks of age (N)" = coverage_6weeks_num,
             "Coverage of review at 6 weeks of age (%)" = coverage_6weeks_percent,
             "Coverage of review at 18 weeks of age (N)" = coverage_18weeks_num,
             "Coverage of review at 18 weeks of age (%)" = coverage_18weeks_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "sixtoeight_visit") { 
    table_data <- table_data %>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage")) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 10 weeks of age (N)" = coverage_10weeks_num,
             "Coverage of review at 10 weeks of age (%)" = coverage_10weeks_percent,
             "Coverage of review at 22 weeks of age (N)" = coverage_22weeks_num,
             "Coverage of review at 22 weeks of age (%)" = coverage_22weeks_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "thirteen_visit") { 
    table_data <- table_data %>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage")) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 14 months of age (N)" = coverage_14months_num,
             "Coverage of review at 14 months of age (%)" = coverage_14months_percent,
             "Coverage of review at 17 months of age (N)" = coverage_17months_num,
             "Coverage of review at 17 months of age (%)" = coverage_17months_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "twentyseven_visit") { 
    table_data <- table_data %>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage")) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 28 months of age (N)" = coverage_28months_num,
             "Coverage of review at 28 months of age (%)" = coverage_28months_percent,
             "Coverage of review at 31 months of age (N)" = coverage_31months_num,
             "Coverage of review at 31 months of age (%)" = coverage_31months_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "fourtofive_visit") { 
    table_data <- table_data %>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage")) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 49 months of age (N)" = coverage_49months_num,
             "Coverage of review at 49 months of age (%)" = coverage_49months_percent,
             "Coverage of review at 52 months of age (N)" = coverage_52months_num,
             "Coverage of review at 52 months of age (%)" = coverage_52months_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "sixin_8wks") {
    table_data <- table_data %>%
      select(area_name, time_period_eligible, denominator, starts_with("uptake")) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Uptake of immunisation at 12 weeks of age (N)" = uptake_12weeks_num,
             "Uptake of immunisation at 12 weeks of age (%)" = uptake_12weeks_percent,
             "Uptake of immunisation at 24 weeks of age (N)" = uptake_24weeks_num,
             "Uptake of immunisation at 24 weeks of age (%)" = uptake_24weeks_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select %in% "sixin_8wks_second") {
    table_data <- table_data %>%
      select(area_name, time_period_eligible, denominator, starts_with("uptake")) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Uptake of immunisation at 16 weeks of age (N)" = uptake_16weeks_num,
             "Uptake of immunisation at 16 weeks of age (%)" = uptake_16weeks_percent,
             "Uptake of immunisation at 28 weeks of age (N)" = uptake_28weeks_num,
             "Uptake of immunisation at 28 weeks of age (%)" = uptake_28weeks_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select %in% "sixin_8wks_third") {
    table_data <- table_data %>%
      select(area_name, time_period_eligible, denominator, starts_with("uptake")) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Uptake of immunisation at 20 weeks of age (N)" = uptake_20weeks_num,
             "Uptake of immunisation at 20 weeks of age (%)" = uptake_20weeks_percent,
             "Uptake of immunisation at 32 weeks of age (N)" = uptake_32weeks_num,
             "Uptake of immunisation at 32 weeks of age (%)" = uptake_32weeks_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select %in% "mmr_1dose") {
    table_data <- table_data %>%
      select(-c("exclude_from_table", "shade_cells", "immunisation")) %>% 
      select(area_name, Cohort = time_period_eligible, 
             "Total number of children" = denominator, 
             "Uptake of immunisation at 13 months of age (N)" = uptake_13m_num,
             "Uptake of immunisation at 13 months of age (%)" = uptake_13m_percent,
             "Uptake of immunisation at 16 months of age (N)" = uptake_16m_num,
             "Uptake of immunisation at 16 months of age (%)" = uptake_16m_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select %in% "mmr_2dose") {
    table_data <- table_data %>%
      select(-c("exclude_from_table", "shade_cells", "immunisation")) %>% 
      select(area_name, Cohort = time_period_eligible, 
             "Total number of children" = denominator, 
             "Uptake of immunisation at 3 years 5 months of age (N)" = uptake_3y5m_num,
             "Uptake of immunisation at 3 years 5 months of age (%)" = uptake_3y5m_percent,
             "Uptake of immunisation at 3 years 8 months of age (N)" = uptake_3y8m_num,
             "Uptake of immunisation at 3 years 8 months of age (%)" = uptake_3y8m_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select == "ae_cardio") {
    table_data <- table_data %>% 
      select(-area_type) %>% 
      rename("Variation (%)" = variation) %>%
      mutate(type = recode_factor(type, "all" = "All", "age" = "Age Group", "dep" = "Deprivation"),
             category = recode_factor(category, "1 - most deprived" = "Quintile 1 - most deprived",
                                      "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4",
                                      "5 - least deprived" = "Quintile 5 - least deprived",
                                      "<65" = "Aged under 65",
                                      "65+" = "Aged 65 and over"),
             week_ending = format(week_ending, "%d %b %y"))
  } else if (input$data_select == "cardio_drugs") {
    table_data <- table_data %>% 
      select(-type) %>% 
      rename("Variation (%)" = variation) %>%
      mutate(week_ending = format(week_ending, "%d %b %y"))
  } else if (input$data_select == "cath_lab") {
    table_data <- table_data %>% 
      rename("Variation (%)" = variation,
             " Catheterisation lab" = lab,
             "Intervention" = groups) %>%
      mutate(type = recode_factor(type, "age" = "Age Group", "sex" = "Sex"),
             week_ending = format(week_ending, "%d %b %y"))
  
  } else if (input$data_select %in% "perinatal") {
    table_data <- table_data %>%
      select(area_name, month_of_year, number_of_deaths_in_month, sample_size, rate, type) %>%
      mutate(type = recode_factor(type, "extperi" = "Extended perinatal deaths", "infantdeaths" = "Infant deaths", "nnd" = "Neonatal deaths", 
                                  "pnnd" = "Post-neonatal deaths", "stillbirths" = "Stillbirths")) %>%
      rename("Area name" = area_name, "Relevant births" = sample_size,
             "Month of year" = month_of_year,
             "Number of deaths" = number_of_deaths_in_month,
             "Rate" = rate,
             "Type" = type)
  } else if (input$data_select %in% "top") {
    table_data <- table_data
  } else if (input$data_select %in% "ante_booking") {
    table_data <- table_data
  }
  
  table_data %>% 
    rename_all(list(~str_to_sentence(.))) %>% # initial capital letter
    select(sort(current_vars())) %>%  # order columns alphabetically
    mutate_if(is.numeric, round, 1)
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