
###############################################...
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
         "ooh_cons" = ooh_cons %>% rename(average_2018_2019 = count_average),
         "sas" = sas %>% rename(average_2018_2019 = count_average),
         "deaths" = deaths %>% rename(average_2015_2019 = count_average),
         "cardio_drugs" = cardio_drugs %>% rename(average_2018_2019 = count_average),
         "cath_lab" = cath_lab %>% rename(average_2018_2019 = count_average),
        "ooh_cardiac" = ooh_cardiac %>% rename(average_2018_2019 = count_average),
        "sas_cardiac" = sas_cardiac %>% rename(average_2018_2019 = count_average),
        "cardio_admissions" = cardio_admissions %>% rename(average_2018_2019 = count_average),        
        "cardio_deaths" = cardio_deaths %>% rename(average_2015_2019 = count_average),       
         "sixin_8wks" = sixtable %>% filter(immunisation == "six-in-one dose 1"),
         "sixin_8wks_second" = sixtable %>% filter(immunisation == "six-in-one dose 2"),
         "sixin_8wks_third" = sixtable %>% filter(immunisation == "six-in-one dose 3"),
        "mmr_1dose" = mmrtable %>% filter(immunisation == "mmr dose 1") ,
        "mmr_2dose" = mmrtable %>% filter(immunisation == "mmr dose 2"),
         "first_visit" = firsttable,
         "sixtoeight_visit" = sixtoeighttable,
         "thirteen_visit" = thirteentable,
         "twentyseven_visit" = twentyseventable,
         "fourtofive_visit" = fourtofivetable,
         "cancer" = cancer_data2,
         # "dce" = dce_data,
         "ui_smr01_all" = ui_smr01_all %>% rename(average_2018_2019 = count_average, "Variation (%)" = variation,Month=week_ending),
         "ui_smr01_rta"= ui_smr01_rta %>% rename(average_2018_2019 = count_average, "Variation (%)" = variation),
         "ui_smr01_poison"=ui_smr01_poison %>% rename(average_2018_2019 = count_average, "Variation (%)" = variation), 
         "ui_smr01_other"=ui_smr01_other %>% rename(average_2018_2019 = count_average, "Variation (%)" = variation),
         "ui_smr01_falls"=ui_smr01_falls %>% rename(average_2018_2019 = count_average, "Variation (%)" = variation),
         "ui_smr01_assaults"=ui_smr01_assaults%>% rename(average_2018_2019 = count_average, "Variation (%)" = variation),
        "sact_weekly" = sact_data_wk_inc,
        "sact_monthly" = sact_data_inc,
         "childdev" = child_dev,
         "breastfeeding" = breastfeeding,
        "perinatal" = perinatal,
        "top" = top_download ,
        "ante_booking" = booking_download,
        "induct" = induct_download,
        "mod"= mod_download,
        "gestation" = gestation_download,
        "apgar" = apgar_download,
        "preterm" = preterm_chart,
        "tears" = tears_download,
        "mhdrugs" = mentalhealth_drugs %>% select(-type) %>% rename(average_2018_2019 = count_average, "Variation (%)" = variation),
        "ae_mh" = ae_mh %>% select(-type) %>% rename(average_2018_2019 = count_average, "Variation (%)" = variation),
        "ooh_mh" = mh_ooh %>% select(-type) %>% rename(average_2018_2019 = count_average, "Variation (%)" = variation),
        "outpats" = outpats %>% 
          rename(appointment_type = admission_type, specialty = spec, average_2018_2019 = count_average),
        'THN_by_HB'=THN_by_HB,
        'DTR_data'=DTR_data,
        'OST_paid'=OST_paid,
       'SASdata'=SASdata[,c(1,2,5,6)]
  ) %>% 
    # Note: character variables are converted to factors in each
    # dataset for use in the table
    # This is because dropdown prompts on the table filters only appear for factors
    mutate_if(is.character, as.factor) 
  
  if (input$data_select %in% c("rapid", "aye", "nhs24", "ooh", "sas", "deaths",
                               "ooh_cardiac", "sas_cardiac","cardio_admissions","cardio_deaths")) {
    table_data %<>%
    # Formatting to a "nicer" style
    # select(-type) %>% 
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
           type = recode_factor(type, "sex" = "Sex", "age" = "Age Group", 
                                "dep" = "Deprivation",
                                "moc" = "Mode of Clinical Interaction"),
           week_ending = format(week_ending, "%d %b %y"))
  }  else if (input$data_select %in% "outpats") { 
    table_data %<>%
      # Formatting to a "nicer" style
      # select(-type) %>% 
      rename("Variation (%)" = variation, time_ending = week_ending) %>% 
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
             type = recode_factor(type, "sex" = "Sex", "age" = "Age Group", 
                                  "dep" = "Deprivation",
                                  "moc" = "Mode of Clinical Interaction",
                                  "eth" = "Ethnic Group"),
             time_ending = ifelse(time_split == "Monthly", format(time_ending, "%b %y"), format(time_ending, "%d %b %y")))
  } else if (input$data_select %in% "first_visit") { 
    table_data %<>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage"), cohort) %>% 
      mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
      arrange(desc(cohort)) %>% 
      select(-cohort) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 06 weeks of age (N)" = coverage_6weeks_num,
             "Coverage of review at 06 weeks of age (%)" = coverage_6weeks_percent,
             "Coverage of review at 18 weeks of age (N)" = coverage_18weeks_num,
             "Coverage of review at 18 weeks of age (%)" = coverage_18weeks_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "sixtoeight_visit") { 
    table_data %<>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage"), cohort) %>% 
      mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
      arrange(desc(cohort)) %>% 
      select(-cohort) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 10 weeks of age (N)" = coverage_10weeks_num,
             "Coverage of review at 10 weeks of age (%)" = coverage_10weeks_percent,
             "Coverage of review at 22 weeks of age (N)" = coverage_22weeks_num,
             "Coverage of review at 22 weeks of age (%)" = coverage_22weeks_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "thirteen_visit") { 
    table_data %<>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage"), cohort) %>% 
      mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
      arrange(desc(cohort)) %>% 
      select(-cohort) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 14 months of age (N)" = coverage_14months_num,
             "Coverage of review at 14 months of age (%)" = coverage_14months_percent,
             "Coverage of review at 17 months of age (N)" = coverage_17months_num,
             "Coverage of review at 17 months of age (%)" = coverage_17months_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "twentyseven_visit") { 
    table_data %<>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage"), cohort) %>% 
      mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
      arrange(desc(cohort)) %>% 
      select(-cohort) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 28 months of age (N)" = coverage_28months_num,
             "Coverage of review at 28 months of age (%)" = coverage_28months_percent,
             "Coverage of review at 31 months of age (N)" = coverage_31months_num,
             "Coverage of review at 31 months of age (%)" = coverage_31months_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "fourtofive_visit") { 
    table_data %<>%
      select(area_name, time_period_eligible, denominator, starts_with("coverage"), cohort) %>% 
      mutate(cohort=factor(cohort,levels=c("weekly","monthly","yearly"))) %>%
      arrange(desc(cohort)) %>% 
      select(-cohort) %>%
      rename(Cohort = time_period_eligible, "Total number of children" = denominator,
             "Coverage of review at 49 months of age (N)" = coverage_49months_num,
             "Coverage of review at 49 months of age (%)" = coverage_49months_percent,
             "Coverage of review at 52 months of age (N)" = coverage_52months_num,
             "Coverage of review at 52 months of age (%)" = coverage_52months_percent,
             "Total coverage of review (N)" = coverage_tot_num,
             "Total coverage of review (%)" = coverage_tot_percent)
  } else if (input$data_select %in% "sixin_8wks") {
    table_data %<>%
      select(area_name, Cohort = time_period_eligible, 
             "Total number of children" = denominator, 
             "Uptake of immunisation at 12 weeks of age (N)" = uptake_12weeks_num,
             "Uptake of immunisation at 12 weeks of age (%)" = uptake_12weeks_percent,
             "Uptake of immunisation at 24 weeks of age (N)" = uptake_24weeks_num,
             "Uptake of immunisation at 24 weeks of age (%)" = uptake_24weeks_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select %in% "sixin_8wks_second") {
    table_data %<>%
      select(area_name, Cohort = time_period_eligible, 
             "Total number of children" = denominator, 
             "Uptake of immunisation at 16 weeks of age (N)" = uptake_16weeks_num,
             "Uptake of immunisation at 16 weeks of age (%)" = uptake_16weeks_percent,
             "Uptake of immunisation at 28 weeks of age (N)" = uptake_28weeks_num,
             "Uptake of immunisation at 28 weeks of age (%)" = uptake_28weeks_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select %in% "sixin_8wks_third") {
    table_data %<>%
      select(area_name, Cohort = time_period_eligible, 
           "Total number of children" = denominator, 
             "Uptake of immunisation at 20 weeks of age (N)" = uptake_20weeks_num,
             "Uptake of immunisation at 20 weeks of age (%)" = uptake_20weeks_percent,
             "Uptake of immunisation at 32 weeks of age (N)" = uptake_32weeks_num,
             "Uptake of immunisation at 32 weeks of age (%)" = uptake_32weeks_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select %in% "mmr_1dose") {
    table_data %<>%
      select(area_name, Cohort = time_period_eligible, 
             "Total number of children" = denominator, 
             "Uptake of immunisation at 13 months of age (N)" = uptake_13m_num,
             "Uptake of immunisation at 13 months of age (%)" = uptake_13m_percent,
             "Uptake of immunisation at 16 months of age (N)" = uptake_16m_num,
             "Uptake of immunisation at 16 months of age (%)" = uptake_16m_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select %in% "mmr_2dose") {
    table_data %<>%
      select(area_name, Cohort = time_period_eligible, 
             "Total number of children" = denominator, 
             "Uptake of immunisation at 3 years 5 months of age (N)" = uptake_3y5m_num,
             "Uptake of immunisation at 3 years 5 months of age (%)" = uptake_3y5m_percent,
             "Uptake of immunisation at 3 years 8 months of age (N)" = uptake_3y8m_num,
             "Uptake of immunisation at 3 years 8 months of age (%)" = uptake_3y8m_percent,
             "Total uptake of immunisation (N)" = uptake_tot_num,
             "Total uptake of immunisation (%)" = uptake_tot_percent)
  } else if (input$data_select == "ae_cardio") {
    table_data %<>%
      select(-area_type) %>% 
      rename("Variation (%)" = variation) %>%
      mutate(type = recode_factor(type, "all" = "All", "age" = "Age Group", "dep" = "Deprivation"),
             category = recode_factor(category, "1 - most deprived" = "Quintile 1 - most deprived",
                                      "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4",
                                      "5 - least deprived" = "Quintile 5 - least deprived",
                                      "<65" = "Aged under 65",
                                      "65+" = "Aged 65 and over"),
             week_ending = format(week_ending, "%d %b %y"))
  } else if (input$data_select %in% c("cardio_drugs")) {
    table_data %<>%
      select(-type) %>% 
      rename("Variation (%)" = variation) %>%
      mutate(week_ending = format(week_ending, "%d %b %y"))
  } else if (input$data_select == "cath_lab") {
    table_data %<>%
      rename("Variation (%)" = variation,
             " Catheterisation lab" = lab,
             "Intervention" = groups) %>%
      mutate(type = recode_factor(type, "age" = "Age Group", "sex" = "Sex"),
             week_ending = format(week_ending, "%d %b %y"))
  } else if (input$data_select %in% "perinatal") {
    table_data %<>%
      select(area_name, month_of_year, number_of_deaths_in_month, sample_size, rate, type) %>%
      mutate(type = recode_factor(type, "extperi" = "Extended perinatal deaths", "infantdeaths" = "Infant deaths", "nnd" = "Neonatal deaths", 
                                  "pnnd" = "Post-neonatal deaths", "stillbirths" = "Stillbirths")) %>%
      rename("Area name" = area_name, "Relevant births" = sample_size,
             "Month of year" = month_of_year,
             "Number of deaths" = number_of_deaths_in_month,
             "Rate" = rate,
             "Type" = type)
  } else if (input$data_select %in% "cancer") {
    table_data <- table_data %>%
      select(area:count21, breakdown) %>%
      filter(breakdown != "None") %>% 
      rename("Area name" = area, "Cancer type" = site,
             "Sex" = sex,
             "Age Group" = age_group,
             "Deprivation Quintile (0=unknown)" = dep,
             "Week Number" = week_number,
             "Count 2019" = count19,
             "Count 2020" = count20,
             "Count 2021" = count21,
             "Breakdown" = breakdown)
  } else if (input$data_select %in% "sact_weekly") {
    table_data <- table_data %>%
      select(week_beginning, region, area, site, appt_reg, treatment, count, week_on_refweek_perc) %>%
      rename("Week beginning" = week_beginning, "Region" = region, "Area name" = area, 
             "Cancer type" = site, "Administration route derivation" = appt_reg,
             "Administration route" = treatment, "Number of appointments" = count,
             "Percentage change vs. average reference Week" = week_on_refweek_perc)
  } else if (input$data_select %in% "sact_monthly") {
    table_data <- table_data %>%
      select(month, region, area, site, treatment, count) %>%
      rename("Month" = month, "Region" = region, "Area name" = area, "Cancer type" = site, 
             "Administration route" = treatment, "Number of patients" = count)
  # } else if (input$data_select %in% "dce") {
  #   table_data <- table_data %>%
  #     select(area:count20, month) %>%
  #     rename("Area name" = area, "Cancer Type" = site, "Stage" = stage, 
  #            "No. Patients 2019" = count19, "No. Patients 2020" = count20, "Month" = month)
  } else if (input$data_select %in% "childdev") {
    table_data %<>%
      select(area_name, month_review, review, number_reviews = no_reviews, 
             meaningful_reviews = no_meaningful_reviews,
             "% meaningful reviews" = pc_meaningful,
             "One or more concerns" = concerns_1_plus,
             "% one or more concerns" = pc_1_plus)
  } else if (input$data_select %in% "breastfeeding") {
    table_data %<>%
      select(area_name, month_review, review, number_reviews = no_reviews, 
             number_valid_reviews = no_valid_reviews,
             exclusive_breastfeeding = exclusive_bf,
             "% exclusive breastfeeding" = pc_excl,
             overall_breastfeeding = overall_bf,
             "% overall breastfeeding" = pc_overall,
             ever_breastfeeding = ever_bf,
             "% ever breastfeeding" = pc_ever)
  } else if (input$data_select %in% "top") {
    table_data <- table_data %>% 
      select(area_name, area_type, termination_month, category = chart_category,
             number_of_terminations, number_of_terminations_gest_under_10wks,
             number_of_terminations_gest_10to12wks, number_of_terminations_gest_over_12wks,
             average_gestation_at_termination)
  } else if (input$data_select %in% "ante_booking") {
    table_data <- table_data %>% 
      select(area_name, area_type, booking_month, booking_week_beginning, category = chart_category,
             number_of_women_booking, number_of_women_booking_gest_under_10wks,
             number_of_women_booking_gest_10to12wks, number_of_women_booking_gest_over_12wks,
             average_gestation_at_booking) %>% 
      mutate(category = case_when(category %in% c("20-24", "25-29", "30-34", "35-39", 
                                                  "40 and over", "Under 20", "1 - most deprived", "2", "3", "4", 
                                                  "5 - least deprived") ~ paste0(category),
                                                  TRUE ~ "All"))
  } else if (input$data_select %in% "induct") {
    table_data <- table_data %>% 
      select(area_name, area_type, month_of_discharge, subgroup, variable,
             induced_37_42, not_induced_37_42, unknown_induced_37_42, births_37_42,
             perc_induced_37_42, perc_not_induced_37_42, perc_unknown_induced_37_42) %>% 
      rename("Total number of singleton live births at 37-42 weeks gestation" = births_37_42,
             "Number of singleton live births at 37-42 weeks gestation that followed induction of labour" = induced_37_42,
             "Number of singleton live births at 37-42 weeks gestation that were not induced" = not_induced_37_42,
             "Number of singleton live births at 37-42 weeks gestation unknown" = unknown_induced_37_42,
             "Percentage (%) of singleton live births at 37-42 weeks gestation that followed induction of labour" = perc_induced_37_42,
             "Percentage (%) of singleton live births at 37-42 weeks gestation that were not induced" = perc_not_induced_37_42,
             "Percentage (%) of singleton live births at 37-42 weeks gestation unknown" = perc_unknown_induced_37_42) %>% 
      mutate(variable = case_when(variable %in% c("20-24", "25-29", "30-34", "35-39", 
                                                  "40 and over", "Under 20", "1 - most deprived", "2", "3", "4", 
                                                  "5 - least deprived", "Unknown") ~ paste0(variable),
                                  TRUE ~ "All")) %>% 
      mutate(subgroup = case_when(subgroup %in% c("SIMD", "AGEGRP") ~ paste0(subgroup),
                                  TRUE ~ "All"))

  } else if (input$data_select %in% "mod") {
    table_data <- table_data %>% 
      select(-c(chart_type, chart_category, dottedline_csection_all, dottedline_csection_elec,
                dottedline_csection_emer, indicator, centreline_csection_all, centreline_csection_elec,
                centreline_csection_emer, perc_denominator)) %>% 
       rename("Number of births - All births" = births_all, 
              "Number of births - Caesarean section" = csection_all,
              "Number of births - emergency Caesarean section" = csection_emer,
              "Number of births - elective Caesarean section" = csection_elec,
              "Number of births - Other/Not Known" = other_not_known,
              "Number of births - assisted vaginal delivery including breech" = assisted_vaginal_inc_breech, 
              "Number of births - spontaneous_vaginal_delivery" = spontaneous_vaginal,
              "Percentage (%) of births - assisted vaginal delivery including breech" = perc_assisted_vaginal_inc_breech, 
              "Percentage (%) of births - Caesarean section" = perc_csection_all,
              "Percentage (%) of births - emergency Caesarean section" = perc_csection_emer,
              "Percentage (%) of births - elective Caesarean section" = perc_csection_elec,
              "Percentage (%) of births - spontaneous vaginal delivery" = perc_spontaneous_vaginal,
              "Percentage (%) of births - other/not known" = perc_other_not_known) %>% 
      mutate(variable = case_when(variable %in% c("20-24", "25-29", "30-34", "35-39", 
                                                  "40 and over", "Under 20", "1 - most deprived", "2", "3", "4", 
                                                  "5 - least deprived", "Unknown") ~ paste0(variable),
                                  TRUE ~ "All")) %>% 
      mutate(subgroup = case_when(subgroup %in% c("SIMD", "AGEGRP") ~ paste0(subgroup),
                                  TRUE ~ "All"))
    
  } else if (input$data_select %in% "gestation") {
  table_data <- table_data %>% 
    select(-c(chart_type, chart_category, dottedline_32_36, dottedline_42plus, dottedline_under32,
              dottedline_under37, indicator, centreline_32_36, centreline_42plus, centreline_under32,
              centreline_under37, perc_denominator)) %>% 
    rename("Number of births - All births (18-44 weeks gestation)" = births_18_44, 
           "Number of births - 32-36 weeks gestation" = births_32_36,
           "Number of births - 37-41 weeks gestation" = births_37_41,
           "Number of births - At or over 42 weeks gestation" = births_42plus,
           "Number of births - Unknown gestation" = births_gest_unknown,
           "Number of births - Under 32 weeks gestation" = births_under32, 
           "Number of births - Under 37 weeks gestation" = births_under37,
           "Number of births - All births" = births_all,
           "Percentage (%) of births - 32-36 weeks gestation" = perc_32_36, 
           "Percentage (%) of births - 37-41 weeks gestation" = perc_37_41,
           "Percentage (%) of births - At or over 42 weeks gestation" = perc_42plus,
           "Percentage (%) of births - Under 32 weeks gestation" = perc_under32,
           "Percentage (%) of births - Under 37 weeks gestation" = perc_under37) %>% 
    mutate(variable = case_when(variable %in% c("20-24", "25-29", "30-34", "35-39", 
                                              "40 and over", "Under 20", "1 - most deprived", "2", "3", "4", 
                                              "5 - least deprived", "Unknown") ~ paste0(variable),
                              TRUE ~ "All")) %>% 
    mutate(subgroup = case_when(subgroup %in% c("SIMD", "AGEGRP") ~ paste0(subgroup),
                                TRUE ~ "All"))
  } else if (input$data_select %in% "apgar") {
    table_data <- table_data %>% 
      select(area_name, area_type, date_of_discharge, subgroup, variable,
             births_37_42_apgar5_0_6, births_37_42_apgar5_7_10, births_37_42_apgar5_unknown, births_37_42_apgar5_known,
             perc_births_37_42_apgar5_0_6, perc_births_37_42_apgar5_7_10) %>% 
      mutate(variable = case_when(variable %in% c("20-24", "25-29", "30-34", "35-39", 
                                                  "40 and over", "Under 20", "1 - most deprived", "2", "3", "4", 
                                                  "5 - least deprived", "Unknown") ~ paste0(variable),
                                  TRUE ~ "All")) %>% 
      mutate(subgroup = case_when(subgroup %in% c("SIMD", "AGEGRP") ~ paste0(subgroup),
                                  TRUE ~ "All"))
  } else if (input$data_select %in% "preterm") {
    table_data %<>%
      select(area_name, quarter, N_deliveries_23_26_NICU_site, N_deliveries_23_26, percentage_NICU_site) %>%
      rename("Area name" = area_name, "Total number of deliveries at 23-26w" = N_deliveries_23_26,
             "Quarter" = quarter,
             "Number of deliveries at 23-26w that occurred in a hospital with a NICU on site" = N_deliveries_23_26_NICU_site,
             "Percentage" = percentage_NICU_site)
    
  } else if (input$data_select %in% "tears") {
    table_data <- table_data %>% 
      select(area_name, area_type, date_of_discharge, subgroup, variable,
             no_tear_37_plus, "1st_2nd_degree_tear_37_42", "3rd_4th_degree_tear_37_42", unspecified_tear_37_42, 
             known_tear_status_37_42, unknown_tear_status_37_42,
             perc_no_tears_37_42,
             perc_1st2nd_tears_37_42, 
             perc_3rd4th_tears_37_42, 
             perc_unspecified_tears_37_42) %>% 
      mutate(variable = case_when(variable %in% c("20-24", "25-29", "30-34", "35-39", 
                                                  "40 and over", "Under 20", "1 - most deprived", "2", "3", "4", 
                                                  "5 - least deprived", "Unknown") ~ paste0(variable),
                                  TRUE ~ "All")) %>% 
      mutate(subgroup = case_when(subgroup %in% c("SIMD", "AGEGRP") ~ paste0(subgroup),
                                  TRUE ~ "All"))
  } else if (input$data_select == "ooh_cons") {
    table_data %<>%
      # Formatting to a "nicer" style
            mutate("Consultation type" = recode_factor(type, 
                                                 "COVID" = "Covid related", 
                                                 "NON COVID" = "Non-covid related",
                                                 "ALL" = "All consultations"),
             week_ending = format(week_ending, "%d %b %y"),
             variation = case_when(type == "COVID" ~ 0,
                                   T ~ variation)) %>% 
      rename("Variation (%)" = variation) %>%
      select(-category, -type)
  }
  
  
  table_data %<>% 
    rename_all(list(~str_to_sentence(.))) %>% # initial capital letter
    mutate_if(is.numeric, round, 1)
  
  if (!(input$data_select %in% c("childdev", "breastfeeding", "cancer", "sact_weekly", 
                                 "sact_monthly"))) {
    table_data %<>% 
      select(sort(current_vars()))  # order columns alphabetically
  }
  
  table_data
  
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
  filename = function() {
    paste(input$data_select, "_data.csv", sep = "")
  },
  content = function(file) {
    # This downloads only the data the user has selected using the table filters
    write_csv(data_table()[input[["table_filtered_rows_all"]], ], file) 
  } 
)

