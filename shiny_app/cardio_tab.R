# Server side for cardiovascular tab

###############################################.
## Reactive controls  ----
###############################################.

# Show list of area names depending on areatype selected
output$geoname_cardio_ui <- renderUI({

  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == input$area_cardio_select])

  selectizeInput("geoname_cardio", label = NULL,
                 choices = areas_summary, selected = "")

})

# Adding 'observeEvent' to allow reactive 'area of interest' selction on cardio tab
observeEvent(input$measure_cardio_select, {
  x <- input$measure_cardio_select
  
  if (x == "cath") {
    cardio_label = "Step 2 - Select the area of interest for cardiac catheterization labs"
    cardio_choices = c("Royal Infirmary of Edinburgh", "Golden Jubilee Hospital")
    hide("geoname_cardio_ui")
  }
  
  if (x == "aye") {
    cardio_label = "Step 2 - Select geography level for cardiovascular A&E attendances"
    cardio_choices = c("Scotland")
    hide("geoname_cardio_ui")
  }
  
  if (x == "drug_presc") {
    cardio_label = "Step 2 - Select geography level for cardiovascular drug prescriptions"
    cardio_choices = c("Scotland", "Health board", "HSC partnership")
    shinyjs::show("geoname_cardio_ui")
  }
  
  updateSelectInput(session, "area_cardio_select",
                    label = cardio_label,
                    choices = cardio_choices,
                    selected = cardio_choices[1]
  )
  
}, ignoreNULL= F)

###############################################.
## Modals ----
###############################################.

# Link action button click to modal launch 
observeEvent(input$btn_cardio_modal, 
             
             if (input$measure_cardio_select == "rapid") {
               showModal(modalDialog(#RAPID ADMISSIONS MODAL
                 title = "What is the data source?",
                 p(""),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }  else if (input$measure_cardio_select == "aye") {
               showModal(modalDialog(# Cardio A&E MODAL
                 title = "What is the data source?",
                 p("This tool provides a weekly summary of people attending A&E departments (Emergency Departments 
                   and Nurse/GP led minor injury units) with cardiovascular problems. It shows data from the recent 
                   past along with historical activity for comparison purposes. The recent trend data is shown by 
                   age group (under and over 65) and broad deprivation category (SIMD)."),
                 p("Additional information relating to the overall A&E activity is available from the ", 
                   tags$a(href="https://beta.isdscotland.org/find-publications-and-data/health-services/hospital-care/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/", 
                          "NHS Performs - weekly update of emergency department activity and waiting time statistics.", 
                          class="externallink")),
                 p("There are two types of data submitted to the A&E datamart: episode and aggregate level data. 
                   All hospitals with Emergency Departments submit episode level data containing a detailed record 
                   for each patient attendance. Some smaller sites (6% of the total annual attendances) – nurse/GP 
                   led minor injury units – can only provide aggregated monthly summary attendance and compliance 
                   figures, as they do not have the IT systems and support to enable collection and submission of 
                   detailed patient level information. The data for sites that submit aggregate level data is not 
                   included in the figures presented in the tool. "),
                 p("Attendances to A&E departments data sourced from the ",
                   tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=1&SubID=3", 
                          "Accident and Emergency Datamart (A&E2).",class="externallink"), 
                   "The A&E2 dataset is managed by ", 
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/", 
                          "Public Health Scotland (PHS).", class="externallink")),
                 p(tags$em("Please note that due to limitations in diagnosis recording in the A&E datamart, the 
                         figures reported for cardiology offer only a very rough indication of cardiovascular 
                         attendances and do not represent the exact figures for cardiovascular attendances at 
                         Emergency Departments. Part of the data quality issues is that the data on cardiovascular 
                         attendance only includes part or no data for NHS Ayrshire and Arran, NHS Fife, 
                         NHS Forth Valley and NHS Lothian.")),
                 p("The following ICD-10 codes were considered for the cardiovascular A&E data subset:"),
                 DT::dataTableOutput("ae_cardio_codes_tbl"),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_cardio_select == "drug_presc") {
               showModal(modalDialog(#Prescribin - Cardio Drugs
                 title = "What is the data source?",
                 p("Text to be written"),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             } else if (input$measure_cardio_select == "cath") {
               showModal(modalDialog(#CATH A&E MODAL
                 title = "What is the data source?",
                 p(""),
                 size = "m",
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
)

# Rendering A&E Cardio Codes table here for inclusion to modal above
output$ae_cardio_codes_tbl <- DT::renderDataTable(
  ae_cardio_codes
)

###############################################.
## Reactive datasets ----
###############################################.


###############################################.
## Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$cardio_explorer <- renderUI({
  # Charts and rest of UI
  if (input$measure_cardio_select == "cath") {
    if (input$area_cardio_select == "Royal Infirmary of Edinburgh") {
      tagList( # Cath cases Golden Jubilee
        h3("Weekly visits to the cardiac catheterization labs at the Royal Infirmary of Edinburgh"),
        actionButton("btn_cardio_modal", "Data source: Royal Infirmary of Edinburgh", icon = icon('question-circle')),
        plot_box("2020 compared with 2019", "cath_rie_overall"),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by sex", "cath_rie_sex_var",
                     "Weekly number of cases by sex", "cath_rie_sex_tot"),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by age group", "cath_rie_age_var",
                     "Weekly number of cases by age group", "cath_rie_age_tot"),
        plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2018-2019 by type of intervention", "cath_rie_type_var",
                     "Weekly number of cases by type of intervention", "cath_rie_type_tot")
      )
      
    } else if (input$area_cardio_select == "Golden Jubilee Hospital") {
    tagList( # Cath cases Golden Jubilee
      h3("Weekly visits to the cardiac catheterization labs at the Golden Jubilee Hospital"),
      actionButton("btn_cardio_modal", "Data source: Golden Jubilee", icon = icon('question-circle')),
      plot_box("2020 compared with 2019", "cath_gj_overall"),
      plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2019 by sex", "cath_gj_sex_var",
                   "Weekly number of cases by sex", "cath_gj_sex_tot"),
      plot_cut_box("Percentage change in cases compared with the
                   corresponding time in 2019 by age group", "cath_gj_age_var",
                   "Weekly number of cases by age group", "cath_gj_age_tot"),
      plot_cut_box("Percentage change in cases compared with the 
                   corresponding time in 2019 by admission type", "cath_adm_gj_var",
                   "Weekly number of cases by admission type", "cath_adm_gj_tot")
    )
    }
    } else if (input$measure_cardio_select == "aye") {
      tagList(# A&E attendances (cardiovascular only)
        tags$em("Please note that due to limitations in diagnosis recording in the A&E datamart, the 
                         figures reported for cardiology offer only a very rough indication of cardiovascular 
                         attendances and do not represent the exact figures for cardiovascular attendances at 
                         Emergency Departments. Part of the data quality issues is that the data on cardiovascular 
                         attendance only includes part or no data for NHS Ayrshire and Arran, NHS Fife, 
                         NHS Forth Valley and NHS Lothian."),
        h3("Weekly cardiovascular A&E attendances in Scotland"),
        actionButton("btn_cardio_modal", "Data source: PHS AE2 Datamart", icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "ae_cardio_overall"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances in Scotland compared with the corresponding
                     time in 2018-2019 by age group", "ae_cardio_age_var",
                     "Weekly number of cardiovascular A&E attendances in Scotland by age group", "ae_cardio_age_tot"),
        plot_cut_box("Percentage change in cardiovascular A&E attendances in Scotland compared with the corresponding
                     time in 2018-2019 by SIMD quintile", "ae_cardio_dep_var",
                     "Weekly number of cardiovascular A&E attendances in Scotland by SIMD quintile", "ae_cardio_dep_tot")
      )
    } else if (input$measure_cardio_select == "drug_presc") {
      tagList(# Prescribing - items dispensed
        h3(paste0("Weekly number of cardiovascular drug items prescribed in ", input$geoname_cardio)),
        actionButton("btn_cardio_modal", "Data source: Prescribing", icon = icon('question-circle')),
        plot_box("2020 compared with 2018-2019 average", "prescribing_all"),
        plot_cut_box(paste0("Percentage change in cardiovascular drug prescriptions in ", input$geoname_cardio, " compared with the corresponding
                     time in 2018-2019 by drug group"), "cardio_drugs_var",
                     paste0("Weekly number of cardiovascular drug prescriptions in ", input$geoname_cardio, " by age group"), "cardio_drugs_tot")
      )
    }
})

###############################################.
## Charts ----
###############################################.
#Cath labs RIE charts
output$cath_rie_overall <- renderPlotly({
  plot_overall_chart(rie_cath %>% filter(groups == "Angiography"), "cath", area = F)})
output$cath_rie_devices <- renderPlotly({
  plot_overall_chart(rie_cath %>% filter(groups == "Devices"), "cath", area = F)})
output$cath_rie_pci <- renderPlotly({
  plot_overall_chart(rie_cath %>% filter(groups == "PCI"), "cath", area = F)})

output$cath_rie_sex_var <- renderPlotly({plot_trend_chart(rie_cath %>% filter(groups == "Angiography"  & type == "sex"), 
                                                          pal_sex)})
output$cath_rie_sex_tot <- renderPlotly({plot_trend_chart(rie_cath %>% filter(groups == "Angiography" & type == "sex"), 
                                                          pal_sex, type = "total", data_name = "cath")})
output$cath_rie_age_var <- renderPlotly({plot_trend_chart(rie_cath %>% filter(groups == "Angiography"  & type == "age"), 
                                                          pal_2ages)})
output$cath_rie_age_tot <- renderPlotly({plot_trend_chart(rie_cath %>% filter(groups == "Angiography"  & type == "age"), 
                                                          pal_2ages, type = "total", data_name = "cath")})

output$cath_rie_type_var <- renderPlotly({
  plot_trend_chart(rie_cath %>% filter(category == "All") %>% 
                     select(-category) %>% rename(category = groups), pal_sex)})
output$cath_rie_type_tot <- renderPlotly({
  plot_trend_chart(rie_cath %>% filter(category == "All") %>% 
                     select(-category) %>% rename(category = groups), 
                   pal_sex, type = "total", data_name = "cath")})


# Cath labs Golden Jubilee charts
output$cath_gj_overall <- renderPlotly({plot_overall_chart(gjub_cath, "cath", area = F)})
output$cath_adm_gj_var <- renderPlotly({plot_trend_chart(gjub_cath, pal_sex)})
output$cath_adm_gj_tot <- renderPlotly({plot_trend_chart(gjub_cath, pal_sex, type = "total", data_name = "cath")})
output$cath_gj_sex_var <- renderPlotly({plot_trend_chart(gjub_monthly %>% filter(type == "sex"), 
                                                         pal_sex, period = "monthly")})
output$cath_gj_sex_tot <- renderPlotly({plot_trend_chart(gjub_monthly %>% filter(type == "sex"), 
                                                         pal_sex, type = "total", data_name = "cath", period = "monthly")})
output$cath_gj_age_var <- renderPlotly({plot_trend_chart(gjub_monthly %>% filter(type == "age"), 
                                                         pal_2ages, period = "monthly")})
output$cath_gj_age_tot <- renderPlotly({plot_trend_chart(gjub_monthly %>% filter(type == "age"), 
                                                         pal_2ages, type = "total", data_name = "cath", period = "monthly")})

output$angio_gj_overall <- renderPlotly({
  
  #Text for tooltip
  tooltip_trend <- c(paste0("Month: ", format(gjub_monthly$month_date, "%b %y"),
                            "<br>", "Angiographies: ", gjub_monthly$n))
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Number of angiographies"
  
  plot_ly(data=gjub_monthly, x=~month_date,  y = ~n) %>% 
    add_trace(type = 'scatter', mode = 'lines', line = list(color = "black"),
              text=tooltip_trend, hoverinfo="text") %>% 
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots) %>% 
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  
})

output$angio_gj_perc <- renderPlotly({
  
  #Text for tooltip
  tooltip_fem <- c(paste0("Month: ", format(gjub_monthly$month_date, "%b %y"),
                          "<br>", "Angiographies: ", gjub_monthly$n_female,
                            "<br>", "Percentage: ", gjub_monthly$percent_female))
  
  tooltip_70 <- c(paste0("Month: ", format(gjub_monthly$month_date, "%b %y"),
                          "<br>", "Angiographies: ", gjub_monthly$n70,
                          "<br>", "Percentage: ", gjub_monthly$percent_70))
  
  
  #Modifying standard layout
  yaxis_plots[["title"]] <- "Percentage over the total"
  
  plot_ly(data=gjub_monthly, x=~month_date) %>% 
    add_trace(y= ~percent_female, type = 'scatter', mode = 'lines', line = list(color = "blue"),
              text=tooltip_fem, hoverinfo="text", name = "% Females") %>% 
    add_trace(y= ~percent_70, type = 'scatter', mode = 'lines', line = list(color = "brown"),
              text=tooltip_70, hoverinfo="text", name = "% aged 70+") %>% 
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) 
  
})

# A&E Cardio charts
output$ae_cardio_overall <- renderPlotly({plot_overall_chart(ae_cardio, data_name = "aye", area = "All")})
output$ae_cardio_age_var <- renderPlotly({plot_trend_chart(ae_cardio, pal_sex, c("age", "all"), data_name = "aye",tab = "cardio")})
output$ae_cardio_age_tot <- renderPlotly({plot_trend_chart(ae_cardio, pal_sex, c("age", "all"), "total", "aye", tab = "cardio")})
output$ae_cardio_dep_var <- renderPlotly({plot_trend_chart(dataset = ae_cardio, pal_chose = pal_depr, split = "dep", type = "variation", data_name = "aye", tab = "cardio")})
output$ae_cardio_dep_tot <- renderPlotly({plot_trend_chart(ae_cardio, pal_depr, split = "dep", type = "total", data_name = "aye", tab = "cardio")})

# Prescribing charts
output$prescribing_all <- renderPlotly({plot_overall_chart(cardio_drugs %>% filter(area_name == input$geoname_cardio), 
                                                           data_name = "drug_presc", area = "All")})
output$cardio_drugs_var <- renderPlotly({plot_trend_chart(cardio_drugs, pal_con, c("condition"), data_name = "drug_presc", tab = "cardio")})
output$cardio_drugs_tot <- renderPlotly({plot_trend_chart(cardio_drugs, pal_con, c("condition"), "total", data_name = "drug_presc", tab = "cardio")})
###############################################.
## Data downloads ----
###############################################.

overall_cardio_download <- reactive({
  
  # Branching this so that depending on input the right variables and names can be used
  # Cath branch
  if (input$measure_cardio_select == "cath") {
    selection <- c("week_ending", "count", "count_average", "variation")
    new_var_name <- "count_2019"
  }
  # A&E branch
  if (input$measure_cardio_select == "aye") {
    selection <- c("week_ending", "area_name", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }
  # Prescribing
  if (input$measure_cardio_select == "drug_presc") {
    selection <- c("week_ending", "area_name", "count", "count_average", "variation")
    new_var_name <- "average_2018_2019"
  }
  
  # Prep data for download
  switch(
    input$measure_cardio_select,
    "cath" = filter_data(gjub_cath, area = F),
    "aye" = filter_data(ae_cardio, area = F),
    "drug_presc" = filter_data(cardio_drugs, area = F)
  ) %>% 
    select_at(selection) %>% 
    rename(!!new_var_name := count_average) %>% 
    mutate(week_ending = format(week_ending, "%d %b %y"))
})

output$download_cardio_data <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    write_csv(overall_cardio_download(),
              file) } 
)


##END