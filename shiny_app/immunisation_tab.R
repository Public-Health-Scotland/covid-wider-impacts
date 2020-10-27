##Server script for immunisations tab

###############################################.
## Modals  ----
###############################################.

# Modal to explain SIMD and deprivation
# Link action button click to modal launch 
observeEvent(input$btn_modal_simd_imm, { showModal(
  modalDialog(
    h5("What is SIMD and deprivation?"),
    p("Children have been allocated to different levels of deprivation based on the small area (data zone) 
      in which they live and the", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD).",
                                           target="_blank"), "score for that area. 
      SIMD scores are based on data for local areas reflecting 38 indicators across 7 domains: 
      income; employment; health; education, skills and training; housing; geographic access; and crime. 
      In this tool we have presented results for children living in different SIMD ‘quintiles’. 
      To produce quintiles, data zones are ranked by their SIMD score then the areas each containing a fifth (20%) 
      of the overall population of Scotland are identified. Children living in the most and least deprived areas 
      that each contain a fifth of the population are assigned to SIMD quintile 1 and 5 respectively."),
    size = "l", 
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
    ) }) 

# Pop-up modal explaining source of data
observeEvent(input$btn_immune_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("The information shown on the numbers of children eligible for, and receiving, routine preschool immunisations is taken from the ",
                 tags$a(href="https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?ID=4&SubID=12",
                        "Scottish Immunisation and Recall System (SIRS).", target="_blank")),
               p(tags$a(href="https://publichealthscotland.scot/",
                        "Public Health Scotland (PHS)", target="_blank"),
                 " routinely receives quarterly data extracts from SIRS for the purpose of producing and ",
                 tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
                        "publishing", target="_blank"),
                " immunisation uptake rates. To allow the more rapid monitoring of the impact of Covid-19 on childhood immunisation uptake rates presented here, PHS is also currently extracting a sub-set of data from SIRS each month."),
               p("SIRS is an electronic system used by all NHS Boards in Scotland. The system facilitates the invitation of children when a scheduled immunisation is due. When a child receives an immunisation, relevant information is returned to administrative staff in the NHS Board child health department. The administrative staff then update the child’s SIRS record accordingly."),
               p("After a child becomes eligible for an immunisation, it takes some time for them to attend their appointment, and for a record of the immunisation provided to subsequently be entered into the SIRS system. We have allowed a 6-week window for this, therefore each release of this page will report on children becoming eligible for an immunisation up to 6 weeks before the date the data were extracted for analysis."),
               p("Although children will generally have their immunisation, and their SIRS record updated accordingly, within 6 weeks of becoming eligible, the pandemic may have influenced not only how quickly eligible children receive their immunisations, but also how long it takes for children’s SIRS records to be updated once an immunisation has been given. Any disruption to SIRS data entry may vary across NHS Boards. Data shown for the most recent cohorts of children will therefore not be fully complete in SIRS and should be viewed as provisional. The uptake rates for each cohort will be refreshed with more up-to-date data every 4 weeks, and rates for the most recent cohorts may increase slightly as relevant records are updated in SIRS."),
               p("Through this tool, data on immunisation uptake are provided for individual NHS Boards and Health and Social Care Partnerships (HSCPs).  Data by Board are reported by NHS Board of treatment as recorded on SIRS. Due to the reconfiguration of NHS Board boundaries, a small proportion of records on SIRS do not reflect the current configuration of NHS Boards.  In these instances, children have been assigned to an NHS Board of treatment based on their home postcode. Data by HSCP (available through the data download button) are reported by HSCP of residence, derived from home postcode recorded on SIRS. As children may receive their immunisations outwith their Board of residence, or have missing postcode information recorded on SIRS, this means that there are some small differences in figures for specific NHS Boards and their corresponding HSCPs."),
               p("Some NHS Boards and HSCPs have small numbers of children eligible for immunisation each week. Uptake rates based on these small numbers are prone to fluctuation, and it is important to bear this in mind when interpreting uptake rates."),
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
## Reactive controls  ----
###############################################.

# Immunisation reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_immun <- renderUI({
  #Lists areas available in   
  areas_summary_immun <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_immun])
  
  if (input$geotype_immun == "Scotland"){areas_summary_immun <- c("Scotland")}
  selectizeInput("geoname_immun", label = NULL, choices = areas_summary_immun, selected = "")
})

# Reactive dataset for flextable filter on geographical area and dose
filter_table_data_immun <- function(dataset, dose){
  dataset %>% filter(area_name == input$geoname_immun & str_detect(immunisation,dose))
}

###############################################.
## Charts ----
###############################################.

# Creating plots for each dataset
#run chart function to generate s curves  
output$immun_6in1_scurve_dose1 <- renderPlotly({plot_scurve(dataset=six_alldose, age_week = "8", dose= "dose 1")})
output$immun_6in1_scurve_dose2 <- renderPlotly({plot_scurve(six_alldose, age_week = "12", dose="dose 2")})
output$immun_6in1_scurve_dose3 <- renderPlotly({plot_scurve(six_alldose, age_week = "16", dose= "dose 3" )})
output$immun_mmr_scurve_dose1 <- renderPlotly({plot_scurve(mmr_alldose, age_week = "1", dose= "dose 1" )})
output$immun_mmr_scurve_dose2 <- renderPlotly({plot_scurve(mmr_alldose, age_week = "3", dose= "dose 2" )})

#run function to generate data tables linked to s-curves  
output$immun_6in1_table_dose1 <- renderUI({immune_table(sixtable,dose="dose 1", age_week = 8)})
output$immun_6in1_table_dose2 <- renderUI({immune_table(sixtable, dose="dose 2", age_week = 12)})
output$immun_6in1_table_dose3 <- renderUI({immune_table(sixtable, dose="dose 3", age_week = 16)})
output$immun_mmr_table_dose1 <- renderUI({immune_table(mmrtable, dose="dose 1", age_week = 1)}) #age week 1 doesn't really make sense as given to children at 1 year
output$immun_mmr_table_dose2 <- renderUI({immune_table(mmrtable, dose="dose 2", age_week = 3)}) #age week 3 doesn't really make sense as given to children at 3 years and 4 month

#run function to generate SIMD bar charts relative changes (only available at scotland level)
output$imm_6in1_simd_chan_dose1 <- renderPlotly({plot_imm_simd(dataset=six_simd_dose1, age_week = "8", dose= "dose 1",
                                                               var_plot = "week12_abs_diff")})
output$imm_6in1_simd_chan_dose2 <- renderPlotly({plot_imm_simd(dataset=six_simd_dose2, age_week = "12", dose= "dose 2",
                                                               var_plot = "week16_abs_diff")})
output$imm_6in1_simd_chan_dose3 <- renderPlotly({plot_imm_simd(dataset=six_simd_dose3, age_week = "16", dose= "dose 3",
                                                               var_plot = "week20_abs_diff")})
output$imm_mmr_simd_chan_dose1 <- renderPlotly({plot_imm_simd(dataset=mmr_simd_dose1, age_week = "1", dose= "dose 1",
                                                              var_plot = "week57_abs_diff")})
output$imm_mmr_simd_chan_dose2 <- renderPlotly({plot_imm_simd(dataset=mmr_simd_dose2, age_week = "3", dose= "dose 2",
                                                              var_plot = "week178_abs_diff")})

#run function to generate SIMD bar charts absolute uptake (only available at scotland level)
output$imm_6in1_simd_tot_dose1 <- renderPlotly({plot_imm_simd(dataset=six_simd_dose1, age_week = "8", dose= "dose 1", 
                                                              var_plot = "uptake_12weeks_percent", base_var = "baseline_12weeks")})
output$imm_6in1_simd_tot_dose2 <- renderPlotly({plot_imm_simd(dataset=six_simd_dose2, age_week = "12", dose= "dose 2", 
                                                              var_plot = "uptake_16weeks_percent", base_var = "baseline_16weeks")})
output$imm_6in1_simd_tot_dose3 <- renderPlotly({plot_imm_simd(dataset=six_simd_dose3, age_week = "16", dose= "dose 3", 
                                                              var_plot = "uptake_20weeks_percent", base_var = "baseline_20weeks")})
output$imm_mmr_simd_tot_dose1 <- renderPlotly({plot_imm_simd(dataset=mmr_simd_dose1, age_week = "1", dose= "dose 1", 
                                                             var_plot = "uptake_57weeks_percent", base_var = "baseline_57weeks")})
output$imm_mmr_simd_tot_dose2 <- renderPlotly({plot_imm_simd(dataset=mmr_simd_dose2, age_week = "3", dose= "dose 2", 
                                                             var_plot = "uptake_178weeks_percent", base_var = "baseline_178weeks")})

###############################################.
## Reactive layout  ----
###############################################.

# The charts and text shown on the app will depend on what the user wants to see
output$immunisation_explorer <- renderUI({
  
  # text for titles of cut charts
  immune_title <- case_when(input$measure_select_immun == "sixin_dose1" ~ paste0("Uptake of first dose of 6-in-1 vaccine (offered to children at 8 weeks of age): ",
                                                                                 input$geoname_immun),
                            input$measure_select_immun == "sixin_dose2" ~ paste0("Uptake of second dose 6-in-1 vaccine (offered to children at 12 weeks of age): ", input$geoname_immun),
                            input$measure_select_immun == "sixin_dose3" ~ paste0("Uptake of third dose 6-in-1 vaccine (offered to children at 16 weeks of age): ", input$geoname_immun),
                            input$measure_select_immun == "mmr_dose1" ~ paste0("Uptake of first dose MMR vaccine (offered to children at 12-13 months of age): ", input$geoname_immun),
                            input$measure_select_immun == "mmr_dose2" ~ paste0("Uptake of second dose MMR vaccine (offered to children at 3 years 4 months of age): ", input$geoname_immun))
  immune_subtitle <-  paste0("Figures based on data extracted from SIRS on ",immunisation_extract_date)
  
  # text for SIMD titles of cut charts - SIMD only available at scotland level so no need for variable geography
  immune_simd_chan_title <- case_when(input$measure_select_immun == "sixin_dose1" ~ "Change in uptake of first dose of 6-in-1 vaccine by 12 weeks of age by deprivation: Scotland (Compared to baseline of children turning 8 weeks in 2019)",
                                      input$measure_select_immun == "sixin_dose2" ~ "Change in uptake of second dose of 6-in-1 vaccine by 16 weeks of age by deprivation: Scotland (Compared to baseline of children turning 12 weeks in 2019)",
                                      input$measure_select_immun == "sixin_dose3" ~ "Change in uptake of third dose of 6-in-1 vaccine by 20 weeks of age by deprivation: Scotland (Compared to baseline of children turning 16 weeks in 2019)",
                                      input$measure_select_immun == "mmr_dose1" ~ "Change in uptake of first dose MMR vaccine by 13 months of age by deprivation: Scotland (Compared to baseline of children turning 12-13 months in 2019)",
                                      input$measure_select_immun == "mmr_dose2" ~ "Change in uptake of second dose MMR vaccine by 3 years 5 months of age by deprivation: Scotland (Compared to baseline of children turning 3 years and 4 months in 2019)")
  
  immune_simd_tot_title <- case_when(input$measure_select_immun == "sixin_dose1" ~ "Uptake of first dose of 6-in-1 vaccine by 12 weeks of age by deprivation: Scotland",
                                     input$measure_select_immun == "sixin_dose2" ~ "Uptake of second dose of 6-in-1 vaccine by 16 weeks of age by deprivation: Scotland",
                                     input$measure_select_immun == "sixin_dose3" ~ "Uptake of third dose of 6-in-1 vaccine by 20 weeks of age by deprivation",
                                     input$measure_select_immun == "mmr_dose1" ~ "Uptake of first dose MMR vaccine by 13 months of age by deprivation",
                                     input$measure_select_immun == "mmr_dose2" ~ "Uptake of second dose MMR vaccine by 3 years 5 months of by age deprivation")
  
  
  # Intro paragraph within imumunisation tab
  intro_6in1 <- p("Immunisation protects children against certain serious infections.  It is important that children ",
                  tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
                         "continue to receive their routine immunisations during the Covid-19 pandemic", target="_blank"),".",
                  "Public Health Scotland and Scottish Government have produced a range of communications reminding parents that the NHS is still open for childhood immunisations, signposting parents to up to date advice via ",
                  tags$a(href="https://twitter.com/NHSImmuniseScot"," Immunise Scotland ", target="_blank"),
                  " and ",tags$a(href="https://www.nhsinform.scot/immunisation","NHS inform", target="_blank"),".")
  
  #Additional commentart/meta data to appear on immunisation tab
  commentary_6in1 <-  tagList(p("All preschool children are offered a total of five immunisation appointments as they reach the following ages: 8, 12, and 16 weeks; 12-13 months; and 3 years and 4 months of age. Multiple immunisations are offered at each appointment. Here, for simplicity, we have just shown the uptake of one of the immunisations offered at each appointment. The charts show the progression of uptake of the relevant immunisation as children age. The data tables provide the uptake rates at three specific time-points. Data is shown for children who have become eligible for immunisation during the pandemic (from March 2020 onwards). Data is also shown for children who became eligible for immunisation before the pandemic (in 2019 and in January and February 2020) for comparison."),
                              p("After a child becomes eligible for an immunisation, it takes time for them to attend their appointment, and for a record of the immunisation provided to subsequently be entered into the SIRS system. We have allowed a 6-week window for this, therefore each release of this page will report on children becoming eligible for an immunisation up to 6 weeks before the date the data were extracted for analysis. Although children will generally have their immunisation, and their SIRS record updated accordingly, within 6 weeks of becoming eligible, the pandemic may have influenced not only how quickly eligible children receive their immunisations, but also how long it takes for children’s SIRS records to be updated once an immunisation has been given. Any disruption to SIRS data entry may vary across NHS Boards. Data shown for the most recent cohorts of children will therefore not be fully complete in SIRS and should be viewed as provisional. The uptake rates for each cohort will be refreshed with more up-to-date data every 4 weeks, and rates for the most recent cohorts may increase slightly as relevant records are updated in SIRS."),
                              p("On this page, data is shown for Scotland and for NHS Board areas. Data for the Island Boards are included within the Scotland total however, due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland, and NHS Western Isles are presented separately for monthly cohorts in the table only. For the second dose of MMR vaccine at the 3 year 4 months appointment specifically, results for NHS Grampian are not shown separately, and NHS Grampian is excluded from the ‘Scotland’ totals. This is because children in NHS Grampian are offered the second dose of MMR vaccine at 4 years of age rather than 3 years 4 months. Separate figures on uptake of the second dose of MMR vaccine from age 4 years are available for NHS Grampian only through the data download button at the top of the page. Data for individual Health and Social Care Partnerships are also provided through the data download button at the top of the page."),
                              p("Some NHS Boards and HSCPs have small numbers of children eligible for immunisation each week. Uptake rates based on these small numbers are prone to fluctuation, and it is important to bear this in mind when interpreting uptake rates.")
                              )
  
  depr_imm <- tagList(
    p("The deprivation chart on the left shows the immunisation uptake for children becoming eligible for their immunisation during the Covid-19 pandemic, compared to those who became eligible in 2019, at all Scotland level. Early uptake achieved by 4 weeks after the children became eligible for their immunisation is considered, as this indicator is available for the most recent cohorts of children as well as the baseline 2019 cohort. The early uptake rates are shown for children living in areas with different levels of deprivation."),
    p("The deprivation chart on the right shows the change in early uptake for children becoming eligible for their immunisation during the Covid-19 pandemic, compared to those who became eligible in 2019.  Again, results are shown for children living in areas with different levels of deprivation. So, for example, if early uptake for children becoming eligible for an immunisation in 2019 and in March 2020 was 80% and 84% respectively, this would be shown on the ‘change’ chart as a 4% absolute increase in early uptake for children becoming eligible in March 2020. The deprivation data download (available through the button above the ‘change’ chart) also provides the relative change (5% in this example) as this allows an easier comparison across deprivation groups if the baseline level of uptake varies between groups.")
  )
  
  # Function to create common layout to all immunisation charts
  imm_layout <- function(s_plot, s_table, simd_tot_plot, simd_chan_plot, age_def = "") {
    tagList(fluidRow(column(12, renderUI(intro_6in1),
                            h4(paste0(immune_title)),
                            p(immune_subtitle))),
            fluidRow(column(6,br(), br(),
                            withSpinner(plotlyOutput(s_plot)),
                            p(age_def)),
                     column(6, uiOutput(s_table))),
            fluidRow(column(12, renderUI(commentary_6in1))),
            if (input$geotype_immun == "Scotland"){
              tagList(fluidRow(column(6, h4(paste0(immune_simd_tot_title))),
                               column(6, h4(paste0(immune_simd_chan_title))),
                               column(6,
                                      actionButton("btn_modal_simd_imm", "What is SIMD and deprivation?",
                                                   icon = icon('question-circle'))),
                               column(6,
                                      downloadButton('download_imm_simd_data', 'Download deprivation data'))),
                      fluidRow(column(6, br(), withSpinner(plotlyOutput(simd_tot_plot))),
                               column(6, br(), withSpinner(plotlyOutput(simd_chan_plot))),
                               depr_imm
                      )
              ) #tagList from if statement
            }
    ) #tagList barcket
  }
  
  # Specify items to display in immunisation ui based on step 2 selection 
  if (input$measure_select_immun == "sixin_dose1") {
    imm_layout(s_plot = "immun_6in1_scurve_dose1", s_table = "immun_6in1_table_dose1", 
               simd_tot_plot = "imm_6in1_simd_tot_dose1", simd_chan_plot = "imm_6in1_simd_chan_dose1")
  }  else if (input$measure_select_immun == "sixin_dose2"){
    imm_layout(s_plot = "immun_6in1_scurve_dose2", s_table = "immun_6in1_table_dose2", 
               simd_tot_plot = "imm_6in1_simd_tot_dose2", simd_chan_plot = "imm_6in1_simd_chan_dose2")
  }  else if (input$measure_select_immun == "sixin_dose3"){
    imm_layout(s_plot = "immun_6in1_scurve_dose3", s_table = "immun_6in1_table_dose3", 
               simd_tot_plot = "imm_6in1_simd_tot_dose3", simd_chan_plot = "imm_6in1_simd_chan_dose3")
  }  else if (input$measure_select_immun == "mmr_dose1"){
    imm_layout(s_plot = "immun_mmr_scurve_dose1", s_table = "immun_mmr_table_dose1", 
               simd_tot_plot = "imm_mmr_simd_tot_dose1", simd_chan_plot = "imm_mmr_simd_chan_dose1",
               age_def = "12 months defined as 53 weeks")
  } else if (input$measure_select_immun == "mmr_dose2"){
    imm_layout(s_plot = "immun_mmr_scurve_dose2", s_table = "immun_mmr_table_dose2", 
               simd_tot_plot = "imm_mmr_simd_tot_dose2", simd_chan_plot = "imm_mmr_simd_chan_dose2", 
               age_def = "3 year 4 months defined as 174 weeks")
  }
  
}) #close immunisation_explorer function

###############################################.
## Data downloads ----
###############################################.

# For the charts at the moment the data download is for the overall one,
# need to think how to allow downloading for each chart
# Reactive dataset that gets the data the user is visualisaing ready to download
imm_data_download <- reactive({
  
  if (input$measure_select_immun == "mmr_dose2" & input$geoname_immun == "NHS Grampian") {
    mmrtable_dose2_gramp %>%
      select(immunisation, area_name, time_period_eligible, denominator, starts_with("uptake"))  %>% 
      rename(cohort = time_period_eligible)
  } else {
    
    data_down <- switch(
      input$measure_select_immun,
      # for data download filter on dose for table appearing in the app
      "sixin_dose1" = filter(sixtable,str_detect(immunisation,"dose 1")),
      "sixin_dose2" = filter(sixtable,str_detect(immunisation,"dose 2")),
      "sixin_dose3" = filter(sixtable,str_detect(immunisation,"dose 3")),
      "mmr_dose1" = filter(mmrtable,str_detect(immunisation,"dose 1")),
      "mmr_dose2"= filter(mmrtable,str_detect(immunisation,"dose 2"))) %>% 
      select(-cohort) %>% 
      rename(cohort = time_period_eligible) %>% 
      mutate_at(vars(contains("percent")), ~format(., digits=1, nsmall=1))#forcing variables to show one decimal digit.
    
    if (input$measure_select_immun %in% "sixin_dose1") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_12weeks_num, uptake_12weeks_percent,
               uptake_24weeks_num, uptake_24weeks_percent, uptake_tot_num, uptake_tot_percent)
    } else if (input$measure_select_immun %in% "sixin_dose2") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_16weeks_num, uptake_16weeks_percent,
               uptake_28weeks_num, uptake_28weeks_percent, uptake_tot_num, uptake_tot_percent)
    } else if (input$measure_select_immun %in% "sixin_dose3") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_20weeks_num, uptake_20weeks_percent,
               uptake_32weeks_num, uptake_32weeks_percent,uptake_tot_num, uptake_tot_percent)
    } else if (input$measure_select_immun %in% "mmr_dose1") {
      data_down <- data_down %>%
        select(immunisation, area_name, cohort, denominator,
               uptake_13m_num, uptake_13m_percent,
               uptake_16m_num, uptake_16m_percent, uptake_tot_num, uptake_tot_percent)
    } else if (input$measure_select_immun %in% "mmr_dose2") {
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
    input$measure_select_immun,
    "sixin_dose1" = six_simd_dose1,
    "sixin_dose2" = six_simd_dose2,
    "sixin_dose3" = six_simd_dose3,
    "mmr_dose1" = mmr_simd_dose1,
    "mmr_dose2"= mmr_simd_dose2) %>% 
    select(-cohort) %>% 
    rename(cohort = time_period_eligible, deprivation_quintile = simdq)
  
  if (input$measure_select_immun %in% "sixin_dose1") {
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

  } else   if (input$measure_select_immun %in% "sixin_dose2") {
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
    
  } else   if (input$measure_select_immun %in% "sixin_dose3") {
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
  } else   if (input$measure_select_immun %in% "mmr_dose1") {
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
    
  } else   if (input$measure_select_immun %in% "mmr_dose2") {
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

###############################################.
## Immunisation Commentary tab content  ----
## Add any additional commentary in reverse date order.  Can only use jump to data action button once.
###############################################.

output$immun_commentary_section <- renderUI({
  tagList(
    bsButton("jump_to_immunisation",label = "Go to data"), #this button can only be used once
    h2("Immunisations - 4th November 2020"),
    p("In this release of information on uptake of pre-school immunisations data have been updated to include children who became eligible until early September."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during the Covid-19 pandemic. Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"),"."),
    h2("Immunisations - 7th October 2020"),
    p("In this release of information on uptake of pre-school immunisations data have been updated to include children who became eligible until early August. The overall trends described in the commentary for 2 September 2020 below continue to apply."),
    h2("Immunisations - 2nd September 2020"),
    p("Information on uptake of pre-school immunisations was updated in this tool on 2 September. The updated data show that uptake of pre-school immunisations for children who became eligible during March 2020 was maintained at a similar level to that seen before the Covid-19 pandemic (children becoming eligible in 2019 and early 2020). Early uptake for children becoming eligible for their immunisation more recently (April 2020 through to early July 2020) has increased, and is now noticeably higher than that seen before the pandemic."),
    p("The data also show that the increase in early uptake of immunisations seen from April 2020 onwards has been seen for children from all deprivation levels. For the 3 doses of the 6-in-1 immunisation, the recent increase in early uptake has been highest in children from the most deprived areas, resulting in a reduction in inequality in early uptake for these immunisations. For the 2 doses of the MMR immunisation, the recent increase in early uptake has been broadly similar across deprivation groups."),
    p("As discussed in the previous commentary below, there are a number of likely reasons for the recent improvement in early uptake of pre-school immunisations. These include increased awareness among parents of the importance of immunisation reinforced by national communications to encourage attendance, as well as local communications and new processes introduced in response to the pandemic. For example, immunisation teams in some NHS Boards have recently been phoning parents/carers shortly before the day of appointment to ensure families are free of symptoms of Covid-19 before attending, reassure them, and answer questions."),
    p("Although recent improvements in early immunisation uptake rates are evident, often among children living in the most deprived areas in particular, it is too soon to determine whether this early improvement will translate into improved final uptake and a reduction in the inequalities gap when measured at later ages.  Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"), "."),
    h2("Immunisations - 12th August 2020"),
    p("Information on uptake of pre-school immunisations was updated in this tool on 12 August (and new information was added to the data download function on uptake in Health and Social Care Partnerships and in the Island NHS Boards).  The updated data show that uptake of pre-school immunisations for children who became eligible during March 2020 was maintained at a similar level to that seen before the Covid-19 pandemic (children becoming eligible in 2019 and early 2020).  Early uptake for children becoming eligible for their immunisation more recently (April 2020 through to early June 2020) has increased, and is now noticeably higher than that seen before the pandemic."),
    p("New information on uptake of pre-school immunisations for children living in areas with different levels of deprivation (Scotland level only) was also added to this tool on 12 August.  Early uptake (achieved by 4 weeks after the children became eligible for their immunisation) is considered, as this indicator is available for the most recent cohorts of children as well as the baseline 2019 cohort. The data show that before the Covid-19 pandemic, children living in the most deprived areas of Scotland were less likely to have received their pre-school immunisations within 4 weeks of becoming eligible than children living in the least deprived areas."),
    p("The new data show that the increase in early uptake of immunisations seen from April 2020 onwards has been seen for children from all deprivation levels.  For the 3 doses of the 6-in-1 immunisation, the recent increase in early uptake has been highest in children from the most deprived areas, resulting in a reduction in inequality in early uptake for these immunisations.  For the 2 doses of the MMR immunisation, the recent increase in early uptake has been broadly similar across deprivation groups."),
    p("As discussed in the previous commentary below, there are a number of likely reasons for the recent improvement in early uptake of pre-school immunisations.  These include increased awareness among parents of the importance of immunisation reinforced by national communications to encourage attendance, as well as local communications and new processes introduced in response to the pandemic. For example, immunisation teams in some NHS Boards have recently been phoning parents/carers shortly before the day of appointment to ensure families are free of symptoms of Covid-19 before attending, reassure them, and answer questions."),
    h2("Immunisations - 8th July 2020"),
    p("On 8 July, information on uptake of the first and second doses of MMR vaccine was added to the tool."),
    p("The first dose of MMR vaccine is offered from 12 months of age at the immunisation appointment scheduled at 12-13 months. Data before the pandemic, for children eligible (turning 12 months) in 2019 show that uptake in Scotland was 65.4% by the time children turned 13 months old. Uptake rates by 13 months were maintained for children eligible in March 2020 and have increased for children eligible in April and early May 2020, with uptake in each of the latest 4 weeks exceeding 75%. This means in April and early May, more children than usual received their immunisation soon after they first became eligible, indicating fewer non-attendances at, or postponements of, scheduled appointments."),
    p("There are a number of likely reasons for this improved early uptake, including increased awareness among parents of the importance of immunisation reinforced by national communications to encourage attendance, as well as local communications and new processes introduced in response to the pandemic. For example, immunisation teams in some NHS Boards have recently been phoning parents/carers shortly before the day of appointment to ensure families are free of symptoms of Covid-19 before attending, reassure them, and answer questions. Although more children received the first dose of MMR immunised by 13 months of age, it is too early to determine whether this will result in any increase in the uptake of the vaccine at 16 months of age, as measured in the tool, or later when measured at the standard reporting age of 2 years in the",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/","routinely published statistics", target="_blank",".")),
    p("The second dose of MMR vaccine is offered at 3 year 4 months. Data before the pandemic, for children eligible in 2019 show that uptake was 52.0% by 3 years 5 months. There was a small decrease in uptake rates by 3 years 5 months for children eligible for immunisation in March 2020 to 49.6%. However, as seen for the first dose of MMR, early uptake rates (by 3 years 5 months) have since increased for children eligible in April and early May 2020, with uptake in each of the latest 3 weeks exceeding 60%."),
    p("This release also includes updated uptake data on each of the doses of the 6-in-1 vaccine, offered at 8, 12 and 16 weeks, to include children eligible in each week in April and early May 2020. Uptake of each of the doses have been maintained throughout the pandemic. For children eligible in April and early May, the pattern of more children than usual receiving their immunisations soon after becoming eligible, is also observed, most notably for the third dose of vaccine, although the effect is less pronounced than was observed for the MMR immunisations. This is because uptake within 4 weeks of becoming eligible is already high for immunisations offered at the earliest ages, as shown in the data before the pandemic. Doses of vaccine which are routinely offered later in schedule of childhood immunisations take longer to reach the high levels of uptake compared to the immunisations offered at the first appointment due to the cumulative effect of missed appointments, and the need to have appropriate intervals between receiving doses of vaccine."),br(),
    h2("Immunisations - 17th June 2020"), 
    p("Information on the uptake of ",
      tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","immunisations that 
             are routinely offered to all preschool children", target="_blank"),
      " has been included in this tool for the first time on 3 June 2020.", br(),
      "Immunisation protects children against many serious infectious diseases including diphtheria, 
      whooping cough, and measles.",
      tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
             "Immunisation services throughout Scotland are continuing during the Covid-19 pandemic",  target="_blank"),".",
      "It is important to maintain the best possible immunisation uptake rates to ensure children 
      remain protected and to prevent a resurgence of these infections.  Including information on 
      childhood immunisation rates in this tool will help us to ensure that immunisation rates remain 
      high throughout the pandemic.",br(),
      "The 6-in-1 vaccine is given to babies at 8, 12 and 16 weeks of age. The vaccine protects against diphtheria, tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and Hepatitis B.",br(),
      "On 3 June 2020, information was provided on the uptake of the first dose of the 6-in-1 vaccine, offered at 8 weeks of age. This showed uptake continues to exceed 90% among children who were due their first dose of the 6-in-1 vaccine in March and early April.",br(),
      "On 17 June, information on the uptake of the second and third doses was added to the tool. The second dose of 6-in-1 vaccine is offered at 12 weeks of age. Data before the pandemic, for children eligible in 2019, show that uptake of the second dose by 16 weeks was 84.5%. Uptake by 16 weeks continues to exceed 80% among children who were due their second dose of the 6-in-1 vaccine in March and early April.",br(),
      "The third dose of 6-in-1 vaccine is offered at 16 weeks of age. Data before the pandemic, for children eligible in 2019, show that uptake of the third dose by 20 weeks was 72.3%. Uptake by 20 weeks continues to exceed 70% among children who were due their third dose of the 6-in-1 vaccine in March and early April.",br(),
      "It is important to note that uptake of the second and third doses take longer to reach 90% and above compared to the first dose, as demonstrated by the data on uptake before the pandemic. This is because some children receive the first dose later than when first offered the vaccine, for example due to missed appointments. As each dose of vaccine is offered 4 weeks apart, missed appointments has a cumulative effect in increasing the time it takes for uptake of the second and third doses to reach and exceed 90%."), br(),
    h2("Immunisations - 3rd June 2020"),
    p("Information on the uptake of ",
      tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","immunisations that 
             are routinely offered to all preschool children", target="_blank"),
      " has been included in this tool for the first time on 3 June 2020.", br(),
      "Immunisation protects children against many serious infectious diseases including diphtheria, 
      whooping cough, and measles.",
      tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
             "Immunisation services throughout Scotland are continuing during the Covid-19 pandemic",  target="_blank"),".",
      "It is important to maintain the best possible immunisation uptake rates to ensure children 
      remain protected and to prevent a resurgence of these infections.  Including information on 
      childhood immunisation rates in this tool will help us to ensure that immunisation rates remain 
      high throughout the pandemic.",br(),
            "On 3 June 2020, information has been provided on the uptake of the first dose of the 6-in-1 
      vaccine, which is offered to children at 8 weeks of age. The vaccine protects against diphtheria, 
      tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and Hepatitis B. 
      Children should also receive a second dose of the vaccine at 12 weeks and a third dose at 16 weeks.",br(),
            "Uptake rates for this immunisation have remained high during the pandemic.  Uptake continues to exceed 90% among
      children who were due their first dose of the 6-in-1 vaccine in March and early April. The recording of data on
      immunisations given by the reporting date will not be fully complete at this stage, particularly for the most recent
      cohorts, so uptake rates are slightly under-reported. In addition, some children will receive the vaccine at a later age,
      for example due to missed or rescheduled appointments, so uptake rates are expected to continue to increase as children age 
      (as shown in the 2019 data provided for comparison)."
          ))
})






#END

