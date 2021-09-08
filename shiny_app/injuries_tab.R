# Server side for injuries tab
###############################################.
## Modals ----
###############################################.
week_standard <- " are allocated to weeks based on the ISO8601 standard. Following this standard
the year 2020 had 53 weeks while 2018 and 2019 had 52. To allow comparisons, we use 
the 2018-2019 average of week 52 value as a comparator for 2020’s week 53.”"

# Link action button click to modal launch 
observeEvent(input$btn_injuries_modal, 
                showModal(modalDialog(# injury discharges MODAL
                 title = "What is the data source?",
                 p("This tool provides a weekly summary of discharges from hospital with an unintentional injury (Acute Hospitals) 
                    in the recent past, along with historical activity for 
                   comparison purposes. The recent trend data is shown by age group, sex
                   and broad deprivation category (SIMD). This data only include Acute Hospital 
                   Discharges and do not include minor injury units and other small hospitals and 
                   health centres in rural areas that carry out Acute Hospital related activity, 
                   for more information on what sites are included please see this ", 
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/Hospital-Site-List/",
                          "hospital list.",  target="_blank")),
                 p("Discharges", week_standard),
                 p(tags$em("Please note that, due to limitations in diagnosis recording in the A&E datamart, the data are 
                            incomplete for a number of NHS Boards. Thus, the figures reported for injuries-related 
                            Discharges offer only a very approximate indication of Discharges. 
                            Additionally, some NHS Boards have moved to a new recording standard which 
                            has not been fully consolidated in the A&E datamart as yet. As a result, figures for 2020, 
                            even prior to the introduction of lockdown measures, appear somehwat lower when compared to 
                            previous years.")),
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))
###############################################.
## Reactive datasets ----
###############################################.
injury_colour <- reactive({case_when(input$type_select == "age" ~ 1,
            input$type_select == "dep" ~2,
            input$type_select == "injurylocation" ~3,
            input$type_select == "sex" ~	3)
})


###############################################.
## Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
# Show list of area names depending on areatype selected
output$geoname_injuries_ui <- renderUI({
  
  areas_summary <- sort(geo_lookup$areaname[geo_lookup$areatype == ifelse(input$area_injuries_select %notin% 
                                                                            c("Scotland", "Health board", "HSC partnership"), 
                                                                          "Scotland", 
                                                                          input$area_injuries_select)])
  
  selectizeInput("geoname_injuries", label = NULL,
                 choices = areas_summary, selected = "")

})


###############################################.
## Charts ----
###############################################.
###############################################.
# Creating plots for each cut and dataset
# Injury time trend charts

                    
 output$ui_smr01_all_overall <- renderPlotly({plot_overall_injury_chart(ui_smr01_all  %>% filter(area_name == input$geoname_injuries & category=="All"),  var1_chosen = "count", var2_chosen = "count_average", 
                                                                    data_name = "ui_smr01_all" )})
 output$ui_smr01_rta_overall <- renderPlotly({plot_overall_injury_chart(ui_smr01_rta %>% filter(area_name == input$geoname_injuries & category=="All"),  var1_chosen = "count", var2_chosen = "count_average", 
                                                                     data_name = "ui_smr01_rta")})
 output$ui_smr01_poison_overall <- renderPlotly({plot_overall_injury_chart(ui_smr01_poison %>% filter(area_name == input$geoname_injuries & category=="All"),  var1_chosen = "count", var2_chosen = "count_average", 
                                                                     data_name = "ui_smr01_poison")})
 output$ui_smr01_falls_overall <- renderPlotly({plot_overall_injury_chart(ui_smr01_falls %>% filter(area_name == input$geoname_injuries & category=="All"),  var1_chosen = "count", var2_chosen = "count_average", 
                                                                     data_name = "ui_smr01_falls")})
 output$ui_smr01_other_overall <- renderPlotly({plot_overall_injury_chart(ui_smr01_other %>% filter(area_name == input$geoname_injuries & category=="All"),  var1_chosen = "count", var2_chosen = "count_average", 
                                                                   data_name = "ui_smr01_other")})
 output$ui_smr01_assaults_overall <- renderPlotly({plot_overall_injury_chart(ui_smr01_assaults %>% filter(area_name == input$geoname_injuries & category=="All"),  var1_chosen = "count", var2_chosen = "count_average", 
  
                                                                                                                            data_name = "ui_smr01_assaults")})
 output$ui_smr01_all_sex <- renderPlotly({
   plot_trend_chart(ui_smr01_all %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select),
                    type = "variation", data_name = "ui_smr01_all" , tab = "injuries", period = "monthly")})
 output$ui_smr01_rta_sex <- renderPlotly({
   plot_trend_chart(ui_smr01_rta %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), 
                    type = "variation", data_name = "ui_smr01_rta" , tab = "injuries")})
 output$ui_smr01_poison_sex <- renderPlotly({
   plot_trend_chart(ui_smr01_poison %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), 
                    type = "variation",data_name = "ui_smr01_poison" , tab = "injuries",period = "monthly")})
 output$ui_smr01_falls_sex <- renderPlotly({
   plot_trend_chart(ui_smr01_falls %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), 
                    type = "variation",  data_name = "ui_smr01_falls" , tab = "injuries",period = "monthly")})
 output$ui_smr01_other_sex <- renderPlotly({
   plot_trend_chart(ui_smr01_other %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), 
                    type = "variation", data_name = "ui_smr01_other" , tab = "injuries",period = "monthly")})
 output$ui_smr01_assaults_sex <- renderPlotly({
   plot_trend_chart(ui_smr01_assaults %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), 
                    type = "variation", data_name = "ui_smr01_assaults" , tab = "injuries",period = "monthly")})
 
 output$ui_smr01_all_tot <- renderPlotly({
   plot_trend_chart(ui_smr01_all %>% filter(area_name == input$geoname_injuries),pal_inj[[injury_colour()]], c(input$type_select), type = "total", 
                    data_name = "ui_smr01_all", tab = "injuries",period = "monthly")})
 output$ui_smr01_rta_tot <- renderPlotly({
   plot_trend_chart(ui_smr01_rta %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), type = "total", 
                    data_name = "ui_smr01_rta", tab = "injuries",period = "monthly")})
 output$ui_smr01_poison_tot <- renderPlotly({
   plot_trend_chart(ui_smr01_poison %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), type = "total", 
                    data_name = "ui_smr01_poison", tab = "injuries",period = "monthly")})
 output$ui_smr01_falls_tot <- renderPlotly({
   plot_trend_chart(ui_smr01_falls %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), type = "total", 
                    data_name = "ui_smr01_falls", tab = "injuries",period = "monthly")})
 output$ui_smr01_other_tot <- renderPlotly({
   plot_trend_chart(ui_smr01_other %>% filter(area_name == input$geoname_injuries),pal_inj[[injury_colour()]], c(input$type_select), type = "total", 
                    data_name = "ui_smr01_other", tab = "injuries",period = "monthly")})
 output$ui_smr01_assaults_tot <- renderPlotly({
   plot_trend_chart(ui_smr01_assaults %>% filter(area_name == input$geoname_injuries), pal_inj[[injury_colour()]], c(input$type_select), type = "total", 
                    data_name = "ui_smr01_assaults", tab = "injuries",period = "monthly")})
# Adding 'observeEvent' to allow reactive 'area of interest' selection on injuries tab

# Injury reactive drop-down control showing list of area names depending on areatype selected
# The charts and text shown on the app will depend on what the user wants to see
output$injuries_explorer <- renderUI({
  # text for name of charts
  dataset <- case_when(input$measure_injury_select == "ui_smr01_all" ~ "ui_smr01_all" ,
                       input$measure_injury_select == "ui_smr01_rta" ~ "ui_smr01_rta" ,
                       input$measure_injury_select == "ui_smr01_poison" ~ "ui_smr01_poison",
                       input$measure_injury_select == "ui_smr01_falls" ~ "ui_smr01_falls",
                       input$measure_injury_select == "ui_smr01_other" ~ "ui_smr01_other",
                       input$measure_injury_select == "ui_smr01_assaults" ~ "ui_smr01_assaults"
  )
  
  # Charts and rest of UI
  # text for titles of charts
  injury_select <- case_when(input$measure_injury_select == "ui_smr01_all" ~ "All unintentional injuries",
                             input$measure_injury_select == "ui_smr01_rta" ~ "Road traffic accident",
                             input$measure_injury_select == "ui_smr01_poison" ~ "Poisoning",
                             input$measure_injury_select == "ui_smr01_falls" ~ "Falls",
                             input$measure_injury_select == "ui_smr01_other" ~ "Other",
                             input$measure_injury_select == "ui_smr01_assaults" ~ "Assaults")
  # text for titles of charts
  injury_split <- case_when(input$type_select == "age" ~ "Age group",
                            input$type_select == "dep" ~ "Deprivation",
                            input$type_select == "injurylocation" ~ "Injury location",
                            input$type_select == "sex" ~	"Sex")
  
  injury_colour <- case_when(input$type_select == "age" ~ 1,
                             input$type_select == "dep" ~ 2,
                             input$type_select == "injurylocation"~ 3,
                             input$type_select == "sex" ~	3)
 
    tagList( # injuries
      h3(paste0("Monthly admissions for ", injury_select)),
      actionButton("btn_injuries_modal", paste0("Data source: SMR01 "), icon = icon('question-circle')),
      plot_box(paste0("2020 and 2021 compared with 2018-2019 average ",", ",input$geoname_injuries), paste0(dataset,"_overall")),
      plot_box(paste0(paste0("Percentage change in admissions for ", injury_select, " compared with the corresponding time in 2018-2019, by ", injury_split),
                      ", ", input$geoname_injuries), paste0(dataset,"_sex")),
      plot_box(paste0(paste0("Monthly number of admissions for ",injury_select,", by ", injury_split),
                      ", ", input$geoname_injuries), paste0(dataset,"_tot")))
                     #   " compared with average of the corresponding time in 2018 and 2019 by medicine groupings")
                 #paste0("Weekly number of patients starting a new treatment course for selected mental health medicines in ", input$geoname_injuries, "injuries_factor")
}) 

## Data downloads ----
###############################################.

overall_injuries_download <- reactive({
  
  # Branching this so that depending on input the right variables and names can be used
  if (input$measure_injury_select == "ui_smr01_all") {
    selection <- c("week_ending", "count","area_name", "count_average", "variation","category")
    new_var_name <- "average_2018_2019"
  }
  if (input$measure_injury_select == "ui_smr01_rta") {
    selection <- c("week_ending", "count","area_name", "count_average", "variation","category")
    new_var_name <- "average_2018_2019"
  }
  if (input$measure_injury_select == "ui_smr01_poison") {
    selection <- c("week_ending", "count","area_name", "count_average", "variation","category")
    new_var_name <- "average_2018_2019"
  }
  if (input$measure_injury_select == "ui_smr01_falls") {
    selection <- c("week_ending", "count","area_name", "count_average", "variation","category")
    new_var_name <- "average_2018_2019"
  }
  if (input$measure_injury_select == "ui_smr01_other") {
    selection <- c("week_ending", "count","area_name", "count_average", "variation","category")
    new_var_name <- "average_2018_2019"
  }
  if (input$measure_injury_select == "ui_smr01_assaults") {
    selection <- c("week_ending", "count","area_name", "count_average", "variation","category")
    new_var_name <- "average_2018_2019"
  }
  # Prep data for download
  switch(
    input$measure_injury_select,
    "ui_smr01_all" = filter_data(ui_smr01_all, area = F),
    "ui_smr01_rta" = filter_data(ui_smr01_rta, area = F),
    "ui_smr01_poison" = filter_data(ui_smr01_poison, area = F),
    "ui_smr01_falls" = filter_data(ui_smr01_falls, area = F),
    "ui_smr01_other" = filter_data(ui_smr01_other, area = F),
    "ui_smr01_assaults" = filter_data(ui_smr01_assaults, area = F)) %>% 
    select_at(selection) %>% 
    rename(!!new_var_name := count_average) %>%
    rename(month=week_ending) %>%
    mutate(month = format(month, "%b %y"))
})

output$download_injuries_data <- downloadHandler(
  filename ="injuries_extract.csv",
  content = function(file) {
    write_csv(overall_injuries_download(),
              file) } 
)

###############################################.
## Commentary ----
###############################################.
output$injuries_commentary <- renderUI({
  tagList()
})
##END

