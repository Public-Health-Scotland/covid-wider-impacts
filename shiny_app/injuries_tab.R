# Server side for injuries tab
###############################################.
## Modals ----
###############################################.
# Link action button click to modal launch 
observeEvent(input$btn_injuries_modal, 
                showModal(modalDialog(# injury discharges MODAL
                 title = "What is the data source?",
                 p("These data provide a monthly summary of  the number of admissions to hospital as a result of
                    an unintentional injury or assault since Jan 2020, with data from 2018-2019 for comparison purposes.
                    The recent trend data is shown by age group, sex, injury location and deprivation category (SIMD)."),
                 p("The source of data is the Scottish Morbidity Record 01 (SMR01) database, which holds information on
                    discharges from non-obstetric and non-psychiatric acute hospitals in Scotland.  The information shown
                    does not include minor injury units and other small hospitals and health centres in rural areas that 
                    carry out Acute Hospital related activity, for more information on what sites are included please see this 
                     ", 
                   tags$a(href="https://www.isdscotland.org/Health-Topics/Emergency-Care/Emergency-Department-Activity/Hospital-Site-List/",
                          "hospital list.",  target="_blank")),
                 p("Injury location and type is determined by the admission type and ICD-10 codes applied to the SMR01 record.
                    Details of the included codes for each category are provided in Appendix 1 of the annual PHS publication
                   ‘Unintentional Injuries in Scotland’",
                   tags$a(href="https://publichealthscotland.scot/media/9854/2021-10-26-ui-2021-report.pdf","Appendix 1",  target="_blank")),
                   p("Note the category ‘Assaults’ is an independent category, and is not included in the data on ‘All unintentional injuries’."),
                    p("The term ‘unintentional injury’ is used in preference to ‘accidents’ as the latter implies that events are inevitable
                    and unavoidable whereas a high proportion of these incidents are now regarded as being preventable. The data shown here
                    include events where it is not possible to determine intent from the hospital records, but do not include those that were
                    documented to be self-harm. Many unintentional injuries result do not result in hospital admission, but are treated by the
                    individual, GPs, at Accident and Emergency departments or by a child's parent or carer, and are therefore not represented in this information."),
                 size = "m",
                 easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)"))))
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
# Adding 'observeEvent' to allow reactive 'area of interest' selction on cardio tab
observeEvent(input$measure_injury_select, {
  x <- input$measure_injury_select
  
  if (x == "ui_smr01_all") {
    injuries_label = "Step 2. Select a geography level"
    injuries_choices = c("Scotland", "Health board", "HSC partnership")
    shinyjs::show("geoname_injuries_ui")
    enable("area_injuries_select")
  }
  
  if (x == "ui_smr01_rta") {
    injuries_label = "Step 2. Select a geography level"
    injuries_choices = c("Scotland","Health board")
    shinyjs::show("geoname_injuries_ui")
    enable("area_injuries_select")
  }
  
  if (x == "ui_smr01_poison") {
    injuries_label = "Step 2. Select a geography level"
    injuries_choices = c("Scotland", "Health board")
    shinyjs::show("geoname_injuries_ui")
    enable("area_injuries_select")
  }
  if (x == "ui_smr01_falls") {
    injuries_label = "Step 2. Select a geography level"
    injuries_choices = c("Scotland", "Health board", "HSC partnership")
    shinyjs::show("geoname_injuries_ui")
    enable("area_injuries_select")
  }
  
  if (x == "ui_smr01_other") {
    injuries_label = "Step 2. Select a geography level"
    injuries_choices = c("Scotland", "Health board")
    shinyjs::show("geoname_injuries_ui")
    enable("area_injuries_select")
  }
  if (x == "ui_smr01_assaults") {
    injuries_label = "Step 2. Select a geography level"
    injuries_choices = c("Scotland")
    shinyjs::show("geoname_injuries_ui")
    enable("area_injuries_select")
  } 
  updateSelectInput(session, "area_injuries_select",
                    label = injuries_label,
                    choices = injuries_choices,
                    selected = injuries_choices[1]
  )
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
      actionButton("btn_injuries_modal", paste0("Data source and definitions"), icon = icon('question-circle')),
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
  tagList(bsButton("jump_to_injuries",label = "Go to data"), #this button can only be used once
           h2("Unintentional Injuries and Assaults - Extracted 31st January 2022"), 
           h3("Background"),
          p("The response to the COVID-19 pandemic has had the potential to influence both the occurrence
                   of unintentional injuries and assaults, and how people receive care after such events. Within
                   this dashboard information is presented on the number of such events resulting in an admission
                   to hospital each month from January 2020, along with data from 2018-2018 for comparison. Data
                   on unintentional injuries is split by type (road traffic accidents, poisonings, falls and other),
                   and all data can be explored by age, sex, deprivation (SIMD) and location of event, as well as at
                   Scotland, Health Board and Health and Social Care Partnership (HSCP) level."),
          p("The data shown here include events where it is not possible to determine intent from the hospital
                   records, but do not include those that were documented to be self-harm. Many unintentional injuries
                   result do not result in hospital admission, but are treated by the individual, GPs, at Accident and
                   Emergency departments or by a child's parent or carer, and are therefore not represented in this
                   information. Changes in the number of admissions for injury do not necessarily mean changes in the
                   number of injury events, but may also reflect changes in how injuries are cared for, for example,
                   more people may seek to treat themselves, or may have been less likely to be admitted for less severe
                   injuries at particular times."),
          p("Description of key findings for months in the period January 2020 to June 2021, compared with
                   average admissions for the same months in 2018-2019"),
          p("There was a substantial fall in the number of admissions for unintentional injury in April 2020, with around
                   one-third fewer admissions compared with the average for the same period in 2018-2019. In the subsequent months,
                   numbers gradually increased, and by August 2020 were at a similar level to previous years. There was a further,
                   smaller fall in admissions in November 2020, but since then the overall number has been similar to that seen in 2018-19."),
          p("The most substantial reduction in April 2020 was seen in children and young people aged 5-24 years, among whom admissions 
                   were between a half and two-thirds lower than previous years. This reduction was particularly notable in admissions for falls,
                   which constitute the largest proportion of unintentional injuries. Among adults there was a notable increase in admissions in
                   January 2021, which was contributed to largely by an increase in the number of admissions for falls among those aged 25-64 years."),
          p("Admissions due to injury following a road-traffic accident have been lower each month from April 2020 to May 2021, compared to 2018-2019,
                   with the exceptions of August 2020 and March 2021, when levels were similar. The number of admissions for poisoning has been similar to
                   previous years throughout this period, with the exception of June 2020, when they were 18% higher. In that month the largest number of
                   admissions was among those aged 25-64 years, although the largest proportional increase on previous years was seen in those aged 5-11 years.
                   The number of admissions for assault was lower than in previous years in March to May 2020, and again from October 2020 to February 2021,
                   at other points the number were similar." ),
          p("Across all unintentional injuries and assaults, the largest number of admissions were among people living in the most deprived fifth of areas in Scotland,
                   according to the Scottish Index of Multiple Deprivation (SIMD 1). However, the percentage change in admissions compared with previous years was similar across deprivation groups,
                   or a mixed picture, with the exception of the increase in falls observed in January 2021, when the largest increase (52%) was seen in those living in the least deprived areas."),
          p("The overall number of admissions per month was similar between men and women up to November 2020. The fall in admissions in April 2020 was more substantial among men (-39%) than women (-26%).
             From December 2020 there has been a larger number of admissions among women, and these have been similar to or higher than previous years, most notably in January 2021, when they were 19% higher
             among women, which was largely attributable to an increase in falls. For men there was a change in the number of admissions for poisoning in May and June 2020, when they were around 20% higher
             than previous years."),
          p("Across the period from May 2020 to June 2021 there was around a 10% to 25% increase in admissions due to injuries occurring in the home, and a reduction in admissions for injuries in other
             and undisclosed locations, compared with 2018-2019. ")
          
          )})
##END

