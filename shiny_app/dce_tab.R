# Server side for dce tab 

###############################################.
## Modals ----
###############################################.
# Pop-up modal explaining source of data

###############################################.
## Reactive datasets ----
###############################################.


dce_data_main <- reactive({
  
  dce_data %>% filter(area == input$geoname_dce, site == input$dce_type, stage == input$dce_stage)
  
})

dce_data_main2 <- reactive({
  
  dce_data %>% filter(region == input$geoname_dce, site == input$dce_type, stage == input$dce_stage)
  
})

dce_data_main3 <- reactive({
  if(input$geoname_dce != "Scotland"){ 
    dce_data %>% filter(area == input$geoname_dce, site == input$dce_type, region != "Scotland")
  } else{
    dce_data %>% filter(area == input$geoname_dce, site == input$dce_type)
  }
})


###############################################.
## Reactive layout ----
###############################################.

# dce reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_dce <- renderUI({
  #Lists areas available in   
  areas_summary_dce <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_dce])
  if(input$geotype_dce == "Cancer Network") {
    selectizeInput("geoname_dce", label = NULL, choices = c("NCA", "SCAN", "WOSCAN"), selected = "NCA")  
  } else if (input$geotype_dce == "Scotland") {
    selectizeInput("geoname_dce", label = NULL, choices = "Scotland", selected = "Scotland")  
  }  
})


# The charts and text shown on the app will depend on what the user wants to see
output$dce_explorer1 <- renderUI({
  
  # text for titles of cut charts
  dce_site <- case_when(input$dce_type == "Breast" ~ "Breast",
                        input$dce_type == "Colorectal" ~ "Colorectal",
                        input$dce_type == "Lung" ~ "Lung"
  )
  
  tagList(
    p("DCE services in Scotland have been disrupted since late March 2020 as a result of the coronavirus 
      pandemic.  It is important to understand whether fewer patients have been diagnosed with cancer as a 
      result of these changes."),
    plot_cut_box(paste0("Number of " , dce_site, " cancer diagnoses in 2019 by stage: "), "dce_inc_bar19",
                 paste0("Number of " , dce_site, " cancer diagnoses in 2020 by stage: "), "dce_inc_bar20"),
    br(),
    plot_cut_box(paste0("Proportion of ", dce_site, " cancer diagnoses in 2019 by stage: "), "dce_inc_bar19_2",
                 paste0("Proportion of ", dce_site, " cancer diagnoses in 2020 by stage: "), "dce_inc_bar20_2"))
  
})

output$dce_explorer2 <- renderUI({
  
  # text for titles of cut charts
  dce_site <- case_when(input$dce_type == "Breast" ~ "Breast",
                        input$dce_type == "Colorectal" ~ "Colorectal",
                        input$dce_type == "Lung" ~ "Lung",
                        input$dce_type == "Combined" ~ "Combined"
  )
  
  tagList(
    p("The below graphs can be filtered by the stage of the cancer when it is detected - choose above."), 
    plot_box(paste0("Monthly count of individuals with ", dce_site, 
                    " cancer - Stage ", input$dce_stage), "dce_incidence") ,
    plot_box(paste0("Percentage change (2019 to 2020) of individuals with ", dce_site,
                    " cancer - Stage ", input$dce_stage), "dce_split")) #,
  # plot_box(paste0("Ratio of individuals with ", dce_site,
  #                 " cancer by Stage "), "dce_split2"))
  # ABOVE TWO LINES COMMENTED OUT UNTIL FORMAT OF CHART AGREED 8-10-21
})

###############################################.
## Charts ----
###############################################.
# Creating plots for each cut and dataset


output$dce_split <- renderPlotly({plot_dce_difference_chart(dce_dataset = dce_data_main2(),
                                                            dce_var1_chosen = "difference")})

output$dce_split2 <- renderPlotly({plot_dce_difference_chart2(dce_dataset = dce_data_main3(),
                                                              dce_var1_chosen = "ratio")})

output$dce_incidence <- renderPlotly({plot_dce_overall_chart(dce_dataset = dce_data_main(), 
                                                             dce_var1_chosen = "count20", dce_var2_chosen = "count19", data_type = input$geotype_dce)})

output$dce_inc_bar19 <- renderPlotly({plot_dce_inc_bar19(dce_dataset = dce_data_main3(), 
                                                         dce_var1 = "count19", data_type = input$geotype_dce)})

output$dce_inc_bar19_2 <- renderPlotly({plot_dce_inc_bar19_2(dce_dataset = dce_data_main3())})

output$dce_inc_bar20 <- renderPlotly({plot_dce_inc_bar20(dce_dataset = dce_data_main3(), data_type = input$geotype_dce)})

output$dce_inc_bar20_2 <- renderPlotly({plot_dce_inc_bar20_2(dce_dataset = dce_data_main3())})


###############################################.
## Data downloads ----
###############################################.


