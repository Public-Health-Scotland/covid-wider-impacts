# Server side for dce tab 

###############################################.
## Modals ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$btn_dce_modal, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("Data to support the Detect Cancer Early (DCE) initiative are collected by Cancer Audit staff across 
                 NHS Scotland and are part of the Scottish National Prospective Cancer Audit data sets, which are 
                 recorded onto the NHS Boards prospective cancer audit systems."),
               p("These data are collected locally by individual NHS Boards using national data standards. The information 
                 is collected as patients progress through their pathway of care from initial referral, investigations and 
                 diagnosis, to staging, treatments and follow-up. Further information on prospective cancer audit data 
                 definitions can be found under QPI data sets in the Cancer Audit section of the PHS website."),
               p(paste0("Figures presented based on data extracted on ",dce_extract_date)), # need to define cancer_extract_date reactive value
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

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

dce_data_dl <- reactive({
  
  dce_data %>% 
    select(area:count20, month) %>%
    rename("Area name" = area, "Cancer Type" = site, "Stage" = stage, 
           "No. Patients 2019" = count19, "No. Patients 2020" = count20, "Month" = month)
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
    
    plot_cut_box(paste0("Number of " , dce_site, " cancer diagnoses in 2019 by stage: "), "dce_inc_bar19",
                 paste0("Number of " , dce_site, " cancer diagnoses in 2020 by stage: "), "dce_inc_bar20"),
    p(em("(Click and drag on chart to zoom in)", style = "font-family: 'calibri'; font-si15pt")),
    p("NK = Not Known"),
    br(),
    plot_cut_box(paste0("Proportion of ", dce_site, " cancer diagnoses in 2019 by stage: "), "dce_inc_bar19_2",
                 paste0("Proportion of ", dce_site, " cancer diagnoses in 2020 by stage: "), "dce_inc_bar20_2"),
    p("NK = Not Known"))
  
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
    p(em("(Click and drag on chart to zoom in)",  style = "font-family: 'calibri'; font-si15pt")),
    br(),
    plot_box(paste0("Percentage change (2019 to 2020) of individuals with ", dce_site,
                    " cancer - Stage ", input$dce_stage), "dce_split") ,
    
    p(em("The cancer networks are regional collaborations working together across NHS Boards to improve
         patient care and cancer services:", style = "font-family: 'calibri'; font-si15pt")),
    p(em("NCA (North Cancer Alliance) NHS Grampian, NHS Highland, NHS Orkney, NHS Shetland, NHS Tayside
         and NHS Western Isles.", style = "font-family: 'calibri'; font-si15pt")),
    p(em("SCAN (South East of Scotland Cancer Network) NHS Borders, NHS Dumfries & Galloway, NHS Fife and
         NHS Lothian.", style = "font-family: 'calibri'; font-si15pt")),
    p(em("WoSCAN (West of Scotland Cancer Network) NHS Ayrshire & Arran, NHS Forth Valley, NHS Greater
         Glasgow & Clyde and NHS Lanarkshire." , style = "font-family: 'calibri'; font-si15pt")))
  
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

output$download_dce_data <- downloadHandler(
  filename ="dce_extract.csv",
  content = function(file) {
    write_csv(dce_data_dl(),
              file) } 
)
