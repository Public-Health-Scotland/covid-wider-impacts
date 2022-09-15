# Wider impacts dashboard - Cancer tab
# Server code

###############################################.
## Modals ----
###############################################.
# Pop-up modal explaining source of data
observeEvent(input$`cancer-source-modal`, 
             showModal(modalDialog(
               title = "What is the data source?",
               p("",
                 tags$a(href="https://www.isdscotland.org/Health-Topics/Cancer/Scottish-Cancer-Registry/How-data-are-collected/",class="externallink")),
               p("The Scottish Cancer Registry receives notifications of cancer from many data sources. Pathology 
                 records are one of the main sources, these are routinely transferred to the registry from the health 
                 board laboratories. These data are valuable to identify and maximise case ascertainment of potential 
                 new cancers."),
               p("Pathology records contain diagnosis information, which has been determined by examining the
                 cells and tissues microscopically.  Microscopic examination is generally considered as the most 
                 accurate method of diagnosis. The specimens used to determine diagnosis are received from various 
                 procedures such as smears and fluids, simple diagnostic punch biopsies, lymph node biopsies to 
                 fuller wide local excisions and resections. Therefore, it is highly likely that there are numerous 
                 pathology reports for one individual. The reports received by the registry related to solid tissue 
                 and cytology specimens. Peripheral blood smears are not included such as leukaemia diagnosed from 
                 peripheral blood film.  The majority of pathology records will relate to new primary cancers, some 
                 records will relate to disease recurrence or known primary cancers and/or metastatic disease."),
               p("The three graphs show numbers of individuals from whom a pathology specimen confirmed cancer since the start of
                 each of the years.  The Community Health Index (CHI) was used to count individuals.  If the same individual had
                 a subsequent cancer specimen reported that year for the same type of cancer, they were not counted again; but they
                 were counted twice or more for those with different types of cancer. "),
               
               p(paste0("Figures presented based on data extracted on ",cancer_extract_date)), # need to define cancer_extract_date reactive value
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))
###############################################.
## Functions ----
###############################################..
## Function for overall cancer charts 

plot_overall_cancer_chart <- function(dataset, var1_chosen, var2_chosen, var3_chosen, var4_chosen, data_name) {
  
  # set plot display if no data
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available, no referrals recorded")
  } else {
    
    
    # Set y axis label
    yaxis_title <- case_when(data_name == "cum" ~ "Cumulative total of individuals",
                             data_name == "inc" ~ "Weekly total of individuals")
    
    yaxis_plots[["title"]] <- yaxis_title
    
    
    #Text for tooltips
    
    measure_name <- case_when(data_name == "cum" ~ "Cumulative total of individuals: ",
                              data_name == "inc" ~ "Weekly total of individuals: ")
    
    denom_period <- case_when(var2_chosen %in% c("count19", "cum_count19")  ~ "2019",
                              var2_chosen %in% c("count_mean_17_19", "cum_count_mean_17_19") ~ "Mean 2017-2019")
    
    value1 <- dataset[[var1_chosen]]
    
    value2 <- dataset[[var2_chosen]]
    
    value3 <- dataset[[var3_chosen]]
    
    value4 <- dataset[[var4_chosen]]
    
    tooltip_1 <- c(paste0("Year: 2020", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value1))
    
    tooltip_2 <- c(paste0("Year: ", denom_period, "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value2))
    
    tooltip_3 <- c(paste0("Year: 2021", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, value3))
    
    tooltip_3.1 <- c(paste0("Year: 2022", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                            "<br>", measure_name, value4))
    
    tooltip_4 <- c(paste0("Year: 2020", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    tooltip_5 <- c(paste0("Year: 2021", "<br>", "Week ending: ", format(dataset$week_ending, "%d %b"),
                          "<br>", measure_name, paste0(format(round(value3, 2), nsmall = 2), "%")))
    
    
    #Creating time trend plot for cumulative totals and incidence
    plot_ly(data=dataset, x=~week_ending) %>%
      add_lines(y = ~get(var4_chosen), line = list(color = "red", opacity = 0.3, width = 3),
                text=tooltip_3.1, hoverinfo="text", name = "2022") %>%
      # 2021 line
      add_lines(y = ~get(var3_chosen), line = list(color = "blue", opacity = 0.3, width = 3),
                text=tooltip_3, hoverinfo="text", name = "2021") %>%
      # 2020 line
      add_lines(y = ~get(var1_chosen), line = list(color = "green", opacity = 0.6, width = 2),
                text=tooltip_1, hoverinfo="text", name = "2020") %>%
      # 2019 line
      add_lines(y = ~get(var2_chosen), line = list(color = "black", dash = 'dash', opacity = 3, width = 1),
                text=tooltip_2, hoverinfo="text", name = denom_period) %>%
      #Layout
      layout(yaxis = yaxis_plots, xaxis = list(title = "Week ending", tickfont = list(size = 13), tick0 = "2020-01-05", dtick = 4*60*60*24*7*1000),
             legend = list(x = 100, y = 0.5)) %>%
      add_vline(x = '2020-03-22', text = "1st lockdown", margin = list(b = 80, t = 20)) %>% 
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}

###############################################.
# CANCER DIFF PLOT - NO BREAKDOWN 
plot_diff_cancer_chart <- function(dataset, periodvar, diffvar1) {
  
  # set plot display if no data
  if (is.data.frame(dataset) && nrow(dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available, no referrals recorded")
  } else {
    
    # Set x axis label
    xaxis_title <-  "Quarter"
    
    xaxis_plots[["title"]] <- xaxis_title
    
    # Set y axis label
    yaxis_title <-  "% Change from 2019 quarter"
    yaxis_plots[["title"]] <- yaxis_title 
    
    #Text for tooltips  
    measure_name <- "Percentage (%) change:"
    
    value1 <- dataset[[diffvar1]]
    
    tooltip_1 <- c(paste0("Quarter: ", dataset$quarter_no, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    tooltip_2 <- c(paste0("Quarter: ", dataset$quarter_no, "<br>",
                          "Age group: ", dataset$age_group, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    tooltip_3 <- c(paste0("Quarter: ", dataset$quarter_no, "<br>", 
                          "Deprivation quintile: ", dataset$dep, "<br>",
                          measure_name, " ", paste0(format(round(value1, 2), nsmall = 2), "%")))
    
    #Creating time trend plot for difference
    
    diff_plot <- plot_ly(data=dataset, x = ~get(periodvar))
    
    if (input$breakdown == "None"){ # DIFF PLOT - No breakdown
      
      diff_plot <- diff_plot %>%
        add_trace(y = ~get(diffvar1),
                  type = 'scatter',
                  mode = 'line',
                  color = 'purple',
                  text = tooltip_1,
                  hoverinfo="text")
      
    } else if (input$breakdown == "Age Group") { # DIFF PLOT - AGE BREAKDOWN
      
      diff_plot <- diff_plot %>%
        add_trace(y = ~get(diffvar1),
                  type = 'scatter',
                  mode = 'line',
                  color = ~age_group,
                  colors = pal_sact,
                  text = tooltip_2,
                  hoverinfo="text")
      
    } else if (input$breakdown == "Deprivation") { #DIFF PLOT - Deprivation BREAKDOWN
      
      diff_plot <- diff_plot %>% 
        add_trace(y = ~get(diffvar1),
                  type = 'scatter',
                  mode = 'line',
                  color = ~dep,
                  colors = pal_cancer_diff,
                  text = tooltip_3, 
                  hoverinfo="text") 
    }
    
    #Layout
    diff_plot %>%  layout(margin = list(b = 80, t=5),
                          xaxis = xaxis_plots, yaxis = yaxis_plots,
                          legend = list(orientation = 'h', x = 0, y = 1.1)) %>%
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}

###############################################.
## Reactive datasets ----
###############################################.


cancer_data_cum_main <- reactive({
  
  cancer_data2 %>% filter(sex == input$gender, 
                          area == input$geoname_cancer, 
                          site == input$cancer_type,
                          breakdown == "None")
  
})

# Reactive datasets for the diff charts
# cancer_data_diff <- reactive({
#   cancer_data_quarters %>% filter(sex == input$gender, 
#                               area == input$geoname_cancer, 
#                               site == input$cancer_type,
#                               breakdown == input$breakdown) %>%
#     # Ordering factor so it shows correctly in Plotly
#     mutate(quarter_no = factor(quarter_no, ordered = TRUE)) %>% 
#     arrange(quarter_no)
# })

cancer_data_diff_2yrs <- reactive({
  cancer_data_quarters_2 %>% filter(sex == input$gender, 
                                  area == input$geoname_cancer, 
                                  site == input$cancer_type,
                                  breakdown == input$breakdown) %>%
    # Ordering factor so it shows correctly in Plotly
    mutate(quarter_no = factor(quarter_no, labels = c("Jan-Mar 2020", "Apr-Jun 2020",
                                                      "Jul-Sep 2020", "Oct-Dec 2020",
                                                      "Jan-Mar 2021", "Apr-Jun 2021",
                                                      "Jul-Sep 2021", "Oct-Dec 2021"), ordered = TRUE)) %>% 
    arrange(quarter_no)
})

cancer_data_dl <- reactive({
  
  cancer_data2 %>%
    filter(sex == "All" & breakdown == "None") %>% 
    select(area, site, week_number, count19, count20, count21) %>% 
    rename("Area name" = area, "Cancer type" = site,
           # "Sex" = sex,
           # "Age Group" = age_group,
           # "Deprivation Quintile (0=unknown)" = dep,
           "Week Number" = week_number,
           "Count 2019" = count19,
           "Count 2020" = count20,
           "Count 2021" = count21) #,
           # "Breakdown" = breakdown)
  
})


###############################################.
## Reactive layout ----
###############################################.

# Cancer reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_cancer <- renderUI({
  #Lists areas available in   
  areas_summary_cancer <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_cancer])
  if(input$geotype_cancer == "Health Boards") {
    selectizeInput("geoname_cancer", label = NULL, choices = c("NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
                                                               "NHS Fife", "NHS Forth Valley", "NHS Grampian", 
                                                               "NHS Greater Glasgow & Clyde", "NHS Highland",
                                                               "NHS Lanarkshire", "NHS Lothian", "NHS Orkney",
                                                               "NHS Shetland", "NHS Tayside", "NHS Western Isles"), 
                   selected = "NHS Ayrshire & Arran")
  } else if (input$geotype_cancer == "Cancer Networks") {
    selectizeInput("geoname_cancer", label = NULL, choices = c("NCA", "SCAN", "WOSCAN"), selected = "NCA")  
  } else if (input$geotype_cancer == "Scotland") {
    selectizeInput("geoname_cancer", label = NULL, choices = "Scotland", selected = "Scotland")  
  }  
})


# The charts and text shown on the app will depend on what the user wants to see
output$cancer_explorer <- renderUI({
  
  #  text for titles of cut charts
  cancer_site <- case_when(input$cancer_type == "All Malignant Neoplasms (Excl. C44)" ~ "All Malignant Neoplasms (Excl. C44)",
                           input$cancer_type == "All Cancers" ~ "All Cancers",
                           input$cancer_type == "Bladder" ~ "Bladder",
                           input$cancer_type == "Bone and Connective Tissue" ~ "Bone and Connective Tissue",
                           input$cancer_type == "Breast" ~ "Breast",
                           input$cancer_type == "Cervical - Females only" ~ "Cervical - Females only",
                           input$cancer_type == "Colorectal" ~ "Colorectal",
                           input$cancer_type == "Head and Neck" ~ "Head and Neck",
                           input$cancer_type == "Hodgkin Lymphoma" ~ "Hodgkin Lymphoma",
                           input$cancer_type == "Kidney" ~ "Kidney",
                           input$cancer_type == "Leukaemias" ~ "Leukaemias",
                           input$cancer_type == "Liver and Intrahepatic Bile Ducts" ~ "Liver and Intrahepatic Bile Ducts",
                           input$cancer_type == "Brain Tumour" ~ "Brain Tumour",
                           input$cancer_type == "Malignant Melanoma of the Skin" ~ "Malignant Melanoma of the Skin",
                           input$cancer_type == "Mesothelioma" ~ "Mesothelioma",
                           input$cancer_type == "Multiple Myeloma and malignant plasma cell neoplasms" ~ "Multiple Myeloma and malignant plasma cell neoplasms",
                           input$cancer_type %in% c("Non-Hodgkin Lymphoma") ~ "Non-Hodgkin Lymphoma",
                           input$cancer_type == "Non-Melanoma Skin Cancer" ~ "Non-Melanoma Skin Cancer",
                           input$cancer_type == "Oesophagus" ~ "Oesophagus",
                           input$cancer_type == "Other" ~ "Other",
                           input$cancer_type == "Ovary - Females only" ~ "Ovary - Females only",
                           input$cancer_type == "Pancreas" ~ "Pancreas",
                           input$cancer_type == "Penis - Males only" ~ "Penis - Males Only",
                           input$cancer_type == "Prostate - Males only" ~ "Prostate - Males only",
                           input$cancer_type == "Stomach" ~ "Stomach",
                           input$cancer_type == "Testis - Males only" ~ "Testis - Males only",
                           input$cancer_type == "Thyroid" ~ "Thyroid",
                           input$cancer_type == "Trachea, Bronchus and Lung" ~ "Trachea, Bronchus and Lung",
                           input$cancer_type == "Uterus - Females only" ~ "Uterus - Females only",
                           input$cancer_type == "Vagina - Females only" ~ "Vagina - Females only",
                           input$cancer_type == "Vulva - Females only" ~ "Vulva - Females only"
  )
  
  
  tagList(
    plot_box(paste0("Total count of individuals having a cancer of type:  ", input$cancer_type, #cancer_site,
                    " confirmed on a pathological specimen since January for 2019 to 2022"), 
             "cancer_overall"),
    p(em(paste0(cancer_extract_date), style = "font-family: 'calibri'; font-si15pt")),
    br(),
    plot_box(paste0("Weekly count of individuals having a cancer of type: ", input$cancer_type, #cancer_site,
                    " confirmed on a pathological specimen since January for 2019 to 2022"), 
             "cancer_incidence"),
    p(em(paste0(cancer_extract_date), style = "font-family: 'calibri'; font-si15pt"))
  )  
  
})

################## ADDITION 18-8-21 TO RE-ARRANGE PAGE #####################

output$cancer_explorer3 <- renderUI({
  
  tagList(
    if(input$breakdown == "Age Group") {
      plot_box(paste0("Percentage change of individuals having a cancer of type: ", input$cancer_type, 
                      " confirmed on a pathological specimen by quarter against equivalent quarter 2019 by Age Group - ", 
                      input$geoname_cancer), "cancer_split")
      
    } else if (input$breakdown == "Deprivation") {
      plot_box(paste0("Percentage change of individuals having a cancer of type: ", input$cancer_type, 
                      " confirmed on a pathological specimen by quarter against equivalent quarter 2019 by Deprivation 
                        quintile - ",
                       input$geoname_cancer), "cancer_split")
    } else {
      plot_box(paste0("Percentage change of individuals having a cancer of type: ", input$cancer_type, 
                      " confirmed on a pathological specimen by quarter against equivalent quarter 2019 - ",
                      input$geoname_cancer), "cancer_split")
    },
    
    p(em(paste0(cancer_extract_date), style = "font-family: 'calibri'; font-si15pt")),
    br(),
    p(em("Note: registrations for non-melanoma skin cancer (ICD-10 C44) are likely to be less complete and less accurate 
         than for other cancer sites. Such cancers are relatively common and usually non-fatal. There is a propensity 
         for multiple tumours to occur in one individual and cancer registries adopt different practices in recording 
         these. The tumours are most common in the elderly population and the completeness of registration in the very 
         elderly is likely to be less than for younger patients. Furthermore, increasing numbers of these cancers are 
         diagnosed and treated within GP surgeries and the registration scheme is not confident that all such cases 
         are notified. Because cancer registries across the world have different practices for recording non-melanoma 
         skin cancer (some do not record them at all), the category 'All Malignant Neoplasms (Excl. C44)' omits these tumours 
         in the interests of making international comparisons of cancer incidence more valid.", style = "font-family: 'calibri'; font-si15pt")))
  
})

################# ADDITION MAY 21 TO RE-ARRANGE PAGE ##############

# MAIN PAGE TEXT
output$cancer_explorer2 <- renderUI({
  
  
  
  tagList(
    p("Cancer services in Scotland have been disrupted since late March 2020 as a result of the coronavirus
      pandemic.  It is important to understand whether fewer patients have been diagnosed with cancer as a
      result of these changes.  The Scottish Cancer Registry published its high quality figures on cancer
      incidence for 2020 on 2nd June 2022.  As a proxy measure of new cancer diagnoses, this dashboard presents
      numbers of individuals from whom a pathology sample found cancer in 2020 through to 2022 and compares them to 2019.
      While only proxy measures, the size of the changes corresponds approximately with those reported by cancer clinicians."),
    p(strong("Note - this does not include all patients who have been newly diagnosed with cancer (by other methods), 
              and will also include some patients who are being followed-up from a pre-2019 diagnosis of cancer.")),
    p(("By the end of 2021 (week ending 27th December), the total number of individuals in Scotland with a pathological confirmation of 
       cancer (excluding non-melanoma skin cancers) in Scotland was "),
      strong ("33,162 ", style = "font-family: 'arial'; font-si20pt;"),
      ("in 2021, "),
      strong ("28,784 ", style = "font-family: 'arial'; font-si20pt;"),
      ("in 2020 and"),
      strong ("33,345 ", style = "font-family: 'arial'; font-si20pt;"),
      ("in 2019, absolute differences of "), 
      strong ("4,561 ", style = "font-family: 'arial'; font-si20pt;"),
      ("individuals in 2020 (an overall cumulative difference of "),
      strong ("-13.7% ", style = "font-family: 'arial'; font-si20pt;"),
      ("), and "),
      strong ("183 ", style = "font-family: 'arial'; font-si20pt;"),
      ("individuals in 2021 (an overall cumulative difference of "),
      strong ("-0.6% ", style = "font-family: 'arial'; font-si20pt;"),
      (").  That is to say, "),
      strong ("4,561 / 183 ", style = "font-family: 'arial'; font-si20pt;"),
      ("fewer patients in Scotland had a pathologically confirmed cancer diagnosis by the end of 2020/2021 than would have 
       been expected.")),
    p("The commonest cancers in Scotland are of the lung, breast (females), prostate (males) and colorectal."),
    p("By the week ending 27th December 2020, compared to the same week in 2019, there were ",
      strong ("570 ", style = "font-family: 'arial'; font-si20pt;"),
        "fewer lung cancers (a total fall of ",
      strong ("20.1%", style = "font-family: 'arial'; font-si20pt;"),
      "); ",
      strong ("806 ", style = "font-family: 'arial'; font-si20pt;"),
      "fewer breast cancers (a total fall of ",
      strong ("15.0%", style = "font-family: 'arial'; font-si20pt;"),
      "); ",
      strong ("529 ", style = "font-family: 'arial'; font-si20pt;"),
      "fewer prostate cancers (a total fall of ",
      strong ("16.8%", style = "font-family: 'arial'; font-si20pt;"),
      "); and ",
      strong ("825 ", style = "font-family: 'arial'; font-si20pt;"),
      "fewer colorectal cancers (a total fall of ",
      strong ("20.4%", style = "font-family: 'arial'; font-si20pt;"),
      "). "),
    p("By the week ending 27th December 2021, compared to the same week in 2019, there were ",
      strong ("509 ", style = "font-family: 'arial'; font-si20pt; color:"),
      "fewer lung cancers (a total fall of ",
      strong ("18.0%", style = "font-family: 'arial'; font-si20pt; color:"),
      "); ",
      strong ("4 ", style = "font-family: 'arial'; font-si20pt;"),
      "more breast cancers (a total increase of less than",
      strong ("1%", style = "font-family: 'arial'; font-si20pt;"),
      "); ",
      strong ("508 ", style = "font-family: 'arial'; font-si20pt;"),
      "more prostate cancers (a total increase of ",
      strong ("16.2%", style = "font-family: 'arial'; font-si20pt;"),
      "); and ",
      strong ("30 ", style = "font-family: 'arial'; font-si20pt;"),
      "more colorectal cancers (a total increase of less than",
      strong ("1%", style = "font-family: 'arial'; font-si20pt;"),
      "). "),
    strong("Note: as the information provided by this dashboard is updated, it will both add more recent data, and
           may also update historical data."),
    br(),
    p(strong(paste0("Last updated: - 3rd August 2022 ;  date of extraction of data: ",cancer_extract_date, ", with pathological records to week ending
             2nd February 2022."))))
  
  
})

###############################################.
## Charts ----
###############################################.
# Creating plots for each cut and dataset

output$cancer_overall <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                                                 var1_chosen = "cum_count20",
                                                                 if(input$baseline == "2019"){
                                                                   var2_chosen = "cum_count19"
                                                                 } else {
                                                                   var2_chosen = "cum_count_mean_17_19"  
                                                                 },
                                                                 var3_chosen = "cum_count21",
                                                                 var4_chosen = "cum_count22",
                                                                 data_name = "cum")})

output$cancer_incidence <- renderPlotly({plot_overall_cancer_chart(cancer_data_cum_main(), 
                                                                   var1_chosen = "count20",
                                                                   if(input$baseline == "2019"){
                                                                     var2_chosen = "count19"
                                                                   } else {
                                                                     var2_chosen = "count_mean_17_19"  
                                                                   },
                                                                   var3_chosen = "count21",
                                                                   var4_chosen = "count22",
                                                                   data_name = "inc")})

# Difference charts
output$cancer_split <- renderPlotly({plot_diff_cancer_chart(cancer_data_diff_2yrs(), periodvar = "quarter_no",
                                                            if(input$cum_baseline == "Standard"){
                                                              diffvar1 = "difference"
                                                              } else {
                                                              diffvar1 = "cum_difference"  
                                                              })})

###############################################.
## Data downloads ----
###############################################.


output$download_cancer_data <- downloadHandler(
  filename ="cancer_extract.csv",
  content = function(file) {
    write_csv(cancer_data_dl(),
              file) } 
) 


#END