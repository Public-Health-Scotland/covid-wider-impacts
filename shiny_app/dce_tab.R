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
    p(em("(NK = Not Known; click and drag on chart to zoom in)", style = "font-family: 'calibri'; font-si15pt")),
    br())
  
})

output$dce_explorer2 <- renderUI({
  
  # text for titles of cut charts
  dce_site <- case_when(input$dce_type == "Breast" ~ "Breast",
                        input$dce_type == "Colorectal" ~ "Colorectal",
                        input$dce_type == "Lung" ~ "Lung",
                        input$dce_type == "Combined" ~ "Combined"
  )
  
  tagList(
    p("The below graphs can be filtered by the stage of the cancer when it is detected - choose below."),
    plot_box(paste0("Monthly count of individuals with ", dce_site,
                    " cancer - Stage ", input$dce_stage), "dce_incidence") ,
    plot_box(paste0("Percentage change (2019 to 2020) of individuals with ", dce_site,
                    " cancer - Stage ", input$dce_stage), "dce_split") ,
    plot_cut_box(paste0("Proportion of ", dce_site, " cancer diagnoses in 2019 by stage: "), "dce_inc_bar19_2",
                 paste0("Proportion of ", dce_site, " cancer diagnoses in 2020 by stage: "), "dce_inc_bar20_2"),
    br(),
    p(em("The cancer networks are regional collaborations working together across NHS Boards to improve
         patient care and cancer services:", style = "font-family: 'calibri'; font-si15pt")),
    p(em("NCA (North Cancer Alliance) - NHS Grampian, NHS Highland, NHS Orkney, NHS Shetland, NHS Tayside
         and NHS Western Isles.", style = "font-family: 'calibri'; font-si15pt")),
    p(em("SCAN (South East of Scotland Cancer Network) - NHS Borders, NHS Dumfries & Galloway, NHS Fife and
         NHS Lothian.", style = "font-family: 'calibri'; font-si15pt")),
    p(em("WoSCAN (West of Scotland Cancer Network) - NHS Ayrshire & Arran, NHS Forth Valley, NHS Greater
         Glasgow & Clyde and NHS Lanarkshire." , style = "font-family: 'calibri'; font-si15pt")))
  
  # plot_box(paste0("Ratio of individuals with ", dce_site,
  #                 " cancer by Stage "), "dce_split2"))
  # ABOVE TWO LINES COMMENTED OUT UNTIL FORMAT OF CHART AGREED 8-10-21
})

###############################################.
## Functions ----
###############################################.

###############################################.
## Function for DCE overall chart ----
###############################################.

plot_dce_overall_chart <- function(dce_dataset, dce_var1_chosen, dce_var2_chosen, data_type) {
  
  # set plot display if no data
  if (is.data.frame(dce_dataset) && nrow(dce_dataset) == 0)
  { plot_nodata(height = 30, text_nodata = "Chart not available")
  } else {
    
    # Set y axis label
    yaxis_title <- "Monthly Total of Individuals"
    
    yaxis_plots[["title"]] <- yaxis_title
    
    # Set x axis label
    xaxis_title <- "Month"
    
    xaxis_plots[["title"]] <- xaxis_title
    
    
    
    #Text for tooltips
    
    dce_measure_name <- "Number of Individuals"
    
    dce_value1 <- dce_dataset[[dce_var1_chosen]]
    
    dce_value2 <- dce_dataset[[dce_var2_chosen]]
    
    
    # dce_tooltip_1 <- c(paste0("Month: ", dce_dataset$month),
    #                    "<br>", dce_measure_name, dce_value1)
    
    dce_tooltip_1 <- c(paste0("Month: ", dce_dataset$month,
                              "<br>", "Year: 2020",
                              "<br>", "Number of Individuals: ", dce_dataset$count20,
                              "<br>", "Area: ", dce_dataset$area))
    
    # dce_tooltip_2 <- c(paste0("Month: ", dce_dataset$month),
    #                    "<br>", dce_measure_name, dce_value2)
    
    dce_tooltip_2 <- c(paste0("Month: ", dce_dataset$month,
                              "<br>", "Year: 2019",
                              "<br>", "Number of Individuals: ", dce_dataset$count19,
                              "<br>", "Area: ", dce_dataset$area))
    
    ### layout - test change
    if (data_type == "Scotland"){
      a <- list(
        title = "Number of Individuals",
        autotick = FALSE,
        ticks = "outside",
        tick0 = 0,
        dtick = 20,
        ticklen = 5,
        tickwidth = 2,
        tickcolor = toRGB("blue"),
        range = c(0, 240)
      )
    }else {
      a <- list(
        title = "Number of Individuals",
        autotick = FALSE,
        ticks = "outside",
        tick0 = 0,
        dtick = 10,
        ticklen = 5,
        tickwidth = 2,
        tickcolor = toRGB("blue"),
        range = c(0, 115)
      )
    }
    ##########################
    
    # Function for horizontal line showing monthly expected
    hline <- function(y = 0, color = "lightgrey") {
      list(
        type = "line",
        y0 = y,
        y1 = y,
        # yref = "paper",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        line = list(color = color, dash = 'dash')
      )
    }
    
    
    #Creating time trend plot for cumulative totals and incidence
    plot_ly(data=dce_dataset, x=~month) %>%
      
      # 2020 line
      add_lines(y = ~get(dce_var1_chosen), line = list(color = pal_overall[1]), text=dce_tooltip_1, hoverinfo="text",
                name = "2020") %>%
      # 2019 line
      add_lines(y = ~get(dce_var2_chosen), line = list(color = pal_overall[2], dash = 'dash'), text=dce_tooltip_2, hoverinfo="text",
                name = "2019") %>%
      add_annotations(x = 0,
                      y = 188,
                      text = "Expected diagnoses",
                      xref = "1",
                      yref = "1",
                      showarrow = FALSE) %>% 
      
      #Layout 
      layout(margin = list(b = 80, t=5),
             shapes = list(hline(180)),
             xaxis = list(title = "Month"),
             yaxis = a,
             # legend = list(title=list(text=paste0("Year")),orientation = 'h', x = 0, y = 1.2)) %>%
             legend = list(orientation = 'h', x = 0, y = 1.2)) %>%
      
      
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
  }
}


###############################################.
## Function for DCE difference charts ----
###############################################.

plot_dce_difference_chart <- function(dce_dataset, dce_var1_chosen) {
  
  # # set plot display if no data
  # if (is.data.frame(dce_dataset) && nrow(dce_dataset) == 0)
  # { plot_nodata(height = 30, text_nodata = "Chart not available")
  # } else {
  
  
  # Set y axis label
  yaxis_title <- "Percentage Change"
  
  yaxis_plots[["title"]] <- yaxis_title
  
  # Set x axis label
  xaxis_title <- "Month"
  
  xaxis_plots[["title"]] <- xaxis_title
  
  
  #Text for tooltips
  
  dce_measure_name <- "Percentage Change "
  
  dce_value1 <- dce_dataset[[dce_var1_chosen]]
  
  
  dce_tooltip_3 <- c(paste0("Month: ", dce_dataset$month,
                            "<br>", "Percentage Change: ", paste0(format(round(dce_value1, 2), nsmall = 2), "%"),
                            "<br>", "Area: ", dce_dataset$area))
  
  
  # #Creating time trend plot for difference
  plot_ly(data=dce_dataset, x=~month, y = ~get(dce_var1_chosen)) %>%
    
    #Creating time trend plot
    add_trace(type = 'scatter', mode = 'lines',
              color = ~area,
              colors = pal_dce_diff,
              text = dce_tooltip_3, hoverinfo="text") %>%
    
    #Layout
    layout(margin = list(b = 80, t=5),
           xaxis = xaxis_plots, yaxis = yaxis_plots,
           legend = list(orientation = 'h', x = 0, y = 1.1)) %>%
    
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
}

###############################################
plot_dce_difference_chart2 <- function(dce_dataset, dce_var1_chosen) {
  
  # # set plot display if no data
  # if (is.data.frame(dce_dataset) && nrow(dce_dataset) == 0)
  # { plot_nodata(height = 30, text_nodata = "Chart not available")
  # } else {
  
  
  # Set y axis label
  yaxis_title <- "Ratio - 2020/2019"
  
  yaxis_plots[["title"]] <- yaxis_title
  
  # Set x axis label
  xaxis_title <- "Month"
  
  xaxis_plots[["title"]] <- xaxis_title
  
  
  #Text for tooltips
  
  dce_measure_name <- "Ratio between 2020/2019 by stage "
  
  dce_value1 <- dce_dataset[[dce_var1_chosen]]
  
  
  dce_tooltip_4 <- c(paste0("Month: ", dce_dataset$month,
                            "<br>", "Ratio: ", paste0(format(round(dce_value1, 2), nsmall = 2)),
                            "<br>", "Stage: ", dce_dataset$stage))
  
  
  # #Creating time trend plot for difference
  plot_ly(data=dce_dataset, x=~month, y = ~get(dce_var1_chosen)) %>%
    
    #Creating time trend plot
    add_trace(type = 'scatter', mode = 'lines',
              color = ~stage,
              colors = pal_dce,
              text = dce_tooltip_4, hoverinfo="text") %>%
    
    #Layout
    layout(margin = list(b = 80, t=5),
           xaxis = xaxis_plots, yaxis = yaxis_plots,
           legend = list(orientation = 'h', x = 0, y = 1.1)) %>%
    
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
}



###############################################.
## Functions for DCE bar charts ----
###############################################.

## 2019 ##

# 1. Stacked incidence by stage
plot_dce_inc_bar19 <- function(dce_dataset, dce_var1, data_type) {
  
  # Set y axis label
  yaxis_title <- "Incidence"
  
  yaxis_plots[["title"]] <- yaxis_title
  
  # Set x axis label
  xaxis_title <- "Month"
  
  xaxis_plots[["title"]] <- xaxis_title
  
  #Text for tooltips
  
  dce_tooltip_4 <- c(paste0("Month: ", dce_dataset$month,
                            "<br>", "Stage: ", dce_dataset$stage,
                            "<br>", "Monthly Total: ", dce_dataset$count19))
  
  if (data_type == "Scotland"){
    b <- list(
      title = "Incidence",
      autotick = FALSE,
      ticks = "outside",
      tick0 = 0,
      dtick = 50,
      ticklen = 5,
      tickwidth = 2,
      tickcolor = toRGB("blue"),
      range = c(0, 500)
    )
  }else {
    b <- list(
      title = "Incidence",
      autotick = FALSE,
      ticks = "outside",
      tick0 = 0,
      dtick = 50,
      ticklen = 5,
      tickwidth = 2,
      tickcolor = toRGB("blue"),
      range = c(0, 260)
    )
  }
  
  
  plot_ly(data=dce_dataset, x = ~month) %>%
    add_bars(y = ~get(dce_var1), color = ~stage, colors = pal_dce, 
             text = dce_tooltip_4, hoverinfo = "text", textposition = "none") %>%
    add_annotations(x = dce_dataset$month,
                    y = (dce_dataset$total19)+10,
                    text = dce_dataset$total19,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    layout(barmode = "stack",
           xaxis = xaxis_plots, yaxis = b,
           legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'normal')) %>%
    
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  
  
}
#}

# 2. Stacked incidence by proportion

plot_dce_inc_bar19_2 <- function(dce_dataset) {
  
  # Set y axis label
  yaxis_title <- "Percentage per Stage at Diagnosis"
  
  yaxis_plots[["title"]] <- yaxis_title
  
  # Set x axis label
  xaxis_title <- "Month"
  
  xaxis_plots[["title"]] <- xaxis_title
  
  #Text for tooltips
  
  dce_tooltip_5 <- c(paste0("Month: ", dce_dataset$month,
                            "<br>", "Stage: ", dce_dataset$stage,
                            "<br>", "Percentage of Total: ", dce_dataset$percent19, "%"))
  
  plot_ly(data=dce_dataset, x = ~month, y = ~percent19, color = ~stage,  colors = pal_dce, 
          text = dce_tooltip_5, hoverinfo = "text", textposition = "none") %>%
    add_bars() %>%
    layout(barmode = "stack",
           xaxis = xaxis_plots, yaxis = yaxis_plots,
           legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'normal')) %>%
    
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  
}

########################################################################

## 2020 ##
# 1. Stacked incidence by stage

plot_dce_inc_bar20 <- function(dce_dataset, data_type) {
  
  # Set y axis label
  yaxis_title <- "Incidence"
  
  yaxis_plots[["title"]] <- yaxis_title
  
  # Set x axis label
  xaxis_title <- "Month"
  
  xaxis_plots[["title"]] <- xaxis_title
  
  #Text for tooltips
  
  dce_tooltip_6 <- c(paste0("Month: ", dce_dataset$month,
                            "<br>", "Stage: ", dce_dataset$stage,
                            "<br>", "Monthly Total: ", dce_dataset$count20))
  
  if (data_type == "Scotland"){
    c <- list(
      title = "Incidence",
      autotick = FALSE,
      ticks = "outside",
      tick0 = 0,
      dtick = 50,
      ticklen = 5,
      tickwidth = 2,
      tickcolor = toRGB("blue"),
      range = c(0, 500)
    )
  }else {
    c <- list(
      title = "Incidence",
      autotick = FALSE,
      ticks = "outside",
      tick0 = 0,
      dtick = 50,
      ticklen = 5,
      tickwidth = 2,
      tickcolor = toRGB("blue"),
      range = c(0, 260)
    )
  }
  
  plot_ly(data=dce_dataset, x = ~month) %>% 
    add_bars(y = ~count20, color = ~stage, colors = pal_dce, 
             text = dce_tooltip_6, hoverinfo = "text", textposition = "none") %>%
    add_annotations(x = dce_dataset$month,
                    y = (dce_dataset$total20)+10,
                    text = dce_dataset$total20,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    
    layout(barmode = "stack",
           xaxis = xaxis_plots, yaxis = c,
           legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'normal')) %>%
    
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  
}
#}

# 2. Stacked incidence by proportion

plot_dce_inc_bar20_2 <- function(dce_dataset) {
  
  # Set y axis label
  yaxis_title <- "Percentage per Stage at Diagnosis"
  
  yaxis_plots[["title"]] <- yaxis_title
  
  # Set x axis label
  xaxis_title <- "Month"
  
  xaxis_plots[["title"]] <- xaxis_title
  
  #Text for tooltips
  
  dce_tooltip_7 <- c(paste0("Month: ", dce_dataset$month,
                            "<br>", "Stage: ", dce_dataset$stage,
                            "<br>", "Percentage of Total: ", dce_dataset$percent20, "%"))  
  
  plot_ly(data=dce_dataset, x = ~month, y = ~percent20, color = ~stage, colors = pal_dce, 
          text = dce_tooltip_7, hoverinfo = "text", textposition = "none") %>%
    add_bars() %>%
    layout(barmode = "stack",
           xaxis = xaxis_plots, yaxis = yaxis_plots,
           legend = list(orientation = 'h', x = 0, y = 1.1, traceorder = 'normal')) %>%
    
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  
}


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
