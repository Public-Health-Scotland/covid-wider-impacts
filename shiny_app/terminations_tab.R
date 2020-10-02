##Server script for terminations tab


# Pop-up modal explaining source of data
observeEvent(input$btn_top_modal, 
             showModal(modalDialog(#Maternal HEALTH MODAL
               title = "What is the data source?",
               p("These data are derived from the Notifications of Abortion to the Chief Medical Officer for Scotland (CMO) under the Abortion (Scotland) Regulations 1991."),
               p("Public Health Scotland (PHS) is responsible for the collation of data derived from notifications of terminations of pregnancy on behalf of the Chief Medical Officer (CMO) in Scotland. A termination of pregnancy (also referred to as a therapeutic or induced abortion) is carried out under the terms of the Abortion Act 1967, which applies to England, Wales and Scotland. Two doctors must agree that a termination of pregnancy is necessary under at least one of the grounds as specified in the 1991 Regulations. There is a legal requirement to notify the CMO in Scotland of all terminations carried out in Scotland within seven days of the termination of pregnancy."),
               p("Further information is available from the PHS ",
                 tags$a(href="https://beta.isdscotland.org/media/5320/2020-08-25-terminations-2019-report.pdf", "annual report on termination of pregnancy up to December 2019",class="externallink"),
                 "the ",
                 tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/sexual-health/termination-of-pregnancy-statistics/", "data tables and charts.",class="externallink"),
                 "are also available."),
               p("Data is presented at Scotland level and this represents the total of all the NHS boards including the island boards, however NHS Orkney, NHS Shetland and NHS Western Isles are not available to drill down to at board of residence level because the number of terminations can be small and therefore disclosive."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain SIMD and deprivation
# Link action button click to modal launch 
observeEvent(input$btn_modal_simd_top, { showModal(
  modalDialog(
    h5("What is SIMD and deprivation?"),
    p("Children have been allocated to different levels of deprivation based on the small area (data zone) 
      in which they live and the", tags$a(href="https://simd.scot/", "Scottish Index of Multiple Deprivation (SIMD).",
                                          class="externallink"), "score for that area. 
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

###############################################.
## ToP Reactive controls  ----
###############################################.

# Pregnancy reactive drop-down control showing list of area names depending on areatype selected
output$geoname_ui_top <- renderUI({
  #Lists areas available in   
  areas_summary_top <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_top])
  selectizeInput("geoname_top", label = NULL, choices = areas_summary_top, selected = "")
})

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset behind trend plot (available at scotland and NHS board level)
top_filter <- function(){
  
  top %>% filter(area_name == input$geoname_top &
                       area_type == input$geotype_top &
                       type %in% c("Scotland","Health board"))
}

#Dataset behind deprivation/age plots (only available at scotland level)
top_filter_split <- function(split){
  
  top %>% filter(area_name == "Scotland" &
                       area_type == "Scotland" &
                       type==split) %>%
    droplevels()
}

###############################################.
## Termination Charts ----
###############################################.

# chart outputs for trend
output$top_trend_n <- renderPlotly({plot_top_trend(measure="top_number")})
output$top_trend_g <- renderPlotly({plot_top_trend(measure="top_gestation")})

output$top_age_n <- renderPlotly({plot_top_split(dataset=top_filter_split("age"), split="age", measure="top_number")})
output$top_age_g <- renderPlotly({plot_top_split(dataset=top_filter_split("age"), split="age", measure="top_gestation")})

output$top_dep_n <- renderPlotly({plot_top_split(dataset=top_filter_split("dep"), split="dep", measure="top_number")})
output$top_dep_g <- renderPlotly({plot_top_split(dataset=top_filter_split("dep"), split="dep", measure="top_gestation")})


###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$top_explorer <- renderUI({
  
  # text for titles of cut charts
  top_subtitle <-  paste0("Figures based on data extracted ",top_extract_date)
  top_trend_title <- paste0("Termination of pregnancy: ",input$geoname_top)
  top_title_n <-  paste0("Number of terminations")
  top_title_g <-   paste0("Average gestation at termination (completed weeks)")
  
  chart_explanation <- paste0("The black line on the ‘number of terminations’ charts for Scotland, and each Health Board, shows a monthly time series of data. The solid blue centreline is the average number of terminations over the period January 2018 to February 2020. The dotted line continues that average to allow determination of whether there has been a change.  The ‘average gestation at termination’ charts follow a similar format.")
  
  #Additional commentart/meta data to appear on immunisation tab
  commentary_top <-  tagList(p("Space for any meta-data/commentary about terminations"))
  
  # Function to create common layout to all immunisation charts
  top_layout <- function(plot_trend_n,plot_trend_g, plot_age_n,plot_age_g,plot_dep_n,plot_dep_g){
    tagList(fluidRow(column(12,
                            h4(top_trend_title),
                            p(top_subtitle),
                            p(chart_explanation)),
                     column(6,
                            h4(paste0(top_title_n)),
                            withSpinner(plotlyOutput("top_trend_n"))),
                     column(6,
                            h4(paste0(top_title_g)),
                            withSpinner(plotlyOutput("top_trend_g")))),
            #only if scotland selected display age and deprivation breakdowns
            if (input$geotype_top == "Scotland"){
              tagList(
                fluidRow(column(12,h4("Terminations by age group: Scotland"))),
                fluidRow(column(6,
                                h4("Number of terminations"),br(),
                                withSpinner(plotlyOutput("top_age_n"))),
                         column(6,
                                #h4(paste0(top_dep_title_n)),
                                h4("Average gestation at termination (completed weeks)"),
                                br(),
                                withSpinner(plotlyOutput("top_age_g")))),
                fluidRow(column(12,h4("Terminations by deprivation: Scotland"),
                                actionButton("btn_modal_simd_top", "What is SIMD and deprivation?",
                                             icon = icon('question-circle')))),
                fluidRow(column(6,
                                h4("Number of terminations"),br(), 
                                withSpinner(plotlyOutput("top_dep_n"))),
                         column(6,
                                h4("Average gestation at termination (completed weeks)"),br(),
                                withSpinner(plotlyOutput("top_dep_g"))))
              )#tagList from if statement
            },
            fluidRow(column(12, renderUI(commentary_top))))
  }
  
  #link plot functions to layouts
  top_layout(plot_trend_n="top_trend_n", plot_trend_g="top_trend_g",
             plot_age_n="top_age_n", plot_age_g="top_age_g",
             plot_dep_n="top_dep_n", plot_dep_g="top_dep_g")
})

#############################################.
## Termination chart functions ----
############################################.

## Trend plot for monthly TOP numbers and average gestation 
plot_top_trend <- function(measure){  
  
  plot_data <- top_filter()
  
  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50, text_nodata = "No data shown for small island boards")
  } else {
    
  # chart legend labels  
  centreline_name <- paste0(input$geoname_top," centreline 01/01/2018 to 01/03/2020")    
  dottedline_name <- paste0(input$geoname_top," projected") 
  
  #switch y-axis according to which measure is selected
  if(measure == "top_number"){
    yaxis_measure <- plot_data$terminations
    yaxis_plots[["title"]] <- "Number of terminations"
    
    tooltip_top <- c(paste0("Month: ",plot_data$month,"<br>",
                            "Number of terminations: ",plot_data$terminations))
    dotted_line <-  plot_data$dottedline_no
    centre_line <-  plot_data$centreline_no
    yname <- "Number of terminations"
    
  } else if (measure  == "top_gestation") {
    yaxis_measure <- plot_data$av_gest
    yaxis_plots[["title"]] <- "Average gestation at termination (completed wks)"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10 weeks
    #yaxis_plots[["tickfont"]] <- list(size=8)
    #yaxis_plots[["titlefont"]] <- list(size=12)
    #tickfont = list(size=14), titlefont = list(size=14)
    

    tooltip_top <- c(paste0("Month: ",plot_data$month,"<br>",
                            "Average gestation at termination: ",format(plot_data$av_gest,digits = 1,nsmall=1)," weeks"))                           
        dotted_line <-  plot_data$dottedline_g
    centre_line <-  plot_data$centreline_g
    yname <- "Average gestation"
  }
  
  #Creating time trend plot
  plot_ly(data=plot_data, x=~month) %>%
    add_lines(y = ~yaxis_measure,  
              line = list(color = "black"), text=tooltip_top, hoverinfo="text",
              marker = list(color = "black"), name = yname ) %>% 
    add_lines(y = ~dotted_line, name = dottedline_name,
              line = list(color = "blue", dash = "longdash"), hoverinfo="none",
              name = "Centreline") %>%
    add_lines(y = ~centre_line, name = centreline_name,
              line = list(color = "blue"), hoverinfo="none",
              name = "Centreline") %>% 
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,  xaxis = xaxis_plots,
           legend = list(x = 0.1, y = 0.1)) %>% #position of legend
    #legend = list(x = 100, y = 0.5)) %>% #position of legend
    #leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
  }}

## Trend plot for monthly termination numbers and average gestation at booking split by age group and simd quintile 
plot_top_split <- function(dataset, split, measure){
  
  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))
  
  #switch y-axis according to which measure is selected
  if(measure == "top_number"){
    yaxis_measure <- dataset$terminations
    yaxis_plots[["title"]] <- "Number of terminations"
    tooltip_top <- c(paste0(tool_tip_split,dataset$category,"<br>",
                            "Month: ",dataset$month,"<br>",
                            "Number of terminations: ",dataset$terminations))
    
  } else if (measure  == "top_gestation") {
    yaxis_measure <- dataset$av_gest
    yaxis_plots[["title"]] <- "Average gestation at termination (completed wks)"
    yaxis_plots[["range"]] <- c(0, 10)  # forcing range from 0 to 10 weeks
    tooltip_top <- c(paste0(tool_tip_split,dataset$category,"<br>",
                            "Month: ",dataset$month,"<br>",
                            "Average gestation at termination: ",format(dataset$av_gest,digits = 1,nsmall=1)," weeks"))
  }

  #adjust datasets accordig to which data split to be displayed
  if(split == "age"){
    dataset <- dataset %>%
      mutate(category = factor(category, levels = c("under 20", "20-24", "25-29","30-34","35-39", "40+")))
    pallette <- pal_age}
  
  if(split == "dep"){
    dataset <- dataset %>% 
      mutate(category = factor(category, levels = c("1 - most deprived", "2", "3","4", "5 - least deprived")))
    pallette <- pal_depr}
  
  #Creating time trend plot
  plot_ly(data=dataset, x=~month, y = ~yaxis_measure) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category, 
              colors = pallette,
              text= tooltip_top, 
              hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,xaxis = xaxis_plots,
           legend = list(x = 100, y = 0.5)) %>% #position of legend
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
}



###############################################.
## Data downloads ----
###############################################.

termination_down_data <- reactive({
     top_download
})

output$download_termination_data <- downloadHandler(
  filename ="terminations_extract.csv",
  content = function(file) {
    write_csv(termination_down_data(),
              file) } 
)

###############################################.
## Commentary tab content  ----
###############################################.
output$top_commentary <- renderUI({
  tagList(
    bsButton("jump_to_top",label = "Go to data"), #this button can only be used once
    h2("Termination of pregnancy - 28th October 2020"))
})




