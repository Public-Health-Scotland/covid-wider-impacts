# Wider impacts dashboard - Births and babies - method of delivery section
# Server code

# Pop-up modal explaining source of data
observeEvent(input$`mod-source-modal`,
             showModal(modalDialog(
               title = "What is the data source?",
               p("The data used for the method of delivery page comes from the Scottish Morbidity Record 02 (SMR02) database.  An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care.  From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."),
               p("For the method of delivery page, SMR02 records for episodes of care involving the delivery of a singleton live birth (i.e. one baby, not twins or more) at any gestation have been used.  The charts presented show the total number of singleton live births, and the number and percentage with the different methods of delivery, in each month from January 2018 onwards.  Method of delivery has been categorised as spontaneous vaginal delivery; assisted vaginal delivery (including forceps, ventouse, and vaginal breech deliveries); elective (i.e. planned) caesarean section; and emergency caesarean section.  The month is based on the date the woman was discharged from hospital after delivery.  Data is shown at all Scotland level, and for women living in each mainland NHS Board area.  Due to small numbers, the charts for individual Island Boards of residence (NHS Orkney, NHS Shetland, and NHS Western Isles) are unstable so these have not been shown.  However, the Island Boards are included in the Scotland total, and data for the Island Boards is available in the spreadsheet provided through the ‘Download data’ button."),
               p("Data is shown for up to and including the most recent month for which SMR02 records are considered near complete.  Data for the most recent months should be viewed as provisional.  Data for all months will be refreshed every time the dashboard page is updated, and data for the most recent months is likely to change slightly as additional SMR02 records are submitted to PHS."),
               p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high.  For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland.  In addition, the recording of method of delivery is very complete.  For the period 1 April 2019 to 31 March 2020, method of delivery was recorded on 99.9% of SMR02 records relating to singleton live births."),
               p("Further information based on SMR02 data is also available from the annual ",
                 tags$a(href="https://publichealthscotland.scot/publications/births-in-scottish-hospitals", "Births in Scottish Hospitals report",class="externallink",target="_blank"),"."),
               size = "m",easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

# Modal to explain run charts rules
observeEvent(input$btn_mod_rules, runchart_modal())
#Modal to explain SIMD and deprivation
observeEvent(input$btn_modal_simd_mod, simd_modal("Women"))

###############################################.
## Reactive controls  ----
###############################################.
# Show list of area names depending on areatype selected
geoname_server("mod") 

###############################################.
##  Reactive datasets  ----
###############################################.

#Dataset 1: behind trend run chart  (available at scotland and NHS board level)
mod_filter <- function(){

  mod_runchart %>% filter(area_name == input$`mod-geoname` &
                            area_type == input$`mod-geotype` &
                            type %in% c("Scotland","Health board"))
}

#Dataset 2: behind line charts for age and deprivation (available for scotland only)
mod_linechart_split <- function(split){

  mod_scot  %>% filter(area_name == "Scotland" &
                    area_type == "Scotland" &
                    type==split)
}

#Dataset 3: behind line chart (available at scotland and NHS board level)
mod_linechart_filter <- function(){

  mod_linechart %>% filter(area_name == input$`mod-geoname` &
                   area_type == input$`mod-geotype` &
                   type %in% c("Scotland","Health board"))
}

###############################################.
## Mode of delivery Charts ----
###############################################.

# chart outputs for trend runcharts
output$mod_trend_csection_all <- renderPlotly({plot_mod_trend(measure="perc_csection_all", shift = "csection_all_shift", trend = "csection_all_trend")})
output$mod_trend_csection_elec <- renderPlotly({plot_mod_trend(measure="perc_csection_elec", shift = "csection_elec_shift", trend = "csection_elec_trend")})
output$mod_trend_csection_emer <- renderPlotly({plot_mod_trend(measure="perc_csection_emer", shift = "csection_emer_shift", trend = "csection_emer_trend")})

#chart outputs for line charts for NHS board and Scot
output$mod_linechart_number <- renderPlotly({plot_mod_linechart(measure="births")})
output$mod_linechart_percent <- renderPlotly({plot_mod_linechart(measure="percent_births")})

#chart outputs for line charts for Scotland only age and deprivation line charts
output$mod_linechart_age_n <- renderPlotly({plot_mod_split(dataset=mod_linechart_split(split="age"),split="age", measure="csection_all")})
output$mod_linechart_age_p <- renderPlotly({plot_mod_split(dataset=mod_linechart_split(split="age"),split="age", measure="perc_csection_all")})
output$mod_linechart_dep_n <- renderPlotly({plot_mod_split(dataset=mod_linechart_split(split="dep"),split="dep", measure="csection_all")})
output$mod_linechart_dep_p <- renderPlotly({plot_mod_split(dataset=mod_linechart_split(split="dep"),split="dep", measure="perc_csection_all")})

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$mod_explorer <- renderUI({

  # text for titles of cut charts
  mod_data_timeperiod <-  paste0("Figures based on data extracted ",mod_extract_date)
  mod_title <- paste0("Percentage of singleton live births delivered by caesarean section: ",input$`mod-geoname`)

  chart_explanation <-
    tagList(p("We have used ",
              tags$a(href= 'https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf',
                     'run charts', target="_blank")," to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the button above: ‘How do we identify patterns in the data?’"),
            p(run_chart_description("Percentage of births by caesarean section",
                                    "the percentage of singleton live births
                                    (at all gestations) that were delivered by
                                    caesarean section in each month from January
                                    2018 onwards",
                                    "the average (median) percentage of births
                                    that were by caesarean section over the
                                    period January 2018 to February 2020
                                    inclusive (the period before the COVID-19
                                    pandemic in Scotland)",
                                    charts_plural = TRUE)))

  # Charts if data is available and one sign for the three charts if not
  if (input$`mod-geoname` %in% c("NHS Shetland", "NHS Orkney", "NHS Western Isles") ){
    charts_mod <-  tagList(fluidRow(br()),
                            column(12,
                                   br(),
                                   h5("Charts not shown as unstable due to small numbers.
                                      Data for the Island Boards is included in the data download.")))
  } else {
    charts_mod <-  tagList(
      column(4,
             h4("All caesarean sections"),
             withSpinner(plotlyOutput("mod_trend_csection_all",
                                      height = height_run_chart))),
      column(4,
             h4("Elective caesarean sections"),
             withSpinner(plotlyOutput("mod_trend_csection_elec",
                                      height = height_run_chart))),
      column(4,
             h4("Emergency caesarean sections"),
             withSpinner(plotlyOutput("mod_trend_csection_emer",
                                      height = height_run_chart))),
      column(12,
             br(), # spacing
             p(mod_data_timeperiod),
             p(chart_explanation)),
      column(12,
             br(), #spacing
             h4(paste0("Number of singleton live births by method of delivery: ",input$`mod-geoname`))),
      column(12,
             withSpinner(plotlyOutput("mod_linechart_number")))
    )
  }


  # Function to create common layout to all immunisation charts
  tagList(fluidRow(column(12,
                          h4(mod_title),
                          actionButton("btn_mod_rules", "How do we identify patterns in the data?")),
                   charts_mod,
                   #only if scotland selected display age and deprivation breakdowns
                   if (input$`mod-geotype` == "Scotland"){
                     tagList(
                       fluidRow(column(12,
                                       h4("Singleton live births delivered by caesarean section by maternal age group: Scotland"))),
                       fluidRow(column(6,
                                       h4("Number of births delivered by caesarean section"),
                                       withSpinner(plotlyOutput("mod_linechart_age_n"))),
                                column(6,
                                       h4("Percentage of births delivered by caesarean section"),
                                       withSpinner(plotlyOutput("mod_linechart_age_p")))),
                       fluidRow(column(12,
                                       br(), # spacing
                                       h4("Singleton live births delivered by caesarean section by maternal deprivation level: Scotland"),
                                       actionButton("btn_modal_simd_mod", "What is SIMD and deprivation?",
                                                    icon = icon('question-circle')))),
                       fluidRow(column(6,
                                       h4("Number of births delivered by caesarean section"),
                                       withSpinner(plotlyOutput("mod_linechart_dep_n"))),
                                column(6,
                                       h4("Percentage of births delivered by caesarean section"),
                                       withSpinner(plotlyOutput("mod_linechart_dep_p"))))
                     )#tagList from if statement
                   }

  ))
})

#############################################.
## Method of delivery chart functions ----
############################################.

## RUNCHART trend chart for monthly c-section percentages : Scotland & NHS Board (except island boards)
## Rather than try and present all the modes of delivery we have opted just to produce a run chart
## showing rates of c-section (by type all, emergency, elective) as these are the modes of deliver that people most want to see

plot_mod_trend <- function(measure, shift, trend){

  plot_data <- mod_filter()

  if (is.data.frame(plot_data) && nrow(plot_data) == 0)
  { plot_nodata(height = 50,
                text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
  } else {


    # centrelines
    centreline_name <- paste0(input$`mod-geoname`," average up to end Feb 2020")
    dottedline_name = "Projected average"

    # format y axis
    measure_name <- "Percentage of births delivered by caesarean (%)"
    yaxis_plots[["range"]] <- c(0, 50)  # forcing range from 0 to 40%
    y_label <- "Percentage of births (%)"

    #switch tooltip according to which measure is provided
    if(measure == "perc_csection_all"){
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_csection_all,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_csection_emer") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_csection_emer,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed

    } else if (measure  == "perc_csection_elec") {
      tooltip_top <- c(paste0("Month: ",format(plot_data$month, "%B %Y"),"<br>",
                              "Percentage: ",format(plot_data$perc_csection_elec,digits = 1,nsmall=1),"%", "<br>"))
      #"Number: ", plot_data$csection_all)) # number of csections have been removed from dataset? not sure if needed
    }

    # Adjust the column used for median line according to which cut of chart to be shown
    centreline_data <- case_when(measure == "perc_csection_all" ~ plot_data$median_csection_all,
                                 measure == "perc_csection_elec" ~ plot_data$median_csection_elec,
                                 measure == "perc_csection_emer" ~ plot_data$median_csection_emer)
    dottedline_data <- case_when(measure == "perc_csection_all" ~ plot_data$ext_csection_all,
                                 measure == "perc_csection_elec" ~ plot_data$ext_csection_elec,
                                 measure == "perc_csection_emer" ~ plot_data$ext_csection_emer)

    x_dates = "month"

    plot_run_chart(plot_data, measure, measure_name, y_label,
                   x_dates, shift, trend, tooltip_top,
                   xaxis_plots, yaxis_plots, bttn_remove,
                   centreline_data, centreline_name,
                   dottedline_data, dottedline_name,
                   x_buffer = 20, width_mode = "narrow")

  }}


#####################################################################################################################.
## LINECHART SCOTLAND: caesarean delivery by age group and deprivation, numbers and percentages - Scotland level only ----
plot_mod_split <- function(dataset, split, measure){

  plot_data <- dataset

  #improve grammar of label to appear in tool tip
  tool_tip_split <- case_when(split=="age" ~ paste0("Age group:"), split=="dep" ~ paste0("Deprivation group:"))

  # Create tooltip for line chart
  tooltip <- c(paste0(tool_tip_split,dataset$category,"<br>",
                      "Month: ", format(plot_data$month, "%B %Y"),"<br>",
                      "Number: ", plot_data$csection_all, "<br>",
                      "Percentage: ", format(plot_data$perc_csection_all,digits=1,nsmall = 1),"%"))

  # adjust chart y axis according to what is being displayed
  if(measure == "perc_csection_all"){
    yaxis_plots[["title"]] <- "Percentage of births (%)"
    if(split == "age"){
      yaxis_plots[["range"]] <- c(0, 70)}  # forcing range from 0 to 70% for age group
    if(split == "dep"){
      yaxis_plots[["range"]] <- c(0, 50)}  # forcing range from 0 to 40% for dep
  }
  if(measure == "csection_all"){
    yaxis_plots[["title"]] <- "Number of births"
  }

  #adjust datasets according to which data split to be displayed
  if(split == "age"){
      plot_data<- plot_data %>%
      mutate(category = factor(category, levels = c("Under 20", "20-24", "25-29","30-34","35-39", "40 and over")))
    pallette <- pal_age}

  if(split == "dep"){
    plot_data <- plot_data %>%
      mutate(category = factor(category, levels = c("1 - most deprived", "2", "3","4", "5 - least deprived")))
    pallette <- pal_depr}

  #Creating trend plot
  plot_ly(data=plot_data, x=~month,  y = ~get(measure)) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~category, colors = pallette,
              text= tooltip, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,  xaxis = xaxis_plots,
           legend = list(orientation = 'h')) %>% #position of legend underneath plot
    #leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)

}

#####################################################################################################################.
#### LINECHART SCOTLAND & NHS BOARD: Births by mode of delivery numbers and percentages ----

plot_mod_linechart <- function(measure){

plot_data <- mod_linechart_filter()

pallette <- pal_age

# adjust chart y axis according to what is being displayed
if(measure == "percent_births"){
  yaxis_plots[["title"]] <- "Percentage of births (%)"
  plot_data <- plot_data %>%  #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?
    filter(mode!="All births")
}

if(measure == "births"){
  yaxis_plots[["title"]] <- "Number of births"
  plot_data <- plot_data #%>% #exclude the "all" category - definitely don't want in % chart but maybe want in numbers chart?

}

# Create tooltip for line chart
tooltip <- c(paste0("Mode of delivery: ", plot_data$mode,"<br>",
                    "Area: ",plot_data$area_name,"<br>",
                    "Month: ",  format(plot_data$month, "%B %Y"),"<br>",
                    "Number of births: ", plot_data$births,"<br>",
                    "Percentage of births: ", format(plot_data$percent_births,digits = 1,nsmall=1),"%"))

if (is.data.frame(plot_data) && nrow(plot_data) == 0)
{ plot_nodata(height = 50,
              text_nodata = "Chart not shown as unstable due to small numbers. Data for the Island Boards is included in the data download.")
} else {

  #Creating trend plot
  plot_ly(data=plot_data, x=~month,  y = ~get(measure)) %>%
    add_trace(type = 'scatter', mode = 'lines',
              color = ~mode, colors = pallette,
              text= tooltip, hoverinfo="text") %>%
    #Layout
    layout(margin = list(b = 80, t=5), #to avoid labels getting cut out
           yaxis = yaxis_plots,  xaxis = xaxis_plots,
           legend = list(orientation = 'h')) %>% #position of legend underneath plot
    #leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)}
}

###############################################.
## Data downloads ----
###############################################.

mod_download_data <- reactive({
  mod_download %>%
    rename(assisted_vaginal = assisted_vaginal_inc_breech, spontaneous = spontaneous_vaginal,
           perc_assisted_vaginal = perc_assisted_vaginal_inc_breech,
           perc_spontaneous = perc_spontaneous_vaginal)
})

output$download_mod_data <- downloadHandler(
  filename ="mode_of_delivery_extract.csv",
  content = function(file) {
    write_csv(mod_download_data(),
              file) }
)


##END
