##Server script for perinatal tab


# Pop-up modal explaining source of data
observeEvent(input$btn_perinatal_modal,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p("The information shown for rates of perinatal mortality is taken from the",
                 tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/deaths-background-information/stillbirths-and-infant-deaths",
                        "National Records for Scotland",class="externallink")),
               p("This NRS page provides information which is specifically about the statistics of
                 perinatal deaths, neonatal deaths, post-neonatal deaths and infant deaths."),
               p(tags$a(href="https://publichealthscotland.scot/","Public Health Scotland (PHS)",class="externallink"),
                 " routinely receives quarterly data extracts from the SMR02 for the purpose of producing and ",
                 (tags$a(href="https://www.isdscotland.org/Health-Topics/Maternity-and-Births/Publications/","publishing",class="externallink")),"
                 information on birth rates."),
               p("Rates based on small numbers are prone to fluctuation. Therefore in boards
                 with smaller numbers of births it is important to consider this when interpreting the rates."),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))
# 
# ###############################################.
# ## Perinatal Reactive controls  ----
# ###############################################.

# Perinatal reactive drop-down control showing list of area names depending on areatype selected 

#  FOR WHEN WE HAVE NHS BOARD DATA
# output$geoname_ui_perinatal <- renderUI({
# 
#   #Lists areas available in
#   areas_summary_perinatal <- sort(geo_lookup$areaname[geo_lookup$areatype == input$geotype_perinatal])
# 
#   selectizeInput("geoname_perinatal", label = NULL, choices = areas_summary_perinatal, selected = "")
# })

#  p_perinatal_table_data <- reactive({ # may add tables to tab
#    table <- p_perinatal_table 
# })
#  
#  u_perinatal_table_data <- reactive({ # may add tables to tab
#    table <- u_perinatal_table 
#  })
# 
# 
# 
# ###############################################.
# ## perinatal Tab Reactive layout  ----
# ###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$perinatal_explorer <- renderUI({

  # text for titles of cut charts
  perinatal_title <- paste0(case_when(input$measure_select_perinatal == "stillbirths" ~ paste0("Rate of stillbirths per 1000 births in Scotland",
                                                                                       input$geoname_perinatal),
                                      input$measure_select_perinatal == "pnnd" ~ paste0("Rate of post-neonatal deaths per 1000 births in Scotland",
                                                                                        input$geoname_perinatal),
                                      input$measure_select_perinatal == "nnd" ~ paste0("Rate of neonatal deaths per 1000 births in Scotland",
                                                                                    input$geoname_perinatal),
                                      input$measure_select_perinatal == "extperi" ~ paste0("Rate of extended perinatal deaths per 1000 births in Scotland",
                                                                                    input$geoname_perinatal),
                                      input$measure_select_perinatal == "infantdeaths" ~ paste0("Rate of infant deaths per 1000 births in Scotland",
                                                                                           input$geoname_perinatal)))

  # Intro paragraph within perinatal tab
  intro_stillbirths <- p("Stillbirths refer to", tags$a(href="https://www.healthcareimprovementscotland.org/our_work/reproductive,_maternal__child/reproductive_health/spimmr_2012.aspx", "a child born after the 28th week of pregnancy which did not breathe or show any
                         signs of life.",class="externallink"), "The stillbirth rate in Scotland in 2018", tags$a(href="https://www.scotpho.org.uk/population-dynamics/pregnancy-births-and-maternity/key-points/", 
                        "was 3.7 per 1,000 total births.",class="externallink"), "It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                        "NHS Scotland and Scottish Government", tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                        "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.",
                        p("Confidence intervals indicate a range of values of which it is likely the true value lies within."))
                        

  intro_pnnd <- p("Post-neonatal deaths refer to", tags$a(href="https://www.healthcareimprovementscotland.org/our_work/reproductive,_maternal__child/reproductive_health/spimmr_2012.aspx", "deaths occuring after the first 4 weeks but within the first year",class="externallink"), "of life.
                  It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                  "NHS Scotland and Scottish Government", tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                  "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.",
                  p("Confidence intervals indicate a range of values of which it is likely the true value lies within."))
  
  intro_nnd <- p("Neonatal deaths refer to", tags$a(href="https://www.healthcareimprovementscotland.org/our_work/reproductive,_maternal__child/reproductive_health/spimmr_2012.aspx", "deaths in the first four weeks",class="externallink"), "of life.
                 It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                 "NHS Scotland and Scottish Government", tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                  "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.",
                 p("Confidence intervals indicate a range of values of which it is likely the true value lies within."))
  
  intro_extperi <- p("Extended perinatal deaths refer to", tags$a(href="https://www.healthcareimprovementscotland.org/our_work/reproductive,_maternal__child/reproductive_health/spimmr_2012.aspx", "the sum of stillbirths and neonatal mortality",class="externallink"), "(deaths within the first 28 days of life).
                     It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                     "NHS Scotland and Scottish Government", tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                      "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.",
                     p("Confidence intervals indicate a range of values of which it is likely the true value lies within."))
  
  intro_infantdeaths <- p("Infant deaths refer to", tags$a(href="https://www.healthcareimprovementscotland.org/our_work/reproductive,_maternal__child/reproductive_health/spimmr_2012.aspx", "all deaths in the first year",class="externallink"), "of life.
                          It is important to monitor the rate of perinatal mortality during the Covid-19 pandemic.",
                          "NHS Scotland and Scottish Government", tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies/",
                          "have produced guidelines",class="externallink"), "for attending antenatal care appointments during the pandemic.",
                        p("Confidence intervals indicate a range of values of which it is likely the true value lies within."))
                  

  # Specify items to display in perinatal ui based on step 2 selection

if (input$measure_select_perinatal == "stillbirths") {
  tagList(
    fluidRow(column(12, renderUI(intro_stillbirths),
                    h4(paste0(perinatal_title)))),
    fluidRow(withSpinner(plotlyOutput("perinatal_scatter_p"))))#,
             #column(6, uiOutput("p_perinatal_table"))) #add potential table later
  
}  else if (input$measure_select_perinatal == "pnnd"){
  tagList(
    fluidRow(column(12, renderUI(intro_pnnd),
                    h4(paste0(perinatal_title)))),
    fluidRow(withSpinner(plotlyOutput("perinatal_scatter_p"))))#,
             #column(6, uiOutput("p_perinatal_table"))) #add potential table later
  
}  else if (input$measure_select_perinatal == "nnd"){
  tagList(
    fluidRow(column(12, renderUI(intro_nnd),
                    h4(paste0(perinatal_title)))),
    fluidRow(withSpinner(plotlyOutput("perinatal_scatter_p"))))#,
             #column(6, uiOutput("p_perinatal_table"))) #add potential table later
  
} else if (input$measure_select_perinatal == "extperi"){
  tagList(
    fluidRow(column(12, renderUI(intro_extperi),
                    h4(paste0(perinatal_title)))),
    fluidRow(withSpinner(plotlyOutput("perinatal_scatter_p"))))#,
             #column(6, uiOutput("p_perinatal_table"))) #add potential table later
  
} else if (input$measure_select_perinatal == "infantdeaths"){
  tagList(
    fluidRow(column(12, renderUI(intro_infantdeaths),
                    h4(paste0(perinatal_title)))),
    fluidRow(withSpinner(plotlyOutput("perinatal_scatter_u"))))#,
             #column(6, uiOutput("u_perinatal_table"))) #add potential table later   

} #end of if statements
  
}) #close perinatal_explorer function
  
  ###############################################.
  ## Charts ----
  ###############################################.
  
  #run chart function to generate p charts
  output$perinatal_scatter_p <- renderPlotly({
    p_perinatal_filter <- p_perinatal %>% filter(type == input$measure_select_perinatal)
    
    yaxis_plots[["title"]] <- "Rate per 1000 births"
    xaxis_plots[["title"]] <- "Month"
    
    plot_ly(data = p_perinatal_filter, x = ~date) %>%
      add_lines(y = ~rate, line = list(color = "black"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Rate") %>%
      add_lines(y = ~centreline, line = list(color = "purple"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Centreline") %>%
      add_lines(y = ~upper_cl_3_std_dev, line = list(color = "red", dash = "dot"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Upper confidence interval to 3 stdev") %>%
      add_lines(y = ~lower_cl_3_std_dev, line = list(color = "red", dash = "dot"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Lower confidence interval to 3 stdev") %>%
      add_lines(y = ~upper_wl_2_std_dev, line = list(color = "blue", dash = "dot"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Upper confidence interval to 2 stdev") %>%
      add_lines(y = ~lower_wl_2_std_dev, line = list(color = "blue", dash = "dot"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Lower confidence interval to 2 stdev") %>%
      
      layout( #to avoid labels getting cut out
        yaxis = yaxis_plots, xaxis = xaxis_plots) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
    
  })
  
  #run chart function to generate p charts
  output$perinatal_scatter_u <- renderPlotly({
    u_perinatal_filter <- u_perinatal %>% filter(type == input$measure_select_perinatal) 
    
    yaxis_plots[["title"]] <- "Rate per 1000 births"
    xaxis_plots[["title"]] <- "Month"
    
    plot_ly(data = u_perinatal_filter, x = ~date) %>%
      add_lines(y = ~rate, line = list(color = "black"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Rate") %>%
      add_lines(y = ~centreline, line = list(color = "purple"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Centreline") %>%
      add_lines(y = ~upper_cl_3_std_dev, line = list(color = "red", dash = "dot"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Upper confidence interval to 3 stdev") %>%
      add_lines(y = ~lower_cl_3_std_dev, line = list(color = "red", dash = "dot"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Lower confidence interval to 3 stdev") %>%
      add_lines(y = ~upper_wl_2_std_dev, line = list(color = "blue", dash = "dot"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Upper confidence interval to 2 stdev") %>%
      add_lines(y = ~lower_wl_2_std_dev, line = list(color = "blue", dash = "dot"),
                #text=tooltip_trend, hoverinfo="text",
                name = "Lower confidence interval to 2 stdev") %>%
      
      layout( #to avoid labels getting cut out
        yaxis = yaxis_plots, xaxis = xaxis_plots) %>% #position of legend
      # leaving only save plot button
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
    
  })


###############################################.
## Commentary ----
###############################################.
output$perinatal_commentary <- renderUI({
  tagList(h2("Perinatal mortality - 1st July 2020"),
          p("Placeholder")
            )
})

##END


