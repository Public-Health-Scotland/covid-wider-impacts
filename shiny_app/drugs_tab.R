#Server side for drugs tab

output$area_drugs_select<-renderUI({
  
  if(input$drug_subcategories == 'Drug and alcohol treatment referrals'){
    selectizeInput("area_drugs_select", "Step 2 - Select the area of interest",
                   choices = c('Scotland','NHS Board','Alcohol and drug partnership'), selected = "Scotland")
  }
  
  else if (input$drug_subcategories == 'Take home naloxone kits'){
    selectizeInput("area_drugs_select", "Step 2 - Select the area of interest",
                   choices = c('Scotland','NHS Board'), selected = "Scotland")
  }
})


#####server associated with location of data
output$geoname_ui_drugs <- renderUI({
  if (input$area_drugs_select=='Alcohol and drug partnership'){
    areas_summary<-ADP_names
    selectizeInput("geoname_drugs", label = 'Select an alcohol and drug partnership',
                   choices = c(areas_summary), selected = "")
  }
  else if (input$area_drugs_select=='NHS Board'){
    areas_summary<-Health_board
    selectizeInput("geoname_drugs", label = 'Select NHS Board',
                   choices = c(areas_summary), selected = "")
  }
  
})

output$types<-renderUI({
  if(input$drug_subcategories=='Drug and alcohol treatment referrals'){
    column(8,
           radioButtons("types", label="Step 3 - Select type of referral",
                        choices = c('Drug','Alcohol','All'),selected = 'Drug'))
  }
  
  else if(input$drug_subcategories=='Take home naloxone kits'){
    column(8,
           radioButtons("types", label="Step 3 - Select source of supply",
                        choices = c('All','Community','Prescribing','Prison'),selected = 'All'))
  }
})




output$TwoYrComparison<-renderPlotly({
  
  ###DTR section###
  
  if (input$area_drugs_select=='Scotland'){
    location<-'Scotland'
  }
  else if (input$area_drugs_select=='Alcohol and drug partnership'){
    location<-input$geoname_drugs
  }
  else if (input$area_drugs_select=='NHS Board'){
    location<-input$geoname_drugs
  }
  
  if(input$drug_subcategories=='Drug and alcohol treatment referrals'){
    plot_data<-subset(DTR_July_update,(Board==location)& Type==input$types)
    trend<-plot_ly(data = plot_data, x = ~Date)
    trend<-trend %>% add_trace(y = ~ `2020 & 2021`,name='2020 & 2021',type='scatter', mode='lines', line=list(color=pal_overall[1]))
    trend<-trend %>% add_trace(y = ~ `Average 2018 & 2019`,name='Average 2018 & 2019',type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'))
    trend <- trend %>% layout(
      title = (sprintf("%s treatment referrals in %s in 2020 and 2021 compared with 2018-19 average",input$types,location)),
      yaxis = list(title = "Number of referrals"),
      hovermode= 'x unified'
    )
    trend <- trend %>%  config(
      displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                            'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                            'hoverClosestCartesian', 'zoom2d', 'pan2d'))
    
  }
  
  else if(input$drug_subcategories=='Take home naloxone kits'){
    plot_data<-subset(THN_by_HB,(Board==location) & (Type==input$types))
    trend<-plot_ly(data = plot_data, x =seq(1:nrow(plot_data)))
    trend<-trend %>% add_trace(y = ~ `2020 & 2021`,name='2020 & 2021',type='scatter', mode='lines', line=list(color=pal_overall[1]))
    trend<-trend %>% add_trace(y = ~ `Average 2018 & 2019`,name='Average 2018 & 2019',type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'))
    trend<-trend %>% layout(
          xaxis=list(
          tickmode='array',
          tickvals=seq(1:nrow(plot_data)),
          ticktext=c(paste(unique(plot_data$Date),'2020',sep=' '),'Jan 2021','Feb 2021','Mar 2021')
        ))
    trend <- trend %>% layout(
      title = (sprintf("Monthly take home naloxone supply in %s, 2020 and 2021 compared with 2018-19 average (%s)",location,input$types)),
      xaxis=list(title='Date'),          
      yaxis = list(title = "Number of THN kits",tickformat=',d'),
      hovermode= 'x unified'
    )
    trend <- trend %>%  config(
      displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                            'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                            'hoverClosestCartesian', 'zoom2d', 'pan2d'))
    
  }
  
})


output$Prop_barplot<-renderUI({
  if(input$drug_subcategories=='Take home naloxone kits'){
    
    
    if (input$area_drugs_select=='Scotland'){
      location<-'Scotland'
    }
    else if (input$area_drugs_select=='Alcohol and drug partnership'){
      location<-input$geoname_drugs
    }
    else if (input$area_drugs_select=='NHS Board'){
      location<-input$geoname_drugs
    }
    
    output$prop_plot<-renderPlotly({
      
      plot_data<-subset(THN_by_HB,(Board==location))
      prop<-plot_ly(data = plot_data, x =seq(1:15),y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Community')],type='bar',name='Community')
      prop<-prop %>% add_trace(y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Prescribing')], name = 'Prescribing')
      prop<-prop %>% add_trace(y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Prison')], name = 'Prison')
      prop<-prop %>% add_trace(y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Ambulance')], name = 'Scottish ambulance service')
      prop <- prop %>% layout(
        separators = '.,', 
        yaxis = list(title = 'Count'),
        barmode = 'stack',
            xaxis=list(
            tickmode='array',
            tickvals=seq(1:nrow(plot_data)),
            ticktext=c(paste(unique(plot_data$Date),' 2020'),'Jan 2021','Feb 2021','Mar 2021')
          ))
      prop <- prop %>% layout(
        title = ("Percentage of take home naloxone kits provided by each source"),
        xaxis=list(title='Date'),
        yaxis = list(title = "Percentage (%)",
                     hoverformat='.1f')
      )
      prop <- prop %>%  config(
        displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                              'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                              'hoverClosestCartesian', 'zoom2d', 'pan2d'))
    })
    
    plotlyOutput('prop_plot')
    
  }
  
})


output$PercentChange<-renderPlotly({
  
  if (input$area_drugs_select=='Scotland'){
    location<-'Scotland'
  }
  else if (input$area_drugs_select=='Alcohol and drug partnership'){
    location<-input$geoname_drugs
  }
  else if (input$area_drugs_select=='NHS Board'){
    location<-input$geoname_drugs
  }
  
  
  if(input$drug_subcategories=='Drug and alcohol treatment referrals'){
    
    plot_data<-subset(DTR_July_update,(Board==location)& Type==input$types)
    change<-plot_ly(data = plot_data, x = ~Date, y = ~Change,type='scatter', mode='lines',line=list(color=pal_overall[1]))
    
    change <- change %>% layout(
      title = (sprintf("Percentage change in %s treatment referrals in %s compared with the corresponding time in 2018-2019",tolower(input$types),location)),
      yaxis = list(title = "% Change",
                   hoverformat='.1f')
    )
    change <- change %>%  config(
      displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                            'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                            'hoverClosestCartesian', 'zoom2d', 'pan2d'))
    
  }
  
  else if(input$drug_subcategories=='Take home naloxone kits'){
    
    plot_data<-subset(THN_by_HB,(Board==location) & (Type==input$types))
    change<-plot_ly(data = plot_data,x =seq(1:nrow(plot_data)), y = ~Change,type='scatter', mode='lines',line=list(color=pal_overall[1]))
    change<-change %>% 
      layout(
        xaxis=list(
          tickmode='array',
          tickvals=seq(1:nrow(plot_data)),
          ticktext=c(paste(unique(plot_data$Date),'2020',sep=' '),'Jan 2021','Feb 2021','Mar 2021')
        ))
    change <- change %>% layout(
      title = (sprintf("Percentage change in supply of take home naloxone kits in %s compared with the corresponding time in 2018-2019 (%s)",location,input$types)),
      yaxis = list(title = "% Change",
                   hoverformat='.1f'
                   ),
      xaxis = list(title = "Date")
    )
    change <- change %>%  config(
      displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                            'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                            'hoverClosestCartesian', 'zoom2d', 'pan2d'))
    
  }
  
})
  
  #### Data for download ####
  
  

output$download_drugs_data <- downloadHandler(
  filename ="drugs_extract.csv",
  content = function(file) {
    
    if(input$drug_subcategories=='Drug and alcohol treatment referrals'){
      
      write_csv(DTR_July_update,
                file) } 
    
    
    else if(input$drug_subcategories=='Take home naloxone kits'){
      write_csv(THN_by_HB,
                file) } 
    
  }
)
  
  



