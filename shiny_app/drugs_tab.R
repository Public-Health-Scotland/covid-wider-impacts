#Server side for drugs tab

output$area_drugs_select<-renderUI({
  
  if(input$drug_subcategories == 'Drug treatment referrals'){
    selectizeInput("area_drugs_select", "Step 2 - Select the area of interest",
                   choices = c('Scotland','ADP','Health board'), selected = "Scotland")
  }
  
  else if (input$drug_subcategories == 'Take home Naloxone kits'){
    selectizeInput("area_drugs_select", "Step 2 - Select the area of interest",
                   choices = c('Scotland','Health board'), selected = "Scotland")
  }
})


#####server associated with location of data
output$geoname_ui_drugs <- renderUI({
  if (input$area_drugs_select=='ADP'){
    areas_summary<-ADP_names
    selectizeInput("geoname_drugs", label = 'Select an associated drugs partnership',
                   choices = c(areas_summary), selected = "")
  }
  else if (input$area_drugs_select=='Health board'){
    areas_summary<-Health_board
    selectizeInput("geoname_drugs", label = 'Select a health board',
                   choices = c(areas_summary), selected = "")
  }
  
})

output$types<-renderUI({
  if(input$drug_subcategories=='Drug treatment referrals'){
    column(8,
           radioButtons("types", label="Step 3 - Select type of referral",
                        choices = c('Drug','Alcohol','All'),selected = 'Drug'))
  }
  
  else if(input$drug_subcategories=='Take home Naloxone kits'){
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
  else if (input$area_drugs_select=='ADP'){
    location<-input$geoname_drugs
  }
  else if (input$area_drugs_select=='Health board'){
    location<-input$geoname_drugs
  }
  
  if(input$drug_subcategories=='Drug treatment referrals'){
    plot_data<-subset(DTR_July_update,(Board==location)& Type==input$types)
    trend<-plot_ly(data = plot_data, x = ~Date)
    trend<-trend %>% add_trace(y = ~ `2020 & 2021`,name='2020 & 2021',type='scatter', mode='lines', line=list(color=pal_overall[1]))
    trend<-trend %>% add_trace(y = ~ `Average 2018 & 2019`,name='Average 2018 & 2019',type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'))
    trend <- trend %>% layout(
      title = ("2020 and 2021 compared with 2018-2019 average"),
      yaxis = list(title = "Number of drug treatment referrals")
    )
    
  }
  
  else if(input$drug_subcategories=='Take home Naloxone kits'){
    plot_data<-subset(THN_by_HB,(Board==location) & (Type==input$types))
    trend<-plot_ly(data = plot_data, x =seq(1:nrow(plot_data)))
    trend<-trend %>% add_trace(y = ~ `2020 & 2021`,name='2020 & 2021',type='scatter', mode='lines', line=list(color=pal_overall[1]))
    trend<-trend %>% add_trace(y = ~ `Average 2018 & 2019`,name='Average 2018 & 2019',type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'))
    trend<-trend %>% 
      layout(
        xaxis=list(
          tickmode='array',
          tickvals=seq(1:nrow(plot_data)),
          ticktext=c(unique(plot_data$Date),'Jan','Feb','Mar')
        ))
    trend <- trend %>% layout(
      title = ("2020 and 2021 compared with 2018-2019 average"),
      xaxis=list(title='Date'),
      yaxis = list(title = "Number of THN kits")
    )
    
  }
  
})


output$Prop_barplot<-renderUI({
  if(input$drug_subcategories=='Take home Naloxone kits'){
    
    
    if (input$area_drugs_select=='Scotland'){
      location<-'Scotland'
    }
    else if (input$area_drugs_select=='ADP'){
      location<-input$geoname_drugs
    }
    else if (input$area_drugs_select=='Health board'){
      location<-input$geoname_drugs
    }
    
    output$prop_plot<-renderPlotly({
      
      plot_data<-subset(THN_by_HB,(Board==location))
      prop<-plot_ly(data = plot_data, x =seq(1:15),y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Community')],type='bar',name='Community')
      prop<-prop %>% add_trace(y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Prescribing')], name = 'Prescribing')
      prop<-prop %>% add_trace(y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Prison')], name = 'Prison')
      prop<-prop %>% add_trace(y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Ambulance')], name = 'Ambulance')
      prop <- prop %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
      prop<-prop %>% 
        layout(
          xaxis=list(
            tickmode='array',
            tickvals=seq(1:nrow(plot_data)),
            ticktext=c(paste(unique(plot_data$Date),' 2020'),'Jan 2021','Feb 2021','Mar 2021')
          ))
      prop <- prop %>% layout(
        title = ("Comparison of the number of kits provided by each source"),
        xaxis=list(title='Date'),
        yaxis = list(title = "Proportion")
      )
    })
    
    plotlyOutput('prop_plot')
    
  }
  
})


output$PercentChange<-renderPlotly({
  
  if (input$area_drugs_select=='Scotland'){
    location<-'Scotland'
  }
  else if (input$area_drugs_select=='ADP'){
    location<-input$geoname_drugs
  }
  else if (input$area_drugs_select=='Health board'){
    location<-input$geoname_drugs
  }
  
  
  if(input$drug_subcategories=='Drug treatment referrals'){
    
    plot_data<-subset(DTR_July_update,(Board==location)& Type==input$types)
    change<-plot_ly(data = plot_data, x = ~Date, y = ~Change,type='scatter', mode='lines',line=list(color=pal_overall[1]))
    
    change <- change %>% layout(
      title = ("Percentage change in drug treatment referrals in Scotland compared with the corresponding time in 2018-2019"),
      yaxis = list(title = "% Change")
    )
    
  }
  
  else if(input$drug_subcategories=='Take home Naloxone kits'){
    
    plot_data<-subset(THN_by_HB,(Board==location) & (Type==input$types))
    change<-plot_ly(data = plot_data,x =seq(1:nrow(plot_data)), y = ~Change,type='scatter', mode='lines',line=list(color=pal_overall[1]))
    change<-change %>% 
      layout(
        xaxis=list(
          tickmode='array',
          tickvals=seq(1:nrow(plot_data)),
          ticktext=c(unique(plot_data$Date),'Jan','Feb','Mar')
        ))
    change <- change %>% layout(
      title = ("Percentage change in supply of THN kits in Scotland compared with the corresponding time in 2018-2019"),
      yaxis = list(title = "% Change"),
      xaxis = list(title = "Date")
    )
    
  }
  
})
  
  #### Data for download ####
  
  

output$download_drugs_data <- downloadHandler(
  filename ="drugs_extract.csv",
  content = function(file) {
    
    if(input$drug_subcategories=='Drug treatment referrals'){
      
      write_csv(DTR_July_update,
                file) } 
    
    
    else if(input$drug_subcategories=='Take home Naloxone kits'){
      write_csv(THN_by_HB,
                file) } 
    
  }
)
  
  



