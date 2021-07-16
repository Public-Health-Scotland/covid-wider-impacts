#Server side for drugs tab


output$drug_subcategories<-renderUI({
  if(input$drug_categories=='Drug harms'){
    radioGroupButtons("drug_subcategories",
                      label= "Select a drug harm",
                      choices = c('Drug treatment referrals', 'Emergency naloxone administration by SAS'), status = "primary",
                      direction = "vertical", justified = T)
  }
  
  else if(input$drug_categories=='Drug services'){
    radioGroupButtons("drug_subcategories",
                      label= "Select a drugs service",
                      choices = c('OST prescribing', 'Take home Naloxone kits'), status = "primary",
                      direction = "vertical", justified = T)
  }
})

output$drugs_types<-renderUI({
  if(input$drug_subcategories=='Drug treatment referrals'){
    column(8,
           radioButtons("DTR_types", label="Step 3 - Select type of referral",
                        choices = c('All','Drug','Alcohol','Co-dependency'),selected = 'All'))
  }
})



output$geoname_ui_drugs <- renderUI({
  
  if( input$area_drugs_select=='Scotland'){
    areas_summary<-'Scotland'
  }
  else if (input$area_drugs_select=='ADP'){
    areas_summary<-Hb
  }#Hb is in the HBdata file extracted from the ADP level data 
  
  selectizeInput("geoname_drugs", label = NULL,
                 choices = c(areas_summary), selected = "")
  
})

output$TwoYrComparison<-renderPlotly({
  
  if(input$drug_subcategories=='Drug treatment referrals'){
    plot_data<-subset(DTR_drug_data2,(Board==input$geoname_drugs)& Type==input$DTR_types)
    trend<-plot_ly(data = plot_data, x = ~Date, y = ~DTR,color=~Year, mode='lines', colors=c(pal_overall[2],pal_overall[1]),dash=c('solid','dash'))
    
    trend <- trend %>% layout(
      title = ("2020 and 2021 compared with 2018-2019 average"),
      yaxis = list(title = "Number of drug treatment referrals")
    )
    
  }
  
})

output$PercentChange<-renderPlotly({
  
  if(input$drug_subcategories=='Drug treatment referrals'){
    plot_data<-subset(long.axis,(Board==input$geoname_drugs)& Type==input$DTR_types)
    change<-plot_ly(data = plot_data, x = ~Date, y = ~Change, mode='lines')
    
    change <- change %>% layout(
      title = ("Percentage change in drug treatment referrals in Scotland compared with the corresponding time in 2018-2019"),
      yaxis = list(title = "% Change")
    )
    
  }
})



