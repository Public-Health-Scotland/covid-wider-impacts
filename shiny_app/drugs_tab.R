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
  
  output$TwoYrComparison<-renderPlot({
    
    if(input$drug_subcategories=='Drug treatment referrals'){
      ggplot(subset(DTR_drug_data,(Board==input$geoname_drugs)& Type==input$DTR_types))+
        geom_line(aes(x=Week,y=DTR,col=Year))+
        ggtitle('Number of treatment referrals')
    }
    
  })
  
  output$PercentChange<-renderPlot({
    
    if(input$drug_subcategories=='Drug treatment referrals'){
      ggplot(subset(sub.full,(Board==input$geoname_drugs)& Type==input$DTR_types))+
        geom_line(aes(x=Week,y=Change))+
        ylab('% Change')+
        ggtitle('% Change from 2018/2019 to 2020')+
        geom_hline(yintercept=0)
    }
  })
  
  
  
  
