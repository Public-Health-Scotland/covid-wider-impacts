#Server side for drugs tab
observeEvent(input$btn_drugs_modal, 
             if(input$drug_subcategories == 'Drug and alcohol treatment referrals'){
             showModal(modalDialog(
               title = "What is the data source?",
              p('referrals'),
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
             else if(input$drug_subcategories == 'Take home naloxone kits'){
              showModal(modalDialog(
                 title = "What is the data source?",
              p('naloxone'),
              easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
               )

output$area_drugs_select<-renderUI({
  
  if(input$drug_subcategories == 'Drug and alcohol treatment referrals'){
    selectizeInput("area_drugs_select", "Step 2 - Select the area of interest",
                   choices = c('Scotland','NHS Board','Alcohol and Drug Partnership'), selected = "Scotland")
  }
  
  else if (input$drug_subcategories == 'Take home naloxone kits'){
    selectizeInput("area_drugs_select", "Step 2 - Select the area of interest",
                   choices = c('Scotland','NHS Board'), selected = "Scotland")
  }
})


#####server associated with location of data
output$geoname_ui_drugs <- renderUI({
  if (input$area_drugs_select=='Alcohol and Drug Partnership'){
    areas_summary<-ADP_names
    selectizeInput("geoname_drugs", label = 'Select an Alcohol and Drug Partnership',
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
                        choices = c('All','Drug','Alcohol'),selected = 'All'))
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
  else if (input$area_drugs_select=='Alcohol and Drug Partnership'){
    location<-input$geoname_drugs
  }
  else if (input$area_drugs_select=='NHS Board'){
    location<-input$geoname_drugs
  }
  
  if(input$drug_subcategories=='Drug and alcohol treatment referrals'){
    plot_data<-subset(DTR_July_update,(Board==location)& Type==input$types & Date<'2021-06-28')#cutting off in line with most recent covid report
    complete_data<-subset(plot_data,Date<='2021-05-24')
    incomplete_data<-subset(plot_data,Date>='2021-05-24')
    trend<-plot_ly(data = complete_data, x = ~Date,y = ~ `2020 & 2021`,name='20/21',type='scatter', mode='lines', line=list(color=pal_overall[1]))
    trend<-trend %>% add_trace(data=incomplete_data, x=~Date,y=~`2020 & 2021`,type='scatter',name='20/21 (Incomplete)',mode='lines',line=list(color=pal_overall[1],dash='dash'),showlegend=FALSE)
    trend<-trend %>% add_trace(data=plot_data,x=~Date,y = ~ `Average 2018 & 2019`,name='Average 18/19',type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'))
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
    trend<-trend %>% add_trace(y = ~ `2020 & 2021`,name='20/21',type='scatter', mode='lines', line=list(color=pal_overall[1]))
    trend<-trend %>% add_trace(y = ~ `Average 2018 & 2019`,name='Average 18/19',type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'))
    trend<-trend %>% layout(
          xaxis=list(
          tickmode='array',
          tickvals=seq(1:nrow(plot_data)),
          ticktext=c(paste(unique(plot_data$Date),'2020',sep=' '),'Jan 2021','Feb 2021','Mar 2021')
          
        ),
        hovermode= 'x unified'
        )
    trend <- trend %>% layout(
      title = (sprintf("Monthly take home naloxone supply in %s, 2020 and 2021 compared with 2018-19 average (%s)",location,input$types)),
    
      xaxis=list(title='Date'),          
      yaxis = list(title = "Number of THN kits")
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
    else if (input$area_drugs_select=='Alcohol and Drug Partnership'){
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
      prop<-prop %>% add_trace(y = plot_data$`Proportion 20/21`[which(plot_data$Type=='Ambulance')], name = 'SAS')
      prop <- prop %>% layout(
        #separators = '.,', 
        barmode = 'stack',
            xaxis=list(
            tickmode='array',
            tickvals=seq(1:nrow(plot_data)),
            ticktext=c(paste(unique(plot_data$Date),' 2020'),'Jan 2021','Feb 2021','Mar 2021')
          ))
      prop <- prop %>% layout(
        title = ("Percentage of take home naloxone kits provided by each source"),
        xaxis=list(title='Date'),
        yaxis = list(title = "Percentage (%)"),
                    # hoverformat=',.0%'),
        hovermode= 'x unified'
      )
      prop <- prop %>%  config(
        displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                              'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                              'hoverClosestCartesian', 'zoom2d', 'pan2d'))
    })
    
    plotlyOutput('prop_plot')
    
  }
  
})


output$PercentChange<-renderUI({
  
  if (input$area_drugs_select=='Scotland'){
    location<-'Scotland'
  }
  else if (input$area_drugs_select=='Alcohol and Drug Partnership'){
    location<-input$geoname_drugs
  }
  else if (input$area_drugs_select=='NHS Board'){
    location<-input$geoname_drugs
  }
  
  
  
  if(input$drug_subcategories=='Drug and alcohol treatment referrals'){
    
    plot_data<-subset(DTR_July_update,(Board==location) & Type==input$types)
    
    if(length(which(is.na(plot_data$Change)))==0){
      
      output$change_plot<-renderPlotly({
    
    tooltip_trend<-c(paste0(
                          "Date: ", plot_data$Date,
                          "<br>", "Change from 2018 - 2019 average: ", plot_data$Change, "%"))
    change<-plot_ly(data = plot_data, x = ~Date, y = ~Change,
                    type='scatter', 
                    mode='lines',
                    line=list(color=pal_overall[1]),
                    text=tooltip_trend, 
                    hoverinfo="text")
    
    change <- change %>% layout(
      title = (sprintf("Percentage change in %s treatment referrals in %s compared with the corresponding time in 2018-2019",tolower(input$types),location)),
      yaxis = list(title = "% Change"),
      hovertemplate = "%{Date}: <br> Change from 2018-19 average: %{Change}"
    )
    change <- change %>%  config(
      displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                            'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                          'hoverClosestCartesian', 'zoom2d', 'pan2d'))
      })  
      plotlyOutput('change_plot')
    }
    else if(length(which(is.na(plot_data$Change)))!=0){
      
      output$data_message<-renderText('Percent change plot not shown due to NA values being produced by comparison with 0 values in 2018/2019 average')
      textOutput('data_message')
    }
  }
  
  else if(input$drug_subcategories=='Take home naloxone kits'){
    
    plot_data<-subset(THN_by_HB,(Board==location) & (Type==input$types))
    if(length(which(is.na(plot_data$Change)))==0){
    
      output$change_plot<-renderPlotly({
      
    tooltip_trend<-c(paste0(
      "Month: ", c(paste(unique(plot_data$Date),'2020',sep=' '),'Jan 2021','Feb 2021','Mar 2021'),
      "<br>", "Change from 2018 - 2019 average: ", plot_data$Change, "%"))
    change<-plot_ly(data = plot_data,x =seq(1:nrow(plot_data)), y = ~Change,
                    type='scatter', 
                    mode='lines',
                    line=list(color=pal_overall[1]),
                    text=tooltip_trend, 
                    hoverinfo="text")
    change<-change %>% 
      layout(
        xaxis=list(
          tickmode='array',
          tickvals=seq(1:nrow(plot_data)),
          ticktext=c(paste(unique(plot_data$Date),'2020',sep=' '),'Jan 2021','Feb 2021','Mar 2021')
        ))
    change <- change %>% layout(
      title = (sprintf("Percentage change in supply of take home naloxone kits in %s compared with the corresponding time in 2018-2019 (%s)",location,input$types)),
      yaxis = list(title = "% Change"
                   ),
      xaxis = list(title = "Date")
    )
    change <- change %>%  config(
      displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                            'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                            'hoverClosestCartesian', 'zoom2d', 'pan2d'))
      })
      
      plotlyOutput('change_plot')
    }
    
    else if(length(which(is.na(plot_data$Change)))!=0){
      
      output$data_message<-renderText('Percent change plot not shown due to NA values being produced by comparison with 0 values in 2018/2019 average')
      textOutput('data_message')
    }
  }
  
})

###############################################.
## Commentary ----
###############################################.
output$drug_commentary <- renderUI({
  tagList(
    bsButton("jump_to_drugs",label = "Go to data"),#this button can only be used once
    h4(strong("Drug treatment referrals")),
    p(strong("Information on the number of referrals to specialist drug and 
             alcohol treatment services was included for the first time on 01 September 
             2021")),
    tags$ul(
      tags$li(""),
      tags$li(""),
      tags$li(""),
      tags$li(""),
      tags$li("")),
    p('For further information, contact phs.drugsteam@phs.scot'),
    
    h4(strong('Take home naloxone kits')),
    tags$ul(
      tags$li(""),
      tags$li(""),
      tags$li(""),
      tags$li(""),
      tags$li("")),
    
    p(strong('Community')),
    tags$ul(
      tags$li(""),
      tags$li("")),
    
    p(strong('Pharmacy')),
    tags$ul(
      tags$li("")),
    
    p(strong('Prisons')),
    tags$ul(
      tags$li(""),
      tags$li("")),
    p('For further information, contact phs.drugsteam@phs.scot')
  )
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
  
  



