#Server side for drugs tab
observeEvent(input$btn_drugs_modal, 
             if(input$drug_subcategories == 'Drug and alcohol treatment referrals'){
             showModal(modalDialog(
               title = "What is the data source?",
              p('This section of the PHS Covid-19 wider impacts dashboard provides weekly information on the number of 
                referrals to specialist alcohol and drug treatment services in Scotland.'),
              p('These data have been extracted from the Drug and Alcohol Treatment Waiting Times (DATWT) database and the new Drug and Alcohol Information System (DAISy) (both Public Health Scotland) . DAISy is a national system that collects drug and alcohol waiting times and treatment information. 
                This replaced the DATWT database and the Scottish Drug Misuse Database (SDMD) systems.'),
              p('Data from the start of 2020 to the latest available week are shown alongside historical activity data (average from 2018 and 2019) for comparison purposes. Data are available for Scotland and at 
                NHS Board and Alcohol and Drug Partnership levels and are also broken down by client type (Drugs, Alcohol and All). '),
              p('DAISy introduced an additional \'co-dependency\' client type (where the referral relates to treatment for both alcohol and drug use), but this has only been recorded in all NHS Boards since April 2021 so is not available as a separate breakdown here. To ensure completeness, the \'Drugs\' category includes all referrals relating to drugs and co-dependency and the \'Alcohol\' category includes all referrals relating to alcohol and co-dependency. From 1 December 2020 onwards, 
                the sum of referrals in the \'Alcohol\' and \'Drugs\' categories will be higher than the \'All\' data category, due to the inclusion of \'co-dependency\' in both.'),
              p('May 2021 figures may currently be underreported due to late data submissions for the most recent weeks (a dashed line is used to indicate the lower degree of certainty in the figures for this period).'),
              p('For further information, contact',
                 tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.'),
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
             else if(input$drug_subcategories == 'Take home naloxone kits'){
              showModal(modalDialog(
                 title = "What is the data source?",
              p('Accidental overdose is a common cause of death among users of opioids such as heroin and morphine. 
                  Naloxone is a drug which reverses the effects of a potentially fatal overdose with these drugs. 
                  Administration of naloxone provides time for emergency services to arrive and for further treatment to be given. Following suitable training, \'take home\' naloxone kits (hereafter referred to as \'THN\') 
                are issued to people at risk of opioid overdose, their friends and family and service workers in order to help prevent overdose deaths. '),
              p('Information on THN kits supplied by community outlets, dispensed by community pharmacies and supplied by prisons on release from custody is shown. 
                For THN supplied by prisons, NHS Board relates to the location of the prison. Monthly data from the start of 2020 up to March 2021 are shown, 
                alongside historical activity data (average from 2018 and 2019) for comparison purposes. Data are available for Scotland and at NHS Board level.'),
              p('Data on THN supplies from community services and the Scottish Prison Service are based on data submitted to PHS from the National Naloxone Database. 
                Data on naloxone dispensed by community pharmacies is extracted from the Prescribing Information System, held by PHS. '),
              p('The data relate to numbers of THN kits distributed rather than individuals. Data can include supplies of multiple kits, repeat supplies and those issued to service workers and family/friends of persons at risk. The supply of THN was expanded to non-drug treatment services (such as homelessness services and mental health services) 
                at the end of April 2020 to ensure continued supply during the COVID-19 pandemic. Data on supply by these services is included in \'Community\' figures.'),
              p('The distribution of THN supplied by each source is shown in this dashboard (chart titled \'Percentage of take home naloxone kits provided by each source\'). This includes data supplied by the Scottish Ambulance Service (SAS), whose figures are not included in the other charts as data are not available nationally for the pre-COVID period. 
                SAS undertook a THN supply pilot between February 2020 and June 2020 and have subsequently rolled out this work on a nationwide basis. '),
              p('For further information, contact',
                tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.'),
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
    trend<-plot_ly(data = complete_data, x = ~Date,y = ~ `2020 & 2021`,name='2020 & 2021',type='scatter', mode='lines', line=list(color=pal_overall[1]))
    trend<-trend %>% add_trace(data=incomplete_data, x=~Date,y=~`2020 & 2021`,type='scatter',name='20/21 (Incomplete)',mode='lines',line=list(color=pal_overall[1],dash='dash'),showlegend=FALSE)
    trend<-trend %>% add_trace(data=plot_data,x=~Date,y = ~ `Average 2018 & 2019`,name='Average \n2018-2019',type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'))
    trend <- trend %>% layout(
      title = (sprintf("Number of %s treatment referrals in 2020 and 2021 \n compared with 2018-19 average (%s)",tolower(input$types),location)),
      yaxis = list(title = "Number of referrals"),
      hovermode= 'x unified',
      hoverlabel=list(namelength=-1)
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
    trend<-trend %>% add_trace(y = ~ `Average 2018 & 2019`,name='Average \n2018-2019',type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'))
    trend<-trend %>% layout(
          xaxis=list(
          tickmode='array',
          tickvals=seq(1:nrow(plot_data)),
          ticktext=c(paste(unique(plot_data$Date),'2020',sep=' '),'Jan 2021','Feb 2021','Mar 2021')
          
        ),
        hovermode= 'x unified',
        hoverlabel=list(namelength=-1)
        )
    trend <- trend %>% layout(
      title = (sprintf("Number of take home naloxone supplied in 2020 and 2021 \n compared with 2018-19 average (%s,%s)",location,input$types)),
    
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
      prop<-plot_ly(data = plot_data, x =seq(1:15),y = plot_data$`2020 & 2021`[which(plot_data$Type=='Community')],type='bar',name='Community',hovertemplate = paste0(plot_data$`Proportion 20/21`[which(plot_data$Type=='Community')],' %'),marker = list(color = pal_drug[1]))
      prop<-prop %>% add_trace(y = plot_data$`2020 & 2021`[which(plot_data$Type=='Prescribing')], name = 'Prescribing',hovertemplate = paste0(plot_data$`Proportion 20/21`[which(plot_data$Type=='Prescribing')],' %'),marker = list(color = pal_drug[2]))
      prop<-prop %>% add_trace(y = plot_data$`2020 & 2021`[which(plot_data$Type=='Prison')], name = 'Prison',hovertemplate = paste0(plot_data$`Proportion 20/21`[which(plot_data$Type=='Prison')],' %'),marker = list(color = pal_drug[3]))
      prop<-prop %>% add_trace(y = plot_data$`2020 & 2021`[which(plot_data$Type=='Ambulance')], name = 'SAS',hovertemplate = paste0(plot_data$`Proportion 20/21`[which(plot_data$Type=='Ambulance')],' %'),marker = list(color = pal_drug[4]))
      prop<-prop %>% add_trace(y = (plot_data$`2020 & 2021`[which(plot_data$Type=='Ambulance')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Prison')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Prescribing')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Community')]),
                                       name = 'Total',type='scatter',mode='markers',colors='black',showlegend=F,
                               hovertemplate = paste0((plot_data$`2020 & 2021`[which(plot_data$Type=='Ambulance')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Prison')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Prescribing')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Community')]),''),
                               marker = list(size = 4,
                                             color = 'rgba(0, 0, 0,0)')) 
      prop <- prop %>% layout(
        #separators = '.,', 
        barmode = 'stack',
            xaxis=list(
            tickmode='array',
            tickvals=seq(1:nrow(plot_data)),
            ticktext=c(paste(unique(plot_data$Date),' 2020'),'Jan 2021','Feb 2021','Mar 2021')
          ))
      prop <- prop %>% layout(
        title = (sprintf("Percentage of take home naloxone provided by source of supply in 2020 and 2021 (%s)",location)),
        xaxis=list(title='Date'),
        yaxis = list(title = "Number of THN kits"),
                    # hoverformat=',.0%'),
        hovermode= 'x unified'
      )
      prop <- prop %>%  config(
        displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                              'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                              'hoverClosestCartesian', 'zoom2d', 'pan2d'))
    })
    
    plotlyOutput('prop_plot',width='100%')
    
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
    
    plot_data<-subset(DTR_July_update,(Board==location) & Type==input$types  & Date<'2021-06-28')
    
    if(length(which(is.na(plot_data$Change)))==0){
      
      output$change_plot<-renderPlotly({
    
    tooltip_trend<-c(paste0(
                          "Date: ", plot_data$Date,
                          "<br>", "Change from 2018-2019 average: ",ifelse(plot_data$Change >= 0, "+", ""), plot_data$Change, "%"))
    change<-plot_ly(data = plot_data, x = ~Date, y = ~Change,
                    type='scatter', 
                    mode='lines',
                    line=list(color=pal_overall[1]),
                    text=tooltip_trend, 
                    hoverinfo="text")
    
    change <- change %>% layout(
      title = (sprintf("Percentage difference in the number of %s treatment referrals in 2020 and 2021 \n compared with 2018-2019 average (%s)",tolower(input$types),location)),
      yaxis = list(title = "% Change"),
      hovertemplate = "%{Date}: <br> Change from 2018-2019 average: %{Change}"
    )
    change <- change %>%  config(
      displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                            'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                          'hoverClosestCartesian', 'zoom2d', 'pan2d'))
      })  
      plotlyOutput('change_plot',width='90%')
    }
    else if(length(which(is.na(plot_data$Change)))!=0){
      
      output$data_message<-renderText('Percent difference plot not shown due to \'not applicable\' values being produced by comparison with 0 values in 2018/2019 average.')
      textOutput('data_message')
    }
  }
  
  else if(input$drug_subcategories=='Take home naloxone kits'){
    
    plot_data<-subset(THN_by_HB,(Board==location) & (Type==input$types))
    if(length(which(is.na(plot_data$Change)))==0){
    
      output$change_plot<-renderPlotly({
      
    tooltip_trend<-c(paste0(
      "Month: ", c(paste(unique(plot_data$Date),'2020',sep=' '),'Jan 2021','Feb 2021','Mar 2021'),
      "<br>", "Change from 2018 & 2019 average: ",ifelse(plot_data$Change >= 0, "+", ""), plot_data$Change, "%"))
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
      title = (sprintf("Percentage difference in supply of take home naloxone in 2020 and 2021 \n compared with 2018-2019 average (%s,%s)",location,input$types)),
      yaxis = list(title = "% Change"
                   ),
      xaxis = list(title = "Date")
    )
    change <- change %>%  config(
      displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = list('select2d', 'lasso2d', 
                                                                            'autoScale2d',   'toggleSpikelines',  'hoverCompareCartesian',  
                                                                            'hoverClosestCartesian', 'zoom2d', 'pan2d'))
      })
      
      plotlyOutput('change_plot',width='92%')
    }
    
    else if(length(which(is.na(plot_data$Change)))!=0){
      
      output$data_message<-renderText('Percent difference plot not shown due to \'not applicable\' values being produced by comparison with 0 values in 2018/2019 average.')
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
    h4(strong("Drug and alcohol treatment referrals")),
    p(strong("Information on the number of referrals to specialist drug and 
             alcohol treatment services was included for the first time on 01 September 
             2021")),
    p(strong(
      'These data on numbers of referrals to specialist drug and alcohol treatment services during the pandemic can be interpreted as a measure of demand for support with substance use issues and/or the capacity of services to process referrals for treatment. 
     Although these data are sourced from the systems that monitor waiting times for drug and alcohol treatment waiting times, they do not indicate the percentage of waits for specialist treatment where the target was met, nor whether individuals were provided with support that met their needs. 
       Information on performance against Scotland’s Drug and Alcohol Treatment Waiting Time target can be found at',
       tags$a(href="https://publichealthscotland.scot/publications/national-drug-and-alcohol-treatment-waiting-times/national-drug-and-alcohol-treatment-waiting-times-1-january-to-31-march-2021/", 
              "https://publichealthscotland.scot/publications/national-drug-and-alcohol-treatment-waiting-times/national-drug-and-alcohol-treatment-waiting-times-1-january-to-31-march-2021/",  target="_blank"), '.'
    )),
    tags$ul(
      tags$li("The numbers of specialist drug and alcohol treatment referrals in January and February 2020 were broadly comparable to the 2018 and 2019 average for the corresponding weeks. Subsequently, 
              a 63% decrease in referrals was observed from week beginning 9 March 2020 (1,156 referrals) to week beginning 23 March 2020 (424 referrals). "),
      tags$li("Since the UK lockdown was implemented on 23 March 2020, drug and alcohol treatment referral numbers have been consistently lower than in the comparable period in 2018 and 2019. From April 2020, a gradual increase has been observed, rising from 387 in the week beginning 6 April 2020 to 1,060 in the week beginning 24 August. This figure remained approximately stable until December, when the annual seasonal decrease in treatment 
              referrals in late November and December 2020 was broadly comparable with decreases observed in previous years. "),
      tags$li("From January 2021 to May 2021, referral numbers remained stable and at a similar level seen in the latter half of 2020 (generally around 20% lower than the 2018 and 2019 average for corresponding weeks)."),
      tags$li("A similar pattern was seen for both drug and alcohol referrals over the 18-month time period, although alcohol treatment referrals dropped to a greater extent following the UK lockdown (at their lowest, alcohol referrals were 74% below the 2018 and 2019 average for the week beginning 6 April, compared with 58% below observed in drug referrals). However, both referral types increased to around 20% below the 2018 and 2019 average by 22 June. "),
      tags$li("The trends described were broadly observed across all NHS Boards and Alcohol and Drug Partnerships.")),

    h4(strong('Take home naloxone kits')),
    tags$ul(
      tags$li("Overall THN supply in Scotland showed peaks between March and May 2020 and around December 2020 and January 2021. Most NHS Boards showed either one or both of these increases."),
      tags$li("Supply by community outlets was the most common source of THN, although this varied between NHS Boards and source distribution has varied over the time period presented here. For example, NHS Dumfries & Galloway showed a large increase in community pharmacy supply between March and May 2020 (25% of all supply in March, 57% in April, and 35% in May, compared with around 6% or lower throughout the rest of time period from January 2020 to March 2021). ")),

    p(strong('Community')),
    tags$ul(
      tags$li("The trend in community outlet supplies per month shows an exceptionally large number of THN kits supplied in April and May 2020 during the initial response to the COVID-19 pandemic (from 961 in March to 2,546 in April and 1,675 in May). Large-scale distributions by NHS Fife and NHS Ayrshire and Arran accounted for the notable increases in supplies observed in April and May 2020 respectively. "),
      tags$li("Aside from these peaks in April and May 2020, weekly supply numbers in 2020 were broadly the same as the combined 2018 and 2019 average. The two minor exceptions to this were smaller peaks in September and December 2020. Although the December 2020 figure was lower than that for of the 2018 & 2019 average, the regular peak observed at that time of year increase suggests a seasonal increase in THN supply.")),
    
    p(strong('Pharmacy')),
    tags$ul(
      tags$li("The number of THN kits dispensed by pharmacies on the basis of a community prescription was consistently higher from March 2020 to March 2021 than the average for 2018 & 2019. In particular, two large peaks in supply were observed in April 2020 (from 193 kits in March 2020 to 1,393 in April) and December 2020 (from 241 in October to 1,418 in December).")),
    
    p(strong('Prisons')),
    tags$ul(
      tags$li("The number of THN supplies issued by prisons per month was consistently higher for the period February 2020 to March 2021 than the corresponding 2018 & 2019 averages. The exception to this was November 2020 when supplies were 21% lower than the 2018 & 2019 average for the same month. "),
      tags$li("The large peak in May 2020 may partially have been a result of the Scottish Prison Service’s ",
              tags$a(href="https://www.sps.gov.uk/Corporate/Information/covid19/covid-19-information-hub.aspx", 
                     "COVID Early Release scheme ",  target="_blank"),
             "in which the Coronavirus (Scotland) Act 2020 provided new powers for the early release of a specific class of prisoners held in Scottish prisons. Early release was deemed necessary in order to provide the Scottish Prison Service with additional operational capacity including allowing for a greater use of single cell occupancy, keeping prison staff and the people in their care safe. It is understood that that scheme is no longer operational, so subsequent increases may reflect other factors.")),
    p('For further information, contact ',
      tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.')
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
  
  



