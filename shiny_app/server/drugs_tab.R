# Wider impacts dashboard - Substance use tab
# Server code

# TODO:
# Too many functions/objects have very generic names
# Needs more commenting

###############################################.
## Modals ----
###############################################.
# Modal for data source and definitions
observeEvent(input$`drugs-source-modal`, 
             if(input$`drugs-measure` == 'Drug and alcohol treatment referrals'){
             showModal(modalDialog(
               title = "What is the data source?",
              p('This section of the PHS Covid-19 wider impacts dashboard provides the weekly number of referrals to specialist alcohol and 
                drug treatment services in Scotland delivering tier 3 and 4 interventions (community-based specialised drug assessment and
                co-ordinated care-planned treatment, and residential specialised drug treatment). These data exclude drug and alcohol treatment
                services in prisons.'),
              p('These data have been extracted from the Drug and Alcohol Treatment Waiting Times (DATWT) database and the new',
                tags$a(href="https://www.isdscotland.org/health-topics/drugs-and-alcohol-misuse/drug-alcohol-information-system/", 
                       "Drug and Alcohol Information System ",  target="_blank"), 
                '(DAISy) (both Public Health Scotland). DAISy is a national system that collects drug and alcohol waiting times
                and treatment information. This replaced the DATWT database and the Scottish Drug Misuse Database (SDMD) systems.
                From 1 December 2020 NHS Ayrshire & Arran, NHS Dumfries & Galloway, NHS Grampian and NHS Western Isles began recording
                waiting times information on DAISy. The remaining NHS Boards transferred to DAISy in April 2021.'),
              p('Drug and Alcohol referral data from the start of 2020 to March 2022 are shown alongside historical activity data
                (average from 2018 and 2019) for comparison purposes. Data are available for Scotland and at NHS Board and Alcohol and Drug Partnership
                levels and are also broken down by client type (Drugs, Alcohol, Co-dependency and All).'),
              p('Direct comparisons between numbers of referrals recorded in DATWT and DAISy should be interpreted carefully,
                with consideration of the following changes in the recording process:'),
              tags$ul(
                tags$li('DAISy introduced an additional ‘co-dependency’ client type (where the referral relates to treatment for both
                        alcohol and drug use), but this has only been recorded in all NHS Boards since April 2021 so is not available 
                        as a separate breakdown in the earlier years.'),
                tags$li('DAISy also introduced a new continuation of care process which affects how referrals are recorded when people 
                        move between services after starting treatment. Whereas in the previous data system a move between services would
                        have been recorded as two separate referrals (a referral for each service), in DAISy, the referral is only entered 
                        once for the initial service and then a move between services is recorded as a continuation of care transfer to the
                        second service, rather than a referral. This change is expected to result in a decrease in the number of new
                        referrals being recorded in DAISy compared to the previous data system. ')),
              p('Services are required to submit accurate and up-to-date waiting times information to PHS. These referrals data is 
                management information and includes all services entering data on DAISy and its predecessor, the DATWT database.'),
              p('For further information, contact',
                 tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.'),
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
             else if(input$`drugs-measure` == 'Take home naloxone kits'){
              showModal(modalDialog(
                 title = "What is the data source?",
              p('Accidental overdose is a common cause of death among users of opioids such as heroin and morphine. 
                  Naloxone is a drug which reverses the effects of a potentially fatal overdose with these drugs. 
                  Administration of naloxone provides time for emergency services to arrive and for further treatment to be given. Following suitable training, \'take home\' naloxone kits (hereafter referred to as \'THN\') 
                are issued to people at risk of opioid overdose, their friends and family and service workers in order to help prevent overdose deaths. '),
              p('Information on THN kits supplied by community outlets, dispensed by community pharmacies and supplied by prisons on release from custody is shown. 
                For THN supplied by prisons, NHS Board relates to the location of the prison. Monthly data from the start of 2020 up to December 2021 are shown, 
                alongside historical activity data (average from 2018 and 2019) for comparison purposes. Data are available for Scotland and at NHS Board level.'),
              p('Data on THN supplies from community services and the Scottish Prison Service are based on data submitted to PHS from the National Naloxone Database. 
                Data on naloxone dispensed by community pharmacies is extracted from the Prescribing Information System, held by PHS. '),
              p('The data relate to numbers of THN kits distributed rather than individuals. Data can include supplies of multiple kits, repeat supplies and those issued to service workers and family/friends of persons at risk. The supply of THN was expanded to non-drug treatment services (such as homelessness services and mental health services) 
                at the end of April 2020 to ensure continued supply during the COVID-19 pandemic. Data on supply by these services is included in \'Community\' figures.'),
              p('The distribution of THN supplied by each source is shown in this dashboard (chart titled \'Percentage of take home naloxone kits provided by each source\'). This includes data supplied by the Scottish Ambulance Service (SAS), whose figures are not included in the other charts as data are not available nationally for the pre-COVID period. 
                SAS undertook a THN supply pilot between February 2020 and June 2020 and have subsequently rolled out this work on a nationwide basis.'),
              p('For further information, contact',
                tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.'),
              easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
             else if(input$`drugs-measure` == 'SAS naloxone administration'){
               showModal(modalDialog(
                 title = "What is the data source?",
                 p('Scottish Ambulance Service (SAS) data on weekly numbers of incidents at which naloxone was administered to a patient have been shared with Public Health Scotland to facilitate the monitoring of drug-related harms and assist in preserving life and informing harm prevention activity. '),
                 p('These data provide an indication of numbers of suspected opioid overdoses attended by ambulance clinicians before and during the pandemic. This allows trends over time to be monitored and acts as a basis for investigating changes that may be associated with the pandemic.  The figures shown relate to the 3-week central moving average of naloxone incidents per week recorded by SAS in the period from beginning of 2018 to April 2022.'),
                 p('Naloxone is a medication which is used to prevent fatal opioid overdose. SAS clinicians have been administering naloxone directly to patients experiencing symptoms of an opioid overdose since around 1998. Scotland’s National Naloxone Programme, supplying take-home naloxone kits directly to people at risk of opioid overdose, has been operational since April 2011. In 2020, SAS commenced a pilot study to supply take-home naloxone to those experiencing a non-fatal opioid overdose or present at the scene of an opioid overdose that they attended.'),
                 p('SAS data on numbers of naloxone incidents are collated from data entered by ambulance clinicians recording medications administered to patients via an electronic tablet in the vehicle. Data recording is typically completed within 30 minutes of the end of an incident. There have been no changes in the guidance given to SAS clinicians regarding the administration of naloxone nor in the recording mechanisms or processes over the time series shown in the analysis.'),
                 p('In spite of this high degree of consistency, the full reasons for naloxone administration are not known. A small percentage of these administrations will have been due to circumstances other than an illicit opioid overdose (for example, some may relate to prescribed opioid overdoses or to adverse reactions associated with medications administered in the course of emergency treatment). Also, in a small number of cases, naloxone may be administered to someone who is unconscious for unconfirmed reasons, which may be confirmed at a later point not to have been an opioid overdose. '),
                 p('Prior to agreeing on this specific indicator, a number of other potential overdose measures were discussed with SAS colleagues. In spite of the availability of a specific ‘overdose’ diagnosis group in SAS data, it was felt that a naloxone administration indicator offered the clearest focus on opioid overdoses, which are most commonly associated with drug-related deaths. '),
                 p('These data relate to the number of incidents at which naloxone was administered. While these data count multiple overdose patients at the same incident separately, multiple naloxone administrations to specific individuals at the same incident are not counted as separate incidents. Under some circumstances, naloxone administration will not successfully reverse an opioid overdose (for example, if administered too late) and these statistics should not be interpreted as equating to numbers of lives saved. Data on interventions in relation to opioid overdose are complex and multi-faceted. '),
                 p('For further information, contact',
                   tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.'),
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
             else if(input$`drugs-measure` == 'OST prescribing'){
               showModal(modalDialog(
                 title = "What is the data source?",
                 p('Information on the characteristics of community prescribing for Opioid Substitution Therapy (OST) drugs (methadone and buprenorphine) in Scotland is obtained from the Prescribing Information System. '),
                 p('These data include all OST prescriptions written by clinicians (doctors, nurses, and pharmacists) in specialist drug treatment clinics or in GP Practices and dispensed in the community.  These data do not include prescriptions dispensed in hospitals or prisons.'),
                 p('The methadone statistics relate to prescriptions for methadone 1mg/1ml oral solution. The buprenorphine figures relate to prescriptions for 2mg, 8mg and 16mg buprenorphine or buprenorphine & naloxone tablets. Prescribing for long-acting buprenorphine injectables (Buvidal®) are not included in these statistics.  '),
                 p(strong('Terminology:')),
                 tags$ul(
                   tags$li("Item: An item is an individual product written on a prescription, e.g. methadone 1mg/ml oral solution. "),
                   tags$li("Quantity: The total quantity of the item requested on the prescription, e.g. 500ml."),
                   tags$li("Quantity per item:  The quantity prescribed per item on a prescription. It is calculated as quantity/number of items.")),
                 p('Other areas of the Covid Wider Impacts Dashboard i.e. Cardiovascular and Mental Health have presented prescribing information using data from e- messages (generated through GP practices) which provide more real-time data. As a significant amount of OST prescribing is undertaken through non-GP clinic settings, paid data is the most complete and robust and used in this analysis.'),
                 p('Paid data is presented by month and refers to prescriptions that have been submitted and processed for payment. Since all dispensers must be reimbursed for the drugs they dispense, this data is regarded as complete. '),
                 p('For further information, contact',
                   tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.'),
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
             else if(input$`drugs-measure` == 'A&E attendances for drug overdose/intoxication'){
               showModal(modalDialog(
                 title = "What is the data source?",
                 p('A weekly breakdown of the number of drug-related attendances at Emergency Departments in Scotland is obtained from Public Health Scotland’s Accident & Emergency Datamart'),
                 p(''),
                 p('Important note: It is not possible to accurately report total attendances for specific conditions using the national A&E dataset, due to the quality of the data available.  
                   Diagnosis/reason for attendance can be recorded in a variety of ways, including in free text fields - and not all NHS Boards submit this information.  
                   The numbers presented in these dashboards therefore give only a high level indication of differences over time and by age and sex, and should be interpreted with caution. PHS are planning work to improve consistency.'),
                 p(''),
                 p('Due to these differences, it is not possible to identify drug involvement or overdose as a presenting condition using only a specific variable or diagnosis code. Attendances for drug intoxications or overdoses are identified using a combination of exact matching of relevant ICD codes and searching of free-text fields.'),
                 p(''),
                 p('Due to small and fluctuating numbers, attendances are presented here as a 3-week rolling average. Numbers by sex are displayed where sex was recorded on the attendance record'),
                 p('Additionally, where the average number of weekly attendences across the 2020 to 2022 time frame is <10, the data at NHS Board level is not displayed.'),
                 p(strong('Terminology:')),
                 p("Drug Intoxication/Overdose: An attendance for a drug intoxication or overdose, either alone, or combined with alcohol intoxication"),
                 p('For further information, contact',
                   tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.'),
                 easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
             }
               )

###############################################.
## Reactive controls ----
###############################################.

output$area_drugs_select<-renderUI({
  
  if(input$`drugs-measure` == 'Drug and alcohol treatment referrals'){
    selectizeInput("area_drugs_select", "Step 2 - Select the area of interest",
                   choices = c('Scotland','NHS Board','Alcohol and Drug Partnership'), selected = "Scotland")
  }

  
  else if (input$`drugs-measure` == 'Take home naloxone kits'||input$`drugs-measure`=='SAS naloxone administration'||input$`drugs-measure` == 'OST prescribing'||input$`drugs-measure` == 'A&E attendances for drug overdose/intoxication'){
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
  if(input$`drugs-measure`=='Drug and alcohol treatment referrals'){
    column(8,
           radioButtons("types", label="Step 3 - Select type of referral",
                        choices = c('All','Drug','Alcohol', 'Co-dependency'),selected = 'All'))
  }
  else if(input$`drugs-measure`=='Take home naloxone kits'){
    column(8,
           radioButtons("types", label="Step 3 - Select source of supply",
                        choices = c('All','Community','Dispensed by community pharmacies'='Prescribing','Prison'),selected = 'All'))
  }
  else if (input$`drugs-measure`=='OST prescribing'){
    column(8, 
           radioButtons('types',label='Step 3 - Select type of treatment',
                        choices=c('Methadone','Buprenorphine'),selected='Methadone'))
  }
  # else if(input$`drugs-measure`=='A&E attendances for drug overdose/intoxication'){
  #   column(8,
  #          radioButtons("types", label="Step 3 - Select type of attendance",
  #                       choices = c('Drug Overdoses','Alcohol Overdoses', 'Drug and Alcohol Overdoses'),selected = 'Drug and Alcohol Overdoses'))
  # }
})
 
###############################################.
## Reactive datasets ----
###############################################.


location<-reactive({
  if (input$area_drugs_select=='Scotland'){
    location<-'Scotland'
  }
  else if (input$area_drugs_select=='Alcohol and Drug Partnership'||input$area_drugs_select=='NHS Board'){
    location<-input$geoname_drugs
  }
}) 


drugs_plot_data<-reactive({ 
  
  if(input$`drugs-measure`=='Drug and alcohol treatment referrals'){
    plot_data<-subset(DTR_data,(Board==location()) & Type==input$types)
  }
  else if(input$`drugs-measure`=='Take home naloxone kits'){
    plot_data<-subset(THN_by_HB,(Board==location()) )
  }
  else if(input$`drugs-measure`=='OST prescribing'){
    plot_data<-subset(OST_paid,(Board==location()) & (Type==input$types))
  }
  else if(input$`drugs-measure`=='SAS naloxone administration'){
    plot_data<-subset(SASdata,(Board==location()))
  }
  else if(input$`drugs-measure`=='A&E attendances for drug overdose/intoxication'){
    plot_data<-subset(Drug_AE_attendances,(Board==location()))
  }
  plot_data
})


###############################################.
## Charts ----
###############################################.

output$drugs_2yr_comp<-renderUI({
  
  ####DTR section####
  plot_data<-drugs_plot_data()
  
  
  if(input$`drugs-measure`=='Drug and alcohol treatment referrals'){
    
    output$trend<-renderPlotly({
      
      lab.text<-c(paste0("Date: ", format(plot_data$Date, format = "%b %d, %Y"),
                         "<br>", 'Number of referrals: ', plot_data$`2020, 2021 & 2022`,
                         "<br>", "Historic average: ", plot_data$`Average 2018 & 2019`))
      
      trend<-plot_ly(data = plot_data, x = ~Date,y = ~ `2020, 2021 & 2022`,name='2020,2021 & 2022',
                     type='scatter', mode='lines', line=list(color=pal_overall[1]),
                     text=lab.text,hoverinfo='text') %>% 
        add_trace(x=~Date,y = ~ `Average 2018 & 2019`,name='Average \n2018-2019',type='scatter',
                  mode='lines', line=list(color=pal_overall[2],dash='dot'),
                  text=lab.text,hoverinfo='text') %>%
        layout(
          title = (ifelse(test = input$types == 'Co-dependency',
                          yes = 'Number of co-dependency treatment referrals in 2020, 2021, and 2022.',
                          no = sprintf("Number of %s treatment referrals in 2020, 2021, and 2022 \n compared with 2018-19 average (%s)",
                                       tolower(input$types),location()))),
          yaxis = list(title = "Number of referrals",
                       rangemode='tozero',
                       fixedrange=TRUE),
          xaxis=list(
            fixedrange=TRUE,
            angle=90)) %>% 
        add_vline(x = '2020-03-23', text = "1st lockdown", margin=list(t=80)) %>%  
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    })
    withSpinner(plotlyOutput('trend',width='100%'))
  }
 
  #### Naloxone Section ####
  
  else if(input$`drugs-measure`=='Take home naloxone kits'){
    
    output$trend<-renderPlotly({
    plot_data<-subset(drugs_plot_data(),(Type==input$types))
    
    lab_text<-c(paste0("Month: ", unique(plot_data$Date),
                       "<br>", 'Number of THN: ', plot_data$`2020 & 2021`,
                       "<br>", "Historic average: ", plot_data$`Average 2018 & 2019`))
    
    trend<-plot_ly(data = plot_data, x =seq(1:nrow(plot_data))) %>% 
      add_trace(y = ~ `2020 & 2021`,name='2020 & 2021',type='scatter', mode='lines', 
                line=list(color=pal_overall[1]),text=lab_text,hoverinfo='text') %>% 
      add_trace(y = ~ `Average 2018 & 2019`,name='Average \n2018-2019',type='scatter', 
                mode='lines', line=list(color=pal_overall[2],dash='dot'),
                text=lab_text,hoverinfo='text') %>% 
      layout(
      xaxis=list(
        tickmode='array',
        tickvals=seq(1,nrow(plot_data),2),
        ticktext=as.character(unique(plot_data$Date))[c( TRUE , rep(FALSE, 1)) ],
        fixedrange=TRUE,
        title='Date'),
      title = (sprintf("Number of take home naloxone supplied in 2020 and 2021 \n compared with 2018-19 average (%s,%s)",location(),input$types)),
      yaxis = list(title = "Number of THN kits",
                   rangemode='tozero',
                   fixedrange=TRUE)) %>% 
      add_vline(x = "3.77", text = "1st lockdown", margin=list(t=80)) %>%  
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)})
    
    withSpinner(plotlyOutput('trend',width='100%'))
  }
  
  #### OST prescribing section ####
  
  else if(input$`drugs-measure`=='OST prescribing'){
    output$trend<-renderPlotly({
    
      
      plot_item<-subset(drugs_plot_data(),(Measurement=='Items'))
      plot_qpi<-subset(drugs_plot_data(),(Measurement=='Quantity per item'))
      lab_text<-c(paste0("Month: ", plot_item$Date,
                         "<br>", 'Number of items: ', plot_item$`2020, 2021 & 2022`,
                         "<br>", "Historic average: ", plot_item$`Average 2018 & 2019`))
      lab_text1<-c(paste0("Month: ", plot_qpi$Date,
                          "<br>", 'Quantity per item (mg): ', plot_qpi$`2020, 2021 & 2022`,
                          "<br>", "Historic average: ", plot_qpi$`Average 2018 & 2019`))
      trend<-plot_ly(data = plot_item,x=seq(1:nrow(plot_item)))
      trend<-trend %>% add_trace(y = ~ `2020, 2021 & 2022`,name='Items: 2020, 2021 & 2022',type='scatter', mode='lines', line=list(color=pal_overall[1],width=3),text=lab_text,hoverinfo='text')
      trend<-trend %>% add_trace(y = ~ `Average 2018 & 2019`,name='Items: Average \n2018-2019',type='scatter', mode='lines', line=list(color=pal_overall[1],dash='dot'),text=lab_text,hoverinfo='text')
      trend<-trend %>% add_trace(data=plot_qpi, x=seq(1:nrow(plot_qpi)),y = ~ `2020, 2021 & 2022`,name='QPI: 2020, 2021 & 2022',type='scatter',yaxis='y2', mode='lines', line=list(color=pal_overall[2],width=3),text=lab_text1,hoverinfo='text')
      trend<-trend %>% add_trace(y = ~ `Average 2018 & 2019`,name='QPI: Average \n2018-2019',type='scatter', mode='lines',yaxis='y2', line=list(color=pal_overall[2],dash='dot'),text=lab_text1,hoverinfo='text')
      trend<-trend %>% layout(
        title=(sprintf("%s prescribing (number of items and quantity (mg) per item (QPI)) by month in 2020, 2021 and 2022 \n compared with 2018-19 average (%s)",input$types,location())),
        xaxis=list(
          title='Date',
          tickmode='array',
          tickvals=seq(1,nrow(plot_item),2),
          ticktext=as.character(plot_item$Date)[c( TRUE , rep(FALSE, 1)) ],
          fixedrange=TRUE
          
        ),
        yaxis=list(title='Number of items',
                   rangemode='tozero',
                   fixedrange=TRUE),
        yaxis2 = list(overlaying = "y", 
                      side = "right", 
                      title='Quantity per item (QPI) (mg)',
                      automargin=T,
                      rangemode='tozero'),
        legend = list(x=1.08,y=1),
        updatemenus = list(
          list(
            type = "buttons",
            direction = "right",
            xanchor = 'center',
            yanchor = "top",
            x = 0.6,
            y = 1.15,
            buttons = list(
              list(method = "restyle",
                   args = list('visible', c(TRUE, TRUE, TRUE,TRUE)),
                   label = "View all"),
              list(method = "restyle",
                   args = list('visible', c(TRUE,TRUE, FALSE, FALSE)),
                   label = "Items"),
              list(method = "restyle",
                   args = list('visible', c(FALSE, FALSE, TRUE,TRUE)),
                   label = "Quantity per item")
            )
          )
        )) %>% 
        add_vline(x = '3.77', text = "1st lockdown", margin=list(t=140)) %>%  
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
      
    })
    withSpinner(plotlyOutput('trend',width='100%'))
  }
  #### SAS Naloxone section ####
  else if(input$`drugs-measure`=='SAS naloxone administration'){
    if(location()=='NHS Shetland'||location()=='NHS Orkney'||location()=='NHS Western Isles'){
      output$data_message<-renderText('Data not shown due to small numbers. Data for the Island Boards is included in the Scotland total')
      textOutput('data_message')
    }
    else{
    output$trend<-renderPlotly({
    
    lab_text1<-c(paste0("Date: ", drugs_plot_data()$Date,
                        "<br>", 'No. of SAS naloxone incidents: ', drugs_plot_data()$`2020, 2021 & 2022`,
                        "<br>", "Historic average: ", drugs_plot_data()$`Average 2018 & 2019`))
    
    trend <- plot_ly(data = drugs_plot_data(),x=drugs_plot_data()$Date) %>% 
      add_trace(y = ~ `2020, 2021 & 2022`,name='2020, 2021 & 2022',type='scatter', mode='lines', 
                line=list(color=pal_overall[1]),text=lab_text1,hoverinfo='text') %>% 
      add_trace(y = ~ `Average 2018 & 2019`,name='Average \n2018-2019',
                type='scatter', mode='lines', line=list(color=pal_overall[2],dash='dot'),
                text=lab_text1,hoverinfo='text') %>% 
      layout(title = (sprintf("3-Week average of the number of SAS incidents where naloxone was administered in 2020, 2021 and 2022 \n compared with 2018-19 average (%s)",location())),
      xaxis=list(title='Date', fixedrange=TRUE ),
      yaxis=list(title='No. of SAS naloxone incidents',
                 rangemode='tozero',fixedrange=TRUE)) %>% 
      add_vline(x = '2020-03-23', text = "1st lockdown", margin = list(t=80)) %>% 
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    })
    withSpinner(plotlyOutput('trend',width='100%'))
    }}
  
  #### Drug related A&E attendances ####
  else if(input$`drugs-measure`=='A&E attendances for drug overdose/intoxication'){
    tagList(#A&E attendances
      p("Important note: It is not possible to accurately report total attendances for specific conditions using the national A&E dataset, due to the quality of the data available.  
        Diagnosis/reason for attendance can be recorded in a variety of ways, including in free text fields - and not all NHS Boards submit this information.  
        The numbers presented in these dashboards therefore give only a high level indication of differences over time and by age and sex, and should be interpreted with caution.  
        Breakdowns by SIMD are not felt to be reliable, as they could be heavily skewed by the demographic profile of the areas represented in the data available. PHS are planning work to improve consistency."))
    
    if(location() %in% c('NHS Shetland', 'NHS Orkney', 'NHS Western Isles', 'NHS Ayrshire & Arran', 'NHS Borders',
                         'NHS Highland', 'NHS Lanarkshire', 'NHS Grampian', 'NHS Tayside', 'NHS Forth Valley', 'NHS Dumfries & Galloway')) {
      output$data_message<-renderText('Data not shown due to small numbers. Data for this NHS Board is included in the Scotland total')
      textOutput('data_message')
    }
    else{
      output$trend <- renderPlotly({
        ## set out the plot data, based on what the user has selected
        plot_data <- subset(drugs_plot_data(), (Gender == "All"))
        
        lab_text<-c(paste0("Average of weeks beginning ",format(plot_data$Date-7, "%d %b %y"), ", ", format(plot_data$Date, "%d %b %y"), ", ", format(plot_data$Date+7, "%d %b %y"),
                            "<br>", 'Number of attendances: ', round(plot_data$`2020 & 2021`,1),
                            "<br>", "Historic average: ", round(plot_data$`Average 2018 & 2019`,1)))
        
        trend <- plot_ly(data = plot_data, 
                         x = plot_data$Date)
        
        trend <- trend %>% add_trace(y = plot_data$`2020 & 2021`,
                                     name = '2020, 2021 & 2022',
                                     type = 'scatter', 
                                     mode = 'lines', 
                                     line = list(color=pal_overall[1]),
                                     text = lab_text,
                                     hoverinfo = 'text')
        
        trend <- trend %>% add_trace(y = plot_data$`Average 2018 & 2019`,
                                     name = 'Average \n2018-2019',
                                     type = 'scatter', 
                                     mode = 'lines', 
                                     line = list(color=pal_overall[2],dash='dot'),
                                     text = lab_text,hoverinfo='text')
        
        trend <- trend %>% layout(xaxis = list(fixedrange=TRUE,
                                           title='Date'),
                                  title = (paste0("3-Week average of number of attendances for Drug Overdose/Intoxication Attendances \nat Emergency Departments in 2020 - 2022 compared with 2018-19 average (",location(), ")")),
                                  yaxis = list(title = "Number of attendances",
                                             rangemode='tozero',
                                             fixedrange=TRUE)) %>% 
          add_vline(x = '2020-03-23', text = "1st lockdown", margin = list(t=80)) %>% 
          config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
      })
      withSpinner(plotlyOutput('trend',width='100%'))
     }
  }
  
})

# Chart for proportions
output$drugs_prop_barplot<-renderUI({
  
  if(input$`drugs-measure`=='Take home naloxone kits'){
    
    output$prop_plot<-renderPlotly({
      
      plot_data<-drugs_plot_data()
      months<-length(unique(plot_data$Date)) #number of unique dates 
      prop <- plot_ly(data = plot_data, x =seq(1:months),
                      y = plot_data$`2020 & 2021`[which(plot_data$Type=='Community')],
                      type='bar',name='Community',
                      hovertemplate = paste0(plot_data$`Proportion 20/21`[which(plot_data$Type=='Community')],' %'),
                      marker = list(color = pal_drug[1])) %>% 
        add_trace(y = plot_data$`2020 & 2021`[which(plot_data$Type=='Prescribing')], 
                  name = 'Dispensed by \ncommunity pharmacies',
                  hovertemplate = paste0(plot_data$`Proportion 20/21`[which(plot_data$Type=='Prescribing')],' %'),
                  marker = list(color = pal_drug[2])) %>% 
        add_trace(y = plot_data$`2020 & 2021`[which(plot_data$Type=='Prison')], name = 'Prison',
                  hovertemplate = paste0(plot_data$`Proportion 20/21`[which(plot_data$Type=='Prison')],' %'),
                  marker = list(color = pal_drug[3])) %>% 
        add_trace(y = plot_data$`2020 & 2021`[which(plot_data$Type=='Ambulance')], name = 'SAS',
                  hovertemplate = paste0(plot_data$`Proportion 20/21`[which(plot_data$Type=='Ambulance')],' %')
                  ,marker = list(color = pal_drug[4])) %>% 
        add_trace(y = (plot_data$`2020 & 2021`[which(plot_data$Type=='Ambulance')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Prison')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Prescribing')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Community')]),
                  name = 'Total', type='scatter', mode='markers',
                  colors='black',showlegend=F,
                  hovertemplate = paste0((plot_data$`2020 & 2021`[which(plot_data$Type=='Ambulance')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Prison')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Prescribing')]+plot_data$`2020 & 2021`[which(plot_data$Type=='Community')]),''),
                  marker = list(size = 4,color = 'rgba(0, 0, 0,0)'))  %>% 
          layout(margin=list(t=80,b=100),  barmode = 'stack',
            xaxis= list( tickmode='array', tickvals=seq(1:months),
            ticktext=unique(plot_data$Date), title='Date', fixedrange=TRUE),
            title = (sprintf("Percentage of take home naloxone provided by source of supply in 2020 and 2021 (%s)",location())),
            yaxis = list(title = "Number of THN kits",fixedrange=TRUE),
            hovermode= 'x unified') %>%  
          config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    })
    
    withSpinner(plotlyOutput('prop_plot', width='100%'))
    
  }
  
})

output$drugs_cum_plot<-renderUI({
  
  if(input$`drugs-measure`=='Take home naloxone kits'){
    output$cum_plot<-renderPlotly({
    plot_data<-subset(drugs_plot_data(),(Type==input$types))
    plot_data1<-plot_data[1:12,]
    plot_21<-plot_data[13:nrow(plot_data),]
    lab_text<-function(x,y,z){
      c(paste0("Year: ", x,
               "<br>", 'Month: ', y,
               "<br>", "Cumulative number of THN kits: ", z))
    }
    trend<-plot_ly(data = plot_data1, x =seq(1:(nrow(plot_data1)+1)))
    trend<-trend %>% add_trace(y = c(0,cumsum(plot_data1$`Average 2018 & 2019`)),name='Average \n2018-2019',type='scatter', mode='lines',line=list(color='green',dash='dot'),text=lab_text('Average 2018-2019',c('',substr(plot_data1$Date,1,3)),c(0,cumsum(plot_data1$`Average 2018 & 2019`))),hoverinfo='text')
    trend<-trend %>% add_trace(y = c(0,cumsum(plot_data1$`2020 & 2021`)),name='2020',type='scatter', mode='lines', line=list(color='black'),text=lab_text('2020',c('',substr(plot_data1$Date,1,3)),c(0,cumsum(plot_data1$`2020 & 2021`))),hoverinfo='text')
    trend<-trend %>% add_trace(data=plot_21, x=seq(1:(nrow(plot_21)+1)),y=c(0,cumsum(plot_21$`2020 & 2021`)), name='2021', type='scatter',mode='lines',line=list(color='blue'),text=lab_text('2021',c('',substr(plot_21$Date,1,3)),c(0,cumsum(plot_21$`2020 & 2021`))),hoverinfo='text')
    trend<-trend %>% 
      layout(
        title=(sprintf("Cumulative number of take home naloxone supplied in 2020 and 2021 \n compared with 2018-19 average (%s, %s)",location(),input$types)),
        xaxis=list(
          tickvals=seq(1:(nrow(plot_data1)+1)),
          ticktext=c('',substr(plot_data1$Date,1,3)),
          title='Month',
          fixedrange=TRUE),
        yaxis = list(title = "Cumulative number of THN kits",
                     fixedrange=TRUE)) %>%
      add_vline(x = '4.77', text = "1st lockdown", margin = list(t=80, l = 2)) %>% 
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    
      })
    withSpinner(plotlyOutput('cum_plot'))
    }


 else if(input$`drugs-measure`=='SAS naloxone administration'){
   if(location()!='NHS Shetland'&&location()!='NHS Orkney'&&location()!='NHS Western Isles'){

   output$cum_plot<-renderPlotly({
   plot_data<-drugs_plot_data()
   plot_data<- plot_data %>%
         mutate(month = format(Date, "%m"), year = format(Date, "%Y")) 
   plot_data1<-subset(plot_data,Date<'2021-01-04')
   plot_21<-subset(plot_data,(Date>='2021-01-04'& Date <='2022-01-02'))
   plot_22<-subset(plot_data,Date>='2022-01-03')
   #aggregating data by month
   plot_data1<-aggregate(.~ month + year,data=plot_data1[,c(3,4,7,8)],FUN=sum, na.rm=T)
   plot_21<-aggregate(.~ month + year,data=plot_21[,c(3,4,7,8)],FUN=sum, na.rm=T)
   plot_22<-aggregate(.~ month + year,data=plot_22[,c(3,4,7,8)],FUN=sum, na.rm=T)
   #adding 2019 week to jan 2020 week
   plot_data1$`Raw 20/21`[2]<-plot_data1$`Raw 20/21`[2]+plot_data1$`Raw 20/21`[1]
   plot_data1$`Raw 2018/19`[2]<-plot_data1$`Raw 2018/19`[2]+plot_data1$`Raw 2018/19`[1]
   plot_21$`Raw 20/21`[2]<-plot_21$`Raw 20/21`[2]+plot_21$`Raw 20/21`[1]
   #removing first row 
   plot_data1<-plot_data1[-1,]
   
   lab_text<-function(x,y,z){
     c(paste0("Year: ", x,
              "<br>", 'Month: ', y,
              "<br>", "Cumulative number of SAS naloxone incidents: ", z))
   }
   y_1819<-cumsum(plot_data1$`Raw 2018/19`)
   y_20<- cumsum(plot_data1$`Raw 20/21`)
   y_21<-cumsum(plot_21$`Raw 20/21`)
   y_22<-cumsum(plot_22$`Raw 20/21`)
   
   trend<-plot_ly(data = plot_data1, x =seq(1:(nrow(plot_data1)+1))) %>% 
     add_trace(y =c(0,y_1819),name='Average \n2018-2019',type='scatter', 
               mode='lines',line=list(color='green', dash='dot'),
               text=lab_text('Average 2018-2019',c('',month.abb[as.numeric(plot_data1$month)]),c(0,y_1819)),
               hoverinfo='text') %>% 
    add_trace(y = c(0,y_20),name='2020',type='scatter', mode='lines',
              line=list(color='black'),
              text=lab_text('2020',c('',month.abb[as.numeric(plot_data1$month)]),c(0,y_20)),
              hoverinfo='text') %>% 
    add_trace(data=plot_21,x=seq(1:(nrow(plot_21)+1)), y=c(0,y_21), name='2021', 
              type='scatter',mode='lines',line=list(color='blue'),
              text=lab_text('2021',c('',month.abb[as.numeric(plot_21$month)]),c(0,y_21)),
              hoverinfo='text') %>% 
    add_trace(data=plot_22,x=seq(1:(nrow(plot_22)+1)), y=c(0,y_22), name='2022', 
              type='scatter',mode='lines',line=list(color='orange'),
              text=lab_text('2022',c('',month.abb[as.numeric(plot_22$month)]),c(0,y_22)),
              hoverinfo='text') %>% 
     layout(
       title=(sprintf("Cumulative number of SAS incidents where naloxone was administered in 2020, 2021, and 2022 \n compared with 2018-19 average (%s)",location())),
       xaxis=list(
         tickvals=seq(1:(nrow(plot_data1)+1)),
         ticktext=c('',month.abb[as.numeric(plot_data1$month)]),
         title='Month',
         fixedrange=TRUE),
       yaxis = list(title = "Cumulative number SAS naloxone incidents",
                    fixedrange=TRUE)) %>% 
     add_vline(x = '4.77', text = "1st lockdown", margin = list(t = 80, l = 2)) %>% 
     config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
   })
   
   withSpinner(plotlyOutput('cum_plot'))
   
  }}

})


output$drugs_perc_change<-renderUI({
  
  if (input$`drugs-measure` == 'Drug and alcohol treatment referrals') {
    
    plot_data<-drugs_plot_data()
    
    if(length(which(is.na(plot_data$Change)))==0){

      output$change_plot<-renderPlotly({

    tooltip_trend<-c(paste0(
                          "Date: ", format(plot_data$Date, format = "%b %d, %Y"),
                          "<br>", "Change from 2018-2019 average: ",ifelse(plot_data$Change >= 0, "+", ""), plot_data$Change, "%"))
    change<-plot_ly(data = plot_data, x = ~Date, y = ~Change,
                    type='scatter', mode='lines',
                    line=list(color=pal_overall[1]),
                    text=tooltip_trend,
                    hoverinfo="text") %>% 
      layout(
      title = (sprintf("Percentage difference in the number of %s treatment referrals in 2020, 2021, and 2022 \n compared with 2018-2019 average (%s)",tolower(input$types),location())),
      yaxis = list(title = "% Change", fixedrange=TRUE),
      xaxis=list(fixedrange=TRUE)) %>% 
      add_vline(x = '2020-03-23', text = "1st lockdown", margin = list(t=80)) %>% 
      config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
      })
      withSpinner(plotlyOutput('change_plot',width='90%'))
    }  
    
   
        else if( input$types == 'Co-dependency'){

      output$data_message<-renderText('Percentage difference plot not shown due to the ‘co-dependency’ client type not being available as a separate breakdown in the earlier years.')
textOutput('data_message')
  }
    
    else if(length(which(is.na(plot_data$Change)))!=0){

      output$data_message<-renderText('Percent difference plot not shown due to \'not applicable\' values being produced by comparison with 0 values in 2018/2019 average.')
      textOutput('data_message')
    }
  }
  
})

output$drugs_quan_plot<-renderUI({
  if (input$`drugs-measure`=='OST prescribing'){
    
    output$quan_plot<-renderPlotly({
      
      plot_quantity<-subset(OST_paid_quantity,(Board==location()) & (Type==input$types))
      lab_text<-c(paste0("Month: ", plot_quantity$Date,
                         "<br>", 'Quantity: ', plot_quantity$Quantity, ' mg'))
      trend<-plot_ly(data = plot_quantity,x=seq(1:nrow(plot_quantity))) %>% 
        add_trace(y = ~ Quantity,type='scatter', mode='lines', line=list(color=pal_overall[1]),text=lab_text,hoverinfo='text') %>% 
        layout(
        title=(sprintf('Total quantity (mg) of %s prescribed per month since January 2018 (%s)',tolower(input$types),location())),
        xaxis=list(
          title=('Date'),
          tickmode='array',
          tickvals=seq(1,nrow(plot_quantity),6),
          ticktext=as.character(plot_quantity$Date)[c( TRUE , rep(FALSE, 5)) ],
          fixedrange=TRUE),
        yaxis=list(title='Quantity (mg)', rangemode='tozero', fixedrange=TRUE)) %>% 
        add_vline(x = '27.77', text = "1st lockdown", margin = list(t=80)) %>% 
        config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    })
    withSpinner(plotlyOutput('quan_plot'))
  }
  
})
  
## A&E Drug attendances by sex
output$drug_gender_plot<-renderPlotly({
  
  plot_drug_sex <- subset(drugs_plot_data(), (Board == location()) & (Gender %in% c("All", "Male","Female")))
  
  lab_text<-c(paste0("Average of weeks beginning: ", format(plot_drug_sex$Date-7, "%d %b %Y"), ", ", format(plot_drug_sex$Date, "%d %b %Y"), ", ", format(plot_drug_sex$Date+7, "%d %b %Y"),
                     "<br>", 'Number of attendances: ', round(plot_drug_sex$`2020 & 2021`,1)))
  
  trend_sex <- plot_ly(data = plot_drug_sex, 
                   x = ~ Date) %>% 
    add_trace(y = ~`2020 & 2021`,
              color = ~ Gender,
              type = 'scatter',
              mode = 'lines', 
              colors = pal_sex,
              text = lab_text,
              hoverinfo = 'text') %>% 
    layout(xaxis = list(fixedrange=TRUE, title='Date'), 
           yaxis = list(title = "Number of attendances",
                        rangemode='tozero', fixedrange=TRUE)) %>% 
    add_vline(x = '2020-03-23', text = "1st lockdown", margin = list(t=80)) %>% 
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
})

## A&E Drug attendances - Pct change
output$drug_ae_change_plot<-renderPlotly({
  
  plot_data <- subset(drugs_plot_data(), (Gender == "All"))
  
  if(length(which(is.na(plot_data$Change)))==0){      
      tooltip_trend<-c(paste0(
        "Average of weeks beginning: ", format(plot_data$Date-7, "%d %b %Y"), ", ", format(plot_data$Date, "%d %b %Y"), ", ", format(plot_data$Date+7, "%d %b %Y"),
        "<br>", "Change from 2018-2019 average: ",ifelse(plot_data$Change >= 0, "+", ""), round(plot_data$Change,1), "%"))
      
      change<-plot_ly(data = plot_data, x = ~Date, y = ~Change,
                      type='scatter',
                      mode='lines',
                      line=list(color=pal_overall[1]),
                      text=tooltip_trend,
                      hoverinfo="text") %>% 
        layout(
        yaxis = list(title = "% Change", fixedrange=TRUE),
        xaxis=list(fixedrange=TRUE)) %>% 
        add_vline(x = '2020-03-23', text = "1st lockdown", margin = list(t=80)) %>% 
        config(displaylogo = F,  displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove)
    }
  else if(length(which(is.na(plot_data$Change)))!=0){
  
  output$data_message<-renderText('Percent difference plot not shown due to \'not applicable\' values being produced by comparison with 0 values in 2018/2019 average.')
  textOutput('data_message')
}

})    
###############################################.
## Reactive layout ----
###############################################.

## This section combines the two A&E sub-plots together (Pct change and Gender)    
output$drugs_ae_explorer <- renderUI({
  
  note_average <- p("Please note that due to small numbers we are presenting 3-week rolling average figures.")
  
  note_dataqual <- p("Important note: It is not possible to accurately report total attendances for specific conditions using the national A&E dataset, due to the quality of the data available.  
                     Diagnosis/reason for attendance can be recorded in a variety of ways, including in free text fields - and not all NHS Boards submit this information.  
                     The numbers presented in these dashboards therefore give only a high level indication of differences over time and by age and sex, and should be interpreted with caution.  
                     Breakdowns by SIMD are not felt to be reliable, as they could be heavily skewed by the demographic profile of the areas represented in the data available. PHS are planning work to improve consistency.")
  
  note_smallboards <- p("")
  
  if (input$`drugs-measure`=='A&E attendances for drug overdose/intoxication') {
   if(location()=='Scotland') {
     tagList(note_dataqual, note_average, 
      plot_cut_box(title_plot1 = paste0("Percentage change in the number of A&E attendances for Drug overdose/intoxications \nin ", location(), " (2020-2022) compared with average of the corresponding time in 2018 and 2019"), 
                   plot_output1 = "drug_ae_change_plot",
                   title_plot2 = paste0("3-Week average of number of attendances for Drug overdose/intoxication \nat Emergency Departments  by sex (", location(),", 2020-2022)"),
                   plot_output2 = "drug_gender_plot"))
   } else {
     tagList(note_smallboards)
     }
  }
})


  #### Data for download ####

output$download_drugs_data <- downloadHandler(
  filename ="drugs_extract.csv",
  content = function(file) {

    if(input$`drugs-measure`=='Drug and alcohol treatment referrals'){
      write_csv(DTR_data,
                file) }

    else if(input$`drugs-measure`=='Take home naloxone kits'){
      write_csv(THN_by_HB,
                file) }
    
    else if(input$`drugs-measure`=='SAS naloxone administration'){
      write_csv(SASdata[c(1,2,5,6)],
                file) }
    else if(input$`drugs-measure`=='OST prescribing'){
      write_csv(OST_paid,
                file)
      write_csv(OST_paid_quantity,
                file) }
    else if(input$`drugs-measure`=='A&E attendances for drug overdose/intoxication'){
      if (location() == "Scotland") {
        x <- Drug_AE_attendances %>%
          filter(Board == "Scotland") %>%
          select(Geography_type, Board, Gender, Date, Type, `2020 & 2021`, `Average 2018 & 2019`) %>%
          rename(`2020, 2021 & 2022` = `2020 & 2021`) %>%
          arrange(Gender, Date)
      } else {
        x <- subset(Drug_AE_attendances,(Board==location() & Gender == "All"))
        x <- x %>%
          select(Geography_type, Board, Gender, Date, Type, `2020 & 2021`, `Average 2018 & 2019`) %>%
          rename(`2020, 2021 & 2022` = `2020 & 2021`) %>%
          mutate(`2020, 2021 & 2022` = as.character(`2020, 2021 & 2022`),
                 `2020, 2021 & 2022` = replace_na(`2020, 2021 & 2022`, "c"),
                 `Average 2018 & 2019` = as.character(`Average 2018 & 2019`),
                 `Average 2018 & 2019` = replace_na(`Average 2018 & 2019`, "c")) %>%
          arrange(Date)
      }
      
      write_csv(x,
                file) }
  }
)
  
  
## END


