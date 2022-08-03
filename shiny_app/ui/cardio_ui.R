# Wider impacts dashboard - Cardiovascular tab
# UI code

cardio_tab <- 
  tabPanel(title = "Cardiovascular", icon = icon("heartbeat"), value = "cardio",
           wellPanel(
             column(4, div(title="Select the data you want to explore.", # tooltip
                           radioGroupButtons("measure_cardio_select",
                                             label= "Step 1 – Select the data you want to explore.",
                                             choices = cardio_list, status = "primary",
                                             direction = "vertical", justified = T))),
             column(4, selectizeInput("area_cardio_select", "Step 2 - Select the area of interest",
                                      choices = c("Scotland"), selected = "Scotland"),
                    uiOutput("geoname_cardio_ui")),
             column(4,  selectInput("diagnosis_select", label = "Step 3. Select diagnosis",
                                    choices = c("Heart Attack","Heart Failure","Stroke"), selected = "Heart Attack")),
             column(4, downloadButton('download_cardio_data', 'Download data'),
                    fluidRow(br()),
                    actionButton('jump_commentary_cardio','Go to commentary'))
           ), #wellPanel bracket
           mainPanel(width = 12,
                     uiOutput("cardio_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket


###############################################.
## Commentary ----
###############################################.
cardio_commentary <- 
  tagList(
 #this button can only be used once
    fluidRow(
      column(8, h2("Cardiovascular")), 
      column(4, div(bsButton("jump_to_cardio", label = "Go to data"), style="float:right"))),
    h3("5 May 2022"),
    h4("Cardiovascular hospital admissions and excess mortality"),
    tags$ul(
      tags$li("Data now available for cardiovascular hospital admissions and excess mortality.
              Information is available by quarter for diagnosis Heart Attack, Heart Failure and Stroke.")),
    
    h3("16 December 2020"),
    h4("Cardiovascular GP out of hour cases"),
    tags$ul(
      tags$li("For GP out of hours services there was a sharp fall of around 30% in cases for cardiovascular problems
              that started in early March 2020, some weeks prior to the introduction of ‘lockdown’ measures in Scotland.
              Contact numbers did not return to previous levels until early April, and during April, May and June were
              around 20% above the average for 2018-19. Trends were similar by age group and deprivation.")),
    h4("Cardiovascular Scottish Ambulance Service incidents"),
    tags$ul(
      tags$li("For Scottish Ambulance Service incidents, there was a sharp initial fall of around 40% in cardiovascular
              related incidents that started in early April 2020, shortly after the introduction of lockdown restrictions.
              This continued until mid-July. The fall in incidents was greatest in the most deprived groups.")),
    
    h3("17 June 2020"),
    h4("Prescribing"),
    p("Information on prescriptions issued for cardiovascular medicines through
      General Practice has been included for the first time on 17th June 2020.
      These data indicate that:"),
    tags$ul(
      tags$li("The number of prescriptions for cardiovascular medicines overall rose sharply in the third week of March, 
              increasing by approximately 35% when compared to the average for the same time period in 2018 and 2019."),
      tags$li("When examining specific groups of cardiovascular medicines routinely prescribed in primary care a similar pattern is seen:",
              tags$ul(
                tags$li("The number of prescriptions rose sharply in March and peaked in the third week."),
                tags$li("The number of prescriptions in April was below that expected from the 2018/2019 average and is 
                        likely a consequence of early ordering of repeat supplies in March."),
                tags$li("By the end of May, the numbers of prescriptions were returning to normal levels."))
              )
      ),
    h4("Cardiovascular A&E attendances"),
    p("Information on cardiovascular attendances at Accident & Emergency Departments is presented in this tool.
      This data is based on coding available in the Accident & Emergency Datamart (managed by Public Health Scotland).
      Note that, due to limitations in diagnosis recording in the A&E datamart, the data are incomplete for a number of
      NHS Boards. Thus, the figures reported for cardiovascular-related attendances offer only a very approximate
      indication of attendances. Additionally, some NHS Boards have moved to a new recording standard which has not
      been fully consolidated in the A&E datamart as yet. As a result, figures for 2020, even prior to the
      introduction of lockdown measures, appear somewhat lower when compared to previous years."), 
    tags$ul(
      tags$li("Overall there was a sharp drop in cardiovascular attendances at Accident and Emergency Departments starting 
              in early March 2020. Attendances were around 60% lower compared to the 2018-2019 average. Levels rose again 
              by the end of May, but remain around 30% below the 2018-19 average."),
      tags$li("This drop in cardiovascular attendances was consistent across both males andfemales, in younger and older 
              patients and across deprivation quintiles.")),
    h4("Cardiac procedures"),
    p("Information on cardiac procedures has been obtained from two of the four cardiac care centres in Scotland 
      (Royal Infirmary of Edinburgh and Golden Jubilee National Hospital). Data on the number of procedures was collected for:"),
    tags$ul(
      tags$li("coronary angiography (an investigation to evaluate whether there is any narrowing of the arteries supplying the heart);"),
            tags$li("cardiac devices, including pacemakers to treat rhythm problems of the heart and"),
            tags$li("percutaneous coronary interventions, cardiac procedures to treat narrowing of the arteries supplying the heart.")),
    p("The major observations are as follows:"),
    tags$ul(
      tags$li("Overall, the number of coronary angiographies dropped from early March 2020. A significant proportion of
               these procedures are elective and these activities are likely to have been reduced in late March 2020."),
      tags$li("The change in percutaneous coronary interventions has been less pronounced. A significant
               proportion of coronary interventions occur in a context of patients suffering a heart
               attack. A proportion of coronary interventions are also planned and elective in nature. "),
      tags$li("The number of device procedures has been lower than expected since the end of March 2020.")),
    br()
    )



#END