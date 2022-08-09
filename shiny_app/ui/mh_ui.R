# Wider impacts dashboard - Mental health tab
# UI code

mh_tab <- 
  tabPanel(title = "Mental health", icon = icon("brain"), value = "mentalhealth",
           wellPanel(
             column(4, selectdata_ui("mh", measure_choices = mentalhealth_list)),
             column(4, selectgeo_ui("mh", area_choices =  c("Scotland", "Health board", "HSC partnership"))),
             column(4, downloadButton("download_mentalhealth_data", "Download data"),
                    fluidRow(br()),
                    actionButton('jump_commentary_mentalhealth','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("mh_explorer")
           )# mainPanel bracket
  ) #tabPanel bracket

###############################################.
## Commentary ----
###############################################.

mentalhealth_commentary <- 
  tagList(
    fluidRow(
      column(8, h2("Mental health")), 
      column(4, div(bsButton("jump_to_mentalhealth", label = "Go to data"), style="float:right"))),  #this button can only be used once
    h3("15 June 2022"),
    p("Information on the number of patients starting a new treatment course for selected mental health medicines (those commonly used for depression, anxiety or insomnia) through General Practice was included for the first time in the COVID Wider Impacts Dashboard on 30 September 2020. This data does not include hospital prescribing. These data indicate:"),
    tags$ul(
      tags$li("The number of patients starting new treatment with the selected medicines fell by almost 40% between the week ending 22nd March, 2020 and the week ending 5th April, 2020 compared with the previous two years' average for the same period. This period corresponds with the first national lockdown in response to COVID-19 in Scotland. Since then, the total numbers have been gradually increasing but have generally remained below the 2018-2019 baseline levels to April 2022."),
      tags$li("Looking at the selected medicines in separate groups, the number of new treatment courses for depression returned to expected in July 2020, whilst new treatment courses for insomnia and anxiety continued to remain below the 2018-2019 baseline to May 2022."),
      tags$li("Observed downward spikes in the trend seen around the Christmas Periods in late December/early January reflect low overall activity in those periods, most likely due to reduced access over the holiday periods.")
    ),
    h3("30 September 2020"),
    h4("Unscheduled care"),
    p("Information on the number of contacts for mental health problems with accident and emergency (A&E) and with primary care out of hours (OOH)
       services was included for the first time on 30 September 2020."),
    tags$ul(
      tags$li("Compared to the pattern seen in previous years, there was a sharp fall of 30-40% in out of hours (OOH) contacts for mental health problems, starting in early March 2020."),
      tags$li("Numbers of OOH contacts for mental health problems remained below the previous average until late April, corresponding to the period of ‘lockdown’ in Scotland.
               Between April and the end of July numbers of contacts rose to around 10% above the previous average."),
      tags$li("The trend in OOH contacts was similar for males and females, and also broadly similar by age and by level of deprivation, with wide fluctuations in numbers of contacts from week to week."),
      tags$li("A&E attendances for mental health problems fell by 40-50% from early March 2020 and by the beginning of September had still not fully recovered, remaining at around 10% below previous levels."),
      tags$li("The trend in A&E attendances was similar for males and females and also broadly similar by age group and by level of deprivation, with wide fluctuations in numbers of contacts from week to week."),
      tags$li("Overall, these falls in the use of unscheduled care for mental health problems are likely to reflect the impact of the Covid-19 pandemic. More detailed discussion of these points is provided on the home page of the dashboard.")),
    h4("Prescribing"),
    p("Information on the number of patients starting a new treatment course for selected mental health medicines (those commonly used for depression, anxiety or
      insomnia) through General Practice has been included for the first time on 30 September 2020. This data indicates:"),
    tags$ul(
      tags$li("The number of patients starting new treatment with the selected medicines fell by almost 40% between the week prior to the introduction of lockdown and early April compared with the previous years' average for the same period.
              Since then, the total numbers have been gradually increasing but have generally remained below normal levels ."),
      tags$li("The number of new treatment courses with medicines for anxiety, depression and insomnia all fell sharply following the introduction of lockdown.
              The number of new treatments courses for depression has returned to expected levels since mid July.
              In early September, new treatment courses for insomnia and anxiety are 25% below activity in 2018 and 2019.  ")),
    br()
  )
