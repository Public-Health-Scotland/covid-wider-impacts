# Wider impacts dashboard - Summary trends tab
# UI code

summary_tab <- 
  tabPanel(title = "Summary trends", icon = icon("area-chart"), value = "summary",
           wellPanel(#actionButton("browser", "browser"),
             column(3, selectdata_ui("summary", measure_choices = data_list)),
             column(4,
                    conditionalPanel(condition = "input['summary-measure'] != 'op' ", 
                                     selectgeo_ui("summary", area_choices =  c("Scotland", "Health board", "HSC partnership"))),
                    # If outpatients selected bring other set of choices
                    conditionalPanel(condition = "input['summary-measure'] == 'op' ", 
                                     selectgeo_ui("op", area_choices = c("Scotland", "Health board of treatment",
                                                                "Health board of residence",
                                                                "HSC partnership of residence")))
             ),
             column(3,
                    conditionalPanel(condition = "input['summary-measure'] != 'op' ", 
                                     selectInput("adm_type", label = "Step 3. Select type of admission.",
                                                 choices = c("All", "Emergency", "Planned"), selected = "All")),
                    conditionalPanel(condition = "input['summary-measure'] == 'ooh' ", 
                                     selectInput("ooh_appt_type", label = "Step 4. Select type of appointment for overall chart.",
                                                 choices = c("All cases", "All consultations" = "ALL",
                                                             "Covid consultations" = "COVID", "Non-covid consultations" = "NON COVID"),
                                                 selected = "All cases")),
                    conditionalPanel(condition = "input['summary-measure'] == 'op' ",
                                     selectInput("appt_type", label = "Step 3. Select type of appointment.",
                                                 choices = c("All", "New", "Return"), selected = "All")),
                    conditionalPanel(condition = "input['summary-measure'] == 'op' ",
                                     selectInput("time_type", label = "Step 4. Select weekly or monthly data.",
                                                 choices = c("Weekly", "Monthly"), selected = "Weekly"))),
             column(2,
                    downloadButton('download_chart_data', 'Download data'), br(), br(),
                    actionButton('summary-commentary','Go to commentary'))
           ), #wellPanel bracket
           mainPanel(width = 12,
                     uiOutput("data_explorer")
           )# mainPanel bracket
         
) # tabpanel bracket


###############################################.
## Commentary  ----
###############################################.

summary_commentary <- 
  tagList(
    fluidRow(
    column(8, h2("Summary trends")), 
    column(4, div(bsButton("jump_to_summary", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    p(h3("15 June 2022 - Outpatient appointments")),
    p("Data are taken from Scottish Morbidity Record (SMR00) and show weekly outpatient appointments to week ending 26 December 2021, 
      with monthly information shown to 31 December 2021. Further information is available by following the 'Data source: SMR00' 
      links on the dashboard."),
    h4("Initial findings: outpatient appointments"),
    tags$ul(
      tags$li("Outpatient appointments fell from the second week of March 2020 onwards: by week ending 19 April 2020, all outpatient 
              appointments had fallen by over two-thirds (-68%) compared to the average of the same week in 2018–19 (from an average 
              of 86,765 in 2018–19 to 27,523 in 2020)."),
      tags$li("This impact was similar across sexes, age groups and deprivation groups. For example, the fall in all appointments was 
              greatest in patients aged 85 and over, dropping by almost three-quarters (-72%), while appointments for patients aged 15–44 
              dropped by two-thirds (-66%). However, by the week ending 26 December 2021, these reductions were 31% for patients aged 85 
              and over and 28% for patients aged 15–44. "),
      tags$li("There were larger relative falls for surgical (-76%) than medical (-64%) specialties in the early stages of the pandemic. 
              However, by week ending 26 December 2021, medical specialties showed a reduction of over a quarter (-29%), while surgical 
              specialties showed a reduction of over one third (-34%) compared to average values for the same week in 2018–19."),
      tags$li("There were larger decreases and a slower recovery in new outpatient appointments
              than in return outpatient appointments."),
      tags$li("Outpatient appointments have generally been recovering from the end of April 2020 onwards but are still not up to 
              pre-pandemic levels. For example, for the week ending 26 December 2021, the total number of appointments remains at around 
              31% below the average of the same week in 2018–19."),
      tags$li("There has been a very large increase in the number of appointments carried out remotely via telephone and videolink. 
              In week ending 26th December 2021, just under one sixth (15%) of all appointments was conducted by telephone, 
              and 1 in 25 (4%) was by videolink. These modes of clinical interaction were uncommon prior to March 2020 but have 
              consistently made up around one fifth of outpatient activity since then."),
      tags$li("The impact of the pandemic on outpatient appointments was similar across ethnic groups; however, interpretation by ethnic 
              group is complicated by the mandating of recording of ethnic group on SMR outpatient (SMR00) returns from 1 February 2021. 
              This is reflected in the fall in the number of appointments with a missing ethnic group, which were 22% lower by December 
              2021 than the corresponding time in 2018–19."),
      tags$li("In December 2021, appointments for patients with the 'White Scottish' ethnic group recorded were around 4% lower than the 
              corresponding time in 2018–19; the number of appointments in other ethnic groups varies between 28% higher (‘Caribbean or 
              Black’) and 4% lower (‘White Other’). It is important to note that the trends for ethnic groups with small populations should 
              be interpreted with caution, as they will be subject to greater variability due to small numbers.")
      ),
    h4("Interpreting these figures"),
    p("Please exercise caution when interpreting these figures, as these data are for management information only.
      For more information on methodology and data quality, please see the ",
      tags$a(href = "https://publichealthscotland.scot/publications/acute-hospital-activity-and-nhs-beds-information-quarterly/",
             "Acute Activity and NHS Beds quarterly publication.",
             target="_blank")),
      
      h3("23 September 2020 - Revision of baseline OOH"),
      p("An issue with previously published 2018 and 2019 baseline Out of Hours (OOH) data was
        identified and has now been corrected. OOH figures from January 2018 to 22nd March 2020 had previously
        referred to numbers of consultations whereas those presented after 23rd March referred to numbers of cases.
        A correction has been applied to ensure that the full time series is now based on numbers of OOH cases.
        The impact of this revision is modest and does not materially affect interpretation of the changes observed in
        post-pandemic activity.
        At a national level adjusting the baseline data has resulted in a reduction in the baseline OOH figure of approximately 10% (1,600).
        The post-pandemic reductions in OOH activity previously reported were also over-estimated
        by around 6% each week, and this has now been corrected. The impact of the data revisions at a sub-national level may vary."),
      
      h3("10 June 2020 - Excess mortality"),
      p("Each week National Records for Scotland (NRS) release provisional deaths data and a ",
        tags$a(href="https://www.nrscotland.gov.uk/covid19stats", "report (external website)",  target="_blank"), 
        " about the numbers of deaths involving COVID-19 in Scotland.
      NRS report that weekly excess mortality (defined as deaths from any cause in 2020,
      both COVID-19 and non-COVID-19, compared with the average of the previous five years)
      peaked at 80% higher in the week ending 12 April, and had reduced to 17% higher by
      the most recent week (ending 24 May)."),
      p("PHS are using the NRS data to provide further insight about excess mortality
      by sex, age group, area deprivation (quintiles of Scottish Index of Multiple Deprivation 2020),
      as well as at NHS Board and HSCP level. Thet numbers of deaths from any
      cause increased markedly at all levels of area deprivation from early April 2020.
      The excess deaths for each SIMD quintile compared with the 2015 to 2019 average
      was between 72% and 98% in the week ending 19 April, and had reduced to less than 25%
      for all quintiles by the latest week (ending 24 May)."),
      
      h3("3 June 2020"),
      p("From the second week of March 2020 there was an abrupt and steep fall in hospital admissions, 
        attendances at Accident and Emergency (A&E) departments and cases in out of hours services.
        Use of all of these services fell to around half the average levels seen 2018-19 and has since recovered
        only slightly. Numbers of NHS 24 111 completed contacts did not change appreciably though the data presented
        here do not include additional NHS 24 services specific to COVID-19. There was a small fall in attended
        ambulance incidents. Further analyses are presented in this interactive online tool."),
      h4("Background"),
      p("The COVID-19 pandemic has direct impacts on health as a result of illness, 
        hospitalisations and deaths due to COVID-19. However, the pandemic also has wider impacts on health and 
        on health inequalities. Reasons for this may include:"),
      tags$ul(
        tags$li("Individuals being reluctant to use health services because they do not want to
                burden the NHS or are anxious about the risk of infection."),
        tags$li("The health service delaying preventative and non-urgent care such as
                some screening services and planned surgery."),
        tags$li("Other indirect effects of interventions to control COVID-19, such as mental or
                physical consequences of distancing measures.")),
      p("Public Health Scotland aims to provide information and intelligence on the wider 
        impacts of COVID-19 on health, healthcare and health inequalities that are not directly due to COVID-19."),
      p("We have focused initially on using the national datasets that are returned to PHS most quickly, 
        as these allow us to monitor impacts with the minimum delay. The work to date has made use of the following data sources:"),
      tags$ul(
        tags$li("The RAPID (rapid and preliminary inpatient data) hospital admissions database."),
        tags$li("A&E attendances."),
        tags$li("NHS 24 completed contacts."),
        tags$li("Out of hours cases."),
        tags$li("Scottish Ambulance Service data.")),
      h4("Initial findings: hospital admissions"),
      tags$ul(
        tags$li("Hospital admissions fell sharply from the second week of March, reaching levels
                nearly 50% below those expected on the basis of admissions during 2018-19."),
        tags$li("There has been some recovery since late April, but numbers of admissions remain
                around 35% below the 2018-19 average."),
        tags$li("Similar patterns are seen by sex and by deprivation, but falls were larger for children
                under 14 years and smaller for those aged 85 years and over."),
        tags$li("There were larger relative falls for surgical than medical specialties."),
        tags$li("There were much larger falls in planned admissions (around 65%) than in 
                emergency admissions (around 40%)."),
        tags$li("There were particularly large falls (around 60%) for emergency paediatric admissions."),
        tags$li("The pattern was broadly similar across NHS Boards; the low level of recorded admissions 
                in NHS Forth Valley is likely to be due to data quality problems.")),
      h4("Initial findings: unscheduled care"),
      tags$ul(
        tags$li("Other data sources showed changes with similar time patterns to those seen for
                hospital admissions. There were larger falls (nearly 60%) for A&E attendances, with similar patterns by 
                age, sex and deprivation."),
        tags$li("NHS 24 111 completed contacts rose substantially for working age adults,
                but fell to around 50% of previous levels for children under 15 years of age, with little sign of recovery
                to previous levels. However it is important to note that while these figures include some contacts related
                to COVID-19, they do not include additional services set up to respond directly to COVID-19.
                Compared to previous years, percentage falls in completed contacts were smaller among those
                living in more deprived areas."),
        tags$li("Compared to earlier years there were large percentage falls (around 55% overall) in
                cases in out of hours services, especially for children, where the fall was around 70%."),
        tags$li("There were more modest falls in attended ambulance incidents (around 15% overall) 
                though the fall was much larger for children (around 50%). These figures include incidents related to COVID-19.")),
      h4("Interpreting these figures"),
      p("These analyses are based on a selected range of data sources that are available to describe
        changes in health service use in Scotland during the COVID-19 pandemic. Hospital admissions, attendances
        at A&E departments, contacts with the NHS 24 111 completed contacts and cases in out of hours
        services all fell to around half the average levels seen 2018-19 and have since recovered only modestly.
        There was a smaller fall in attended ambulance incidents and no appreciable change in NHS 24 111 completed
        contacts. These falls are likely to reflect a range of factors, including public anxiety about using NHS
        services, changes in the delivery of NHS services in response to rising numbers of COVID-19 hospital admissions
        and actions to defer planned activity in order to be prepared for expected COVID-19 related demand.
        The changes preceded by around a week the introduction of social distancing measures. The impact was
        particularly large for children under 14 years, with larger percentage falls in hospital admissions, NHS 24 111
        completed contacts, out of hours cases and ambulance incidents. As expected, the falls in hospital
        admissions were larger for planned than for emergency admissions and larger for surgical than medical
        admissions. There was little evidence from these data sources that social inequalities in the use of these
        services increased during this period."),
      h4("Future work"),
      p("Work is under way to broaden the range of data sources available – within the next few weeks
      we expect to publish information on health visitor checks, perinatal mortality,
      excess mortality (in collaboration with NRS), prescribing and cardiovascular presentations."),
      br()
      )


#END 