#Code for child health ui sections

##############################################.
# Immunisations ----
##############################################.
immunisations_tab <- 
  tabPanel(title = "Immunisations", value = "imm",
         wellPanel(#actionButton("browser", "browser"),
           column(4, selectdata_ui("immun", measure_choices = data_list_immun)),
           column(4, selectgeo_ui("immun", area_choices =  c("Scotland", "Health board")),
                  div(title="Select the time periods you want to explore. You can click in the box then click on time periods in the dropdown to add them, or click on the x to remove a time period.",
                      p(tags$b("Step 3. Select time periods of interest.")),
                      uiOutput("dates_ui_immun"),
                      actionButton("btn_update_time_immun", "Update time periods")
                  )
           ),
           column(4, sourcemodal_ui("immun"),
                  fluidRow(br()),
                  actionButton("imm_elig_defs", "Eligibility definitions",  icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton('download_imm_data', 'Download data'),
                  fluidRow(br()),
                  actionButton('immun-commentary','Go to commentary')
           )
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("immunisation_explorer")
         )# mainPanel bracket
) # tabpanel bracket

      ###############################################.
      # Immunisation commentary ----
      ###############################################.

immun_commentary_section <- 
  tagList(
    fluidRow(
      column(8, h2("Immunisations")), 
      column(4, div(bsButton("jump_to_immunisation", label = "Go to data"), style="float:right"))),  #this button can only be used once

    h3("2 March 2022"),
    p("When a cohort becomes eligible for any of the immunisations reported this data will now only be 
      refreshed for the next 12 months; as the data becomes more complete, uptake rates for these cohorts 
      stabilise within this period. Only minor changes to uptake rates are observed if the data is updated 
      monthly beyond 12 months and this is mainly driven by movements into or out of the cohort, such as a 
      child leaving Scotland. Older data will continue to be reported but will no longer be refreshed."),
    
    h3("6 October 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 6 October and 
      includes information on cohorts eligible for their immunisations to week beginning 9 August 2021. 
      It should be noted that the data recorded for the most recent eligible cohorts will not be fully 
      complete at this stage. This means that immunisation uptake is likely to be under-reported and 
      will be updated as the data becomes more complete."),
    p("Please note that going forward the dashboard will continue to be updated on the first Wednesday 
      of each month, but the commentary will only be updated in the case of exceptions. Background 
      information on interpreting the data is provided in the commentary for previous updates below. 
      Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://publichealthscotland.scot/publications/childhood-immunisation-statistics-scotland/",
             "official statistics publications", target="_blank"), "."),
    
    h3("1 September 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 1 September 
      and includes information on cohorts eligible for their immunisations to week beginning 5 July 2021."),
    p("Background information on interpreting the data is provided in the commentary for previous updates below."),
    
    h3("4 August 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 4 August and 
      includes information on cohorts eligible for their immunisations to week beginning 7 June 2021."),
    p("Background information on interpreting the data is provided in the commentary for previous updates below."),
    
    h3("7 July 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 7 July. 
      It should be noted that the data recorded for the most recent eligible cohorts will not be 
      fully complete at this stage. This means that immunisation uptake is likely to be under-reported 
      and will be updated as the data becomes more complete."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during 
      the Covid-19 pandemic. Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://publichealthscotland.scot/publications/childhood-immunisation-statistics-scotland/",
             "official statistics publications", target="_blank"), "."),
    
    h3("2 June 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 2 June. 
      It should be noted that the data recorded for the most recent eligible cohorts will not be 
      fully complete at this stage. This means that immunisation uptake is likely to be under-reported 
      and will be updated as the data becomes more complete."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during 
      the Covid-19 pandemic. Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"), "."),
    
    h3("5 May 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 5 May. 
      Information is provided on children becoming eligible for immunisation during the Covid-19 
      pandemic (in March 2020 to February 2021) as well as before the pandemic (2019, January 2020, 
      and February 2020). The data downloads include more detailed information, including by Health 
      and Social Care Partnership, and weekly cohorts (note that due to small numbers of children 
      in the Island Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are presented 
      for monthly and yearly cohorts only)."),
    p("It should be noted that the data recorded for the most recent eligible cohorts will not be 
      fully complete at this stage. This means that immunisation uptake is likely to be under-reported 
      and will be updated as the data becomes more complete."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during 
      the Covid-19 pandemic. Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"), "."),
    
    h3("7 April 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 7 April. 
      The charts and table show annual data for children who became eligible for immunisation in 2019 
      (before the pandemic) and in this update we have added annual data for children who became 
      eligible in 2020. To ensure the display is not over-crowded we have also reduced the number 
      of monthly cohorts shown in the charts and table to the latest available 6 months (August 2020 
      to January 2021)."),
    p("The full data including monthly data for children eligible from January 2020 and weekly data 
      for children eligible up to week beginning 8 February 2021 are available through the data download. 
      In future updates we plan to add drop-down functionality so that users have the option to choose 
      the time-periods to show in the charts and table."),
    p("It should be noted that the data recorded for the most recent eligible cohorts, including for 
      2020, will not be fully complete at this stage. This means that immunisation uptake is likely 
      to be under-reported and will be updated as the data becomes more complete. Data for a few children 
      are not included in the eligible cohort and uptake figures due to an issue in the source data. 
      The impact of this on the reported rates at Scotland level will be minor."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during 
      the Covid-19 pandemic. The data shows that early uptake (achieved by 4 weeks after the children 
      became eligible for their immunisation) was higher in 2020 than in 2019. For detail on some 
      observations in the pattern of uptake during the pandemic see previous commentary. Information 
      on final achieved uptake will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"), "."),
    
    h3("3 March 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 3 March. 
      Monthly data are provided on children who became eligible during the COVID-19 pandemic 
      (in March 2020 to December 2020) as well as before the pandemic (2019, January 2020 and 
      February 2020). Weekly data are available through the data download, and includes data for 
      children eligible up to week beginning 4 January 2021. It should be noted the immunisation 
      uptake data recorded for the most recent eligible cohorts will not be fully complete at this 
      stage. Data for a few children are not included in the eligible cohort and uptake figures due 
      to an issue in the source data. The impact on the reported rates at Scotland level will be minor."),
    p("Although early uptake of each of the vaccines appears noticeably lower for children who became 
      eligible in December 2020 compared with previous months, additional investigation of the 
      2019 baseline data showed comparable decreases in early uptake were also seen for children who 
      became eligible in December 2019. The data also showed that while early uptake was lower, 
      uptake among children eligible in December 2019 subsequently increased to levels comparable 
      with 2019 as a whole. The decreases in early uptake for children who became eligible in December 
      2020 are therefore not unusual and are thought to relate to the Christmas holiday period."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during 
      the Covid-19 pandemic. For detail on some observations in the pattern of uptake during the pandemic 
      see previous commentary including 2 September 2020 below. Information on final achieved uptake 
      will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"), "."),
    
    h3("3 February 2021"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 3 February. 
      Monthly data are provided on children who became eligible during the COVID-19 pandemic (in March 
      2020 to November 2020) as well as before the pandemic (2019, January 2020 and February 2020). 
      Weekly data are available through the data download, and includes data for children eligible 
      up to week beginning 7 December 2020. It should be noted the immunisation uptake data recorded 
      for the most recent eligible cohorts will not be fully complete at this stage."),
    p("The data issue affecting the figures in the previous release appears to be resolved, with 
      further checks on the data ongoing (see commentary for 23 December 2020 below)."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during 
      the Covid-19 pandemic. For detail on some observations in the pattern of uptake during the pandemic 
      see previous commentary including 2 September 2020 below. Information on final achieved uptake 
      will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"), "."),
    
    h3("23 December 2020"),
    p("Information on the uptake of pre-school immunisations was updated in this tool on 23 December. 
      Monthly data are provided on children who became eligible during the COVID-19 pandemic (in March 
      2020 to September 2020) as well as before the pandemic (2019, January 2020 and February 2020). 
      Weekly data are no longer shown in the charts and tables but are available through the data 
      download, and includes data for children eligible up to week beginning 19 October 2020. It should 
      be noted the immunisation uptake data recorded for the most recent eligible cohorts will not be 
      fully complete at this stage."),
    p("In this update some uptake rates are slightly under-reported; this is due to an issue this month 
      with the source data which has affected the accuracy of the eligible cohort data for all time-periods. 
      At Scotland level the monthly and weekly uptake rates are thought to be under-reported by -0.1 to -1.0 %. 
      The impact on some NHS Board and Health & Social Care Partnership rates will be greater. It is 
      anticipated the data issue will have been corrected in the next planned update of the dashboard 
      on 3 February 2020."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during the 
      Covid-19 pandemic. Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"), "."),
    
    h3("4 November 2020"),
    p("In this release of information on uptake of pre-school immunisations data have been updated 
      to include children who became eligible until early September."),
    p("Uptake of pre-school immunisations has remained high for children who became eligible during 
      the Covid-19 pandemic. Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"),"."),
    
    h3("7 October 2020"),
    p("In this release of information on uptake of pre-school immunisations data have been updated to 
      include children who became eligible until early August. The overall trends described in the 
      commentary for 2 September 2020 below continue to apply."),
    
    h3("2 September 2020"),
    p("Information on uptake of pre-school immunisations was updated in this tool on 2 September. 
      The updated data show that uptake of pre-school immunisations for children who became eligible 
      during March 2020 was maintained at a similar level to that seen before the Covid-19 pandemic 
      (children becoming eligible in 2019 and early 2020). Early uptake for children becoming eligible 
      for their immunisation more recently (April 2020 through to early July 2020) has increased, and 
      is now noticeably higher than that seen before the pandemic."),
    p("The data also show that the increase in early uptake of immunisations seen from April 2020 onwards 
      has been seen for children from all deprivation levels. For the 3 doses of the 6-in-1 immunisation, 
      the recent increase in early uptake has been highest in children from the most deprived areas, 
      resulting in a reduction in inequality in early uptake for these immunisations. For the 2 doses of 
      the MMR immunisation, the recent increase in early uptake has been broadly similar across deprivation groups."),
    p("As discussed in the previous commentary below, there are a number of likely reasons for the recent 
      improvement in early uptake of pre-school immunisations. These include increased awareness among 
      parents of the importance of immunisation reinforced by national communications to encourage attendance, 
      as well as local communications and new processes introduced in response to the pandemic. For example, 
      immunisation teams in some NHS Boards have recently been phoning parents/carers shortly before the 
      day of appointment to ensure families are free of symptoms of Covid-19 before attending, reassure them, 
      and answer questions."),
    p("Although recent improvements in early immunisation uptake rates are evident, often among children 
      living in the most deprived areas in particular, it is too soon to determine whether this early 
      improvement will translate into improved final uptake and a reduction in the inequalities gap when 
      measured at later ages.  Information on final achieved uptake will continue to be provided through ",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "official statistics publications", target="_blank"), "."),
    
    h3("12 August 2020"),
    p("Information on uptake of pre-school immunisations was updated in this tool on 12 August (and new 
      information was added to the data download function on uptake in Health and Social Care Partnerships 
      and in the Island NHS Boards).  The updated data show that uptake of pre-school immunisations for 
      children who became eligible during March 2020 was maintained at a similar level to that seen before 
      the Covid-19 pandemic (children becoming eligible in 2019 and early 2020).  Early uptake for children 
      becoming eligible for their immunisation more recently (April 2020 through to early June 2020) has 
      increased, and is now noticeably higher than that seen before the pandemic."),
    p("New information on uptake of pre-school immunisations for children living in areas with different 
      levels of deprivation (Scotland level only) was also added to this tool on 12 August.  Early uptake 
      (achieved by 4 weeks after the children became eligible for their immunisation) is considered, as this 
      indicator is available for the most recent cohorts of children as well as the baseline 2019 cohort. 
      The data show that before the Covid-19 pandemic, children living in the most deprived areas of Scotland 
      were less likely to have received their pre-school immunisations within 4 weeks of becoming eligible 
      than children living in the least deprived areas."),
    p("The new data show that the increase in early uptake of immunisations seen from April 2020 onwards has 
      been seen for children from all deprivation levels. For the 3 doses of the 6-in-1 immunisation, the 
      recent increase in early uptake has been highest in children from the most deprived areas, resulting 
      in a reduction in inequality in early uptake for these immunisations.  For the 2 doses of the MMR 
      immunisation, the recent increase in early uptake has been broadly similar across deprivation groups."),
    p("As discussed in the previous commentary below, there are a number of likely reasons for the recent 
      improvement in early uptake of pre-school immunisations.  These include increased awareness among 
      parents of the importance of immunisation reinforced by national communications to encourage attendance, 
      as well as local communications and new processes introduced in response to the pandemic. For example, 
      immunisation teams in some NHS Boards have recently been phoning parents/carers shortly before the day 
      of appointment to ensure families are free of symptoms of Covid-19 before attending, reassure them, 
      and answer questions."),
    
    h3("8 July 2020"),
    p("On 8 July, information on uptake of the first and second doses of MMR vaccine was added to the tool."),
    p("The first dose of MMR vaccine is offered from 12 months of age at the immunisation appointment 
      scheduled at 12-13 months. Data before the pandemic, for children eligible (turning 12 months) 
      in 2019 show that uptake in Scotland was 65.4% by the time children turned 13 months old. Uptake 
      rates by 13 months were maintained for children eligible in March 2020 and have increased for children 
      eligible in April and early May 2020, with uptake in each of the latest 4 weeks exceeding 75%. This means 
      in April and early May, more children than usual received their immunisation soon after they first 
      became eligible, indicating fewer non-attendances at, or postponements of, scheduled appointments."),
    p("There are a number of likely reasons for this improved early uptake, including increased awareness 
      among parents of the importance of immunisation reinforced by national communications to encourage 
      attendance, as well as local communications and new processes introduced in response to the pandemic. 
      For example, immunisation teams in some NHS Boards have recently been phoning parents/carers shortly 
      before the day of appointment to ensure families are free of symptoms of Covid-19 before attending, 
      reassure them, and answer questions. Although more children received the first dose of MMR immunised 
      by 13 months of age, it is too early to determine whether this will result in any increase in the uptake 
      of the vaccine at 16 months of age, as measured in the tool, or later when measured at the standard 
      reporting age of 2 years in the",
      tags$a(href="https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/",
             "routinely published statistics", target="_blank",".")),
    p("The second dose of MMR vaccine is offered at 3 year 4 months. Data before the pandemic, for children 
      eligible in 2019 show that uptake was 52.0% by 3 years 5 months. There was a small decrease in uptake 
      rates by 3 years 5 months for children eligible for immunisation in March 2020 to 49.6%. However, as 
      seen for the first dose of MMR, early uptake rates (by 3 years 5 months) have since increased for 
      children eligible in April and early May 2020, with uptake in each of the latest 3 weeks exceeding 60%."),
    p("This release also includes updated uptake data on each of the doses of the 6-in-1 vaccine, offered 
      at 8, 12 and 16 weeks, to include children eligible in each week in April and early May 2020. Uptake 
      of each of the doses have been maintained throughout the pandemic. For children eligible in April 
      and early May, the pattern of more children than usual receiving their immunisations soon after 
      becoming eligible, is also observed, most notably for the third dose of vaccine, although the effect 
      is less pronounced than was observed for the MMR immunisations. This is because uptake within 4 weeks 
      of becoming eligible is already high for immunisations offered at the earliest ages, as shown in the 
      data before the pandemic. Doses of vaccine which are routinely offered later in schedule of childhood 
      immunisations take longer to reach the high levels of uptake compared to the immunisations offered at 
      the first appointment due to the cumulative effect of missed appointments, and the need to have 
      appropriate intervals between receiving doses of vaccine."),
    
    h3("17 June 2020"), 
    p("Information on the uptake of ",
      tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","immunisations that 
             are routinely offered to all preschool children (external website)", target="_blank"),
      " has been included in this tool for the first time on 3 June 2020.", br(),
      "Immunisation protects children against many serious infectious diseases including diphtheria, 
      whooping cough, and measles.",
      tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
             "Immunisation services throughout Scotland are continuing during the Covid-19 pandemic (external website)",  target="_blank"),".",
      "It is important to maintain the best possible immunisation uptake rates to ensure children 
      remain protected and to prevent a resurgence of these infections.  Including information on 
      childhood immunisation rates in this tool will help us to ensure that immunisation rates remain 
      high throughout the pandemic."),
    p("The 6-in-1 vaccine is given to babies at 8, 12 and 16 weeks of age. The vaccine protects against 
      diphtheria, tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and 
      Hepatitis B."),
    p("On 3 June 2020, information was provided on the uptake of the first dose of the 6-in-1 vaccine, 
      offered at 8 weeks of age. This showed uptake continues to exceed 90% among children who were 
      due their first dose of the 6-in-1 vaccine in March and early April."),
    p("On 17 June, information on the uptake of the second and third doses was added to the tool. 
      The second dose of 6-in-1 vaccine is offered at 12 weeks of age. Data before the pandemic, for 
      children eligible in 2019, show that uptake of the second dose by 16 weeks was 84.5%. Uptake by 
      16 weeks continues to exceed 80% among children who were due their second dose of the 6-in-1 vaccine 
      in March and early April."),
    p("The third dose of 6-in-1 vaccine is offered at 16 weeks of age. Data before the pandemic, for 
      children eligible in 2019, show that uptake of the third dose by 20 weeks was 72.3%. Uptake by 
      20 weeks continues to exceed 70% among children who were due their third dose of the 6-in-1 
      vaccine in March and early April."),
    p("It is important to note that uptake of the second and third doses take longer to reach 90% 
      and above compared to the first dose, as demonstrated by the data on uptake before the pandemic. 
      This is because some children receive the first dose later than when first offered the vaccine, 
      for example due to missed appointments. As each dose of vaccine is offered 4 weeks apart, missed 
      appointments has a cumulative effect in increasing the time it takes for uptake of the second 
      and third doses to reach and exceed 90%."),
    
    h3("3 June 2020"),
    p("Information on the uptake of ",
      tags$a(href="https://www.nhsinform.scot/healthy-living/immunisation","immunisations that 
             are routinely offered to all preschool children (external website)", target="_blank"),
      " has been included in this tool for the first time on 3 June 2020.", br(),
      "Immunisation protects children against many serious infectious diseases including diphtheria, 
      whooping cough, and measles.",
      tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/healthy-living/coronavirus-covid-19-immunisation-and-screening",
             "Immunisation services throughout Scotland are continuing during the Covid-19 pandemic (external website)",  target="_blank"),".",
      "It is important to maintain the best possible immunisation uptake rates to ensure children 
      remain protected and to prevent a resurgence of these infections.  Including information on 
      childhood immunisation rates in this tool will help us to ensure that immunisation rates remain 
      high throughout the pandemic."),
    p("On 3 June 2020, information has been provided on the uptake of the first dose of the 6-in-1 
      vaccine, which is offered to children at 8 weeks of age. The vaccine protects against diphtheria, 
      tetanus, pertussis (whooping cough), polio, Haemophilus influenzae type b (Hib) and Hepatitis B. 
      Children should also receive a second dose of the vaccine at 12 weeks and a third dose at 16 weeks."),
    p("Uptake rates for this immunisation have remained high during the pandemic. Uptake continues to 
      exceed 90% among children who were due their first dose of the 6-in-1 vaccine in March and early 
      April. The recording of data on immunisations given by the reporting date will not be fully complete 
      at this stage, particularly for the most recent cohorts, so uptake rates are slightly under-reported. 
      In addition, some children will receive the vaccine at a later age, for example due to missed or 
      rescheduled appointments, so uptake rates are expected to continue to increase as children age 
      (as shown in the 2019 data provided for comparison)."), 
    br()
    )



##############################################.
# Child Health reviews ----
#############################################.
childreview_tab <- 
  tabPanel(title = "Child health reviews", value = "child_health",
         wellPanel(
           column(4, selectdata_ui("childr", measure_choices = data_list_child)), 
           column(4, selectgeo_ui("childr", area_choices =  c("Scotland", "Health board")),
                  div(title="Select the time periods you want to explore. You can click in the box then click on time periods in the dropdown to add them, or click on the x to remove a time period.",
                      p(tags$b("Step 3. Select time periods of interest.")),
                      uiOutput("dates_ui_child"),
                      actionButton("btn_update_time_child", "Update time periods"))),
           column(4, sourcemodal_ui("childr"),
                  fluidRow(br()),
                  downloadButton("download_visit_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_hv","Go to commentary"))
           #actionButton("browser", "Browser")
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("child_health_explorer")
         )# mainPanel bracket
)# tabpanel bracket


      ###############################################.
      ## Child health reviews commentary ----
      ###############################################.
# This also includes breastfeeding and child development data
child_comments <-
  tagList(
    fluidRow(
      column(8, h2("Child health reviews")), 
      column(4, div(bsButton("jump_to_childreview", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("1 September 2021"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 1 September, 
      and includes information on children becoming eligible for review up to June 2021. Background 
      information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
    p("Please note that going forward the dashboard will continue to be updated on the first Wednesday 
      of each month, but the commentary will only be updated in the case of exceptions."),
    
    h3("4 August 2021"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 4 August, 
      and includes information on children becoming eligible for review up to May 2021. Background 
      information on interpreting the data is provided in the commentary for 8 and 15 July 2020 below."),
    
    h3("7 July 2021"),
    h4("What is reported?"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 7 July. 
      Information is provided on children becoming eligible for a review during the Covid-19 pandemic 
      (in March 2020 to April 2021) as well as before the pandemic (2019, January 2020, and February 2020). 
      The month and year time periods for which data is shown in the chart and table is now selectable 
      using “Step 3. Select time periods of interest.” The data downloads include more detailed information, 
      including by Health and Social Care Partnership, and weekly cohorts (note that due to small numbers 
      of children in the Island Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are 
      presented for monthly and yearly cohorts only)."),
    h4("Findings"),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during 
      the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all 
      other reviews was lower for children who became eligible in the early months of the pandemic, than 
      in 2019. However, data from summer 2020 onwards shows that coverage appears to be recovering, with 
      reviews happening in a more timely manner."),
    p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, 
      particularly the 4-5 year review, it will take some time for final achieved coverage to be known. 
      Information on final achieved coverage will continue to be provided through PHS ",
      tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 8 and 
      15 July 2020 below."),
    
    h3("2 June 2021"),
    h4("What is reported?"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 2 June. 
      Information is provided on children becoming eligible for a review during the Covid-19 pandemic 
      (in March 2020 to March 2021) as well as before the pandemic (2019, January 2020, and February 2020). 
      The month and year time periods for which data is shown in the chart and table is now selectable 
      using “Step 3. Select time periods of interest.” The data downloads include more detailed information, 
      including by Health and Social Care Partnership, and weekly cohorts (note that due to small numbers of 
      children in the Island Boards, results for NHS Orkney, NHS Shetland and NHS Western Isles are presented 
      for monthly and yearly cohorts only)."),
    h4("Findings"),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during 
      the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all 
      other reviews was lower for children who became eligible in the early months of the pandemic, than in 
      2019. However, data from summer 2020 onwards shows that coverage appears to be recovering, with reviews 
      happening in a more timely manner."),
    p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, 
      particularly the 4-5 year review, it will take some time for final achieved coverage to be known. 
      Information on final achieved coverage will continue to be provided through PHS ",
      tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 8 and 
      15 July 2020 below."),
    
    h3("5 May 2021"),
    h4("What is reported?"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 5 May. 
      Information is provided on children becoming eligible for a review during the Covid-19 pandemic 
      (in March 2020 to February 2021) as well as before the pandemic (2019, January 2020, and February 
      2020). Due to the volume of data available, the charts and table now show annual data for children 
      who became eligible for review in 2019 and 2020, and monthly data for children who became eligible 
      for review in the most recent 6 months for which data are available. The data downloads include 
      more detailed information, including by Health and Social Care Partnership, and weekly cohorts 
      (note that due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland 
      and NHS Western Isles are presented for monthly and yearly cohorts only)."),
    h4("Findings"),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during 
      the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all 
      other reviews was lower for children who became eligible in the early months of the pandemic, than in 
      2019. However, data from summer 2020 onwards shows that coverage appears to be recovering, with reviews 
      happening in a more timely manner."),
    p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, 
      particularly the 4-5 year review, it will take some time for final achieved coverage to be known. 
      Information on final achieved coverage will continue to be provided through PHS ",
      tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 8 and 15 
      July 2020 below."),
    
    h3("7 April 2021"),
    h4("What is reported?"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 7 April. 
      Information is provided on children becoming eligible for a review during the Covid-19 pandemic 
      (in March 2020 to January 2021) as well as before the pandemic (2019, January 2020, and February 
      2020). Due to the volume of data available, the charts and table now show annual data for children 
      who became eligible for review in 2019 and 2020, and monthly data for children who became eligible 
      for review in the most recent 6 months for which data are available. The data downloads include more 
      detailed information, including by Health and Social Care Partnership, and weekly cohorts (note that 
      due to small numbers of children in the Island Boards, results for NHS Orkney, NHS Shetland and NHS 
      Western Isles are presented for monthly and yearly cohorts only)."),
    p("Data for a small number children are not included in the eligible cohort and coverage figures due 
      to an issue in the source data. The impact on the reported rates at Scotland level will be minor."),
    h4("Findings"),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during 
      the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of all
      other reviews was lower for children who became eligible in the early months of the pandemic, than in 
      2019. However, data from summer 2020 onwards shows that coverage appears to be recovering, with 
      reviews happening in a more timely manner."),
    p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, 
      particularly the 4-5 year review, it will take some time for final achieved coverage to be known. 
      Information on final achieved coverage will continue to be provided through PHS ",
      tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 8 and 
      15 July 2020 below."),
    
    h3("3 March 2021"),
    h4("What is reported?"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 3 March. 
      Information is provided on children becoming eligible for a review during the Covid-19 pandemic 
      (in March 2020 to December 2020) as well as before the pandemic (2019, January 2020, and February 
      2020). Weekly data are no longer shown in the charts and tables but are available through the data 
      download, and includes data for children eligible up to week beginning 4 January 2021. It should 
      be noted that the coverage data recorded for the most recent eligible cohorts will not be fully 
      complete at this stage. Data for a few children are not included in the eligible cohort and coverage 
      figures due to an issue in the source data. The impact on the reported rates at Scotland level 
      will be minor."),
    h4("Findings"),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during 
      the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of
      all other reviews was lower for children who became eligible in March and April 2020, than in 2019. 
      There is some evidence of ‘catch-up’, with coverage for March and April improving with time, however 
      coverage still lags behind 2019 levels."),
    p("For the later child health reviews, which have a much longer timeframe for reviews to be delivered, 
      particularly the 4-5 year review, it will take some time for final achieved coverage to be known. 
      Information on final achieved coverage will continue to be provided through PHS ",
      tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 8 and 
      15 July 2020 below."),
    
    h3("3 February 2021"),
    h4("What is reported?"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 3 February. 
      Information is provided on children becoming eligible for a review during the Covid-19 pandemic 
      (in March 2020 to November 2020) as well as before the pandemic (2019, January 2020, and February 
      2020). Weekly data are no longer shown in the charts and tables but are available through the data 
      download, and includes data for children eligible up to week beginning 7 December 2020. It should 
      be noted that the coverage data recorded for the most recent eligible cohorts will not be fully 
      complete at this stage."),
    h4("Data quality"),
    p("The data issue affecting the figures in the previous release appears to be resolved, with further 
      checks on the data ongoing (see commentary for 23 December 2020 below)."),
    h4("Findings"),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible 
      throughout the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. 
      Coverage of the 6-8 week review (at 10 weeks), was 6 percentage points lower in March 2020 than 
      in 2019, but in subsequent months coverage has been similar to the previous year. For the older 
      age group reviews, an impact on coverage is apparent for children who became eligible in March 
      and April 2020. There is some evidence of ‘catch-up’, with coverage for these cohorts improving 
      with time, however coverage to date still lags behind 2019 levels for these months. For children 
      becoming eligible from May to September 2020, coverage to date for the 13-15 month review is 
      slightly higher than the 2019 level, and for the 27-30 month review is slightly lower. Although, 
      for the later child health reviews, which have a much longer timeframe for reviews to be delivered, 
      particularly the 4-5 year review, it will take some time for final achieved coverage to be known. 
      Information on final achieved coverage will continue to be provided through ",
      tags$a(href = "https://publichealthscotland.scot/publications/", "PHS official statistics publications.",  target="_blank")),
    p("Further background information on interpreting the data is provided in the commentary for 
      8 and 15 July 2020 below."),
    
    h3("23 December 2020"),
    h4("What is reported?"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 
      23 December. Information is provided on children becoming eligible for a review during the 
      Covid-19 pandemic (in March 2020 to September 2020) as well as before the pandemic (2019, 
      January 2020, and February 2020). Weekly data are no longer shown in the charts and tables 
      but are available through the data download, and includes data for children eligible up to 
      week beginning 19 October 2020. It should be noted that the coverage data recorded for the 
      most recent eligible cohorts will not be fully complete at this stage."),
    h4("Data quality"),
    p("In this update some coverage rates are slightly under-reported; this is due to an issue 
      this month with the source data which has affected the accuracy of the eligible cohort data 
      for all time-periods. At Scotland level the monthly and weekly coverage rates are thought 
      to be under-reported by -0.1 to -1.0 %. The impact on some NHS Board and Health & Social 
      Care Partnership rates will be greater. It is anticipated the data issue will have been 
      corrected in the next planned update of the dashboard on 3 February 2020."),
    p("It should also be noted that NHS Greater Glasgow & Clyde have a backlog of 13-15 month, 
      27-30 month, and 4-5 year reviews to be entered into CHSP due to staffing shortages. 
      Entry of the first visit and 6-8 week review data has been prioritised, so these are up-to-date."),
    h4("Findings"),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible 
      during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. 
      Coverage of all other reviews was lower for children who became eligible in March and April 
      2020, than in 2019. There is some evidence of ‘catch-up’, with coverage for March and April 
      improving with time, however coverage still lags behind 2019 levels."),
    p("For the later child health reviews, which have a much longer timeframe for reviews to be 
      delivered, particularly the 4-5 year review, it will take some time for final achieved 
      coverage to be known. Information on final achieved coverage will continue to be provided 
      through PHS ",
      tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 
      8 and 15 July 2020 below."),
    
    h3("4 November 2020"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 4 November. 
      Information is provided on children becoming eligible for a review during the Covid-19 pandemic 
      (in March 2020 to August 2020) as well as before the pandemic (2019, January 2020, and February 
      2020). Information has now been added at Health & Social Care Partnership level and this available 
      through the data download function. Weekly data are no longer shown in the charts but are available 
      through the data download."),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible during 
      the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. Coverage of 
      all other reviews was lower for children who became eligible in March and April 2020, than in 2019. 
      There is some evidence of ‘catch-up’, with coverage for March and April improving with time, however 
      coverage still lags behind 2019 levels. There is some evidence that coverage has been lower in 
      June and July, which may be attributable to services focusing on delivery of ‘catch-up’ reviews 
      for children who became eligible earlier in the year."),
    p("For the later child health reviews, which have a much longer timeframe for reviews to be 
      delivered, particularly the 4-5 year review, it will take some time for final achieved coverage 
      to be known. Information on final achieved coverage will continue to be provided through PHS ",   
      tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 8 
      and 15 July 2020 below."),
    
    h3("7 October 2020"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 7 October. 
      Information is provided on children becoming eligible for a review during the Covid-19 pandemic 
      (in March 2020 to early August 2020) as well as before the pandemic (2019, January 2020, and 
      February 2020)."),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible 
      during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. 
      Coverage of all other reviews was lower for children who became eligible in March and April 
      2020, than in 2019. Recent data show that rates are recovering in the majority of NHS Boards, 
      with coverage for the 6-8 week and 13-15 month reviews returning to pre-pandemic levels by 
      May 2020. For the later child health reviews, which have a much longer timeframe for reviews 
      to be delivered, particularly the 4-5 year review, it will take some time for final achieved 
      coverage to be known. There is some evidence of ‘catch-up’, with coverage for March and April 
      improving with time, and in some boards coverage in May and June exceeds pre-pandemic levels. 
      Information on final achieved coverage will continue to be provided through PHS ",
      tags$a(href = "https://publichealthscotland.scot/publications/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 
      8 and 15 July 2020 below."),
    
    h3("2 September 2020"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 2 
      September. Information is provided on children becoming eligible for a review during the 
      Covid-19 pandemic (in March 2020 to early July 2020) as well as before the pandemic (2019, 
      January 2020, and February 2020)."),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible 
      during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. 
      Coverage of all other reviews had fallen for children eligible since March 2020. Recent 
      data show that rates are beginning to recover in most, but not all, NHS Boards. There is 
      some evidence of ‘catch-up’, with coverage for March and April improving with time, but 
      this has still not reached the levels achieved in 2019. For the later child health reviews, 
      which have a much longer timeframe for reviews to be delivered, particularly the 4-5 year 
      review, it will take some time for final achieved coverage to be known. Information on final 
      achieved coverage will continue to be provided through official statistics publications."),
    p("Further background information on interpreting the data is provided in the commentary for 
      8 and 15 July 2020 below."),
    
    h3("12 August 2020"),
    p("Information on uptake of pre-school child health reviews was updated in this tool on 12 
      August. Information is now provided on children becoming eligible for a review during the 
      Covid-19 pandemic (in March 2020 to early June 2020) as well as before the pandemic (2019, 
      January 2020, and February 2020)."),
    p("Coverage of the Health Visitor first visit has remained high for children becoming eligible 
      during the pandemic, with more than 95% of babies receiving their review by 6 weeks of age. 
      Coverage of all other review had fallen for children eligible in March 2020, but recent data 
      for April and May show that rates are beginning to recover in most, but not all, NHS Boards. 
      For the later child health reviews, which have a much longer timeframe for reviews to be 
      delivered, it will take some time for final achieved coverage to be known. Information on 
      final achieved coverage will continue to be provided through ",
      tags$a(href = "https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/", "official statistics publications",  target="_blank"), "."),
    p("Further background information on interpreting the data is provided in the commentary for 
      8 and 15 July 2020 below."),
    
    h3("15 July 2020"), 
    p("Information on the uptake of child health reviews that are routinely offered to all preschool 
      children by Health Visitors was included in this tool for the first time on 10 June 2020. 
      Data was subsequently refreshed on 8 July 2020.  Commentary relating to those releases is 
      provided below."),
    p("Information on coverage of the 4-5 year review was included for the first time on 15 July 
      2020. Data from before the pandemic, for children becoming eligible in 2019, show that coverage of the 4-5 year review by 49 months was 11%, rising to 29% by 52 months. Coverage continues to increase as children age beyond this point. Overall coverage for children eligible in 2019 was 52% by the time data was extracted for analysis (22 June 2020). This is a fairly new review, which has actually not been implemented in all board areas yet (no data is shown for NHS Dumfries & Galloway as they implemented the review in May 2020, and NHS Highland are scheduled to implement the review on 3 August 2020) and Government policy states that this review should be offered to all children turning 4 years old from April 2020 onwards. Therefore, we expect the baseline for 2019 to be low for this review as it is still becoming established. However, data for children eligible in January and February 2020 show that coverage was gradually beginning to rise before it fell in March 2020, and weekly data for April shows coverage by 49 months is between 5-9%. These children have not yet reached 52 months of age, and we would expect coverage to increase over time.", br(), br(),
      "In general, the impact of the Covid-19 pandemic on early coverage of the 4-5 year review has been very variable between Boards. Coverage has been well maintained in some areas, but is very low for children becoming eligible for review during the pandemic in other areas.  In areas showing low coverage, this may be due to Health Visitors prioritising the earlier reviews (first visit and 6-8 weeks). In addition, ", 
      tags$a(href="https://www.gov.scot/publications/coronavirus-covid-19-nursing-and-community-health-staff-guidance/",
             "national guidance (external website)", target="_blank"), 
      " during the pandemic has recommended that the earlier reviews (first visit, 6-8w) should 
      continue as face to face contacts whereas the later reviews (13-15m, 27-30m, 4-5 yr) should 
      also continue, but be provided via NHS near-me (a secure video conferencing facility) or 
      telephone where possible. In some NHS Board areas this remote delivery of reviews has not 
      been possible, and this is reflected in the data."),
    
    h3("8 July 2020"), 
    p("Information on the uptake of child health reviews that are routinely offered to all preschool 
      children by Health Visitors was included in this tool for the first time on 10 June 2020. 
      Child health reviews incorporate assessment of children's health, development, and wider 
      wellbeing alongside provision of health promotion advice and parenting support. Routine 
      child health reviews help ensure that children’s health and development is progressing as 
      expected for their age and stage, and allow any concerns to be addressed. It is important 
      that children continue to receive their routine health reviews during the Covid-19 pandemic. 
      On 10 June 2020, information was provided on the coverage of the Health Visitor first visit, 
      which is offered to children at 10-14 days of age. Coverage of the 6-8 week review was added 
      on 24 June, and coverage of the 13-15 month and 27-30 month review were added on 8 July (and 
      data for the first visit and 6-8 week review were also updated)."),
    p("Coverage rates for the Health Visitor first visit have remained high during the pandemic. 
      Coverage continues to exceed 90% among children who were due their review in March and April."),
    p("Data from before the pandemic, for children becoming eligible in 2019, show that coverage 
      of the 6-8 week review by 10 weeks was 83%. Coverage has gradually fallen since the beginning 
      of 2020, and was around 69-77% for children becoming eligible for the review in March and April."),
    p("There are a number of important factors to take into account when interpreting this information. 
      Unlike all the other pre-school child health reviews, the 6-8 week review is a two stage process 
      involving the baby’s Health Visitor and their GP. Usually, the Health Visitor first visits the 
      family at home to conduct a general assessment of the baby’s progress and the family’s wellbeing. 
      Then, the GP offers an appointment in the practice to conduct a detailed physical examination of 
      the baby. Usually the GP appointment happens shortly after the Health Visitor visit, and the 
      data from both assessments is then returned together to the NHS Board child health administrative 
      department for entry into the CHSP-PS system. Since the start of the Covid-19 pandemic, 
      Scottish Government policy has been that the 6-8 week review should continue to be provided. 
      In practice, to minimise the number of times babies are brought into the practice, in some 
      areas the GP element of the review may have been deferred, for example until the baby is due 
      a routine immunisation appointment at a later stage. Areas may then vary in terms of whether 
      Health Visitors return information on their part of the 6-8 week review for entry into the 
      baby’s CHSP-PS record, or whether no information is returned until the GP part of the review 
      is completed. As GPs start to ‘catch up’ with their part of outstanding 6-8 week reviews, we 
      would expect to see coverage rates for children becoming eligible for this review during the 
      pandemic increasing. It is important to note therefore that no record of a 6-8 week review on 
      the CHSP-PS system does not necessarily mean that the baby has not been seen at all: they may 
      have been visited by their Health Visitor, but not yet examined by their GP."),
    p("For babies born prematurely, the 6-8 week review is offered to children 6-8 weeks following 
      their expected date of delivery rather than their actual date of birth. This is to ensure a 
      ‘fair’ assessment of children’s progress against what would be expected of a baby at that stage. 
      As the information shown in the dashboard is based on children’s actual date of birth rather 
      than due dates, premature babies will appear to have their review ‘late’, when in fact it was 
      offered appropriately. This partially accounts for why coverage of the 6-8 week review continues 
      to increase as babies attain older ages. Finally, it should also be restated that we have 
      allowed a 6 week window for review completion and data entry, that is we have reported on 
      reviews provided to children becoming eligible for their reviews up to 6 weeks prior to the 
      date we extracted data from the CHSP-PS system. The results of a completed review would generally 
      be expected to be entered into the CHSP-PS system within this 6 week time frame. In practice 
      however occasional data entry delays occur. These may be worse during the Covid-19 pandemic, 
      and may vary between areas. For all these reasons, review coverage for the most recent cohorts 
      should therefore be taken as provisional, and is likely to increase over time as relevant data 
      accumulates."),
    p("Data for the 13-15 month review show that 41% of children becoming eligible for review in 2019 
      had received their review by 14 months, rising to 81% by 17 months. Coverage continues to increase as 
      children age beyond this point.  Overall coverage for children eligible in 2019 was 84% by the time 
      data was extracted for analysis (22 June 2020). Early coverage of the 13-15 month review was 
      noticeably lower for children becoming eligible during the Covid-19 pandemic. Among children 
      eligible in March and April 2020, 26-35% had received their review by 14 months. These children 
      have not yet reached 17 months of age, and we would expect coverage to increase over time."),
    p("Data for the 27-30 month review show that 33% of children becoming eligible for review in 2019 
      had received their review by 28 months, rising to 81% by 31 months. Coverage continues to increase 
      as children age beyond this point. Overall coverage for children eligible in 2019 was 90% by the 
      time data was extracted for analysis (22 June 2020). Early coverage of the 27-30 month review 
      was noticeably lower for children becoming eligible during the Covid-19 pandemic. Among children 
      eligible in March and April 2020, 20-26% had received their review by 28 months. These children 
      have not yet reached 31 months of age, and we would expect coverage to increase over time. It 
      should be noted that NHS Greater Glasgow & Clyde have a different policy for calling children 
      for this review: they call at 30 months rather than 27 months as in the rest of Scotland. Hence, 
      coverage by 28 months for children in NHS Greater Glasgow & Clyde is very low: this also affects 
      the overall figures for Scotland as NHS GG&C is a large Board."),
    p("In general, the impact of the Covid-19 pandemic on early coverage of the 13-15 month and 27-30 
      month reviews has been very variable between Boards. Coverage has been well maintained in some 
      areas, but is very low for children becoming eligible for review during the pandemic in other 
      areas.  In areas showing low coverage, this may be due to Health Visitors prioritising the earlier 
      reviews (first visit and 6-8 weeks). In addition, ", tags$a(href="https://www.gov.scot/publications/coronavirus-covid-19-nursing-and-community-health-staff-guidance/",
                                                                  "national guidance (external website)", target="_blank"), 
      " during the pandemic has recommended that the earlier reviews (first visit, 6-8w) should 
      continue as face to face contacts whereas the later reviews (13-15m, 27-30m) should also continue, 
      but be provided via NHS near-me (a secure video conferencing facility) or telephone where possible. 
      In some NHS Board areas this remote delivery of reviews has not been possible, and this is reflected 
      in the data."),
    br()
  )


##############################################.
# Breastfeeding  ----
#############################################.
breastfeeding_tab <- 
  tabPanel(title = "Breastfeeding", value = "breastfeeding",
           filters_ui(id = "bf", measure_choices = data_list_bf,
                      area_choices = c("Scotland", "Health board")), # Filters and options
         mainPanel(width = 12,
                   uiOutput("breastfeeding_explorer")
         )# mainPanel bracket
) # tabpanel bracket

    ###############################################.
    # Breastfeeding commentary ----
    ###############################################.
breastfeeding_commentary <-
  tagList(
    fluidRow(
      column(8, h2("Breastfeeding")), 
      column(4, div(bsButton("jump_to_breastfed", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("1 September 2021"),
    p("Information on breastfeeding has been updated in this tool on 1 September 2021. 
      This is based on data recorded at child health reviews undertaken by health visiting 
      teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old. 
      Data is shown by month of review from January 2019 to June 2021. Background information 
      on interpreting the data is provided in the commentary for 30 September 2020 below."),
    p("Please note that going forward the dashboard will continue to be updated on the first 
      Wednesday of each month, but the commentary will only be updated in the case of exceptions."),
    
    h3("4 August 2021"),
    p("Information on breastfeeding has been updated in this tool on 4 August 2021. This is 
      based on data recorded at child health reviews undertaken by health visiting teams when 
      babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old. Data is 
      shown by month of review from January 2019 to May 2021. Background information on 
      interpreting the data is provided in the commentary for 30 September 2020 below."),
    
    h3("7 July 2021"),
    p("Information on breastfeeding has been updated in this tool on 7 July 2021. This is 
      based on data recorded at child health reviews undertaken by health visiting teams 
      when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to April 2021, so comparisons 
      can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information at Health & Social Care Partnership level is available in the data 
      download function."),
    p("At Scotland level, the data show that there was a small increase in the overall 
      proportion of babies recorded as having been breastfed at both the Heath Visitor 
      first visit, and 6-8 week review in the early months of the pandemic, but this now 
      appears to have fallen back to pre-pandemic levels."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("2 June 2021"),
    p("Information on breastfeeding has been updated in this tool on 2 June 2021. This is 
      based on data recorded at child health reviews undertaken by health visiting teams 
      when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to March 2021, so comparisons 
      can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information at Health & Social Care Partnership level is available in the data download 
      function."),
    p("At Scotland level, the data show that there was a small increase in the overall 
      proportion of babies recorded as having been breastfed at both the Heath Visitor 
      first visit, and 6-8 week review in the early months of the pandemic, but this now 
      appears to have fallen back to pre-pandemic levels."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("5 May 2021"),
    p("Information on breastfeeding has been updated in this tool on 5 May 2021. This is 
      based on data recorded at child health reviews undertaken by health visiting teams 
      when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to February 2021, so comparisons 
      can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information at Health & Social Care Partnership level is available in the data 
      download function."),
    p("At Scotland level, the data show that there was a small increase in the overall 
      proportion of babies recorded as having been breastfed at both the Heath Visitor 
      first visit, and 6-8 week review in the early months of the pandemic, but this now 
      appears to have fallen back to pre-pandemic levels."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("7 April 2021"),
    p("Information on breastfeeding has been updated in this tool on 7 April 2021. This is 
      based on data recorded at child health reviews undertaken by health visiting teams 
      when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to January 2021, so comparisons 
      can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information at Health & Social Care Partnership level is available in the data 
      download function."),
    p("At Scotland level, the data show that there was a small increase in the overall 
      proportion of babies recorded as having been breastfed at both the Heath Visitor 
      first visit, and 6-8 week review in the early months of the pandemic, but this now 
      appears to have fallen back to pre-pandemic levels."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("3 March 2021"),
    p("Information on breastfeeding has been updated in this tool on 3rd March 2021. This 
      is based on data recorded at child health reviews undertaken by health visiting 
      teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to December 2020, so comparisons 
      can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information is shown at Health & Social Care Partnership level, but this is only 
      available in the data download function."),
    p("At Scotland level, the data show that the small increase in the overall proportion 
      of babies recorded as having been breastfed has been maintained through to December 2020, 
      for both the Heath Visitor first visit, and 6-8 week review."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("3 February 2021"),
    p("Information on breastfeeding has been updated in this tool on 3rd February 2021. 
      This is based on data recorded at child health reviews undertaken by health visiting 
      teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to November 2021, so comparisons 
      can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information is shown at Health & Social Care Partnership level, but this is only 
      available in the data download function."),
    p("At Scotland level, the data show that the small increase in the overall proportion of 
      babies recorded as having been breastfed has been maintained through to November 2020, 
      for both the Heath Visitor first visit, and 6-8 week review."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("23 December 2020"),
    p("Information on breastfeeding has been updated in this tool on 23rd December 2020. This 
      is based on data recorded at child health reviews undertaken by health visiting teams 
      when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to September 2020, so comparisons 
      can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information is shown at Health & Social Care Partnership level, but this is only 
      available in the data download function."),
    p("At Scotland level, the data show that the small increase in the overall proportion 
      of babies recorded as having been breastfed has been maintained through to September 
      2020, for both the Heath Visitor first visit, and 6-8 week review. "),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("4 November 2020"),
    p("Information on breastfeeding has been updated in this tool on 4th November 2020. 
      This is based on data recorded at child health reviews undertaken by health visiting 
      teams when babies are 10-14 days (Health Visitor [HV] First Visit) and 6-8 weeks old."),
    p("Data is shown by month of review from January 2019 to August 2020, so comparisons 
      can be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("Information is now included for NHS Grampian, as their data recording issues have now 
      been resolved. Information is also shown at Health & Social Care Partnership level, 
      but this is only available in the data download function."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("30 September 2020"),
    p("Information on breastfeeding has been included in this tool for the first time on 
      30 September 2020. This is based on data recorded at child health reviews undertaken 
      by health visiting teams when babies are 10-14 days (Health Visitor [HV] First Visit) 
      and 6-8 weeks old."),
    p("Data is shown on breastfeeding initiation (has the child ever been breastfed), and 
      the child’s breastfeeding status over the 24 hours prior to their child health review 
      (exclusive breastfeeding and overall breastfeeding [includes mixed breast and formula 
      feeding])."),
    p("Data is shown by month of review from January 2019 to July 2020, so comparisons can 
      be made for babies receiving their reviews before and during the COVID-19 pandemic."),
    p("At Scotland level, there was a small increase in the proportion of babies recorded at 
      their HV First Visit as ever having been breastfed, and as still receiving some 
      breastfeeding, in April and May 2020 (babies born March/April/May). For example, 56% 
      of babies having their HV First Visit in April 2020 were recorded as overall breastfed, 
      compared to the pre-pandemic average of 52%.  Similarly, there was a small increase in 
      the proportion of babies recorded at their 6-8 week review as still receiving breastfeeding 
      in May 2020 (babies born March/April). Breastfeeding rates have returned to previous 
      average levels for babies receiving their HV First Visit and 6-8 week review from June 
      2020 onwards."),
    p("The proportion of babies receiving a HV First Visit (and having their review record 
      entered into the CHSP-PS electronic system) is usually very high (>95%) and this has 
      been well maintained during the COVID-19 pandemic. The proportion of babies receiving 
      a 6-8 week review is also usually high (>90% if sufficient follow up time allowed) and 
      this has been reasonably well maintained during the COVID-19 pandemic in most, but not 
      all, NHS Board areas. This can be seen by examining the number of HV First Visits and 
      6-8 week reviews provided month by month on the Breastfeeding page of this tool, and 
      through the more detailed data provided on review coverage on the Child Health Reviews 
      page."),
    p("This means that the trends seen in the proportion of babies recorded as being breastfed 
      are likely to be real, rather than the result of changes in data recording. A temporary 
      increase in breastfeeding rates for babies born during the first wave of the COVID-19 
      pandemic in Scotland may reflect women having more time to initiate and maintain 
      breastfeeding during lockdown due to fewer competing demands on their time, for example 
      through reduced visits from friends and family."),
    br()
  ) #tagList bracket

###############################################.
## Child development ----
###############################################.
childdev_tab <- 
  tabPanel(title = "Child development", value = "child_dev",
           filters_ui(id = "childdev", measure_choices = data_list_childdev,
                      area_choices = c("Scotland", "Health board")), # Filters and options
         mainPanel(width = 12,
                   uiOutput("childdev_explorer")
         )# mainPanel bracket
) # tabpanel bracket

    ###############################################.
    # Child development commentary ----
    ###############################################.
childdev_commentary <-
  tagList(
    fluidRow(
      column(8, h2("Child development")), 
      column(4, div(bsButton("jump_to_childdev", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("6 April 2022"),
    p("Information on child development has been updated on 6 April 2022 to include 
      information on reviews undertaken up to January 2022. This is based on child health 
      reviews undertaken by health visiting teams when children are 13-15 months and 27-30 
      months old. Background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    p("In this release, information is provided for the first time on the percentage of children with developmental concerns by developmental domain (examples of development domains include vision, fine motor skills and personal/social development). This information has been added in response to the rise in the percentage of children with one or more developmental concerns observed in 2021, as noted in the commentary below for November and December 2021. These data are provided at Scotland level only due to small numbers in some domains in individual Health Boards. More information on developmental domains and how they are assessed is provided in the annual,",
      tags$a(href = "https://publichealthscotland.scot/media/6578/2021-04-27-early-child-development-publication-report.pdf", "Early Child Development", target="_blank"), "report produced by PHS."),
    p("In January 2022 the most frequent domain in which there was a concern about development 
      at 13-15 months was gross motor skills (5.6% of children reviewed), and at 27-30 months 
      was speech, language and communication (12.0% of children reviewed). Formal analysis of 
      trends and change is not presented here, however the proportion of children at 13-15 months 
      with a documented concern about speech, language and communication in 2021 appears higher 
      than that observed in 2019 and 2020. Likewise at 27-30 months the proportion of children 
      identified with concerns about development in the speech, language & communication, 
      emotional/behavioural, personal/social, and problem solving domains appears higher in 
      2021 than in the previous two years. "),
    p("PHS will continue to provide monthly monitoring of these data, and the next annual report 
      with detailed analysis by developmental domain and population group for children eligible 
      for review in 2020/21 will be published on 26th April 2022."),
    
    h3("1 December 2021"),
    p("Information on child development has been updated on 1st December 2021 to include 
      information on reviews undertaken up to September 2021. This is based on child health 
      reviews undertaken by health visiting teams when children are 13-15 months and 27-30 
      months old. Background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    p("As reported last month, the percentage of children who are reported to have a concern 
      in at least one developmental domain remains above the pre-pandemic centreline for both 
      reviews. In September 2021 11.8% of children reviewed at 13-15 months of age had a concern 
      documented, compared with a pre-pandemic baseline of 9.6%. At 27-30 months 18.7% of 
      children reviewed had a concern documented, compared with a pre-pandemic baseline of 
      14.6%."),
    p("In this release, information is provided for the first time on the percentage of children 
      with at least one developmental concern, by socioeconomic deprivation (as measured by 
      Scottish Index of Multiple Deprivation (SIMD) of area of residence). ",
      tags$a(href = "https://publichealthscotland.scot/media/6578/2021-04-27-early-child-development-publication-report.pdf", "Annual reporting", target="_blank"), " of data on child development has previously demonstrated that a higher proportion of children living in more deprived areas are identified as having developmental concerns, than those in less deprived areas.
      The data in this release show that, at 27-30 months, an increase in the percentage of 
      children with at least one developmental concern has been observed across all deprivation 
      groups. There remains a steep socioeconomic gradient; in September 2021, 27.4% of children 
      in the most deprived areas had a least one concern, compared with 13.7% in the least 
      deprived areas. At 13-15 months the recent changes among deprivation groups are less 
      clear. This is likely, in part, to be due to the adoption, in May 2019, of this review 
      in NHS Greater Glasgow and Clyde, which contains a substantial proportion of children 
      who live in more deprived areas in Scotland."),
    p("The commentary below includes potential reasons for the recent observed changes; PHS 
      will continue to provide monthly monitoring of these data, and a full annual report 
      with more detailed analysis by developmental domain and population group will be published 
      in April 2022."),
    
    h3("3 November 2021"),
    p("Information on child development has been updated on 3rd November 2021 to include 
      information on reviews undertaken in August 2021. This is based on child health reviews 
      undertaken by health visiting teams when children are 13-15 months and 27-30 months 
      old. Background information on interpreting the data is provided in the commentary for 
      30 September 2020 below."),
    p("This release shows that there has been a recent increase in the percentage of children 
      reviewed who are reported to have a concern in at least one developmental domain. In 
      August 2021 12.6% of children reviewed at 13-15 months of age had a concern documented, 
      compared with a pre-covid baseline of 9.6%. At 27-30 months 19.3% of children reviewed 
      had a concern documented, compared with a pre-covid baseline of 14.6%. Both measures 
      have been consistently above the expected level since February 2021, suggesting that 
      these findings are less likely to be due to chance variation alone."),
    p("A number of factors may influence these data. They may reflect a true change in the 
      proportion of children who are experiencing developmental delay or disorders in this 
      period. Public Health Scotland is working to understand the impact of the COVID-19 
      pandemic on younger children and their families, through the COVID-19 early years 
      resilience and impact survey (",
      tags$a(href ="https://publichealthscotland.scot/our-areas-of-work/covid-19/covid-19-data-and-intelligence/covid-19-and-children-research/covid-19-early-years-resilience-and-impact-survey-ceyris/", "CEYRIS", target="_blank"),
      "). The findings from CEYRIS help to identify areas for action to support the health, wellbeing and development of children. In addition, changes in the data may reflect changes in the way child health reviews are undertaken and reported. It appears that such changes may contribute to the findings in some health board areas. However, the sustained nature and consistency of the finding across several areas, in combination with a quite consistent proportion of reviews having full meaningful information recorded, suggests that there are likely to be common contributing factors."),
    p("PHS will continue to provide monthly monitoring of these data, and a full annual 
      report with more detailed analysis by developmental domain and population group will 
      be published in April 2022."),
    
    h3("1 September 2021"),
    p("Information on child development has been updated on 1 September 2021. This is based 
      on data recorded at child health reviews undertaken by health visiting teams when 
      children are 13-15 months and 27-30 months old. Data is shown by month of review from 
      January 2019 to June 2021. Background information on interpreting the data is provided 
      in the commentary for 30 September 2020 below."),
    p("Please note that going forward the dashboard will continue to be updated on the first 
      Wednesday of each month, but the commentary will only be updated in the case of 
      exceptions."),
    
    h3("4 August 2021"),
    p("Information on child development has been updated on 4 August 2021. This is based 
      on data recorded at child health reviews undertaken by health visiting teams when 
      children are 13-15 months and 27-30 months old. Data is shown by month of review 
      from January 2019 to May 2021. Background information on interpreting the data is 
      provided in the commentary for 30 September 2020 below."),
    
    h3("7 July 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 7 July 2021. This is based on 
      data recorded at child health reviews undertaken by health visiting teams when children 
      are 13-15 months and 27-30 months old. Data is shown on the proportion of children with 
      1 or more developmental concern recorded on their child health review record. Data is 
      also shown on the overall number of reviews provided, and on the number of reviews with 
      full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to April 2021, so comparisons can 
      be made for children receiving their reviews before and during the COVID-19 pandemic. 
      For the 13-15 month review specifically, no data is available for the period January 
      to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered 
      in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only 
      available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development 
      domains was substantially lower in April and May 2020, than the level seen in 2019. 
      Data from more recent months show that this has improved, with full meaningful data 
      recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least 
      one developmental concern documented remain similar to pre-pandemic levels, having 
      been much lower in April 2020. This drop was associated with the reduction in complete 
      data recording, and is likely to reflect changes in ascertainment of developmental 
      concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("2 June 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 2 June 2021. This is based on 
      data recorded at child health reviews undertaken by health visiting teams when children 
      are 13-15 months and 27-30 months old. Data is shown on the proportion of children with 
      1 or more developmental concern recorded on their child health review record. Data is 
      also shown on the overall number of reviews provided, and on the number of reviews with 
      full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to February 2021, so comparisons 
      can be made for children receiving their reviews before and during the COVID-19 pandemic. 
      For the 13-15 month review specifically, no data is available for the period January to 
      April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in 
      NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only 
      available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development 
      domains was substantially lower in April and May 2020, than the level seen in 2019. 
      Data from more recent months show that this has improved, with full meaningful data 
      recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one 
      developmental concern documented remain similar to pre-pandemic levels, having been 
      much lower in April 2020. This drop was associated with the reduction in complete data 
      recording, and is likely to reflect changes in ascertainment of developmental concerns 
      (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("5 May 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 5 May 2021. This is based on data 
      recorded at child health reviews undertaken by health visiting teams when children are 
      13-15 months and 27-30 months old. Data is shown on the proportion of children with 1 
      or more developmental concern recorded on their child health review record. Data is 
      also shown on the overall number of reviews provided, and on the number of reviews with 
      full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to February 2021, so comparisons 
      can be made for children receiving their reviews before and during the COVID-19 pandemic. 
      For the 13-15 month review specifically, no data is available for the period January to 
      April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in 
      NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only 
      available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development 
      domains was substantially lower in April and May 2020, than the level seen in 2019. 
      Data from more recent months show that this has improved, with full meaningful data 
      recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one 
      developmental concern documented remain similar to pre-pandemic levels, having been 
      much lower in April 2020. This drop was associated with the reduction in complete data 
      recording, and is likely to reflect changes in ascertainment of developmental concerns 
      (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("7 April 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 7 April 2021. This is based on 
      data recorded at child health reviews undertaken by health visiting teams when children 
      are 13-15 months and 27-30 months old. Data is shown on the proportion of children with 
      1 or more developmental concern recorded on their child health review record. Data is 
      also shown on the overall number of reviews provided, and on the number of reviews with 
      full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to January 2021, so comparisons 
      can be made for children receiving their reviews before and during the COVID-19 pandemic. 
      For the 13-15 month review specifically, no data is available for the period January to 
      April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in 
      NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only 
      available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development 
      domains was substantially lower in April and May 2020, than the level seen in 2019. 
      Data from more recent months show that this has improved, with full meaningful data 
      recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one 
      developmental concern documented remain similar to the levels observed in 2019, having 
      been much lower in April 2020. This drop was associated with the reduction in complete 
      data recording, and is likely to reflect changes in ascertainment of developmental 
      concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("3 March 2021"),
    h4("What is reported?"),
    p("Information on child development has been updated on 3rd March 2021. This is based 
      on data recorded at child health reviews undertaken by health visiting teams when 
      children are 13-15 months and 27-30 months old. Data is shown on the proportion of 
      children with 1 or more developmental concern recorded on their child health review 
      record. Data is also shown on the overall number of reviews provided, and on the number 
      of reviews with full meaningful data recorded for every development domain."),
    p("Data is shown by month of review from January 2019 to December 2020, so comparisons 
      can be made for children receiving their reviews before and during the COVID-19 pandemic. 
      For the 13-15 month review specifically, no data is available for the period January 
      to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered 
      in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only 
      available through the data download function."),
    h4("Data quality"),
    p("The proportion of reviews which have meaningful data recorded for all development 
      domains was substantially lower in April and May 2020, than the level seen in 2019. 
      Data from more recent months show that this has improved, with full meaningful data 
      recording returning to around 90% for both reviews in autumn 2020."),
    h4("Findings"),
    p("At Scotland level, the data show that the proportion of children having at least one 
      developmental concern documented remain similar to the levels observed in 2019, having 
      been much lower in April 2020. This drop was associated with the reduction in complete 
      data recording, and is likely to reflect changes in ascertainment of developmental 
      concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("3 February 2021"),
    p("Information on child development has been updated on 3rd February 2021. This is based 
      on data recorded at child health reviews undertaken by health visiting teams when 
      children are 13-15 months and 27-30 months old."),
    p("Data is shown on the proportion of children with 1 or more developmental concern 
      recorded on their child health review record. Data is also shown on the overall number 
      of reviews provided, and on the number of reviews with full meaningful data recorded for 
      every development domain."),
    p("Data is shown by month of review from January 2019 to November 2020, so comparisons 
      can be made for children receiving their reviews before and during the COVID-19 pandemic. 
      For the 13-15 month review specifically, no data is available for the period January 
      to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered 
      in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only 
      available through the data download function."),
    p("At Scotland level, the data show that the proportion of children having at least one 
      developmental concern documented has returned towards almost the levels observed in 2019, 
      having been much lower in April 2020. This supports the interpretation that the observed 
      lower levels in April 2020 were most likely to be attributable to changes in ascertainment 
      of developmental concerns (either identification or recording)."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("23 December 2020"),
    p("Information on child development has been updated on 23rd December 2020. This is based 
      on data recorded at child health reviews undertaken by health visiting teams when children 
      are 13-15 months and 27-30 months old."),
    p("Data is shown on the proportion of children with 1 or more developmental concern recorded 
      on their child health review record. Data is also shown on the overall number of reviews 
      provided, and on the number of reviews with full meaningful data recorded for every 
      development domain."),
    p("Data is shown by month of review from January 2019 to September 2020, so comparisons 
      can be made for children receiving their reviews before and during the COVID-19 pandemic. 
      For the 13-15 month review specifically, no data is available for the period January 
      to April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered 
      in NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards. It should 
      also be noted that NHS Greater Glasgow & Clyde have a backlog of 13-15 month and 27-30 
      month reviews to be entered into CHSP due to staffing shortages, so numbers will increase 
      in subsequent updates."),
    p("Data is available at Health & Social Care Partnership (HSCP) level, but this is only 
      available through the data download function."),
    p("At Scotland level, the data show that the proportion of children having at least one 
      developmental concern documented has returned towards the levels observed in 2019, 
      having been much lower in April 2020. This supports the interpretation that the observed 
      lower levels in April 2020 were most likely to be attributable to changes in ascertainment 
      of developmental concerns (either identification or recording). "),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("4 November 2020"),
    p("Information on child development has been updated on 4th November 2020. This is based 
      on data recorded at child health reviews undertaken by health visiting teams when children 
      are 13-15 months and 27-30 months old."),
    p("Data is shown on the proportion of children with 1 or more developmental concern 
      recorded on their child health review record. Data is also shown on the overall number 
      of reviews provided, and on the number of reviews with full meaningful data recorded for 
      every development domain."),
    p("Data is shown by month of review from January 2019 to August 2020, so comparisons can 
      be made for children receiving their reviews before and during the COVID-19 pandemic. 
      For the 13-15 month review specifically, no data is available for the period January to 
      April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in 
      NHS GG&C from May 2019 onwards. This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards. Data 
      for NHS Grampian has now been included as their data entry issues have now been resolved."),
    p("Data has been added at Health & Social Care Partnership (HSCP) level, but this is only 
      available through the data download function."),
    p("Further background information on interpreting the data is provided in the commentary 
      for 30 September 2020 below."),
    
    h3("30 September 2020"),
    p("Information on child development has been included in this tool for the first time on 
      30th September 2020. This is based on data recorded at child health reviews undertaken 
      by health visiting teams when children are 13-15 months and 27-30 months old."),
    p("Data is shown on the proportion of children with 1 or more developmental concern recorded 
      on their child health review record.  Data is also shown on the overall number of reviews 
      provided, and on the number of reviews with full meaningful data recorded for every 
      development domain."),
    p("Data is shown by month of review from January 2019 to July 2020, so comparisons can be 
      made for children receiving their reviews before and during the COVID-19 pandemic. For 
      the 13-15 month review specifically, no data is available for the period January to 
      April 2019 for NHS Greater Glasgow & Clyde, as this review has only been delivered in 
      NHS GG&C from May 2019 onwards.  This means that information for ‘Scotland’ excludes 
      NHS GG&C for January to April 2019, and includes NHS GG&C for May 2019 onwards. ‘Scotland’ 
      also excludes NHS Grampian for the whole time period shown, due to problems with entry 
      of data into the CHSP-PS system in Grampian during the COVID-19 pandemic."),
    p("At Scotland level, the proportion of children recorded at their 13-15 month review 
      as having at least one developmental concern fell in March and April 2020 before returning 
      to pre-pandemic levels by May 2020. A similar, but more pronounced, pattern was seen for 
      the 27-30 month review, with the proportion of children reviewed having at least one 
      developmental concern recorded falling in March and April 2020 before returning to 
      pre-pandemic levels by June 2020. In April 2020, 9% of children undergoing a 27-30 
      month review had a developmental concern identified compared to the pre-pandemic average 
      of 16%."),
    p("The proportion of children receiving their 13-15 month or 27-30 month review (and having 
      their review record entered into the CHSP-PS electronic system) is usually high (around 
      90% if sufficient follow up time allowed).  Delivery of these reviews was inevitably 
      disrupted at the start of the COVID-19 pandemic, as shown by the dip in the number of 
      reviews delivered in March to May 2020.  The number of reviews delivered per month 
      recovered to pre-pandemic levels in June 2020.  Further information on review coverage 
      is provided on the Child Health Reviews page of this tool."),
    p("When the number of 13-15 month and 27-30 month reviews delivered fell at the start of 
      the COVID-19 pandemic, so did the proportion that had full meaningful data recorded 
      against each of the eight developmental domains assessed during reviews. In April 2020, 
      77% of 13-15 month review records and 73% of 27-30 month review records had full 
      meaningful data on child development recorded, compared to previous averages of around 90%. ",
      tags$a(href ="https://www.gov.scot/publications/coronavirus-covid-19-nursing-and-community-health-staff-guidance/", "National guidance (external website)", target="_blank"),
      " issued at the start of the pandemic recommended that the 13-15 month and 27-30 month 
      reviews should be conducted remotely (by phone or video consultation) where feasible. 
      The fall in data completeness relating to child development on review records may 
      therefore reflect difficulties in completing a full developmental assessment without 
      face to face contact."),
    p("Given this, it is likely that the dip in March to May 2020 in the proportion of children 
      undergoing 13-15 month and 27-30 month reviews who were identified as having a developmental 
      concern reflects a fall in the ascertainment of developmental problems, rather than a 
      genuine fall in the proportion of children with developmental delay."),
    p("The combined impact of fewer children having reviews, and a lower proportion of those 
      reviewed having developmental concerns identified, means that across Scotland during the 
      period March to July 2020 around 300 children fewer than would have been expected based 
      on pre-pandemic levels were identified as having a developmental concern at 13-15 months, 
      and 800 children fewer were identified as having a developmental concern at 27-30 months. 
      It is not currently clear to what extent these ‘missing’ children may be identified in 
      coming months, either through ‘catch up’ reviews provided later than usual, or through 
      their parents proactively raising concerns about their development with relevant services 
      such as their Health Visitor, GP, or early learning and childcare staff."),
    br()
    ) #tagList bracket

# END