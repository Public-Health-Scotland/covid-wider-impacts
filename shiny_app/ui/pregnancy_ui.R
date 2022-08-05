# Wider impacts dashboard - Pregnancy tab
# UI code

##############################################.
# Antenatal booking ----
##############################################.
antenatal_tab <- 
  tabPanel(title = "Antenatal booking", value = "booking",
         wellPanel(
           column(4, selectgeo_ui("booking", area_choices =  c("Scotland", "Health board"), step_no = "1")),
           column(4,offset=2,
                  sourcemodal_ui("booking"),
                  fluidRow(br()),
                  downloadButton("download_ante_booking_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_booking","Go to commentary"))
           #actionButton("browser", "Browser")
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("booking_explorer")
         )# mainPanel bracket
) #tab panel

      ##############################################.
      # Antenatal booking commentary ----
      ##############################################.

booking_commentary <- 
  tagList(
    fluidRow(
      column(8, h2("Antenatal booking")), 
      column(4, div(bsButton("jump_to_booking", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("4 May 2022"),
    p("The numbers of women booked for antenatal care in NHS Fife have shown unusual 
      fluctuations in recent weeks. We have informed NHS Fife of this and are working 
      with them to try to understand the data."),
    
    h3("6 April 2022"),
    p("The numbers of women booked for antenatal care and the average gestation of women 
      booked in NHS Forth Valley have shown unusual fluctuations in recent weeks. We have 
      informed NHS Forth Valley of this and are working with them to try to understand the data."),
    
    h3("2 February 2022"),
    p("The sudden drop in numbers of women booking for antenatal care during the weeks 
      starting 27th December 2021 and 3rd January 2022 is thought to be as a result of 
      the Christmas and new year public holidays. A similar decrease can be seen during 
      the previous year’s Christmas and new year period and the extent of the decrease is 
      likely to depend on whether the four public holidays fall across a two or three week 
      period. Most NHS Boards showed some level of reduction in their numbers of women 
      booked over this period."),
    
    h3("3 November 2021"),
    p("The average gestation at booking for NHS Forth Valley has been noted to be above 
      the revised (Feb 2021 onwards) average line for the last 6 data points.  We are 
      linking with NHS Forth Valley to investigate this pattern of increased average 
      gestation at booking further."),
    
    h3("1 September 2021"),
    p("The average gestation is noted to be higher than usual in the latest week presented 
      for NHS Borders. This is likely to be the effect of small numbers of bookings in NHS 
      Borders. A few later bookings can dramatically alter the average gestation at booking 
      for a particular week (e.g. 26 July). Some of these may be in pregnancies that were 
      originally booked elsewhere. There is no evidence of a sustained pattern of increased 
      average gestation in NHS Borders although we will continue to monitor these data. "),
    
    h3("7 July 2021"),
    p("In this release of information on antenatal booking data (7th July 2021) data have 
      been updated to include women booking for antenatal care up to the week beginning 7th 
      June 2021. A new centreline line for average gestation has been included for NHS Forth 
      Valley because a technical change to the way their data are recorded is thought to have 
      resulted in data which more accurately represent the timing of when women book for antenatal 
      care in NHS Forth Valley. The new centreline starts from the week beginning 1st March 2021 
      and will be calculated over the period 1st March - 12th July 2021 after which a projected 
      centreline will be presented on the average gestation chart for NHS Forth Valley."),
    
    h3("2 June 2021"),
    p("In this release of information on antenatal booking data (2nd June 2021) data have 
      been updated to include women booking for antenatal care up to the week beginning 
      3rd May 2021. Since the previous release, which showed data up until the week beginning 
      5th April 2021, numbers of women booking for antenatal care in Scotland have reduced 
      (to 869 in week of 3rd May). This is likely to be as a result of fewer women booking 
      over the May public holiday. This reduction is also reflected in the numbers of bookings 
      by NHS Board with NHS Borders, NHS Lothian and NHS Greater Glasgow & Clyde showing notable 
      decreases for the week beginning 3rd May 2021. NHS Forth Valley have recorded six consecutive 
      data points below their average number of bookings."),
    p("The updated (all-Scotland) data in this release show that the average gestation at which 
      women booked for antenatal care in recent weeks is around the average based on the pre-pandemic 
      period. Recent data on average gestation by NHS Board are more varied. Lower than average 
      gestation at booking has been observed over at least six consecutive data points in the 
      most recent NHS Ayrshire & Arran, NHS Dumfries & Galloway, NHS Highland, NHS Lanarkshire 
      and NHS Lothian data. NHS Forth Valley continues to show an increased average gestation 
      at booking in recent weeks compared to their pre-pandemic average. This is believed to 
      be as a result of a technical change in data recording and we are continuing to work with 
      the Health Board to clarify this."),
    
    h3("5 May 2021"),
    p("In this release of information on antenatal booking data (5th May 2021) data have 
      been updated to include women booking for antenatal care up to the week beginning 
      5th April 2021. Since the previous release, which showed data up until the week beginning 
      8th March 2021, numbers of women booking for antenatal care in Scotland have reduced 
      slightly (to 971 in week of 5 April) but are still at a level which is very similar to 
      the average numbers seen pre-pandemic. Numbers of bookings in different NHS Boards vary.
      NHS Borders, NHS Grampian and NHS Highland are all showing runs of at least six consecutive 
      data points above their average number of bookings per week, that continue into April. 
      NHS Ayrshire & Arran have recorded six consecutive data points below their average number 
      of bookings."),
    p("The updated (all-Scotland) data in this release show that the average gestation at which 
      women booked for antenatal care is at a very similar level to the average based on the 
      pre-pandemic period: at 9.3 weeks. A higher than average gestation at booking for women 
      aged under 20 is evident in six out of the last seven time points (average gestation of 
      11.2 weeks, in week of 5 April). Recent data on average gestation by NHS Board are more 
      varied. Lower than average gestation at booking has been observed over at least six consecutive 
      data points in the most recent NHS Ayrshire & Arran, NHS Lanarkshire and NHS Lothian data. 
      NHS Forth Valley has shown a sharp rise in average gestation at booking in recent weeks. 
      This is believed to be as a result of a technical change in data recording and we are 
      continuing to work with the Health Board to clarify this."),
    
    h3("7 April 2021"),
    p("In this release of information on antenatal booking data (7th April 2021) data have 
      been updated to include women booking for antenatal care up to the week beginning 8th March 2021. 
      Since the previous release, which showed data up until the week beginning 1st February 2021, 
      numbers of women booking for antenatal care in Scotland have returned to a level which 
      is very similar to the average numbers seen pre-pandemic at just over 1,000 women per 
      week. Numbers of bookings in different NHS Boards vary. NHS Fife, NHS Greater Glasgow & Clyde, 
      NHS Highland and NHS Lanarkshire are all showing runs of at least six consecutive data points 
      above their average number of bookings per week, that continue into March. NHS Forth Valley 
      have shown a marked decrease over the last five weeks in their recorded data for the number 
      of women booking for antenatal care. This is thought to be as a result of a data recording 
      issue and does not represent the true number of women booking in NHS Forth Valley. We are 
      working with NHS Forth Valley to rectify this."),
    p("The updated (all-Scotland) data in this release show that the average gestation at which 
      women booked for antenatal care is at a very similar level to the average based on the 
      pre-pandemic period: at 9.3 weeks. Recent data on average gestation by NHS Board are 
      more varied. Lower than average gestation at booking has been observed over at least six 
      consecutive data points in the most recent NHS Ayrshire & Arran, NHS Dumfries & Galloway, 
      NHS Highland, NHS Lanarkshire and NHS Lothian data.  NHS Forth Valley has shown a sharp 
      rise in average gestation at booking in recent weeks, but as noted above, this is thought 
      to be as a result of a data recording issue which we are working to rectify with the Health Board."),
    
    h3("3 March 2021"),
    p("In this release of information on antenatal booking data (3rd March 2021) data have been 
      updated to include women booking for antenatal care up to the week beginning 1st February 2021. 
      Since the previous release, which showed data up until the week beginning 4th January 2021, 
      numbers of women booking for antenatal care in Scotland reached a peak during the week beginning 
      11th January 2021 and have since decreased but still remained high, well above the average 
      numbers seen pre-pandemic.  Much of this increase in numbers is likely to be due to women 
      delaying booking until after the Christmas and new year holidays. A similar increase can be seen
      over the same period last year.  The extent of the Christmas and new year reduction on numbers 
      and subsequent increase in January is more prominent in the larger NHS Boards such as NHS 
      Greater Glasgow & Clyde, NHS Lothian and NHS Lanarkshire."),
    p("The updated (all-Scotland) data in this release show that the average gestation at which 
      women booked for antenatal care remains just below the average based on the pre-pandemic 
      period. The recent data on average gestation by NHS Board are more varied.  Lower than average 
      gestation at booking is observed in recent weeks in NHS Ayrshire & Arran, NHS Dumfries and Galloway, 
      NHS Forth Valley and NHS Lanarkshire."),
    p("A new average line has been included for NHS Tayside because the data sourced from their 
      Badgernet Maternity information system (introduced in August 2020) are thought to more accurately 
      represent the timing of when women book for antenatal care in NHS Tayside than the earlier 
      (pre-August 2020) data sourced from their Protos information system. Further detail 
      on this is included in the commentary dated 3rd February 2021."),
    
    h3("3 February 2021"),
    p("In this third release of information on antenatal booking data (3rd February 2021) data 
      have been updated to include women booking for antenatal care up to the week beginning 4th 
      January 2021. Previous releases of data have shown that from mid-May to end September the 
      number of women booking for antenatal care had been consistently lower than expected based 
      on pre-pandemic average levels. At the end of September numbers started to rise and have 
      been increasing throughout October, November and most of December.  Although this increase 
      may be partly explained by some women planning their pregnancies during these months, having 
      previously delayed their pregnancy during the first Coronavirus lockdown, the increase is 
      also consistent with a seasonal pattern of increasing numbers of bookings that we usually 
      see each year during the Autumn months."),
    p("The sudden drop in numbers of women booking for antenatal care during the weeks starting 
      21st December and 28th December 2020 is thought to be as a result of the Christmas and new 
      year public holidays. A similar decrease can be seen during the previous year’s Christmas 
      and new year period and the extent of the decrease is likely to depend on whether the four 
      public holidays fall across a two or three week period. All NHS Boards showed some level of 
      reduction in their numbers of women booked over this period. "),
    p("The updated (all-Scotland) data in this release (for November and December 2020) show that 
      the average gestation at which women booked for antenatal care continues to be just below the 
      average based on the pre-pandemic period. The recent data on average gestation by NHS Board 
      are more varied, most notably NHS Tayside have shown higher average gestations of women booking 
      compared to their pre-pandemic average since August 2020. This change reflects a number of 
      factors: transition of local care pathways to accommodate changes resulting from the impact of 
      the Covid-19 pandemic; contemporaneous local transition to the Badgernet Maternity information
      system, and reinforced compliance with local care pathways to ensure booking of women between 
      8 to 10 weeks gestation. Data for more recent months for NHS Tayside, which show average gestations 
      of between 8 to 10 weeks, are thought to more accurately represent the timing of when women 
      book for antenatal care in NHS Tayside than the earlier (pre-August 2020) data sourced from 
      the Protos information system."),
    
    h3("2 December 2020"),
    p("In this second release of information on antenatal booking data (2 Dec 2020) data have 
      been updated to include women booking for antenatal care up to the week beginning 26th 
      October 2020.  The initial release of data on 28th October 2020 showed that from mid-May 
      to end September the number of women booking for antenatal care had been consistently 
      lower than expected based on previous average levels. During October numbers have increased 
      and the most recent data show numbers are at a similar level to the pre-pandemic period. 
      The average gestation at which women booked for antenatal care fell slightly from the 
      end of March 2020, before increasing back to previous levels around August 2020. 
      The most recent data show that the average gestation at booking during September and 
      October continues to be just below the average based on the pre-pandemic period. 
      Looking at the data for women living in different NHS Board areas across Scotland,
      the pattern of a temporary dip in gestation at booking coinciding with the first wave 
      of the COVID-19 pandemic in Scotland is evident in some but not all areas. This probably 
      reflects the fact that the detail of how maternity services were reconfigured in response 
      to COVID-19 varied across Scotland. From August 2020 onwards, the recorded gestation at 
      booking has remained higher than usual for women living in NHS Tayside. Public Health 
      Scotland is working with NHS Tayside to explore this issue."),
    
    h3("28 October 2020"),
    p("Information on the number of women booking for antenatal care, and the average gestation 
      (stage of pregnancy) at which they booked, was included in this tool for the first time on 
      28 October 2020."),
    p("The ",
      tags$a(href = "https://www.nhsinform.scot/ready-steady-baby/pregnancy/your-antenatal-care/your-booking-appointment-booking-visit", 
             "‘booking’ appointment (external website)", class="externallink",target="_blank"),
      " is the first main appointment a woman has with her local maternity service once she 
      knows she is pregnant. At the booking appointment, women are assessed by a midwife who 
      can then tailor the subsequent care they receive during their pregnancy to their particular 
      preferences and needs.  Women are encouraged to book before they are 13 weeks pregnant, 
      and ideally before they are 10 weeks pregnant."),
    p("As an essential service, maternity care including ‘booking’ has been provided throughout 
      the COVID-19 pandemic, and ",
      tags$a(href = "https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies", 
             "women have been encouraged to attend all their scheduled antenatal appointments (external website)", class="externallink"),
      ".  However, ",
      tags$a(href = "https://www.rcog.org.uk/globalassets/documents/guidelines/2020-07-10-guidance-for-antenatal-and-postnatal.pdf", 
             "how some elements of maternity care are delivered has changed (external website)", class="externallink",target="_blank"),
      ", to minimise the number of visits women need to make to clinics and hospitals."),
    p("In general, prior to COVID-19, women were offered an initial in-person booking appointment 
      (including various face to face tests such as blood tests and blood pressure monitoring) 
      then a follow up appointment for their early pregnancy ultrasound scan. Since March 2020, 
      in many areas women have been offered an initial remote consultation, for example using the 
      Near Me video consultation system, then an in-person ‘one stop’ follow up appointment for all 
      their face to face tests and their scan."),
    p("At the start of the COVID-19 pandemic, Public Health Scotland worked with NHS Boards to set 
      up a new national data return providing information on women booking for antenatal care (see 
      the Data source button on the dashboard page). This provides the information required to monitor 
      in a timely way both the direct impact of COVID-19 on pregnant women, and the wider impacts of 
      changes to maternity services and how women interact with services.  The data return is based 
      on an extract of data recorded by midwives in local clinical information systems.  The information 
      relates to the first main appointment a woman has with her maternity service: as noted above, 
      during COVID-19 this will have changed from an in-person to a remote consultation in many areas."),
    p("The data shows that, at all Scotland level, the number of women booking for antenatal care week 
      by week remained broadly constant from April 2019 (when the data starts) to the end of 2019. 
      As would be expected there was then a dip reflecting the Christmas holidays, with higher numbers 
      of women booking just before and just after the holidays. The number of women booking then returned 
      to previous levels until mid-May 2020.  From mid-May to end September (the latest point for which 
      data is currently available), the number of women booking has been consistently lower than expected 
      based on previous average levels.  Over the 19 weeks from week beginning 18 May 2020 to week 
      beginning 21 September 2020, around 1,400 fewer women than would have been expected based on 
      pre-pandemic levels have booked for antenatal care in Scotland.  As women most commonly book at 
      around 9 weeks gestation, women booking from mid-May onwards will broadly reflect women getting 
      pregnant from late March 2020 onwards, i.e. the point at which the initial UK wide lockdown was 
      implemented in response to COVID-19."),
    p("Looking at the data for women living in different NHS Board areas across Scotland, the pattern 
      of a recent fall in the number of women booking for antenatal care is evident in some but not all areas."),
    p("Fewer women booking for antenatal care could reflect fewer women who become pregnant choosing 
      to continue with their pregnancy and/or fewer women becoming pregnant. It is therefore helpful 
      to consider the data on antenatal booking alongside the data on terminations of pregnancy provided 
      through this tool (see the Commentary on Terminations of pregnancy for more information). Considering 
      both sets of data, it seems likely that both reasons apply. It is likely that the higher than usual 
      number of terminations of pregnancy provided in March and April 2020 at least partially contributed 
      to the initial fall in the number of women booking for antenatal care from mid-May. Conversely, 
      the subsequent sustained reduction seen in both the number of terminations and the number of women 
      booking for antenatal care is likely to reflect a reduction in the number of women becoming pregnant 
      from April 2020 onwards. Further analysis is required to accurately examine trends in the number 
      of women becoming pregnant during the COVID-19 pandemic, their subsequent choices to continue with 
      or terminate their pregnancy, and what this means for future trends in the number of births in Scotland."),
    p("At all Scotland level, prior to COVID-19, the average gestation at which women booked for 
      antenatal care was around 9 and a half weeks of pregnancy.  This fell slightly from the end 
      of March 2020, reaching around 8 and a half weeks by end June 2020 before increasing back 
      to previous levels from August 2020 onwards. This temporary reduction in the average gestation 
      at booking means that the recent fall seen in the number of women booking is unlikely to be 
      due to women deferring, or being unable to access, booking until later in their pregnancy. 
      This further confirms that it is likely that the number of women becoming pregnant has been 
      lower than usual from April 2020 onwards."),
    p("Looking at the data for women living in different NHS Board areas across Scotland, the 
      pattern of a temporary dip in gestation at booking coinciding with the first wave of the 
      COVID-19 pandemic in Scotland is evident in some but not all areas.  This probably reflects 
      the fact that the detail of how maternity services were reconfigured in response to COVID-19 
      varied across Scotland.  From August 2020 onwards, the recorded gestation at booking has been 
      higher than usual for women living in NHS Tayside.  This is due to a temporary data recording 
      issue following implementation of a new clinical information system in NHS Tayside at that time. 
      Public Health Scotland is working with NHS Tayside to resolve this and we expect that future 
      releases of the antenatal booking data through this tool will see the average gestation return 
      to a level which is more typical for NHS Tayside."),
    p("At all Scotland level, the recent reduction in the number of women booking for antenatal 
      care has been more evident in younger (compared to older) women, and in women living in 
      more (compared to less) deprived areas. In general, there is no substantial variation 
      in average gestation at booking by maternal age group or deprivation level. The temporary 
      dip in average gestation at booking associated with the first wave of COVID-19 in Scotland 
      has been seen in women from all age groups and from all deprivation levels."),
    br()
  )



###############################################.
## Termination of pregnancy  ----
###############################################.
terminations_tab <- 
  tabPanel(title = "Termination of pregnancy", value = "terminations",
         wellPanel(
           column(4, div(title="Select a breakdown",
                         p(tags$b("Step 1. Select a geography level and then an area of interest.")),
                         selectInput("geotype_top", label = NULL, choices= c("Scotland", "Health board"),
                                     selected = "Scotland")),
                  uiOutput("geoname_ui_top")),
           column(4,offset=4,
                  actionButton("btn_top_modal", "Data source: Notifications of Abortion", icon = icon('question-circle')),
                  fluidRow(br()),
                  downloadButton("download_termination_data", "Download data"),
                  fluidRow(br()),
                  actionButton("jump_commentary_top","Go to commentary"))
         ), #well panel
         mainPanel(width = 12,
                   uiOutput("top_explorer")
         )# mainPanel bracket
) # tabPanel bracket


      ###############################################.
      # Termination of pregnancy commentary ----
      ###############################################.

top_commentary <-
  tagList(
    fluidRow(
      column(8, h2("Termination of pregnancy")), 
      column(4, div(bsButton("jump_to_top", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("2 June 2021"),
    p("This latest release reports on the number of terminations of pregnancy in Scotland 
      up to February 2021. In this latest month 981 terminations were provisionally notified 
      and numbers may be updated in subsequent releases. This is the lowest monthly figure 
      reported since January 2018: Previous years have reported 1,093 in February 2018; 
      1,174 in February 2019; 1,230 in February 2020. In February 2021 two mainland boards 
      notified their lowest number of terminations since January 2018: Grampian (82) and 
      Lothian (176). Average gestation at termination in Scotland for February 2021 (6.9 weeks) 
      remained similar to that reported in recent months and ranged between 6 weeks in Grampian 
      to 8.3 weeks in Fife."),
    p("The shift of average gestations below the centreline (average gestation between January 
      2018 to February 2020) started in March 2020. To a greater or lesser extent this shift has 
      been mirrored across all the mainland boards except in Ayrshire and Arran, Borders and Fife. 
      Also of note, the average gestation in Highland has been trending upwards (but remains below 
      board average), and the lowest average gestations since January 2018 were reported in this 
      release in Ayrshire and Arran (6.7 weeks), Dumfries and Galloway (6.5 weeks) and Grampian 
      (6.0 weeks)."),
    p("There continued to be little variation in average gestation by either age group or deprivation 
      category in February 2021. The drop in numbers (referred to above) in February 2021 was 
      reflected across all age groups and deprivation categories. The range in average gestation 
      by age group was 6.8 weeks (25 to 29 and 40 and over) to 7.2 weeks (35 to 39). For deprivation 
      category the range was from 6.8 week (SIMD 3 and 4) to 7.1 weeks (SIMD 1 - most deprived)."),
    
    h3("5 May 2021"),
    p("This latest release reports on the number of terminations of pregnancy in Scotland up to 
      January 2021. In this latest month 1,108 terminations were provisionally notified, which is 
      comparable with the January average for 2018, 2019 and 2020 of 1,191."),
    p("Observing these figures are provisional and may be updated in subsequent reports, we note 
      that Dumfries and Galloway’s terminations for January 2021 are not shown this month as their 
      total numbers fell below the threshold that we can safely report numbers of terminations without 
      potentially compromising patient confidentiality. Ayrshire and Arran notified their lowest number 
      of terminations (42) since January 2018. We noted in the last release a downward trend in Tayside, 
      which reversed in January 2021."),
    p("Average gestation at termination in Scotland for January 2021 (6.8 weeks) remained similar to 
      that reported in recent months. It is the eleventh consecutive month where the average gestation
      was below the Scotland average gestation (to end February 2020). The average gestation ranged 
      from 6.1 weeks in Grampian (the lowest notified by Grampian since January 2018) to 8 weeks in 
      Highland."),
    p("As in previous releases, we see little variation in average gestation by deprivation (most 
      deprived - 7 weeks and least deprived - 6.6 weeks). The average gestation was slightly higher 
      in the under 20 age group (7.3 weeks) and the 40 and over age group (7.1 weeks) compared with 
      6.6 weeks in the 25 to 29 age group."),
    
    h3("7 April 2021"),
    p("This latest release reports on the number of terminations of pregnancy in Scotland up to 
      December 2020. In this latest month 1,053 terminations were provisionally notified, which is 
      comparable with the same months in 2019 and 2018 (1,076 and 1,092 respectively)."),
    p("We reported in last month’s release that terminations in Lothian had been steadily rising, 
      reaching a 7-month high in November 2020 (248 terminations). This trend reversed in December 
      2020 with notifications dropping to 193. This is close to the number of terminations notified 
      in December 2019 (192). We also reported that Forth Valley recorded their lowest number of 
      terminations (44) since January 2018. This increased to 76 in December 2020 and was the 
      highest number of terminations recorded by Forth Valley since October 2019. We note a downward 
      trend in numbers of terminations reported by Tayside from 107 in August 2020 to 79 in December 
      2020."),
    p("Average gestation at termination in Scotland for December (6.7 weeks) remained similar to that 
      reported in recent months. The average gestational range in mainland Boards in December was 
      between 6.3 weeks (Lothian and Greater Glasgow & Clyde) and 7.9 weeks (Highland). All areas 
      remained under their Board averages except Borders. This is probably a reflection of the small 
      numbers of terminations carried out by this Board. "),
    p("Latest data also continues to show little variation in average gestation by age or deprivation."),
    
    h3("3 March 2021"),
    p("This latest release reports on the number of terminations of pregnancy in Scotland up to November 
      2020. In this latest month 1,060 terminations were provisionally notified and although this is 
      below the Scotland average, it is comparable with the number of terminations reported in November 
      2019 (1,068) and November 2018 (1,111)."),
    p("There was some variation in numbers reported across the Boards. Of note, Lothian reported a seven 
      month high in November (245 terminations), above the Board average of 218. For the first time 
      since March 2020, Ayrshire and Arran also reported terminations above the board average, the 
      third consecutive monthly increase in the number of terminations. In contrast, Forth Valley 
      recorded their lowest number of terminations since January 2018.  Dumfries and Galloway’s 
      terminations for November 2020 are not shown this month as their total numbers fell below the 
      threshold that we can safely report numbers of terminations without potentially compromising 
      patient confidentiality."),
    p("Average gestation at termination in Scotland for November (6.8 weeks) remained similar to that 
      reported in recent months. The pattern varied by Board, but all areas remained under their Board 
      averages except Borders, their variation is probably related to the small numbers of terminations 
      carried out by this Board. The combination of telemedicine (a remote consultation by telephone 
      or video call) and early medical abortion at home (where both drugs are taken at home) continued 
      to affect gestation at termination."),
    p("The numbers of terminations between age groups and between the most and least deprived areas 
      continues to show little clear variation this has remained the case since July 2020. There is 
      even less variation between age groups and between the most and least deprived areas in 
      respect of the average gestation at termination [range 6.7 to 6.9 weeks]."),
    
    h3("3 February 2021"),
    p("In this latest release of information on terminations of pregnancy up to September 2020, 
      the provisional numbers reported in Scotland showed numbers of terminations rising gradually 
      over the last four months. With numbers of terminations at 1072 in September 2020, since May 
      2020 the number of terminations in Scotland has remained below the Scotland average from 
      January 2018. The majority of Boards recorded an increase in terminations between August 
      and September 2020."),
    p("Average gestation at termination in Scotland decreased in the period February to August 2020 
      from 8 weeks to 6.6 weeks. This probably reflects changes in the configuration of termination 
      care services in response to COVID-19 across Scotland. For Scotland as a whole, average 
      gestation of terminations in Scotland has remained below 7 weeks since May 2020.  This is 
      most clearly seen in Health Board returns from NHS Greater Glasgow and Clyde and NHS Tayside. 
      Overall, the variation in gestation at termination between Board areas in September was minor 
      (range 7.7 to 6.4 weeks)."),
    p("The numbers of terminations between age groups and between most and least deprived areas in 
      Scotland have showed little clear variation since July 2020."),
    p("In Scotland in September 2020, there was a slight widening in the gap of average gestation 
      at time of termination between the most and least deprived areas. In the most deprived areas 
      the average gestation was 7 weeks compared to 6.3 weeks in the least deprived areas."),
    p("There was little variation across the six age groups at time of termination (range 6.9 to 6.6 weeks)."),
    
    h3("2 December 2020"),
    p("In this second release of information on terminations of pregnancy, the provisional numbers 
      of terminations reported in Scotland in August 2020 fell to the lowest level reported since 
      January 2018. This continues the trend of a fall in numbers from May 2020 onwards and is consistent 
      across some but not all Board areas. The decrease in the average gestation at termination (6.6 weeks 
      in the last month) has also continued in Scotland as a whole and any variation between Boards 
      probably reflects minor variation in service provision between Boards."),
    p("The impact of reductions in numbers of terminations in younger women continues to be evident. 
      The observation of an increase in the average gestation reported in women aged over 40 (to 7.5 
      weeks in August) may simply represent the impact of variation within small numbers of terminations 
      in this age group. Overall, there remains no substantial variation in average gestation at 
      termination by maternal age group or deprivation level."),
    
    h3("28 October 2020"),
    p("Information on the number of terminations of pregnancy carried out in Scotland, and the 
      average gestation (stage of pregnancy) at which they occurred, was included in this tool 
      for the first time on 28 October 2020."),
    p("Termination of pregnancy (also referred to as a therapeutic or induced abortion) is provided 
      under the terms of the Abortion Act 1967 and subsequent regulations.  When a healthcare 
      practitioner provides a termination of pregnancy, there is a legal requirement for them to notify 
      the Chief Medical Officer of the termination within seven days of it taking place. Public Health 
      Scotland is responsible for the collation of data derived from notifications of terminations of 
      pregnancy on behalf of the Chief Medical Officer.  This notification data has been used in this 
      tool (see Data source button on the dashboard page).  Detailed information on terminations is 
      published each year by Public Health Scotland.  The ",
      tags$a(href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics", 
             "most recent report", class="externallink",target="_blank"),
      " covers the year to December 2019."),
    p("As an essential service, ",
      tags$a(href = "https://www.rcog.org.uk/en/guidelines-research-services/guidelines/coronavirus-abortion/", 
             "care relating to termination of pregnancy has been provided throughout the COVID-19 pandemic (external website)", 
             class="externallink", target="_blank"),
      ".   Termination of pregnancy can be carried out as a medical procedure or, less commonly, 
      a surgical procedure.  Medical terminations involve the woman taking two different medicines 
      24-48 hours apart to end her pregnancy. Prior to Oct 2017, women having a medical termination 
      were required to attend a clinic or hospital on two occasions to take the first and then, separately, 
      the second medicine. From ",
      tags$a(href = "https://www.sehd.scot.nhs.uk/cmo/CMO(2017)14.pdf", "October 2017 (external website)", 
             class="externallink",target="_blank"),
      ", women requiring an early medical termination (at up to 9 weeks and 6 days gestation) were able to 
      take the second medicine away with them at the end of their first appointment, and subsequently take 
      that at home.  From ",
      tags$a(href = "https://www.sehd.scot.nhs.uk/cmo/CMO(2020)09.pdf", "31 March 2020 (external website)", 
             class="externallink", target="_blank"),
      ", in response to the COVID-19 pandemic, women requiring an early medical termination (at up to 11 
      weeks and 6 days gestation) have been able to have an initial remote consultation, by telephone or 
      video call, then take both medicines at home."),
    p("Since 31 March 2020, there has been variation between NHS Boards across Scotland in exactly how 
      care relating to termination of pregnancy has been provided.  Almost all Boards have provided 
      some remote consultations.  After their initial consultation, some women have been required to 
      attend for an ultrasound scan (for example to confirm how far along their pregnancy is if there 
      is some doubt about that, or to see if they have an ectopic pregnancy) before medicines are provided. 
      Once a woman has been confirmed as eligible for an early medical termination, in some areas both 
      sets of medicine have been delivered to the woman’s home, whereas in other areas women have been 
      required to pick up their medicine from a clinic reception.  On 30 September 2020, the Scottish 
      Government issued a ",
      tags$a(href = "http://www.gov.scot/publications/consultation-future-arrangements-early-medical-abortion-home/", 
             "consultation (external website)", class="externallink", target="_blank"),
      " on whether the recent changes extending women’s access to early medical termination at home 
      should be retained after the COVID-19 pandemic. The consultation will be open until 5 
      January 2021."),
    p("The data shows that, at all Scotland level, the number of terminations of pregnancy 
      provided month by month remained broadly constant from January 2018 (when the data 
      shown starts) to February 2020 inclusive. The number of terminations was then higher 
      than usual in March and April 2020, before falling to lower than usual levels in May, 
      June, and July 2020 (with July 2020 being the latest month for which data are currently 
      available). Over March and April 2020, around 500 more terminations than would have been 
      expected based on pre-pandemic average levels were provided in Scotland. This is likely 
      to reflect a higher proportion than usual of women who found they were pregnant at the 
      start of the COVID-19 pandemic in Scotland choosing not to continue with their pregnancy. 
      As discussed in the commentary on the Antenatal booking data provided through this tool, 
      it is likely that the lower than usual numbers of termination provided from May 2020 onwards 
      reflects a reduction in the number of women becoming pregnant from April 2020 onwards. Further 
      analysis is required to accurately examine trends in the number of women becoming pregnant during 
      the COVID-19 pandemic, their subsequent choices to continue with or terminate their pregnancy, 
      and what this means for future trends in the number of births in Scotland."),
    p("Looking at the data for women living in different NHS Board areas across Scotland, the 
      pattern of an increase in the number of terminations of pregnancy in March and April 2020, 
      then a subsequent fall from May 2020 onwards is evident in some but not all areas."),
    p("At all Scotland level, prior to COVID-19, the average gestation at which terminations of 
      pregnancy took place was around 7 and a half weeks of pregnancy. This fell slightly to 
      around 7 weeks in April 2020, then fell further to around 6 and a half weeks in May to 
      July 2020.  This decrease in the average gestation at termination means that the recent 
      reduction seen in the number of terminations is unlikely to be due to women deferring, 
      or being unable to access, termination until later in their pregnancy."),
    p("Looking at the data for women living in different NHS Board areas across Scotland, the 
      pattern of a reduction in average gestation at termination from April 2020 onwards is 
      evident in some but not all areas. This probably reflects the fact that the detail of 
      how termination care was reconfigured in response to COVID-19 varied across Scotland."),
    p("At all Scotland level, the recent reduction in the number of terminations of pregnancy 
      has been more evident in younger (compared to older) women. The reduction has been seen 
      in women living in areas with all levels of deprivation. In general, there is no substantial 
      variation in average gestation at termination by maternal age group or deprivation level. 
      The reduction in average gestation at termination from April 2020 onwards has been seen 
      in women from all age groups and from all deprivation levels."),
    br()
  )

#END
