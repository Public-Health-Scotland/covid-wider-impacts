# Wider impacts dashboard - Births and babies tab
# UI code

##############################################.
# Inductions ----
##############################################.
inductions_tab <- 
  tabPanel(title = "Induction of labour", value = "inductions",
           wellPanel(
             column(4, selectgeo_ui("induct", area_choices =  c("Scotland", "Health board"), step_no = "1")),
             column(4,offset=2,
                    sourcemodal_ui("induct"),
                    fluidRow(br()),
                    downloadButton("download_induct_data", "Download data"),
                    fluidRow(br()),
                    actionButton("induct-commentary","Go to commentary"))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("induct_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket


      ###############################################.
      # Inductions commentary ----
      ###############################################.

induction_commentary <-
  tagList(
    fluidRow(
      column(8, h2("Induction of labour")), 
      column(4, div(bsButton("jump_to_induction", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("7 September 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley in May 2022, so the proportion 
      of births that are induced in this month is likely to change in future releases of 
      the dashboard."),    
    
    h3("3 August 2022"),
    p("Data are thought to be incomplete for NHS Fife in April 2022, so the proportion of births 
      that are induced in this month is likely to change in future releases of the dashboard. 
      Data submissions from NHS Forth Valley were insufficient to report for April 2022. These 
      will be updated in future dashboard releases."),
    
    h3("6 July 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley in March 2022 and for NHS Fife 
      in February 2022, so the proportion of births that are induced in these months is likely 
      to change in future releases of the dashboard."),
    
    h3("1 June 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley and for NHS Fife in February 2022, 
      so the proportion of births that are induced in February 2022 is likely to change in future 
      releases of the dashboard."),
    
    h3("4 May 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 and January 2022 
      so the proportion of births that are induced for these months is likely to change in future 
      releases of the dashboard."),
    
    h3("1 December 2021"),
    p("The percentage in NHS Greater Glasgow & Clyde of singleton live births at 37-42 weeks 
      gestation that followed induction of labour has been consistently higher than the pre-pandemic 
      average since October 2020, and reached 44% in August 2021. When undertaken for appropriate 
      reasons, and by appropriate methods, induction is useful and benefits both mothers and newborn. 
      We will continue to monitor and are working with NHS Greater Glasgow & Clyde to explore further. "),
    
    h3("1 September 2021"),
    p("The data at all Scotland level show that the proportion induced (the percentage of singleton 
      live births at 37-42 weeks gestation that followed induction of labour) has continued to show 
      little change across the period presented (January 2018 to May 2021). However, some NHS Boards 
      have shown recent increases in the proportion of women induced with NHS Ayrshire & Arran, NHS 
      Dumfries & Galloway, NHS Fife and NHS Greater Glasgow & Clyde all now reporting 40% of women 
      induced or higher."),
    
    h3("2 June 2021"),
    p("In this release of information on induction of labour (2nd June 2021) data have been updated 
      to include women discharged after delivery up to and including February 2021. The data at all 
      Scotland level show that the proportion induced (the percentage of singleton live births at 
      37-42 weeks gestation that followed induction of labour) has continued to show little change 
      across the period presented (January 2018 to February 2021). The data by NHS Board of residence 
      are more varied. In February 2021, NHS Borders, NHS Dumfries & Galloway and NHS Forth Valley 
      have continued the pattern of at least 6 consecutive months showing a lower proportion of 
      inductions compared to their pre-pandemic average. NHS Ayrshire & Arran have recorded an 11th 
      consecutive month showing a higher proportion of inductions compared to their pre-pandemic 
      average. However there has been a sequential month-on-month decrease in the proportion of 
      inductions in the last 3 consecutive months in NHS Ayrshire and Arran."),
    
    h3("5 May 2021"),
    p("In this release of information on induction of labour (5th May 2021) data have been updated 
      to include women discharged after delivery up to and including January 2021. The data at all 
      Scotland level show that the proportion induced (the percentage of singleton live births at 
      37-42 weeks gestation that followed induction of labour) has continued to show little change 
      across the period presented (January 2018 to January 2021). The data by NHS Board of residence 
      are more varied. In January 2021, NHS Dumfries & Galloway and NHS Forth Valley have continued 
      the pattern of at least 6 consecutive months showing a lower proportion of inductions compared 
      to their pre-pandemic average (although both also showed periods of lower proportions of inductions 
      in 2019). NHS Ayrshire & Arran have recorded a 10th consecutive month showing a higher proportion 
      of inductions compared to their pre-pandemic average."),
    
    h3("7 April 2021"),
    p("In this third release of information on induction of labour (7th April 2021) data have been 
      updated to include women discharged after delivery up to and including December 2020. The data 
      at all Scotland level show that the proportion induced (the percentage of singleton live births 
      at 37-42 weeks gestation that followed induction of labour) has continued to show little change 
      across the period presented (January 2018 to December 2020). Data show that the percentage of 
      women aged under 20 who were induced in December 2020 was recorded at its highest level (55%) 
      during the last 3 years. The data by NHS Board of residence are more varied. In December 2020, 
      NHS Dumfries & Galloway and NHS Forth Valley have continued the pattern of at least 6 consecutive 
      months showing a lower proportion of inductions compared to their pre-pandemic average (although 
      both also showed periods of lower proportions of inductions in 2019). NHS Ayrshire & Arran have 
      recorded a 9th consecutive month showing a higher proportion of inductions compared to their 
      pre-pandemic average."),
    
    h3("3 March 2021"),
    p("In this third release of information on induction of labour (3rd March 2021) data have been 
      updated to include women discharged after delivery up to and including November 2020. The 
      data at all Scotland level show that the proportion induced (the percentage of singleton live 
      births at 37-42 weeks gestation that followed induction of labour) has continued to show little 
      change across the period presented (January 2018 to November 2020). The data by NHS Board of 
      residence are more varied. In November 2020, NHS Dumfries & Galloway and NHS Forth Valley have 
      continued the pattern of a number of consecutive months showing a lower proportion of inductions 
      compared to their pre-pandemic average (although both showed periods of lower proportions of 
      inductions in 2019). After a period where the proportion of births following induction was below 
      the pre-pandemic average, the proportion in NHS Highland is noted to have increased in November 
      2020 to a level higher than the Health Board average (42%). NHS Ayrshire & Arran and NHS Fife 
      have continued the pattern of a number of consecutive months showing a higher proportion of 
      inductions compared to their pre-pandemic average. However, data are thought to be incomplete 
      for NHS Fife for November 2020 so this proportion could change in future releases of the dashboard."),
    
    h3("3 February 2021"),
    p("In this second release of information on induction of labour (3rd February 2021) data have been 
      updated to include women discharged after delivery up to and including October 2020. The data 
      at all Scotland level show that the proportion induced (the percentage of singleton live births 
      at 37-42 weeks gestation that followed induction of labour) has continued to show little change 
      across the period presented (January 2018 to October 2020). The data by NHS Board of residence 
      are more varied. NHS Dumfries & Galloway, NHS Forth Valley and NHS Highland have shown lower 
      proportions of inductions in recent months than their pre-pandemic average (although both 
      NHS Dumfries & Galloway and NHS Forth Valley  also showed periods of lower proportions of 
      inductions in 2019). NHS Lanarkshire showed a period of lower inductions in January to July 
      2020 but this has returned close to the long-term pre-pandemic average. NHS Ayrshire & Arran 
      have shown slightly higher proportions since April 2020. Data for NHS Fife also show higher 
      proportions of induction than their pre-pandemic average over recent months, particularly in 
      October 2020. However, data are thought to be incomplete for NHS Fife for October 2020 so this 
      proportion is likely to change in future releases of the dashboard."),
    
    h3("16 December 2020"),
    p("Information on induction of labour was included in this tool for the first time on 16 December 2020."),
    p("'",
      tags$a(href="https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/getting-ready-for-the-birth/induced-labour", 
             "Induction of labour (external website)",class="externallink",target="_blank"),
      "' is when a woman is given a medical intervention to start her labour, rather than waiting for 
      labour to start spontaneously.  It is offered because there are medical reasons meaning it is 
      considered safer (for the mother or baby) for the baby to be born, or because a woman is past 
      her ‘due date’.  There are different approaches to inducing labour, for example using medicines, 
      a medical ‘balloon’ device that sits at the neck of the womb, and/or breaking the woman’s waters."),
    p("Care for women around the time they are giving birth is an essential, time critical service that 
      cannot be deferred.  As such, it has been provided throughout the COVID-19 pandemic, and maternity 
      staff have not been redeployed to support other services.  The way that some elements of this care 
      are provided has changed in response to COVID-19 however, to minimise the risk of infection and to 
      allow services to continue to provide safe care during times when a high number of staff may be off 
      work, for example due to needing to isolate."),
    p("Guidance issued by the ",
      tags$a(href="https://www.gov.scot/collections/coronavirus-covid-19-guidance/#health,careandsocialwork", 
             "Scottish Government (external website)",class="externallink",target="_blank"),
      " and ",
      tags$a(href="https://www.rcog.org.uk/coronavirus-pregnancy", "Royal College of Obstetricians and Gynaecologists (external website)",
             class="externallink",target="_blank"),
      "to maternity services at the height of the first wave of the pandemic noted that:"),
    tags$ul(
      tags$li("It may be necessary for services to temporarily suspend the option for women to deliver at 
              home or in midwife led units, and to concentrate delivery care within obstetric units"),
      tags$li("Additional restrictions on the use of water births were recommended"),
      tags$li("Care pathways for women requiring induction of labour should be amended to ensure the 
              early stages of the induction process were delivered on an outpatient basis wherever possible"),
      tags$li("Services should consider deferring a planned induction of labour or elective caesarean section 
              if a woman was isolating due to having COVID-19, or having been in contact with a case, if it 
              was safe to do so"),
      tags$li("Services should support low risk women in the early latent phase of labour to remain at home 
              wherever possible"),
      tags$li("In general, strict restrictions on visitors for patients in hospital were advised, however 
              women giving birth could still be accompanied by their chosen birth partner")
    ),
    p("The information on induction of labour presented through this tool is taken from hospital discharge 
      records, specifically records relating to the care of women delivering a singleton live birth (i.e. 
      one baby, not twins or more) at 37-42 weeks gestation (i.e. up to 3 weeks before or after their due date). 
      Further technical information is available through the ‘Data source’ button on the dashboard page."),
    p("The data shows that, at all Scotland level, the percentage of singleton live births at 37-42 weeks 
      gestation that followed induction of labour (the ‘induction rate’) has remained broadly constant 
      (at around 34%) from January 2018 (when the data shown starts) to end September 2020 (the latest 
      point for which data is currently available).  Prior to the COVID-19 pandemic, the induction rate 
      was somewhat variable between NHS Board areas of residence.  There is also variation between areas 
      in how the induction rate has changed around the time of the pandemic, with some areas (for example 
      NHS Ayrshire & Arran and NHS Greater Glasgow & Clyde) showing a small increase and other areas (for 
      example NHS Dumfries & Galloway, NHS Forth Valley, and NHS Lanarkshire) showing a small decrease."),
    p("The induction rate tends to be highest among mothers in the youngest (<20 years) and oldest (40+ 
      years) age groups, and among mothers living in the most deprived areas of Scotland.  These patterns 
      have persisted during the COVID-19 pandemic."),
    br()
  )



##############################################.
# Method of delivery ----
##############################################.
mode_delivery_tab <- 
  tabPanel(title = "Method of delivery", value = "mod",
           wellPanel(
             column(4, selectgeo_ui("mod", area_choices =  c("Scotland", "Health board"), step_no = "1")),
             column(4,offset=2,
                    sourcemodal_ui("mod"),
                    fluidRow(br()),
                    downloadButton("download_mod_data", "Download data"),
                    fluidRow(br()),
                    actionButton('mod-commentary','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("mod_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket



      ###############################################.
      # Method of delivery commentary ----
      ###############################################.

mod_commentary <-
  tagList(
    fluidRow(
      column(8, h2("Method of delivery")), 
      column(4, div(bsButton("jump_to_mod", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("7 September 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley in May 2022, so the proportion 
      of births that are delivered by caesarean section in this month is likely to change in 
      future releases of the dashboard."), 
    
    h3("3 August 2022"),
    p("Data are thought to be incomplete for NHS Fife in April 2022, so the proportion of births 
      that are delivered by caesarean section in this month is likely to change in future releases 
      of the dashboard. Data submissions from NHS Forth Valley were insufficient to report for 
      April 2022. These will be updated in future dashboard releases."),
    
    h3("6 July 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley in March 2022 and for NHS Fife in 
      February 2022, so the proportion of births that are delivered by caesarean section in these 
      months is likely to change in future releases of the dashboard."),
    
    h3("1 June 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley and for NHS Fife in February 2022, 
      so the proportion of births that are delivered by caesarean section in February 2022 is 
      likely to change in future releases of the dashboard."),
    
    h3("4 May 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 and January 2022 
      so the proportion of births that are delivered by caesarean section for these months is 
      likely to change in future releases of the dashboard."),
    
    h3("6 April 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 so the proportion 
      of births that are delivered by elective caesarean section, which appears lower than that 
      for most other NHS Boards, is likely to change in future releases of the dashboard"),
    
    h3("6 October 2021"),
    p("The proportion of all caesarean sections in Scotland has risen and remains higher than 
      the pre-pandemic average for a number of consecutive months with the proportion approaching 40%.
      Over the last five months the proportions of births recorded as caesarean sections in NHS 
      Greater Glasgow & Clyde have been consistently above 40%. We are linking with NHS Greater 
      Glasgow & Clyde to investigate this further."),
    
    h3("1 September 2021"),
    p("In this release of information on method of delivery (1st September 2021) data have been 
      updated to include women discharged after delivery up to and including May 2021. The data 
      for all Scotland show that the proportion of all caesarean sections has risen, they remain 
      higher than the pre-pandemic average in the last 12 consecutive months and are now approaching 
      40%.  In recent months the proportions recorded for NHS Grampian have been particularly high, 
      approaching 50%. We are linking with NHS Grampian to investigate this further."),
    
    h3("7 July 2021"),
    p("In this release of information on method of delivery (7th July 2021) data have been updated 
      to include women discharged after delivery up to and including March 2021. The data for all 
      Scotland show that the proportion of all caesarean sections has risen, they remain higher 
      than the pre-pandemic average in the last 12 consecutive months and are now approaching 40%."),
    p("A sharp increase was noted for NHS Fife in the most recent month. We are linking with NHS Fife 
      to explore possible reasons for this."),
    
    h3("2 June 2021"),
    p("In this release of information on method of delivery (2nd June 2021) data have been updated 
      to include women discharged after delivery up to and including February 2021. The data for 
      all Scotland show that the proportion of both emergency caesarean sections and elective 
      caesarean sections have remained higher than the pre-pandemic average in recent months 
      (at least the last 11 consecutive months). However, the shift in the proportion of elective 
      caesarean sections (and of all caesarean sections) predates the COVID-19 pandemic. Including 
      February 2021, NHS Grampian, NHS Greater Glasgow & Clyde and NHS Highland recorded a higher 
      proportion of elective sections than their pre-pandemic average for at least the last 6 
      consecutive months. In NHS Ayrshire & Arran there has been a sequential month-on-month decrease 
      in the proportion of emergency caesarean sections in the last 5 consecutive months. NHS Dumfries 
      & Galloway, NHS Fife, NHS Grampian, NHS Greater Glasgow & Clyde, and NHS Lothian have continued, 
      in February 2021, to show at least 6 consecutive months with a higher proportion of emergency 
      sections than their pre-pandemic average. Including February 2021, NHS Dumfries & Galloway, 
      NHS Fife, NHS Grampian, NHS Greater Glasgow & Clyde and NHS Lothian have continued to show at 
      least 6 consecutive months with a higher proportion of all caesarean sections than their 
      pre-pandemic average. However there has been a sequential month-on-month decrease in the 
      proportion of all caesarean sections in the last 5 consecutive months in NHS Lothian."),
    
    h3("5 May 2021"),
    p("In this release of information on method of delivery (5th May 2021) data have been updated 
      to include women discharged after delivery up to and including January 2021. The data for 
      all Scotland show that the proportion of both emergency caesarean sections and elective 
      caesarean sections have remained higher than the pre-pandemic average in recent months 
      (at least the last 10 consecutive months). However, the shift in the proportion of elective 
      caesarean sections predates the COVID-19 pandemic. Including January 2021, NHS Grampian and 
      NHS Highland recorded a higher proportion of elective sections than their pre-pandemic average 
      for at least the last 6 consecutive months. NHS Dumfries & Galloway, NHS Greater Glasgow & Clyde 
      and NHS Lothian have continued, in January 2021, to show at least 6 consecutive months with a 
      higher proportion of emergency sections than their pre-pandemic average. In January 2021, 
      NHS Dumfries & Galloway and NHS Lothian have continued to show at least 6 consecutive months 
      with a higher proportion of all caesarean sections than their pre-pandemic average. Including 
      Jan 2021, NHS Greater Glasgow & Clyde have shown 5 consecutive months where a sequential 
      month-on-month decrease in the proportion of all caesarean sections has occurred."),
    
    h3("7 April 2021"),
    p("In this third release of information on method of delivery (7th April 2021) data have been 
      updated to include women discharged after delivery up to and including December 2020. The data 
      for all Scotland show that the proportion of both emergency caesarean sections and elective 
      caesarean sections have remained higher than the pre-pandemic average in recent months 
      (at least the last nine consecutive months). However, the shift in the proportion of elective 
      caesarean sections predates the COVID-19 pandemic. In December 2020, NHS Grampian recorded a 
      higher proportion of elective sections than their pre-pandemic average for the 14th consecutive 
      month. NHS Dumfries & Galloway and NHS Greater Glasgow & Clyde have continued in December 2020 
      to show at least 6 consecutive months with a higher proportion of emergency sections than their 
      pre-pandemic average. In December 2020, NHS Fife and NHS Lothian have continued to show at 
      least 6 consecutive months with a higher proportion of all caesarean sections than their 
      pre-pandemic average. However, data are thought to be incomplete for NHS Fife for December 
      2020 so this proportion could change in future releases of the dashboard."),
    
    h3("3 March 2021"),
    p("In this third release of information on method of delivery (3rd March 2021) data have been 
      updated to include women discharged after delivery up to and including November 2020. The data 
      for all Scotland show that both the proportion of emergency caesarean sections and elective 
      caesarean sections have remained higher than the pre-pandemic average for a number of consecutive 
      months.  However, the shift in the proportion of elective caesarean sections predates the COVID-19 
      pandemic.  In November 2020, NHS Ayrshire & Arran and NHS Grampian continue their run of consecutive 
      months showing a higher proportion of elective sections than their pre-pandemic average. NHS Dumfries 
      & Galloway, NHS Greater Glasgow & Clyde and NHS Fife have continued to show consecutive months with 
      a higher proportion of emergency sections than their pre-pandemic average.  However, data are thought 
      to be incomplete for NHS Fife for November 2020 so this proportion could change in future releases 
      of the dashboard."),
    
    h3("3 February 2021"),
    p("In this second release of information on method of delivery (3rd February 2021) data have been 
      updated to include women discharged after delivery up to and including October 2020. Both the proportion 
      of emergency caesarean sections and elective caesarean sections in Scotland as a whole have increased 
      over this time. The rise appears to be driven by the increase in elective caesarean proportions with a 
      notable increase in deliveries by elective caesarean from 16.4% to 18.4% between August and October 
      2020. The data by NHS Board of residence show more varied patterns. An increase in the proportion of 
      elective caesareans is evident in some, but not all, NHS Board areas. For example NHS Greater 
      Glasgow & Clyde (GGC), NHS Tayside and NHS Ayrshire and Arran, have a higher proportion of elective 
      sections, although the shift precedes the pandemic period in GGC and the proportion of emergency 
      sections in GGC is also elevated from April to October 2020. NHS Lothian and NHS Grampian also show 
      an increase in both categories of caesarean section during 2020. The percentage of emergency and all 
      caesarean sections are up in NHS Dumfries & Galloway from May to October 2020.  The percentage of 
      emergency caesarean sections is down in NHS Lanarkshire from February to September 2020. NHS Fife 
      shows a sharper increase in emergency caesarean section proportions than elective caesareans, however, 
      data are thought to be incomplete for NHS Fife for October 2020 so this proportion is likely to 
      change in future releases of the dashboard."),
    
    h3("16 December 2020"),
    p("Information on method of delivery was included in this tool for the first time on 16 December 2020."),
    p("The ‘",
      tags$a(href="https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/assisted-birth", 
             "method of delivery (external website) ",class="externallink",target="_blank"),
      "' refers to the way a baby is born.  Different methods of delivery include spontaneous vaginal 
      delivery (a natural birth); assisted vaginal delivery (including vaginal delivery by forceps or 
      ventouse, or vaginal delivery of a breech baby); or a caesarean section (an operation to deliver 
      the baby through a cut in the mother’s abdomen).  A caesarean section can be elective (planned in 
      advance and provided before labour has started) or emergency (unplanned, and usually but not 
      always provided after labour has started)."),
    p("Care for women around the time they are giving birth is an essential, time critical service that 
      cannot be deferred.  As such, it has been provided throughout the COVID-19 pandemic, and maternity 
      staff have not been redeployed to support other services.  The way that some elements of this care 
      are provided has changed in response to COVID-19 however, to minimise the risk of infection and to 
      allow services to continue to provide safe care during times when a high number of staff may be off 
      work, for example due to needing to isolate."),
    p("Guidance issued by the ",
      tags$a(href="https://www.gov.scot/collections/coronavirus-covid-19-guidance/#health,careandsocialwork", 
             "Scottish Government (external website)",class="externallink",target="_blank"),
      " and ",
      tags$a(href="https://www.rcog.org.uk/coronavirus-pregnancy", "Royal College of Obstetricians and 
             Gynaecologists (external website)",class="externallink",target="_blank"),
      "to maternity services at the height of the first wave of the pandemic noted that:"),
    tags$ul(
      tags$li("It may be necessary for services to temporarily suspend the option for women to deliver 
              at home or in midwife led units, and to concentrate delivery care within obstetric units"),
      tags$li("Additional restrictions on the use of water births were recommended"),
      tags$li("Care pathways for women requiring induction of labour should be amended to ensure the early 
              stages of the induction process were delivered on an outpatient basis wherever possible"),
      tags$li("Services should consider deferring a planned induction of labour or elective caesarean section 
              if a woman was isolating due to having COVID-19, or having been in contact with a case, if it 
              was safe to do so"),
      tags$li("Services should support low risk women in the early latent phase of labour to remain at home 
              wherever possible"),
      tags$li("In general, strict restrictions on visitors for patients in hospital were advised, however 
              women giving birth could still be accompanied by their chosen birth partner")
    ),
    p("The information on method of delivery presented through this tool is taken from hospital discharge 
      records, specifically records relating to the care of women delivering a singleton live birth 
      (i.e. one baby, not twins or more) at any stage of pregnancy.  Further technical information is 
      available through the ‘Data source’ button on the dashboard page."),
    p("The data shows that, at all Scotland level, the percentage of singleton live births delivered by 
      caesarean section (the ‘caesarean section rate’) has gradually increased from January 2018 (when 
      the data shown starts) to end September 2020 (the latest point for which data is currently available). 
      The increase is particularly seen in the elective caesarean section rate, but is also evident in the 
      emergency caesarean section rate.  The upward trend in the elective and emergency caesarean section 
      rates predates the COVID-19 pandemic, and it has continued during the pandemic. Whilst caesarean 
      section can be a lifesaving operation for mothers and babies, the high and rising caesarean section 
      rate seen in many countries over recent years is a ",
      tags$a(href="https://obgyn.onlinelibrary.wiley.com/doi/full/10.1111/1471-0528.13526", "cause for 
             concern (external website)",class="externallink",target="_blank"),
      ". Excessive use of caesarean sections can carry unnecessary risks for mothers and babies."),
    p("Prior to the COVID-19 pandemic, the caesarean section rate was somewhat variable between NHS 
      Board areas of residence.  There is also some variation between areas in how the caesarean 
      section rate has changed around the time of the pandemic, for example the emergency caesarean 
      section rate has increased noticeably for women living in NHS Fife, whereas the elective and 
      emergency caesarean section rates have decreased for women living in NHS Lanarkshire."),
    p("There is a very clear gradient in the caesarean section rate by maternal age, with the rate 
      being lowest among mothers in the youngest (<20 years) age group and highest among mothers 
      in the oldest (40+ years) age group. These patterns have persisted during the COVID-19 
      pandemic. As women from the least deprived areas of Scotland tend to have their children at 
      older ages than women from more deprived areas, this means that the caesarean section rate 
      tends to be highest among mothers living in the least deprived areas."),
    br()
    )



##############################################.
# Gestation at delivery ----
##############################################.
gestation_tab <- 
  tabPanel(title = "Gestation at delivery", value = "gestation",
           wellPanel(
             column(4, selectgeo_ui("gest", area_choices =  c("Scotland", "Health board"), step_no = "1")),
             column(4,offset=2,
                    sourcemodal_ui("gest"),
                    fluidRow(br()),
                    downloadButton("download_gest_data", "Download data"),
                    fluidRow(br()),
                    actionButton("gest-commentary","Go to commentary"))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("gestation_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket


      ###############################################.
      # Gestation at delivery commentary ----
      ###############################################.

gestation_commentary <-
  tagList(
    fluidRow(
      column(8, h2("Gestation at delivery")), 
      column(4, div(bsButton("jump_to_gestation", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("7 September 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley in May 2022, so the proportion 
      of births that are delivered pre-term and post-term in this month are likely to change 
      in future releases of the dashboard."), 
    
    h3("3 August 2022"),
    p("Data are thought to be incomplete for NHS Fife in April 2022, so the proportion of births 
      that are delivered pre-term and post-term in this month are likely to change in future releases 
      of the dashboard. Data submissions from NHS Forth Valley were insufficient to report for 
      April 2022. These will be updated in future dashboard releases."),
    
    h3("6 July 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley in March 2022 and for NHS Fife in 
      February 2022, so the proportion of births that are delivered pre-term and post-term in 
      these months are likely to change in future releases of the dashboard."),
    
    h3("1 June 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley and for NHS Fife in February 2022, 
      so the proportions of births that are delivered pre-term and post-term in February 2022 
      are likely to change in future releases of the dashboard."),
    
    h3("4 May 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 and January 2022 
      so the proportions of births that are delivered pre-term and post-term for these months are 
      likely to change in future releases of the dashboard."),
    
    h3("6 April 2022"),
    p("Data are thought to be incomplete for NHS Forth Valley for December 2021 so the proportion 
      of births that are preterm, which appears lower than that for most other NHS Boards, is likely 
      to change in future releases of the dashboard."),
    
    h3("2 March 2022"),
    p("The percentage of singleton live births delivered at or over 42 weeks (‘post-term’) in NHS 
      Grampian has been increasing and has remained above the pre-pandemic average for the last 
      8 consecutive months, although it should be noted that numbers of post-term births delivered 
      each month are very small.  NHS Grampian have been made aware of the data and PHS will continue 
      to monitor."),
    
    h3("1 December 2021"),
    p("NHS Tayside have shown a recent drop in their proportion of births delivered at 32-36 weeks 
      gestation, however, numbers involved are very small and so are likely to fluctuate from month 
      to month. "),
    
    h3("3 November 2021"),
    p("Following 11 months (October 2019 to September 2020) where the percentage of singleton live 
      births in NHS Lothian delivered at or over 42 weeks (‘post-term’) was below the pre-pandemic 
      average of 1.9%, the proportion of post-term births has now been above the pre-pandemic average 
      for 10 consecutive months (October 2020 to July 2021). We are working with the board in order 
      to investigate this further."),
    
    h3("2 June 2021"),
    p("In this release of information on gestation at delivery (2nd June 2021) data have been updated 
      to include women discharged after delivery up to and including February 2021. The data at all 
      Scotland level show that the preterm proportion (the percentage of singleton live births delivered 
      at under 37 weeks gestation) has decreased in February 2021 to a level of 5.5%, although the 
      previous two months were either side of the pre-pandemic average. The data by NHS Board vary. 
      In February 2021, NHS Borders and NHS Ayrshire & Arran have recorded a preterm proportion lower 
      than their pre-pandemic average for at least 6 consecutive months. In the last 5 consecutive months, 
      NHS Fife have shown a sequential month-on-month increasing trend in the proportion of singleton 
      live births delivered at under 32 weeks gestation. Including February 2021, NHS Borders have shown 
      8 consecutive months where the proportion of births delivered between 32-36 weeks gestation was 
      lower than their pre-pandemic average."),
    p("In February 2021, the percentage of singleton live births in Scotland delivered at or over 42 
      weeks (‘post-term’) has continued to be below its usual historical level (for a 16th consecutive 
      month). In February 2021, NHS Greater Glasgow & Clyde recorded a lower than average post-term 
      proportion for the 17th consecutive month."),
    
    h3("5 May 2021"),
    p("In this release of information on gestation at delivery (5th May 2021) data have been updated 
      to include women discharged after delivery up to and including January 2021. The data at all 
      Scotland level show that the preterm proportion (the percentage of singleton live births 
      delivered at under 37 weeks gestation) in January 2021 is 6.5%, a level similar to the 
      pre-pandemic average. The data by NHS Board vary. In January 2021, NHS Borders has recorded 
      a preterm proportion lower than their pre-pandemic average for the 7th consecutive month albeit 
      based on very small numbers. NHS Fife has recorded unusually high preterm proportions in December 
      2020 and January 2021. However, data are thought to be incomplete for NHS Fife for these two 
      periods so these proportions could change in future releases of the dashboard. PHS are working 
      with NHS Fife to clarify this situation. Including Jan 21, NHS Tayside have shown 6 consecutive 
      months where the proportion of births delivered between 32-36 weeks gestation was lower than their 
      pre-pandemic average."),
    p("In January 2021, the percentage of singleton live births in Scotland delivered at or over 42 weeks 
      (‘post-term’) has continued to be below its usual historical level (for a 15th consecutive month). 
      In January 2021, NHS Greater Glasgow & Clyde recorded a lower than average post-term proportion 
      for the 16th consecutive month. Including Jan 21, NHS Highland have shown 5 consecutive months 
      where a sequential month-on-month decrease in the proportion of post-term births has occurred."),
    
    h3("7 April 2021"),
    p("In this third release of information on gestation at delivery (7th April 2021) data have been 
      updated to include women discharged after delivery up to and including December 2020. The data 
      at all Scotland level show that the preterm proportion (the percentage of singleton live births 
      delivered at under 37 weeks gestation) in December 2020 is at a level very similar to the pre-pandemic 
      average at 7.0%. The data by NHS Board vary.  In December 2020, NHS Borders has recorded a preterm 
      proportion lower than their pre-pandemic average for the sixth consecutive month albeit based on 
      very small numbers. NHS Fife has recorded an unusually high preterm proportion in December 2020. 
      However, data are thought to be incomplete for NHS Fife for December 2020 so this proportion 
      could change in future releases of the dashboard."),
    p("In December 2020, the percentage of singleton live births in Scotland delivered at or over 42 
      weeks (‘post-term’) has continued to be below its usual historical level (for a 14th consecutive month). 
      In December 2020, NHS Greater Glasgow & Clyde recorded a lower than average post-term proportion 
      for the 15th consecutive month."),
    
    h3("3 March 2021"),
    p("In this third release of information on gestation at delivery (3rd March 2021) data have been 
      updated to include women discharged after delivery up to and including November 2020. The data 
      at all Scotland level show that the preterm proportion (the percentage of singleton live births 
      delivered at under 37 weeks gestation) still remains fractionally below the pre-pandemic average 
      in November 2020. The data by NHS Board vary but there are no notable changes in the preterm 
      proportion for November 2020."),
    p("In November 2020, the percentage of singleton live births in Scotland delivered at or over 42 
      weeks (‘post-term’) has continued to be below its usual historical level. NHS Greater Glasgow 
      & Clyde have continued their run of consecutive months showing a lower than average post-term 
      proportion."),
    
    h3("3 February 2021"),
    p("In this second release of information on gestation at delivery (3rd February 2021) data have 
      been updated to include women discharged after delivery up to and including October 2020. 
      The data at all Scotland level show that the preterm proportion (the percentage of singleton 
      live births delivered at under 37 weeks gestation), having been lower than the pre-pandemic 
      average during the period March to July 2020, has increased slightly but still remains 
      fractionally below the pre-pandemic average at 6.7% in October 2020."),
    p("The data by NHS Board of residence show more varied patterns.  NHS Forth Valley has shown 
      quite low proportions of preterm births in recent months compared to their pre-pandemic 
      average with 4.9% in October 2020.  NHS Ayrshire & Arran, NHS Dumfries & Galloway and 
      NHS Lothian also show periods where the percentage of preterm births is lower than the 
      long-term average. However these periods start before the pandemic period. No NHS Boards are 
      showing particularly high preterm numbers. Data are thought to be incomplete for NHS Fife for 
      October 2020 so the proportion of births that are preterm, which is higher than that for most 
      other NHS Boards, is likely to change in future releases of the dashboard."),
    p("The percentage of singleton live births in Scotland delivered at or over 42 weeks (‘post-term’) 
      has continued to be fractionally below its usual historical level. This pattern began in November 2019, 
      and no specific change in the post-term proportion has been seen during the COVID-19 pandemic. 
      The pattern at board level is more variable. NHS Greater Glasgow & Clyde has shown a decrease 
      over recent months with the percentage for October 2020 being the lowest in the period shown at 0.7%."),
    
    h3("16 December 2020"),
    p("Information on gestation at delivery was included in this tool for the first time on 16 December 2020."),
    p("‘Gestation at delivery’ refers to the number of completed weeks pregnant a woman is when she 
      delivers her baby. Babies are ‘due’ at 40 completed weeks gestation. Those born between 37 and 41 
      weeks inclusive are considered to be born ‘at term’. Babies born at under 37 weeks (more than three 
      weeks before their due date) are considered ",
      tags$a(href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/after-the-birth/premature-babies", 
             "preterm or premature (external website)",  target="_blank"),
      ", with those born at under 32 weeks considered very preterm and those born at 32 to 36 weeks inclusive 
      considered moderately preterm. Babies born at or over 42 weeks (more than two weeks after their due date) 
      are considered post-term or over-due. Babies born preterm are at increased risk of both short and long term 
      health and developmental problems, with the ",
      tags$a(href = "https://www.tommys.org/pregnancy-information/premature-birth/how-long-do-you-stay-in-hospital-after-birth/gestational-age-and-medical-needs", "risk increasing the earlier a baby is born (external website)",  target="_blank"),
      ". Babies are also at increased risk when pregnancies extend post-term, in particular the ",
      tags$a(href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/getting-ready-for-the-birth/induced-labour", "risk of stillbirth (external website)",  target="_blank"),
      " increases from 42 weeks gestation onwards."),
    p("Care for women and babies around the time they are giving birth/being born is an essential, time critical 
      service that cannot be deferred. As such, it has been provided throughout the COVID-19 pandemic, and 
      maternity and neonatal staff have not been redeployed to support other services. The way that some 
      elements of this care are provided has changed in response to COVID-19 however, to minimise the risk 
      of infection and to allow services to continue to provide safe care during times when a high number 
      of staff may be off work, for example due to needing to isolate. Relevant guidance has been issued by the ",
      tags$a(href = "https://www.gov.scot/collections/coronavirus-covid-19-guidance/#health,careandsocialwork", 
             "Scottish Government (external website)",  target="_blank"),
      ", the ",
      tags$a(href = "https://www.rcog.org.uk/coronavirus-pregnancy", 
             "Royal College of Obstetricians and Gynaecologists (external website)",  target="_blank"),
      ", and the ",
      tags$a(href = "https://www.bapm.org/pages/182-perinatal-covid-19-resources", 
             "British Association for Perinatal Medicine (external website)",  target="_blank"),
      "."),
    p("The current evidence suggests that ",
      tags$a(href = "https://www.birmingham.ac.uk/research/who-collaborating-centre/pregcov/about/publications.aspx", 
             "women with COVID-19 are at increased risk of preterm delivery (external website)",  target="_blank"),
      ". Conversely, several studies (for example from the ",
      tags$a(href = "https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(20)30223-1/fulltext", 
             "Netherlands (external website)",  target="_blank"),
      ", ",
      tags$a(href = "https://www.medrxiv.org/content/10.1101/2020.05.22.20109793v1", "Denmark (external website)",  
             target="_blank"),
      ",  and ",
      tags$a(href = "https://gh.bmj.com/content/5/9/e003075", "Ireland (external website)",  target="_blank"),
      ") have reported that the overall number or proportion of babies born preterm or with low birthweight 
      fell during the ‘lockdown’ period implemented in response to COVID-19.  The reasons for this finding are 
      currently unclear, but may reflect the combined impact of factors such as a reduction in infections 
      other than COVID-19, improved air quality, and changes to antenatal care, which together more than 
      outweigh any direct effect of COVID-19."),
      p("The information on gestation at delivery presented through this tool is taken from hospital 
        discharge records, specifically records relating to the care of women delivering a singleton 
        live birth (i.e. one baby, not twins or more) at a known gestation (between 18-44 weeks inclusive).  
        Further technical information is available through the ‘Data source’ button on the dashboard page."),
      p("The data shows that, at all Scotland level, the percentage of singleton live births delivered at 
        under 37 weeks gestation (the ‘preterm rate’) was slightly lower than usual over the period March 
        to July 2020 (at just over 6% compared to the more usual 6.8%).  This was driven by a dip in the 
        percentage of births delivered at 32-36 weeks (the ‘moderately preterm rate’): there has been no 
        change in the percentage of births delivered at under 32 weeks (the ‘very preterm rate’). 
        The percentage of singleton live births delivered at or over 42 weeks (the ‘post-term rate’) 
        has been fractionally below its usual historical level since November 2019, but no specific 
        change in the post-term rate has been seen during the COVID-19 pandemic."),
      p("Prior to the COVID-19 pandemic, the preterm rate was somewhat variable between NHS Board 
        areas of residence.  There is also some variation between areas in how the preterm rate has 
        changed around the time of the pandemic.  The preterm rate for women living in NHS Ayrshire 
        & Arran and NHS Lothian has shown a particularly pronounced fall during the pandemic. No area 
        has seen a sustained increase in the preterm rate during the pandemic.  The hospital delivery 
        discharge records returned to Public Health Scotland that are used in this tool are incomplete 
        for NHS Fife for September 2020: this is likely to account for the unusually high very preterm 
        rate seen for women living in NHS Fife in this month specifically.  We expect this unusually 
        high rate to change when more records are received and this page of the dashboard is refreshed."),
      p("The preterm rate tends to be highest among mothers in the youngest (<20 years) and oldest 
        (40+ years) age groups, however differences between age groups are not pronounced. There is a 
        clear gradient in the preterm rate by deprivation, with the rate being highest among mothers 
        living in the most deprived areas of Scotland. These patterns have persisted during the COVID-19 pandemic."),
      br()
    )
  


###############################################.
# Apgar ----
###############################################.
apgar_tab <- 
  tabPanel(title = "Apgar scores", value = "apgar",
           wellPanel(
             column(4, selectgeo_ui("apgar", area_choices =  c("Scotland", "Health board"), step_no = "1")),
             column(4,offset=2,
                    sourcemodal_ui("apgar"),
                    fluidRow(br()),
                    downloadButton("download_apgar_data", "Download data"),
                    fluidRow(br()),
                    actionButton("jump_commentary_apgar","Go to commentary"))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("apgar_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket


      ###############################################.
      # Apgar commentary  ----
      ###############################################.

apgar_commentary <- 
  tagList(
    fluidRow(
      column(8, h2("Apgar scores")), 
      column(4, div(bsButton("jump_to_apgar", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("3 August 2022"),
    p("Data are thought to be incomplete for NHS Fife and NHS Forth Valley in April 2022, 
      so the proportion of babies with a low Apgar score in this month is likely to change 
      in future releases of the dashboard."),
    
    h3("2 June 2021"),
    p("In this release of information on Apgar scores (2nd June 2021) data have been updated 
      to include women discharged after delivery up to and including February 2021. The data 
      show that, at all Scotland level, the percentage of singleton live born babies delivered 
      at 37-42 weeks gestation which have a low 5 minute Apgar score (less than 7) in February 
      2021 was at a very similar level to the pre-pandemic average at 1.8%."),
    p("The Apgar score data by NHS Board are presented quarterly and this information will next 
      be updated on 7th July 2021."),
    
    h3("14 April 2021"),
    p("Information on Apgar scores was included in this tool for the first time on 14 April 2021."),
    p("The Apgar score measures the condition of newborn babies. It was developed to allow health 
      professionals to quickly identify babies needing resuscitation after delivery. Babies are 
      scored 0, 1, or 2 for each of their heart rate; respiratory effort; muscle tone; response 
      to stimulation; and colour. Scores therefore range from 0 to 10, with higher scores indicating 
      a better condition.  Scores of 7 or over are generally interpreted as ‘reassuring’, with scores 
      of 4-6 considered moderately low, and scores of 0-3 considered very low. The Apgar score is 
      measured at 1 and 5 minutes after delivery for all babies in Scotland."),
    p("Low Apgar scores at 5 minutes after delivery are associated with a higher risk of neonatal death, 
      neonatal morbidity, and longer term problems with babies’ development. Babies born preterm can 
      have lower scores due to their overall immaturity rather than a specific problem such as lack of 
      oxygen during delivery. Due to this, the association between low Apgar scores and poor outcomes 
      is generally stronger for babies born at term (at 37-41 weeks gestation) or post-term (at ≥42 weeks 
      gestation) compared to those born preterm (at <37 weeks gestation)."),
    p("The information on Apgar scores presented through this tool is taken from hospital discharge records, 
      specifically records relating to the care of women delivering a singleton live birth (i.e. one baby, 
      not twins or more) at 37-42 weeks gestation inclusive.  Further technical information is available 
      through the ‘Data source’ button on the dashboard page."),
    p("The data shows that, at all Scotland level, just under 2% of singleton, live born babies delivered 
      at 37-42 weeks gestation have a low 5 minute Apgar score (less than 7).  The percentage of babies 
      with a low score has been broadly similar over the whole period examined (January 2018 to, currently, 
      December 2020). In particular, no increase in the percentage of babies with a low score has been seen 
      during the COVID-19 pandemic. In fact the percentage of babies born with a 5 minute Apgar score of 
      less than 7 was consistently slightly lower from Jan to Sep 2020 than the Jan 2018 to Feb 2020 average."),
    p("Prior to the COVID-19 pandemic, the percentage of babies with a low 5 minute Apgar score was similar 
      across mainland NHS Boards, ranging from around 1% of babies born to mothers living in NHS Highland 
      to around 2.5% of babies born to mothers living in NHS Lanarkshire.  Within each Board, the percentage 
      of babies with a low score fluctuates over time, as would be expected by chance.  No increase in the 
      percentage of babies with a low score has been seen during the COVID-19 pandemic in any Board."),
    p("The percentage of babies with a low 5 minute Apgar score is similar for babies born to mothers 
      from different age groups, and for babies born to mothers living in areas with different levels 
      of deprivation.  No changes to these patterns have been seen during the COVID-19 pandemic."),
    br()
    )
  


###############################################.
## Preterm ----
###############################################.
preterm_tab <- 
  tabPanel(title = "Location of extremely preterm deliveries", value = "preterm",
           wellPanel(
             column(6, div(title="",
                           p(tags$b("Location of extremely preterm deliveries data is only available at Scotland level.")))),
             column(4,offset=2,
                    sourcemodal_ui("preterm"),
                    fluidRow(br()),
                    downloadButton("download_preterm_data", "Download data"),
                    fluidRow(br()),
                    actionButton("jump_commentary_preterm","Go to commentary"))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("preterm_explorer")
           )# mainPanel bracket
  ) # tabPanel bracket

      ###############################################.
      # Preterm commentary ----
      ###############################################.

preterm_commentary <-
  tagList(
    fluidRow(
      column(8, h2("Location of extremely preterm deliveries")), 
      column(4, div(bsButton("jump_to_preterm", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("6 October 2021"),
    p("The ‘location of extremely preterm deliveries’ measure is used to monitor whether babies 
      born at 23-26 weeks gestation are born at a hospital with a neonatal intensive care unit. 
      It is desirable that this percentage is as high as possible, but inevitably some extremely 
      preterm deliveries occur in other locations, for example, a woman presenting to her local 
      maternity unit may be considered too far advanced in labour to safely transfer to a different 
      hospital before her baby is born. Control charts are used to help differentiate between 
      expected random variation (which is a particular issue for rare events such as extremely 
      premature births), and substantial changes which warrant further investigation, with expected 
      levels based on previous data.  Last quarter (April-June 2021) it was noted that the percentage 
      of extremely preterm deliveries occurring in a hospital with a neonatal intensive care unit 
      was lower than expected, with 70.8% of babies delivered at such a site, compared with the 
      warning limit of 71.3%.  However, the value remained above the lower control limit of 64.1%, 
      indicating that this observation is within the range of expected random variation. The data 
      will continue to be monitored on a quarterly basis."),
    
    h3("14 April 2021"),
    p("Information on the location of extremely preterm deliveries was included in this tool for 
      the first time on 14 April 2021."),
    p("Babies born preterm (at least 3 weeks before their due date) are at increased risk of neonatal 
      death, neonatal morbidity, and longer term developmental problems compared to babies born at term 
      (around their due date).  The earlier in pregnancy a baby is born, the higher the risks."),
    p("There is evidence that the outcomes of extremely preterm babies (here defined as those born 
      between 23 and 26 weeks gestation inclusive) are influenced by where they are born.  Extremely 
      preterm babies are more likely to survive and be healthy if they are born in a hospital that has 
      an on-site neonatal intensive care unit.  In addition, extremely preterm babies cared for in larger 
      neonatal intensive care units (those caring for high numbers of very unwell babies) have better 
      outcomes than babies cared for in smaller units."),
    p("Reflecting this evidence, the British Association of Perinatal Medicine ", 
      tags$a(href = "https://www.bapm.org/resources/80-perinatal-management-of-extreme-preterm-birth-before-27-weeks-of-gestation-2019", 
             "recommends (external website)",  target="_blank"), 
      " that when a woman is thought to be at imminent risk of extremely preterm delivery she should be 
      transferred to a maternity unit in a hospital with an on-site neonatal intensive care unit to 
      allow her baby (or babies in the case of a multiple pregnancy of twins or more) to be born in 
      the safest place.  In addition, whilst the overall number of neonatal units is not changing in 
      Scotland, the number of units that are ",
      tags$a(href = "https://www.bapm.org/resources/296-draft-optimal-arrangements-for-neonatal-intensive-care-units-in-the-uk", 
             "classed as neonatal intensive care units (external website)",  target="_blank"),
      " (also known as level III units, those able to provide the most complex, specialist care) is 
      reducing over time in line with ",
      tags$a(href = "https://www.gov.scot/publications/best-start-five-year-forward-plan-maternity-neonatal-care-scotland/", 
             "national policy (external website)",  target="_blank"),
      " to concentrate expertise and improve babies’ outcomes."
      ),
    p("The information on location of extremely preterm deliveries presented through this tool is 
      taken from hospital discharge records relating to the care of women delivering one or more 
      live born babies at 23-26 weeks gestation inclusive.  The charts presented show the number 
      and percentage of these deliveries that occurred in a hospital that had a neonatal intensive 
      care unit on site at the time of the delivery.  Information on which hospitals have had a 
      neonatal intensive care unit on site over the time period of interest (from January 2018), 
      and associated dates, has been provided by the ",
      tags$a(href = "https://www.perinatalnetwork.scot/", "Scottish Perinatal Network (external website)",  
             target="_blank"), 
      ". Due to the small number of deliveries at this very early gestation, data is only shown 
      at all Scotland level, and no breakdown is provided by maternal age group or deprivation level. 
      Further technical information is available through the ‘Data source’ button on the dashboard page."),
    p("The data shows that, at all Scotland level over the whole time period examined (January 2018 to, 
      currently, December 2020), just under 9 in every 10 (87%) extremely preterm deliveries occurred in 
      a hospital with a neonatal intensive care unit on site at the time of the delivery. The percentage 
      has been consistently within both the warning and control limits over the whole time period examined, 
      suggesting that any fluctuation seen has been due to chance, with no unexpected changes evident. 
      In particular, no decline in the percentage of extremely preterm deliveries that occurred in a hospital 
      with a neonatal intensive care unit on site has been seen during the COVID-19 pandemic."),
    p("In general, it is inevitable that some extremely preterm deliveries occur in locations other than 
      hospitals with a neonatal intensive care unit on site. For example, a woman presenting to her local 
      maternity unit may be considered too far advanced in labour to safely transfer to a different hospital 
      before her baby is born."),
    br()
    )
  


##############################################.
# Perineal tears  ----
##############################################.
perineal_tab <- tabPanel(title = "Perineal tears", value = "tears",
                         wellPanel(
                           column(4, selectgeo_ui("tears", area_choices =  c("Scotland", "Health board"), step_no = "1")),
                           column(4,offset=2,
                                  sourcemodal_ui("tears"),
                                  fluidRow(br()),
                                  downloadButton("download_tears_data", "Download data"),
                                  fluidRow(br()),
                                  actionButton("jump_commentary_tears","Go to commentary"))
                         ), #well panel
                         mainPanel(width = 12,
                                   uiOutput("tears_explorer")
                         )# mainPanel bracket
) # tabPanel bracket

      ###############################################.
      # Perineal tears commentary ----
      ###############################################.

tears_commentary <- 
  tagList(
    fluidRow(
      column(8, h2("Perineal tears")), 
      column(4, div(bsButton("jump_to_tears", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3("3 August 2022"),
    p("Data are thought to be incomplete for NHS Fife and NHS Forth Valley in April 2022, 
      so the proportion of births that result in a tear in this month is likely to change 
      in future releases of the dashboard. "),
    
    h3("2 February 2022"),
    p("The percentage of women resident in NHS Lanarkshire giving birth vaginally to a singleton 
      live or stillborn baby with a cephalic presentation between 37-42 weeks gestation, 
      and who have a third or fourth degree perineal tear has been above the pre-pandemic 
      average for six successive quarters (covering the period April 2020 - September 2021). 
      The pre-pandemic rate was 3% and that for July-September 2021 3.7%. It is important to note 
      that some women resident in NHS Lanarkshire receive delivery care from bordering health 
      boards. At Scotland level the pre-pandemic rate of third and fourth degree perineal tears 
      was 3.5% and that for July-September 2021 was 3.2%. NHS Lanarkshire have been made aware of this 
      data and PHS will continue to monitor."),
    
    h3("3 November 2021"),
    p("The percentage of women giving birth vaginally to a singleton live or stillborn baby 
      with a cephalic presentation between 37-42 weeks gestation who have a third or fourth 
      degree perineal tear has been above the pre-pandemic average for seven successive 
      quarters (including April-June 2021) in both NHS Dumfries & Galloway and NHS Greater 
      Glasgow & Clyde. We are working with these two boards in order to investigate this 
      further. NHS Dumfries & Galloway have indicated that their overall numbers of third 
      and fourth degree tears are small. They routinely review all women who have had a 
      third or fourth degree tear at their Clinical Incident Review Group and they have 
      not so far identified any common themes."),
    
    h3("16 June 2021"),
    p("Information on perineal tears was included in this tool for the first time on 
      16 June 2021."),
    p("When a woman is giving birth, the baby stretches the mother’s vagina and perineum. 
      Occasionally, the tissues cannot stretch enough, and a tear (called a ",
      tags$a(href= 'https://www.rcog.org.uk/en/patients/patient-leaflets/third--or-fourth-degree-tear-during-childbirth/',
             "'perineal tear (external website)'", target="_blank"),") occurs. The perineum 
      is the area between a woman’s vagina and anus."),
    p("Perineal tears are classified as 1st to 4th degree, with 4th degree tears being the 
      most serious. First degree tears just involve the skin of the perineum or lining of 
      the lower vagina. Second degree tears also involve the muscles of the perineum. Third 
      degree tears extend further back and also involve the muscles surrounding the anus. 
      Fourth degree tears extend further into the lining of the anus or rectum (lower bowel)."),
    p("Third and 4th degree tears are also known as obstetric anal sphincter injury. These tears 
      require surgical repair immediately after delivery. Most women recover completely following 
      a 3rd or 4th degree tear, however some are left with persistent problems controlling their 
      bowels (anal incontinence)."),
    p("Most tears are unexpected and it’s hard to predict which women will have a tear, although 
      tears are more common during a woman’s first vaginal delivery, if the baby is big 
      (over 4kg birthweight), or if the second stage of labour goes on for a long time."),
    p("An ",
      tags$a(href= 'https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/assisted-birth/perineal-tears-and-episiotomy',
             'episiotomy (external website)', target="_blank")," may be offered if a woman is 
      thought to be at risk of a tear. An episiotomy is a controlled cut made by a healthcare 
      professional through the vaginal wall and perineum that is repaired with stitches after 
      delivery. An episiotomy does not guarantee that a tear will not happen, as the episiotomy 
      cut may extend and become a tear. Women requiring assisted vaginal delivery (with forceps 
      or ventouse) are at high risk of a tear so would generally be offered an episiotomy."),
    p("Care for women around the time they are giving birth is an essential, time critical service 
      that cannot be deferred. As such, it has been provided throughout the COVID-19 pandemic, 
      and maternity staff have not been redeployed to support other services."),
    p("However, there have been some changes to how delivery care is provided in response to COVID-19, 
      to minimise the risk of infection and to allow services to continue to provide safe care 
      during times when a high number of staff may be off work, for example due to needing to 
      isolate. These changes have varied over time and between areas. For example, guidance 
      issued by the  ",
      tags$a(href= 'https://www.gov.scot/collections/coronavirus-covid-19-guidance/#health,careandsocialwork',
             'Scottish Government (external website)', target="_blank")," and ",
      tags$a(href= 'https://www.rcog.org.uk/coronavirus-pregnancy',
             'Royal College of Obstetricians and Gynaecologists (external website)', target="_blank"),
      " to maternity services at the height of the first wave of the pandemic (spring 2020) 
      noted that it may be necessary for services to temporarily suspend the option for women to 
      deliver at home or in midwife led units, and to concentrate delivery care within obstetric 
      units. This tool allows us to monitor whether changes to care provision associated with 
      COVID-19 have led to any changes in the outcomes of women or babies."),
    p("The information on perineal tears presented through this tool is taken from hospital discharge 
      records, specifically records relating to the care of women undergoing spontaneous or assisted 
      vaginal delivery of a singleton live or stillborn baby (i.e. one baby, not twins or more) with 
      cephalic (i.e. ‘head first’) presentation at 37-42 weeks gestation (i.e. up to 3 weeks before 
      or after their due date).  Further technical information is available through the ‘Data source’ 
      button on the dashboard page."),
    p("The data shows that, at all Scotland level, the percentage of women giving birth vaginally to 
      a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation 
      who have a 3rd or 4th degree perineal tear (the ‘tear rate’) has remained broadly constant at 
      around 3.5% from January 2018 (when the data shown starts) to end February 2021 (the latest 
      point for which data is currently available)."),
    p("The tear rate varies somewhat between NHS Board areas of residence: for example the average 
      rates in mainland Boards in the period prior to the COVID-19 pandemic ranged from 1.5% for 
      women in NHS Dumfries & Galloway to 4.6% in NHS Lothian. No areas have shown a clear change 
      in the tear rate since the start of the COVID-19 pandemic."),
    p("The tear rate does not show any clear relationship to maternal age. The tear rate tends to 
      be highest among mothers living in the least deprived areas of Scotland, and this pattern has 
      not changed during the COVID-19 pandemic."),
    br()
  )
  

#############################################.
# Stillbirths and infant deaths (perinatal) ----
###############################################.
perinatal_tab <- 
  tabPanel(title = "Stillbirths and infant deaths", value = "perinatal_mortality",
           wellPanel(
             column(4, selectdata_ui("perinatal", measure_choices = data_list_perinatal)),
             column(4, sourcemodal_ui("perinatal"),
                    fluidRow(br()),
                    downloadButton("download_perinatal_data", "Download data"),
                    fluidRow(br()),
                    actionButton('jump_commentary_perinatal','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("perinatal_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket

###############################################.
# Stillbirths and infant deaths commentary ----
###############################################.
perinatal_commentary <- 
  tagList(
    fluidRow(
      column(8, h2("Stillbirths and infant deaths")), 
      column(4, div(bsButton("jump_to_perinatal_mortality", label = "Go to data"), style="float:right"))),  #this button can only be used once

    h3("4 May 2022"),
    p("In this release of information on stillbirths and infant deaths, data have been updated 
      to include events that occurred in March 2022."),
    p("In March 2022 the neonatal mortality rate (4.6 per 1,000 live births) exceeded the upper 
      control limit of 4.4 per 1,000. The extended perinatal mortality rate, which captures both 
      stillbirths and neonatal deaths was 9.1 per 1,000 live and stillbirths; this was above the 
      upper warning limit of 8.4 per 1,000 but did not exceed the control limit of 9.6 per 1,000. 
      Similarly, the overall infant mortality rate (5.9 per 1,000 live births) exceeded the warning 
      limit (5.1 per 1,000), but not the upper control limit 6.0 per 1,000). Post-neonatal deaths 
      (those that occur after 4 weeks of age) were within the expected range."),
    p("Each of the losses reflected in the information reported here is a tragedy for those involved. 
      The review processes described below (2nd March 2022) will be important in understanding 
      and learning from these events."),
    p("The effects of COVID-19 infection, and the safety and protection of COVID-19 vaccination 
      in pregnancy, ",
      tags$a(href = "https://academic.oup.com/ije/advance-article/doi/10.1093/ije/dyab243/6491903?login=true",
             "continue to be monitored in Scotland (external website) ", target = "_blank"),
      tags$a(href = "https://obgyn.onlinelibrary.wiley.com/doi/full/10.1002/uog.23619",
             "and internationally (external website)", target = "_blank"),
      ". There is evidence that COVID-19 infection during pregnancy is associated with worse 
      outcomes for mothers and babies. In Scotland it has been found that among babies born to 
      mothers who had COVID-19 infection in the month prior to birth, the extended perinatal 
      mortality rate was 13.4 per 1,000 live and stillbirths (95% confidence interval 8.1-21.9)",
      tags$a(href = "https://publichealthscotland.scot/media/12100/22-03-09-covid19-winter_publication_report.pdf",
             "(COVID-19 winter publication report)", target = "_blank"),"."),
    p("COVID-19 vaccination is a safe and effective way to reduce the risk of COVID-19 in pregnancy, 
      and vaccination is strongly recommended. By mid-February 28,301 women had been vaccinated during 
      pregnancy in Scotland, and 54% of women who gave birth in January 2022 had received at least two 
      doses of vaccine, either during or prior to pregnancy. Among babies born to mothers who received 
      a dose of COVID-19 vaccination in the month prior to birth, the extended perinatal mortality rate 
      was 5.2 per 1,000 live and stillbirths (95% confidence interval 2.9-8.9)",
      tags$a(href = "https://publichealthscotland.scot/media/12100/22-03-09-covid19-winter_publication_report.pdf",
             "(COVID-19 winter publication report).", target = "_blank"),
      "More information and support with decision-making is available from the Royal College of 
      Obstetricians and Gynaecologists", 
      tags$a(href = "https://www.rcog.org.uk/guidance/coronavirus-covid-19-pregnancy-and-women-s-health/vaccination/covid-19-vaccines-pregnancy-and-breastfeeding-faqs/", "(https://www.rcog.org.uk/guidance/coronavirus-covid-19-pregnancy-and-women-s-health/vaccination/covid-19-vaccines-pregnancy-and-breastfeeding-faqs/)", target = "_blank"), "and NHS Inform", tags$a(href = "https://www.nhsinform.scot/covid-19-vaccine/the-vaccines/pregnancy-breastfeeding-and-the-coronavirus-vaccine", "(https://www.nhsinform.scot/covid-19-vaccine/the-vaccines/pregnancy-breastfeeding-and-the-coronavirus-vaccine)", target = "_blank"),"."),
    
    h3("2 March 2022"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to 
      include events that occurred in January 2022, when all reported measures of perinatal and 
      infant mortality were within expected limits."),
    p("As described in the dashboard information box ‘How do we identify patterns in the data?’, 
      control charts are used to provide an indication of when changes in these data are less 
      likely to be due to chance alone. In refreshed data now available for 2021, the months March 
      to October are identified as a ‘shift’ in the neonatal mortality rate, and the months April 
      to November as a ‘shift’ in the infant mortality rate. A ‘shift’ describes when there is a 
      sequence of 8 or more months of data that are above (or below) the average level, which 
      here is based on the pre-pandemic mortality rates from 2017 to 2019. These ‘shifts’ are 
      indicated by the blue markers on the chart for the relevant sequence of months. This pattern 
      suggests there was a sustained period in the middle part of 2021 when neonatal and infant 
      mortality rates were higher than pre-pandemic levels, rather than fluctuating around this 
      level as would be expected with random variation. In the most recent months, the rates for 
      both measures have been below the average level. No shifts are noted for stillbirths, 
      extended perinatal mortality or post-neonatal deaths."),
    
    p("As noted below, neonatal and infant deaths are subject to a number of review and learning 
      processes to identify and mitigate any contributing factors. Systematic information on 
      deaths occurring in neonatal units is gathered via the",
      tags$a(href = "https://www.npeu.ox.ac.uk/pmrt", "Perinatal Mortality Review Tool (external website);", 
             target = "_blank"), "the UK-wide collaboration,",
      tags$a(href = "https://www.npeu.ox.ac.uk/mbrrace-uk", "MBRRACE-UK (external website),", target = "_blank"),
      " provides surveillance and investigation of maternal deaths, stillbirths and infant 
      deaths, and there is a",
      tags$a(href = "https://www.gov.scot/publications/maternity-neonatal-perinatal-adverse-event-review-process-scotland/",
             "standardised approach to review of perinatal adverse events in Scotland (external website).", 
             target = "_blank"),
      " All child deaths in Scotland are now reviewed to ensure that contributing factors are understood, 
      and that learning is used in ",
      tags$a(href = "https://www.healthcareimprovementscotland.org/our_work/governance_and_assurance/deaths_of_children_reviews.aspx",
             "prevention and improving care quality (external website).", target = "_blank"), "
      A number of agencies consider the outcomes of these review processes, including the",
      tags$a(href = "https://www.perinatalnetwork.scot/neonatal/", "Scottish National Neonatal Network (external website),", target = "_blank"),
      "the ",
      tags$a(href = "https://ihub.scot/improvement-programmes/scottish-patient-safety-programme-spsp/spsp-programmes-of-work/maternity-and-children-quality-improvement-collaborative-mcqic/",
             "Maternity and Children Quality Improvement Collaborative (external website)", target = "_blank"), 
      " and the Scottish Government."),
    p("Annual data on stillbirths and infant deaths are produced by ",
      tags$a(href = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables",
             "National Records of Scotland (external website) ", target = "_blank"),
      "and information for 2021 is scheduled to be published in June 2022."),
    
    h3("2 February 2022"),
    p("In this release of information on stillbirths and infant deaths, data have been updated 
      to include events that occurred in November and December 2021. In these months all reported 
      measures of perinatal and infant mortality were within expected limits."),
    p("In addition, in this dashboard release information on gestation at delivery has been updated 
      under the ‘Births and babies’ tab to include births in September and October 2021, which is 
      of interest in follow up to the commentary below on neonatal mortality in September 2021. 
      These data show that in September 2021, 0.8% of singleton live births occurred at under 32 
      weeks gestation, and 5.5% at 32-36 weeks gestation. Both of these figures are close to the 
      expected level based on the average over the period January 2018 to February 2020. As these 
      data are based on singleton births only, data were also reviewed separately to assess the 
      total number and percentage of premature babies, including those from multiple births. 
      This showed that the total number of babies born at under 32 weeks gestation in September 
      2021 was relatively high, and in the upper quartile for monthly values in January to 
      October 2021, however it was not exceptional in comparison to the observed values in this period."),
    p("Further information on COVID-19 infection and vaccination in pregnancy in Scotland, 
      including data on neonatal infections and extended perinatal mortality rate have also 
      been published recently (see ", tags$a(href="https://www.nature.com/articles/s41591-021-01666-2",
                                             "SARS-CoV-2 infection and COVID-19 vaccination rates in 
                                             pregnant women in Scotland (external website)", target="_blank"), " and ",
      tags$a(href="https://www.publichealthscotland.scot/publications/show-all-releases?id=20580", 
             "Public Health Scotland publications", target="_blank"), "). The information in 
      these publications provides further reassurance regarding the safety of vaccination in pregnancy
      and highlights the effective protection it provides for pregnant women and their babies."),
    
    h3("1 December 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated 
      to include events that occurred in October 2021."),
    p("In October 2021 all reported measures of perinatal and infant mortality were within expected 
      limits. The neonatal mortality rate, which was raised in September 2021, was 3.3 per 1,000 live 
      births in October 2021 and returned to within the warning limit (3.6 per 1,000). The overall infant 
      mortality rate (4.9 per 1,000 live births) in October 2021 was close to, but did not breach the 
      upper warning limit (5.0 per 1,000), whereas in September 2021 it was above this (5.5 per 1,000). 
      Stillbirths and post-neonatal deaths (those that occur after 4 weeks of age) were at expected 
      levels in October 2021."),
    p("As referenced in the commentary on September 2021 data, all neonatal deaths are the subject of 
      local and national review processes. In addition to this, the higher than expected numbers that 
      month prompted additional review of available data at national level, in particular with respect 
      to the role of prematurity, and to understand any relationship with COVID-19 infections. Findings 
      from this review are preliminary, as relevant information at national level on the total number 
      of births and gestational age of babies in that period is not yet fully complete, but will be 
      by February 2022 (see below)."), 
    p("Initial findings suggest that, overall, the number of births in September 2021 was at the expected 
      level. Preliminary information on prematurity suggests that the number of babies born at less than 
      32 weeks gestation in September 2021 was at the upper end of monthly numbers seen in 2021 to date. 
      This may contribute to the neonatal mortality rate, as prematurity is associated with an increased 
      risk of neonatal death. The relevant dashboard indicators on live births and gestational age will 
      be updated to include September 2021 information, using the most complete data available, in the 
      next dashboard update in February 2022."),
    p("There is no information at this stage to suggest that any of the neonatal deaths in September 
      2021 were due to COVID-19 infection of the baby. Likewise, preliminary review does not indicate 
      that maternal COVID-19 infection played a role in these events. Several surveillance programmes 
      focussing on direct impact of COVID-19 on pregnant women and babies are underway. The ",
      tags$a(href="https://www.ed.ac.uk/usher/eave-ii/covid-19-in-pregnancy-in-scotland", 
             "COVID-19 in Pregnancy in Scotland study (COPS) (external website)", target="_blank"),
      "has been established to provide population-level monitoring and analysis of the occurrence and 
      outcomes of COVID-19 infection in pregnancy. Monthly reporting of cases is available within the ", 
      tags$a(href="https://publichealthscotland.scot/publications/covid-19-statistical-report/",
             "PHS COVID-19 Statistical Report", target="_blank"), "and will next be updated on the 8th 
      December. At UK-level, surveillance of any complications of COVID-19 among neonates is being undertaken ",
      "through the ", tags$a(href="https://www.rcpch.ac.uk/work-we-do/bpsu/study-neonatal-complications-coronavirus-disease-covid-19",
                             "British Paediatric Surveillance Unit (external website)", target="_blank"), "."),
    p("Whilst COVID-19 does not appear to have played a role in the tragic deaths which occurred in 
      September 2021, there is international evidence which shows that COVID-19 infection during pregnancy 
      is associated with a higher chance of problems for both mother and baby. The Royal College of 
      Obstetricians and Gynaecologists (RCOG) maintains a review of the literature on COVID-19 in pregnancy, 
      with an update published in ", 
      tags$a(href="https://www.rcog.org.uk/en/guidelines-research-services/guidelines/coronavirus-pregnancy/", 
             "November 2021 (external website)", target="_blank"), ".",
      "COVID-19 infection during pregnancy has been linked to an increased risk of stillbirth. Symptomatic 
      COVID-19 is associated with an increased likelihood of premature birth due to a need to deliver the 
      baby early for the health of mother or baby. "),
    p("In view of the small but important risks of COVID-19 infection in pregnancy, pregnant women are 
      encouraged to take up the offer of COVID-19 vaccination. Information on this is available from the ", 
      tags$a(href="https://www.rcog.org.uk/globalassets/documents/guidelines/2021-02-24-combined-info-sheet-and-decision-aid.pdf",
             "RCOG (external website)", target="_blank"), ", and from ",
      tags$a(href="https://www.nhsinform.scot/covid-19-vaccine/the-vaccines/pregnancy-breastfeeding-and-the-coronavirus-vaccine",
             "NHS Inform (external website)", target="_blank"), ". There is good evidence that it is 
      effective at preventing severe COVID-19 illness. In Scotland, in",
      tags$a(href="https://publichealthscotland.scot/publications/covid-19-statistical-report/covid-19-statistical-report-3-november-2021/",
             "data available", target="_blank"), "to the end of September 2021, 99 women had been 
      admitted to critical care within 21 days of testing positive for COVID-19 during pregnancy, 
      of whom 98 were unvaccinated. Vaccine safety monitoring takes place within ",
      tags$a(href="https://publichealthscotland.scot/media/8413/covid-19-vaccine-surveillance-report-oct21-english.pdf",
             "Scotland", target="_blank"),
      " and internationally, with more than 200,000 women having received the vaccine during pregnancy 
      across the UK and US, with no concerning safety signals (see ", 
      tags$a(href="https://www.rcog.org.uk/en/guidelines-research-services/guidelines/coronavirus-pregnancy/",
             "Coronavirus (COVID-19) Infection in Pregnancy report (external website)", target="_blank"), ").",
      tags$a(href="https://www.gov.uk/government/news/new-ukhsa-study-provides-more-safety-data-on-covid-19-vaccines-in-pregnancy",
             "Recently published data (external website)", target="_blank"), " from England provides 
      further reassurance regarding birth outcomes among vaccinated women."),
    
    h3("3 November 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated 
      to include events that occurred in September 2021."),
    p("In September 2021 both the neonatal mortality rate (4.9 per 1,000 live births) and the 
      extended perinatal mortality rate (9.9 per 1,000 live and stillbirths) exceeded their upper 
      control limits of 4.3 and 9.4, respectively. Extended perinatal death rate is a measure which 
      combines stillbirths and neonatal deaths. Examining the data shows that the increase in extended 
      perinatal mortality reflects the higher than expected neonatal deaths, and stillbirths that were 
      at, but not lower than, their expected level. The overall infant mortality rate (5.5 per 1,000 
      live births) exceeded the warning limit (5.0), but not the upper control limit (5.9). This was 
      due to the high number of neonatal deaths. Post-neonatal deaths (those that occur after 4 weeks 
      of age) were not increased."),
    p("Each of these events is a tragedy for those involved. There are a number of existing processes 
      through which these events, in common with all neonatal deaths, will be reviewed. All child deaths 
      in Scotland are now reviewed to ensure that contributing factors are understood, and that learning 
      is used in ",
      tags$a(href="https://www.healthcareimprovementscotland.org/our_work/governance_and_assurance/deaths_of_children_reviews.aspx",
             "prevention and improving care quality (external website)", target="_blank"), ". ",
      "Systematic information on deaths occurring in neonatal units is gathered via the ",
      tags$a(href="https://www.npeu.ox.ac.uk/pmrt", "Perinatal Mortality Review Tool (external website)"),
      " and the UK-wide collaboration, MBRRACE-UK, provides surveillance and investigation of maternal 
      deaths, stillbirths and infant deaths. ",
      tags$a(href="https://www.gov.scot/publications/maternity-neonatal-perinatal-adverse-event-review-process-scotland/", "A standardised approach (external website)"),
      "to review of perinatal adverse events has also recently been adopted in Scotland"),
    p("As the overall number of deaths occurring each month is fortunately small, mortality rates tend 
      to fluctuate from month to month just by chance. Control charts are a tool that help tell the 
      difference between expected chance variation and changes which warrant further investigation. 
      Exceeding the upper control limit indicates there is a higher likelihood that there are factors 
      beyond random variation that may have contributed to the number of deaths that occurred. In view 
      of this, in addition to the processes outlined above, Public Health Scotland is working with the ",
      tags$a(href="https://www.perinatalnetwork.scot/neonatal/" , 
             "Scottish National Neonatal Network (external website)"), ", the ",
      tags$a(href="https://ihub.scot/improvement-programmes/scottish-patient-safety-programme-spsp/spsp-programmes-of-work/maternity-and-children-quality-improvement-collaborative-mcqic/" , "Maternity and Children Quality Improvement Collaborative (external website)"),
      " and the Scottish Government to understand any possible contributing factors to the most recent 
      infant mortality patterns, and to incorporate findings into existing prevention and improvement 
      work. Further information on the results of this work will be provided in future commentary."),
    
    h3("2 June 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include 
      events that occurred in April 2021. The rate of stillbirths, and all reported infant death measures, 
      remained within the warning threshold limits this month. The stillbirth rate in April 2021 was 
      2.4 per 1,000 total births (baseline, pre-pandemic average 3.8 per 1,000 total births), the neonatal 
      death rate was 2.7 per 1,000 live births (average 2.2 per 1,000 live births), and the infant mortality 
      rate was 3.5 per 1,000 live births (average 3.3 per 1,000 live births). "),
    
    h3("5 May 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include 
      events that occurred in March 2021. The rate of stillbirths, and all reported infant death measures, 
      remained within the warning threshold limits this month. The stillbirth rate in March 2021 was 
      4.5 per 1,000 total births (average 3.8 per 1,000 total births), and the infant mortality rate was 
      3.3 per 1,000 live births (average 3.3 per 1,000 live births). "),
    p("All the stillbirth and infant death data have been revised in this latest release. Originally we 
      reported these events from January 2017, and this has now been changed to July 2017. Also, a fixed 
      centreline (average) has been recalculated for every chart using the data for the months July 2017 
      to December 2019.  The dotted centreline continues that average through the more recent time period 
      to allow determination of whether the values seen in these months are unexpectedly low or high. 
      The use of a fixed centreline increases sensitivity of detection of signals in more recent data, 
      since recent observations within the pandemic period do not contribute to this reference centreline."),
    
    h3("7 April 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to 
      include events that occurred in February 2021. The rate of stillbirths, and all reported infant 
      death measures, remained within the warning threshold limits this month. The stillbirth rate in 
      February 2021 was 4.3 per 1,000 total births, and infant mortality rate was 3.2 per 1,000 live births."),
    p("Further background information is available within the commentary for July 2020."),
    
    h3("3 March 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to include 
      events that occurred in January 2021. The rate of stillbirths, and all reported infant death measures, 
      remained within the warning threshold limits this month. The stillbirth rate in January 2021 was 
      4.9 per 1,000 total births, and infant mortality rate was 3.6 per 1,000 live births. "),
    p("Last month we reported an issue involving a small number of infant deaths which were not included 
      in the data files sent from NRS to PHS (affecting less than 3% of infant deaths). We now believe 
      that this discrepancy has been resolved and any data affected have been retrospectively updated 
      on the dashboard. "),
    
    h3("3 February 2021"),
    p("In this release of information on stillbirths and infant deaths, data have been updated to 
      include events that occurred in December 2020. In the intervening months since previous reporting 
      on deaths up to October 2020, the rate of stillbirths and infant deaths remained within the warning 
      threshold limits. The stillbirth rate in December 2020 was 3.5 per 1,000 total births, the lowest 
      rate since August 2020 (2.4 per 1,000 total births)."),
    p("Presenting rates for post-neonatal deaths (PNND) has been changed from a P chart to a U chart. 
      Both types of chart are a means of identifying any important changes in the data (see the ‘How 
      do we identify patterns in the data?’ box for more information). Changing to a U chart brings 
      the reporting of the PNNDs into line with that for infant deaths. Neither the rates nor the control 
      and warning limits are materially affected by this change. "),
    p("The data for these indicators are sourced from NRS, however, a recent issue has come to light 
      whereby a small number of infant deaths are not included in the data files sent from NRS to PHS. 
      The numbers involved are thought to be very small (less than 3% of infant deaths) and affect data 
      since July 2020.  Any impact on the overall rates included in the dashboard are likely to be 
      minimal. We are working with NRS to resolve this discrepancy as soon as possible. Once resolved 
      data relating to these deaths will be included retrospectively on the dashboard."),
    
    h3("2 December 2020"),
    p("In this release of information on stillbirths and infant deaths (2 Dec 2020), data have been 
      updated to include events that occurred in October 2020. Last month it was noted that the 
      rate of post-neonatal deaths in September breached the warning limit (though not the control 
      limit). Continued monitoring shows that this rate has returned to a lower level in October, 
      at 1.4 per 1,000 live births. Whilst each of these events is a tragedy for those involved, 
      in October the numbers remained small, and all stillbirth and infant death measures were 
      within the warning limits."),
    
    h3("4 November 2020"),
    p("In this release of information on stillbirths and infant deaths (4 Nov 2020) data have 
      been updated to include events that occurred in September 2020. In September the rate of 
      stillbirths, neonatal deaths and extended perinatal deaths remained within control limits. 
      Post-neonatal deaths are those which occur after 4 weeks of age, but within the first year 
      of life. In September the rate of post-neonatal deaths was 2.2 per 1,000 live births. This 
      is above the warning limit of 2.1 per 1,000, but below the control limit of 2.6 per 1,000. 
      These thresholds are shown on the control charts, and are used to help differentiate between 
      expected random variation and substantial changes which warrant further investigation. 
      The overall infant mortality rate, which includes all deaths of children aged under 1 year 
      (both below and above 4 weeks of age), remained within the warning limit. This pattern suggests 
      that the higher rate of post-neonatal deaths in September reflects random variation in what is 
      a tragic, but fortunately rare event. Monthly monitoring of these data will continue."),
    
    h3("7 October 2020"),
    p("In this release of information on stillbirths and infant deaths (7 Oct 2020), data have 
      been updated to include events that occurred in August 2020.", br(),
      "Last month it was noted that the rate of stillbirths in July breached the warning limit 
      (though not the control limit). Continued monitoring shows that this rate has returned to 
      a lower level in August, at 2.4 per 1,000 total births.", br(),
      "Neonatal, post-neonatal and infant deaths have remained within the expected range, and 
      were relatively low in August 2020."),
    
    h3("2 September 2020"),
    p("In this release of information on stillbirths and infant deaths (2 Sept 2020) data have 
      been updated to include events that occurred in July 2020.", br(),
      "In July the rate of stillbirths was 6.0 per 1,000 total births, which was higher than 
      the warning limit of 5.8, but below the control limit of 6.7. Whilst each event is important 
      and a tragedy for those involved, the numbers of stillbirths are small overall, and therefore 
      rates fluctuate from month to month just by chance. Control charts are used to help differentiate 
      between expected random variation and substantial changes which warrant further investigation. 
      The stillbirth rate in July was within the control limit, indicating that this observation is 
      within the range of expected random variation.", br(),
      "As there is a relationship between the rate of stillbirth and neonatal mortality, the extended 
      perinatal mortality rate is used to present a combined figure for these two measures.
      In July 2020, this was 7.4 per 1,000 total births, which is below the warning limit. Monthly 
      monitoring of these data will continue.", br(),
      "Post-neonatal and infant deaths have remained within the expected range, and were relatively 
      low in July 2020."),
    
    h3("5 August 2020"),
    p("In this second release of information on stillbirths and infant deaths (5 Aug 2020) data have 
      been updated to include June 2020. Whilst each of these events is a tragedy for those involved, 
      in June the numbers remained small, and all stillbirth and infant death measures were within 
      the warning limits.", br(),
      "Rates of stillbirths and extended perinatal deaths approached, but did not breach, the upper 
      warning limit in May 2020. However, rates for both fell below the average in June 2020.", br(),
      "Further background information on the data sources used to monitor stillbirths and infant 
      death rates, and how to interpret the control charts, is provided in the commentary for 
      1 July 2020 below."),
    
    h3("1 July 2020"),
    h4("Background"),
    p("It is important to monitor the levels of stillbirth and infant mortality during the COVID-19 
    pandemic, as they may be influenced by maternal health and well-being, by how maternity and 
    child health services are provided, and by how people seek and interact with care. NHS Scotland and 
    Scottish Government",
      tags$a(href="https://www.nhsinform.scot/illnesses-and-conditions/infections-and-poisoning/coronavirus-covid-19/parents-and-families/coronavirus-covid-19-pregnancy-and-newborn-babies",
             "have produced guidelines (external website)", target="_blank"), 
      "for attending antenatal and postnatal care appointments during the pandemic."), br(),
    p("The tool shows monthly data for stillbirths and infant deaths (those occurring under the 
      age of one year). These are based on data from National Records for Scotland (NRS), and are 
      presented as rates per 1,000 live births for neonatal, post-neonatal and infant deaths and 
      per 1,000 total (live and still) births for stillbirths and extended perinatal deaths."),
    h4("Control Charts"),
    p("Control charts have been used to support interpretation of these data. As numbers of deaths 
      are relatively low, mortality rates tend to fluctuate from month to month just by chance: 
      control charts help differentiate between expected random variation and changes which warrant 
      further investigation."), br(),
    p("In this first release of information on stillbirths and infant deaths (1 July 2020), data are 
      shown for January 2017 to May 2020, with the most recent three months (March-May 2020) being 
      those when health and health services may have been affected by COVID-19. In this period the 
      only observations which have reached a ‘warning limit’ as indicated by the relevant control 
      chart were neonatal deaths in March 2020, where the rate was just above the upper warning limit 
      (3.7/1,000 compared to the UWL of 3.6/1,000), but did not breach the upper control limit (the 
      trigger for further investigation). In April and May there were fewer neonatal deaths, and the 
      rate fell to below the upper warning limit. Rates of stillbirths and extended perinatal deaths 
      are being closely monitored, as these approached, but did not breach, the upper warning limit 
      in May 2020."),
    h4("Data Sources"),
    p("NRS", tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2019",
                    "publishes (external website)", target="_blank"), 
      "information on stillbirths and infant deaths registered in Scotland."), 
    p("Across the UK, surveillance of perinatal deaths is undertaken by MBRRACE-UK (Mothers and 
      Babies: Reducing Risk through Audits and Confidential Enquiries across the UK). The latest 
      MBRRACE-UK perinatal mortality",
      tags$a(href="https://www.npeu.ox.ac.uk/mbrrace-uk#mbrrace-uk-perinatal-mortality-surveillance-report", 
             "report (external website)",  target="_blank"),
      "(providing information on babies born in 2017) provides background information on factors 
      that influence perinatal deaths."),
    p("Within Scotland, the", 
      tags$a(href="https://ihub.scot/improvement-programmes/scottish-patient-safety-programme-spsp/maternity-and-children-quality-improvement-collaborative-mcqic/",
      "Maternal and Children Quality Improvement Collaborative (MCQIC) (external website)",  target="_blank"), 
      "focuses on care quality to improve outcomes for babies, children and their mothers. One of the key 
      outcomes they track is stillbirths."),
    br()
    )
    

##END