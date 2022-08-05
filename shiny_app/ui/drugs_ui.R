# Wider impacts dashboard - Substance use tab
# UI code

drugs_tab <- 
  tabPanel(title = "Substance use", icon = icon("tablets"), value = "drugs",
         wellPanel(
           column(4, div(title="Select the data you want to explore", # tooltip
                         radioGroupButtons("drug_subcategories",
                                           label= "Step 1 – Select the data you want to explore",
                                           choices = c('Take home naloxone kits',
                                                       'Scottish Ambulance Service naloxone administration'= 'SAS naloxone administration',
                                                       'Drug and alcohol treatment referrals',
                                                       'Opioid substitution therapy prescribing'='OST prescribing',
                                                       'A&E attendances for drug overdose/intoxication'),
                                           status = "primary",
                                           direction = "vertical", justified = T))),
           column(4,uiOutput('area_drugs_select'),
                  uiOutput("geoname_ui_drugs")),
           
           column(4, uiOutput("types")),
           column(4,downloadButton('download_drugs_data', 'Download data'),
                  actionButton('jump_commentary_drugs','Go to commentary'),
                  fluidRow(br()),
                  actionButton("btn_drugs_modal", "Data source and definitions",
                               icon = icon('question-circle'))
           )
         ),#wellPanel bracket
         
         mainPanel(width = 12,
                   #actionButton('browser','browser'),
                   p('Last updated: 29 June 2022'),
                   fluidRow(br()),
                   uiOutput('Quan_plot'),
                   fluidRow(br()),
                   uiOutput('TwoYrComparison'),
                   fluidRow(br()),
                   fluidRow(br()),
                   uiOutput('Cum_plot'),
                   fluidRow(br()),
                   fluidRow(br()),
                   uiOutput('Prop_barplot'),
                   uiOutput('PercentChange'),
                   uiOutput('drug_AE_explorer'),
                   fluidRow(br())
                   
         )# mainPanel bracket
         
) # tabpanel bracket


###############################################.
# Drugs commentary ----
###############################################.
drug_commentary <-
  tagList(
    fluidRow(
      column(8, h2("Substance use")), 
      column(4, div(bsButton("jump_to_drugs", label = "Go to data"), style="float:right"))),  #this button can only be used once
    
    h3('Take home naloxone kits'),
    tags$ul(
      tags$li('Since January 2020 the monthly number of THN kits supplied in Scotland has remained 
              consistently higher than the 2018 & 2019 average. '),
      tags$li("Overall THN supply in Scotland showed peaks between March and May 2020 and around 
              December 2020 and January 2021. Most NHS Boards showed either one or both of these 
              increases."),
      tags$li("Supply by community outlets was the most common source of THN, although this varied 
              between NHS Boards and source distribution has varied over the time period presented 
              here. For example, NHS Dumfries & Galloway showed a large increase in community pharmacy 
              supply between March and May 2020 (25% of all this boards' supply in March, 57% in April, 
              and 35% in May, compared with around 12% or lower throughout the rest of time period from 
              January 2020 to June 2021). ")),
    
    h4("Community"),
    tags$ul(
      tags$li("The trend in community outlet supplies per month shows an exceptionally large number 
              of THN kits supplied in April and May 2020 during the initial response to the COVID-19 
              pandemic (from 961 in March to 2,546 in April and 1,675 in May). Large-scale distributions 
              by NHS Fife and NHS Ayrshire and Arran accounted for the notable increases in supplies 
              observed in April and May 2020 respectively. "),
      tags$li("Monthly supply numbers in 2020 were broadly the same as the combined 2018 and 2019 average 
              until January 2021.
              The two minor exceptions to this were smaller peaks in September and December 2020. 
              Although the December 2020 figure was lower than that of the 2018 & 2019 average, the 
              regular peak observed at that time of year increase suggests a seasonal increase in 
              THN supply."),
      tags$li("From January 2021 to September 2021, the number of THN kits provided have been 
              consistently higher than the 2018 & 2019 average, with a notable increase observed from 
              August 2021 onwards. This is a result of a new Scottish Government campaign to increase 
              the supply of THN kits to members of the public. ")),
    
    h4("Pharmacy"),
    tags$ul(
      tags$li("The number of THN kits dispensed by pharmacies on the basis of a community prescription 
              was consistently higher from March 2020 to September 2021 than the average for 2018 & 2019. 
              In particular, two large peaks in supply were observed in April 2020 and December 2020.")),
    
    h4("Prisons"),
    tags$ul(
      tags$li("The number of THN supplies issued by prisons per month was consistently higher for the 
              period February 2020 to September 2021 than the corresponding 2018 & 2019 averages. 
              The exception to this was November 2020 when supplies were 21% lower than the 2018 & 
              2019 average for the same month. "),
      tags$li("The large peak in May 2020 may partially have been a result of the Scottish Prison 
              Service\'s ",
              tags$a(href="https://www.sps.gov.uk/Corporate/Information/covid19/covid-19-information-hub.aspx", 
                     "COVID Early Release scheme (external website) ",  target="_blank"),
              "in which the Coronavirus (Scotland) Act 2020 provided new powers for the early release 
              of a specific class of prisoners held in Scottish prisons. Early release was deemed 
              necessary in order to provide the Scottish Prison Service with additional operational 
              capacity including allowing for a greater use of single cell occupancy, keeping prison 
              staff and the people in their care safe. It is understood that that scheme is no longer 
              operational, so subsequent increases may reflect other factors.")),
    
    h3("Drug and alcohol treatment referrals"),
    tags$ul(
      tags$li('The number of specialist drug and alcohol treatment referrals in January and February 
              2020 was broadly comparable to the 2018 and 2019 average for the corresponding weeks. 
              Subsequently, a 63% decrease in referrals was observed from week beginning 9 March 2020 
              (1,156 referrals) to week beginning 23 March 2020 (424 referrals).'),
      tags$li('Since the UK lockdown was implemented on 23 March 2020, drug and alcohol treatment 
              referral numbers have been consistently lower than in the comparable period in 2018 and 
              2019. From April 2020, a gradual increase has been observed, rising to a broadly stable 
              average of just below 1,000 referrals per week between August and December 2020.  A 
              seasonal decrease in treatment referrals occurred in late November and December 2020 
              broadly comparable with the decreases observed in previous years.'),
      tags$li('During 2021, weekly drug and alcohol referral numbers remained at a similar level seen 
              in the latter half of 2020, at just below 1,000 referrals per week, approximately 20% 
              lower than the 2018 and 2019 weekly average.'),
      tags$li('A similar pattern was seen for both drug and alcohol referrals. In the latter half of 
              2021, the number of drug referrals fall below the 2020 levels for the corresponding weeks. 
              However, combined with the co-dependency referrals, the combined number of referrals are 
              broadly similar to the 2020 drug referral levels.  This apparent fall in drug referrals 
              is possibly an artifact of the introduction of DAISy (the new data system) and the new 
              co-dependency category.'),
      tags$li('In the first three months of 2022 the number of drug and alcohol referrals was lower 
              than observed in the corresponding months of 2020 and 2021, and at just above 900 referrals 
              per week, was approximately 25% lower than the 2018 and 2019 weekly average.'),
      tags$li('The Scotland trends described were observed across many NHS Boards and Alcohol and Drug 
              Partnerships, although there will have been some variation between areas.')
      ),
    
    h3("SAS naloxone administration"),
    tags$ul(
      tags$li("The trends for SAS naloxone administration in 2020 and 2021 are generally in line with 
              the trend seen in the average of 2018 and 2019. The 3-week average in both 2020 and 2021 
              data and the historic average show considerable variation over time. "),
      tags$li("From January 2020 to the beginning of June 2020 the number of SAS naloxone incidents 
              were roughly similar those seen on average in 2018 and 2019. The biggest difference 
              between the two trend lines can be seen at the end of June to beginning of July where 
              the historic average line peaked at 131 Naloxone incidents compared with 95 incidents 
              in 2020. "),
      tags$li('Following this difference, from August 2020 there was a decreasing trend in the number 
              of SAS naloxone incidents followed by an increase from January 2021. This increase in 
              the number of SAS naloxone incidents reaches a peak of 127 at the beginning of July 2021, 
              followed by a small decrease to around 110 incidents in August 2021. This trend, beginning 
              in January 2021, closely follows the trend seen on average in 2018 and 2019.')),
    
    h3("OST prescribing"),
    h4("Methadone"),
    p('The total quantity of methadone prescribed has remained fairly consistent since January 2018, 
      at around 40 million mg per month. This suggests that the changes in methadone prescribing observed 
      during the COVID-19 pandemic did not influence the total quantity of methadone prescribed. However, 
      there has been variation in how this has been prescribed. '),
    tags$ul(
      tags$li('From January to March 2020 the number of prescribed methadone items per month was similar 
              to the number of items prescribed in the same period for the 2018-2019 average. After a 
              large decrease from March 2020 to May 2020, the number of prescribed methadone items has 
              remained constantly below the 2018-2019 average.  '),
      tags$li('The quantity of methadone prescribed per item from January to March 2020 was similar to 
              the 2018-2019 average, but following a large increase in March 2020 has remained higher 
              than the historic average.')
    ),
    h4("Buprenorphine"),
    p('There was a clear increase in the total quantity of buprenorphine prescribed, from 1.2 million 
      mg per month in January 2018 to 1.6 million mg per month in July 2020. '),
    tags$ul(
      tags$li('From January to April 2020, the number of items of buprenorphine prescribed per month 
              was higher than the 2018-2019 average. After peaking in March 2020 the number of items 
              prescribed per month decreased and was similar to the 2018-2019 average in the period 
              from May to October 2020. Since November 2020, the number of items prescribed per month 
              has remained above the 2018-2019 average. '),
      tags$li('The quantity of buprenorphine prescribed per item in 2020 and 2021 was higher than the 
              2018-2019 average across the whole time series, in particular from April 2020 onwards. ')
    ),
    
    h3("A&E attendances for drug overdose/intoxication"),
    h4("2020"),
    tags$ul(
      tags$li('There was a large decrease in the number of drug-related overdose/intoxication attendances 
              at Emergency Departments in Scotland in the weeks immediately prior to the UK lockdown.'), 
      tags$li('Following the introduction of the UK lockdown, attendances increased throughout Spring and 
              Summer 2020.'),
      tags$li('This was followed by a decreasing trend of attendances from September to the end of 2020. ')),
    h4("2021"),
    tags$ul(
      tags$li('Between January and August 2021, a long-term increasing trend in number of drug-related 
              attendances was observed.'), 
      tags$li('In September, October and November 2021, the weekly average numbers of drug-related ED 
              attendances decreased.')),
    h4("2022"),
    tags$ul(
      tags$li("Between December 2021 and Feburary 2022 the numbers of drug-related attendances fell 
              below the 2018 & 2019 average and remain lower than observed in the corresponding months 
              of 2020 and 2021."),
      tags$li("In March 2022 number of attendences for drug overdoses or intoxications fell to their 
              lowest point since the start of the 2020 Lockdown, but increased steadily in April 2022, 
              reaching the historic average trend."),
      tags$li("For further information, contact ", 
          tags$b(tags$a(href="mailto:phs.drugsteam@phs.scot", "phs.drugsteam@phs.scot",  target="_blank")),'.')),
    br()
      )

#END