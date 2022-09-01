# Wider impacts dashboard - Injuries tab
# UI code

injuries_tab <- 
  tabPanel(title = "Injuries", icon = icon("user-injured"), value = "injuries",
           wellPanel(#actionButton("browser", "browser"),
             column(4, selectdata_ui("injury", measure_choices = injury_data_list)),
             column(4,  selectgeo_ui("injury", area_choices =  c("Scotland", "Health board", "HSC partnership"))),
             column(4,  selectInput("type_select", label = "Step 3. Select type of split",
                                    choices = injury_split_list, selected="Age group")),
             column(4, sourcemodal_ui("injury"),
                    fluidRow(br()),
                    downloadButton("download_injuries_data", "Download data"),
                    fluidRow(br()),
                    actionButton('jump_commentary_injuries','Go to commentary'))
           ), #well panel
           mainPanel(width = 12,
                     uiOutput("injuries_explorer")
           )# mainPanel bracket
  ) # tabpanel bracket


###############################################.
## Commentary ----
###############################################.
injuries_commentary <- 
  tagList(
    fluidRow(
      column(8, h2("Injuries")), 
      column(4, div(bsButton("jump_to_injuries", label = "Go to data"), style="float:right"))),  #this button can only be used once
          h3("Unintentional injuries and assaults - extracted 31 January 2022"), 
          h4("Background"),
          p("The response to the COVID-19 pandemic has had the potential to influence both the occurrence
            of unintentional injuries and assaults, and how people receive care after such events. Within
            this dashboard information is presented on the number of such events resulting in an admission
            to hospital each month from January 2020, along with data from 2018-2018 for comparison. Data
            on unintentional injuries is split by type (road traffic accidents, poisonings, falls and other),
            and all data can be explored by age, sex, deprivation (SIMD) and location of event, as well as at
            Scotland, Health Board and Health and Social Care Partnership (HSCP) level."),
          p("The data shown here include events where it is not possible to determine intent from the hospital
            records, but do not include those that were documented to be self-harm. Many unintentional injuries
            result do not result in hospital admission, but are treated by the individual, GPs, at Accident and
            Emergency departments or by a child's parent or carer, and are therefore not represented in this
            information. Changes in the number of admissions for injury do not necessarily mean changes in the
            number of injury events, but may also reflect changes in how injuries are cared for, for example,
            more people may seek to treat themselves, or may have been less likely to be admitted for less severe
            injuries at particular times."),
          p("Description of key findings for months in the period January 2020 to June 2021, compared with
            average admissions for the same months in 2018-2019"),
          p("There was a substantial fall in the number of admissions for unintentional injury in April 2020, with around
            one-third fewer admissions compared with the average for the same period in 2018-2019. In the subsequent months,
            numbers gradually increased, and by August 2020 were at a similar level to previous years. There was a further,
            smaller fall in admissions in November 2020, but since then the overall number has been similar to that seen in 2018-19."),
          p("The most substantial reduction in April 2020 was seen in children and young people aged 5-24 years, among whom admissions 
            were between a half and two-thirds lower than previous years. This reduction was particularly notable in admissions for falls,
            which constitute the largest proportion of unintentional injuries. Among adults there was a notable increase in admissions in
            January 2021, which was contributed to largely by an increase in the number of admissions for falls among those aged 25-64 years."),
          p("Admissions due to injury following a road-traffic accident have been lower each month from April 2020 to May 2021, compared to 2018-2019,
            with the exceptions of August 2020 and March 2021, when levels were similar. The number of admissions for poisoning has been similar to
            previous years throughout this period, with the exception of June 2020, when they were 18% higher. In that month the largest number of
            admissions was among those aged 25-64 years, although the largest proportional increase on previous years was seen in those aged 5-11 years.
            The number of admissions for assault was lower than in previous years in March to May 2020, and again from October 2020 to February 2021,
            at other points the number were similar." ),
          p("Across all unintentional injuries and assaults, the largest number of admissions were among people living in the most deprived fifth of areas in Scotland,
            according to the Scottish Index of Multiple Deprivation (SIMD 1). However, the percentage change in admissions compared with previous years was similar across deprivation groups,
            or a mixed picture, with the exception of the increase in falls observed in January 2021, when the largest increase (52%) was seen in those living in the least deprived areas."),
          p("The overall number of admissions per month was similar between men and women up to November 2020. The fall in admissions in April 2020 was more substantial among men (-39%) than women (-26%).
            From December 2020 there has been a larger number of admissions among women, and these have been similar to or higher than previous years, most notably in January 2021, when they were 19% higher
            among women, which was largely attributable to an increase in falls. For men there was a change in the number of admissions for poisoning in May and June 2020, when they were around 20% higher
            than previous years."),
          p("Across the period from May 2020 to June 2021 there was around a 10% to 25% increase in admissions due to injuries occurring in the home, and a reduction in admissions for injuries in other
            and undisclosed locations, compared with 2018-2019. "),
    br()
          )


##END