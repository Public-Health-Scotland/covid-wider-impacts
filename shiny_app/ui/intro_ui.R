intro_tab <- 
  tabPanel("Home", icon = icon("info-circle"), value = "intro",
         
         fluidRow(column(9, h3("COVID-19 wider impacts on the health care system")),
                  column(3, actionButton("new_next", tags$b("New content and future updates"),
                                         icon = icon('calendar-alt')))),
         p("The COVID-19 pandemic has wider impacts on individuals’ health, and their use of healthcare services,
           than those that occur as the direct result of infection"),
         p("Reasons for this may include:"),
         tags$ul(
           tags$li("Individuals being reluctant to use health services because they do not want to burden
                   the NHS or are anxious about the risk of infection."),
           tags$li("The health service delaying preventative and non-urgent care such as some screening
                   services and planned surgery."),
           tags$li("Other indirect effects of interventions to control COVID-19, such as changes to employment and income, changes in access to education, social isolation, family violence and abuse, changes in the accessibility and use of food, alcohol, drugs and gambling, or changes in physical activity and transport pattern.")
           ),
         p("More detailed background information on these potential impacts is provided by the Scottish Public Health Observatory in a section on ",
           tags$a(href="https://www.scotpho.org.uk/comparative-health/coronavirus-covid-19/covid-19-wider-impacts/",
                  "Covid-19 wider impacts (external website)", class="externallink",target="_blank"),"."),
         p("This information tool provides an overview of changes in health and use of healthcare during the COVID-19
           pandemic in Scotland, drawing on a range of national data sources."),
         p("We are providing information on different topics as quickly as we can, given the different time lags
           that apply to different national data sources. For example, Public Health Scotland receives information
           on patients attending Accident & Emergency within days; but there can be a delay of at least six weeks
           before we receive detailed information on patients discharged from hospital after having a baby."),
         p("Depending on the topic being looked at, information will be shown for patients in different age groups;
           for males and females; and for people living in areas with different levels of material deprivation.
           Information will also be shown for different locations across Scotland, such as NHS Board areas."),
         p("This tool will be updated monthly. New releases will be published at the same time as the Public Health Scotland ",
           tags$a(href="https://publichealthscotland.scot/publications/covid-19-statistical-report/",
                  "COVID-19 report for Scotland.",  target="_blank")),
         p("Note that some numbers may not sum to the total as disclosure control methods have been applied
           to the data in order to protect patient confidentiality."),
         p("Pre-Release Access: under terms of the 'Pre-Release Access to Official Statistics (Scotland) Order 2008',
           PHS is obliged to publish information on those receiving Pre-Release Access ('Pre-Release Access' refers to
           statistics in their final form prior to publication). Shown below are details of those receiving standard
           Pre-Release Access. "),
         p("Standard Pre-Release Access:"),
         tags$ul(
           tags$li("Scottish Government Health Department"),
           tags$li("NHS Board Chief Executives"),
           tags$li("NHS Board Communication Leads")),
         p("If you have any questions relating to the data presented please contact us at: ",
           tags$b(tags$a(href="mailto:phs.statsgov@phs.scot", "phs.statsgov@phs.scot",  target="_blank")), "."),
         p("You can access the code used to produce this tool in this ",
           tags$a(href="https://github.com/Public-Health-Scotland/covid-wider-impacts", "GitHub repository (external website)",  target="_blank"), "."),
         h3("Other sources of information: "),
         tags$ul(
           tags$li("Public Health Scotland publishes ",
                   tags$a(href="https://publichealthscotland.scot/our-areas-of-work/sharing-our-data-and-intelligence/coronavirus-covid-19-data-and-guidance/", "information",  target="_blank"),
                   "on the direct health
                   impacts of COVID-19 as well as guidance for professionals and public."),
           tags$li("The Scottish Government publishes a ",
                   tags$a(href="https://data.gov.scot/coronavirus-covid-19/", "dashboard (external website)",  target="_blank"),
                   " which brings together data and
          evidence on the impacts of COVID-19 on health, society and the economy."),
           tags$li("The Improvement Service publishes a ",
                   tags$a(href="https://scotland.shinyapps.io/is-covid-economic-impact", "dashboard (external website)",  target="_blank"),
                   " on the economic impacts of the pandemic in Scotland."),
           tags$li("Public Health Scotland publishes ",
                   tags$a(href="https://publichealthscotland.scot/our-areas-of-work/covid-19/covid-19-data-and-intelligence/covid-19-and-children-research/",
                          "a series of reports",  target="_blank"),
                   " on the direct and wider impacts of the pandemic on children and young people."),
           tags$li("Transport Scotland publishes ",
                   tags$a(href="https://www.transport.gov.scot/publications/", "information (external website)",  target="_blank"),
                   " on transport trends and public attitudes towards
                   transport for the pandemic period.")
         )
         ) #tabPanel bracket