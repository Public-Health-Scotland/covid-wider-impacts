intro_tab <- 
  tabPanel("Home", icon = icon("info-circle"), value = "intro",
           
           sidebarLayout(
             sidebarPanel(width = 3,
                          radioGroupButtons("home_select",
                                            choices = home_list, status = "primary",
                                            direction = "vertical", justified = T)),
             
             mainPanel(width = 9,
                       # About
                       conditionalPanel(
                         condition= 'input.home_select == "about"',
                         tagList(h3(tags$b("COVID-19 wider impacts on the health care system")),
                                 p("The COVID-19 pandemic has wider impacts on individuals’ health, and their use of healthcare services,
                                   than those that occur as the direct result of infection."),
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
                                 p("Many of the indicators in this tool are updated monthly. Depending on the topic being looked at, information will be shown for patients in different age groups;
                                   for males and females; and for people living in areas with different levels of material deprivation.
                                   Information will also be shown for different locations across Scotland, such as NHS Board areas."), br(),
                                 
                                 p(tags$b("Pre-release access")),
                                 p("Under terms of the 'Pre-Release Access to Official Statistics (Scotland) Order 2008',
                                   PHS is obliged to publish information on those receiving Pre-Release Access ('Pre-Release Access' refers to
                                   statistics in their final form prior to publication). Shown below are details of those receiving standard
                                   Pre-Release Access. "),
                                 p("Standard pre-release access is provided to:"),
                                 tags$ul(
                                   tags$li("Scottish Government Health Department"),
                                   tags$li("NHS Board Chief Executives"),
                                   tags$li("NHS Board Communication Leads"))
                                 ) # tagList
                                 ), # conditionalPanel
                       
                       # Using the dashboard
                       conditionalPanel(
                         condition= 'input.home_select == "use"',
                         tagList(h3(tags$b("Using the dashboard")),
                                 p("There are tabs across the top for the each topic area within the dashboard - these range from Summary trends which give an
                                   overview of hospital activity to more focussed areas such as child health or mental health. The Commentary tab provides 
                                   relevant updates for each of the sections, for example if any new data has been added or there is some points of interest in the data
                                   to highlight."),
                                 p("Note that some numbers may not sum to the total as disclosure control methods have been applied
                                   to the data in order to protect patient confidentiality."), br(),
                                 
                                 p(tags$b("Interacting with the dashboard")),
                                 p(tags$li("On each tab there are menu boxes that allow the user to select the data they wish to explore and drop-down menus 
                                           to drill down into the data for a specific NHS Board or subgroup of interest.")), 
                                 p(tags$li("On the line charts,
                                           clicking on a category in the legend will remove it from the chart. This is useful to reduce the number of lines
                                           on the chart and makes them easier to see. A further click on the categories will add them back into the chart.")),
                                 p(tags$li("There are information buttons alongside some of the charts that contain further detail about the data source and definitions used, or provide guidance
                                           on how to interpret the data in the charts.")), br(),
                                 
                                 p(tags$b("Downloading data")),
                                 p(tags$li("There is the option to download data as a csv file on the Data tab. You can select the data you wish to
                                           download from the drop-down menu and use the filter boxes in the table header to further filter the
                                           data in the table. Then click 'Download data' to download the data in the table into a csv file.")),
                                 p(tags$li("Some of the data is also available on the ",
                                           tags$a(href="https://www.opendata.nhs.scot/dataset/hospital-standardised-mortality-ratios",
                                                  "Scottish Health and Social Care Open Data platform (external website).", target="_blank"))),
                                 p(tags$li("To download an image of any of the charts in the dashboard, click the camera icon in the top-right
                                           corner of the chart and a png image file will automatically download."))
                                 ) #tagList
                                 ), # condtionalPanel
                       
                       # Further information
                       conditionalPanel(
                         condition= 'input.home_select == "info"',
                         tagList(h3(tags$b("Further sources of information")),
                                 p(tags$li("You can access the code used to produce this tool in this ",
                                           tags$a(href="https://github.com/Public-Health-Scotland/covid-wider-impacts", "GitHub repository (external website)",  target="_blank"), ".")),
                                 
                                 p(tags$li("Public Health Scotland publishes ",
                                           tags$a(href="https://publichealthscotland.scot/our-areas-of-work/sharing-our-data-and-intelligence/coronavirus-covid-19-data-and-guidance/", "information",  target="_blank"),
                                           "on the direct health impacts of COVID-19 as well as guidance for professionals and public.")),
                                 p(tags$li("The Scottish Government publishes a ",
                                           tags$a(href="https://data.gov.scot/coronavirus-covid-19/", "dashboard (external website)",  target="_blank"),
                                           " which brings together data and evidence on the impacts of COVID-19 on health, society and the economy.")),
                                 p(tags$li("The Improvement Service publishes a ",
                                           tags$a(href="https://scotland.shinyapps.io/is-covid-economic-impact", "dashboard (external website)",  target="_blank"),
                                           " on the economic impacts of the pandemic in Scotland.")),
                                 p(tags$li("Public Health Scotland publishes ",
                                           tags$a(href="https://publichealthscotland.scot/our-areas-of-work/covid-19/covid-19-data-and-intelligence/covid-19-and-children-research/",
                                                  "a series of reports",  target="_blank"),
                                           " on the direct and wider impacts of the pandemic on children and young people.")),
                                 p(tags$li("Transport Scotland publishes ",
                                           tags$a(href="https://www.transport.gov.scot/publications/", "information (external website)",  target="_blank"),
                                           " on transport trends and public attitudes towards transport for the pandemic period.")), br(),
                                 p(tags$b("Contact us")),
                                 p("Please contact the ", tags$a(href="mailto:phs.qualityindicators@phs.scot", "Quality Indicators team"), 
                                   "if you have any questions about the data in this dashboard.")
                         ) # tagList
                       ), # conditionalPanel
                       
                       # Accessibility
                       conditionalPanel(
                         condition= 'input.home_select == "accessibility"',
                         tagList(h3(tags$b("Accessibility")),
                                 p("This website is run by ", tags$a(href="https://www.publichealthscotland.scot/",
                                                                     "Public Health Scotland", target="_blank"),
                                   ", Scotland's national organisation for public health. As a new organisation formed
                                   on 1 April 2020, Public Health Scotland is currently reviewing its web estate. Public
                                   Health Scotland is committed to making its website accessible, in accordance with
                                   the Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility
                                   Regulations 2018. This accessibility statement applies to the dashboard that accompanies
                                   the HSMR quarterly publication."),
                                 p(tags$a(href="https://mcmw.abilitynet.org.uk/", "AbilityNet (external website)", target="_blank"),
                                   " has advice on making your device easier to use if you have a disability."),
                                 
                                 p(tags$b("Compliance status")),
                                 p("This site has not yet been evaluated against Web Content Accessibility Guidelines
                                   version 2.1 level AA standard."),
                                 
                                 p(tags$b("Reporting any accessibility problems with this website")),
                                 p("If you wish to contact us about any accessibility issues you encounter on this
                                   site, please email ", tags$a(href="mailto:phs.qualityindicators@phs.scot", "phs.qualityindicators@phs.scot", ".")),
                                 
                                 p(tags$b("Enforcement procedure")),
                                 p("The Equality and Human Rights Commission (EHRC) is responsible for enforcing the
                                   Public Sector Bodies (Websites and Mobile Applications) (No. 2) Accessibility Regulations
                                   2018 (the ‘accessibility regulations’). If you’re not happy with how we respond to your complaint,",
                                   tags$a(href="https://www.equalityadvisoryservice.com/", "contact the Equality Advisory and Support Service (EASS) (external website).",
                                          target = "_blank")),
                                 
                                 p(tags$b("Preparation of this accessibility statement")),
                                 p("This statement was prepared on 15 June 2022. It was last reviewed on 15 June 2022.")
                                 ) # tagList
                         ) # conditonalPanel
                       ) # mainPanel
                       ) # sidebarLayout
         ) # tabPanel