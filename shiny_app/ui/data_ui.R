data_tab <- 
  tabPanel(title = "Data", icon = icon("table"), value = "table",
         p("This section allows you to view the data in table format.
        You can use the filters to select the data you are interested in.
        You can also download the data as a csv file using the download button.
        Some of the data is also hosted in the",
           tags$a(href="https://www.opendata.nhs.scot/dataset?groups=covid-19",
                  "Scottish Health and Social Care Open Data portal (external website)",  target="_blank"), "."),
         column(6, selectInput("data_select", "Select the data you want to explore.",
                               choices = data_list_data_tab)),
         column(6, downloadButton('download_table_csv', 'Download data')),
         mainPanel(width = 12,
                   DT::dataTableOutput("table_filtered"))
) # tabpanel bracket
