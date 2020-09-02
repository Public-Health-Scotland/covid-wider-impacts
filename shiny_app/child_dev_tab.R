##Server script for child development tab

###############################################.
## Modal ----
###############################################.

# Pop-up modal explaining source of data
observeEvent(input$btn_childdev_modal,
             showModal(modalDialog(#RAPID ADMISSIONS MODAL
               title = "What is the data source?",
               p("Data source: xxxx."),
               p("Placeholder"),
               size = "m",
               easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)"))))

###############################################.
##  Reactive layout  ----
###############################################.
# The charts and text shown on the app will depend on what the user wants to see
output$childdev_explorer <- renderUI({
  
  tagList(
    fluidRow(column(12, 
                    p("Placeholder for intro text if required"),
                    h4(paste0("Child development reviews at ", input$measure_select_childdev)))),
    fluidRow(withSpinner(plotlyOutput("childdev_chart")))
    )#tagLIst bracket
  
  }) #close perinatal_explorer function

###############################################.
## Charts ----
###############################################.



###############################################.
## Data downloads ----
###############################################.

output$download_childdev_data <- downloadHandler(
  filename ="stillbirth_infantdeaths_extract.csv",
  content = function(file) {
    write_csv(perinatal_down_data(),
              file) } 
)


###############################################.
## Commentary ----
###############################################.
output$childdev_commentary <- renderUI({
  tagList(
    bsButton("jump_to_childdev",label = "Go to data"), #this button can only be used once
    h2("Child development reviews -  2020"),
    p("Placeholder")
  ) #tagLIst bracket
})
