#Server side

function(input, output, session) {
  
  # For debugging
  # observeEvent(input$browser, browser())
  
  ###############################################.
  ## Functions 
  # Sourcing file with functions code
  source(file.path("functions_server.R"),  local = TRUE)$value
  
  ###############################################.
  # Summary trends tab  
  source(file.path("summary_tab.R"),  local = TRUE)$value
  
  ###############################################.
  ## Cardiovascular tab
  source(file.path("cardio_tab.R"),  local = TRUE)$value
  
  ###############################################.
  ## Data tab
  source(file.path("data_tab.R"),  local = TRUE)$value
  
  ###############################################.
  ## To move around tabs
  
  observeEvent(input$jump_summary, {
    updateTabsetPanel(session, "intabset", selected = "summary")
  })
  
  observeEvent(input$jump_table, {
    updateTabsetPanel(session, "intabset", selected = "table")
  })
  
} # server end