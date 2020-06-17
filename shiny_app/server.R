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
  ## Immunisation tab
  source(file.path("immunisation_tab.R"),  local = TRUE)$value
  
  ###############################################.
  ## Child Health tab
  source(file.path("child_health_tab.R"),  local = TRUE)$value
  
  ###############################################.
  ## Perinatal tab
  # source(file.path("perinatal_tab.R"),  local = TRUE)$value
  
  ###############################################.
  ## Data tab
  source(file.path("data_tab.R"),  local = TRUE)$value
  
  ###############################################.
  ## To move around tabs  
  observeEvent(input$jump_summary, {
    updateTabsetPanel(session, "intabset", selected = "summary")
  })
  
  observeEvent(input$jump_cardio, {
    updateTabsetPanel(session, "intabset", selected = "cardio")
  })
  
  observeEvent(input$jump_table, {
    updateTabsetPanel(session, "intabset", selected = "table")
  })
  
  
# Jump to commentary tab
  observeEvent(input$jump_commentary, {
    updateTabsetPanel(session, "intabset", selected = "comment")
  })

# Jump to child health IMMUNISATION tab  
  observeEvent(input$jump_immunisation, {
    updateTabsetPanel(session, "intabset", selected = "child")
  })
  
# Jump to child health review tab   
  observeEvent(input$jump_childhealth, {
    updateTabsetPanel(session, "intabset", selected = "child_health")
  })  
  
  
  
## Collapse panels used in commentary tab
  
  observeEvent(input$summary_button, ({
    updateCollapse(session, "collapse_commentary", open = "Key trends")
  }))
  observeEvent(input$immunisation_button, ({
    updateCollapse(session, "collapse_commentary", open = "Immunisation")
  }))
  observeEvent(input$ch_review_button, ({
    updateCollapse(session, "collapse_commentary", open = "Child health")
  }))
  observeEvent(input$cardio_button, ({
    updateCollapse(session, "collapse_commentary", open = "Cardiovascular")
  }))
  

  
  
  
} # server end