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
  # Cardiovascular tab
  source(file.path("cardio_tab.R"),  local = TRUE)$value
  
  ###############################################.
  # Immunisation tab
  source(file.path("immunisation_tab.R"),  local = TRUE)$value
  
  ###############################################.
  # Child Health tab
  source(file.path("child_health_tab.R"),  local = TRUE)$value
 
  ###############################################.
  # Perinatal tab
  source(file.path("perinatal_tab.R"),  local = TRUE)$value

  ###############################################.
  # Data tab
  source(file.path("data_tab.R"),  local = TRUE)$value
  
  ###############################################.

## Observe events to improve navigation between tabs of the app

# To jump to data pages    
  observeEvent(input$jump_to_summary, {updateTabsetPanel(session, "intabset", selected = "summary")})
  
  observeEvent(input$jump_to_cardio, {updateTabsetPanel(session, "intabset", selected = "cardio")})
  
  observeEvent(input$jump_to_table, {updateTabsetPanel(session, "intabset", selected = "table")})

  observeEvent(input$jump_to_immunisation, {updateTabsetPanel(session, "intabset", selected = "child")})
  
  observeEvent(input$jump_to_childhealth, {updateTabsetPanel(session, "intabset", selected = "child_health")})  
  
  observeEvent(input$jump_to_perinatal_mortality, {updateTabsetPanel(session, "intabset", selected = "perinatal_mortality")})
  
# To jump to commentary tab - requires multiple lines becuase action buttons must have unique ID
  observeEvent(input$jump_commentary_child, {updateTabsetPanel(session, "intabset", selected = "comment")})  
  observeEvent(input$jump_commentary_hv, {updateTabsetPanel(session, "intabset", selected = "comment")})  
  observeEvent(input$jump_commentary_cardio, {updateTabsetPanel(session, "intabset", selected = "comment")})  
  observeEvent(input$jump_commentary_summary, {updateTabsetPanel(session, "intabset", selected = "comment")})  
  observeEvent(input$jump_commentary_perinatal, {updateTabsetPanel(session, "intabset", selected = "comment")})
  
#trying to find way to link multiple action buttons to one observeEvent - sort of works but then creates an loop with undesired effect
# observe({
#     input_btn <- paste0("jump_commentary_", input$intabset)
#     lapply(input_btn,
#            function(x){
#              observeEvent(
#                input[[x]],
#                {updateTabsetPanel(session, "intabset", selected = "comment")}
#              )}
#     )})

## ObserveEvents to open collapsepanels in commentary tab when sidepanel option clicked
  observeEvent(input$summary_button, ({
    updateCollapse(session, "collapse_commentary", open = "Key trends")}))
  
  observeEvent(input$immunisation_button, ({
    updateCollapse(session, "collapse_commentary", open = "Immunisation")}))
  
  observeEvent(input$ch_review_button, ({
    updateCollapse(session, "collapse_commentary", open = "Child health")}))
  
  observeEvent(input$cardio_button, ({
    updateCollapse(session, "collapse_commentary", open = "Cardiovascular")}))
  
  observeEvent(input$perinatal_button, ({
    updateCollapse(session, "collapse_commentary", open = "Stillbirths and infant deaths")}))
  
} # server end