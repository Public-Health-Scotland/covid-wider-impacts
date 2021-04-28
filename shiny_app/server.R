#Server side
 credentials <- readRDS("admin/credentials_ch.rds")

function(input, output, session) {
  
 # Shinymanager Auth

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  
  # For debugging
   # observeEvent(input$browser, browser())
  
  ##############################################.
  # New content and future updates ----
  ##############################################.
  observeEvent(input$new_next,
                 showModal(modalDialog(# Cardio A&E MODAL
                   title = "New content added and future updates",
                   h4("New content and updates"),
                   tags$ul(
                     tags$li("14th April - New sections added for births and babies data:
                             Apgar scores and location of birth for extremely preterm infants."),
                     tags$li("7th April - Monthly update of data (most sections)."),
                     tags$li("31st March - New data on outpatient appointments."),
                     tags$li("10th March - Update of cancer section data."),
                     tags$li("3rd March - Update of all data, except cancer section."),
                     tags$li("3rd February - Update of all data, except cancer section.")
                 ),
                   h4("Future updates"),
                   p("Please keep in mind that these dates are tentative and subject to change with short notice."),
                   tags$ul(
                     tags$li("14th April - New data on Apgar scores and location of birth for extremely preterm infants."),
                     tags$li("5th May - Monthly update of data (most sections)."),
                     tags$li("2nd June - New data on injury hospital admissions."),
                     tags$li("June 2021 - New data on self-harm hospital admissions.")
                   ),
                   size = "m",
                   easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")))
  )

  ##############################################.
  # Sourcing server files for each tab ----
  ###############################################.
  # Sourcing file with functions code
  source(file.path("functions_server.R"),  local = TRUE)$value

  ###############################################.
  # Summary trends tab
  source(file.path("summary_tab.R"),  local = TRUE)$value

  ##############################################.
  # Cardiovascular tab
  source(file.path("cardio_tab.R"),  local = TRUE)$value

  ###############################################.
  # Immunisation tab
  source(file.path("immunisation_tab.R"),  local = TRUE)$value

  ###############################################.
  # Child health reviews tab
  source(file.path("child_health_tab.R"),  local = TRUE)$value

  ###############################################.
  # Pregnancy tabs
  source(file.path("antenatal_booking_tab.R"),  local = TRUE)$value
  source(file.path("terminations_tab.R"),  local = TRUE)$value

  ###############################################.
  # Births and Babies tabs
  source(file.path("perinatal_tab.R"),  local = TRUE)$value
  source(file.path("apgar_tab.R"),  local = TRUE)$value
  source(file.path("preterm_tab.R"),  local = TRUE)$value
  source(file.path("mode_of_delivery_tab.R"),  local = TRUE)$value
  source(file.path("inductions_tab.R"),  local = TRUE)$value
  source(file.path("gestation_at_delivery_tab.R"),  local = TRUE)$value

  ###############################################.
  # Child development tab
  source(file.path("child_dev_tab.R"),  local = TRUE)$value

  ###############################################.
  # Breastfeeding tab
  source(file.path("breastfeeding_tab.R"),  local = TRUE)$value

  ###############################################.
  # Mental health tab
  source(file.path("mental_health_tab.R"),  local = TRUE)$value

  ###############################################.
  # Cancer tab
  source(file.path("cancer_tab.R"),  local = TRUE)$value
  
  ###############################################.
  # Data tab
  source(file.path("data_tab.R"),  local = TRUE)$value

##############################################.
# jump to data pages from commentary ----
##############################################.
  observeEvent(input$jump_to_summary, {updateTabsetPanel(session, "intabset", selected = "summary")})
  observeEvent(input$jump_to_cardio, {updateTabsetPanel(session, "intabset", selected = "cardio")})
  observeEvent(input$jump_to_table, {updateTabsetPanel(session, "intabset", selected = "table")})
  observeEvent(input$jump_to_immunisation, {updateTabsetPanel(session, "intabset", selected = "imm")})
  observeEvent(input$jump_to_childreview, {updateTabsetPanel(session, "intabset", selected = "child_health")})
  observeEvent(input$jump_to_perinatal_mortality, {updateTabsetPanel(session, "intabset", selected = "perinatal_mortality")})
  observeEvent(input$jump_to_booking, {updateTabsetPanel(session, "intabset", selected = "booking")})
  observeEvent(input$jump_to_top, {updateTabsetPanel(session, "intabset", selected = "terminations")})

  observeEvent(input$jump_to_mod, {updateTabsetPanel(session, "intabset", selected = "mod")})
  observeEvent(input$jump_to_induction, {updateTabsetPanel(session, "intabset", selected = "inductions")})
  observeEvent(input$jump_to_gestation, {updateTabsetPanel(session, "intabset", selected = "gestation")})
  observeEvent(input$jump_to_apgar, {updateTabsetPanel(session, "intabset", selected = "apgar")})
  observeEvent(input$jump_to_preterm, {updateTabsetPanel(session, "intabset", selected = "preterm")})

  observeEvent(input$jump_to_childdev, {updateTabsetPanel(session, "intabset", selected = "child_dev")})
  observeEvent(input$jump_to_breastfed, {updateTabsetPanel(session, "intabset", selected = "breastfeeding")})
  observeEvent(input$jump_to_mentalhealth, {updateTabsetPanel(session, "intabset", selected = "mentalhealth")})
  observeEvent(input$jump_to_cancer, {updateTabsetPanel(session, "intabset", selected = "cancer")})

  ###############################################.
  ## jump to commentary tab from data tabs ----
  ###############################################.
# To jump to commentary tab and ensures correct panel is expanded - requires multiple lines becuase action buttons must have unique ID
  observeEvent(input$jump_commentary_imm, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Immunisation")})

  observeEvent(input$jump_commentary_hv, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Child health reviews")})

  observeEvent(input$jump_commentary_cardio, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Cardiovascular")})

  observeEvent(input$jump_commentary_summary, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Summary trends")})

  observeEvent(input$jump_commentary_perinatal, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Stillbirths and infant deaths")})

  observeEvent(input$jump_commentary_booking, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Antenatal bookings")})

  observeEvent(input$jump_commentary_top, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Termination of pregnancy")})

  observeEvent(input$jump_commentary_mod, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Method of delivery")})

  observeEvent(input$jump_commentary_induction, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Induction of labour")})
  
  observeEvent(input$jump_commentary_apgar, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Apgar scores")})
  
  observeEvent(input$jump_commentary_preterm, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Location of extremely preterm deliveries")})

  observeEvent(input$jump_commentary_gestation, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Gestation at delivery")})

  observeEvent(input$jump_commentary_childdev, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Child development")})

   observeEvent(input$jump_commentary_breastfed, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Breastfeeding")})

   observeEvent(input$jump_commentary_mentalhealth, {updateTabsetPanel(session, "intabset", selected = "comment")
    updateCollapse(session, "collapse_commentary", open = "Mental health")})

   observeEvent(input$jump_commentary_cancer, {updateTabsetPanel(session, "intabset", selected = "comment")
     updateCollapse(session, "collapse_commentary", open = "Cancer")})

  observeEvent(input$jump_commentary_oohissue, {updateTabsetPanel(session, "intabset", selected = "comment")})
  observeEvent(input$jump_commentary_oohissue_sum, {updateTabsetPanel(session, "intabset", selected = "comment")})

###############################################.
## Opening collapse panels in commentary ----
###############################################.

# ObserveEvents to open collapsepanels in commentary tab when sidepanel option clicked
observeEvent(input$summary_button, ({
  updateCollapse(session, "collapse_commentary", open = "Summary trends")}))

   observeEvent(input$immunisation_button, ({
     updateCollapse(session, "collapse_commentary", open = "Immunisation")}))

   observeEvent(input$ch_review_button, ({
     updateCollapse(session, "collapse_commentary", open = "Child health reviews")}))

   observeEvent(input$cardio_button, ({
     updateCollapse(session, "collapse_commentary", open = "Cardiovascular")}))

  observeEvent(input$perinatal_button, ({
    updateCollapse(session, "collapse_commentary", open = "Stillbirths and infant deaths")}))

  observeEvent(input$mentalhealth_button, ({
    updateCollapse(session, "collapse_commentary", open = "Mental health")}))

  observeEvent(input$childdev_button, ({
    updateCollapse(session, "collapse_commentary", open = "Child development")}))

  observeEvent(input$breastfeeding_button, ({
    updateCollapse(session, "collapse_commentary", open = "Breastfeeding")}))

  observeEvent(input$booking_button, ({
    updateCollapse(session, "collapse_commentary", open = "Antenatal bookings")}))

  observeEvent(input$top_button, ({
    updateCollapse(session, "collapse_commentary", open = "Termination of pregnancy")}))

  observeEvent(input$mod_button, ({
    updateCollapse(session, "collapse_commentary", open = "Method of delivery")}))

  observeEvent(input$induction_button, ({
    updateCollapse(session, "collapse_commentary", open = "Induction of labour")}))

  observeEvent(input$gestation_button, ({
    updateCollapse(session, "collapse_commentary", open = "Gestation at delivery")}))
  
  observeEvent(input$apgar_button, ({
    updateCollapse(session, "collapse_commentary", open = "Apgar scores")}))
  
  observeEvent(input$preterm_button, ({
    updateCollapse(session, "collapse_commentary", open = "Location of extremely preterm deliveries")}))

  observeEvent(input$cancer_button, ({
  updateCollapse(session, "collapse_commentary", open = "Cancer")}))

  
} # server end

##END
