###############################################.
## Select geography ----
###############################################.
# geoname_server <- function(input, output, session) {
#   
#   output$geoname_op_ui <- renderUI({
#     
#     areas_summary <- sort(area_type_op$area_name[area_type_op$area_type == input$`op-geotype`])
#     selectizeInput("geoname_op", label = NULL,
#                    choices = areas_summary, selected = "")
#     
#   })
# }
