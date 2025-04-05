summary_module_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$sample_count <- renderText({
      req(filtered_data())
      nrow(filtered_data())
    })
    
    output$year_count <- renderText({
      req(filtered_data())
      length(unique(filtered_data()$year))
    })
    
    output$parameter_count <- renderText({
      req(filtered_data())
      length(unique(filtered_data()$parameter))
    })
    
    output$event_count <- renderText({
      req(filtered_data())
      if ("full_field" %in% names(filtered_data())) {
        length(unique(filtered_data()$full_field))
      } else {
        "N/A"
      }
    })
    
    output$field_count <- renderText({
      req(filtered_data())
      length(unique(filtered_data()$field_name))
    })
  })
}