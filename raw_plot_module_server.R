raw_plot_module_server <- function(id, filtered_data, filtered_data2) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(filtered_data(), filtered_data2())
      
      all_params <- union(
        unique(filtered_data()$parameter),
        unique(filtered_data2()$parameter)
      )
      
      print(all_params)
      
      updatePickerInput(
        session,
        inputId = "parameter",
        choices = sort(all_params),
        selected = isolate(input$parameter)
      )
    })
    
    # Create combined dataset

      main_data <- reactive({ 
        filtered_data() %>%
        filter(parameter %in% input$parameter) %>%
        mutate(group = "Main Filter")
      })
      
      compare_data <-reactive({ 
        filtered_data2() %>%
        filter(parameter %in% input$parameter) %>%
        mutate(group = "Compare Filter")
    })
    
    
    # Plot for filtered_data()
      output$plot1 <- renderPlot({
        req(main_data())
        
        ggplot(main_data(), aes(
          x = sample_date,
          y = result,
          color = parameter,
          group = parameter  # important to group lines correctly
        )) +
          geom_point(alpha = 0.6, size = 2) +
          geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1) +
          scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
          labs(
            x = "Sample Date",
            y = "Result",
            title = "Soil Chemistry Over Time",
            color = "Parameter"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
    
    # Plot for filtered_data2()
      output$plot2 <- renderPlot({
        req(compare_data())

        ggplot(compare_data(),
               aes(
                 x = sample_date,
                 y = result,
                 color = parameter,
                 group = parameter  # important to group lines correctly
               )) +
          geom_point(alpha = 0.6, size = 2) +
          geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1) +
          scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
          labs(
            x = "Sample Date",
            y = "Result",
            title = "Soil Chemistry Over Time",
            color = "Parameter"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
  })
}
