plot_module_server <- function(id, filtered_data, filtered_data2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update parameter choices dynamically based on both datasets
    observe({
      req(filtered_data(), filtered_data2())
      
      all_params <- union(
        unique(filtered_data()$parameter),
        unique(filtered_data2()$parameter)
      )
      
      updatePickerInput(
        session,
        inputId = "selected_parameters",
        choices = sort(all_params)
      )
    })
    
    # Create combined dataset
    comparison_data <- reactive({
      req(filtered_data(), filtered_data2(), input$selected_parameters)
      
      main_data <- filtered_data() %>%
        filter(parameter %in% input$selected_parameters) %>%
        mutate(group = "Main Filter")
      
      compare_data <- filtered_data2() %>%
        filter(parameter %in% input$selected_parameters) %>%
        mutate(group = "Compare Filter")
      
      bind_rows(main_data, compare_data)
    })
    
    
    return(comparison_data)
    
    # Create Boxplot
  #   output$comparison_plot <- plotly::renderPlotly({
  #     req(comparison_data())
  #     
  #     p <- comparison_data() %>%
  #       ggplot(aes(
  #         x = parameter,
  #         y = result,
  #         fill = group,
  #         text = paste0(
  #           "Field: ", full_field, "<br>",
  #           "Group: ", group, "<br>",
  #           "Parameter: ", parameter, "<br>",
  #           "Result: ", round(result, 2)
  #         )
  #       )) +
  #       geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
  #       geom_jitter(
  #         width = 0.2,
  #         size = 1.5,
  #         aes(color = group),
  #         alpha = 0.7
  #       ) +
  #       scale_fill_manual(values = c("Main Filter" = "#1f77b4", "Compare Filter" = "#ff7f0e")) +
  #       scale_color_manual(values = c("Main Filter" = "#1f77b4", "Compare Filter" = "#ff7f0e")) +
  #       theme_minimal() +
  #       labs(
  #         x = "Parameter",
  #         y = "Result",
  #         fill = "Group",
  #         color = "Group",
  #         title = "Comparison of Main vs Compare (Boxplots)"
  #       ) +
  #       theme(
  #         axis.text.x = element_text(angle = 45, hjust = 1),
  #         plot.title = element_text(hjust = 0.5, size = 16)
  #       )
  #     
  #     plotly::ggplotly(p, tooltip = "text")
  #   })
  #   
   })
}

