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
      output$plot1 <- renderPlotly({
        
        req(main_data())
        
        if (input$plot_type == "dot") {
          p <- ggplot(main_data(),
                      aes(
                        x = sample_date,
                        y = result,
                        color = parameter,
                        group = parameter,
                        text = paste(
                          "Date:", sample_date,
                          "<br>Result:", round(result, 2),
                          "<br>Parameter:", parameter
                        )
                      )) +
            geom_point(alpha = 0.6, size = 2) +
            geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            labs(
              x = "Sample Date",
              y = "Result",
              title = "Soil Chemistry Over Time (Dot Plot)",
              color = "Parameter"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          # BAR PLOT (Aggregated Means)
        } else if (input$plot_type == "box") {
          
          p <- ggplot(main_data(),
                      aes(
                        x = parameter,
                        y = result,
                        fill = parameter,
                        text = paste(
                          "Parameter:", parameter,
                          "<br>Result:", round(result, 2),
                          "<br>Date:", sample_date
                        )
                      )) +
            geom_boxplot(outlier.alpha = 0.2) +
            labs(
              x = "Parameter",
              y = "Result",
              title = "Soil Chemistry by Parameter (Boxplot)",
              fill = "Parameter"
            ) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none"  # optional, to reduce redundancy
            )
        } else if (input$plot_type == "heat") {
          # Ridge plot
          heatmap_df <- main_data() %>%
            filter(!is.na(result), !is.na(parameter), !is.na(full_field)) %>%
            group_by(full_field, parameter) %>%
            summarize(mean_result = mean(result, na.rm = TRUE), .groups = "drop") %>%
            pivot_wider(names_from = parameter, values_from = mean_result)
          
          # Convert to matrix for plotly heatmap (remove the field column after preserving)
          heatmap_matrix <- heatmap_df %>%
            column_to_rownames("full_field") %>%
            as.matrix()
          
          # Create the interactive heatmap
          p <- plot_ly(
            x = colnames(heatmap_matrix),
            y = rownames(heatmap_matrix),
            z = heatmap_matrix,
            type = "heatmap",
            colorscale = "Viridis",
            hovertemplate = paste(
              "Field: %{y}<br>",
              "Parameter: %{x}<br>",
              "Mean Result: %{z:.2f}<extra></extra>"
            )
          ) %>%
            layout(
              title = "Soil Chemistry Heatmap (Mean Results per Field)",
              xaxis = list(title = "Parameter", tickangle = -45),
              yaxis = list(title = "Field"),
              margin = list(l = 100, r = 20, b = 100, t = 40)
            )
        }
        
        
        
        if (input$plot_type %in% c("dot", "box")) {
          ggplotly(p, tooltip = "text") %>%
            layout(legend = list(orientation = "h", x = 0, y = -0.3))
        } else {
          p  
        }
          
    })
      
    
    # Plot for filtered_data2()
      output$plot2 <- renderPlotly({
        
        req(compare_data())
        
        if (input$plot_type == "dot") {
          p <- ggplot(compare_data(),
                      aes(
                        x = sample_date,
                        y = result,
                        color = parameter,
                        group = parameter,
                        text = paste(
                          "Date:", sample_date,
                          "<br>Result:", round(result, 2),
                          "<br>Parameter:", parameter
                        )
                      )) +
            geom_point(alpha = 0.6, size = 2) +
            geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            labs(
              x = "Sample Date",
              y = "Result",
              title = "Soil Chemistry Over Time (Dot Plot)",
              color = "Parameter"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          # BAR PLOT (Aggregated Means)
        } else if (input$plot_type == "box") {
          
          p <- ggplot(compare_data(),
                      aes(
                        x = parameter,
                        y = result,
                        fill = parameter,
                        text = paste(
                          "Parameter:", parameter,
                          "<br>Result:", round(result, 2),
                          "<br>Date:", sample_date
                        )
                      )) +
            geom_boxplot(outlier.alpha = 0.2) +
            labs(
              x = "Parameter",
              y = "Result",
              title = "Soil Chemistry by Parameter (Boxplot)",
              fill = "Parameter"
            ) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none"  # optional, to reduce redundancy
            )
        } else if (input$plot_type == "heat") {
          # Ridge plot
          heatmap_df <- compare_data() %>%
            filter(!is.na(result), !is.na(parameter), !is.na(full_field)) %>%
            group_by(full_field, parameter) %>%
            summarize(mean_result = mean(result, na.rm = TRUE), .groups = "drop") %>%
            pivot_wider(names_from = parameter, values_from = mean_result)
          
          # Convert to matrix for plotly heatmap (remove the field column after preserving)
          heatmap_matrix <- heatmap_df %>%
            column_to_rownames("full_field") %>%
            as.matrix()
          
          # Create the interactive heatmap
          p <- plot_ly(
            x = colnames(heatmap_matrix),
            y = rownames(heatmap_matrix),
            z = heatmap_matrix,
            type = "heatmap",
            colorscale = "Viridis",
            hovertemplate = paste(
              "Field: %{y}<br>",
              "Parameter: %{x}<br>",
              "Mean Result: %{z:.2f}<extra></extra>"
            )
          ) %>%
            layout(
              title = "Soil Chemistry Heatmap (Mean Results per Field)",
              xaxis = list(title = "Parameter", tickangle = -45),
              yaxis = list(title = "Field"),
              margin = list(l = 100, r = 20, b = 100, t = 40)
            )
        }
        
        
        
        if (input$plot_type %in% c("dot", "box")) {
          ggplotly(p, tooltip = "text") %>%
            layout(legend = list(orientation = "h", x = 0, y = -0.3))
        } else {
          p  
        }
          
        
      })
  })
}
