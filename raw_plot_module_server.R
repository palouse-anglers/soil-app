raw_plot_module_server <- function(id, filtered_data, filtered_data2, selected_parameter) {
  moduleServer(id, function(input, output, session) {
    

    
    # Create combined dataset

      main_data <- reactive({ 
        filtered_data() %>%
        filter(parameter %in% selected_parameter()) %>%
        mutate(group = "Main Filter")
      })
      
      compare_data <-reactive({ 
        filtered_data2() %>%
        filter(parameter %in% selected_parameter()) %>%
        mutate(group = "Compare Filter")
    })
    
      
      # slope_data <- main_data() %>%
      #   group_by(parameter) %>%
      #   summarise(Group_A_mean = mean(result, na.rm = TRUE)) %>%
      #   inner_join(
      #     compare_data() %>%
      #       group_by(parameter) %>%
      #       summarise(Group_B_mean = mean(result, na.rm = TRUE)),
      #     by = "parameter"
      #   )
      # 

      

    
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
                          "<br>Field:",full_field,
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
                          "<br>Field:",full_field,
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
                          "<br>Field:",full_field,
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
      
          output$plot3 <- renderPlotly({
          req(main_data(), compare_data())
          
          slope_data <- main_data() %>%
            group_by(parameter) %>%
            summarise(Group_A_mean = mean(result, na.rm = TRUE)) %>%
            inner_join(
             compare_data() %>%
                group_by(parameter) %>%
                summarise(Group_B_mean = mean(result, na.rm = TRUE)),
              by = "parameter"
            )
          
          slope_data_long <- slope_data %>%
            pivot_longer(cols = c(Group_A_mean, Group_B_mean),
                         names_to = "group", values_to = "mean") %>%
            mutate(group = recode(group, "Group_A_mean" = "Group A", "Group_B_mean" = "Group B"))
          
          p <- ggplot(slope_data_long, aes(
            x = group,
            y = mean,
            group = parameter,
            color = parameter,
            text = paste0("Parameter: ", parameter, "<br>Group: ", group, "<br>Mean: ", round(mean, 2))
          )) +
            geom_line() +
            geom_point(size = 2) +
            labs(title = "Slope Chart: Parameter Means", y = "Mean Result", x = "") +
            theme_minimal() +
            theme(legend.position = "none")
          
          ggplotly(p, tooltip = "text")
        })
      
          output$plot4 <- renderPlotly({
            req(main_data(), compare_data())
            
      # Step 1: Summarize data
            slope_data <- main_data() %>%
              group_by(parameter) %>%
              summarise(Group_A_mean = mean(result, na.rm = TRUE)) %>%
              inner_join(
                compare_data() %>%
                  group_by(parameter) %>%
                  summarise(Group_B_mean = mean(result, na.rm = TRUE)),
                by = "parameter"
              )
            
    # Step 2: Pivot to long format
            heat_data <- slope_data %>%
              pivot_longer(cols = c(Group_A_mean, Group_B_mean),
                           names_to = "group", values_to = "mean") %>%
              mutate(group = recode(group, "Group_A_mean" = "Group A", "Group_B_mean" = "Group B"))
            
            # Step 3: Create ggplot heatmap
            p <- ggplot(heat_data, aes(x = group, y = reorder(parameter, mean), fill = mean,
                                       text = paste0("Parameter: ", parameter,
                                                     "<br>Group: ", group,
                                                     "<br>Mean: ", round(mean, 2)))) +
              geom_tile(color = "white") +
              scale_fill_gradient(low = "#f5f5f5", high = "#78c2ad") +
              labs(
                title = "Heat Map: Parameter Means (Group A vs B)",
                x = "Group", y = "Parameter", fill = "Mean Result"
              ) +
              theme_minimal()
            
            # Step 4: Convert to interactive plot
            ggplotly(p, tooltip = "text")
          })
  
          })
}
