

compare_module_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    watershed_choices <- data %>%
      select(huc8_name, hc12_name) %>%
      distinct() %>%
      arrange(huc8_name, hc12_name)%>%
      group_by(huc8_name) %>%
      group_split() %>%
      setNames(map_chr(., ~ unique(.x$huc8_name))) %>%
      map(~ .x$hc12_name)
    
    unique_depths <- c("0-2","0-3","0-4","0-6","0-7","0-8","0-12",
                       "2-4","3-6","4-6","4-8","6-9","6-12","12-24")
    
    
    
    
    
    
    filtered_data_fields <- reactive({
      out <- data
      
      if (!is.null(input$year) && length(input$year) > 0) {
        out <- out %>% filter(year %in% input$year)
      }
      
      if (!is.null(input$month) && length(input$month) > 0) {
        out <- out %>% filter(month %in% input$month)
      }
      
      if (!is.null(input$sample_depth) && length(input$sample_depth) > 0) {
        out <- out %>% filter(depth %in% input$sample_depth)
      }
      
      if (!is.null(input$watershed) && length(input$watershed) > 0) {
        out <- out %>% filter(hc12_name %in% input$watershed)
      }
      
      if (isTRUE(input$only_with_coords)) {
        out <- out %>% filter(!is.na(latitude), !is.na(longitude))
      }
      
      out
    })
    
    observeEvent(
      {
        input$year
        input$month
        input$sample_depth
        input$watershed
        input$only_with_coords
      },
      {
        req(filtered_data_fields())
        
        available_fields <- filtered_data_fields() %>%
          select(full_field, fixed_field_id) %>%
          distinct() %>%
          arrange(fixed_field_id) %>%
          pull(full_field)
        
        if (length(available_fields) > 0) {
          updatePickerInput(
            session,
            "selected_fields",
            choices = available_fields,
            selected = available_fields
          )
        } else {
          updatePickerInput(
            session,
            "selected_fields",
            choices = available_fields,
            selected = ""
          )
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )
    
    # observe({
    #   
    #   req(filtered_data_fields())
    #   
    #   available_fields <- filtered_data_fields() %>%
    #     select(full_field, fixed_field_id) %>%
    #     distinct() %>%
    #     arrange(fixed_field_id) %>%
    #     pull(full_field)
    #   
    #   if (length(available_fields) > 0) {
    #     updatePickerInput(
    #       session,
    #       "selected_fields",
    #       choices = available_fields,
    #       selected = available_fields  
    #     )
    #   } else {
    #     updatePickerInput(
    #       session,
    #       "selected_fields",
    #       choices = character(0),
    #       selected = character(0) 
    #     )
    #   }
    #   
    #   
    # })
    
    
    # year 
    observe({
      updatePickerInput(
        session, "year",
        choices = sort(unique(data$year)),
        selected = sort(unique(data$year))  
      )
    })
    
    # month 
    observe({
      updatePickerInput(
        session, "month",
        choices = sort(unique(data$month)),
        selected = sort(unique(data$month))  
      )
    })
    
    # Depth picker
    observe({
      updatePickerInput(
        session, "sample_depth",
        choices = unique_depths,  
        selected = unique_depths  
      )
    })
    
    # param
    observe({
      
      ppm <-  data %>% 
        filter(str_detect(parameter,"ppm")) %>%
        pull(parameter) %>%
        unique()
      
      updatePickerInput(
        session, "parameter",
        choices = sort(unique(data$parameter)),
        selected= ppm
      )
    })
    
    
    # Watershed picker
    observe({
      
      watershed_choices <- data %>%
        select(huc8_name, hc12_name) %>%
        distinct() %>%
        arrange(huc8_name, hc12_name) %>%
        group_by(huc8_name) %>%
        group_split() %>%
        setNames(map_chr(., ~ unique(.x$huc8_name))) %>%
        map(~ .x$hc12_name)
      
      updatePickerInput(
        session, "watershed",
        choices = watershed_choices,
        selected = unlist(watershed_choices, use.names = FALSE)
      )
    })
    
    
    filtered_data <- reactive({
      
      filtered_data_fields() %>%
     # filter(year %in% input$year) %>%
     # filter(depth %in% input$sample_depth) %>%
     #filter(hc12_name %in% input$watershed) %>%
      filter(full_field %in% input$selected_fields) %>%
      filter(parameter %in% input$parameter)
      
    })
    
    
    # output$comparison_plot <- renderPlot({
    #   req(filtered_data())
    #   req(input$parameter)
    # 
    #   ggplot(filtered_data(), aes(x = full_field, y = result, fill = full_field)) +
    #     geom_col(position = "dodge") +
    #     facet_wrap(~ parameter, scales = "free_y") +  # <<<< Facet automatically by parameter
    #     labs(
    #       title = "Comparison Across Selected Parameters",
    #       x = "Field",
    #       y = "Result Value"
    #     ) +
    #     theme_minimal() +
    #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #   })
    # 
    
    
    return(filtered_data)
    
   })
}