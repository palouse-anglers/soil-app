plot_module_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Enriched data with units
    enriched_data <- reactive({
      req(filtered_data())
      
      filtered_data() %>%
        mutate(
          unit = case_when(
            str_detect(parameter, "ppm") ~ "ppm",
            str_detect(parameter, "lbs") ~ "lbs/acre",
            str_detect(parameter, "%") ~ "%",
            str_detect(parameter, "meq/100g") ~ "meq/100g",
            str_detect(parameter, "dS/m") ~ "dS/m",
            str_detect(parameter, "inches") ~ "inches",
            TRUE ~ "Other"
          )
        )
    })
    
    # Define parameter groups
    soil_physical_params <- c("Density 2 (mill-lbs/ac-depth)", "Density (g/ml)", "Efferves (Scale 0-7)",
                              "Wilting Point", "Field Capacity", "H2O wet/dry", "Avl H2O (inches)", "Avl H2O % (%)", "Soil Texture")
    
    salinity_params <- c("Ec(1:1) (dS/m)", "Est. SS (dS/m)")
    
    ph_params <- c("pH 1:1", "pH (A-E)")
    
    om_params <- c("OM (%)")
    
    nitrogen_params <- c("NH4N (ppm)", "NH4N# (lbs/ac-depth)", "NO3N (ppm)", "NO3N# (lbs/ac-depth)")
    
    pk_params <- c("Bray P(1:10) (ppm)", "Olsen P (ppm)", "Olsen K (ppm)")
    
    sulfur_params <- c("SO4S (ppm)", "SO4S# (lbs/ac-depth)", "Cl (ppm)")
    
    micronutrient_params <- c("B (ppm)", "Zn (ppm)", "Mn (ppm)", "Cu (ppm)", "Fe (ppm)")
    
    aluminum_params <- c("Al (ppm)", "Al(KCl) (ppm)")
    
    cec_params <- c("Exch Na (meq/100g)", "Exch Ca (meq/100g)", "Exch Mg (meq/100g)", "Exch K (meq/100g)",
                    "Total Bases (meq/100g)", "Est CEC (meq/100g)", "CEC (meq/100g)", 
                    "Ca (KCl) (ppm)", "Mg (KCl) (ppm)", "Na (KCl) (ppm)")
    
    # Define a little helper function to create plots
    plot_group <- function(data, params) {
      
      p <- data %>%
        filter(parameter %in% params) %>%
        ggplot(aes(
          x = year,
          y = result,
          color = field_name,
          group = field_name,
          text = paste0(
            "Field: ", field_name, "<br>",
            "Year: ", year, "<br>",
            "Parameter: ", parameter, "<br>",
            "Result: ", round(result, 2)
          )
        )) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        facet_wrap(~ parameter, scales = "free_y") +
        labs(
          x = "Year",
          y = "Result"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(size = 11, face = "bold")
        )
      
      plotly::ggplotly(p, tooltip = "text")
    }
    
    # Render plots for each group
    output$soil_physical_plot <- renderPlotly({ plot_group(enriched_data(), soil_physical_params) })
    output$salinity_plot <- renderPlotly({ plot_group(enriched_data(), salinity_params) })
    output$ph_plot <- renderPlotly({ plot_group(enriched_data(), ph_params) })
    output$om_plot <- renderPlotly({ plot_group(enriched_data(), om_params) })
    output$nitrogen_plot <- renderPlotly({ plot_group(enriched_data(), nitrogen_params) })
    output$pk_plot <- renderPlotly({ plot_group(enriched_data(), pk_params) })
    output$sulfur_plot <- renderPlotly({ plot_group(enriched_data(), sulfur_params) })
    output$micronutrient_plot <- renderPlotly({ plot_group(enriched_data(), micronutrient_params) })
    output$aluminum_plot <- renderPlotly({ plot_group(enriched_data(), aluminum_params) })
    output$cec_plot <- renderPlotly({ plot_group(enriched_data(), cec_params) })
    
  })
}