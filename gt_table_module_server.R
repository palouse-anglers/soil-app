gt_table_module_server <- function(id, filtered_data, filtered_data2) {
  moduleServer(id, function(input, output, session) {
    output$comparison_table <- render_gt({
      
      # Validate inputs
      req(filtered_data(), filtered_data2())
      
      summary_table <- bind_rows(
        filtered_data() %>% mutate(group = "Group A"),
        filtered_data2() %>% mutate(group = "Group B")
      ) %>%
        filter(!is.na(result)) %>%
        group_by(group, parameter) %>%
        summarize(
          n = n(),
          mean = mean(result, na.rm = TRUE),
          sd = sd(result, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = group,
          values_from = c(n, mean, sd),
          names_glue = "{group}_{.value}"
        ) %>%
        gt() %>%
        fmt_number(columns = contains("mean"), decimals = 2) %>%
        fmt_number(columns = contains("sd"), decimals = 2) %>%
        fmt_number(columns = contains("n"), decimals = 0) %>%
        tab_header(title = "Parameter-Level Comparison of Soil Chemistry Results")
    })
  })
}
