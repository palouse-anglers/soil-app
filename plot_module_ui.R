plot_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyWidgets::pickerInput(
      inputId = ns("selected_parameters"),
      label = "Select Parameters",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} Parameters"
      )
    )
    #plotly::plotlyOutput(ns("comparison_plot"), height = "700px")
  )
}

