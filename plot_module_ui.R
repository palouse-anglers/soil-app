plot_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyWidgets::pickerInput(
      inputId = ns("selected_parameters"),
      label = "Select Parameters to Compare:",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} Parameters"
      )
    ),
    
    br(),
    plotly::plotlyOutput(ns("comparison_plot"), height = "700px")
  )
}



# plot_module_ui <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     h4("Build a Custom Comparison"),
#     
#     # Dynamic grouping for available fields
#     shinyWidgets::pickerInput(
#       inputId = ns("group_by"),
#       label = "Group Available Fields By:",
#       choices = c("Watershed" = "huc8_name", "Depth" = "depth"),
#       selected = "huc8_name"
#     ),
#     
#     uiOutput(ns("field_buckets")),
#     
#     shinyWidgets::pickerInput(
#       inputId = ns("selected_parameters"),
#       label = "Select Parameters:",
#       choices = NULL,
#       multiple = TRUE,
#       options = shinyWidgets::pickerOptions(
#         `actions-box` = TRUE,
#         `live-search` = TRUE,
#         `selected-text-format` = "count > 2"
#       )
#     ),
#     
#     actionButton(ns("build_comparison"), "Build Comparison", class = "btn-primary"),
#     br(), br(),
#     plotly::plotlyOutput(ns("comparison_plot"), height = "600px")
#   )
# }





# plot_module_ui <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     h4("Build a Comparison"),
#     
#     uiOutput(ns("field_buckets")),   # <<<< placeholder for dynamic bucket_list
#     
#     shinyWidgets::pickerInput(
#       inputId = ns("selected_parameters"),
#       label = "Select Parameters:",
#       choices = NULL,
#       multiple = TRUE,
#       options = shinyWidgets::pickerOptions(
#         `actions-box` = TRUE,
#         `live-search` = TRUE,
#         `selected-text-format` = "count > 2"
#       )
#     ),
#     
#     actionButton(ns("build_comparison"), "Build Comparison", class = "btn-primary"),
#     br(), br(),
#     plotly::plotlyOutput(ns("comparison_plot"), height = "600px")
#   )
# }

# plot_module_ui <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     bslib::layout_column_wrap(
#       width = 1/2,
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Soil Physical Properties"),
#         plotlyOutput(ns("soil_physical_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Salinity and Conductivity"),
#         plotlyOutput(ns("salinity_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Soil Acidity (pH)"),
#         plotlyOutput(ns("ph_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Organic Matter"),
#         plotlyOutput(ns("om_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Nitrogen (NH₄⁺ and NO₃⁻)"),
#         plotlyOutput(ns("nitrogen_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Phosphorus and Potassium (P, K)"),
#         plotlyOutput(ns("pk_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Sulfur and Chloride (S, Cl)"),
#         plotlyOutput(ns("sulfur_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Micronutrients"),
#         plotlyOutput(ns("micronutrient_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Aluminum and Toxicity Indicators"),
#         plotlyOutput(ns("aluminum_plot"), height = "600px")
#       ),
#       bslib::card(
#         full_screen = TRUE,
#         bslib::card_header("Cation Exchange Capacity and Base Saturation"),
#         plotlyOutput(ns("cec_plot"), height = "600px")
#       )
#     )
#   )
# }