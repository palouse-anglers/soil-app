plot_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Soil Physical Properties"),
        plotlyOutput(ns("soil_physical_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Salinity and Conductivity"),
        plotlyOutput(ns("salinity_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Soil Acidity (pH)"),
        plotlyOutput(ns("ph_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Organic Matter"),
        plotlyOutput(ns("om_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Nitrogen (NH₄⁺ and NO₃⁻)"),
        plotlyOutput(ns("nitrogen_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Phosphorus and Potassium (P, K)"),
        plotlyOutput(ns("pk_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Sulfur and Chloride (S, Cl)"),
        plotlyOutput(ns("sulfur_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Micronutrients"),
        plotlyOutput(ns("micronutrient_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Aluminum and Toxicity Indicators"),
        plotlyOutput(ns("aluminum_plot"), height = "600px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Cation Exchange Capacity and Base Saturation"),
        plotlyOutput(ns("cec_plot"), height = "600px")
      )
    )
  )
}