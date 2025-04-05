summary_module_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    bslib::value_box(
      title = "Samples",
      value = textOutput(ns("sample_count")),
      showcase = bsicons::bs_icon("file-text-fill")
    ),
    bslib::value_box(
      title = "Parameters",
      value = textOutput(ns("parameter_count")),
      showcase = icon("flask")
    ),
    bslib::value_box(
      title = "Years with Samples",
      value = textOutput(ns("year_count")),
      showcase = bsicons::bs_icon("clock-history")
    ),
    bslib::value_box(
      title = "Events",
      value = textOutput(ns("event_count")),
      showcase = icon("calendar")
    ),
    bslib::value_box(
      title = "Fields",
      value = textOutput(ns("field_count")),
      showcase = icon("map-marker-alt")
    )
  )
}