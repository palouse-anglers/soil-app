# soil_chat_ui.R
soil_chat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    chat_ui(ns("chat")),
    verbatimTextOutput(ns("query_text")),
    DT::DTOutput(ns("result_table"))
  )
}