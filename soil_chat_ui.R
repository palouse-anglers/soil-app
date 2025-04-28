# soil_chat_ui.R
soil_chat_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h2("Table Bot"),
    shiny::checkboxInput(ns("wake_up"), "Wake up Table Bot", value = FALSE),
    #chat_ui(ns("chat")),
    div(id = ns("robot_emoji_box"),
        textOutput(ns("robot_emoji"))
    ),
    div(id = ns("chat_box"),  # <-- wrap chat_ui in a div
        chat_ui(ns("chat"))
    ),
    verbatimTextOutput(ns("query_text")),
    DT::DTOutput(ns("result_table"))
  )
}