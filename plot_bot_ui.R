# plot_bot_ui <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     shinyjs::useShinyjs(),
#     h2("Plot Bot"),
#     shiny::checkboxInput(ns("wake_up"), "Wake up Plot Bot", value = FALSE),
#     div(id = ns("robot_emoji_box"),
#         textOutput(ns("robot_emoji"))
#     ),
#     div(id = ns("chat_box"),  # <-- wrap chat_ui in a div
#         chat_ui(ns("chat"))
#     ),
#     verbatimTextOutput(ns("r_code_text")),
#     plotlyOutput(ns("plot_output"), height = "600px")
#   )
# } 

plot_bot_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Soil Chemistry Plot Bot"),
    mainPanel(
      chat_ui(ns("chat_plot")),
      verbatimTextOutput(ns("r_code_text")),
      plotlyOutput(ns("plot_output"), height = "600px")
    )
  )
}