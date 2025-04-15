# raw_plot_module.R

raw_plot_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(pickerInput(
      inputId = ns("parameter"),
      label = "Select Parameter:",
      choices = NULL, # to be populated dynamically
      multiple = TRUE,
      width = "100%",
      options = list(`live-search` = TRUE)
    ),
    radioButtons(
      inputId = ns("plot_type"),
      label = "Plot Type:",
      choices = c("Dot Plot" = "dot", "Bar Plot" = "bar"),
      selected = "dot",
      inline = TRUE
    )),
    
    fluidRow(
      column(
        width = 6,
        card(full_screen = T,
          card_header("Filtered Data 1"),
          card_body(plotOutput(ns("plot1")))
        )
      ),
      column(
        width = 6,
        card(full_screen = T,
          card_header("Filtered Data 2"),
          card_body(plotOutput(ns("plot2")))
        )
      )
    )
  )
}
