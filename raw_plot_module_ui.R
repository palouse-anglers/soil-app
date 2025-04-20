# raw_plot_module.R

raw_plot_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
    radioButtons(
      inputId = ns("plot_type"),
      label = "Plot Type:",
      choices = c("Dot Plot" = "dot", "Box Plot" = "box","Heat Plot"="heat"),
      selected = "dot",
      inline = TRUE
    )),
    
    fluidRow(
      column(
        width = 6,
        card(full_screen = T,
          card_header("Filtered Data 1"),
          card_body(plotlyOutput(ns("plot1")))
        )
      ),
      column(
        width = 6,
        card(full_screen = T,
          card_header("Filtered Data 2"),
          card_body(plotlyOutput(ns("plot2")))
        )
      )
    )
  )
}
