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
          card_header("Group A"),
          card_body(plotlyOutput(ns("plot1")))
        )
      ),
      column(
        width = 6,
        card(full_screen = T,
          card_header("Group B"),
          card_body(plotlyOutput(ns("plot2")))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        card(full_screen = T,
             card_header("Slope Means"),
             card_body(plotlyOutput(ns("plot3")))
        )
      )
      
    ),
    fluidRow(
      column(
        width = 12,
        card(full_screen = T,
             card_header("Heat Means"),
             card_body(plotlyOutput(ns("plot4")))
        )
      )
      
    )
    
    
  )
}
