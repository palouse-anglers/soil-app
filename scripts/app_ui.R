library(shiny)

ui <- fluidPage(
  titlePanel("Field Comparison Tool"),
  sidebarLayout(
    sidebarPanel(
      compare_module_ui("comparer")
    ),
    mainPanel(
      # Plot output already handled inside the module
    )
  )
)
