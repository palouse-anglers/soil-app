library(shiny)
library(shinyWidgets)
library(tidyverse)

source("compare_module_ui.R")
source("compare_module_server.R")
source("map_module_ui.R")
source("map_module_server.R")

ui <- fluidPage(
  titlePanel("Field Comparison Tool"),
  sidebarLayout(
    sidebarPanel(
      compare_module_ui("comparer")
    ),
    mainPanel(
      map_module_ui("mapper")
      )
  )
)

server <- function(input, output, session) {
  load("data/soil_data3.RData")
  
  soil_data3 <- new_soil_data_long_coords %>%
  dplyr:: mutate(full_field=paste0(field_name,"-",field_id))
  
  filtered_data <- compare_module_server("comparer", data = soil_data3)
  
  map_module_server("mapper",filtered_data)
  
}

shinyApp(ui = ui, server = server)