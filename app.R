library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib)

source("compare_module_ui.R")
source("compare_module_server.R")
source("map_module_ui.R")
source("map_module_server.R")
source("summary_module_ui.R")
source("summary_module_server.R")
source("plot_module_ui.R")
source("plot_module_server.R")

ui <- page_sidebar(
  title = "Columbia County Soil Health",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  sidebar = sidebar(width = '400px',
    title = "Filters",
    compare_module_ui("comparer")
    ),
  navset_tab(id = "main_tabs",
             summary_module_ui("summarizer"),
  nav_panel(
      title = "Map",
      page_fillable(
      map_module_ui("mapper")
      )
  ),
  nav_panel(
    title = "Plots",
    page_fillable(
      plot_module_ui("plotter")
    )
  ),
  
  
  )
)

server <- function(input, output, session) {
  load("data/soil_data3.RData")
  
  soil_data3 <- new_soil_data_long_coords %>%
  dplyr:: mutate(full_field=paste0(field_name,"-",field_id)) %>%
  mutate(across(c(huc8_name,hc12_name),~
                  ifelse(is.na(.x),"unknown",.x)))
  
  filtered_data <- compare_module_server("comparer", data = soil_data3)
  
  map_module_server("mapper",filtered_data)
  summary_module_server("summarizer",filtered_data)
  plot_module_server("plotter",filtered_data)
}

shinyApp(ui = ui, server = server)