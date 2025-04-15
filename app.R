library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib)
library(sortable)

source("compare_module_ui.R")
source("compare_module_server.R")
source("map_module_ui.R")
source("map_module_server.R")
source("summary_module_ui.R")
source("summary_module_server.R")
source("raw_plot_module_ui.R")
source("raw_plot_module_server.R")

ui <- page_sidebar(
  title = "Columbia County Soil Health",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  sidebar = sidebar(width = '500px',
    title = "Main Filters",
    compare_module_ui("original")
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
    title = "Build a Compare",
    page_fillable(
      layout_columns(
        col_widths = c(8,4),  # Sidebar 4/12, Plot 8/12
        
        ### Left Column (Comparison Filters inside a Card)
        bslib::card(
          full_screen = TRUE,
          card_header("Compare Filters"),
          compare_module_ui("compare")
        ),
        
        ### Right Column (Comparison Plot inside a Card)
        bslib::card(
          full_screen = TRUE,
          card_header("Instructions"),
          card_body("")
        )
      )
    )
  ),
  nav_panel(
    title = "Raw Plots",
    page_fillable(
      # layout_columns(
      #   col_widths = c(4, 8),
        
      bslib::card(
          full_screen = TRUE,
          card_header("Compare Filters"),
          raw_plot_module_ui("raw_compare_plot")
        )
  
  )
    )

  ))

server <- function(input, output, session) {
  load("data/soil_data3.RData")
  
  soil_data3 <- new_soil_data_long_coords %>%
  dplyr:: mutate(full_field=paste0(field_name,"-",field_id)) %>%
    mutate(month=lubridate::month(label = TRUE,sample_date,abbr = TRUE)) %>%
  mutate(across(c(huc8_name,hc12_name),~
                  ifelse(is.na(.x),"unknown",.x)))
  
  filtered_data <- compare_module_server("original", data = soil_data3)
  filtered_data2 <- compare_module_server("compare", data = soil_data3)
  
  map_module_server("mapper",filtered_data)
  summary_module_server("summarizer",filtered_data)
  raw_plot_module_server(
    id = "raw_compare_plot",
    filtered_data = filtered_data,
    filtered_data2 = filtered_data2
  )
}

shinyApp(ui = ui, server = server)