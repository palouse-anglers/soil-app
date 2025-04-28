library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bslib)
library(sortable)
library(plotly)
library(leaflet)
library(ggridges)
library(gtsummary)
library(gt)
library(sf)
library(shinychat)
library(DBI)
library(RSQLite)
library(ellmer)
library(promises)
library(future)

source("compare_module_ui.R")
source("compare_module_server.R")
source("map_module_ui.R")
source("map_module_server.R")
source("summary_module_ui.R")
source("summary_module_server.R")
source("raw_plot_module_ui.R")
source("raw_plot_module_server.R")
source("gt_table_module_ui.R")
source("gt_table_module_server.R")
source("soil_chat_ui.R")
source("soil_chat_server.R")
source("plot_bot_ui.R")
source("plot_bot_server.R")

plan(multisession)



ui <- page_sidebar(
  title = "Columbia County Soil Health",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  sidebar = sidebar(
    width = '500px',
    title = "Subset and Compare Groups",
    bslib::accordion(
    id = "compare_groups",
  bslib::accordion_panel(
    title = "Group A",
    compare_module_ui("original")
    ),
    bslib::accordion_panel(
      title = "Group B",
      compare_module_ui("compare")
      )),
  pickerInput(
    inputId = "parameter",
    label = "Select Parameter:",
    choices = NULL, # to be populated dynamically
    multiple = TRUE,
    width = "100%",
    options = list(`live-search` = TRUE)
  )
),
  navset_tab(id = "main_tabs",

  nav_panel(
      title = "Map",
      page_fillable(
      map_module_ui("mapper")
      )
  ),
  # nav_panel(
  #   title = "Build a Compare",
  #   page_fillable(
  #     
  #     bslib::card(
  #       full_screen = TRUE,
  #       card_header("Compare Filters (Group B)")
  #       
  #       ### Picker Inputs (stacked vertically)
  #       #compare_module_ui("compare")
  #       #card_footer(summary_module_ui("summarizer_compare", theme = "secondary"))
  #       )
  #     )
  # ),
  nav_panel(
    title = "Plots",
    page_fillable(
      # layout_columns(
      #   col_widths = c(4, 8),
      bslib::accordion(
        id = "value_boxes",
        bslib::accordion_panel(
          title = "Summaries",
          summary_module_ui("summarizer"),
          summary_module_ui("summarizer_compare",group = "B", theme = "secondary")),
      ),  
      bslib::card(
          full_screen = TRUE,
          card_header("Compare Filters"),
          raw_plot_module_ui("raw_compare_plot")
        )
  
  )
    ),
  nav_panel(
    title = "Tables",
    page_fillable(
      bslib::card(
        full_screen = TRUE,
        card_header("Summary Table"),
        gt_table_module_ui("raw_compare_table")
      )
      
    )
  ),
  nav_panel(
    title = "Table Bot",
    page_fillable(
      bslib::card(
        full_screen = TRUE,
        card_header("Chat GPT"),
        soil_chat_ui("soil_chat")
      )
      
    )
  ),
  nav_panel(
    title = "Plot Bot",
    page_fillable(
      bslib::card(
        full_screen = TRUE,
        card_header("Chat GPT"),
        plot_bot_ui("plotbot")
      )
      
    )
  ),
  

  ))

server <- function(input, output, session) {
  load("data/soil_data3.RData")
  
  soil_data3 <- new_soil_data_long_coords %>%
  dplyr:: mutate(full_field=paste0(field_name,"-",field_id)) %>%
    mutate(month=lubridate::month(label = TRUE,sample_date,abbr = TRUE)) %>%
  mutate(across(c(huc8_name,hc12_name),~
                  ifelse(is.na(.x),"unknown",.x)))
  
  db <- dbConnect(RSQLite::SQLite(), "soil_data3.sqlite")
  
  filtered_data <- compare_module_server("original", data = soil_data3)
  filtered_data2 <- compare_module_server("compare", data = soil_data3)
  selected_param <- reactive({ input$parameter })
  
  
  map_module_server("mapper",filtered_data,filtered_data2)
  summary_module_server("summarizer",filtered_data)
  summary_module_server("summarizer_compare",filtered_data2)

 soil_chat_server("soil_chat", database_path = "soil_data3.sqlite") 
  
 gt_table_module_server("raw_compare_table",filtered_data, filtered_data2)

 plot_bot_server("plotbot",soil_data3 = soil_data3)
 
  raw_plot_module_server(
    id = "raw_compare_plot",
    filtered_data = filtered_data,
    filtered_data2 = filtered_data2,
    selected_parameter = selected_param
  )
  
  observe({
    req(filtered_data(), filtered_data2())
    
    all_params <- union(
      unique(filtered_data()$parameter),
      unique(filtered_data2()$parameter)
    )
    
   
    updatePickerInput(
      session,
      inputId = "parameter",
      choices = sort(all_params),
      selected = isolate(selected_param())
    )
  })

  }

shinyApp(ui = ui, server = server)