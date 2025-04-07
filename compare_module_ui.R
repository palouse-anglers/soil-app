library(shinyWidgets)

compare_module_ui <- function(id) {
  ns <- NS(id)
  
  unique_depths <- c("0-2","0-3","0-4","0-6","0-7","0-8","0-12",
                     "2-4","3-6","4-6","4-8","6-9","6-12","12-24")
  
  
  tagList(
    pickerInput(
      inputId = ns("year"),
      label = "Select Year:",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} Years"
      )
    ),
    pickerInput(
      inputId = ns("month"),
      label = "Select Month:",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} Months"
      )
    ),
    pickerInput(
      inputId = ns("sample_depth"),
      label = "Select Sample Depth:",
      choices = unique_depths,
      multiple = TRUE,
      options = pickerOptions(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} Depths",
      )
    ),
    
  pickerInput(
      inputId = ns("watershed"),
      label = "Select Watershed(s):",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 1",
        `count-selected-text` = "{0} Watersheds",
        
        )
    ),
    shinyWidgets::switchInput(
      inputId = ns("only_with_coords"),
      label = "Has Coordinates",
      onLabel = "Yes", offLabel = "All",
      value = TRUE,   
      size = "mini"
    ),
    pickerInput(
      inputId = ns("selected_fields"),
      label = "Select Events by Field",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        `dropup-auto` = FALSE,
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} Events"
      )
    ),
    pickerInput(
      inputId = ns("parameter"),
      label = "Select Parameter:",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        `selected-text-format` = "count > 1",
        `count-selected-text` = "{0} Parameters",
      )
    )
  )
}