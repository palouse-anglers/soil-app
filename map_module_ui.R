map_module_ui <- function(id) {
 
   ns <- NS(id)
  
  tagList(
    leaflet::leafletOutput(ns("map_plot"), height = "750px")
  )
}