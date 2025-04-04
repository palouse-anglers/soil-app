server <- function(input, output, session) {
  
  # Load your data
  load("data/soil_data3.RData")  # assuming you saved it there
  
  compare_module_server("comparer", data = new_soil_data_long_coords)
}
