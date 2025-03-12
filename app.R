library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(shinyWidgets)
library(plotly)
library(RColorBrewer)
library(geojsonsf)
 

# HUc12 watersheds --------------------------------------------------------


columbia_county_boundary_url <- "https://services.arcgis.com/XG15cJAlne2vxtgt/ArcGIS/rest/services/WA_Columbia_Web_LiDAR/FeatureServer/1/query?where=1%3D1&objectIds=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=geojson"
columbia_county_boundary <- suppressWarnings(geojson_sf(geojson = columbia_county_boundary_url))


columbia_huc12_url <- "http://142.93.92.104:8080/geoserver/Columbia/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Columbia:all_columbia_huc_12s&outputFormat=application/json"

# Convert GeoJSON to an sf object
columbia_sf_huc_data <- geojson_sf(columbia_huc12_url)


labels <- paste(
  "<strong>", columbia_sf_huc_data$hc12_name, "</strong><br>",
  "<span style='font-size: 10px;'>", columbia_sf_huc_data$huc8_name, "</span>"
) %>%
  lapply(htmltools::HTML)

#load("data/soil_data.RData")
load("data/soil_data2.RData")

soil_data2 <- soil_data2 %>%
  dplyr:: mutate(full_field=paste(`Field Name`,"-",field))

parameter_names <- soil_data2 %>%
  dplyr::select('Bulk Density (mill #/ac-depth)':'K (Bicarb) (meq/100g)',
  -c(year,path),`Al (KCl) (ppm)`,`Al (DTPA) (ppm)` ) %>%
  names() 



ui <- page_sidebar(theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Soil Chemistry Comparison",
  tags$head(tags$style(
    HTML(
      "
        .bslib-full-screen-enter {
          bottom: var(--bslib-full-screen-enter-bottom);
        }
      "
    )
  )),
  sidebar = sidebar(width = 
                      "325px",
    shinyjs::useShinyjs(),
    pickerInput(
      inputId = "selected_fields",
      label = "Select Fields to Compare:",
      choices = unique(soil_data2$full_field),
      selected = c("Sheep Camp - 15A","Harting - 01","Price - 26A"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} Fields",
        `max-options` = 8 
      )
    ),
    
    pickerInput(
      inputId = "parameter",
      label = "Select Parameter:",
      choices = parameter_names,
      selected = "pH ((1:1))",
      options = list(
        `live-search` = TRUE
      )
    ),
    
    pickerInput(
      inputId = "depth",
      label = "Select Depth (inches):",
      choices = c("0/3","3/6","6/12"),
      selected = "0/3",
      multiple = TRUE,
      options = list(
      `live-search` = TRUE,
        size = 5
      )
    ),
    
    sliderInput("year_range", "Select Year Range:",
                min = min(soil_data2$year),
                max = max(soil_data2$year),
                value = c(min(soil_data2$year), max(soil_data2$year)),
                step = 1,
                sep = "")
  ),
  layout_column_wrap(
    width = 1/2,
  bslib::card(
    full_screen = TRUE,
    card_header(textOutput("plot_title")),
    card_body(plotlyOutput("box_plot"))
  ),
  
  bslib::card(
    full_screen = TRUE,
    card_header("Sample Locations"),
    leafletOutput("map_plot", height = 300)
  )),
  
  layout_column_wrap(
    width = 1/2,
    bslib::card(
      full_screen = TRUE,
      card_header(textOutput("range")),
      tableOutput("summary_stats")
    ), 
    bslib::card(
    full_screen = TRUE,
    card_header("Detailed Data"),
    card_body(DTOutput("data_table"))
  )),
  
 
)

server <- function(input, output, session) {
  
  # Dynamic plot title
  output$plot_title <- renderText({
    paste(input$parameter, "Distribution by Field")
  })
  
  # observe({
  #   
  #    
  #     data <- filtered_data()
  #     
  #     all_na_columns <- data %>%
  #       select(-full_field) %>%
  #       summarise(across(everything(), ~ all(is.na(.)))) %>%
  #       unlist() %>%
  #       all()  
  #     
  #     if (all_na_columns) {
  #       hide("box_plot")  
  #     } else {
  #       show("box_plot")
  #     }
  #   
  #   
  #   
  # })
  
  # Filtered dataset
  filtered_data <- reactive({
    soil_data2 %>%
      filter(
        full_field %in% input$selected_fields,
        `Depth (inches)` %in%  input$depth,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })
  
 output$range <- renderText({
   
  paste("Summary Statistics:",
  paste(min(filtered_data()$year, na.rm = TRUE), 
  max(filtered_data()$year, na.rm = TRUE), sep = "–")
  )
  
   
   
 })
  
  
  # Get the selected parameter values
  selected_values <- reactive({
    filtered_data()[[input$parameter]]
  })
  
  

  field_colors <- reactive({
    # Generate color palette for up to 5 fields using "Set2"
    colors <- brewer.pal(5, "Set2")
    selected_fields <- input$selected_fields
    setNames(colors[seq_along(selected_fields)], selected_fields)
  })
  
  
  
  # field_colors <- reactive({
  #   n_fields <- length(input$selected_fields)
  #   colors <- colorRampPalette(brewer.pal(min(8, n_fields), "Set2"))(n_fields)
  #   setNames(colors, input$selected_fields)
  # })
  
  
  
  # field_colors <- reactive({
  #   # Generate colors for all fields from field_locations
  #   all_colors <- setNames(
  #     colorRampPalette(brewer.pal(8, "Set2"))(length(unique(soil_data2$field))),
  #     unique(soil_data2$field)
  #   )
  #   # Return only the colors for selected fields, maintaining consistent colors
  #   all_colors[input$selected_fields]
  # })
  
  
  # Base map with all possible field locations
  output$map_plot <- renderLeaflet({
    #pal <- colorFactor(palette = "Set2", domain = soil_data2$field)
    
    
    #n_fields <- length(unique(soil_data2$field))
    #custom_pal <- colorRampPalette(brewer.pal(8, "Set2"))(n_fields)
    
    pal <- colorFactor(palette = field_colors(), domain = input$selected_fields)
    
    print(pal)
    
    leaflet() %>%
      leaflet.esri::addEsriDynamicMapLayer(
        url = "https://webgis.dor.wa.gov/arcgis/rest/services/Base/WADOR_Base_PLSS/MapServer",
        group = "SectTownRange",
        options = tileOptions(opacity = 0.8) # Adjust opacity if needed
      ) %>%
      addPolygons(data=columbia_county_boundary,fill = FALSE,color = "black",stroke = "black",group = "County")%>%
      leaflet.extras::addSearchOSM() %>%
      setView(lng = -117.9074, lat = 46.29717, zoom = 9) %>%
      addWMSTiles(
        baseUrl = "https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WMSServer?",
        layers = "0",
        options = leaflet::WMSTileOptions(
          format = "image/png32",
          version = "1.3.0",
          minZoom = 3,
          maxZoom = 16,
          transparent = TRUE
        ),group = "Waterways"
      ) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group="Gray") %>%
      addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
      addProviderTiles("CartoDB.DarkMatter", group="Dark") %>%
      addProviderTiles("Esri.NatGeoWorldMap", group="Topo") %>%
      addProviderTiles("OpenStreetMap", group="Street") %>%
      addPolygons(
        data = columbia_sf_huc_data,
        group="Watersheds",
        color = "black",  # Polygon border color
        weight = 2,
        opacity = 1,
        #fillOpacity = 0,
        #fillColor = "#66c2a5",
        label = ~labels,
        labelOptions = labelOptions(
          style = list(
            "color" = "black",
            "font-weight" = "bold",
            "background-color" = "white",
            "border-radius" = "5px",
            "padding" = "5px"
          ),
          textsize = "14px",
          direction = "auto",
          sticky = FALSE,  # Ensures the label doesn't stick when not hovering
          htmlEscape = FALSE  # Render HTML in labels
        ),
        highlightOptions = highlightOptions(
          weight = 5,
          color = "purple",
          fillOpacity = 0.1,
          bringToFront = TRUE
        )
      ) %>%
      leaflet.extras::addFullscreenControl() %>%
      addLayersControl(
        overlayGroups = c("Waterways","SectTownRange","County","Watersheds"),
        baseGroups = c("Topo","Imagery", "Dark", "Street","Gray")
      ) %>%
      hideGroup("SectTownRange") %>%
      hideGroup("County") %>%
      hideGroup("Watersheds") %>%
      setView(
        lng = mean(soil_data2$Longitude,na.rm = TRUE),
        lat = mean(soil_data2$Latitude,na.rm = TRUE),
        zoom = 9
      )
  })
  
  
  # Filtered locations with valid coordinates
  valid_locations <- reactive({
    soil_data2 %>%
      filter(
        full_field %in% input$selected_fields,
        !is.na(Latitude),
        !is.na(Longitude)
      )
  })
  
  # Update markers when selection changes
  observe({
    #pal <- colorFactor(palette = "Set2", domain = soil_data2$field)
    
    #n_fields <- length(unique(soil_data2$field))
    #custom_pal <- colorRampPalette(brewer.pal(8, "Set2"))(n_fields)
  pal <- colorFactor(palette = field_colors(), domain = input$selected_fields)
    
    

    req(nrow(valid_locations()) > 0)
    
      leafletProxy("map_plot") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data = valid_locations(),
        lng = ~Longitude,
        lat = ~Latitude,
        color = "black",
        fillColor = ~pal(full_field),
        radius = 8,
        weight=2,
        stroke = TRUE,
        fillOpacity = 0.7,
        popup = ~paste("Field:", full_field)
      ) %>%

      addLegend(
        position = "bottomleft",
        pal = pal,
        values = valid_locations()$full_field,
        title = "Fields"
      )
    
    # Adjust bounds if there are selected fields
    if(nrow(valid_locations()) > 0) {
      leafletProxy("map_plot") %>%
        flyToBounds(
          lng1 = min(valid_locations()$Longitude) - 0.1,
          lat1 = min(valid_locations()$Latitude) - 0.1,
          lng2 = max(valid_locations()$Longitude) + 0.1,
          lat2 = max(valid_locations()$Latitude) + 0.1
        )
    }
  })
  
  
  
  # Interactive box plot using plotly
  output$box_plot <- renderPlotly({
    
    req(isTruthy(input$depth))
    req(isTruthy(input$selected_fields))
   
    data <- filtered_data()
    
    # Ensure data exists
    req(nrow(data) > 0, cancelOutput = TRUE)
    
    # Ensure selected parameter exists and is not all NA
    if (!(input$parameter %in% colnames(data)) || all(is.na(data[[input$parameter]]))) {
      return(NULL)  # Prevent ggplotly() from running if the parameter is missing
    }
    
    p <- ggplot(data, aes(x = full_field, y = .data[[input$parameter]], fill = full_field)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.3) +
      theme_minimal() +
      labs(x = "Field", 
           y = input$parameter,
           fill = "Field") +
      theme(legend.position = "none") +
    #scale_fill_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2") +  # Keep depth colors
    scale_color_manual(values = field_colors()) 
    
    ggplotly(p, tooltip = c("y", "fill")) %>%
      config(
        toImageButtonOptions = list(
          format = "png",
          filename = paste0(input$parameter, "_distribution"),
          width = 800,
          height = 500
        ),
        modeBarButtons = list(list(
          "toImage",
          "zoom2d",
          "pan2d",
          "zoomIn2d",
          "zoomOut2d",
          "autoScale2d",
          "resetScale2d"
        ))
      )
    
  
  })
  
  # Summary statistics
  output$summary_stats <- renderTable({
    
    req(isTruthy(input$depth))
    req(isTruthy(input$selected_fields))
    
    filtered_data() %>%
      rename(Field=full_field) %>%
      group_by(Field) %>%
      summarize(
        Mean = round(mean(.data[[input$parameter]]), 2),
        Min = round(min(.data[[input$parameter]]), 2),
        Max = round(max(.data[[input$parameter]]), 2),
        SD = round(sd(.data[[input$parameter]]), 2),
        N = length(1:n())
      ) #%>%
      #rename_with(~paste(input$parameter, ., sep = "_"),-Field)
  })
  
  # Detailed data table
  output$data_table <- renderDT({
    
    req(isTruthy(input$selected_fields))
    req(isTruthy(input$depth))
    
    filtered_data() %>%
      select(Field=full_field, Year=year, `Depth (inches)`, !!sym(input$parameter)) %>%
      arrange(Field, Year) %>%
      datatable(options = list(pageLength = 25))
  })
}


observe({
  if (is.null(input$depth) || input$depth == "") {
    showNotification("Please select a depth", type = "warning", duration = 3)
  }
})

shinyApp(ui, server)
