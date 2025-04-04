
map_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Load Layers at Server Startup ---
    
    # Soil sample points
    soil_points_url <- "http://142.93.92.104:8080/geoserver/Columbia/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Columbia%3Asoil_sample_locations_orig&maxFeatures=50&outputFormat=application%2Fjson"
    soil_points_sf <- geojsonsf::geojson_sf(soil_points_url) %>%
      dplyr::mutate(full_field = paste0(field_name, "-", field_id))
    
    # Columbia County boundary
    columbia_county_boundary_url <- "https://services.arcgis.com/XG15cJAlne2vxtgt/ArcGIS/rest/services/WA_Columbia_Web_LiDAR/FeatureServer/1/query?where=1%3D1&f=geojson"
    columbia_county_boundary <- suppressWarnings(geojsonsf::geojson_sf(geojson = columbia_county_boundary_url))
    
    # HUC12 Watersheds
    columbia_huc12_url <- "http://142.93.92.104:8080/geoserver/Columbia/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Columbia:all_columbia_huc_12s&outputFormat=application/json"
    columbia_sf_huc_data <- geojsonsf::geojson_sf(columbia_huc12_url)
    
    # Create watershed labels
    labels <- paste(
      "<strong>", columbia_sf_huc_data$hc12_name, "</strong><br>",
      "<span style='font-size: 10px;'>", columbia_sf_huc_data$huc8_name, "</span>"
    ) %>%
      lapply(htmltools::HTML)
    
    # --- Render Leaflet Map ---
    output$map_plot <- leaflet::renderLeaflet({
      
      leaflet() %>%
        # Base ESRI layer (PLSS)
        leaflet.esri::addEsriDynamicMapLayer(
          url = "https://webgis.dor.wa.gov/arcgis/rest/services/Base/WADOR_Base_PLSS/MapServer",
          group = "SectTownRange",
          options = leaflet::tileOptions(opacity = 0.8)
        ) %>%
        
        # Columbia County boundary
        addPolygons(
          data = columbia_county_boundary,
          fill = FALSE,
          color = "black",
          stroke = TRUE,
          group = "County"
        ) %>%
        
        # Search Bar
        leaflet.extras::addSearchOSM() %>%
        
        # Map View Setup
        setView(lng = -117.9074, lat = 46.29717, zoom = 9) %>%
        
        # WMS Waterways
        addWMSTiles(
          baseUrl = "https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WMSServer?",
          layers = "0",
          options = leaflet::WMSTileOptions(
            format = "image/png32",
            version = "1.3.0",
            minZoom = 3,
            maxZoom = 16,
            transparent = TRUE
          ),
          group = "Waterways"
        ) %>%
        
        # Base Layers
        addProviderTiles("Esri.WorldGrayCanvas", group = "Gray") %>%
        addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
        addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
        addProviderTiles("Esri.NatGeoWorldMap", group = "Topo") %>%
        addProviderTiles("OpenStreetMap", group = "Street") %>%
        
        # LiDAR Overlays
        addWMSTiles(
          "http://142.93.92.104:8080/geoserver/Columbia/wms",
          layers = "Columbia:full_lidar",
          options = WMSTileOptions(format = "image/png", transparent = TRUE),
          group = "lidar"
        ) %>%
        addWMSTiles(
          "http://142.93.92.104:8080/geoserver/Columbia/wms",
          layers = "Columbia:full_lidar_hillshade",
          options = WMSTileOptions(format = "image/png", transparent = TRUE),
          group = "hillshade"
        ) %>%
        
        # Watersheds
        addPolygons(
          data = columbia_sf_huc_data,
          group = "Watersheds",
          color = "black",
          weight = 2,
          opacity = 1,
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
            sticky = FALSE,
            htmlEscape = FALSE
          ),
          highlightOptions = highlightOptions(
            weight = 5,
            color = "purple",
            fillOpacity = 0.1,
            bringToFront = TRUE
          )
        ) %>%
        
        # Fullscreen + Layer Control
        leaflet.extras::addFullscreenControl() %>%
        addLayersControl(
          overlayGroups = c("Waterways", "SectTownRange", "County", "Watersheds", "hillshade", "lidar"),
          baseGroups = c("Topo", "Imagery", "Dark", "Street", "Gray"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("SectTownRange") %>%
        hideGroup("lidar") %>%
        hideGroup("hillshade") %>%
        hideGroup("County") %>%
        hideGroup("Watersheds")
    })
  })
}
  
  
