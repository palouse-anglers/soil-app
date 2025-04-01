# Add watersheds to soil chemistry locations
# Add nutrient_type
# pivot longer and clean soil chem names

library(dplyr)
library(DT)
library(leaflet)
library(sf)
library(geojsonsf)
library(DT)
library(tidyverse)
load("data/soil_data2.RData")


y2014_farm_names <- data.table::fread("../../../Downloads/2014 CCD Soil Test Data.xlsx - Sheet1.csv",
                                      na.strings = "") %>% 
  as_tibble() %>%
  mutate(farm_id=ifelse(farm_id=="NWL",NA_character_,farm_id))%>%
  fill(farm_id,.direction="down") %>%
  filter(!is.na(farm)) %>%
  group_by(farm_id) %>%
  mutate(item=1:n()) %>%
  slice_max(item)


new_soil_data <- readxl::read_xlsx(skip = 1, "../../../Downloads/CCD & WSUCCEXT  2013-2024 Soil Data.xlsx") 

new_soil_lookup_table <- data.table::fread("../../../Downloads/soil_variable_classification.csv") %>%
  set_names(c("parameter","nutrient_type"))

var_names <- names(new_soil_data)
units <- new_soil_data %>%
  slice(1)

colnames(new_soil_data) <- ifelse(!is.na(units),paste0(var_names," (",units,")"),var_names)



new_soil_data_long <- new_soil_data %>%
  select(-c(...11,...12,...13)) %>%
  filter(!is.na(Account)) %>%
  mutate(across(c(`Start Depth (inches)`:`Soil Texture`),~as.numeric(.x))
         )%>%
  select(-`Price ($)`) %>%
  pivot_longer(
    cols = -c(Account:'Lab ID',`DateSample`,'Client','Start Depth (inches)','End Depth (inches)'),   
    names_to = "parameter",
    values_to = "result"
  ) %>%
  left_join(new_soil_lookup_table,by="parameter") %>%
  rename(field_id=`Field ID`)


# TODO Poindexter 29 (missing), 29-1,29-2,29-01 (missing) 29-0 (missing)
yr2013_table <- tibble(
field_id = c("13-14","13-15","13-11","13-74","13-49","13-20","13-25","13-61","13-29"),
field_name=c("control_native_site","Sheep Camp","Brown","control_native_site",
             "Roberts North","Cahill","Hawks Trust","Moyer Home","Poindexter"),
field_w_coords=c(NA_character_,"15A","11A",NA_character_,NA_character_,"20",NA_character_,NA_character_,
        "29-1")
)

yr2014_table <- tibble(
  field_id = c("14-1","14-3","14-12","14-13","14-14", "14-15","14-16","14-47",  ),
  field_name=c("Harting","Harting","Skip Mead","Skip Mead","control_native_site","Sheep Camp","North End"),
  field_w_coords=c("01",NA_character_,NA_character_,NA_character_,NA_character_,"15A",NA_character_,)
)

                                                                                 



columbia_huc12_url <- "http://142.93.92.104:8080/geoserver/Columbia/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Columbia:all_columbia_huc_12s&outputFormat=application/json"

# Convert GeoJSON to an sf object
# drop geometry, we only need watershed names here
columbia_huc12s <- geojson_sf(columbia_huc12_url) 

sf_use_s2(FALSE)
# Optionally look at how many:

# Fix invalid geometries
watersheds_fixed <- st_make_valid(columbia_huc12s)

unique_sites <- 
  soil_data2 %>%
  distinct(Latitude,Longitude) %>%
  tidyr::drop_na()



sample_points_sf <- st_as_sf(unique_sites, coords = c("Longitude", "Latitude"), crs = 4326)


points_with_watershed <- st_join(sample_points_sf, watersheds_fixed, join = st_within) %>%
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2]
  ) %>%
  select(Longitude,Latitude,huc8_name,hc12_name,huc12) %>%
  st_drop_geometry()



soil_data_with_huc12 <- soil_data2 %>%
left_join(points_with_watershed,by=c("Longitude","Latitude")) %>%
  pivot_longer(
  cols = -c(field,
            path,
            `Field Name`,
            `Contact name`,
             year, 
            `Depth (inches)`,
            Latitude, Longitude,
            huc8_name,
            hc12_name,
            huc12),
  names_to = "parameter",
  values_to = "result"
) %>%
janitor::clean_names()




  classification_map <- tibble::tibble(
    parameter = c(
      "Al (DTPA) (ppm)", "Al (KCl) (ppm)", "Available Moisture (inches/depth)",
      "Base Saturation (%)", "Boron (DTPA) (ppm)", "Buffer pH (A/E ,SMP) (NA)",
      "Bulk Density (mill #/ac-depth)", "CEC (meq/100g)", "Ca (1N KCl) (meq/100g)",
      "Ca/CEC (%)", "Chloride (H2O) (lbs/acre-depth)", "Chloride (H2O) (ppm)",
      "Cu (DTPA) (ppm)", "Efferv. (0-7)", "Est. CEC (meq/100g)", "Fe (DTPA) (ppm)",
      "K (1N KCl) (meq/100g)", "K (Bicarb) (meq/100g)", "K (Bicarb) (ppm)", "K/CEC (%)",
      "Lime (%)", "Mg (1N KCl) (meq/100g)", "Mg/CEC (%)", "Mn (DTPA) (ppm)",
      "NH4N (1N KCl) (lbs/acre)", "NH4N (1N KCl) (ppm)", "NO3N (CaSO4) (lbs/acre)",
      "NO3N (CaSO4) (ppm)", "Na (1N KCl) (meq/100g)", "Na/CEC (%)", "OM (Walk.-Blk) (%)",
      "P ( Bray) (ppm) ", "P (Bicarb) (ppm)", "SO4S (DTPA) (lbs/acre)",
      "SO4S (DTPA) (ppm)", "Soluble Salts (mmhos/cm)", "Total Bases (meq/100g)",
      "Zn (DTPA) (ppm)", "pH ((1:1))"
    ),
    nutrient_type = c(
      "other", "other", "other", "other", "micronutrient", "other", "other", "other",
      "macronutrient", "other", "micronutrient", "micronutrient", "micronutrient", "other",
      "other", "micronutrient", "macronutrient", "macronutrient", "macronutrient", "other",
      "other", "macronutrient", "other", "micronutrient", "macronutrient", "macronutrient",
      "macronutrient", "macronutrient", "other", "other", "other", "macronutrient",
      "macronutrient", "macronutrient", "macronutrient", "other", "other", "micronutrient", "other"
    )
  )

    

soil_data_with_huc12_type <- soil_data_with_huc12 %>%
left_join(classification_map,by="parameter") %>%
mutate(file=str_remove(path,patter = "../../../Downloads/pH Soil Data/"))%>%
select(field,field_name,year,depth_inches,latitude,
       longitude, huc8_name, hc12_name, huc12, parameter, result, nutrient_type, file)


distinct_soil_locations <- soil_data_with_huc12_type %>%
  group_by(field) %>%
  mutate(sample_years = paste(unique(year),sep=",",collapse = ",")) %>%
  ungroup()%>% 
  distinct(field,field_name,sample_years,latitude,longitude,hc12_name,huc12) %>%
  filter(!is.na(latitude))
  


  st_as_sf(distinct_soil_locations,coords = c("latitude" ,"longitude"),crs=4326) %>%
  sf::st_write(,dsn="../soil-app/data/soil_sample_locations.shp")

# Historical Soil Data- modeled from open meteo API
  
  
  save(soil_data_with_huc12_type,file="data/soil_data_long.RData")

# https://archive-api.open-meteo.com/v1/archive?latitude=46.37086,46.4165,46.36046,46.30637,46.30874,46.22412,46.46987,46.47005,46.39428,46.32702,46.24632,46.30361,46.24391,46.39547,46.39551,46.24518,46.3083,46.38238,46.24012,46.40346,46.40195,46.21057,46.32625,46.38015,46.30605,46.23344,46.2451,46.37707,46.3368,46.23509,46.41462,46.42099,46.39427,46.43482,46.3584,46.35804,46.33936,46.29304&longitude=-117.9872,-117.8027,-117.7615,-117.846,-117.9444,-117.9623,-118.0829,-118.0833,-118.0888,-118.063,-118.0583,-117.838,-118.0825,-117.9928,-117.9926,-118.1159,-118.0208,-117.8648,-117.9174,-117.8617,-117.8569,-117.9774,-118.1149,-117.8434,-118.133,-118.0849,-118.0815,-117.9725,-118.0034,-118.0907,-117.755,-117.8067,-118.0336,-117.8299,-117.9012,-117.7137,-117.8333,-118.0579&start_date=2015-01-01&end_date=2025-03-02&daily=wind_speed_10m_max,wind_direction_10m_dominant,precipitation_sum,temperature_2m_max,temperature_2m_min,soil_moisture_0_to_10cm_mean,soil_moisture_7_to_28cm_mean&hourly=temperature_2m&precipitation_unit=inch

# Split the csv up
# join location data and samples by location_id 
historic_soil_locations <-  
  read.csv(nrows = 38,"../../../Downloads/open-meteo-46.36N117.94W621m.csv")

historic_soil_names <-  
  read.csv(skip = 38,nrows=10,"../../../Downloads/open-meteo-46.36N117.94W621m.csv")

names <- c("location_id","time","wind_speed_10m_max (km/h)","wind_direction_10m_dominant (°)","precipitation_sum (inch)","temperature_2m_max (°C)","temperature_2m_min (°C)","soil_moisture_0_to_10cm_mean (undefined)","shortwave_radiation_sum (MJ/m²)","soil_moisture_7_to_28cm_mean (m³/m³)")

historic_soil_samp <-  
  read.csv(skip = 39,"../../../Downloads/open-meteo-46.36N117.94W621m.csv")

historic_samples <- historic_soil_samp %>%
  left_join(historic_soil_locations,by="location_id")%>%
  janitor::clean_names()%>%
  mutate(time=lubridate::ymd(time),
         year=lubridate::year(time)) %>%
  left_join(distinct_soil_locations %>% select(field, field_name,
                                               huc12, hc12_name, 
                                               longitude, latitude),by = c("longitude","latitude"))



historic_samples_yearly_summary <- historic_samples %>%
  select(
    location_id, year,
    wind_speed_10m_max_km_h:soil_moisture_7_to_28cm_mean_m_m
  ) %>%
  group_by(location_id, year) %>%
  mutate(precipitation_sum_year_inch = sum(precipitation_sum_inch)) %>%
  rename(precipitation_mean_daily = precipitation_sum_inch) %>%
  summarise(across(
    everything(),
    ~ mean(.x, na.rm = TRUE)
  ), .groups = "drop") %>%
  ungroup() %>%
  mutate(
    precipitation_bin = cut(
      precipitation_sum_year_inch,
      breaks = quantile(precipitation_sum_year_inch, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  )








