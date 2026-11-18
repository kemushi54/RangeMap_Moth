# prepare data for web
library(magrittr)
library(data.table)
library(dplyr)
library(raster)
library(leaflet)
library(geojsonio)
library(htmlwidgets)

# species list
species_list <- 
  readRDS("data/species_list.rds")

# species info
species_info <- 
  readRDS("data/species_list_20230525.rds") |> 
  mutate(Alphabet = substr(scientific_name, 1, 1)) |> 
  select(Family_ch,
         Alphabet,
         scientific_name)

fwrite(species_info, "data/sp_table.csv")


# html of SDM results
lapply(seq_along(species_info$scientific_name), function(s){
  
  # Load GeoJSON files for Species 1 (sp1) and its thresholds
  df <- 
    fread(sprintf("data/binary_table_minmax/%s.csv", species_info$scientific_name[s]))
  
  range_min <- 
    filter(df, variable == "min") |> 
    dplyr::select(x, y, value) |> 
    rasterFromXYZ(crs = sp::CRS("+init=epsg:3826"))
  
  range_max <- 
    filter(df, variable == "max") |> 
    dplyr::select(x, y, value) |> 
    rasterFromXYZ(crs = sp::CRS("+init=epsg:3826"))
  
  # Create the base leaflet map
  mymap <- leaflet() %>%
    # Base map
    # Add Satellite map (ESRI World Imagery)
    addWMSTiles(
      "http://maps.nlsc.gov.tw/S_Maps/wms",
      layers = "PHOTO2",
      group = "正射影像"
    ) %>%
    # Add OpenTopoMap (with contour lines)
    addWMSTiles(
      "http://maps.nlsc.gov.tw/S_Maps/wms",
      layers = "EMAP15",
      group = "通用電子地圖 (含等高線)"
    ) %>%
    setView(lng = 121.5, lat = 25.0, zoom = 7)
    
  
  # Add species and threshold layers to the map
    mymap <- mymap |> 
      addRasterImage(range_min, colors = "red", opacity = 0.3,
                     options = tileOptions(zIndex = 500)) |> 
      addRasterImage(range_max, colors = "red", opacity = 0.4,
                     options = tileOptions(zIndex = 500))
  
  # Add layer controls for species and thresholds
  mymap <- mymap %>%
    addLayersControl(
      baseGroups = c("通用電子地圖 (含等高線)", "正射影像"),
      # overlayGroups = c("通用電子地圖 (含等高線)", "正射影像"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  saveWidget(mymap, sprintf("www/map/%s.html", species_info$scientific_name[s]))
})
