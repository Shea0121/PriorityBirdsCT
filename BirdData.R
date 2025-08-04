install.packages("avilistr")
install.packages("rebird")
install.packages("dpylr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("leaflet")
install.packages("plotly")
install.packages("stringr")
install.packages("sf")
install.packages("leaflet.extras")
install.packages("gt")
install.packages("rlang") #needed for update to rlang to 1.1.4 to run gt
library(plotly)
library(avilistr)
library(dplyr)
library(rebird)
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)
library(sf)
library(leaflet.extras)
library(gt)



data(avilist_2025)
data(avilist_metadata) 

file.edit("~/.Renviron")
readRenviron("~/.Renviron")





priority_species <- c(
  "American Black Duck", "American Oystercatcher", "Bald Eagle", 
  "Black-crowned Night-Heron", "Cerulean Warbler", "Saltmarsh Sparrow", 
  "Upland Sandpiper", "Wood Thrush", "Piping Plover", "Least Tern", 
  "Northern Harrier", "Bobolink", "Prairie Warbler", "Eastern Meadowlark", 
  "Sedge Wren", "Great Egret", "Northern Bobwhite", "American Woodcock", 
  "Marbled Godwit", "Swainson's Hawk", "Eastern Whip-poor-will", 
  "Ruby-throated Hummingbird", "Black-billed Cuckoo", "Scarlet Tanager", 
  "White-eyed Vireo", "Common Nighthawk", "Little Blue Heron", 
  "Barn Owl", "Grasshopper Sparrow"
)

# Get recent sightings in CT (eBird region code)
recent <- ebirdregion(loc = "US-CT", max = 500)

# Filter to only priority species
priority_recent <- recent %>%
  filter(comName %in% priority_species) %>%
  mutate(obsDt = as.Date(obsDt))  # Convert date column

# Create color palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = as.numeric(priority_recent$obsDt)
)
# Create interactive map
leaflet(priority_recent) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    ~lng, ~lat,
    color = ~pal(as.numeric(obsDt)),
    radius = 6,
    popup = ~paste0("<b>", comName, "</b><br>", obsDt, "<br>Location: ", locName),
    stroke = FALSE,
    fillOpacity = 0.85
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~as.numeric(obsDt),
    title = "Observation Date",
    opacity = 1,
    labFormat = labelFormat(transform = function(x) as.Date(x, origin = "1970-01-01"))
  )

# Get recent sightings in CT (eBird region code), max 30-day history
recent <- ebirdregion(loc = "US-CT", back = 30, max = 5000)

# Filter to only priority species
priority_recent <- recent %>%
  filter(comName %in% priority_species) %>%
  mutate(obsDt = as.Date(obsDt))  # Convert date column

# Create color palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = as.numeric(priority_recent$obsDt)
)

# Create interactive map
leaflet(priority_recent) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    ~lng, ~lat,
    color = ~pal(as.numeric(obsDt)),
    radius = 6,
    popup = ~paste0("<b>", comName, "</b><br>", obsDt, "<br>Location: ", locName),
    stroke = FALSE,
    fillOpacity = 0.85
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~as.numeric(obsDt),
    title = "Observation Date",
    opacity = 1,
    labFormat = labelFormat(transform = function(x) as.Date(x, origin = "1970-01-01"))
  )

# Load Natural Diversity Database shapefile
nddb <- st_read("Natural_Diversity_Database/Natural_Diversity_Database.shp")
# Transform to long/lat WGS84
nddb <- st_transform(nddb, crs = 4326)

# Preview the geometry
plot(nddb$geometry)

# Convert eBird sightings to sf points
priority_sf <- priority_recent %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

# Create the leaflet map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Add NDDB polygons
  addPolygons(data = nddb,
              fillColor = "green",
              fillOpacity = 0.3,
              weight = 1,
              color = "darkgreen",
              group = "NDDB Areas") %>%
  
  # Add eBird sightings
  addCircleMarkers(data = priority_sf,
                   radius = 6,
                   fillColor = "red",
                   fillOpacity = 0.8,
                   stroke = FALSE,
                   popup = ~paste0("<b>", comName, "</b><br>", obsDt, "<br>Location: ", locName),
                   group = "Bird Sightings") %>%
  
  # Add layer toggle
  addLayersControl(
    overlayGroups = c("NDDB Areas", "Bird Sightings"),
    options = layersControlOptions(collapsed = FALSE)
  )


#Species sighting frequencies 
priority_df <- read.csv("filtered_priority_ebird.csv")


priority_summary <- priority_df %>%
  count(comName, sort = TRUE) %>%
  rename(
    "Bird Name" = comName,
    "Sightings in Last 30 Days" = n
  )

# Format with gt
priority_summary %>%
  gt() %>%
  tab_header(
    title = "Bird Sightings in the Last 30 Days"
  ) %>%
  cols_label(
    `Bird Name` = "Bird Species",
    `Sightings in Last 30 Days` = "Number of Sightings"
  ) %>%
  fmt_number(
    columns = `Sightings in Last 30 Days`,
    decimals = 0
  ) %>%
  tab_options(
    table.font.size = "medium",
    row.striping.background_color = "#f9f9f9",
    heading.title.font.size = 16,
    heading.title.font.weight = "bold"
  )

# Add lat and lon columns
priority_sf <- priority_sf %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  )

#Heatmap
leaflet(priority_sf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    lng = ~longitude,
    lat = ~latitude,
    radius = 20,
    blur = 1,
    max = 1  # Adjust max to intensify the colors
  ) %>%
  fitBounds(
    lng1 = min(priority_sf$longitude),
    lat1 = min(priority_sf$latitude),
    lng2 = max(priority_sf$longitude),
    lat2 = max(priority_sf$latitude)
  )


