install.packages("avilistr")
install.packages("rebird")
install.packages("dpylr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("leaflet")
install.packages("plotly")
install.packages("stringr")
install.packages("sf")
library(plotly)
library(avilistr)
library(dplyr)
library(rebird)
library(ggplot2)
library(lubridate)
library(leaflet)
library(stringr)
library(sf)


data(avilist_2025)
data(avilist_metadata) 

file.edit("~/.Renviron")
readRenviron("~/.Renviron")


# Get basic information about the data
nrow(avilist_2025)  # Total records
sum(avilist_2025$Taxon_rank == "species")  # Number of species

#filter for specific bird
thrush_codes <- avilist_2025 %>%
  filter(Family == "Turdidae", Taxon_rank == "species") %>%
  select(Scientific_name, Species_code_Cornell_Lab) %>%
  filter(!is.na(Species_code_Cornell_Lab))

head(thrush_codes)

#basic info
str(avilist_2025)
colnames(avilist_2025)
head(avilist_2025)

#Unique Values
unique(avilist_2025$Family)

#Species Count
species_count <- avilist_2025 %>%
  filter(Taxon_rank == "species") %>%
  group_by(Family) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

head(species_count)

#Missing Codes
missing_codes <- avilist_2025 %>%
  filter(is.na(Species_code_Cornell_Lab)) %>%
  select(Scientific_name, Family)



# Coordinates roughly centered in Connecticut
lat <- 41.6
lng <- -72.7
dist_km <- 50  

recent_ct <- ebirdgeo(lat = lat, lng = lng, dist = dist_km, maxResults = 20)
print(recent_ct)


# Convert obsDt to Date class
birds$obsDate <- as.Date(birds$obsDt)

# Filter for last 7 days (or any window you want)
recent_birds <- birds %>%
  filter(obsDate >= Sys.Date() - 7)

# Count species frequency in recent sightings
top_recent_birds <- recent_birds %>%
  count(comName, sort = TRUE) %>%
  head(10)


# Plot
ggplot(top_recent_birds, aes(x = reorder(comName, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Recently Observed Bird Species in Last 7 Days",
       x = "Species",
       y = "Number of Observations")






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
nddb <- st_read("/Users/sheagelsomino/Downloads/Natural_Diversity_Database/Natural_Diversity_Database.shp")
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
getwd()
install.packages("usethis")    
library(usethis)

create_github_token()
gitcreds::gitcreds_set()

