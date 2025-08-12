library(shiny)
library(dplyr)
library(rebird)
library(sf)
library(leaflet)
library(leaflet.extras)
library(gt)
library(lubridate)



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

# Load NDDB shapefile once (make sure the path is correct)
nddb <- sf::st_read("Natural_Diversity_Database/Natural_Diversity_Database.shp", quiet = TRUE) %>%
  st_transform(crs = 4326)

library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",  # try "flatly", "cerulean", etc.
    primary = "#2C3E50",
    secondary = "#18BC9C",
    base_font = font_google("Roboto")
  ),
  
  titlePanel(
    div(style = "text-align: center; font-size: 28px; font-weight: bold;",
        "Connecticut Priority Bird Sightings – Last 30 Days"),
    windowTitle = "CT Priority Birds"
  ),
  
  tabsetPanel(
    tabPanel("Priority Species List",
             div(style = "padding: 15px;",
                 h4("Connecticut Priority Bird Species"),
                 tags$ul(
                   tags$li(
                     tags$span(style = "color: orange; font-weight: bold;", "Near Threatened: "),
                     "Sedge Wren, Bobolink, Piping Plover, Wood Thrush, Upland Sandpiper"
                   ),
                   tags$li(
                     tags$span(style = "color: red; font-weight: bold;", "High Concern: "),
                     "Saltmarsh Sparrow"
                   ),
                   tags$li(
                     tags$span(style = "color: darkred; font-weight: bold;", "Vulnerable: "),
                     "Cerulean Warbler"
                   ),
                   tags$li(
                     tags$span(style = "color: green; font-weight: bold;", "Low Concern: "),
                     "American Black Duck, American Oystercatcher, Bald Eagle, Black-crowned Night-Heron, Least Tern, Northern Harrier, Prairie Warbler, Eastern Meadowlark, Great Egret, Northern Bobwhite, American Woodcock, Marbled Godwit, Swainson's Hawk, Eastern Whip-poor-will, Ruby-throated Hummingbird, Black-billed Cuckoo, Scarlet Tanager, White-eyed Vireo, Common Nighthawk, Little Blue Heron, Barn Owl, Grasshopper Sparrow"
                   )
                 )
             )
    ),
    tabPanel("Sightings Frequency",  
             gt_output("freq_table")
    ),
    tabPanel("Sightings & Conservation Map",
             leafletOutput("sightings_map", height = 600)
    ),
    tabPanel("Heatmap of Sightings",
             leafletOutput("heatmap_map", height = 600)
    )
  ),
  
  div(style = "text-align: center; margin-top: 20px; font-size: 14px; color: #555;",
      "Species list adapted from ",
      tags$a(href = "https://019669fd-c3a2-a236-f8cb-a7568038d5e9.share.connect.posit.cloud/", target = "_blank",
             "Connecticut Priority Bird Species"),
      " by Daly Analytics."
  )
)




server <- function(input, output, session) {
  output$map_summary <- renderText({
    paste0(
      "Showing ", nrow(priority_recent()), " sightings of priority species in the last 30 days, totaling ",
      sum(priority_recent()$howMany), " birds spotted."
    )
  })
  
  # Get recent sightings 
  priority_recent <- reactive({
    recent <- ebirdregion(loc = "US-CT", back = 30, max = 5000, key = "uq6va2898b7s")
    recent %>%
      filter(comName %in% priority_species) %>%
      mutate(obsDt = as.Date(obsDt))
  })
  
  # Summary table data
  priority_summary <- reactive({
    priority_recent() %>%
      group_by(comName) %>%
      summarise(
        `Overall Sightings` = n(),                      # number of observation events
        `Total Birds Spotted` = sum(howMany, na.rm = TRUE),  # sum of birds reported
        .groups = "drop"
      ) %>%
      arrange(desc(`Total Birds Spotted`)) %>%
      rename(`Bird Name` = comName)
  })
  
  # Render frequency table with gt
  output$freq_table <- render_gt({
    priority_summary() %>%
      gt() %>%
      tab_header(title = "Priority Bird Sightings in the Last 30 Days") %>%
      cols_label(
        `Bird Name` = "Bird Species",
        `Overall Sightings` = "Observation Events",
        `Total Birds Spotted` = "Number of Birds"
      ) %>%
      fmt_number(
        columns = c(`Overall Sightings`, `Total Birds Spotted`),
        decimals = 0
      ) %>%
      tab_options(
        table.font.size = "medium",
        row.striping.background_color = "#f9f9f9",
        heading.title.font.size = 16,
        heading.title.font.weight = "bold"
      )
  })
  
  
  # Prepare sf points for mapping
  priority_sf <- reactive({
    df <- priority_recent()
    st_as_sf(df, coords = c("lng", "lat"), crs = 4326) %>%
      mutate(
        longitude = st_coordinates(.)[,1],
        latitude = st_coordinates(.)[,2]
      )
  })
  
  # Color palette for sightings by date
  pal <- reactive({
    colorNumeric(palette = "YlOrRd", domain = as.numeric(priority_recent()$obsDt))
  })
  
  # Sightings & conservation map
  output$sightings_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = nddb,
        fillColor = "green",
        fillOpacity = 0.3,
        weight = 1,
        color = "darkgreen",
        group = "NDDB Areas"
      ) %>%
      addCircleMarkers(
        data = priority_sf(),
        radius = 7,
        fillColor = "orange",   
        fillOpacity = 0.8,
        stroke = TRUE,
        color = "black",
        weight = 1,
        popup = ~paste0(
          "<b>", comName, "</b><br>",
          "Date: ", obsDt, "<br>",
          "Location: ", locName, "<br>",
          "Birds Spotted: ", howMany
        ),
        group = "Bird Sightings"
      ) %>%
      addLayersControl(
        overlayGroups = c("NDDB Areas", "Bird Sightings"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  
  # Heatmap of sightings
  output$heatmap_map <- renderLeaflet({
  pal_heat <- colorNumeric("YlOrRd", domain = NULL)

  leaflet(priority_sf()) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # Add NDDB polygons first (drawn underneath the heatmap)
    addPolygons(
      data = nddb,
      fillColor = "green",
      fillOpacity = 0.3,
      weight = 1,
      color = "darkgreen",
      group = "NDDB Areas"
    ) %>%
    
    # Add heatmap layer
    addHeatmap(
      lng = ~longitude,
      lat = ~latitude,
      radius = 30,
      blur = 1,
      max = 0.15
    ) %>%
    
    # Fit view to data
    fitBounds(
      lng1 = min(priority_sf()$longitude),
      lat1 = min(priority_sf()$latitude),
      lng2 = max(priority_sf()$longitude),
      lat2 = max(priority_sf()$latitude)
    ) %>%
    
    # Add heatmap legend
    addLegend(
      position = "bottomright",
      pal = pal_heat,
      values = c(0, 1),
      title = "Sightings Density",
      opacity = 0.7,
      labFormat = function(type, cuts, p) {
        paste0(round(cuts * 100), "%")
      }
    )
})

}

shinyApp(ui, server)
