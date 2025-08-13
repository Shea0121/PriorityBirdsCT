library(shiny)
library(dplyr)
library(rebird)
library(sf)
library(leaflet)
library(leaflet.extras)
library(gt)
library(lubridate)
library(bslib)
library(shinycssloaders)


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
    
    tabPanel(
      "Sightings Frequency",
      withSpinner(
        gt_output("freq_table"),
        type = 4,
        color = "#2C6E49"
      )
    ),
    
    tabPanel("Sightings & Conservation Map",
             wellPanel(
               h4("About This Map"),
               p("This interactive map displays recent sightings of Connecticut's priority bird species 
              from the last 30 days. Each dot represents a sighting of one of the 29 listed species, 
              with the dot color showing its conservation status (green = Low Concern, orange = Near Threatened, 
              red = High Concern, dark red = Vulnerable)."),
               p("The green shaded areas come from the Natural Diversity Data Base (NDDB), which highlights 
              approximate locations of endangered, threatened, and special concern species, as well as 
              important natural communities in Connecticut."),
               p("Click a dot to see details such as species name, date, location, and number of birds observed.")
             ),
             withSpinner(
               leafletOutput("sightings_map", height = 600),
               type = 4, color = "#2C6E49"
             )
    ),
    
    tabPanel("Heatmap of Sightings",
             wellPanel(
               h4("About This Heatmap"),
               p("This map shows the density of recent priority bird sightings in Connecticut from the last 30 days. 
              Darker blue areas indicate higher concentrations of sightings, while lighter blue areas indicate fewer sightings."),
               p("The NDDB polygons are shown in green, just like in the first map, to provide context 
              for areas of special conservation interest."),
               p("Use zoom and pan to explore, and compare the heatmap with the sightings map to spot patterns.")
             ),
             withSpinner(
               leafletOutput("heatmap_map", height = 600),
               type = 4, color = "#2C6E49"
             )
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
    recent <- ebirdregion(loc = "US-CT", back = 30, max = 5000)
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
  
  
  # Reactive sf object with status column
  priority_sf <- reactive({
    # Get the current value of priority_recent
    df <- priority_recent()
    
    # Add status column
    df <- df %>%
      mutate(
        status = case_when(
          comName %in% c("Sedge Wren", "Bobolink", "Piping Plover", "Wood Thrush", "Upland Sandpiper") ~ "Near Threatened",
          comName %in% c("Saltmarsh Sparrow") ~ "High Concern",
          comName %in% c("Cerulean Warbler") ~ "Vulnerable",
          comName %in% c(
            "American Black Duck", "American Oystercatcher", "Bald Eagle", 
            "Black-crowned Night-Heron", "Least Tern", "Northern Harrier", 
            "Prairie Warbler", "Eastern Meadowlark", "Great Egret", 
            "Northern Bobwhite", "American Woodcock", "Marbled Godwit", 
            "Swainson's Hawk", "Eastern Whip-poor-will", "Ruby-throated Hummingbird", 
            "Black-billed Cuckoo", "Scarlet Tanager", "White-eyed Vireo", 
            "Common Nighthawk", "Little Blue Heron", "Barn Owl", "Grasshopper Sparrow"
          ) ~ "Low Concern",
          TRUE ~ NA_character_
        )
      )
    
    # Convert to sf with coordinates
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
    # Color palette for conservation status
    pal_status <- reactive({
      colorFactor(
        palette = c("green", "darkred", "orange", "red"),  # colors in your desired order
        levels = c("Low Concern", "Vulnerable", "Near Threatened", "High Concern")
      )
    })
    
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
        fillColor = ~pal_status()(status), # color by status
        fillOpacity = 0.8,
        stroke = TRUE,
        color = "black",
        weight = 1,
        popup = ~paste0(
          "<b>", comName, "</b><br>",
          "Date: ", obsDt, "<br>",
          "Location: ", locName, "<br>",
          "Birds Spotted: ", howMany, "<br>",
          "Conservation Status: ", status  # added status
        ),
        group = "Bird Sightings"
      ) %>%
      addLayersControl(
        overlayGroups = c("NDDB Areas", "Bird Sightings"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        "bottomright",
        pal = pal_status(),
        values = priority_sf()$status,
        title = "Conservation Status"
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
      pal_heat <- colorNumeric(c("lightblue","skyblue","blue","navy"), domain = NULL),
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
