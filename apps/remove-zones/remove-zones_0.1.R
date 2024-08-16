
 # deployApp(appDir="/Users/rof011/spatialtools/apps/remove-zones/",
 #           appPrimaryDoc="remove-zones_0.1.R", appName="remove-zones", appTitle="remove-zones")


library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(geojsonio)
library(terra)
library(raster)
library(gstat)
library(RColorBrewer)

# Define color palette for habitats
habitat_pal <-  c(
  "Plateau" = "cornsilk2",
  "Back Reef Slope" = "darkcyan",
  "Reef Slope" = "darkseagreen4",
  "Sheltered Reef Slope" = "darkslategrey",
  "Inner Reef Flat" = "darkgoldenrod4",
  "Outer Reef Flat" = "darkgoldenrod2",
  "Reef Crest" = "coral3"
)

# Read the lizard map GeoJSON and process it
lizard_map <- st_read("www/geomorphic.geojson", quiet = TRUE, crs = 4326) |>
  mutate(val = case_when(
    class == "Plateau" ~ 4,
    class == "Back Reef Slope" ~ 5,
    class == "Reef Slope" ~ 2,
    class == "Sheltered Reef Slope" ~ 5,
    class == "Inner Reef Flat" ~ 1,
    class == "Outer Reef Flat" ~ 2,
    class == "Reef Crest" ~ 2,
    TRUE ~ 0  # For any other values, assign 0
  )) |>
  filter(class %in% c("Plateau", "Back Reef Slope", "Reef Slope", "Sheltered Reef Slope", 
                      "Inner Reef Flat", "Outer Reef Flat", "Reef Crest"))

# Create a color palette for the classes
colorPalette <- colorFactor(
  palette = habitat_pal,
  domain = lizard_map$class
)

# Function to create the SpatRaster
create_aggregated_raster <- function(xmin, xmax, ymin, ymax, seed, model, beta, psill, range, nmax, gridsize) {
  set.seed(seed)  # Set the seed for reproducibility
  bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax) # Define the bounding box
  res <- gridsize / 111000  # Create a raster template with the specified grid size
  r <- raster(xmn = bbox["xmin"], xmx = bbox["xmax"], ymn = bbox["ymin"], ymx = bbox["ymax"], res = res)
  
  grid <- as.data.frame(xyFromCell(r, 1:ncell(r)))  # Define a spatial grid using the same resolution as the raster
  coordinates(grid) <- ~x+y
  gridded(grid) <- TRUE
  
  # Generate a Gaussian random field with spatial correlation
  gstat_model <- gstat(formula = z~1, locations = ~x+y, dummy = TRUE,
                       beta = beta, model = vgm(psill = psill^2, model = model, range = range), nmax = nmax)
  sim <- predict(gstat_model, newdata = grid, nsim = 1)
  grid$sim1 <- sim$sim1   # Add the simulated values to the grid
  
  # Ensure the simulated data matches the raster dimensions
  if (length(grid$sim1) != ncell(r)) {
    stop("The number of simulated values does not match the number of raster cells.")
  }
  
  r[] <- grid$sim1 # Convert the simulated data to a raster
  
  return(terra::rast(r))
}

# Create the SpatRaster r1
r1 <- create_aggregated_raster(xmin = 145.43, xmax = 145.46, ymin = -14.71, ymax = -14.68, model="Exp", seed = 101, beta = 10, psill = 2.5, range = 0.01, nmax = 12, gridsize = 20) + 14

# Convert SpatRaster to a Leaflet-compatible tile layer
# r1_pal <- colorNumeric(c("blue", "yellow", "red"), values(r1), na.color = "transparent")

r1_pal <- colorNumeric(
  palette = rev(brewer.pal(11, "RdBu")),  # Reverse the palette if you want red for high values and blue for low
  domain = values(r1),
  na.color = "transparent"
)

ui <- fluidPage(
  titlePanel(""),
  leafletOutput("map", height = 1000)
)

server <- function(input, output, session) {
  
  # Initialize the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lng = -0.09, lat = 51.505, zoom = 13) %>%
      addPolygons(data = lizard_map,
                  fillColor = ~colorPalette(class),
                  color = "#BDBDC3",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.7,
                  group = "Lizard Map") %>%
      addRasterImage(r1, colors = r1_pal, opacity = 0.7, group = "Raster Layer") %>%
      addDrawToolbar(
        targetGroup = "Zones",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polylineOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = TRUE,
        polygonOptions = TRUE,
        circleMarkerOptions = FALSE,
        markerOptions = FALSE
      ) %>%
      addLayersControl(
        overlayGroups = c("Zones", "Lizard Map", "Raster Layer"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Basemap",
                       options = providerTileOptions(minZoom = 8, maxZoom = 21)) %>%
      setView(lng = 145.445, lat = -14.695, zoom = 15)
  })
  
  # Observe the drawing event and handle the crop logic
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    coords <- feature$geometry$coordinates[[1]]
    
    # Convert drawn polygon to sf object
    coords_matrix <- do.call(rbind, lapply(coords, function(coord) {
      c(as.numeric(coord[1]), as.numeric(coord[2]))
    }))
    
    drawn_polygon <- st_polygon(list(coords_matrix))
    drawn_sf <- st_sfc(drawn_polygon, crs = 4326)
    
    # Convert sf object to SpatVector for terra operations
    drawn_vect <- vect(drawn_sf)
    
    # Apply inverse masking to the raster using the drawn polygon
    r1_inverse_masked <- mask(r1, drawn_vect, inverse = TRUE)
    
    # Update the map with the inverse masked raster
    leafletProxy("map") %>%
      clearGroup("Raster Layer") %>%
      addRasterImage(r1_inverse_masked, colors = r1_pal, opacity = 0.7, group = "Raster Layer")
    
    # Perform the inverse masking on the vector layer
    # Create a 1000km buffer around the bounding box of the lizard_map
    bbox <- st_bbox(lizard_map)
    bbox_polygon <- st_as_sfc(st_bbox(lizard_map)) %>%
      st_transform(crs = st_crs(lizard_map)) %>%
      st_buffer(dist = 1000000)  # Buffer by 1000 km
    
    # Subtract the drawn polygon from the buffered area to get the inverse mask
    inverse_mask <- st_difference(bbox_polygon, drawn_sf)
    
    # Intersect the lizard_map with the inverse mask to retain only areas outside the drawn polygon
    lizard_cropped <- st_intersection(lizard_map, inverse_mask)
    
    # Update the vector layer on the map
    leafletProxy("map") %>%
      clearGroup("Lizard Map") %>%
      addPolygons(data = lizard_cropped,
                  fillColor = ~colorPalette(class),
                  color = "#BDBDC3",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.7,
                  group = "Lizard Map")
  })
}

shinyApp(ui = ui, server = server)