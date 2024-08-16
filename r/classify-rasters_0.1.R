library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(geojsonio)
library(terra)
library(sp)
library(gstat)

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
r1_pal <- colorNumeric(c("blue", "yellow", "red"), values(r1), na.color = "transparent")

ui <- fluidPage(
  titlePanel("Leaflet Drawing and Inverse Masking in Shiny"),
  leafletOutput("map", height = 600)
)

server <- function(input, output, session) {
  
  # Initialize the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 145.445, lat = -14.695, zoom = 13) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Basemap",
                       options = providerTileOptions(minZoom = 8, maxZoom = 21)) %>%
      addRasterImage(r1, colors = r1_pal, opacity = 0.7, group = "Raster Layer") %>%
      addDrawToolbar(
        targetGroup = "drawn",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polylineOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = TRUE,
        polygonOptions = TRUE,
        circleMarkerOptions = FALSE,
        markerOptions = FALSE
      ) %>%
      addLayersControl(
        overlayGroups = c("drawn", "Raster Layer"),
        options = layersControlOptions(collapsed = FALSE)
      )
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
    
    # Create an inverse mask by using mask with inverse = TRUE
    r1_inverse_masked <- mask(r1, drawn_vect, inverse = TRUE)
    
    # Add the inverse masked raster to the map
    leafletProxy("map") %>%
      clearGroup("Raster Layer") %>%
      addRasterImage(r1_inverse_masked, colors = r1_pal, opacity = 0.7, group = "Raster Layer")
  })
}

shinyApp(ui = ui, server = server)