# deployApp(appDir="/Users/rof011/spatialtools/apps/rotate-polygons/",
#           appPrimaryDoc="rotate-polygons_0.1.R", appName="rotate-polygons", appTitle="rotate-polygons")


library(shiny)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)

# Define the Leaflet.draw.rotate plugin as a dependency
rotatePlugin <- htmlDependency(
  name = "leaflet-draw-rotate",
  version = "1.0.0",
  src = c(file = "dist"),
  script = c("leaflet-draw-rotate.js", "Edit.Rectangle.Rotate.js"),
  stylesheet = "demo.css"
)

# Function to register the plugin on the map
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

ui <- fluidPage(
  useShinyjs(),
  leafletOutput("map", height = "1200px")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = TRUE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Basemap",
                       options = providerTileOptions(minZoom = 8, maxZoom = 21)) %>%
      setView(lng = 145.445, lat = -14.695, zoom = 13) %>%
      registerPlugin(rotatePlugin) %>%
      onRender(
        "function(el, x) {
          L.Edit.Rectangle.prototype.setOptions({ uniformScaling: false });
          var rotateControl = new L.Control.Draw.Rotate();
          this.addControl(rotateControl);
        }"
      )
  })
}

shinyApp(ui, server)