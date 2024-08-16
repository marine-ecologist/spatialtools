library(sf)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(shiny)

create_plot <- function(lat, lng) {
  plot <- st_polygon(list(matrix(c(
    lng - 0.00045, lat - 0.00045,
    lng - 0.00045, lat + 0.00045,
    lng + 0.00045, lat + 0.00045,
    lng + 0.00045, lat - 0.00045,
    lng - 0.00045, lat - 0.00045
  ), ncol = 2, byrow = TRUE)))
  st_sfc(plot, crs = 4326)
}

ui <- fluidPage(
  leafletOutput("map", width="1600px", height = "1200px")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addDrawToolbar(
        targetGroup = 'plots',
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = FALSE
      ) %>%
      addLayersControl(
        overlayGroups = c('plots'),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      onRender("
        function(el, x) {
          this.on('draw:created', function(e) {
            var type = e.layerType,
                layer = e.layer;

            if (type === 'marker') {
              var latLng = layer.getLatLng();
              var plot = create_plot(latLng.lat, latLng.lng);

              L.geoJSON(plot).addTo(this);

              // Customize marker here
              layer.setIcon(L.icon({
                iconUrl: 'https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-icon.png',
                iconSize: [25, 41], // size of the icon
                iconAnchor: [12, 41], // point of the icon which will correspond to marker's location
                popupAnchor: [1, -34], // point from which the popup should open relative to the iconAnchor
                shadowUrl: 'https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-shadow.png',
                shadowSize: [41, 41] // size of the shadow
              }));
            }

            this.addLayer(layer);
          });

          function create_plot(lat, lng) {
            var plot = {
              'type': 'Polygon',
              'coordinates': [[
                [lng - 0.00045, lat - 0.00045],
                [lng - 0.00045, lat + 0.00045],
                [lng + 0.00045, lat + 0.00045],
                [lng + 0.00045, lat - 0.00045],
                [lng - 0.00045, lat - 0.00045]
              ]]
            };
            return plot;
          }
        }
      ") %>%
      setView(lng = 145.448, lat = -14.7004, zoom = 17)
  })
}

shinyApp(ui, server)