library(shiny)
library(shinyWidgets)
library(leaflet)
library(httr)
library(raster)
library(fontawesome)
library(mapview)
library(leafem)
library(leaflet.extras)
library(rnaturalearthhires)
library(cmocean)
library(shinycssloaders)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-app-container {
        max-width: 1200px;
        margin: auto;
        align: center;
      }
      html, body {
        height: 100%;
      }
      .container-fluid {
        height: 100%;
      }
      #map {
        height: 1000px; 
      }
      .main-title {
        text-align: center;
      }
      .shiny-input-container .switch label {
        font-size: 6px; 
      }
    "))
  ),
  div(class = "main-title", titlePanel("")),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Ocean Data Viewer"),
      tags$p("Interactive map for daily sea surface temperature data via ",
             tags$a(href = "https://coastwatch.pfeg.noaa.gov/erddap/index.html",
                    "NOAA data servers", target = "_blank")),
      tags$p("."),
      
      ### Set location
      selectInput("dataLocation", HTML(paste(fa("globe"), "Start location:")),
                  choices = list("Great Barrier Reef" = "GBR", "Ningaloo" = "Ningaloo")),
      
      ### Set data type
      selectInput("dataType", HTML(paste(fa("code-branch"), "Data source:")),
                  choices = list(
                    "Sea Surface Temperatures (NOAA)" = "CRW_SST",
                    "Sea Surface Temperature Anomalies (NOAA)" = "CRW_SSTANOMALY",
                    "Degree Heating Weeks (NOAA)" = "CRW_DHW"
                  )),
      
      ### Set date
      dateInput("date", HTML(paste(fa("calendar"), "Date:")), value = Sys.Date() - 7),
      
      ### Set map opacity
      sliderInput("opacity", HTML(paste(fa("wand-magic-sparkles"), "Map Transparency")),
                  min = 0, max = 1, value = 0.6, step = 0.1),
      
      ### Set base maps
      tags$p(HTML(paste(fa("clone"), "<strong>Base maps:</strong>"))),
      
      switchInput("showCoralReefs", label = "Coral Atlas", value = FALSE, size = "mini", labelWidth="200"),
      switchInput("showSatelliteImage", label = "Satellite Image", value = FALSE, size = "mini", labelWidth="200"),
      
      ### Set location
      
      tags$p(HTML(paste(fa("thermometer-2"), "<strong>Color scale:</strong>"))),
      
      switchInput("useRange", label = "Fixed range", value = FALSE, size = "mini", labelWidth="200"),
      
      sliderInput("rng", "", value = c(18, 36), step = 0.5, min = 18, max = 36),
      
      tags$a(href = "mailto:george.roff@csiro.au", HTML(paste(fa("envelope"), " Contact")), target = "_blank"),
      width = 2
    ),
    
    mainPanel(
      leafletOutput("map", width = "100%", height = "1000px") %>%
        withSpinner(type = 6),
      width = 8,
      height = 8
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive logic to define appropriate range based on selected data type
  reactiveRange <- reactive({
    switch(input$dataType,
           "CRW_SST" = c(18, 36),
           "CRW_SSTANOMALY" = c(-4.5, 4.5),
           "CRW_DHW" = c(0, 18))
  })
  
  # Observe changes in dataType and update the slider's min and max
  observeEvent(input$dataType, {
    range_vals <- reactiveRange()  # Get the range for the selected data type
    updateSliderInput(session, "rng",
                      min = range_vals[1],
                      max = range_vals[2],
                      value = c(range_vals[1], range_vals[2]))  # Set the default range
  })
  
  reactiveLocation <- reactive({
    switch(input$dataLocation,
           "GBR" = c(145.425, -14.675, 9),
           "Ningaloo" = c(113.3, -21.55, 8))
  })
  
  output$map <- renderLeaflet({
    loc <- reactiveLocation()
    leaflet() %>%
      addTiles(options = tileOptions(maxZoom = 12)) %>%
      setView(lng = loc[1], lat = loc[2], zoom = loc[3])
  })
  
  inputTrigger <- reactive({
    list(
      bounds = input$map_bounds,
      date = input$date,
      dataType = input$dataType,
      showCoralReefs = input$showCoralReefs,
      showSatelliteImage = input$showSatelliteImage,
      opacity = input$opacity,
      useRange = input$useRange,
      userRange = input$rng
    )
  })
  
  observe({
    trigger <- inputTrigger()
    
    bounds <- input$map_bounds
    if (!is.null(bounds)) {
      lat_range <- c(bounds$south, bounds$north)
      lon_range <- c(bounds$west, bounds$east)
      
      tryCatch({
        erddap_url <- paste0(
          "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.csv?",
          input$dataType, "%5B(", input$date, "T12:00:00Z):1:(", input$date,
          "T12:00:00Z)%5D%5B(", lat_range[1], "):1:(", lat_range[2], ")%5D%5B(",
          lon_range[1], "):1:(", lon_range[2], ")%5D"
        )
        
        temp_erddap <- tempfile(fileext = ".csv")
        GET(url = erddap_url, write_disk(temp_erddap, overwrite = TRUE), timeout(60))
        
        # read csv and convert to raster
        csvdata <- read.csv(temp_erddap, header = TRUE, skip = 1) |>
          dplyr::select(3, 2, 4)
        
        
        ocean_data <- tidyterra::as_spatraster(csvdata, crs = "EPSG:4326") |>
          raster::raster()
        
        file_size_formated <- function(size){
          
          k = size/1024.0 ^ 1
          m = size/1024.0 ^ 2
          g = size/1024.0 ^ 3
          t = size/1024.0 ^ 4
          
          if (t > 1) {
            outSize = paste0(round(t,2),"TB")
          } else if (g > 1) {
            outSize = paste0(round(g,2),"GB")
          } else if (m > 1) {
            outSize = paste0(round(m,2),"MB")
          } else if (k > 1) {
            outSize = paste0(round(k,2),"KB")
          } else{
            outSize = paste0(round(size,2),"B")
          }
          
          return(outSize)
        }
        
        
      }, error = function(e) {
        # Handle missing data and incorrect dates
        showNotification("No satellite data available for this date.", type = "error")
      })
      
      # Calculate the range for legend values based on user input or raster data
      if (input$useRange) {
        # Use manually specified range from sliderInput
        raster_min <- input$rng[1]
        raster_max <- input$rng[2]
      } else {
        # Use dynamic range from raster data
        raster_min <- minValue(ocean_data[[1]])
        raster_max <- maxValue(ocean_data[[1]])
      }
      
      num_breaks = 8
      step_size = (raster_max - raster_min) / (num_breaks - 1)
      
      # Generate the legend values
      legend_values = seq(from = raster_min, to = raster_max, by = step_size)
      
      
      if (input$dataType=="CRW_DHW"){
        
        DHWbins <- 0:20
        DHWcolors <- c("0"="#433374", "1"="#615192", "2"="#7f6faf", "3"="#9d8dcd", "4"="#ffff54",
                       "5"="#f9dd4a", "6"="#f4bc41", "7"="#f19b38", "8"="#ea3323", "9"="#c0281b",
                       "10"="#921c12", "11"="#641009", "12"="#d88252", "13"="#a85f34", "14"="#754025",
                       "15"="#502f19", "16"="#dc2fe8", "17"="#b726c1", "18"="#921c9b", "19"="#6e1274", "20"="#2d0330")
                       ocean_data_pal <- colorBin(palette = DHWcolors,
                                                  domain = DHWbins,
                                                  bins = 20,
                                                  na.color = "transparent")
                       
                       } else if (input$dataType == "CRW_SSTANOMALY") {
                         
                         ocean_data_pal = colorNumeric(
                           palette = "RdYlBu",
                           na.color = "#d2f8f9",
                           reverse = TRUE,
                           domain = c(raster_min, raster_max)
                         )
                         
                       } else if (input$dataType == "CRW_SST") {
                         
                         ocean_data_pal = colorNumeric(
                           palette = "RdBu",
                           na.color = "#d2f8f9",
                           reverse = TRUE,
                           domain = c(raster_min, raster_max)
                         )
                       }
      
      id_name <- names(csvdata)[3]
      
      m <- leafletProxy("map") %>%
        clearGroup(id_name) %>%
        clearControls() %>%
        clearImages()
      
      if (isTRUE(input$showCoralReefs)) {
        m <- m %>%
          addWMSTiles(
            baseUrl = "https://allencoralatlas.org/geoserver/ows?",
            layers = "coral-atlas:benthic_data_verbose", # Example layer, replace with the actual layer name you need
            options = WMSTileOptions(
              format = "image/png", # Request PNG format
              transparent = TRUE,
              version = "1.3.0",
              crs = CRS("EPSG:4326")
            ),
            attribution = "© Allen Coral Atlas",
            group = id_name
          )
      }
      
      if (isTRUE(input$showSatelliteImage)) {
        m <- m %>%
          addProviderTiles("Esri.WorldImagery", group = id_name)
      }
      
      m %>%
        addRasterImage(ocean_data[[1]], layerId = id_name, group = id_name,
                       opacity = input$opacity, color = ocean_data_pal) %>%
        addLegend(pal = ocean_data_pal, position = "bottomright", values = legend_values,
                  title = paste0(input$dataType, " (", id_name, ")"), group = id_name) %>%
        addImageQuery(ocean_data[[1]], type = "mousemove", layerId = id_name, position = "topright") %>%
        addMouseCoordinates() %>%
        clearGroup("CoralReefs")
      }
    })
        }

shinyApp(ui, server)