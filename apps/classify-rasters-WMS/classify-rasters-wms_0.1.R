 # deployApp(appDir="/Users/rof011/spatialtools/apps/classify-rasters-wms/",
 #           appPrimaryDoc="classify-rasters-wms_0.1.R", appName="classify-rasters-wms", appTitle="classify-rasters-wms")


library(shiny)
library(leaflet)
library(terra)
library(raster)
library(gstat)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(viridisLite)
library(leafem)
library(httr)
library(dplyr)
library(tidyterra)
library(leaflet.extras)
library(cmocean)
library(shinycssloaders)

# Helper function to create UI elements for a layer
singleLayerUI <- function(id, name) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("toggle"), label = paste("", name, " layer"), value = FALSE),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("toggle")),
      radioGroupButtons(
        inputId = ns("displayType"),
        label = "Display Type:",
        choices = c("Continuous", "Categorical"),
        status = "primary"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Categorical'", ns("displayType")),
        numericInput(ns("nBins"), "Number of Bins:", value = 3, min = 2, max = 6),
        plotOutput(ns("histogram"), height = "150px"),  # Adjusted height for 2/3 size
        uiOutput(ns("slidersUI")),
        actionButton(ns("reclassify"), "Reclassify"),
        verbatimTextOutput(ns("naCount")), # Output for NA count
        verbatimTextOutput(ns("debugInfo")) # Output for debugging information
      )
    )
  )
}

# Define UI
ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      singleLayerUI("layer1", "SST (MUR 0.01°)"),
      width = 3
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "1200px"),
      width = 9
    )
  )
)


server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "Basemap",
                       options = providerTileOptions(minZoom = 8, maxZoom = 21)) %>%
      setView(lng = 145.445, lat = -14.695, zoom = 13)
  })
  
  inputTrigger <- reactive({
    list(
      bounds = input$map_bounds,
      showCoralReefs = input$showCoralReefs
    )
  })
  
  ocean_data <- reactiveVal(NULL) # Initialize a reactive value
  
  observe({
    trigger <- inputTrigger()
    
    bounds <- input$map_bounds
    if (!is.null(bounds)) {
      lat_range <- c(bounds$south, bounds$north)
      lon_range <- c(bounds$west, bounds$east)
      
      tryCatch({
        erddap_url <- paste0(
          "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.csv?",
          "analysed_sst", "%5B(", Sys.Date() - 7, "T12:00:00Z):1:(", Sys.Date() - 7,
          "T12:00:00Z)%5D%5B(", lat_range[1], "):1:(", lat_range[2], ")%5D%5B(",
          lon_range[1], "):1:(", lon_range[2], ")%5D"
        )
        
        temp_erddap <- tempfile(fileext = ".csv")
        GET(url = erddap_url, write_disk(temp_erddap, overwrite = TRUE), timeout(60))
        
        # read csv and convert to raster
        csvdata <- read.csv(temp_erddap, header = TRUE, skip = 1) |>
          dplyr::select(3, 2, 4)
        
        r <- tidyterra::as_spatraster(csvdata, crs = "EPSG:4326")
        
        if (!is.null(r)) {
          ocean_data(r)
        } else {
          showNotification("Failed to load raster data.", type = "error")
        }
        
      }, error = function(e) {
        showNotification("No satellite data available for this date.", type = "error")
      })
      
      if (!is.null(ocean_data())) {
        raster_min <- minValue(raster(ocean_data())[[1]])
        raster_max <- maxValue(raster(ocean_data())[[1]])
        
        ocean_data_pal = colorNumeric(
          palette = "RdBu",
          na.color = "#d2f8f9",
          reverse = TRUE,
          domain = c(raster_min, raster_max)
        )
        
        id_name <- "analysed_sst"
        
        m <- leafletProxy("map") %>%
          clearGroup(id_name) %>%
          clearControls() %>%
          clearImages()
        
        m %>%
          addRasterImage(raster(ocean_data())[[1]], layerId = id_name, group = id_name,
                         opacity = 0.6, color = ocean_data_pal) %>%
          addLegend(pal = ocean_data_pal, position = "bottomright", values = seq(raster_min, raster_max, length.out = 8),
                    title = paste0("", "(", id_name, ")"), group = id_name) %>%
          addImageQuery(raster(ocean_data())[[1]], type = "mousemove", layerId = id_name, position = "topright") %>%
          addMouseCoordinates() %>%
          clearGroup("CoralReefs")
      }
    }
  })
  
  output[["layer1-slidersUI"]] <- renderUI({
    req(ocean_data()) # Ensure ocean_data is available
    
    num_sliders <- input[["layer1-nBins"]]
    
    r_min <- round(min(values(ocean_data()), na.rm = TRUE), 2)
    r_max <- round(max(values(ocean_data()), na.rm = TRUE), 2)
    breaks <- seq(r_min, r_max, length.out = num_sliders + 1)
    
    sliders <- lapply(1:num_sliders, function(i) {
      sliderInput(NS("layer1", paste0("slider", i)), "",  # Remove header
                  min = r_min, max = r_max, value = c(round(breaks[i], 2), round(breaks[i + 1], 2)), step = 0.02)
    })
    do.call(tagList, sliders)
  })
  
  output[["layer1-histogram"]] <- renderPlot({
    req(ocean_data()) # Ensure ocean_data is available
    
    values_r1 <- as.numeric(values(ocean_data()))
    num_sliders <- input[["layer1-nBins"]]
    
    if (num_sliders > 1) {
      breaks <- sapply(1:num_sliders, function(i) {
        input[[NS("layer1", paste0("slider", i))]]
      })
      breaks <- unique(c(-Inf, unlist(breaks), Inf))
      class_labels <- cut(values_r1, breaks = breaks, labels = FALSE)
      
      
      slidercolors <- colorRampPalette(RColorBrewer::brewer.pal(num_sliders, "RdBu"))(num_sliders+1)
      
      ggplot(data = data.frame(value = values_r1, class = as.factor(class_labels)), aes(x = value, fill = class)) +
        geom_histogram(color = "white", bins = 30, show.legend = FALSE) +  # Use bins = 30
        scale_fill_manual(values = slidercolors) +  # Dynamically set the number of colors
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    } else {
      ggplot(data = data.frame(value = values_r1), aes(x = value)) +
        geom_histogram(binwidth = 1, color = "white", fill = "skyblue") +
        theme_minimal() +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    }
  })
  
  observe({
    req(input[["layer1-nBins"]]) # Ensure numSliders input is available
    
    lapply(2:input[["layer1-nBins"]], function(i) {
      observeEvent(input[[NS("layer1", paste0("slider", i-1))]], {
        new_min <- input[[NS("layer1", paste0("slider", i-1))]][2]
        current_value <- input[[NS("layer1", paste0("slider", i))]]
        new_value <- c(new_min, max(new_min + 0.1, current_value[2]))
        
        updateSliderInput(session, NS("layer1", paste0("slider", i)), min = round(min(values(ocean_data()), na.rm = TRUE), 2), max = round(max(values(ocean_data()), na.rm = TRUE), 2), value = new_value)
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    req(input[["layer1-toggle"]])
    req(ocean_data()) # Ensure ocean_data is available
    displayType <- input[["layer1-displayType"]]
    raster_layer <- ocean_data()
    
    if (displayType == "Continuous") {
      raster_min <- minValue(raster(ocean_data())[[1]])
      raster_max <- maxValue(raster(ocean_data())[[1]])
      
      ocean_data_pal = colorNumeric(
        palette = "RdBu",
        na.color = "#d2f8f9",
        reverse = TRUE,
        domain = c(raster_min, raster_max)
      )
      
      id_name <- "analysed_sst"
      
      leafletProxy("map") %>%
        addProviderTiles(providers$Esri.WorldImagery, 
                         group = "Basemap",
                         options = providerTileOptions(minZoom = 8, maxZoom = 21)) %>%
        addSimpleGraticule(
          showOriginLabel = TRUE,
          redraw = "move",
          hidden = FALSE,
          zoomIntervals = list(
            list(start = 1, end = 3, interval = 10),
            list(start = 4, end = 9, interval = 1),
            list(start = 10, end = 16, interval = 0.1),
            list(start = 17, end = 20, interval = 0.00089831182) # 100m in degrees at equator
          ),
          layerId = NULL,
          group = NULL
        ) %>%
        clearGroup("SST") %>%
        addRasterImage(
          raster(raster_layer),
          colors = ocean_data_pal,
          opacity = 0.8,
          group = "SST"
        ) %>%
        addImageQuery(raster(raster_layer), 
                      type = "mousemove", 
                      digits = 1, 
                      layerId = "SST", 
                      group = "SST", 
                      project = FALSE, 
                      prefix = "", 
                      position = "bottomright")
    } else {
      observeEvent(input[["layer1-reclassify"]], {
        req(ocean_data()) # Ensure ocean_data is available
        num_breaks <- as.numeric(input[["layer1-nBins"]])
        
        binBreaks <- sapply(1:num_breaks,          function(j) {
          round(input[[NS("layer1", paste0("slider", j))]][2], 2)
        }
        )
        
        # Ensure no gaps between breaks
        binBreaks <- sort(binBreaks)
        binBreaks <- unique(binBreaks)  # Remove duplicates if any
        
        # Adjust bin breaks to be contiguous
        rcl <- matrix(nrow = num_breaks, ncol = 3)
        for (i in 1:num_breaks) {
          rcl[i, 1] <- if (i == 1) -Inf else binBreaks[i-1]
          rcl[i, 2] <- binBreaks[i]
          rcl[i, 3] <- i
        }
        rcl <- rbind(rcl, c(binBreaks[num_breaks], Inf, num_breaks + 1))
        
        classified_raster <- classify(raster_layer, rcl)
        
        classified_raster <- as.factor(classified_raster)
        levels <- levels(classified_raster)[[1]]$ID
        palette <- "RdBu"
        color_pal <- colorFactor(palette = palette, domain = levels, na.color = "transparent")
        
        leafletProxy("map") %>%
          clearGroup("SST") %>%
          addMouseCoordinates() %>%
          addImageQuery(raster(classified_raster), 
                        type = "mousemove", 
                        digits = 1, 
                        layerId = "SST", 
                        group = "SST", 
                        project = FALSE, 
                        prefix = "", 
                        position = "bottomright") %>%
          addRasterImage(
            classified_raster,
            colors = color_pal,
            opacity = 0.8,
            group = "SST",
            layerId = "SST"
          ) %>%
          addLayersControl(overlayGroups = "SST") %>%
          addLegend(pal = color_pal, values = levels, title = "Reclassified Values")
      })
    }
  })
}

shinyApp(ui = ui, server = server)