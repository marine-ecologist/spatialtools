 # deployApp(appDir="/Users/rof011/spatialtools/apps/classify-rasters/",
 #           appPrimaryDoc="classify-rasters_0.1.R", appName="classify-rasters", appTitle="classify-rasters")



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

# Function to count NA values in a raster
count_na_cells <- function(raster_layer) {
  return(sum(is.na(values(raster_layer))))
}

# Function to save the raster to an RDS file
save_raster_to_rds <- function(raster_layer, file_path) {
  saveRDS(raster_layer, file_path)
}

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
        numericInput(ns("nBins"), "Number of Bins:", value = 4, min = 2, max = 6),
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
      singleLayerUI("layer1", "Chlorophyll"),
      width = 3
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "1200px"),
      width = 9
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Generate slider input for the layer
  output[["layer1-slidersUI"]] <- renderUI({
    num_sliders <- input[["layer1-nBins"]]
    
    # Get the range from the raster values
    r_min <- ceiling(min(values(r1), na.rm = TRUE))-1
    r_max <- floor(max(values(r1), na.rm = TRUE))+1
    breaks <- seq(r_min, r_max, length.out = num_sliders + 1)
    
    sliders <- lapply(1:num_sliders, function(i) {
      sliderInput(NS("layer1", paste0("slider", i)), "",  # Remove header
                  min = r_min, max = r_max, value = c(round(breaks[i], 0), round(breaks[i + 1], 0)))
    })
    do.call(tagList, sliders)
  })
  
  output[["layer1-histogram"]] <- renderPlot({
    values_r1 <- values(r1) |> as.vector()
    num_sliders <- input[["layer1-nBins"]]
    
    if (num_sliders > 1) {
      breaks <- sapply(1:num_sliders, function(i) {
        input[[NS("layer1", paste0("slider", i))]]
      })
      breaks <- unique(c(-Inf, unlist(breaks), Inf))
      class_labels <- as.factor(cut(values_r1, breaks = breaks, labels = FALSE))
      
      ggplot(data = data.frame(value = values_r1, class = class_labels), aes(x = value, fill = class)) +
        geom_histogram(color = "white", show.legend = FALSE, bins = 30) +  # Adjust bins to let ggplot2 decide the number of bins
        scale_fill_viridis_d() +          
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
        new_value <- c(new_min, max(new_min + 1, current_value[2]))
        
        updateSliderInput(session, NS("layer1", paste0("slider", i)), min = floor(min(values(r1), na.rm = TRUE)), max = ceiling(max(values(r1), na.rm = TRUE)), value = new_value)
      }, ignoreInit = TRUE)
    })
  })
  
  observe({
    req(input[["layer1-toggle"]])
    displayType <- input[["layer1-displayType"]]
    raster_layer <- r1
    
    if (displayType == "Continuous") {
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
        clearGroup("Chlorophyll") %>%
        addRasterImage(
          raster(raster_layer),
          colors = colorNumeric(palette = "viridis", domain = values(raster_layer), na.color = "transparent"),
          opacity = 0.8,
          group = "Chlorophyll"
        ) %>%
        addImageQuery(raster(raster_layer), 
                      type = "mousemove", 
                      digits =1, 
                      layerId = "Chlorophyll", 
                      Group = "Chlorophyll", 
                      project=FALSE, 
                      prefix="", 
                      position = "bottomright") 
        
    } else {
      observeEvent(input[["layer1-reclassify"]], {
        num_breaks <- as.numeric(input[["layer1-nBins"]])
        
        binBreaks <- sapply(1:num_breaks, function(j) {
          round(input[[NS("layer1", paste0("slider", j))]][2], 0)
        })
        
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
        
        # Save the raster to an RDS file
        #save_raster_to_rds(classified_raster, "classified_raster.rds")
        
        # Debugging output
        debug_info <- paste("Reclassification Matrix:\n", paste(apply(rcl, 1, function(x) paste(x, collapse = ", ")), collapse = "\n"), "\n\n",
                            "Original Values Range: ", min(values(raster_layer), na.rm = TRUE), " to ", max(values(raster_layer), na.rm = TRUE), "\n\n",
                            "NA cells count:", count_na_cells(classified_raster))
        
        # Output debugging information and NA count
        output[["layer1-naCount"]] <- renderText({
          paste("NA cells count:", count_na_cells(classified_raster))
        })
        
        output[["layer1-debugInfo"]] <- renderText({
          debug_info
        })
        
        classified_raster <- as.factor(classified_raster)
        levels <- levels(classified_raster)[[1]]$ID
        palette <- viridis(length(levels))
        color_pal <- colorFactor(palette = palette, domain = levels, na.color = "transparent")
        
        
        leafletProxy("map") %>%
          clearGroup("layer1") %>%
          addMouseCoordinates() %>%
          addImageQuery(raster(classified_raster), 
                        type = "mousemove", 
                        digits =1, 
                        layerId = "Chlorophyll", 
                        Group = "Chlorophyll", 
                        project=FALSE, 
                        prefix="", 
                        position = "bottomright") %>%
          addRasterImage(
            classified_raster,
            colors = color_pal,
            opacity = 0.8,
            group = "Chlorophyll",
            layerId = "Chlorophyll"
          ) %>%
          addLegend(pal = color_pal, values = levels, title = "Reclassified /nValues")
      })
    }
  })
  
  # Initialize the map
  output$map <- renderLeaflet({
    leaflet() %>%
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
      setView(lng = 145.445, lat = -14.695, zoom = 15)
  })
}

shinyApp(ui = ui, server = server)