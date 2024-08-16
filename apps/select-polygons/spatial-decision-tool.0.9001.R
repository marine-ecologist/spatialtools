# deployApp(appDir="/Users/rof011/spatialtools/apps/select-polygons/",
#           appPrimaryDoc="spatial-decision-tool.0.9001.R", appName="spatial-decision-tool", appTitle="spatial-decision-tool")


library(tidyverse)
library(raster)
library(gstat)
library(sp)
library(terra)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(htmltools)
library(htmlwidgets)
library(sf)
library(patchwork)
library(RColorBrewer)
library(ggplot2)
library(tidyterra)


###### 1.  Read in vector data


habitat_pal <-  c(
  "Plateau" = "cornsilk2",
  "Back Reef Slope" = "darkcyan",
  "Reef Slope" = "darkseagreen4",
  "Sheltered Reef Slope" = "darkslategrey",
  "Inner Reef Flat" = "darkgoldenrod4",
  "Outer Reef Flat" = "darkgoldenrod2",
  "Reef Crest" = "coral3"
)

lizard_map <- st_read("www/geomorphic.geojson", quiet = TRUE, crs = 4326) |>
  mutate(val = case_when(
    class == "Plateau" ~ 4,
    class == "Back Reef Slope" ~ 5,
    class == "Reef Slope" ~ 2,
    class == "Sheltered Reef Slope" ~ 5,
    class == "Inner Reef Flat" ~ 1,
    class == "Outer Reef Flat" ~ 2,
    class == "Reef Crest" ~ 2,
    TRUE ~ 0  # For any other values, assign NA
  )) |> 
  filter(class %in% c("Plateau", "Back Reef Slope", "Reef Slope", "Sheltered Reef Slope", 
                      "Inner Reef Flat", "Outer Reef Flat","Reef Crest"))

# convert to raster

sf_to_raster <- function(sf_object, xmin, xmax, ymin, ymax, gridsize) {
  # Calculate the resolution
  res <- gridsize / 111000
  
  # Create an empty raster with the specified boundaries and resolution
  raster_template <- rast(nrows = round((ymax - ymin) / res),
                          ncols = round((xmax - xmin) / res),
                          xmin = xmin, xmax = xmax,
                          ymin = ymin, ymax = ymax,
                          crs = "EPSG:4326")
  
  # Rasterize the sf object using the 'val' field
  raster_result <- rasterize(sf_object, raster_template, field = "val", fun = mean, background = NA)
  
  return(raster_result)
}

lizard_raster <- sf_to_raster(lizard_map, xmin = 145.43, xmax = 145.46, ymin = -14.71, ymax = -14.68, gridsize = 20)

palfrey_se_sf <- st_sf(geometry = st_sfc(st_point(c(145.448641, -14.700424)), crs = 4326))




colorPalette <- colorFactor(
  palette = habitat_pal,
  domain = lizard_map$class
)




###### 2.  Read in Raster data


### Simulate Raster data
create_aggregated_raster <- function(xmin, xmax, ymin, ymax, seed, model, beta, psill, range, nmax, gridsize){
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

# Simulate raster data
r1 <- create_aggregated_raster(xmin = 145.43, xmax = 145.46, ymin = -14.71, ymax = -14.68, model="Exp", seed = 101, beta = 10, psill = 2.5, range = 0.01, nmax = 12, gridsize = 20) + 14
r2 <- create_aggregated_raster(xmin = 145.43, xmax = 145.46, ymin = -14.71, ymax = -14.68, model="Exp", seed = 101, beta = 40, psill = 10, range = 0.05, nmax = 20, gridsize = 50)
r3 <- create_aggregated_raster(xmin = 145.43, xmax = 145.46, ymin = -14.71, ymax = -14.68, model="Cir", seed = 101, beta = 40, psill = 10, range = 0.05, nmax = 20, gridsize = 50)
r4 <- create_aggregated_raster(xmin = 145.43, xmax = 145.46, ymin = -14.71, ymax = -14.68, model="Mat", seed = 101, beta = 100, psill = 20, range = 0.0005, nmax = 100, gridsize = 125)
r5 <- create_aggregated_raster(xmin = 145.43, xmax = 145.46, ymin = -14.71, ymax = -14.68, model="Cir", seed = 101, beta = 40, psill = 10, range = 0.0005, nmax = 100, gridsize = 100)
r6 <- create_aggregated_raster(xmin = 145.43, xmax = 145.46, ymin = -14.71, ymax = -14.68,  model="Cir", seed = 101, beta = 40, psill = 10, range = 0.005, nmax = 100, gridsize = 200)

r1 <- raster::raster(r1)
r2 <- raster::raster(r2)
r3 <- raster::raster(r3)
r4 <- raster::raster(r4)
r5 <- raster::raster(r5)
r6 <- raster::raster(r6)


# Classify rasters into ordinal bins


# Function to categorize rasters

# Function to categorize rasters
cut_raster_bins <- function(input, rev=FALSE) {
  r_min <- min(values(input), na.rm = TRUE)
  r_max <- max(values(input), na.rm = TRUE)
  breaks <- seq(r_min, r_max, length.out = 6)  # Create breaks for the bins (0 to 5, even split)
  
  if(isTRUE(rev)){
    rcl <- matrix(c(-Inf, breaks[2], 1,   # Adjust the classification matrix to include all values
                    breaks[2], breaks[3], 2,
                    breaks[3], breaks[4], 3,
                    breaks[4], breaks[5], 4,
                    breaks[5], Inf, 5),
                  ncol = 3, byrow = TRUE)
    
    classified_raster <- classify(input, rcl = rcl)  # Classify the raster
    
    # Convert to categorical (factor) raster with ordered levels
    categories <- factor(classified_raster[], 
                         levels = 1:5, 
                         labels = c("low", "medium-low", "medium", "medium-high", "high"), 
                         ordered = TRUE)
  } else {
    rcl <- matrix(c(-Inf, breaks[2], 5,   # Adjust the classification matrix to include all values
                    breaks[2], breaks[3], 4,
                    breaks[3], breaks[4], 3,
                    breaks[4], breaks[5], 2,
                    breaks[5], Inf, 1),
                  ncol = 3, byrow = TRUE)
    
    classified_raster <- classify(input, rcl = rcl)  # Classify the raster
    
    # Convert to categorical (factor) raster with ordered levels
    categories <- factor(classified_raster[], 
                         levels = 1:5, 
                         labels = rev(c("low", "medium-low", "medium", "medium-high", "high")), 
                         ordered = TRUE)
  }  
  
  
  categorized_raster <- setValues(classified_raster, categories)
  
  # Add the 'category' level
  #categorized_raster$category <- c(categorized_raster, category = categories)
  
  return(categorized_raster)
}
  
  

r1_bins <- cut_raster_bins(rast(r1))
r2_bins <- cut_raster_bins(rast(r2))
r3_bins <- cut_raster_bins(rast(r3))
r4_bins <- cut_raster_bins(rast(r4))
r5_bins <- cut_raster_bins(rast(r5))
r6_bins <- cut_raster_bins(rast(r6))


# setup color pals
r1_colors <- colorNumeric(palette = rev(brewer.pal(9, "RdYlBu")), domain = values(r1), na.color = "transparent")
r2_colors <- colorNumeric(palette = rev(brewer.pal(9, "BrBG")), domain = values(r2), na.color = "transparent")
r3_colors <- colorNumeric(palette = rev(brewer.pal(9, "RdGy")), domain = values(r3), na.color = "transparent")
r4_colors <- colorNumeric(palette = brewer.pal(9, "Blues"), domain = values(r4), na.color = "transparent")
r5_colors <- colorNumeric(palette = brewer.pal(9, "Greens"), domain = values(r5), na.color = "transparent")
r6_colors <- colorNumeric(palette = brewer.pal(9, "PiYG"), domain = values(r6), na.color = "transparent")


###### 2.  Comine and resample spatial datalayers

# main rasters:
r1_resampled <- r1_bins
r2_resampled <- resample(r2_bins, r1_bins)
r3_resampled <- resample(r3_bins, r1_bins)
r4_resampled <- resample(r4_bins, r1_bins)
r5_resampled <- resample(r5_bins, r1_bins)
r6_resampled <- resample(r6_bins, r1_bins)


# vector
lizard_raster_resampled <- resample(lizard_raster, r1_bins)

# Create a mask that identifies NA cells non-reef
na_mask <- is.na(lizard_raster_resampled) 

# set NA mask over rasters
r1_resampled[na_mask] <- NA
r2_resampled[na_mask] <- NA
r3_resampled[na_mask] <- NA
r4_resampled[na_mask] <- NA
r5_resampled[na_mask] <- NA
r6_resampled[na_mask] <- NA
lizard_raster_resampled[na_mask] <- NA

# r1_resampled <- raster::raster(r1_resampled)
# r2_resampled <- raster::raster(r2_resampled)
# r3_resampled <- raster::raster(r3_resampled)
# r4_resampled <- raster::raster(r4_resampled)
# r5_resampled <- raster::raster(r5_resampled)
# r6_resampled <- raster::raster(r6_resampled)
# lizard_raster_resampled <- raster::raster(lizard_raster_resampled)
cat_final <- raster::raster((lizard_raster_resampled + r1_resampled + r2_resampled + r3_resampled + r4_resampled + r5_resampled + r6_resampled) / 6)
cat_final_colors <- colorNumeric(palette = brewer.pal(9, "RdYlBu"), domain = values(cat_final), na.color = "transparent")



### Initiate Leaflet
# Define Leaflet.draw.rotate plugin as a dependency
rotatePlugin <- htmlDependency(
  name = "leaflet-draw-rotate",
  version = "1.0.0",
  src = c(file = "dist"),
  script = c("leaflet-draw-rotate.js", "Edit.Rectangle.Rotate.js"),
  stylesheet = "demo.css"
)

# Register the plugin on the map
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

# Create color palettes using RColorBrewer

ui <- fluidPage(
  tags$head(includeScript('dist/leaflet-draw-rotate.js', 'type' = 'text/javascript', 'data-unique-tag' = 'unique'),
#            includeScript('dist/L.Path.Transform.js', 'type' = 'text/javascript', 'data-unique-tag' = 'unique'),
            includeScript('dist/Edit.Rectangle.Rotate.js', 'type' = 'text/javascript', 'data-unique-tag' = 'unique')),
  tags$style(HTML("
    .center-container {
      display: flex;
      justify-content: center;
    }
  ")),
  fluidRow(
    column(
      width = 12,
      div(class = "center-container",  style = "vertical-align: top;", leafletOutput("map", width = "1600", height = "900px"))
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(class = "center-container full-width-container",
          switchInput(
            inputId = "plot_switch",
            label = "visualise",
            onLabel = "Categorical",
            offLabel = "Continuous",
            value = FALSE
          )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(class = "center-container", style = "vertical-align: top;", plotOutput("plot", width = "1600", height = "600"))
    )
  )
)

# Shiny app server
server <- function(input, output, session) {
  latlongs <- reactiveValues(coords = NULL, shape = NULL)
  
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
      registerPlugin(rotatePlugin) %>%
      onRender(
        "function(el, x) {
          L.Edit.Rectangle.prototype.setOptions({ uniformScaling: false });
          var rotateControl = new L.Control.Draw.Rotate();
          this.addControl(rotateControl);
        }"
      ) %>%
      addRasterImage(r1, colors = r1_colors, opacity = 0.8, group = "Temperature", layerId = "Temperature", project=TRUE) %>%
      addRasterImage(r2, colors = r2_colors, opacity = 0.8, group = "Sedimentation", layerId = "Sedimentation", project=TRUE) %>%
      addRasterImage(r3, colors = r3_colors, opacity = 0.8, group = "DHW", layerId = "DHW", project=TRUE) %>%
      addRasterImage(r4, colors = r4_colors, opacity = 0.8, group = "Wave Exposure", layerId = "Wave Exposure", project=TRUE) %>%
      addRasterImage(r5, colors = r5_colors, opacity = 0.8, group = "Chlorophyll", layerId = "Chlorophyll", project=TRUE) %>%
      addRasterImage(r6, colors = r6_colors, opacity = 0.8, group = "Salinity",  layerId = "Salinity", project=TRUE) %>%
      addRasterImage(cat_final, colors = cat_final_colors, opacity = 1,  group = "Restoration score", layerId = "Restoration score", project=TRUE) %>%
      
      addLegend(position = "bottomright", title = "Temperature", group = "Temperature", values = values(r1), pal = r1_colors, opacity = 1) %>%
      addLegend(position = "bottomright", title = "Sedimentation", group = "Sedimentation", values = values(r2), pal = r2_colors, opacity = 1) %>%
      addLegend(position = "bottomright", title = "DHW", group = "DHW", values = values(r3), pal = r3_colors, opacity = 1) %>%
      addLegend(position = "bottomright", title = "Wave Exposure", group = "Wave Exposure", values = values(r4), pal = r4_colors, opacity = 1) %>%
      addLegend(position = "bottomright", title = "Chlorophyll", group = "Chlorophyll", values = values(r5), pal = r5_colors, opacity = 1) %>%
      addLegend(position = "bottomright", title = "Salinity", group = "Salinity", values = values(r6), pal = r6_colors, opacity = 1) %>%
      addLegend(position = "bottomright", title = "Score", group = "Restoration score", values = values(cat_final), pal = cat_final_colors, opacity = 1) %>%
      
      addImageQuery(r1, type = "mousemove", digits=1, group = "Temperature", layerId = "Temperature", project=TRUE, prefix="", position = "bottomright") %>%
      addImageQuery(r2, type = "mousemove", digits=1, group = "Sedimentation", layerId = "Sedimentation", project=TRUE, prefix="", position = "bottomright") %>%
      addImageQuery(r3, type = "mousemove", digits=1, group = "DHW", layerId = "DHW", project=TRUE, prefix="", position = "bottomright") %>%
      addImageQuery(r4, type = "mousemove", digits=1, group = "Wave Exposure", layerId = "Wave Exposure", project=TRUE, prefix="", position = "bottomright") %>%
      addImageQuery(r5, type = "mousemove", digits=1, group = "Chlorophyll", layerId = "Chlorophyll", project=TRUE, prefix="", position = "bottomright") %>%
      addImageQuery(r6, type = "mousemove", digits=1, group = "Salinity", layerId = "Salinity", project=TRUE, prefix="", position = "bottomright") %>%
      addImageQuery(cat_final, type = "mousemove", digits=1, group = "Restoration score", layerId = "Restoration score", project=TRUE, prefix="", position = "bottomright") %>%
      
      
      # addRasterImage(r1_bins, colors = rev(brewer.pal(5, "RdYlBu")), opacity = 0.8, group = "Binned Temperature") %>%
      # addRasterImage(r2_bins, colors = (brewer.pal(5, "RdYlBu")), opacity = 0.8, group = "Binned Sedimentation") %>%
      # addRasterImage(r3_bins, colors = rev(brewer.pal(5, "RdYlBu")), opacity = 0.8, group = "Binned DHW") %>%
      # addRasterImage(r4_bins, colors = rev(brewer.pal(5, "RdYlBu")), opacity = 0.8, group = "Binned Wave Exposure") %>%
      # addRasterImage(r5_bins, colors = rev(brewer.pal(5, "RdYlBu")), opacity = 0.8, group = "Binned Chlorophyll") %>%
      # addRasterImage(r6_bins, colors = (brewer.pal(5, "RdYlBu")), opacity = 0.8, group = "Binned Salinity") %>%
      
      
      # addLegend(position = "bottomright", title = "Binned Temperature", group = "Binned Temperature", values = raster_levels, pal = pal, opacity = 1) %>%
      # addLegend(position = "bottomright", title = "Binned Sedimentation", group = "Binned Sedimentation", values = raster_levels, pal = pal, opacity = 1) %>%
      # addLegend(position = "bottomright", title = "Binned DHW", group = "Binned DHW", values = raster_levels, pal = pal, opacity = 1) %>%
      # addLegend(position = "bottomright", title = "Binned Wave Exposure", group = "Binned Wave Exposure", values = raster_levels, pal = pal, opacity = 1) %>%
      # addLegend(position = "bottomright", title = "Binned Chlorophyll", group = "Binned Chlorophyll", values = raster_levels, pal = pal, opacity = 1) %>%
      # addLegend(position = "bottomright", title = "Binned Salinity", group = "Binned Salinity", values = raster_levels, pal = pal, opacity = 1) %>%
      
      addMouseCoordinates() %>%
      addPolygons(
        data = lizard_map,
        group = "Habitat",
        fillColor = ~colorPalette(class),
        color = "black",
        fillOpacity = 0.5,
        weight = 1
      ) %>%
      addPolygons(
        data = lizard_map,
        group = "Lines",
        fillColor = NA,
        color = "black",
        fillOpacity = 0,
        weight = 0.5
      ) %>%
      setView(lng = 145.448, lat = -14.7004, zoom = 18) %>%
      addLayersControl(
        baseGroups = c("Basemap"),
        overlayGroups = c("Habitat",
                          "Temperature", #"Binned Temperature",
                          "Sedimentation",# "Binned Sedimentation",
                          "DHW",# "Binned DHW",
                          "Wave Exposure", #"Binned Wave Exposure",
                          "Chlorophyll", #"Binned Chlorophyll",
                          "Salinity", # "Binned Salinity",
                          "Restoration score",
                          "Plots"
                          
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Habitat", "Temperature", "Sedimentation", "DHW", "Wave Exposure", "Chlorophyll", "Salinity"))
                 # "Binned Temperature", "Binned Sedimentation", "Binned DHW", "Binned Wave Exposure", "Binned Chlorophyll", "Binned Salinity"))
  })
  
  
  updateShape <- function(coords) {
    
    plot_rasters <- function(input, subset, title, pal, bbox, rev=FALSE){
      
      if (isTRUE(rev)){
        ggplot() + theme_bw() +
          ggtitle(title) +
          geom_spatraster(data = subset) +
          geom_tile(data = subset, aes(x = x, y = y, fill = layer), color = "black", linewidth = 0.1, show.legend = FALSE) +
          geom_sf(data = plot_shp, fill = NA, linewidth = 2, col="darkred") +
          scale_fill_gradientn(colors = rev(brewer.pal(9, pal)), na.value = "transparent", limits=c(floor(minmax(input)[1]), floor(minmax(input)[2]))) +
          coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))+
          theme(plot.margin = unit(c(0, 5, 0, 5), "pt"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                legend.position="top")
        
      } else {
        ggplot() + theme_bw() +
          ggtitle(title) +
          geom_spatraster(data = subset) +
          geom_tile(data = subset, aes(x = x, y = y, fill = layer), color = "black", linewidth = 0.1, show.legend = FALSE) +
          geom_sf(data = plot_shp, fill = NA, linewidth = 2, col="darkred") +
          scale_fill_gradientn(colors = (brewer.pal(9, pal)), na.value = "transparent", limits=c(floor(minmax(input)[1]), floor(minmax(input)[2]))) +
          coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]))+
          theme(plot.margin = unit(c(0, 5, 0, 5), "pt"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                legend.position="top")
      }
      
    }
    
    plot_rasters_ordinal <- function(input, subset, title, bbox){
      ggplot() + theme_bw() +
        ggtitle(title) +
        geom_spatraster(data = subset) +
        geom_tile(data = subset, aes(x = x, y = y, fill = layer), color = "black", linewidth = 0.1, show.legend = TRUE) +
        geom_sf(data = plot_shp, fill = NA, linewidth = 2, col="darkred") +
        scale_fill_manual(
          values = rev(brewer.pal(5, "RdYlBu")),
          limits = c("low", "medium-low", "medium", "medium-high", "high"),
          drop = FALSE,
          na.value = "transparent",
          name = "",
          guide = guide_legend(nrow = 1)
        ) +
        coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
        theme(
          plot.margin = unit(c(0, 5, 0, 5), "pt"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          legend.direction = "vertical",  
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
            legend.key.height = unit(10, "pt"),
            legend.key.width = unit(10, "pt")
          
        ) 
    }
    
    
    
    # extract dimensions for leaflet.draw rectangle
    latlongs$shape <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
    plot_shp <- latlongs$shape
    bbox <- st_bbox(plot_shp)
    
    # plot outputs
    output$plot <- renderPlot({
      
      ### habitat vectors
      h_crop <- st_intersection(plot_shp, lizard_map)
      
      h_map_plot <-
        ggplot() +
        theme_bw() +
        ggtitle("") +
        geom_sf(data = h_crop, aes(fill = class)) +
        scale_fill_manual(values = habitat_pal, na.value = "transparent") +
        geom_sf(data = plot_shp, fill = NA, linewidth = 1, color = "darkred") +
        theme(
          plot.margin = unit(c(0, 5, 5, 5), "pt"), # Adjust margin to give space for the legend
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "top" 
        )
      
      
      h_map_plot_below <-
        ggplot() +
        theme_bw() +
        ggtitle("Habitats") +
        geom_sf(data = h_crop, aes(fill = val)) +
        scale_fill_gradientn(colors=rev(brewer.pal(5, "RdYlBu")), limits=c(1,5), na.value = "transparent") +
        geom_sf(data = plot_shp, fill = NA, linewidth = 1, color = "darkred") +
        theme(
          plot.margin = unit(c(0, 5, 0, 5), "pt"), # Adjust margin to give space for the legend
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
            legend.position = "bottom",
            legend.direction = "vertical",  
            legend.key.height = unit(10, "pt"),
            legend.key.width = unit(10, "pt"),
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 0)
            )
      
      ### rasters
     
      r1 <- rast(r1)
      r2 <- rast(r2)
      r3 <- rast(r3)
      r4 <- rast(r4)
      r5 <- rast(r5)
      r6 <- rast(r6)
      cat_final <- rast(cat_final)
      
      # crop rasters to area
      r1_crop <- crop(r1, plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r2_crop <- crop(r2, plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r3_crop <- crop(r3, plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r4_crop <- crop(r4, plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r5_crop <- crop(r5, plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r6_crop <- crop(r6, plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      cat_final_crop <- crop(cat_final, plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      
      
      # plot subset rasters
      r1_map_plot <- plot_rasters(r1, r1_crop, "", "RdYlBu", rev=TRUE, bbox)
      r2_map_plot <- plot_rasters(r2, r2_crop, "", "BrBG", bbox)
      r3_map_plot <- plot_rasters(r3, r3_crop, "", "RdGy", rev=TRUE, bbox)
      r4_map_plot <- plot_rasters(r4, r4_crop, "", "Blues", bbox)
      r5_map_plot <- plot_rasters(r5, r5_crop, " ", "Greens", bbox)
      r6_map_plot <- plot_rasters(r6, r6_crop, "", "PiYG", rev=TRUE, bbox)
      cat_final_map_plot <- plot_rasters(cat_final, cat_final_crop, "Combined datalayer", "RdYlBu", bbox)
      
      # crop ordinal rasters
      r1_crop_bins <- crop(cut_raster_bins(r1), plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r2_crop_bins <- crop(cut_raster_bins(r2), plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r3_crop_bins <- crop(cut_raster_bins(r3), plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r4_crop_bins <- crop(cut_raster_bins(r4), plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r5_crop_bins <- crop(cut_raster_bins(r5), plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      r6_crop_bins <- crop(cut_raster_bins(r6), plot_shp, mask = TRUE, touches = TRUE, snap = "out")
      
      # bin ordinal rasters
      r1_map_plot_ord <- plot_rasters_ordinal(r1, r1_crop_bins, title="Temperature (20m) ", bbox=bbox)
      r2_map_plot_ord <- plot_rasters_ordinal(r2, r2_crop_bins, title="Sedimentation (50m) ", bbox=bbox)
      r3_map_plot_ord <- plot_rasters_ordinal(r3, r3_crop_bins, title="DHW (100m) ", bbox=bbox)
      r4_map_plot_ord <- plot_rasters_ordinal(r4, r4_crop_bins, title="Wave exposure (20m grid) ", bbox=bbox)
      r5_map_plot_ord <- plot_rasters_ordinal(r5, r5_crop_bins, title="Chlorophyll (50m grid)", bbox=bbox)
      r6_map_plot_ord <- plot_rasters_ordinal(r6, r6_crop_bins, title="Salinity (125m grid) ", bbox=bbox)
      

      if (input$plot_switch) {
        
      plot1 <- (h_map_plot | r1_map_plot | r2_map_plot | r3_map_plot | r4_map_plot | r5_map_plot | r6_map_plot) /
      (h_map_plot_below | r1_map_plot_ord | r2_map_plot_ord | r3_map_plot_ord | r4_map_plot_ord | r5_map_plot_ord | r6_map_plot_ord)
      plot1
      } else {
      plot2 <- ggplot() + theme_bw() + geom_spatraster(data = cat_final_crop)  +
       scale_fill_gradientn(colors = (brewer.pal(11, "RdYlBu")), na.value = "transparent", limits=c(1.2,5)) +
       geom_sf(data = h_crop, fill = NA, linewidth=0.25) +
       geom_sf(data = plot_shp, fill = NA, linewidth = 2, col="darkred") +
       theme(plot.margin = unit(c(50, 5, 50, 5), "pt"),
             title=element_text(size = 8),
             axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             legend.position = "bottom",
             legend.text = element_text(size = 14),
             legend.title = element_text(size = 0))
        
      plot2
      }

      
      
        

    })
  }
  
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    coords <- matrix(unlist(feat$geometry$coordinates), ncol = 2, byrow = TRUE)
    latlongs$coords <- coords
    updateShape(coords)
  })
  
  observeEvent(input$map_draw_edited_features, {
    feat <- input$map_draw_edited_features$features[[1]]
    coords <- matrix(unlist(feat$geometry$coordinates), ncol = 2, byrow = TRUE)
    latlongs$coords <- coords
    updateShape(coords)
  })
  
  output$coords <- renderPrint({
    if (!is.null(latlongs$shape)) {
      print("")
    }
  })
}

# Run the app
shinyApp(ui, server)
