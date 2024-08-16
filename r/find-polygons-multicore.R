library(sf)
library(future.apply)
library(tictoc)

# Import reef polygons, create union (or returns multiple intersections 
geomorphic_seeding <- st_read("/Users/rof011/spatialtools/apps/remove-zones/www/geomorphic.geojson") %>% 
  mutate(area=as.numeric(st_area(.))) |> 
  st_transform(20353) |> 
  filter(area > 10000) |> 
  mutate(n=round(as.numeric(area)/1000)) 

tic()

# Set up parallel processing plan
plan(multisession, workers = availableCores())

# Parallel lapply sampling process using future_lapply
seeded_points_list <- future_lapply(seq_len(nrow(geomorphic_seeding)), function(i) {
  polygon <- geomorphic_seeding[i, ]
  points <- st_sample(polygon, size = polygon$n, type = "random")
  st_sf(class = polygon$class, geometry = points)
})

# Combine the list into a single sf object
seeded_points <- do.call(rbind, seeded_points_list) |> st_sf() |> st_transform(20353)

# Define the width and length of the rectangle
width <- 100  # Example width (in the same units as your CRS)
length <- 100  # Example length (in the same units as your CRS)

# Use future_lapply to create the buffered polygons
buffered_polygons_list <- future_lapply(seq_len(nrow(seeded_points)), function(i) {
  
  # Extract the coordinates of the current point
  x <- sf::st_coordinates(seeded_points)[i, 1]
  y <- sf::st_coordinates(seeded_points)[i, 2]
  
  # Set parameters for the rectangle around the point
  x_min <- x - (width / 2)
  x_max <- x + (width / 2)
  y_min <- y - (length / 2)
  y_max <- y + (length / 2)
  
  # Create the rectangular polygon
  polygon <- sf::st_polygon(list(rbind(
    c(x_min, y_min), 
    c(x_min, y_max), 
    c(x_max, y_max), 
    c(x_max, y_min), 
    c(x_min, y_min)
  )))
  
  return(polygon)
})

# Combine the results into a single sf object
buffered_polygons_sf <- sf::st_sfc(buffered_polygons_list, crs = sf::st_crs(seeded_points)) |> st_as_sf()

# Intersect the plots with the reef polygons, calculate overlap
intersections <- st_intersection(buffered_polygons_sf, st_union(geomorphic_seeding)) %>%
  mutate(intersection_area = st_area(.)) %>%
  mutate(percentage_overlap = (as.numeric(intersection_area) / 10000) * 100)

# Reset the plan to sequential processing after completion (optional)
plan(sequential)

toc()