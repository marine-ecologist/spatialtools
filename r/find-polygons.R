library(sf)
library(future.apply)

# create function to set hectare plots

set_hectare <- function(input = NULL, width = NULL, length = NULL) {
  
  # Initialize an empty list to store polygons
  polygons_list <- vector("list", length = nrow(input))
  
  # Loop over each point in the input
  for (i in seq_len(nrow(input))) {
    
    # Extract the coordinates of the current point
    x <- sf::st_coordinates(input)[i, 1]
    y <- sf::st_coordinates(input)[i, 2]
    
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
    
    # Add the polygon to the list
    polygons_list[[i]] <- polygon
  }
  
  # Combine all polygons into an sf object
  polygons_sf <- sf::st_sfc(polygons_list, crs = sf::st_crs(input)) |> st_as_sf()
  
  return(polygons_sf)
}


# Import reef polygons, create union (or returns multiple intersections 
geomorphic_seeding <- st_read("/Users/rof011/spatialtools/apps/remove-zones/www/geomorphic.geojson") %>% 
  mutate(area=as.numeric(st_area(.))) |> 
  st_transform(20353) |> 
  filter(area > 10000) |> 
  mutate(n=round(as.numeric(area)/1000)) 

library(tictoc)

tic()

# lapply sampling process
seeded_points <- st_sf(do.call(rbind, lapply(seq_len(nrow(geomorphic_seeding)), function(i) {
  polygon <- geomorphic_seeding[i, ]
  points <- st_sample(polygon, size = polygon$n, type = "random")
  st_sf(class = polygon$class, geometry = points)
}))) |> st_transform(20353)
toc()

tic()

# future_lapply to parallelize the sampling process
plan(multisession, workers = availableCores()-1)

seeded_points <- future_lapply(seq_len(nrow(geomorphic_seeding)), function(i) {
  polygon <- geomorphic_seeding[i, ]
  points <- st_sample(polygon, size = polygon$n, type = "random")
  st_sf(class = polygon$class, geometry = points)
}) 

plan(sequential)

toc()

# Combine the results into a single sf object and transform the CRS
seeded_points <- st_sf(do.call(rbind, seeded_points)) |> st_transform(20353)


# Create 100x100m squares around each point
buffered_polygons <- set_hectare(seeded_points, 100, 100) 

# Intersect the plots with the reef polygons
intersections <- st_intersection(buffered_polygons, st_union(geomorphic_seeding)) |> 
  mutate(intersection_area = st_area(intersections))

# For each plot calculate the percentage overlap with underlying reef polygons
intersections <- intersections |> 
  mutate(percentage_overlap = (intersections$intersection_area / st_area(buffered_polygons)) * 100) 


ggplot() + theme_bw() +
  geom_histogram(data=intersections, aes(x=as.numeric(percentage_overlap)), alpha=0.2, fill="red", color="black")

library(tmap)
tmap_mode("view")
tm_shape(intersections |> filter(as.numeric(percentage_overlap) > 50)) +
  tm_polygons()
  