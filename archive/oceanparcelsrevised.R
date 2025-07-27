
library(tidyverse)
library(sf)
library(tmap)


moore_habitats <- read_sf("/Users/rof011/spatialtools/data/MooreCluster_SpatialPolygons.gpkg", quiet=TRUE)



import_particles <- function(input){

  ocean_parcels_particles <- read.csv(input) |>
    drop_na(lon, lat) |>
    st_as_sf(coords=c("lon", "lat")) |>
    st_set_crs(4326) |>
    st_transform(20353) |>
    mutate(time=ymd_hms(time)) |>  
    mutate(day=day(time)) 
  
  ocean_parcels_particles <- ocean_parcels_particles |>
    mutate(nday = ifelse(day==30, 0, day)+1) |> 
    mutate(trajectory=as.numeric(trajectory), nday=as.numeric(nday)) |> 
    st_make_valid() %>%
    arrange(trajectory, nday, time)
  
  
  add_next_nday_point <- function(data) {
    max_nday <- max(data$nday)  # Identify the maximum `nday` for the trajectory
    
    data %>%
      group_by(nday) %>%
      summarise(
        geometry = {
          current_nday <- unique(nday)  # Get the unique value of `nday` in this group
          
          combined_geom <- st_combine(geometry)
          
          if(current_nday < max_nday) {
            next_point <- data %>%
              filter(nday == current_nday + 1) %>%
              slice(1) %>%
              pull(geometry)
            combined_geom <- st_combine(c(combined_geom, next_point))
          }
          
          st_cast(combined_geom, "LINESTRING")
        },
        .groups = "drop"
      )
  }
  
  # Apply the function to each trajectory and combine results
  ocean_parcels_lines_tmp <- ocean_parcels_particles %>%
    group_by(trajectory) %>%
    group_modify(~ add_next_nday_point(.x)) %>%
    ungroup() %>%
    st_as_sf()
  
  
  # Function to ensure a LINESTRING has at least two points by duplicating the last point if necessary
  ensure_two_points <- function(geometry) {
    coords <- st_coordinates(geometry)
    if (nrow(coords) < 2) {
      coords <- rbind(coords, coords)  
    }
    st_linestring(coords)
  }
  
  
  ocean_parcels_lines <- ocean_parcels_lines_tmp %>%
    dplyr::mutate(geometry = purrr::map(geometry, ensure_two_points)) %>%  # Use purrr::map to retain the list-column structure
    dplyr::mutate(geometry = st_zm(geometry)) %>%  # Drop the Z axis
    st_sf() |> 
    st_set_crs(20353) # Recreate the crs
  
  ocean_parcels_lines

}


Moore_2D_0.5m <- import_particles("/Users/rof011/GBR_connectivity/outputs/Moore_2D_0.5m.zarr/Moore_2D_0.5m.csv")
Moore_2D_2.5m <- import_particles("/Users/rof011/GBR_connectivity/outputs/Moore_2D_2.25m.zarr/Moore_2D_2.5m.csv")




