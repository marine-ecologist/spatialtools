library(sf)
library(dplyr)

# Ensure geometries are valid and ordered by trajectory, nday, and time
tmp_ordered <- ocean_parcels_particles %>%
  st_make_valid() %>%
  arrange(trajectory, nday, time)

# Function to add the first point of the next nday to the current nday
add_next_nday_point <- function(data) {
  max_nday <- max(data$nday)  # Identify the maximum `nday` for the trajectory
  
  data %>%
    group_by(nday) %>%
    summarise(
      geometry = {
        current_nday <- unique(nday)  # Get the unique value of `nday` in this group
        
        # Combine current `nday` points
        combined_geom <- st_combine(geometry)
        
        # If this is not the last `nday`, add the first point of the next `nday`
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
ocean_parcels_paths <- tmp_ordered %>%
  group_by(trajectory) %>%
  group_modify(~ add_next_nday_point(.x)) %>%
  ungroup() %>%
  st_as_sf()


tm_shape(tmp_extended_all |> filter(trajectory=="1001")) + 
  tm_lines("nday") +
tm_shape(ocean_parcels_particles |> filter(trajectory=="1001")) + 
  tm_dots("nday")


