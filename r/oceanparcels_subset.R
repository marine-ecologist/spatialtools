library(tidyverse)
library(sf)
library(future.apply)
library(tictoc)
library(lwgeom)
library(tmap)

set.seed(123)


reef_polygons <- st_read("/Users/rof011/Downloads/GBR_connectivity/datafiles/MooreCluster_SpatialPolygons.gpkg") |>
  mutate(reef_names = sub("_.*", "", Reef))

moore_release <- reef_polygons |> filter(site_id=="Moore_16071_Slope_13b")

st_write(moore_release, file.path("/Users/rof011", "moore_release.gpkg"))


tmap_mode("view")
tm_basemap("Esri.WorldImagery") +
tm_shape(reef_polygons)+
tm_polygons("site_id", fill.alpha=0.2)
