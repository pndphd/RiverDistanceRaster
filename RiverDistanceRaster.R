##### Description #####
# This script is a test to setup a river distance raster as well as clip flow rasters
# for SAM 2.0

#____________________________________________________________________#
##### Options #####
# Set the desired EPGS code for the GIS files
EPSG = 32610
# Set the resolution you want
# This i show often a point is placed on the river line in m
resolution = 20
#____________________________________________________________________#

##### Load Libraries #####
library(sf)
library(leaflet)
library(tidyverse)
library(raster)
library(viridis)

##### Load The Necessary Data #####
# Load the center line
riverLine = st_read("./GIS/River Distance/Center Line.shp") %>% st_transform(EPSG) %>% st_zm()
# Load the area you care about
riverDomain = st_read("./GIS/River Distance/domain.shp") %>% st_transform(EPSG) 
# Load the top of the reach marker
topMarker = st_read("./GIS/River Distance/top.shp") %>% st_transform(EPSG) %>% st_zm()

##### Functions #####
# Make a function to check if the river distances are backwards
# and flip them if they are
flipCheck = function(df, checkDist, distance, altDistance){
  if(abs(sum(df[[checkDist]]))!=0){
    df = df %>% 
      # use mutate_ because passing strings as col names
      mutate_(distance = altDist)
  }
  return(df)
}
    
##### Do GIS manulapition #####
# we are going to rasterize a vertor, so first make the blank raster template
blankRaster = raster(ext = extent(riverDomain), resolution = resolution/2, crs = crs(riverDomain)) 

# Place sample points along the line
samplePoints = st_line_sample(x = riverLine, density = 1/resolution) %>%
  st_sf() %>%
  st_cast("POINT") %>% 
  # Assign each one a distance
  mutate(distance = 1:n()*resolution)

# Use voronoi cells and joins to make a polygon layer of distances
# then rasterize it
voronoi = st_voronoi(x = st_union(samplePoints)) %>%
  st_sf() %>%
  st_cast() %>% 
  # Join with the points to get the distance attribute form the samplePoints layer
  st_join(samplePoints) %>%
  # Join to get the attribute form the top marker point
  st_join(topMarker) %>% 
  # If we flip the distances make a row of the new distances
  # also make a row colomn which will just be 0 if no flip is necessary
  # but will have a value oif we need to
  arrange(-distance) %>% 
  mutate(altDist = 1:n()*resolution,
         checkDist = distance - ifelse(is.na(place), distance, max(distance))) %>% 
  # Check to see if it needs to be flipped an if so do it
  flipCheck("checkDist", "distance", "altDist") %>% 
  # Clip by the river domain
  st_intersection(riverDomain) %>%
  # rasterize the distance field
  rasterize(blankRaster, field = "distance")

# MAke a Viridis color pallet
values = c(0, max(samplePoints$distance))
pal = colorNumeric("viridis", values, na.color = "transparent")

# Map out the raster
lookAt = riverLine
l = leaflet(lookAt %>% st_transform(4326)) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(noWrap = F)) %>%
  addPolylines() %>% 
  addRasterImage(voronoi, colors = pal, project = 4326) %>% 
  addCircleMarkers(data = (topMarker %>% st_transform(4326)))
  # addPolygons(data = (riverDomain %>% st_transform(4326))) %>% 
  # addPolygons(data = (voronoi %>% st_transform(4326)), color = ~pal(distance))
l

# Write the raster
writeRaster(x = voronoi, filename = "./GIS/River Distance/distance raster", format = "ascii", prj = T, overwrite = T)
