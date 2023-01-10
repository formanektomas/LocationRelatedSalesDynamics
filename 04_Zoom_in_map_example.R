library(ggplot2)
library(RCzechia)
library(dplyr)
library(tidygeocoder)
library(osmdata)
library(hereR)
library(mapview)
rm(list = ls())
# 
#
#
bboxPlzen <- st_bbox(c(xmin = 13.364, xmax = 13.422, ymax = 49.762, ymin = 49.7266), crs = st_crs(4326))
bboxPlzen <- st_as_sfc(bboxPlzen)
map1 <- mapview(bboxPlzen)
#
#
load("Isochrones.RData")
isochrones <- isochrones5min %>% 
  mutate(ID = as.factor(ID))
#
mapview(isochrones)
mapIso <- mapview(isochrones)
postOffices <- opq(bbox = iconv("Plzeň",to = "UTF-8")) %>%
  add_osm_feature(key = "amenity", 
                  value = "post_office") %>%
  osmdata_sf(quiet = F)
Posts <- postOffices$osm_points %>% 
  st_intersection(bboxPlzen)
mapview::mapview(Posts, label = T, layer.name = "Post", color = "red", col.regions = "red" ,cex = 8, map = mapIso@map )
#
#
