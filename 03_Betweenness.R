# .. (satelite adds concrete and asphalt roads, city-squares, etc.)
#
# http://download.geofabrik.de/europe/czech-republic.html#
# goto "raw directory index" 
# download czech-republic-190101-free.shp.zip ; ~1GB, year 2019 data
# extract .shp and .shx files to R working directory
#
#
#
library(ggplot2)
library(mapview)
library(dplyr)
library(sf)
library(gstat) 
library(RCzechia)
library(sfnetworks)
library(tidygraph)
#
# rm(list=ls())
#
# read in streets and cities
ulice <- read_sf('gis_osm_roads_free_1.shp') # 
st_crs(ulice) <- 4326
obce <- obce_polygony() # z RCzechia
my_city <- obce %>% 
  filter(NAZ_OBEC == "Plzeň") %>% 
  select(NAZ_OBEC) %>% 
  summarize()
map_obec <- mapview::mapview(obce[obce$NAZ_OBEC == "Plzeň",])
#
#
# select streets in the city
city_streets <- ulice %>% 
  st_intersection(my_city) 
mapview::mapview(city_streets, map = map_obec@map)
#
#
# object contains both multilinestring and linestring, sfnetworks handles
# linestring only
summary(city_streets$geometry)
city_streets <- st_cast(st_cast(city_streets, "MULTILINESTRING"),"LINESTRING")
summary(city_streets$geometry)
#
#
#
# 
net <- as_sfnetwork(city_streets)
autoplot(net)
#
#
#
# centrality
net <- net %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness(normalized = F, directed = FALSE)) %>% 
  mutate(bc_s = scale(bc)) %>% 
  arrange(bc_s) %>% 
  mutate(cc = centrality_closeness(normalized = T)) %>% 
  mutate(cc_s = scale(cc)) %>% 
  mutate(ec = centrality_eigen(weights = NULL, scale = F)) %>% 
  mutate(ec_s = scale(ec))
#
#
#
library(ggmap)
load("Maps.RData")
ggmap(Pils_map)
#
#
net_3857_edges <- st_transform(st_as_sf(net, "edges"), 3857)
net_3857_nodes <- st_transform(st_as_sf(net, "nodes"), 3857)
my_city <- st_transform(my_city, 3857)
#
# zoom-in (inset) map bbox
bboxPlzen <- st_bbox(c(xmin = 13.364, xmax = 13.422, ymax = 49.762, ymin = 49.7266), crs = st_crs(4326))
bboxPlzen <- st_as_sfc(bboxPlzen)
bboxPlzen <- st_transform(bboxPlzen, 3857)
#
#
#
# Combinantion of google maps and sf objects:
#
# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
#
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
#
#
#
#
map <- ggmap_bbox(Pils_map)
#
#
#
#
#
ggmap(map) + 
  coord_sf(crs = st_crs(3857)) +
  geom_sf(inherit.aes = F, data = my_city, col="black", fill="white",alpha=0.6) +
  geom_sf(inherit.aes = F, data = net_3857_edges, col = "black") +
  geom_sf(inherit.aes = F, data = net_3857_nodes, aes(col = bc_s, size = bc_s)) +
  scale_colour_gradient2(low = "grey", mid = "blue",high = "yellow") +
  geom_sf(inherit.aes = F, data = bboxPlzen, col="red",size=1,fill=NA) +
  theme(legend.position='none') 
  
