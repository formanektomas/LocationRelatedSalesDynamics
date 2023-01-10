# satellite image of Pilsen
library(ggmap)
register_google(key = "insert_your_own_Google_API_key") # follow ?register_google
#
# Google registration & satellite-based maps can be troubleshooted here:
# https://stackoverflow.com/questions/60061173/error-in-ggmap-must-be-an-array-and-http-400-bad-request
#
Pils_map <- get_googlemap(center = c(lon=13.375,lat=49.74), zoom = 12, scale = 2, 
                    maptype = "satellite", language = "en-EN",
                    color="color")
#
ggmap(Pils_map)
#
#
#
library(RCzechia)
library(tidyverse)
obce <- obce_polygony() # city boundary from the {RCzechia} package
my_city <- obce %>% 
  filter(NAZ_OBEC == "Plzeň") %>% 
  select(NAZ_OBEC) %>% 
  summarize()
my_city <- st_transform(my_city, 3857)
#
#
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
map <- ggmap_bbox(Pils_map)
#
#
#
#
ggmap(map) + 
  coord_sf(crs = st_crs(3857)) +
  geom_sf(inherit.aes = F, data = my_city, col="blue", size=1.5, fill=NA) +
  theme(legend.position='none') 
#
# save Global Environment to "Maps.RData" for subsequent use