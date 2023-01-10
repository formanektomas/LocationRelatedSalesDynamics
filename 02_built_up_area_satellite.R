library(RCzechia)
library(terra) 
library(tidyterra)
library(tidyverse)
library(ggplot2)
library(ggmap)
rm(list = ls())
#
#
# https://lcviewer.vito.be/download , select Built-up , 2019 & save as 2019.tif
#
# https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html
# https://dieghernan.github.io/tidyterra/reference/geom_spatraster_rgb.html
#
raster_download <- "2019.tif"
year2019 <- terra::rast(raster_download)
# plot(year2019)
#
# Crop to Pilsen
e1 <- terra::ext(13.26531,13.48503,49.66884,49.81084) # rough bbox for Pilsen
P1 <- terra::crop(year2019,e1,mask=T)
plot(P1) # base plot
#
obce <- obce_polygony() # city boundary from the  {RCzechia} package
my_city <- obce %>% 
  filter(NAZ_OBEC == "Plzeň") %>% 
  select(NAZ_OBEC) %>% 
  summarize()
my_city_vect <- terra::vect(my_city)
P2 <- terra::mask(P1,my_city_vect,inverse=F)
plot(P2) 
#
### Main plot
load("Maps.RData")
#
# Project to 3859: 
P2 <- terra::project(P2, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null
+wktext +no_defs")
names(P2) # 
names(P2) <- "y2019" # 
my_city <- st_transform(my_city, 3857)
#
# zoom-in (inset) map bbox
bboxPlzen <- st_bbox(c(xmin = 13.364, xmax = 13.422, ymax = 49.762, ymin = 49.7266), crs = st_crs(4326))
bboxPlzen <- st_as_sfc(bboxPlzen)
bboxPlzen <- st_transform(bboxPlzen, 3857)
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
#---- main plot
#
ggmap(map) + 
  coord_sf(crs = st_crs(3857)) +
  geom_spatraster(data = P2, aes(fill = y2019)) +
  scale_fill_viridis_c(alpha=0.5, na.value= NA, ,option = "plasma")+
  geom_sf(inherit.aes = F, data = my_city, col="black", fill=NA) +
  geom_sf(inherit.aes = F, data = bboxPlzen, col="red",size=1,fill=NA) +
  theme(legend.position='none') 