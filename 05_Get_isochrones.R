#####################################################################
#   
library(Rcpp)
library(ggplot2)
library(RCzechia)
library(tidyverse)
library(tidygeocoder)
library(osmdata)
library(hereR)
rm(list = ls())
# 
#
hereR::set_key("API key") # you need to provide your own API key 
# .. if necessary,
# .. register at https://developer.here.com/
# .. create a Project & get API key
#
#
#########################################################
# Locations of 3 stores (illustrative data)
#
load("Isochrones.RData")
#
#
mapview::mapview(Stores)
mapOfStores <- mapview::mapview(Stores)
#
# .. if GPS coordinates are unknow but you have addresses,
# .. use    hereR::geocode(X)
# .. for syntax, see ?hereR::geocode
#
#########################################################
#
# 10 min pedestrian walk ischorones
#
isochrones <- Stores 
#
for (i in 1:nrow(isochrones)){
  X <- hereR::isoline(poi = isochrones[i,], # point of interest, center of iso.
                      transport_mode = "pedestrian", # walk
                      range = 60 * 10, # units are seconds, this is for 10 minutes
                      range_type = "time", # other options available
                      aggregate=F) # useful when isochones overlap
  isochrones$geometry[i] <- X$geometry # Store "point" gets replaced by iso polygon.
  print(i)
  print(X$geometry)
  # time delay between queries can be removed, but is has proven useful
  # .. server can limit frequency of (cost free) external access,
  # .. which may result in errors.
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<1.5){} #dummy while loop
}
#
#
mapview::mapview(isochrones, label = "name", map = mapOfStores@map )
#############################################################
