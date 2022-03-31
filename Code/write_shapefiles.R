library(here)
library(tidyverse)
library(sf)

logs <- read_csv(here("Output/_Results/22-082B_22-085E_DiveEvents.txt")) %>% 
  st_as_sf(coords = c("long_r", "lat_r"), crs = 4326)

ab.obs <- logs %>% 
  filter(comment %in% c("H. sorenseni", "H. kamtschatkana"))

st_write(logs, here("Output/logs_22-082B-22-085E.shp"), delete_layer = TRUE)
st_write(ab.obs, here("Output/abs_22-082B-22-085E.shp"), delete_layer = TRUE)

logs <- read_csv(here("Output/_Results/22-086A_22-086F_DiveEvents.txt")) %>% 
  st_as_sf(coords = c("long_r", "lat_r"), crs = 4326)

ab.obs <- logs %>% 
  filter(comment %in% c("H. sorenseni", "H. kamtschatkana"))

st_write(logs, here("Output/logs_22-082B-22-086F.shp"), delete_layer = TRUE)
st_write(ab.obs, here("Output/abs_22-082B-22-086F.shp"), delete_layer = TRUE)
