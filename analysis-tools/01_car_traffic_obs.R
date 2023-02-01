###############################################################################
# This code converts typical hourly volume data from DOT to the format that   #
# can be used for simulation model's car traffic comparison                   #
###############################################################################

library(tidyverse)
library(fs)
library(sf)
library(lubridate)
library(readxl)
# library(rosm)
library(ggspatial)
library(lwgeom)
library(stringr)
library(igraph)
library(nngeo)  # for st_azimuth

source('../network-tools/network-modification/addNetworkDetails.R')
source('../network-tools/xml2sqlite/convertXMLNetwork.R')
source('./prepareCarData.R')

# Making sure the output dir structure exists -----------
if (!dir.exists("./data/observationsJoined/")) dir.create("./data/observationsJoined/")

# Reading Data ------------

carDataFile="./sample_data/observations/car/TYPICAL_HOURLY_VOLUME_DATA.csv"
htvnDataFile="./sample_data/observations/car/homogenous_traffic_flow-shp/3ad2d9d5-dd49-40bf-86c4-ccf114d2e4582020328-1-cs85vw.3kgy7.shp"

gMelbBoundary <- st_read("./sample_data/boundaries/GreaterMelbourneArea/GMEL_SA4_2016_Polygons.sqlite")  %>% 
  st_transform(28355) %>%
  summarise() 

# Data prep -----------------------

# making sure that car data is in the proper format
if (file.exists("./sample_data/observations/car/carDataCroped.sqlite")) {
  carDataCroped <- st_read("./sample_data/observations/car/carDataCroped.sqlite", 
                           quiet=T)
}else{
  prepareCarData(carDataFile="./sample_data/observations/car/TYPICAL_HOURLY_VOLUME_DATA.csv",
                 htvnDataFile="./sample_data/observations/car/homogenous_traffic_flow-shp/3ad2d9d5-dd49-40bf-86c4-ccf114d2e4582020328-1-cs85vw.3kgy7.shp",
                 gMelbBoundary)
  carDataCroped <- st_read("./sample_data/observations/car/carDataCroped.sqlite", 
                           quiet=T)
}

# Making sure that the network exists and is in correct format:

if (file.exists("./sample_data/networks/networkConverted.sqlite")) {
  networkLinks <- st_read("./sample_data/networks/networkConverted.sqlite", 
                          layer = "links", quiet=T) %>% 
    rename(link_id=id, from_id=from, to_id=to, highway=type) %>%  
    mutate(is_oneway=1)
  networkNodes <- st_read("./sample_data/networks/networkConverted.sqlite", 
                          layer = "nodes", quiet=T) 
}else{
  if (!dir.exists("./sample_data/networks/")) dir.create("./sample_data/networks/")
  networkConverted <- convertXMLNetowrk(xmlFile = "./sample_data/simOutputs/output_network.xml.gz",
                                        netCRS = 28355)
  networkNodes <- networkConverted[[1]]
  networkLinks <- networkConverted[[2]] %>% 
    rename(link_id=id, from_id=from, to_id=to, highway=type) %>%  
    mutate(is_oneway=1)
}

## AM volume --------------------------
amCarData <- carDataCroped %>% 
  filter(tis_route1==0)

amHighVolRoads <- amCarData %>% 
  st_drop_geometry() %>% 
  group_by(road_nbr) %>% 
  summarise(road_vol=mean(total_vol)) %>% 
  slice_max(order_by = road_vol, prop=0.1)

amHighVolCarData <- amCarData %>% 
  filter(road_nbr %in% amHighVolRoads$road_nbr) %>% 
  group_by(road_nbr) %>% 
  slice_max(order_by = total_vol, n=1)

# Plot of AM Peak Count locations

amHighVolCarData %>% 
  st_centroid() %>% 
  ggplot() + 
  annotation_map_tile(type="osmgrayscale",zoom=10, alpha=0.6) +
  geom_sf(aes(color=total_vol)) +
  # scale_fill_viridis_c(trans = "sqrt", alpha = .8) +
  scale_color_viridis_c(option="magma", name="Traffic volume") +
  theme_void() + 
  guides( colour = guide_legend()) +
  theme(
    legend.position = c(0.15, 0.15),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )

ggsave("~/Dropbox/Apps/Overleaf/MATSimMelbournePaper/figs/carAmVol.png",
       width = 7, height = 7)

# Joining it to the network

carAmObsJoined2Network <- addCarLinks(carData = amHighVolCarData,
                                      links= networkLinks,
                                      nodes= networkNodes)
st_write(carAmObsJoined2Network, 
         "./sample_data/observationsJoined/carAmObsJoined2Network.sqlite", 
         delete_dsn = T)
#glimpse(carAmObsJoined2Network)

## PM volume -------

pmCarData <- carDataCroped %>% 
  filter(tis_route1==1)

pmHighVolRoads <- pmCarData %>% 
  st_drop_geometry() %>% 
  group_by(road_nbr) %>% 
  summarise(road_vol=mean(total_vol)) %>% 
  slice_max(order_by = road_vol, prop=0.1)
pmHighVolCarData <- pmCarData %>% 
  filter(road_nbr %in% pmHighVolRoads$road_nbr) %>% 
  group_by(road_nbr) %>% 
  slice_max(order_by = total_vol, n=1)
# st_write(pmHighVolCarData, "./data/observationsJoined/pmHighVolCarData.sqlite", delete_dsn = T)


pmHighVolCarData %>% 
  st_centroid() %>% 
  ggplot() + 
  annotation_map_tile(type="osmgrayscale",zoom=10, alpha=0.6) +
  geom_sf(aes(color=total_vol)) +
  # scale_fill_viridis_c(trans = "sqrt", alpha = .8) +
  scale_color_viridis_c(option="magma", name="Traffic volume") +
  theme_void() + 
  guides( colour = guide_legend()) +
  theme(
    legend.position = c(0.15, 0.15),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )

ggsave("~/Dropbox/Apps/Overleaf/MATSimMelbournePaper/figs/carPmVol.png",
       width = 7, height = 7)

carPmObsJoined2Network <- addCarLinks(carData = pmHighVolCarData, 
                                      links= networkLinks,
                                      nodes= networkNodes)
st_write(carPmObsJoined2Network, "./data/observationsJoined/carPmObsJoined2Network.sqlite", delete_dsn = T)
glimpse(carPmObsJoined2Network)