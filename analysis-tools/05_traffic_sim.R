library(tidyverse)
library(ggplot2)
library(ggspatial)
library(sf)
library(vroom)
library(fs)
source("../network-tools/xml2sqlite/convertXMLNetwork.R")

if (!file.exists("./sample_data/simOutputs/trip_links.txt")) {
  stop("processed sim outputs not found - use even2csv.sh code to convert")
}else{

### link volume analysis

# reading trip link events

tripLinks <- vroom("./sample_data/simOutputs/trip_links.txt")
# tripLinks %>% slice_sample(prop=0.1) %>% write_delim("./data/simOutputs/trip_links_small.txt")
# tripLinks <- vroom("./data/simOutputs/trip_links_small.txt") 

# adding mode to link events

vehicleTrips <- read_csv("./data/simOutputs/vehicle_trip.txt")

vehicleLists <- vehicleTrips %>% 
  distinct(person,vehicle,networkMode) 

tripLinksWithVehicle <- tripLinks %>% 
  left_join(vehicleLists, by = "vehicle") %>% 
  # Filtering to non-pt vehicles 
  filter(!(grepl("bus", vehicle) | grepl("train", vehicle) | grepl("tram", vehicle)))
# tripLinksWithVehicle %>% count(networkMode)

# We can either try to join to get the duration on each link, however, it will result in missing many records, instead here we only consider link exit event as our measure for count:
  
exitLinksWithTime <- tripLinksWithVehicle %>% 
  filter(type=="left_link") %>% 
  # mutate(id=row_number()) %>% 
  dplyr::select(-type) %>% 
  mutate(leftHour=  floor(time / 3600)) %>% 
  mutate(leftHMS=paste(sprintf("%02d",leftHour),
                       sprintf("%02d", floor(time %% 3600 / 60)),
                       sprintf("%02d", floor(time %% 60 )),
                       sep=":"))

#### Aggregating exiting link event by the hour of the day

# plotting link exit events

exitLinksWithTime %>% 
  group_by(leftHour) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=leftHour, y=n)) +
  geom_col() +
  labs(title = "Aggregated exiting link event count by the hour of the day") +
  xlab("Hour of the day") +
  ylab("Link exit even count")

## Network level analysis

# processing network XML

if (file.exists("./data/networks/networkConverted.sqlite")) {
  networkLinks <- st_read("./data/networks/networkConverted.sqlite", 
                          layer = "links", quiet=T)
}else{
  networkConverted <- convertXMLNetowrk(xmlFile = "./data/simOutputs/output_network.xml",
                                        netCRS = 28355)
  networkLinks <- networkConverted[[2]]
}

### Road network with Hourly traffic volume

# Joining Hourly car traffic data to road network

carExitLinkAggregatedHourly <- exitLinksWithTime %>%
  # carExitLinkAggregatedHourly <- tempDf %>% 
  filter(networkMode=="car") %>% # filter to car trips only
  mutate(id=as.character(link)) %>% 
  group_by(id, leftHour) %>% 
  summarise(hourly_vol=n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols=id, names_from=leftHour, values_from=hourly_vol) %>% 
  mutate(total_vol = rowSums(across(where(is.numeric)),na.rm = T))

networkLinksWithHourlyVol_car <- networkLinks %>% 
  left_join(carExitLinkAggregatedHourly, by = "id")   
st_write(networkLinksWithHourlyVol_car, 
         "./data/simOutputJoined/networkLinksWithHourlyVol.sqlite",
         layer="car",
         delete_layer = T)

# plotting Hourly car traffic volume

# networkLinksWithHourlyVol$hourly_vol <- st_read("networkLinksWithHourlyVol.sqlite")

networkLinksWithHourlyVol_car %>% 
  filter(total_vol>0) %>% 
  # filter(hourly_vol>10) %>% 
  ggplot() +
  # annotation_map_tile(type="osmgrayscale",zoom=9, alpha=0.6) +
  geom_sf(aes(fill=total_vol)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .8) 


# BIKE ----------

# Joining Hourly bike traffic data to road network

bikeExitLinkAggregatedHourly <- exitLinksWithTime %>%
  # bikeExitLinkAggregatedHourly <- tempDf %>% 
  filter(networkMode=="bike") %>% # filter to car trips only
  mutate(id=as.character(link)) %>% 
  group_by(id, leftHour) %>% 
  summarise(hourly_vol=n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols=id, names_from=leftHour, values_from=hourly_vol) %>% 
  mutate(total_vol = rowSums(across(where(is.numeric)),na.rm = T))

networkLinksWithHourlyVol_bike <- networkLinks %>% 
  left_join(bikeExitLinkAggregatedHourly, by = "id")   

st_write(networkLinksWithHourlyVol_bike, 
         "./data/simOutputJoined/networkLinksWithHourlyVol.sqlite",
         layer="bike",
         delete_layer = T)

# plotting Hourly bike traffic volume

# networkLinksWithHourlyVol$hourly_vol <- st_read("networkLinksWithHourlyVol.sqlite")

networkLinksWithHourlyVol_bike %>% 
  drop_na(total_vol) %>% 
  # filter(hourly_vol>10) %>% 
  ggplot() +
  # annotation_map_tile(type="osmgrayscale",zoom=9, alpha=0.6) +
  geom_sf(aes(fill=total_vol)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .8) 

}