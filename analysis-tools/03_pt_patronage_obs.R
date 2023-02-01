## Public transport


### Adding nodes from MATSim Network
# Add station nodes
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

# Making sure the output dir structure exists -----------
if (!dir.exists("./data/observationsJoined/")) dir.create("./data/observationsJoined/")

# Reading stations' patronage survey data

# stationData <- readxl::read_excel("~/ownCloud/io.github.jafshin/calibration-validation/data/observations/pt/2016 Station Access Mode.xlsx",sheet = "Sheet2", skip = 3)  
stationData <- readxl::read_excel("./sample_data/observations/pt/2016 Station Access Mode.xlsx",
                                  sheet = "Sheet2", skip = 3)  

stationDataFiltered <- stationData %>%   
  dplyr::select(-`Station Group`) %>% 
  filter(!is.na(Station)) %>% 
  mutate(stationID=stringr::str_extract(Station,pattern = "[0-9]+"))  %>% 
  mutate(stationName=stringr::str_extract(Station,pattern = "[A-z](.*)"))  
# 
# Reading stop locations from GTFS data

col_spec <- cols(
  stop_id = col_double(),
  stop_name = col_character(),
  stop_lat = col_double(),
  stop_lon = col_double()
)

# stationLocations <- read_csv("~/ownCloud/io.github.jafshin/calibration-validation/data/observations/pt/stops.txt", col_types = col_spec) %>% 
stationLocations <- read_csv("./data/observations/pt/stops.txt", col_types = col_spec) %>% 
  mutate(stationID=as.character(stop_id)) %>% 
  # filter to rows containing 'Railway Station' and not '/' (used for bus or tram stops at stations) 
  filter(grepl("Railway Station", stop_name) & !grepl("/", stop_name)) %>%
  # replace the pattern 'space + Railway + any number of other characters' with nothing
  mutate(stationName = gsub(" Railway.*","", stop_name)) %>%
  # fix some name mismatches between patronage and GTFS names
  mutate(stationName = if_else(stationName=="McKinnon", "Mckinnon",
                               if_else(stationName=="Jolimont-MCG", "Jolimont",
                                       if_else(stationName=="Showgrounds", "Showgrounds Station",
                                               stationName)))) %>%
  dplyr::select(stationName, stop_lat, stop_lon)

# Joining stop locations to the patronage data

stationDataCoordinated <- stationDataFiltered %>% 
  left_join(stationLocations, by="stationName") %>%
  # remove duplicates (eg where Vline contains metro and Vline with same name)
  distinct(stationName, .keep_all=T)  

write_csv(stationDataCoordinated, "stationDataCoordinated.csv")
# stationDataCoordinated %>% glimpse()

# Plotting the Station data

stationDataWithGeom <- stationDataCoordinated %>% 
  filter(!is.na(stop_lat)) %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), remove=F, crs = 4326) %>% 
  st_transform(28355) %>% 
  dplyr::select(stationName, stationID, Total=`Total Result`, geometry)

# Map of the train stops included for the calibration: 

stationDataWithGeom %>% 
  st_intersection(gMelbBoundary) %>%
  ggplot() +
  annotation_map_tile(type="osmgrayscale",zoom=9) +
  geom_sf(aes(color=Total, size= Total)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) 

stationDataJoined2Network <- addStationNodes(patronageData = stationDataWithGeom, 
                                       networkLinks,
                                       networkNodes,
                                       gMelbBoundary)
st_write(stationDataJoined2Network, "./data/observationsJoined/stationDataJoined2Network.sqlite", delete_dsn = T)

# Check that result is a connected network (except for Showgrounds and Flemington Racecourse)
# Note that there are also  other links for express services
# Note link gap between Highett and Cheltenham, because Southland station missing from patronage data
rail.lines <- networkLinks %>%
  filter(from_id %in% stationDataJoined2Network$node_id & 
           to_id %in% stationDataJoined2Network$node_id)

ggplot() + 
  geom_sf(data = stationDataJoined2Network, color = 'red') +
  geom_sf(data = rail.lines, color = 'blue')

# Further check that results form a connected graph of 216 stations

# make graph to check that all stops are in fact connected
rail.graph <- graph_from_data_frame(rail.lines[,c("from_id","to_id")], directed = F) %>%
  suppressWarnings()

# count vertices in largest subgraph
components <- clusters(rail.graph)
biggest_cluster_id <- which.max(components$csize)
vertices <- V(rail.graph)[components$membership == biggest_cluster_id]
no.vertices <- length(vertices)
no.vertices  # should be 216 (218 stations in patronage data, excl. Showgrounds and Flemington Racecourse)
