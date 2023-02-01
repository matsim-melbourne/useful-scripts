###############################################################################
# This code converts bicycle count data from vicroad to a format that         #
# can be used for simulation model's bike traffic comparison                  #
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

# Making sure the output dir structure exists -----------
if (!dir.exists("./data/observationsJoined/")) dir.create("./data/observationsJoined/")


# paths <- dir_ls("~/ownCloud/io.github.jafshin/calibration-validation/data/observations/bicycle/2018-03/",glob = "*.zip" )
paths <- dir_ls("./data/observations/bicycle/2018-03/",glob = "*.zip" )
# Extracting all the bike count data
walk(paths, ~unzip(.x, exdir = "data/cyclingVolFiles"))
# reading the data it
cyclingVol_paths <- dir_ls("./data/cyclingVolFiles/")
col_spec <- cols(
  DATA_TYPE = col_character(),
  TIS_DATA_REQUEST = col_double(),
  SITE_XN_ROUTE = col_double(),
  LOC_LEG = col_double(),
  DATE = col_character(),
  TIME = col_time(format = ""),
  CLASS = col_double(),
  LANE = col_double(),
  SPEED = col_double(),
  WHEELBASE = col_double(),
  HEADWAY = col_double(),
  GAP = col_double(),
  AXLE = col_double(),
  AXLE_GROUPING = col_double(),
  RHO = col_double(),
  VEHICLE = col_character(),
  DIRECTION = col_character()
)
cyclingVol <- map_dfr(cyclingVol_paths, ~ read_csv(.x, col_types = col_spec))

# Filtering to mid-week day data

cyclingVolFiltered <- cyclingVol %>% 
  filter(!DATE%in%c("12/03/2018", "30/03/2018", "31/03/2018")) %>% # Removing public holidays
  mutate(DATE=dmy(DATE)) %>% 
  mutate(DOW = wday(DATE, label=TRUE)) %>% 
  filter(DOW%in%c("Tue", "Wed", "Thu")) # Selecting mid-week days

# Counting cycling trips based on counter location, direction, date and hour of the day

cyclingVolCounted <- cyclingVolFiltered %>% 
  mutate(hour=hour(TIME)) %>% 
  group_by(SITE_XN_ROUTE, LOC_LEG, DIRECTION, DATE, hour) %>% 
  summarise(count=n()) %>% 
  ungroup()
# glimpse(cyclingVolCounted)

# Aggregating count data by averaging over all days

cyclingVolAverage <- cyclingVolCounted %>% 
  group_by(SITE_XN_ROUTE, LOC_LEG, DIRECTION, hour) %>% 
  summarise(avgCount=round(mean(count))) %>% 
  ungroup() %>% 
  mutate(siteNumber=as.character(SITE_XN_ROUTE)) %>% 
  mutate(directionID=as.character(LOC_LEG)) %>% 
  dplyr::select(siteNumber, directionID, dir=DIRECTION, hour, count=avgCount) 
# glimpse(cyclingVolAverage)

### Having a look at the final data 

cyclingVolAverage %>% count(siteNumber, directionID, dir) %>% 
  ggplot(aes(x=dir, y=n))+
  geom_col(aes(fill=dir))

#Plotting one of the count locations
cyclingVolAverage %>% filter(siteNumber==6411) %>% 
  ggplot(aes(x=hour, y=count)) +
  geom_point(aes(color=dir)) +
  geom_line(aes(color=dir))

# Plotting aggregated values 
cyclingVolAverage %>% 
  group_by(hour, siteNumber) %>% 
  summarise(routeVol=sum(count)) %>%
  summarise(total_vol=mean(routeVol)) %>% 
  ggplot(aes(x=hour, y=total_vol)) +
  geom_point() +
  geom_line()

### Adding coordinates to the count points

# Reading the location points

# cycleCountersMeta <- read_xlsx("~/ownCloud/io.github.jafshin/calibration-validation/data/observations/bicycle/VicRoads_Bike_Site_Number_Listing.xlsx",
cycleCountersMeta <- read_xlsx("./data/observations/bicycle/VicRoads_Bike_Site_Number_Listing.xlsx",
                               skip=2, col_names=c("id", "site", "gps", "desc", "comment")) 
# cycleCountersMeta %>% glimpse()

# Adding the location points and MATSim network links to the count data

# Notes: 
#  - VicRoads site no 7, (BIKE LANE) FLEMINGTON RD NB 10M SE OF DRYBURGH ST, may select the link in Flemington Road immediately north-east of Dryburgh St, rather than immediately south-east
# - VicRoads site no 10, (BIKE LANE) ROYAL PDE NB 10M N OF GATEHOUSE ST, may select the link in Royal Pd immediately south of Gatehouse St, rather than immediately north

cyclingObsJoined2Network <- addBikeLinks(cyclingVolAverage = cyclingVolAverage,
                                         cycleCountersMeta = cycleCountersMeta,
                                         networkLinks,
                                         networkNodes)
st_write(cyclingObsJoined2Network, "./data/observationsJoined/cyclingObsJoined2Network.sqlite", delete_layer=T)
# cyclingObsJoined2Network %>% glimpse()

# Mapping selected cycling count locations

cyclingObsJoined2Network %>% 
  group_by(siteNumber,directionID) %>% 
  summarise(total_vol=sum(count)) %>%  
  ggplot() +
  annotation_map_tile(type="osmgrayscale",zoom=10, alpha=0.8) +
  geom_sf(aes(color=total_vol))  
