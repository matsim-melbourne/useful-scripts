## Walking Data

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

# Reading the walk count data
# Source data: 
  # https://data.melbourne.vic.gov.au/Transport/Pedestrian-Counting-System-Monthly-counts-per-hour/b2ak-trbp

col_spec <- cols(
  ID = col_double(),
  Date_Time = col_character(),
  Year = col_double(),
  Month = col_character(),
  Mdate = col_double(),
  Day = col_character(),
  Time = col_double(),
  Sensor_ID = col_double(),
  Sensor_Name = col_character(),
  Hourly_Counts = col_double()
)
walkData <- read_csv("./sample_data/observations/walk/Pedestrian_Counting_System_-_Monthly__counts_per_hour_.csv", col_types = col_spec) 
# walkData %>% glimpse()

walkDataFiltered <- walkData %>% 
  filter(Year==2018,Month=="March") %>% 
  filter(Day%in%c("Tuesday", "Wednesday", "Thursday"))
walkDataFiltered %>% count(Year,Month,Day)

# walkDataFiltered %>% glimpse()

### Adding location to the count data

# Source for sensor locations:
#   https://data.melbourne.vic.gov.au/Transport/Pedestrian-Counting-System-Sensor-Locations/h57g-5234

col_spec <- cols(
  sensor_id = col_double(),
  sensor_description = col_character(),
  sensor_name = col_character(),
  installation_date = col_date(format = ""),
  status = col_character(),
  note = col_character(),
  direction_1 = col_character(),
  direction_2 = col_character(),
  latitude = col_double(),
  longitude = col_double(),
  location = col_character()
)
# walkSensorLocs <- read_csv("~/ownCloud/io.github.jafshin/calibration-validation/data/observations/walk/Pedestrian_Counting_System_-_Sensor_Locations.csv", col_types = col_spec)
walkSensorLocs <- read_csv("./data/observations/walk/Pedestrian_Counting_System_-_Sensor_Locations.csv", col_types = col_spec)
walkSensorLocs %>% glimpse()

walkDataJoined <- walkDataFiltered %>% 
  left_join(walkSensorLocs, by=c("Sensor_ID"="sensor_id")) 
walkDataJoined %>% glimpse()

walkDataCoordinated <- walkDataJoined %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(28355) %>% 
  dplyr::select(ID,Sensor_ID, Sensor_Name, Hourly_Counts, direction_1, 
                direction_2, Time, geometry)
# st_write(walkDataCoordinated, "./data/observationsJoined/walkDataCoordinated.sqlite", delete_layer=T)
walkDataCoordinated %>% glimpse()

# Mapping walking count point locations

walkDataCoordinated %>% 
  group_by(Sensor_ID) %>% 
  summarise(total_vol=sum(Hourly_Counts)) %>% 
  ggplot() +
  annotation_map_tile(type="osmgrayscale",zoom=12, alpha=0.8) +
  geom_sf(aes(color=total_vol))  

### Adding links from MATSim Network

# Each Sensor has two directions (eg direction_1: North; direction_2: South).  This step adds fields link_id_1, from_id_1 and to_id_2 for the MATSim Network links and nodes for direction_1; and link_id_2, from_id_2 and to_id_2 for direction_2.
# 
# Many Sensors are on one side of a road only, eg Sensor 36 (Queen St near Collins St, west side). In that case, usually the links for the Sensor are:
#   - the same, being a two-directional link that represents the relevant road/path, or
# - different, representing two one-directional links that each represent the relevant road/path.
# In these caes, as the Sensor is on one side of the road only, its observations should cover about 50% of the total number of pedestrians using the link or links in both directions (that is, the pedestrians walking on both sides of the road).
# 
# In other cases, the Sensor represents the whole of a path in both directions, eg Sensor 25 (shared use path near Melbourne Convention & Exhibition Centre).  In that case, usually the link for the sensor is a two-directional link that represents the path.  Its observations should cover the total number of pedestrians using the link in both directions.
# 
# However, there are many cases where a road is represented by two two-directional links; or by three or four links.  In these cases, one or two of these links will be selected for the Sensor, but the Sensor's observations will include pedestrians using both the selected links, and other unselected links that also represent the same road.  Examples include the following Sensors:
# - 1 & 2 (Bourke St Mall - 3 links)
# - 4, 15, 54 & 66 (locations in Swanston St - two 2-way links: footway and roadway)
# - 17 & 18 (Collins St near Exhibition St - two 2-way links: footway and roadway)
# - 21 (Russel St near Bourke St - 4 links)
# - 30 & 56 (locations in Lonsdale St - 3 links)
# - 44 (Swanston St near Elgin St - 3 links)
# - 48 (Victoria St near Elizabeth St - two 2-way links: footway and roadway)
# - 49 (Therry St - 2-way walkway and 1-way roadway)
# - 64 (Royal Pde near Grattan St - 3 links)
# - 75 (Spring St near Flinders St - 3 links)
# 
# Sensor 5 (Princes Bridge) selects the link in Swanston St just north of Princes Bridge, rather than Princes Bridge itself.  This is not necessarily a bad outcome, because the link which represents Princes Bridge is selected by Sensor 29 (St Kilda Rd - Alexandra Gardens).
# 
# Sensors 19 & 20 (Little Bourke Street) return NA results in eastbound direction.  This is because the street is one way, and there is no adjacent walking link in the correct direction.
# 
# Sensor 57 (Bourke St Bridge) selects a link which crosses Wurundjeri Way (as the Bourke St Bridge does); but the full bridge across the rail station is not included in the MATSim Network, so cannot be selected.
# 
# Sensors 77 and 78 (Harbour Esplanade pedestrian path and bike path) both select the same link.  This is because that link represents both of those paths in the MATSim network.

 walkObsJoined2Network <- addWalkLinks(walkData = walkDataCoordinated,
                                        walkSensorLocs = walkSensorLocs,
                                        networkLinks,
                                        networkNodes)

 st_write(walkObsJoined2Network, "./data/observationsJoined/walkObsJoined2Network.sqlite",
          delete_dsn = T)

 # walkDataCoordinated %>% glimpse()
