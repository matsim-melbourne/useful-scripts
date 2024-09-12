#!/usr/bin/env Rscript

# libraries and functions -------------------------------------------------
suppressPackageStartupMessages(library(sf)) # for spatial things
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for manipulating data
suppressPackageStartupMessages(library(tidytransit)) # for processing gtfs
suppressPackageStartupMessages(library(hms)) # for processing time



# data locations ----------------------------------------------------------
gtfs_location <- "sa3-to-sa1/data/gtfs_au_vic_ptv_20191004.zip"
sa1_location  <- "sa3-to-sa1/data/1270055001_sa1_2016_aust_shape/SA1_2016_AUST.shp"
sa3_location  <- "sa3-to-sa1/data/1270055001_sa3_2016_aust_shape/SA3_2016_AUST.shp"

# Data URLs for sa1 and sa3 regions. These will need to be unzipped inside the data folder
# https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa1_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&6F308688D810CEF3CA257FED0013C62D&0&July%202016&12.07.2016&Latest
# https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa3_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&43942523105745CBCA257FED0013DB07&0&July%202016&12.07.2016&Latest

# The gtfs file is the one used in: https://github.com/matsim-melbourne/demand

# read in and clean the sa1 and sa3 regions -------------------------------
sa3 <- st_read(sa3_location) %>%
  filter(STE_NAME16=="Victoria") %>%
  mutate(sa3_code=as.integer(SA3_CODE16)) %>%
  dplyr::select(sa3_code) %>%
  st_transform(7899) # to VicGrid GDA2020

sa1 <- st_read(sa1_location) %>%
  filter(STE_NAME16=="Victoria") %>%
  mutate(sa1_code=as.numeric(SA1_MAIN16)) %>%
  mutate(sa3_code=as.integer(SA3_CODE16)) %>%
  dplyr::select(sa1_code,sa3_code) %>%
  st_transform(7899) # to VicGrid GDA2020



# distance from sa3 centroid ----------------------------------------------
sa1_centroids <- sa1 %>%
  st_centroid() %>%
  arrange(sa1_code)

sa3_centroids <- sa3 %>%
  st_centroid()

# we need the sa3 location for each sa1 region to make st_distance work properly
sa3_centroids_long <- inner_join(
  sa1_centroids%>%st_drop_geometry(),
  sa3_centroids,
  by="sa3_code") %>% 
  st_sf() %>%
  arrange(sa1_code)

centroid_distance <- st_distance(sa1_centroids,sa3_centroids_long,by_element=T)

sa1_no_geom <- sa1_centroids %>% st_drop_geometry()
sa1_no_geom$centroid_distance <- as.numeric(centroid_distance)



# GTFS --------------------------------------------------------------------
gtfs <- read_gtfs(gtfs_location)

# extract stops with their locations, filtered to study area
stops <- gtfs$stops %>%
  st_as_sf(coords=c("stop_lon", "stop_lat"), crs=4326) %>%
  st_transform(7899)

# table of stops and route types
stop_types <- gtfs$stop_times %>%
  left_join(gtfs$trips, by = "trip_id") %>%
  left_join(gtfs$routes, by = "route_id") %>%
  # keep only distinct stop_id and route_type combinations
  dplyr::select(stop_id, agency_id) %>%
  distinct() %>%
  filter(agency_id %in% c(1,2,4,5,6)) %>%
  mutate(pt_stop_type = case_when(
    agency_id == 1  ~ "1. vline",
    agency_id == 2  ~ "3. train",
    agency_id == 4  ~ "4. bus",
    agency_id == 5  ~ "2. regional bus",
    agency_id == 6  ~ "2. regional bus"
  )) %>%
  dplyr::select(stop_id, pt_stop_type)

stops_final <- stops %>%
  left_join(stop_types, by = "stop_id") %>%
  dplyr::select(pt_stop_type)

rm(gtfs)

tmp <- stops_final%>%
  filter(pt_stop_type=="1. vline")



# putting it all together -------------------------------------------------
sa1_vline <- sa1 %>%
  st_filter(
    stops_final%>%filter(pt_stop_type=="1. vline"),
    .predicate=st_intersects) %>%
  st_drop_geometry() %>%
  dplyr::select(sa1_code) %>%
  mutate(pt_type="1. vline")

sa1_regbus <- sa1 %>%
  st_filter(
    stops_final%>%filter(pt_stop_type=="2. regional bus"),
    .predicate=st_intersects) %>%
  st_drop_geometry() %>%
  dplyr::select(sa1_code) %>%
  mutate(pt_type="2. regional bus")

sa1_train <- sa1 %>%
  st_filter(
    stops_final%>%filter(pt_stop_type=="3. train"),
    .predicate=st_intersects) %>%
  st_drop_geometry() %>%
  dplyr::select(sa1_code) %>%
  mutate(pt_type="3. train")

sa1_bus <- sa1 %>%
  st_filter(
    stops_final%>%filter(pt_stop_type=="4. bus"),
    .predicate=st_intersects) %>%
  st_drop_geometry() %>%
  dplyr::select(sa1_code) %>%
  mutate(pt_type="4. bus")

sa1_pt <- bind_rows(
  sa1_vline,
  sa1_regbus,
  sa1_train,
  sa1_bus
)

sa1_final <- sa1_no_geom %>%
  left_join(sa1_pt, by="sa1_code") %>%
  arrange(sa1_code,pt_type,centroid_distance) %>%
  group_by(sa1_code) %>%
  slice_head() %>%
  ungroup()

sa3_final <- sa1_final %>%
  arrange(sa3_code,pt_type,centroid_distance) %>%
  group_by(sa3_code) %>%
  slice_head() %>%
  ungroup()

write.csv(sa3_final, "nominated_sa1.csv", row.names=F)

# to check on a map
# st_write(sa1%>%inner_join(sa3_final, by="sa1_code"), "nominated_sa1.sqlite", delete_dsn=T)
