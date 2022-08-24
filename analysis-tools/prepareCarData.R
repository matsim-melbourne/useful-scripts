prepareCarData <- function(carDataFile="./data/observations/car/TYPICAL_HOURLY_VOLUME_DATA.csv",
                           htvnDataFile="./data/observations/car/homogenous_traffic_flow-shp/3ad2d9d5-dd49-40bf-86c4-ccf114d2e4582020328-1-cs85vw.3kgy7.shp",
                           gMelbBoundary
                           ){
  ###############################################################################
  # Day of Week where 1=Monday, 2=Tuesday,3=Wednesday,                          #
  # 4=Thursday,5=Friday,6=Saturday and 7=Sunday                                 #
  ###############################################################################
  # A=Actual count, Ehis=Estimate based on historic data,                       #
  # Egmr=Estimate based on global substitution, Erte=Estimate based on route    #
  ###############################################################################
  print("Processing Car Data")
  # carData <- read_csv("~/ownCloud/io.github.jafshin/calibration-validation/data/observations/car/TYPICAL_HOURLY_VOLUME_DATA.csv", col_types = col_spec)
  carData <- read_csv(carDataFile)
  
  carDataFiltered <- carData %>% 
    filter(PERIOD_TYPE=="SCHOOL TERM/NORMAL") %>% 
    filter(DOW==3) %>% 
    # Filter to those with high enough number traffic
    mutate(HMGNS_FLOW_ID=as.character(HMGNS_FLOW_ID)) %>% 
    mutate(HMGNS_LNK_ID=as.character(HMGNS_LNK_ID)) %>% 
    dplyr::select(-DOW) %>% 
    mutate(total_vol=rowSums(across(where(is.numeric)))) 
  
  
  # TIS_ROUTE_FLOW=	Traffic Information Route Flow (1=Generally out of Melb, Generally 0= into Melb)
  # HMGNS_FLOW_ID	Text	10	Homogeneous flow id eg 1234. An internal reference for Homogenous Flow. Homogeneous flow - The traffic volume information associated with the traffic flow along a link that is representative of all travel along the whole link
  # ROAD_NBR	Number	8	ROAD_NUMBER
  
  # htvnData <- st_read("~/ownCloud/io.github.jafshin/calibration-validation/data/observations/car/homogenous_traffic_flow-shp/3ad2d9d5-dd49-40bf-86c4-ccf114d2e4582020328-1-cs85vw.3kgy7.shp") %>% 
  htvnData <- st_read(htvnDataFile) %>% 
    st_transform(28355) %>% 
    mutate(HMGNS_FLOW_ID=as.character(HMGNS_FLOW))
  carDataJoined <- carDataFiltered %>%
    left_join(htvnData, by="HMGNS_FLOW_ID") %>% 
    st_as_sf()
  # st_write(carDataJoined, "./data/observationsJoined/carDataJoined.sqlite", delete_dsn = T)
  
  # gMelbBoundary <- st_read("~/ownCloud/Data/ABS_Boundaries/GreaterMelbourneArea/GMEL_SA4_2016_Polygons.sqlite")  %>% 
  # gMelbBoundary <- st_read(sa4BoundaryFile)  %>% 
  #   st_transform(28355) %>%
  #   summarise()  # to dissolve SA4s (without this, roads get split at SA4 boundaries)
  
  carDataCroped <- carDataJoined %>% 
    st_intersection(gMelbBoundary)
  
  st_write(carDataCroped, "./data/observations/car/carDataCroped.sqlite", 
           delete_dsn = T,
           quiet=T)
  
  return()
}

