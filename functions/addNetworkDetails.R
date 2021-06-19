# 1. Car data
# -----------------------------------------------------------------------------
# Function to add links from network to car data.  Approach:
# - filter Network to freeways and main roads
# - select links in a 300m buffer around each road of interest, and dupicate any one way
# - filter out any that are in wrong direction
# - filter out any where azimuth (bearing) is >17.5 degrees away from road
# - select the remaining link closest to the road midpoint
# - add the link row number to the car data
# - also add the link's from and to node id's

addCarLinks <- function(carData,
                        links,
                        nodes,
                        # network = "./generatedNetworks/MATSimMelbNetwork.sqlite",
                        # azimuth tolerance (to filter out links > 17.5 degrees from road azimuth)
                        az.tol = 17.5) {
  
   carData = amHighVolCarData; links= networkLinks; nodes= networkNodes; az.tol = 17.5
  
  # read in car data, and add columns for link_row and node from_ and to_ id's 
  carData <- carData %>% 
    st_as_sf() %>% 
    mutate(link_row = "NA", from_id = "NA", to_id = "NA")
  # read in links and nodes
  links <- links %>%
    # filter to main roads only ( don't include trunk_link, primary_link, secondary_link)
    filter(highway %in% c("motorway", "motorway_link", "trunk", "primary", "secondary"))

  nodes <- nodes %>%
    # filter to nodes used in links, to remove any disconnected (not really necessary)
    filter(id %in% links$from_id | id %in% links$to_id)
  # i=1
  for (i in 1:nrow(carData)) { 
    # find direction of road
    direction <- carData[i, "flow"] %>%
      st_drop_geometry() %>%
      stringr::word(., 1)  # first word in the "flow" column for the road: EAST, WEST, NORTH or SOUTH
    
    # find azimuth of road
    point1 <- lwgeom::st_startpoint(carData[i, ])
    point2 <- lwgeom::st_endpoint(carData[i, ])
    azimuth <- st_azimuth(point1, point2)
    
    # buffer the road to 300m
    local.area <- st_buffer(carData[i, ], 300)
    
    # find links in the local area, and duplicate in opposite direction if two way
    filtered.links <- links %>%
      filter(st_intersects(GEOMETRY, local.area, sparse = FALSE)) %>% 
      mutate(correct_dir = 0, correct_az = 0)
    
    # Commenting these out as now we are starting from XML network and all links are already uni-directional in the xml - AJ 18 Jun 2021 
    # potential.links.oneway <- filtered.links %>%
    #   filter(is_oneway == 1)
    # 
    # potential.links.twoway <- filtered.links %>%
    #   filter(is_oneway == 0)
    # 
    # potential.links.twoway.reversed <- potential.links.twoway %>%
    #   mutate(new.from = to_id, new.to = from_id) %>%
    #   mutate(from_id = new.from, to_id = new.to)
    # 
    # potential.links <- dplyr::bind_rows(potential.links.oneway,
    #                                     potential.links.twoway,
    #                                     potential.links.twoway.reversed) %>%
    potential.links <- filtered.links %>% 
      dplyr::select(link_id, from_id, to_id)
    
    # filter to links in correct direction and with correct azimuth
    # j=1
    for (j in 1:nrow(potential.links)) {
      link <- potential.links[j, ]
      
      # set correct_dir to 1 if correct direction
      startpoint <- nodes[nodes$id == link$from_id, ]
      endpoint <- nodes[nodes$id == link$to_id, ]
      if (# startpoint has lower easting coordinate
          direction == "EAST" & startpoint$x < endpoint$x | 
          # startpoint has higher easting coordinate
          direction == "WEST" & startpoint$x > endpoint$x | 
          # startpoint has lower northing coordinate
          direction == "NORTH" & startpoint$y < endpoint$y | 
          # startpoint has higher northing coordinate
          direction == "SOUTH" & startpoint$y > endpoint$y) {
        potential.links[j, "correct_dir"] <- 1
      }

      # find azimuth for link
      link.az <- st_azimuth(startpoint, endpoint)
      potential.links[j, "azimuth"] <- link.az  # not used - just for debugging
      
      # if  link.az is within tolerance, set "correct_az" to 1
      if (azimuth-az.tol >= 0 & azimuth+az.tol <= 360) {
        if (link.az > azimuth-az.tol & link.az < azimuth+az.tol) {
          potential.links[j, "correct_az"] <- 1
        }
      } else if (azimuth-az.tol < 0) {
        if (link.az > azimuth-az.tol+360 | link.az < azimuth+az.tol) {
          potential.links[j, "correct_az"] <- 1
        }
      } else if (azimuth+az.tol > 360) {
        if (link.az > azimuth-az.tol | link.az < azimuth+az.tol-360) {
          potential.links[j, "correct_az"] <- 1
        }
      }
    }
    
    potential.links <- potential.links %>%
      filter(correct_dir == 1 & correct_az == 1)

    # get midpoint of road (contained in the data)
    midpoint <- carData[i, ] %>% 
      st_drop_geometry() %>%
      st_as_sf(coords = c("midpnt_lon", "midpnt_lat"), remove = F, crs = 4326) %>%
      st_transform(28355)
    
    # find filtered link closest to the midpoint (st_nearest_feature returns the index)
    closest.link <- potential.links[st_nearest_feature(midpoint, potential.links), ]
    # carData <- carData %>% st_as_sf()
    # complete link_row with row number of closest.link
    carData$link_row[i] <- as.character(closest.link$link_id)
    
    # complete from_id and to_id with node numbers for closest link
    carData$from_id[i] <- closest.link$from_id
    carData$to_id[i] <- closest.link$to_id
  }
  return(carData)
}



# 2. Cycling data
# -----------------------------------------------------------------------------
# Function to add links from network to cycling data.  Approach:
# - filter Network to separate bike lane and bike path networks
# - select links from relevant subnetwork in a 100m buffer around each sampling site, and dupicate any one way
# - filter out any that are in wrong direction
# - select the remaining link closest to the sampling site
# - add the link row number to the cycling data
# - also add the link's from and to node id's

addBikeLinks <- function(cyclingVolAverage,
                         cycleCountersMeta,
                         links,
                         nodes) {
                         # network = "./generatedNetworks/MATSimMelbNetwork.sqlite") {
  cycleCounterCoordinated <- cycleCountersMeta %>% 
    # Extracting the site number
    mutate(siteNumber=stringr::str_extract(site,pattern = "X[0-9]+")) %>% 
    mutate(siteNumber=stringr::str_extract(siteNumber,pattern = "[0-9]+")) %>%
    # Extracting the long lat
    mutate(lat=stringr::str_extract(gps,pattern = "-[0-9]+.[0-9]+")) %>% 
    mutate(long=stringr::str_extract(gps,pattern = "\\+[0-9]+.[0-9]+")) %>% 
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
    st_transform(28355) %>% 
    # Extracting the type ('lane' if 'BIKE LANE'; otherwise 'path' (all others are 'BIKE PATH'))
    mutate(type = ifelse(grepl("BIKE LANE", desc), "lane", "path")) %>%
    dplyr::select(siteNumber, desc, comment, geometry, type)
  
  # find direction for one way sites
  oneway.sites <- cycleCounterCoordinated %>%
    # filter to duplicate sites (one for each direction)
    group_by(siteNumber) %>%
    mutate(duplicate = n()) %>%
    filter(duplicate > 1) %>%
    ungroup() %>%
    mutate(orig.easting = st_coordinates(geometry)[, "X"],
           orig.northing = st_coordinates(geometry)[, "Y"]) %>%
    # if road runs N-S, lower easting is northbound
    group_by(siteNumber) %>%
    arrange(., orig.easting) %>%
    mutate(N_S = ifelse(row_number() == 1, "N", "S")) %>%
    ungroup() %>%
    # if road runs E-W, lower northing is westbound
    group_by(siteNumber) %>%
    arrange(., orig.northing) %>%
    mutate(E_W = ifelse(row_number() == 1, "W", "E")) %>%
    ungroup() #%>%

  # find direction for two way sites
  twoway.sites <- cycleCounterCoordinated %>%
    filter(!siteNumber %in% oneway.sites$siteNumber) %>%
    # add original easting/northing
    mutate(orig.easting = st_coordinates(geometry)[, "X"],
           orig.northing = st_coordinates(geometry)[, "Y"]) %>%
    # move easting for one location 5m to west, to avoid joining to wrong path
    mutate(new.easting = ifelse(grepl("SEACOMBE GROVE", desc), orig.easting - 5,
                                orig.easting),
           new.northing = orig.northing) %>%
    st_drop_geometry() %>%
    st_as_sf(coords = c("new.easting", "new.northing"), crs = 28355)
  
  # make location table, joining locations to one and two way sites
  locationTable <- cyclingVolAverage %>%
    dplyr::select(siteNumber, directionID, dir) %>%
    distinct()
  
  locationTable.onewayNS <- locationTable %>%
    left_join(., oneway.sites, by = c("siteNumber" = "siteNumber", "dir" = "N_S")) %>%
    filter(!is.na(desc))
  
  locationTable.onewayEW <- locationTable %>%
    left_join(., oneway.sites, by = c("siteNumber" = "siteNumber", "dir" = "E_W")) %>%
    filter(!is.na(desc))
  
  locationTable.twoway <- locationTable %>%
    left_join(., twoway.sites, by = "siteNumber") %>%
    filter(!is.na(desc))
  
  locationTable <- dplyr::bind_rows(locationTable.onewayNS,
                                    locationTable.onewayEW,
                                    locationTable.twoway) %>%
    dplyr::select(siteNumber, directionID, dir, desc, comment, geometry, orig.easting, orig.northing, type) %>%
    st_as_sf(.)
  
  # read in links and nodes
  # links <- st_read(network, layer = "links", quiet=T) %>%
  #   mutate(link_id = row_number())
  
  lane.links <- links %>%
    filter(cycleway %in% c("lane", "seperated_lane", "shared_lane"))
  
  path.links <- links %>%
    filter(cycleway == "bikepath")
  
  nodes <- nodes %>%
    # filter to nodes used in links, to remove any disconnected (not really necessary)
    filter(id %in% lane.links$from_id | id %in% lane.links$to_id | 
             id %in% path.links$from_id | id %in% path.links$to_id)
  
  for (i in 1:nrow(locationTable)) {
    
    # get direction of site
    direction <- locationTable[i, "dir"] %>%
      st_drop_geometry()
    
    # get type of site (lane or path)
    type <- locationTable[i, "type"] %>%
      st_drop_geometry()
    
    # buffer the location to 100m
    local.area <- st_buffer(locationTable[i, ], 100)
    
    # find links (lane or path) in the local area, and duplicate in opposite direction if two way
    if (type == "lane") {
      relevant.links <- lane.links
    } else if (type == "path") {
      relevant.links <- path.links
    }
    
    filtered.links <- relevant.links %>%
      filter(st_intersects(GEOMETRY, local.area, sparse = FALSE)) %>%
      mutate(correct_dir = 0)
    
    # check that 'filtered links' is not empty; if it is, check the other link network
    # (required for Napier St Fitzroy - a lane in the Network, but actually a 10m path connecting two road segments)
    if (nrow(filtered.links) == 0) {
      if (type == "lane") {
        relevant.links <- path.links
      } else if (type == "path") {
        relevant.links <- lane.links
      }
      filtered.links <- relevant.links %>%
        filter(st_intersects(GEOMETRY, local.area, sparse = FALSE)) %>%
        mutate(correct_dir = 0)
    }
    
    potential.links.oneway <- filtered.links %>%
      filter(is_oneway == 1)
    
    potential.links.twoway <- filtered.links %>%
      filter(is_oneway == 0)
    
    potential.links.twoway.reversed <- potential.links.twoway %>%
      mutate(new.from = to_id, new.to = from_id) %>%
      mutate(from_id = new.from, to_id = new.to)
    
    potential.links <- dplyr::bind_rows(potential.links.oneway,
                                        potential.links.twoway,
                                        potential.links.twoway.reversed) %>%
      dplyr::select(link_id, from_id, to_id, correct_dir)
    
    # filter to links in correct direction
    for (j in 1:nrow(potential.links)) {
      link <- potential.links[j, ]
      
      # set correct_dir to 1 if correct direction
      startpoint <- nodes[nodes$id == link$from_id, ]
      endpoint <- nodes[nodes$id == link$to_id, ]
      if (direction == "E" & startpoint$x < endpoint$x | # startpoint has lower easting coordinate
          direction == "W" & startpoint$x > endpoint$x | # startpoint has higher easting coordinate
          direction == "N" & startpoint$y < endpoint$y | # startpoint has lower northing coordinate
          direction == "S" & startpoint$y > endpoint$y) # startpoint has higher northing coordinate
      {
        potential.links[j, "correct_dir"] <- 1
      }
    }
    
    potential.links <- potential.links %>%
      filter(correct_dir == 1)
    
    # find potential link closest to the site (st_nearest_feature returns the index)
    closest.link <- potential.links[st_nearest_feature(locationTable[i, ], potential.links), ]
    
    # complete link_row with row number of closest.link
    locationTable[i, "link_row"] <- closest.link$link_id
    
    # complete from_id and to_id with node numbers for closest link
    locationTable[i, "from_id"] <- closest.link$from_id
    locationTable[i, "to_id"] <- closest.link$to_id
    
    # restore sf class to locationTable (previous rows drop it)
    locationTable <- locationTable %>% st_as_sf(.)
  }
  
  # restore original geometry
  locationTable <- locationTable %>%
    st_drop_geometry() %>%
    st_as_sf(coords = c("orig.easting", "orig.northing"), crs = 28355) %>%
    dplyr::select(-type)
  
  # join locations to average volumes
  cyclingVolCoordinated <- cyclingVolAverage %>% 
    left_join(locationTable, by = c("siteNumber","dir", "directionID")) %>% 
    st_as_sf()
  
  return(cyclingVolCoordinated)
}



# 3. Walking data
# -----------------------------------------------------------------------------
# Function to add links from network to walking data.  Approach:
# - filter Network to walkable links
# - select links in a 50m buffer around each sampling site, and dupicate any one way
# - for each of the sensor's two directions, filter out any that are in wrong direction
# - filter out any where azimuth (bearing) is > 'az.tol' degrees away from road:
  # - 'az.tol' is generally 30 degrees, but needs adustment for some sites
  # - and note directions adjusted for different grid pattern in CBD vs elsewhere
# - for each of the sensor's two directions, select the remaining link closest to the sensor site
# - add the link row number to the cycling data
# - also add the link's from and to node id's

addWalkLinks <- function(walkData,
                         walkSensorLocs,
                         links,
                         nodes) {
                         # network = "./generatedNetworks/MATSimMelbNetwork.sqlite") {
  
  # read in sensor locations, and add columns for link_row and node from_ and to_ id's 
  locationTable <- walkSensorLocs %>%
    # remove where no direction (these locations don't appearin walkData)
    filter(!is.na(direction_1) & !is.na(direction_2)) %>%
    st_as_sf(coords = c("longitude", "latitude"), remove=F, crs = 4326) %>%
    st_transform(28355)
  
  # read in links and nodes
  links <- links %>%
    # filter to walkable links
    filter(str_detect(modes, "walk"))
  
  nodes <- nodes %>%
    # filter to nodes used in links, to remove any disconnected (not really necessary)
    filter(id %in% links$from_id | id %in% links$to_id)
  
  for (i in 1:nrow(locationTable)) {
    
    # set az.adj (azimuth adjustment) to match grid pattern - in CBD (northing <= 5813600), 
    # 'north' is about az 339; elsewhere (northing > 5813600, 'north' is about az 6)
    if (st_coordinates(locationTable[i, "geometry"])[, "Y"] <= 5813600) {
      az.adj <- -21
    } else {
      az.adj <- 6
    }
    
    # set az.tol (azimuth tolerance) to exclude links too far from target azimuth (bearing)
    # generally 30 degrees is effective; some need special override
    sensor_id <- locationTable[i, "sensor_id"] %>%
      st_drop_geometry()
    if (sensor_id == 14) {  # Sandridge Bridge, off-grid alignment
      az.tol <- 65
    } else if (sensor_id == 21) {  # Russell St nr Bourke St, avoid nearby lane 
      az.tol <- 15
    } else if (sensor_id == 40) {  # Spring St nr Lonsdale St, footpath alignment nr Parliament Stn
      az.tol <- 45
    } else if (sensor_id == 72) {  # Flinders St, avoid path through Fed Sq
      az.tol <- 10
    } else {  # default for all other sensors
      az.tol <- 30
    }
    
    # buffer the location to 100m
    local.area <- st_buffer(locationTable[i, ], 50)
    
    # find links in the local area, and duplicate in opposite direction if two way
    filtered.links <- links %>%
      filter(st_intersects(GEOMETRY, local.area, sparse = FALSE)) %>%
      mutate(correct_az_1 = 0, correct_az_2 = 0)
    
    potential.links.oneway <- filtered.links %>%
      filter(is_oneway == 1)
    
    potential.links.twoway <- filtered.links %>%
      filter(is_oneway == 0)
    
    potential.links.twoway.reversed <- potential.links.twoway %>%
      mutate(new.from = to_id, new.to = from_id) %>%
      mutate(from_id = new.from, to_id = new.to)
    
    potential.links <- dplyr::bind_rows(potential.links.oneway,
                                        potential.links.twoway,
                                        potential.links.twoway.reversed) %>%
      dplyr::select(link_id, from_id, to_id, correct_az_1, correct_az_2)
    
    # add azimuth to potential links
    for (j in 1:nrow(potential.links)) {
      link <- potential.links[j, ]
      azimuth <- st_azimuth(nodes[nodes$id == link$from_id, ], 
                            nodes[nodes$id == link$to_id, ])
      potential.links[j, "azimuth"] <- azimuth
      
      # for direction_1 and direction_2, filter to links within azimuth tolerance
      for (k in c("direction_1", "direction_2")) {
        # get direction of direction_1 or direction_2 as applicable
        direction <- locationTable[i, k] %>% 
          st_drop_geometry()
        
        # get azimuth of the potential link
        azimuth <- potential.links[j, "azimuth"] %>%
          st_drop_geometry()
        
        # set correct_az_1 or correct_az_2 to 1 if azimuth is within az.tol of direction
        if (direction == "North") {
          if (abs(az.adj) > az.tol) {  # where abs(az.adj) > az.tol, tolerance zone cannot cross 360
            if (azimuth > 360 + az.adj - az.tol &
                azimuth < 360 + az.adj + az.tol) {
              if (k == "direction_1") {
                potential.links[j, "correct_az_1"] <- 1
              } else if (k == "direction_2") {
                potential.links[j, "correct_az_2"] <- 1
              }
            }
          } else { # otherwise, crosses 360
            if (azimuth > 360 + az.adj - az.tol |
                azimuth < az.adj + az.tol) {
              if (k == "direction_1") {
                potential.links[j, "correct_az_1"] <- 1
              } else if (k == "direction_2") {
                potential.links[j, "correct_az_2"] <- 1
              }
            }
          }
        } else if (direction == "East") {
          if (azimuth > 90 + az.adj - az.tol &
              azimuth < 90 + az.adj + az.tol) {
            if (k == "direction_1") { 
              potential.links[j, "correct_az_1"] <- 1
            } else if (k == "direction_2") {
              potential.links[j, "correct_az_2"] <- 1
            }
          }
        } else if (direction == "South") {
          if (azimuth > 180 + az.adj - az.tol &
              azimuth < 180 + az.adj + az.tol) {
            if (k == "direction_1") {
              potential.links[j, "correct_az_1"] <- 1
            } else if (k == "direction_2") {
              potential.links[j, "correct_az_2"] <- 1
            }
          }
        } else if (direction == "West") {
          if (azimuth > 270 + az.adj - az.tol &
              azimuth < 270 + az.adj + az.tol) {
            if (k == "direction_1") {
              potential.links[j, "correct_az_1"] <- 1
            } else if (k == "direction_2") {
              potential.links[j, "correct_az_2"] <- 1
            }
          }
        }
        
        # filter to links where az is correct
        if (k == "direction_1") {
          potential.links.az <- potential.links %>%
            filter(correct_az_1 == 1)
        } else if (k == "direction_2") {
          potential.links.az <- potential.links %>%
            filter(correct_az_2 == 1)
        }
        
        # find filtered link closest to the sensor (st_nearest_feature returns the index)
        sensor <- locationTable[i, ]
        closest.link <- potential.links.az[st_nearest_feature(sensor, potential.links.az), ]
        
        # complete link_row, from_id and to_id for each direction
        if (k == "direction_1") {
          locationTable[i, "link_row_1"] <- closest.link$link_id
          locationTable[i, "from_id_1"] <- closest.link$from_id
          locationTable[i, "to_id_1"] <- closest.link$to_id
        } else if (k == "direction_2") {
          locationTable[i, "link_row_2"] <- closest.link$link_id
          locationTable[i, "from_id_2"] <- closest.link$from_id
          locationTable[i, "to_id_2"] <- closest.link$to_id
        }
        
        # restore sf class to locationTable (previous rows drop it)
        locationTable <- locationTable %>% st_as_sf(.)
      }
    }
  }
  
  # convert locationTable to data needed for join
  locationTable <- locationTable %>%
    dplyr::select(sensor_id, 
                  link_row_1, from_id_1, to_id_1,
                  link_row_2, from_id_2, to_id_2) %>%
    st_drop_geometry
  
  # read in walkData
  # walkData <- st_read(walkData, layer = layer, quiet=T)
  
  # join link data to walkData
  walkData <- walkData %>%
    left_join(locationTable, by = "sensor_id")
  
  return(walkData)
}



# 4. Station patronage data
# -----------------------------------------------------------------------------
# Function to add nodes from network to station patronage data.  Approach:
# - make graph of PT links and nodes, confined to those >400m
# - select largest sub-graph (which contains the rail network)
# - from the nodes remaining in that sub-graph, find closest node to station
# - add the node id to the patronage file

addStationNodes <- function(patronageData,
                            links,
                            nodes,
                            gMelbBoundary) {
                            # network = "./generatedNetworks/MATSimMelbNetwork.sqlite") {
  
  patronageData = stationDataWithGeom; links=networkLinks; nodes=networkNodes
  
  # read in links and nodes; filter to PT over 400m (smallest station distance Riversdale-Willison 418m:
  # https://maps.philipmallis.com/distances-between-melbourne-railway-stations-a-quick-map/ )
  links <- links %>% 
    filter(modes == "pt" & length > 400) 
  
  nodes <- nodes %>%
    filter(id %in% links$from_id | id %in% links$to_id)
  
  # make graph; find nodes from largest connected sub-graph; filter nodes and links accordingly
  # see https://stackoverflow.com/questions/64344845/getting-the-biggest-connected-component-in-r-igraph
  g <- graph_from_data_frame(st_drop_geometry(links[,c("from_id","to_id")]),
                                 directed = F, vertices = nodes)
  
  components <- clusters(g)
  biggest_cluster_id <- which.max(components$csize)
  vertices <- V(g)[components$membership == biggest_cluster_id]
  vert_ids <- as.numeric(vertices$name)
  
  nodes <- nodes %>%
    filter(id %in% vert_ids) %>%
    st_intersection(gMelbBoundary)
  
  links <- links %>%
    filter(from_id %in% nodes$id & to_id %in% nodes$id)
  
  # read in station patronage data, and add column for node_id
  # patronageData <- st_read(patronageData, layer = layer, quiet=T)
  
  # find nearest nodes (st_nearest_feature returns the index; find the corresponding node id in 'nodes')
  nearest.nodes <- nodes[st_nearest_feature(patronageData, nodes), "id"] %>%
    st_drop_geometry()
  
  # add to station patronage data - but not for Flemington Racecourse and Showgrounds: they are not
  # in the links/nodes network, so their 'nearest nodes' are distant station
  patronageData <- patronageData %>%
    mutate(node_id = ifelse(stationName %in% c("Showgrounds Station", "Flemington Racecourse"), NA,
                            nearest.nodes$id))
  
  return(patronageData)
}

