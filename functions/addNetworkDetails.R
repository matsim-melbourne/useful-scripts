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
                        layer,
                        network = "./generatedNetworks/MATSimMelbNetwork.sqlite",
                        # azimuth tolerance (to filter out links > 17.5 degrees from road azimuth)
                        az.tol = 17.5) {
  
  # read in car data, and add columns for link_row and node from_ and to_ id's 
  carData <- st_read(carData, layer = layer, quiet=T) %>%
    mutate(link_row = NA, from_id = NA, to_id = NA)
  
  # read in links and nodes
  links <- st_read(network, layer = "links", quiet=T) %>%
    mutate(link_id = row_number()) %>%
    # filter to main roads only ( don't include trunk_link, primary_link, secondary_link)
    filter(highway %in% c("motorway", "motorway_link", "trunk", "primary", "secondary"))

  nodes <- st_read(network, layer = "nodes", quiet=T) %>%
    # filter to nodes used in links, to remove any disconnected (not really necessary)
    filter(id %in% links$from_id | id %in% links$to_id)
  
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
      dplyr::select(link_id, from_id, to_id)
    
    # filter to links in correct direction and with correct azimuth
    for (j in 1:nrow(potential.links)) {
      link <- potential.links[j, ]
      
      # set correct_dir to 1 if correct direction
      startpoint <- nodes[nodes$id == link$from_id, ]
      endpoint <- nodes[nodes$id == link$to_id, ]
      if (direction == "EAST" & startpoint$x < endpoint$x | # startpoint has lower easting coordinate
          direction == "WEST" & startpoint$x > endpoint$x | # startpoint has higher easting coordinate
          direction == "NORTH" & startpoint$y < endpoint$y | # startpoint has lower northing coordinate
          direction == "SOUTH" & startpoint$y > endpoint$y) # startpoint has higher northing coordinate
      {
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
    
    # complete link_row with row number of closest.link
    carData[i, "link_row"] <- closest.link$link_id
    
    # complete from_id and to_id with node numbers for closest link
    carData[i, "from_id"] <- closest.link$from_id
    carData[i, "to_id"] <- closest.link$to_id
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
                         network = "./generatedNetworks/MATSimMelbNetwork.sqlite") {
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
  links <- st_read(network, layer = "links", quiet=T) %>%
    mutate(link_id = row_number())
  
  lane.links <- links %>%
    filter(cycleway %in% c("lane", "seperated_lane", "shared_lane"))
  
  path.links <- links %>%
    filter(cycleway == "bikepath")
  
  nodes <- st_read(network, layer = "nodes", quiet=T) %>%
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
# TBC





# 4. Station patronage data
# -----------------------------------------------------------------------------
# Function to add nodes from network to station patronage data.  Approach:
# - make graph of PT links and nodes, confined to those >400m
# - select largest sub-graph (which contains the rail network)
# - from the nodes remaining in that sub-graph, find closest node to station
# - add the node id to the patronage file

addStationNodes <- function(patronageData,
                            layer,
                            network = "./generatedNetworks/MATSimMelbNetwork.sqlite") {
  
  # read in links and nodes; filter to PT over 400m (smallest station distance Riversdale-Willison 418m:
  # https://maps.philipmallis.com/distances-between-melbourne-railway-stations-a-quick-map/ )
  links <- st_read(network, layer = "links", quiet=T) %>%
    mutate(link_id = row_number()) %>%
    filter(highway == "pt" & length > 400)
  
  nodes <- st_read(network, layer = "nodes", quiet=T) %>%
    filter(id %in% links$from_id | id %in% links$to_id)
  
  # make graph; find nodes from largest connected sub-graph; filter nodes and links accordingly
  # see https://stackoverflow.com/questions/64344845/getting-the-biggest-connected-component-in-r-igraph
  graph <- graph_from_data_frame(links, directed = F, vertices = nodes)
  
  components <- clusters(graph)
  biggest_cluster_id <- which.max(components$csize)
  vertices <- V(graph)[components$membership == biggest_cluster_id]
  vert_ids <- as.numeric(vertices$name)
  
  nodes <- nodes %>%
    filter(id %in% vert_ids) %>%
    st_intersection(gMelbBoundary)
  
  links <- links %>%
    filter(from_id %in% nodes$id & to_id %in% nodes$id)
  
  # read in station patronage data, and add column for node_id
  patronageData <- st_read(patronageData, layer = layer, quiet=T)
  
  # find nearest nodes (st_nearest_feature returns the index; find the corresponding node id in 'nodes')
  nearest.nodes <- nodes[st_nearest_feature(patronageData, nodes), "id"] %>%
    st_drop_geometry()
  
  # add to station patronage data - but not for Flemington Racecourse and Showgrounds: they are not
  # in the links/nodes network, so their 'nearest nodes' are distant station
  patronageData <- patronageData %>%
    mutate(node_id = ifelse(stationname %in% c("Showgrounds Station", "Flemington Racecourse"), NA,
                            nearest.nodes$id))
  
  return(patronageData)
}

