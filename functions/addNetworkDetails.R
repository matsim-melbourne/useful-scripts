# Function to add links from network to car data.  Approach:
# - select links in a 300m buffer around each road of interest
# - filter out any that are one way in wrong direction
# - filter out any where azimuth (bearing) is >17.5 degrees away from road
# - select the remaining link closest to the road midpoint
# - add the link row number to the car data
# - also add the link's from and to node id's (reversing, where necessary, for two way links)

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
    # filter to main roads only (for now, don't include trunk_link, primary_link, secondary_link)
    filter(highway %in% c("motorway", "motorway_link", "trunk", "primary", "secondary"))
  
  nodes <- st_read(network, layer = "nodes", quiet=T) %>%
    # filter to nodes used in links, to remove any disconnected
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
    
    # find links in the local area where (1) either two way or correct one way, and (2) within az.tol
    filtered.links <- links %>%
      filter(st_intersects(GEOMETRY, local.area, sparse = FALSE)) %>% 
      mutate(correct_dir = 0, correct_az = 0)
    
    for (j in 1:nrow(filtered.links)) {
      link <- filtered.links[j, ]
      
      # set correct_dir to 1 if two way, or correct one way
      startpoint <- nodes[nodes$id == link$from_id, ]
      endpoint <- nodes[nodes$id == link$to_id, ]
      if (link$is_oneway == 0) {
        filtered.links[j, "correct_dir"] <- 1
      } else {
        if (direction == "EAST" & startpoint$x < endpoint$x | # startpoint has lower easting coordinate
            direction == "WEST" & startpoint$x > endpoint$x | # startpoint has higher easting coordinate
            direction == "NORTH" & startpoint$y < endpoint$y | # startpoint has lower northing coordinate
            direction == "SOUTH" & startpoint$y > endpoint$y) # startpoint has higher northing coordinate
        {
          filtered.links[j, "correct_dir"] <- 1
        }
      }
      
      # find azimuth for link
      link.az1 <- st_azimuth(startpoint, endpoint)
      filtered.links[j, "azimuth"] <- link.az1  # not used - just for debugging
      
      # for two-way links, calculate a second opposite azimuth (+/- 180 degrees)
      if (link$is_oneway == 0) {
        if (link.az1 <= 180) { 
          link.az2 <- link.az1+180
        } else {
          link.az2 <- link.az1-180
        }
      } else {
        link.az2 <- link.az1
      }
      
      # if either link.az is within tolerance, set "correct_az" to 1
      # (for one-way links, checking both link.az's just performs the same check twice)
      for (link.az in c(link.az1, link.az2)) {
        if (azimuth-az.tol >= 0 & azimuth+az.tol <= 360) {
          if (link.az > azimuth-az.tol & link.az < azimuth+az.tol) {
            filtered.links[j, "correct_az"] <- 1
          }
        } else if (azimuth-az.tol < 0) {
          if (link.az > azimuth-az.tol+360 | link.az < azimuth+az.tol) {
            filtered.links[j, "correct_az"] <- 1
          }
        } else if (azimuth+az.tol > 360) {
          if (link.az > azimuth-az.tol | link.az < azimuth+az.tol-360) {
            filtered.links[j, "correct_az"] <- 1
          }
        }
      }
    }
    
    filtered.links <- filtered.links %>%
      filter(correct_dir == 1 & correct_az == 1)

    # get midpoint of road (contained in the data)
    midpoint <- carData[i, ] %>%
      st_drop_geometry() %>%
      st_as_sf(coords = c("midpnt_lon", "midpnt_lat"), remove = F, crs = 4326) %>%
      st_transform(28355)
    
    # find filtered link closest to the midpoint (st_nearest_feature returns the index)
    closest.link <- filtered.links[st_nearest_feature(midpoint, filtered.links), ]
    
    # complete link_row with row number of closest.link
    carData[i, "link_row"] <- closest.link$link_id
    
    # complete from_id and to_id with node numbers for closest link
    if (closest.link$is_oneway == 1) {
      # for one way links, from_and to_ id's are same as in closest link
      carData[i, "from_id"] <- closest.link$from_id
      carData[i, "to_id"] <- closest.link$to_id
    } else {
      # for two way links, order from_ and to_ by matching direction
      closest.start <- nodes[nodes$id == closest.link$from_id, ]
      closest.end <- nodes[nodes$id == closest.link$to_id, ]
      # where points are ordered in correct order - 
      if (direction == "EAST" & closest.start$x < closest.end$x | # startpoint has lower easting coordinate
          direction == "WEST" & closest.start$x > closest.end$x | # startpoint has higher easting coordinate
          direction == "NORTH" & closest.start$y < closest.end$y | # startpoint has lower northing coordinate
          direction == "SOUTH" & closest.start$y > closest.end$y) # startpoint has higher northing coordinate
      {
        carData[i, "from_id"] <- closest.link$from_id
        carData[i, "to_id"] <- closest.link$to_id
      } else {
        # where points are not ordered in correct order, reverse - 
        carData[i, "from_id"] <- closest.link$to_id
        carData[i, "to_id"] <- closest.link$from_id
      }
    }
  }
  return(carData)
}


# Function to add nodes from network to station patronage data.  Approach:
# - make graph of PT links and nodes, confined to those >400m
# - select largest sub-graph (which contains the rail network)
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

