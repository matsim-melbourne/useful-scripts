convertXMLNetowrk <- function(xmlFile = "./data/simOutput/output_network.xml",
                              netCRS = 28355){
  # xmlFile <- "./data/simOutput/output_network.xml"
  # netCRS = 28355
  
  require(xml2)
  library(sf)
  library(dplyr)
  
  # Reading the network xml
  netXML <- read_xml(xmlFile)
  
  # Processing nodes --------------------------------------------------------
  
  # Extracting Nodes from XML
  netNodes <- netXML %>% 
    xml_find_all("//node")  
  nodeMainAttribs <- names(xml_attrs(netNodes[1])[[1]]) %>% as.list()
  nodes <- map_dfc(nodeMainAttribs, ~{netNodes %>% xml_attr(.x)}) %>% 
    set_names(nodeMainAttribs)
  
  # converting nodes to sf objects
  nodes_sf <- nodes %>% 
    mutate(GEOMETRY=paste0("POINT(",x," ",y,")")) %>% 
    st_as_sf(wkt = "GEOMETRY", crs = netCRS) %>% 
    as.data.frame() %>% 
    st_sf()
  
  st_write(nodes_sf, 'networkConverted.sqlite', 
           layer = 'nodes', driver = 'SQLite', layer_options = 'GEOMETRY=AS_XY',
           delete_layer = T)
  
  # Processing links --------------------------------------------------------
  
  # Extracting Links
  netLinks <- netXML %>% 
    xml_find_all("//link")  
  linkMainAttribs <- names(xml_attrs(netLinks[1])[[1]]) %>% as.list()
  links <- map_dfc(linkMainAttribs, ~{netLinks %>% xml_attr(.x)}) %>% 
    set_names(linkMainAttribs)
  
  # Adding from and to X,Y to links
  linksJoined <- links %>% 
    left_join(nodes, by = c("from"="id")) %>% 
    rename(fromX=x, fromY=y) %>% 
    left_join(nodes, by = c("to"="id")) %>% 
    rename(toX=x, toY=y)
  
  # converting links to sf objects
  links_sf <- linksJoined %>% mutate(GEOMETRY=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
    st_as_sf(wkt = "GEOMETRY", crs = netCRS) %>% 
    as.data.frame() %>%
    st_sf()
  
  st_write(links_sf, 'networkConverted.sqlite', 
           layer = 'links', driver = 'SQLite', layer_options = 'GEOMETRY=AS_XY',
           delete_layer = T)
  
 return(list(nodes_sf,links_sf)) 
}