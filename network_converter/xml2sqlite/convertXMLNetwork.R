convertXMLNetowrk <- function(xmlFile = "./data/simOutputs/output_network.xml.gz",
                              netCRS = 28355){
  # xmlFile <- "./data/simOutputs/output_network.xml.gz"
  # netCRS = 28355
  
  require(xml2)
  library(sf)
  library(dplyr)
  library(purrr)
  
  # Reading the network xml
  netXML <- read_xml(gzfile(xmlFile))
  
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
  
  st_write(nodes_sf, './data/networks/networkConverted.sqlite', 
           layer = 'nodes', driver = 'SQLite', layer_options = 'GEOMETRY=AS_XY',
           delete_layer = T)
  
  # Processing links --------------------------------------------------------
  
  # Extracting Links
  netLinks <- netXML %>% 
    xml_find_all("//link")  
  linkMainAttribs <- names(xml_attrs(netLinks[1])[[1]]) %>% as.list()
  links <- map_dfc(linkMainAttribs, ~{netLinks %>% xml_attr(.x)}) %>% 
    set_names(linkMainAttribs)
  
  # Porcessing any additional links attributes
  nExtraAttribs <- xml_length(xml_child(netLinks[1]))
  # i=1
  for(i in 1:nExtraAttribs){
    attribName <- xml_attrs(xml_child(xml_child(netLinks[1]), 
                                      search = i))["name"] %>% 
      as.character()
    
    extraAttribContent <- netXML %>%
      xml_find_all(xpath = paste0("///attribute[@name='", attribName, "']")) %>%
      xml_text() %>% as_tibble() 
    colnames(extraAttribContent) <- attribName
    if (nrow(links)==nrow(extraAttribContent)) {
      links <- cbind(links, extraAttribContent)
      print(paste0("Joining the attribute: ", attribName))
    }else{
      print(paste0("Attribute: ", attribName, " was not found for all links, so ignoring!"))
    }
  }
  
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
  
  st_write(links_sf, './data/networks/networkConverted.sqlite', 
           layer = 'links', driver = 'SQLite', layer_options = 'GEOMETRY=AS_XY',
           delete_layer = T)
  
 return(list(nodes_sf,links_sf)) 
}