network_create <- function(x,ee_link,ee_thresh,es_thresh) {
  # for the social-ecological links
  # ee_thresh = distance threshold for the ecological-ecological links, and es_thresh = distance threshold
  # Here ee_link is a TRUE/FALSE parameter indicating whether the ecological-ecological links are present or not,
  # for the ecological-ecological and social-ecological links.
  # (i.e., whether ecological links are present) and a distance threshold for the presence of links
  # based on whether ecological nodes are connectivity dependent
  # it also creates a social-ecological network for supply and demand nodes
  # and also a matrix of the distances between patches
  # this generates a polygon representation of the landscape that includes the patch type

  # create attribute table
  attribs <- rasterToPolygons(x$ls, dissolve=TRUE) %>% disaggregate %>% st_as_sf %>% 
    mutate(ID = as.character(1:n()),
           patch_code = layer,
           patch_area = st_area(.)) %>% 
    select(-layer) %>% cbind(x$params %>% as.data.frame %>% t)
  # create the Euclidean distance matrix
  distances <- st_distance(attribs)
  # at the moment this sets binary links such that two nodes are linked if the
  # distance is less than the median and assumes bidirectionality
  bin_dist <- apply(distances, 1:2, function(x) ifelse(x < median(distances), 1, 0))
  
  diag(bin_dist) <- 0
  bin_dist[lower.tri(bin_dist)] <- 0
  
  fanmod <- as_tibble(bin_dist) %>% 
    rownames_to_column(var = "int1") %>% 
    gather(key = "int2", value = "value", -int1) %>% 
    mutate(int2 = str_remove(int2, "V")) %>% 
    filter(value == 1) %>% 
    select(-value) %>% 
    # get patch type (color) for the first node
    inner_join(attribs %>% select(int1 = ID, int3 = patch_code) %>% st_set_geometry(NULL)) %>% 
    # get patch type (color) for the second node
    inner_join(attribs %>% select(int2 = ID, int4 = patch_code) %>% st_set_geometry(NULL)) %>% 
    select(int1, int2, int3, int4)
  fname = paste0("results/for_fanmod/fs_", 
                x$params['f_supply'], "_ps_", x$params['p_supply'], 
                "_fd_", x$params['f_demand'], "_pd_", x$params['p_demand'],
                "_rep_", x$params['reps'], ".txt")
  write_delim(fanmod, fname, colnames = FALSE)
  # return the three items as a list
  return(list(attribs = attribs, distances = distances, fanmod = fanmod))

  # create the social-ecological network based on the ee_link, ee_thresh, and es_thresh
  # this generates a list of the node attributes (node colour) and the link presence/absence
  # between each node
  #get node types

  attribs_df <- as.data.frame(attribs,row.names=NULL)
  supply_nodes <- which(attribs_df[, "patch_type"] == "supply")
  print(supply_nodes)
  demand_nodes <- which(attribs_df[, "patch_type"] == "demand")
  supply_demand_nodes <- which(attribs_df[, "patch_type"] == "supply/demand")
  # split supply/demand nodes into separate supply and demand nodes for the network
  add_supply_nodes <- attribs_df[supply_demand_nodes,"patch_type"]
  add_supply_nodes[1:length(add_supply_nodes)] <- c_r_levels[2,2]
  add_demand_nodes <- attribs_df[supply_demand_nodes,"patch_type"]
  add_demand_nodes[1:length(add_demand_nodes)] <- c_r_levels[3,2]
  node_types <- concat.factor(attribs_df[supply_nodes, "patch_type"], add_supply_nodes, attribs_df[demand_nodes, "patch_type"], add_demand_nodes)
  #get node areas
  node_areas <- c(attribs_df[supply_nodes, "patch_area"], attribs_df[supply_demand_nodes, "patch_area"], attribs_df[demand_nodes, "patch_area"], attribs_df[supply_demand_nodes, "patch_area"])

  #get the discrete links for the network
  net_links <- matrix(0, nrow=length(node_types), ncol=length(node_types))
  #get the ecological-ecological links
  if (ee_link == TRUE) {
    net_links[which(node_types == "supply"), which(node_types == "supply")] <- ifelse(distances[c(supply_nodes,supply_demand_nodes), c(supply_nodes,supply_demand_nodes)] <= ee_thresh,1,0)
  }
  #get the social-ecological links
  net_links[which(node_types == "supply"), which(node_types == "demand")] <- ifelse(distances[c(supply_nodes,supply_demand_nodes), c(demand_nodes,supply_demand_nodes)] <= es_thresh,1,0)
  net_links[which(node_types == "demand"), which(node_types == "supply")] <- ifelse(distances[c(demand_nodes,supply_demand_nodes), c(supply_nodes,supply_demand_nodes)] <= es_thresh,1,0)

  network <- list(node_type = node_types, node_areas = node_areas, net_links = net_links)

  # return the three items as a list
  return(list(attribs = attribs, distances = distances, network = network))
  
}
