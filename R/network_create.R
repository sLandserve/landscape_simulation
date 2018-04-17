network_create <- function(x, ee_link, ee_thresh, es_thresh) {
  # creates: (1) the polygon layer of the landscape, (2) a matrix of Euclidean distances, (3) a network
  # representation, and (4) the networks in fanmod format
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

  # create the social-ecological network based on the ee_link, ee_thresh, and es_thresh
  # this generates a list of the node attributes (node colour) and the link presence/absence
  # between each node

  attribs_df <- as.data.frame(attribs)
  supply_nodes <- which(attribs_df[, "patch_code"] == 1)
  demand_nodes <- which(attribs_df[, "patch_code"] == 2)
  supply_demand_nodes <- which(attribs_df[, "patch_code"] == 3)
  # split supply/demand nodes into separate supply and demand nodes for the network
  if (length(supply_demand_nodes) > 0) {
    add_supply_nodes <- attribs_df[supply_demand_nodes,"patch_code"]
    add_supply_nodes[1:length(add_supply_nodes)] <- 1
    add_demand_nodes <- attribs_df[supply_demand_nodes,"patch_code"]
    add_demand_nodes[1:length(add_demand_nodes)] <- 2
    node_types <- c(attribs_df[supply_nodes, "patch_code"], add_supply_nodes, attribs_df[demand_nodes, "patch_code"], add_demand_nodes)
  } else {
    node_types <- c(attribs_df[supply_nodes, "patch_code"], attribs_df[demand_nodes, "patch_code"])
  }

  #get node areas
  if (length(supply_demand_nodes) > 0) {
    node_areas <- c(attribs_df[supply_nodes, "patch_area"], attribs_df[supply_demand_nodes, "patch_area"], attribs_df[demand_nodes, "patch_area"], attribs_df[supply_demand_nodes, "patch_area"])
  } else{
    node_areas <- c(attribs_df[supply_nodes, "patch_area"], attribs_df[demand_nodes, "patch_area"])

  }
  #get the discrete links for the network
  net_links <- matrix(0, nrow=length(node_types), ncol=length(node_types))
  #get the ecological-ecological links
  if (ee_link == TRUE) {
    if (length(supply_demand_nodes) > 0) {
      net_links[which(node_types == 1), which(node_types == 1)] <- ifelse(distances[c(supply_nodes,supply_demand_nodes), c(supply_nodes,supply_demand_nodes)] <= ee_thresh,1,0)
    } else {
      net_links[which(node_types == 1), which(node_types == 1)] <- ifelse(distances[supply_nodes, supply_nodes] <= ee_thresh,1,0)
    }
  }
  #get the social-ecological links
  if (length(supply_demand_nodes) > 0) {
    net_links[which(node_types == 1), which(node_types == 2)] <- ifelse(distances[c(supply_nodes,supply_demand_nodes), c(demand_nodes,supply_demand_nodes)] <= es_thresh,1,0)
    net_links[which(node_types == 2), which(node_types == 1)] <- ifelse(distances[c(demand_nodes,supply_demand_nodes), c(supply_nodes,supply_demand_nodes)] <= es_thresh,1,0)
  } else {
    net_links[which(node_types == 1), which(node_types == 2)] <- ifelse(distances[supply_nodes, demand_nodes] <= es_thresh,1,0)
    net_links[which(node_types == 2), which(node_types == 1)] <- ifelse(distances[demand_nodes, supply_nodes] <= es_thresh,1,0)
  }

  network <- list(node_type = node_types, node_areas = node_areas, net_links = net_links)

  # create fanmod representation of discrete network - assumes bidirectionality

  bin_dist <- net_links
  diag(bin_dist) <- 0
  bin_dist[lower.tri(bin_dist)] <- 0
  fanmod <- as_tibble(bin_dist) %>%
    rownames_to_column(var = "int1") %>%
    gather(key = "int2", value = "value", -int1) %>%
    mutate(int2 = str_remove(int2, "V")) %>%
    filter(value == 1) %>%
    select(-value) %>%
    # get patch type (colour) for the first node - note here that we convert node types to supply = 0 and demand = 1 to suit fanmod format
    inner_join(as_tibble(cbind(as.character(1:length(node_types)), as.character(node_types - 1))) %>% select(int1 = V1, int3 = V2)) %>%
    # get patch type (colour) for the second node - note here that we convert node types to supply = 0 and demand = 1 to suit fanmod format
    inner_join(as_tibble(cbind(as.character(1:length(node_types)), as.character(node_types - 1))) %>% select(int2 = V1, int4 = V2)) %>%
    select(int1, int2, int3, int4)

  fname = paste0("results/for_fanmod/fs_",
                x$params['f_supply'], "_ps_", x$params['p_supply'],
                "_fd_", x$params['f_demand'], "_pd_", x$params['p_demand'],
                "_rep_", x$params['reps'], ".txt")
  write_delim(fanmod, fname, col_names = FALSE)

  #Laura's code for fanmod input files - not deployed at present - but adapted to networks above

  #bin_dist <- apply(distances, 1:2, function(x) ifelse(x < median(distances), 1, 0))
  #diag(bin_dist) <- 0
  #bin_dist[lower.tri(bin_dist)] <- 0

  #fanmod <- as_tibble(bin_dist) %>%
  #  rownames_to_column(var = "int1") %>%
  #  gather(key = "int2", value = "value", -int1) %>%
  #  mutate(int2 = str_remove(int2, "V")) %>%
  #  filter(value == 1) %>%
  #  select(-value) %>%
  #  # get patch type (color) for the first node
  #  inner_join(attribs %>% select(int1 = ID, int3 = patch_code) %>% st_set_geometry(NULL)) %>%
  #  # get patch type (color) for the second node
  #  inner_join(attribs %>% select(int2 = ID, int4 = patch_code) %>% st_set_geometry(NULL)) %>%
  #  select(int1, int2, int3, int4)
  #  fname = paste0("results/for_fanmod/fs_",
  #              x$params['f_supply'], "_ps_", x$params['p_supply'],
  #              "_fd_", x$params['f_demand'], "_pd_", x$params['p_demand'],
  #              "_rep_", x$params['reps'], ".txt")
  #write_delim(fanmod, fname, col_names = FALSE)

  #colate parameter values
  params_temp <- as.vector(c(ee_link, ee_thresh, es_thresh))
  names(params_temp) <- c("ee_link", "ee_thresh", "es_thresh")
  params <- c(x$params, params_temp)

  # return the three items as a list
  return(list(params = params, attribs = attribs, distances = distances, network = network, fanmod = fanmod))

}
