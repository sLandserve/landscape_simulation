#'Create social-ecological networks from raster landscapes
#'
#'@description `network_create` takes as input a raster (either real data or result of
#'  `ls_create` and derives the underlying social ecological network
#'
#'@param ls raster containing cells classified as supply, demand and neutral
#'
#'@param es_thresh distance threshold for the ecological-social links
#'
#'@param ee_thresh distance threshold for the ecological-ecological links (default value of `NULL` means no ecological-ecological links)
#'
#'@param ss_thresh distance threshold for the social-social links (default value of `NULL` means no social-social links)
#'
#'@return A list containing (1) the polygon layer of the landscape, (2) a matrix of
#'  Euclidean distances, (3) a network representation, and (4) the networks in fanmod
#'  format
#'
#'@keywords ecosystem services, spatial, social ecological system, neutral landscape model
#'
#'@export
network_create <- function(x, es_thresh, ee_thresh = NULL, ss_thresh = NULL) {
  # create attribute table
  all_nodes <- rasterToPolygons(x$ls, dissolve=TRUE) %>%
    disaggregate %>%
    st_as_sf %>%
    mutate(ID = as.character(1:n()),
           patch_type = case_when(layer == 0 ~ "supply",
                                  layer == 1 ~ "neutral",
                                  TRUE ~ "demand"),
           patch_area = st_area(.)) %>%
    select(-layer) %>%
    filter(patch_type != "neutral") %>%
    mutate(patch_code = case_when(patch_type == "supply" ~ 0,
                                  patch_type == "demand" ~ 1,
                                  TRUE ~ NaN))

  # create the social-ecological network based on ee_thresh, es_thresh, and ss_thresh (ss_thresh currently not implemented)
  # this generates a list of the node attributes (node colour) and the link presence/absence
  # between each node
  supply_nodes <- which(all_nodes$patch_type == "supply")
  demand_nodes <- which(all_nodes$patch_type == "demand")

  net_links <- st_distance(all_nodes)

  net_links[supply_nodes, demand_nodes] <- ifelse(net_links[supply_nodes, demand_nodes] <= es_thresh, 1, 0)
  net_links[demand_nodes, supply_nodes] <- ifelse(net_links[demand_nodes, supply_nodes] <= es_thresh, 1, 0)

  if(!is.null(ee_thresh)) {
    net_links[supply_nodes, supply_nodes] <- ifelse(net_links[supply_nodes, supply_nodes] <= ee_thresh, 1, 0)
  } else {
    net_links[supply_nodes, supply_nodes] <- 0
  }

  if(!is.null(ss_thresh)) {
    net_links[demand_nodes, demand_nodes] <- ifelse(net_links[demand_nodes, demand_nodes] <= ee_thresh, 1, 0)
  } else {
    net_links[demand_nodes, demand_nodes] <- 0
  }

  network <- list(node_code = all_nodes$patch_code, node_type = all_nodes$patch_type, node_areas = all_nodes$patch_area, net_links = net_links)

  # create fanmod representation of discrete network - assumes bidirectionality
  # NB this is output to file and not returned by the model
  bin_dist <- net_links
  diag(bin_dist) <- 0
  bin_dist[lower.tri(bin_dist)] <- 0
  fanmod <- as_tibble(bin_dist) %>%
    rownames_to_column(var = "int1") %>%
    gather(key = "int2", value = "value", -int1) %>%
    mutate(int2 = str_remove(int2, "V")) %>%
    filter(value == 1) %>%
    select(-value) %>%
    # get patch type (colour) for the first node
    inner_join(as_tibble(cbind(as.character(1:length(network$node_code)), as.character(network$node_code))) %>% select(int1 = V1, int3 = V2)) %>%
    # get patch type (colour) for the second node
    inner_join(as_tibble(cbind(as.character(1:length(network$node_code)), as.character(network$node_code))) %>% select(int2 = V1, int4 = V2)) %>%
    select(int1, int2, int3, int4)

  fname = paste0("results/for_fanmod/fs_",
                 x$params['f_supply'], "_ps_", x$params['p_supply'],
                 "_fd_", x$params['f_demand'], "_pd_", x$params['p_demand'],
                 "_rep_", x$params['rep'], ".txt")

  #NOTE TO LAURA - NOT SURE WHY WE HAVE THE SYSTEM TIME ADDED TO THE FILE NAME BELOW - REPLACED WITH REP NUMBER ABOVE
  #FOR NOW. I ASSUME IT WAS FOR DEBUGGING.
  #dtime <- Sys.time()
  #dtime <- str_replace_all(dtime, "[[:punct:]]", "")
  #fname = paste0("results/for_fanmod/fs_",
  #               x$params['f_supply'], "_ps_", x$params['p_supply'],
  #               "_fd_", x$params['f_demand'], "_pd_", x$params['p_demand'],
  #               "_", dtime, ".txt")

  write_delim(fanmod, fname, col_names = FALSE)

  return(list(network = network, params = x$params))
}
