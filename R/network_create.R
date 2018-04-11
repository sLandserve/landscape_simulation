network_create <- function(x) {
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
}
