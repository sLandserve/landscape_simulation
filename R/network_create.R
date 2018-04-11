network_create <- function(x) {
  # create attribute table
  attribs <- rasterToPolygons(x$ls, dissolve=TRUE) %>% disaggregate %>% st_as_sf %>% 
    mutate(ID = as.character(1:n()),
           patch_code = layer,
           patch_type = factor(layer, labels = c("neutral", "supply", "demand", "supply/demand")),
           patch_area = st_area(.)) %>% 
    select(-layer) %>% cbind(x$params %>% as.data.frame %>% t)
  
  # create the Euclidean distance matrix
  distances <- st_distance(attribs)
  
  fanmod <- as_tibble(distances) %>% 
    rownames_to_column(var = "int1") %>% 
    gather(key = "int2", value = "value", -int1) %>% 
    mutate(int2 = str_remove(int2, "V")) %>% 
    filter(value == 1) %>% 
    select(-value) %>% 
    # get patch type (color) for the first node
    inner_join(nw1$attribs %>% select(int1 = ID, int3 = patch_code)) %>% 
    # get patch type (color) for the second node
    inner_join(nw1$attribs %>% select(int2 = ID, int4 = patch_code)) %>% 
    select(int1, int2, int3, int4)
  
  fname = paste0("results/for_fanmod/fs_", 
                x$params['f_supply'], "_ps_", x$params['p_supply'], 
                "_fd_", x$params['f_demand'], "_pd_", x$params['p_demand'],
                "_rep_", x$params['reps'], ".txt")
  write_delim(fanmod, fname)
  # return the three items as a list
  return(list(attribs = attribs, distances = distances, fanmod = fanmod))
}

