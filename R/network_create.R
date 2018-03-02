network_create <- function(x) {
  # create attribute table
  attribs <- rasterToPolygons(x$ls, dissolve=TRUE) %>% disaggregate %>% st_as_sf %>% 
    mutate(ID = 1:n(),
           patch_type = factor(layer, labels = c("neutral", "supply", "demand")),
           patch_area = st_area(.)) %>% 
    select(-layer) %>% cbind(x$params %>% as.data.frame %>% t)
  
  # create the Euclidean distance matrix
  distances <- st_distance(attribs)
  
  # return the two items as a list
  return(list(attribs = attribs, distances = distances))
}