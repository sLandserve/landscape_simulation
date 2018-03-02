ls_create <- function(x) {
  supply <- nlm_mpd(ncol = x['ncol'], nrow = x['nrow'], roughness = x['f_supply'], verbose = FALSE) %>% 
    util_classify(weighting = c(1 - x['p_supply'], x['p_supply']))
  
  demand <- nlm_mpd(ncol = x['ncol'], nrow = x['nrow'], roughness = x['f_demand'], verbose = FALSE) %>%
    util_classify(weighting = c(1 - x['p_demand'], x['p_demand']))*2
  
  ls <- supply + demand
  
  # at the moment, any 'supply/demand' cells are being converted to demand cells - will make this more    
  # realistic if we use this method (and not the ABM version)
  ls[ls == 3] <- 2
  ls <- as.factor(ls)
  c_r_levels <- levels(ls)[[1]]
  c_r_levels[["Categories"]] <- c("neutral", "supply", "demand")
  levels(ls) <- c_r_levels
  
  # get a dataframe out for ease of plotting
  ls_df <- as.data.frame(ls, xy = TRUE) %>% cbind(x %>% as.data.frame %>% t)
  
  return(list(params = x, ls = ls, ls_df = ls_df))
}