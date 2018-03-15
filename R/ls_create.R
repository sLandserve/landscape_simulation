ls_create <- function(x) {
  supply <- nlm_mpd(ncol = x['ncol'], nrow = x['nrow'], roughness = x['f_supply'], verbose = FALSE) %>% 
    util_classify(weighting = c(1 - x['p_supply'], x['p_supply']))
  
  demand <- nlm_mpd(ncol = x['ncol'], nrow = x['nrow'], roughness = x['f_demand'], verbose = FALSE) %>%
    util_classify(weighting = c(1 - x['p_demand'], x['p_demand']))*2
  
  ls <- supply + demand
  
  ls <- as.factor(ls)
  c_r_levels <- levels(ls)[[1]]
  
  # this allows for situations where there is no overlap of supply/demand
  if (nrow(c_r_levels) == 4) {
    c_r_levels[["Categories"]] <- c("neutral", "supply", "demand", "supply/demand")
  } else {
    c_r_levels[["Categories"]] <- c("neutral", "supply", "demand")
  }
  
  levels(ls) <- c_r_levels
  
  # get a dataframe out for ease of plotting
  ls_df <- as.data.frame(ls, xy = TRUE) %>% cbind(x %>% as.data.frame %>% t)
  
  return(list(params = x, ls = ls, ls_df = ls_df))
}