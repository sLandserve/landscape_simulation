ls_create <- function(x) {
  # get the parameters from the input

  
  # create a gradient surface
  # NB we are using a planar gradient, but we can look into the other types - edge and distance)
  g <- nlm_planargradient(x["ncol"], x["nrow"])
  
  # create supply and demand surfaces 
  # here we control the fragmentation and the amount
  supply <- nlm_fbm(x["ncol"], x["nrow"], fract_dim = as.numeric(x["f_supply"]))
  demand <- nlm_fbm(x["ncol"], x["nrow"], fract_dim = as.numeric(x["f_demand"]))
  
  # create the analysis landscape: this takes 3 steps: 
    # 1. merge supply and demand
    # 2. merge gradient
    # 3. classify
  ls <- util_merge(supply, demand) %>% 
    util_merge(g, scalingfactor = x["inter"]) %>% 
    util_classify(weighting = c(x["p_supply"], 1 - (x["p_supply"] + x["p_demand"]), x["p_demand"]), 
                  level_names = c("supply", "neutral", "demand"))

  # get a dataframe out for ease of plotting
  ls_df <- as.data.frame(ls, xy = TRUE) %>% cbind(x %>% as.data.frame %>% t)
  
  return(list(params = x, ls = ls, ls_df = ls_df))
}


