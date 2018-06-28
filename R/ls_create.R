#'Create landscapes of ecosystem service supply (ecological) and demand (social) patches
#'
#'@description `ls_create` simulates ecosystem service supply and demand landscapes based
#'  on a number of parameters: proportion of supply/demand; fragmentation of
#'  supply/demand; interspersion of supply/demand
#'
#'@param nrow Number of rows in the raster
#'
#'@param ncol Number of columns in the raster
#'
#'@param p_supply Proportion of the landscape which is ES supply
#'
#'@param p_demand Proportion of the landscape which is ES demand
#'
#'@param f_supply Fragmentation level of the ES supply (in the range [0, 1] with 1 being
#'  the most fragmented)
#'
#'@param f_demand Fragmentation level of the ES supply (in the range [0, 1] with 1 being
#'  the most fragmented)
#'
#'@param inter Interpersion between ES supply and demand (in the range [0, 1] with 1 being
#'  completely interspersed)
#'
#'@param rep This is an integer which is just to keep track of which replicate results are from
#'
#'@return A list containing a raster of the supply, demand, and if appropriate (p_supply +
#'  p_demand != 1) neutral landcover types and the parameters used to generate it
#'
#'@keywords ecosystem services, spatial, social ecological system, neutral landscape model
#'
#'@export

ls_create <- function(nrow,
                      ncol,
                      p_supply,
                      p_demand,
                      f_supply,
                      f_demand,
                      inter,
                      rep = NULL) {

  params <- data.frame(nrow = nrow, ncol = ncol, p_supply = p_supply, p_demand = p_demand, f_supply = f_supply, f_demand = f_demand, inter = inter, rep = rep)
  # at the moment we scale the f_supply/f_demand =(0, 1] to be [1.5, 0.0001] and f_supply/f_demand == 0 to be 2.
  # a) it means we get a fuller range of fract_dim from fbm while avoiding the fact this function is unstable between 1.5 & 2
  # b) it makes more sense because increasing fragmentation matches with increasing f_supply/f_demand
  f_supply <- ifelse(f_supply == 0, 2, 1.5 - 1.4999*f_supply)
  f_demand <- ifelse(f_demand == 0, 2, 1.5 - 1.4999*f_demand)

  # create a gradient surface
  g <- nlm_planargradient(ncol,
                          nrow)

  # create supply and demand surfaces
  # here we control the fragmentation and the amount
  supply <- nlm_fbm(ncol,
                    nrow,
                    fract_dim = f_supply)
  demand <- nlm_fbm(ncol,
                    nrow,
                    fract_dim = f_demand)

  # create the analysis landscape: this takes 3 steps:
  # 1. merge supply and demand
  ls <- util_merge(supply, demand)
  # 2. merge gradient
  ls <- util_merge(ls,
                   g,
                   scalingfactor = 1 - inter)
  # 3. classify
  ls <- util_classify(ls,
                      weighting = c(p_supply, 1 - (p_supply + p_demand), p_demand),
                      level_names = c("supply", "neutral", "demand"))

  return(list(ls = ls, params = params))
}
