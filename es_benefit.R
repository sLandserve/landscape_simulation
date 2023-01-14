# simulate using purrr
library(sLandserv)
library(furrr)
library(tidyverse)
library(sf)

# 1. set the parameters ----
nrow <- ncol <- 65 # NB all landscapes will need to be of dimension 2^n + 1 due to the way the mid-point displacement method works
p_supply <- p_demand <- c(0.1, 0.2, 0.3, 0.4, 0.5)
f_supply <- f_demand <- seq(0, 1, by = 0.2)
grad <- 0.5
inter <- seq(0, 1, by = 0.2)
ee_thresh <- c(9.19, 45.96, 91.92) # chosen to represent 10%, 50%, and 100% of the diagonal length of the landscape
es_thresh <- c(9.19, 45.96, 91.92) # chosen to represent 10%, 50%, and 100% of the diagonal length of the landscape
alpha <- c(0.8, 1.0, 1.2) # chosen to represent a linear increase in log(ES supply) with log(patch size)
                          # and a 20% faster and 20% slower proportional increase in ES supply with log(patch size)
beta <- c(-0.2, 0, 0.2) # chosen to represent no effect of supply-suppy conenctions, a 20% negative effect on proportional ES supply
                        # per area connected as proportion of focal patch area, and a 20% positive effect on proportional ES supply
                        # per area connected as proportion of focal patch area
excludable <- c(TRUE, FALSE) # chosen to be excludable or non-exludable
rival <- c(TRUE, FALSE) # chosen to ne rival or non rival
gamma <- c(0, 0.25, 0.5) # 1/gamma is the elasticity of substitution (gamma is also a measure of risk-aversion)
                         # as gamma goes up then the ES becomes less substitutable, so gamma is essentially
                         # a measure of non-substitutability
                         # chosen to be perfectly substitable to increasding levels of non-substitability

# 2. Function for one set of landscape parameters
es_benefit <- function(nrow, ncol, p_supply, p_demand, f_supply, f_demand, grad, inter, params) {
  ee_thresh <- unique(params$ee_thresh) %>% sort
  es_thresh_excl <- unique(select(params, es_thresh, excludable)) %>%
    mutate(threshexcl = as.numeric(paste(es_thresh, ifelse(excludable, 1, 0), sep = ""))) %>%
    arrange(threshexcl)
  es_thresh <- es_thresh_excl$es_thresh
  excludable <- es_thresh_excl$excludable

  lscape <- ls_create(nrow = nrow,
                      ncol = ncol,
                      p_supply = p_supply,
                      p_demand = p_demand,
                      f_supply = f_supply,
                      f_demand = f_demand,
                      grad = grad,
                      inter = inter)

  ee_net <- map(ee_thresh,
                       create_ee_network,
                       ls_supply = lscape$ls_supply,
                       supply_area = "patch_area")

  es_net <- map2(es_thresh, excludable,
                       create_es_network,
                       ls_supply = lscape$ls_supply,
                       ls_demand = lscape$ls_demand,
                       supply_area = "patch_area",
                       demand_area = "patch_area")

  # this is to avoid having the networks repeated multiple times in the dataframe
  # which caused the function to generate objects too large for memory
  calc_ben <- function(ee_net_id, es_net_id, rival, alpha, beta, gamma, ee_net, es_net) {
    ee_network <- ee_net[[ee_net_id]]$network
    es_network <- es_net[[es_net_id]]$network
    ee_out <- ee_net[[ee_net_id]]$params
    es_out <- es_net[[es_net_id]]$params
    out <- calculate_benefit(ee_network, es_network, rival, alpha, beta, gamma)
    out <- bind_cols(out, ee_out, es_out) %>%
      select(-rival, -alpha, -beta, -gamma, -ee_thresh, -es_thresh, -excludable)
    return(out)
  }

  benefit <- params %>%
    mutate(ee_net_id = ee_thresh %>% as.factor %>% as.numeric,
           es_net_id = as.numeric(paste(es_thresh, ifelse(excludable, 1, 0), sep = "")) %>% as.factor %>% as.numeric,
           benefit = future_pmap(.l = list(ee_net_id = ee_net_id,
                                           es_net_id = es_net_id,
                                           rival = rival,
                                           alpha = alpha,
                                           beta = beta,
                                           gamma = gamma),
                                 calc_ben, ee_net = ee_net, es_net = es_net)) %>%
                                  unnest(cols = c(benefit)) %>% select(-ee_net_id, -es_net_id) %>%
    mutate(p_supply = p_supply, p_demand = p_demand, f_supply = f_supply, f_demand = f_demand, grad = grad, inter = inter)

  return(benefit)
}

strt <- Sys.time()

if (supportsMulticore()) {
  plan(multicore)
} else {
  plan(multisession)
}

# 3. create the landscape parameters ----
out <- crossing(nrow, ncol, p_supply, p_demand, f_supply, f_demand, grad, inter, ee_thresh, es_thresh,
                alpha, beta, excludable, rival, gamma) %>%
                group_by(nrow, ncol, p_supply, p_demand, f_supply, f_demand, grad, inter) %>%
                group_nest(.key = "params") %>%
                future_pmap_dfr(es_benefit, .options = furrr_options(seed = TRUE))

save(out, file = tempfile(tmpdir = "results/benefit_replicates", fileext = ".rda"))

print(paste0("Replicate complete: ", Sys.time() - strt))
