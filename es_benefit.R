# simulate using purrr
library(sLandserv)
library(furrr)
library(tidyverse)
library(sf)

# 1. set the parameters ----
nrow <- ncol <- 65 # NB all landscapes will need to be of dimension 2^n + 1 due to way mid-point displacement method works
p_supply <- p_demand <- c(0.1, 0.2, 0.3, 0.4, 0.5)
f_supply <- f_demand <- seq(0, 1, by = 0.1)
grad <- 0
inter <- 0
ee_thresh <- c(9, 23, 46, 69, 83) # chosen to represent 10%, 25%, 50%, 75%, and 90% of the diagonal length of the landscape
es_thresh <- c(9, 23, 46, 69, 83) # chosen to represent 10%, 25%, 50%, 75%, and 90% of the diagonal length of the landscape
rival <- c(TRUE, FALSE)
alpha <- c(0.3, 1.0) #chosen to represent a species area relationship and a linear relationship with patch size
beta <- c(-0.1, -0.01, 0, 0.01, 0.1) #this represents the effect of supply-supply connections
gamma <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5) # included 0 here to represent a flat demand curve (i.e., perfectly substitutable) -
          # as gamma goes up then the ES becomes less substitutable, so gamma is a measure of non-substitutability

# 2. Function for one set of landscape parameters
es_benefit <- function(nrow, ncol, p_supply, p_demand, f_supply, f_demand, grad, inter, params) {
  ee_thresh <- unique(params$ee_thresh)
  es_thresh <- unique(params$es_thresh)

  lscape <- ls_create(nrow = nrow,
                      ncol = ncol,
                      p_supply = p_supply,
                      p_demand = p_demand,
                      f_supply = f_supply,
                      f_demand = f_demand,
                      grad = grad,
                      inter = inter)

  ee_net <- future_map(ee_thresh %>% unique,
                       create_ee_network,
                       ls_supply = lscape$ls_supply,
                       supply_area = "patch_area")

  es_net <- future_map(es_thresh %>% unique,
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
    out <- bind_cols(out, ee_out, es_out)
    return(out)
  }

  benefit <- params %>%
    mutate(ee_net_id = ee_thresh %>% as.factor %>% as.numeric,
           es_net_id = es_thresh %>% as.factor %>% as.numeric,
           benefit = future_pmap(.l = list(ee_net_id = ee_net_id,
                                           es_net_id = es_net_id,
                                           rival = rival,
                                           alpha = alpha,
                                           beta = beta,
                                           gamma = gamma),
                                 calc_ben, ee_net = ee_net, es_net = es_net)) %>%
    unnest() %>%
    mutate(p_supply = p_supply, p_demand = p_demand, f_supply = f_supply, f_demand = f_demand, grad = grad, inter = inter)

  return(benefit)
}

strt <- Sys.time()

plan(multiprocess)

# 3. create the landscape parameters ----
out <- crossing(nrow, ncol, p_supply, p_demand, f_supply, f_demand, grad, inter, ee_thresh, es_thresh, rival, alpha, beta, gamma) %>%
  group_by(nrow, ncol, p_supply, p_demand, f_supply, f_demand, grad, inter) %>%
  nest(.key = params) %>%
  future_pmap_dfr(es_benefit) %>%
  select(-rival1, -alpha1, -beta1, -gamma1, -lambda, -phi, -ee_thresh1, -es_thresh1)

save(out, file = tempfile(tmpdir = "results/benefit_replicates", fileext = ".rda"))

print(paste0("Replicate complete: ", Sys.time() - strt))
