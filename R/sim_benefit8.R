library(plyr)
library(sLandserv)
library(tidyverse)

strt <- Sys.time()
param_table <- read_csv("data/param_table8.csv")

# 1. simulate landscapes ----
ls_sim <- apply(param_table, 1, function(x) {
  ls_create(nrow = x[["nrow"]], 
            ncol = x[["ncol"]],
            p_supply = x[["p_supply"]], 
            p_demand = x[["p_demand"]], 
            f_supply = x[["f_supply"]], 
            f_demand = x[["f_demand"]], 
            inter = x[["inter"]])})

# 3. create networks ----
ee_thresh <- c(10, 30)
es_thresh <- c(10, 30)
thresholds <- expand.grid(ee_thresh = ee_thresh, 
                          es_thresh = es_thresh)

networks <- apply(thresholds, 1, function(x) {
  lapply(ls_sim, function(ls) {
    network_create(ls = ls$ls, 
                   params = ls$params,
                   es_thresh = x[["es_thresh"]],
                   ee_thresh = x[["ee_thresh"]])
  })
})

networks <- unlist(networks, recursive = FALSE)


# 4. calculate benefit ----
rival <- c(TRUE, FALSE)
alpha <- c(-1, 0, 1)
beta <- seq(0.1, 0.5, by = 0.1)
gamma <- seq(0.1, 0.5, by = 0.1)

benefit_params <- expand.grid(rival = rival, 
                              alpha = alpha, 
                              beta = beta, 
                              gamma = gamma)

benefit <- ddply(benefit_params, names(benefit_params), function(x) {
  ldply(networks, function(net) {
    calculate_benefit(net = net,
                      rival = x[["rival"]],
                      alpha = x[["alpha"]],
                      beta = x[["beta"]],
                      gamma = x[["gamma"]])
  })
})

save(benefit, file = tempfile(tmpdir = "results", fileext = ".Rda"))

runtime <- strt - Sys.time()


