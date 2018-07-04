library(plyr)
library(sLandserv)
library(tidyverse)
library(doSNOW)

for(i in 1:50) {
  strt <- Sys.time()
  
  # 1. generate the parameter table ----
  nrows <- ncols <- 65 # NB all landscapes will need to be of dimension 2^n + 1 due to way mid-point displacement method works
  p_supply <- p_demand <- c(0.1, 0.4)
  f_supply <- f_demand <- inter <- seq(0, 1, by = 0.25)
  
  # combine parameters into a table for creating landscapes
  param_table <- expand.grid(nrow = nrows,
                             ncol = ncols,
                             p_supply = p_supply,
                             p_demand = p_demand,
                             f_supply = f_supply,
                             f_demand = f_demand,
                             inter = inter)
  
  # 2. simulate landscapes ----
  
  cl <- makeCluster(4) # create parellel clusters
  registerDoSNOW(cl)
  
  ls_sim <- foreach(i=1:nrow(param_table),
                    .packages = c("sLandserv"),
                    .export = c("param_table")) %dopar% {
                      ls_create(nrow = param_table[i, "nrow"],
                                ncol = param_table[i, "ncol"],
                                p_supply = param_table[i, "p_supply"],
                                p_demand = param_table[i, "p_demand"],
                                f_supply = param_table[i, "f_supply"],
                                f_demand = param_table[i, "f_demand"],
                                inter = param_table[i, "inter"])
                    }
  
  # 3. create networks ----
  ee_thresh <- c(10, 30)
  es_thresh <- c(10, 30)
  thresholds <- expand.grid(ee_thresh = ee_thresh, 
                            es_thresh = es_thresh)
  
  
  
  networks <- apply(thresholds, 1, function(x) {
    foreach(ls = ls_sim, 
            .packages = c("sLandserv", "tidyverse", "sf"),
            .export = c("x", "ls_sim")) %dopar% {
              network_create(ls = ls$ls, 
                             params = ls$params,
                             es_thresh = x[["es_thresh"]],
                             ee_thresh = x[["ee_thresh"]])
            }})
  
  networks <- unlist(networks, recursive = FALSE)
  
  
  # 4. calculate benefit ----
  rival <- c(TRUE, FALSE)
  alpha <- c(0.5, 1, 1.5)
  beta <- seq(-0.2, 0.2, by = 0.1)
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
  
  runtime <- Sys.time() - strt
  print(paste0("Iteration ", i, " complete: ", runtime))
}

