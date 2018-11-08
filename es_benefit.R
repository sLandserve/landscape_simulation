# simulate using purrr
library(tidyverse)
library(furrr)
library(sLandserv)

# 1. set the paramters ----
nrow <- ncol <- 65 # NB all landscapes will need to be of dimension 2^n + 1 due to way mid-point displacement method works
p_supply <- p_demand <- c(0.1, 0.2, 0.3, 0.4)
f_supply <- f_demand <- inter <- seq(0, 1, by = 0.2)
ee_thresh <- c(9, 23, 46, 69, 92) # chosen to represent 10%, 25%, 75%, 100% of the diagonal length of the landscape
es_thresh <- c(9, 23, 46, 69, 92) # chosen to represent 10%, 25%, 75%, 100% of the diagonal length of the landscape
rival <- c(TRUE, FALSE)
alpha <- c(0.3, 1.0)
beta <- c(-0.1, -0.05, -0.01, 0, 0.01, 0.05, 0.1)
gamma <- c(0, 0.1, 0.5) # included 0 here to represent a flat demand curve (i.e., perfectly substitutable)

strt <- Sys.time()

plan(multisession)

# 2. create the landscape parameters ----
benefit <- crossing(nrow, ncol, p_supply,p_demand,f_supply,f_demand,inter) %>%
  slice(1:5) %>% 
  # 3. simulate the landscapes ----
  mutate(ls_sim = future_pmap(., ls_create),
       # and get the bits we'll use later out
       ls_supply = future_map(ls_sim, "ls_supply"),
       ls_demand = future_map(ls_sim, "ls_demand"),
       params = future_map(ls_sim, "params")) %>% 
  select(-ls_sim) %>%
  
  # 4. create the network threshold parameters ----
  crossing(ee_thresh, es_thresh) %>%

  # 5. create the networks ----
  mutate(ee_network = select(., ls_supply, ee_thresh, params) %>% 
         mutate(supply_area = "patch_area") %>% 
         future_pmap(create_ee_network),
       params = future_map(ee_network, "params")) %>% 
  mutate(es_network = select(., ls_supply, ls_demand, es_thresh, params) %>% 
           mutate(supply_area = "patch_area", demand_area = "patch_area") %>% 
           future_pmap(create_es_network),
         # get the bits we'll use later
         params = future_map(es_network, "params"),
         ee_network = future_map(ee_network, "network"),
         es_network = future_map(es_network, "network")) %>% 
  select(-ls_supply, -ls_demand) %>% 
  
  # 6. create benefit parameters ----
  crossing(rival, alpha, beta, gamma) %>%
  mutate(benefit = select(., ee_network, es_network, rival, alpha, beta, gamma, params) %>%
           future_pmap(calculate_benefit)) %>%
  select(benefit) %>%
  unnest

  # 7. output for analysis ----
  save(benefit, file = tempfile(tmpdir = "results/benefit_replicates", fileext = ".Rda"))

print(paste0("Replicate complete: ", Sys.time() - strt))
