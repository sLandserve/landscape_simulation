# simulate using purrr
library(tidyverse)
library(furrr)
library(sLandserv)

# 1. set the paramters ----
nrow <- ncol <- 65 # NB all landscapes will need to be of dimension 2^n + 1 due to way mid-point displacement method works
p_supply <- p_demand <- c(0.1, 0.4)
f_supply <- f_demand <- inter <- seq(0, 1, by = 0.25)
ee_thresh <- c(20, 30, 50)
es_thresh <- c(20, 30, 50)
rival <- c(TRUE, FALSE)
alpha <- 1
beta <- c(0, 0.5)
gamma <- c(0.0001, 0.1)

# THOUGHTS ABOUT PARAMETERS 
# beta - strong effect on the outcome. We either need to really
# think about the range of beta, or change it. connected_area^beta might make more sense
# than exp(beta*connected_area)

# trying a run with one parameter combination for now

strt <- Sys.time()

plan(multiprocess)

# 2. create the landscape parameters ----
benefit <- crossing(nrow, ncol, p_supply,p_demand,f_supply,f_demand,inter) %>% 
  # 3. simulate the landscapes ----
  mutate(ls_sim = pmap(., ls_create),
       # and get the bits we'll use later out
       ls_supply = map(ls_sim, "ls_supply"),
       ls_demand = map(ls_sim, "ls_demand"),
       params = map(ls_sim, "params")) %>% 
  
  # 4. create the network threshold parameters ----
  crossing(ee_thresh, es_thresh) %>% 
  
  # 5. create the networks ----
  mutate(ee_network = select(., ls_supply, ee_thresh, params) %>% 
         mutate(supply_area = "patch_area") %>% 
         pmap(create_ee_network),
       params = map(ee_network, "params")) %>% 
  mutate(es_network = select(., ls_supply, ls_demand, es_thresh, params) %>% 
           mutate(supply_area = "patch_area", demand_area = "patch_area") %>% 
           pmap(create_es_network),
         # get the bits we'll use later
         params = map(es_network, "params"),
         ee_network = map(ee_network, "network"),
         es_network = map(es_network, "network")) %>% 
  
  # 6. create benefit parameters ----
  crossing(rival, alpha, beta, gamma) %>% 
  mutate(benefit = select(., ee_network, es_network, rival, alpha, beta, gamma, params) %>% 
           future_pmap(calculate_benefit)) %>% 
  select(benefit) %>% 
  unnest
  
  # 7. output for analysis ----
  save(benefit, file = tempfile(tmpdir = "results/benefit_replicates", fileext = ".Rda"))

print(paste0("Replicate complete: ", Sys.time() - strt))


