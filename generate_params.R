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

n <- nrow(param_table) / 10

for(i in 1:10) {
  write.csv(param_table[(((i-1)*n) + 1):i*n,], paste0("data/param_table", i, ".csv"))  
}
