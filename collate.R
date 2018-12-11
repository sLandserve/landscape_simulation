library(tidyverse)

f = list.files("results/benefit_replicates", full.names = TRUE)

read_rda = function(x) {
	load(x)
	return(out)
}

out = map_dfr(f, read_rda)

save(out, file = "results/all_replicates.rda")
