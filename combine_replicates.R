f <- list.files("results/benefit_replicates/", pattern = "file", full.names = TRUE)

out <- ldply(f, function(x) get(load(x)))

save(out, "results/benefit_all_replicates.rda")