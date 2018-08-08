library(tidyverse)
library(rmarkdown)

alpha <- c(0.5, 1, 1.5)
beta <- c(-0.2, 0, 0.2)
gamma <- c(0.1, 0.5)

abg <- crossing(alpha, beta, gamma)

for(i in 1:nrow(abg)) {
  alpha_val = abg$alpha[i]
  beta_val = abg$beta[i]
  gamma_val = abg$gamma[i]
  fname_mean <- paste0("benefit_plots_mean", i, ".md")
  fname_cv <- paste0("benefit_plots_cv", i, ".md")
  render("benefit_plots.Rmd", output_format = "github_document", output_file = fname_mean, params = list(fn = "mean", measure = "benefit", alpha_val = alpha_val, beta_val = beta_val, gamma_val = gamma_val))
  render("benefit_plots.Rmd", output_format = "github_document", output_file = fname_cv, params = list(fn = "cv", measure = "benefit", alpha_val = alpha_val, beta_val = beta_val, gamma_val = gamma_val))
}
