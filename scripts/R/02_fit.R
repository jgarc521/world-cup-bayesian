# 02_fit.R
# Fit the dynamic negative-binomial model with recency weighting.

library(cmdstanr)
library(posterior)
library(tidyverse)

stan_data   <- readRDS("data/processed/stan_data.rds")
team_lookup <- readRDS("data/processed/team_lookup.rds")
W  <- stan_data$W
Tn <- nrow(team_lookup)

mod <- cmdstan_model("scripts/stan/dynamic_poisson.stan")

init_fn <- function() list(
  mu         = 0.1,
  mu_wc      = 0.3,
  home_adv   = 0.1,
  phi        = 5.0,
  sigma_team = 0.2,
  sigma_att  = 0.05,
  sigma_def  = 0.05,
  z_att      = matrix(0, nrow = Tn, ncol = W),
  z_def      = matrix(0, nrow = Tn, ncol = W)
)

fit <- mod$sample(
  data            = stan_data,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 1000,
  adapt_delta     = 0.95,
  max_treedepth   = 12,
  seed            = 1,
  refresh         = 200,
  init            = init_fn
)

# ---- diagnostics ----
print(fit$diagnostic_summary())
print(fit$summary(c("mu", "mu_wc", "home_adv", "phi", "sigma_team", "sigma_att", "sigma_def")))
cat(sprintf("\ncor(mu, home_adv) = %.2f\n",
            cor(as_draws_df(fit$draws("mu"))$mu,
                as_draws_df(fit$draws("home_adv"))$home_adv)))

# ---- face validity ----
sim_vars <- c("mu", "mu_wc", "home_adv", "phi", "sigma_att", "sigma_def",
              sprintf("att[%d,%d]", 1:Tn, W),
              sprintf("def[%d,%d]", 1:Tn, W))
post <- fit$draws(variables = sim_vars, format = "df")

get_mean <- function(par, t) mean(post[[sprintf("%s[%d,%d]", par, t, W)]])

strength <- team_lookup %>%
  rowwise() %>%
  mutate(att = get_mean("att", team_id),
         def = get_mean("def", team_id)) %>%
  ungroup() %>%
  mutate(rating = att + def) %>%
  arrange(desc(rating))

cat("\nTop 20 teams by current latent strength:\n")
print(head(strength, 20), n = 20)

# ---- PPC sanity check on mu ----
mu_mean    <- mean(post$mu)
mu_wc_mean <- mean(post$mu_wc)
cat(sprintf("\nPosterior mean mu     = %.3f => baseline goals (qualifier) = %.3f\n",
            mu_mean, exp(mu_mean)))
cat(sprintf("Posterior mean mu_wc  = %.3f => WC baseline goals          = %.3f\n",
            mu_wc_mean, exp(mu_mean + mu_wc_mean)))

saveRDS(post,     "data/processed/posterior_slim.rds")
saveRDS(strength, "data/processed/strength_current.rds")
cat("\nPosterior saved.\n")