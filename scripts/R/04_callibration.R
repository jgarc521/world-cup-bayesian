# 04_calibration.R
# Calibration check on the group-stage matches already played:
# - Brier score (mean squared error of win/draw/loss probabilities)
# - Reliability diagram (predicted probability vs actual frequency)
# - Posterior predictive check on total goals (are we under/over-predicting scoring?)

library(tidyverse)
library(lubridate)

RESULTS_URL <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"
N_DRAWS     <- 1000    # posterior draws to average over per match

stan_data   <- readRDS("data/processed/stan_data.rds")
team_lookup <- readRDS("data/processed/team_lookup.rds")
post        <- readRDS("data/processed/posterior_slim.rds")
W <- stan_data$W
Tn <- nrow(team_lookup)
id <- setNames(team_lookup$team_id, team_lookup$team)

att_mat <- matrix(0, nrow(post), Tn)
def_mat <- matrix(0, nrow(post), Tn)
for (t in 1:Tn) {
  att_mat[, t] <- post[[sprintf("att[%d,%d]", t, W)]]
  def_mat[, t] <- post[[sprintf("def[%d,%d]", t, W)]]
}
mu_vec    <- post$mu
mu_wc_vec <- post$mu_wc
n_draw  <- nrow(post)

# ---- matches to evaluate: WC group stage already played ----
played <- read_csv(RESULTS_URL, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(tournament == "FIFA World Cup",
         date >= as.Date("2026-06-01"), date <= Sys.Date(),
         !is.na(home_score), !is.na(away_score)) %>%
  transmute(date, home = home_team, away = away_team,
            hg = as.integer(home_score), ag = as.integer(away_score),
            result = case_when(hg > ag ~ "home", hg == ag ~ "draw", TRUE ~ "away"))

# keep only teams the model knows about
played <- played %>% filter(home %in% names(id), away %in% names(id))
cat(sprintf("Evaluating on %d played WC matches\n", nrow(played)))

# ---- predicted probabilities by simulation ----
# For each match, draw N_DRAWS posterior samples and simulate a scoreline each time.
# P(home win) = fraction of sims where home goals > away goals, etc.
set.seed(42)
draws_idx <- sample.int(n_draw, N_DRAWS, replace = TRUE)

pred <- played %>%
  rowwise() %>%
  mutate(
    lam_h = list(exp(mu_vec[draws_idx] + mu_wc_vec[draws_idx] + att_mat[draws_idx, id[home]] - def_mat[draws_idx, id[away]])),
    lam_a = list(exp(mu_vec[draws_idx] + mu_wc_vec[draws_idx] + att_mat[draws_idx, id[away]] - def_mat[draws_idx, id[home]])),
    gh    = list(rpois(N_DRAWS, lam_h)),
    ga    = list(rpois(N_DRAWS, lam_a)),
    p_home = mean(gh > ga),
    p_draw = mean(gh == ga),
    p_away = mean(gh < ga),
    exp_hg = mean(lam_h),
    exp_ag = mean(lam_a)
  ) %>%
  select(-lam_h, -lam_a, -gh, -ga) %>%
  ungroup()

# ---- Brier score ----
# Multi-class Brier: BS = mean over matches of sum_k (p_k - I_k)^2
brier <- pred %>%
  mutate(
    I_home = as.integer(result == "home"),
    I_draw = as.integer(result == "draw"),
    I_away = as.integer(result == "away"),
    bs = (p_home - I_home)^2 + (p_draw - I_draw)^2 + (p_away - I_away)^2
  ) %>%
  summarise(brier_score = mean(bs))

cat(sprintf("\nBrier score: %.4f\n", brier$brier_score))
cat("(Reference: random guess = 0.667, perfect = 0.000, coin-flip on winner = ~0.50)\n")

# ---- reliability diagram (home-win probability only for clarity) ----
bins <- pred %>%
  mutate(bin = cut(p_home, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(mean_pred = mean(p_home),
            actual_freq = mean(result == "home"),
            n = n(), .groups = "drop") %>%
  filter(!is.na(bin))

cat("\nReliability (home win) — predicted vs actual:\n")
print(bins, n = 20)

# ---- posterior predictive check on goals ----
cat(sprintf("\nGoals PPC:\n"))
cat(sprintf("  Observed mean goals per team per match : %.3f\n",
            mean(c(played$hg, played$ag))))
cat(sprintf("  Model predicted mean                  : %.3f\n",
            mean(c(pred$exp_hg, pred$exp_ag))))
cat(sprintf("  Observed pct of matches with 0-0      : %.1f%%\n",
            100 * mean(played$hg == 0 & played$ag == 0)))

# ---- save for the visualisation ----
saveRDS(pred,  "data/processed/calibration_pred.rds")
saveRDS(bins,  "data/processed/reliability_bins.rds")

# ---- quick ggplot reliability diagram ----
p <- ggplot(bins, aes(mean_pred, actual_freq)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = n), colour = "#1a6eb5") +
  geom_line(colour = "#1a6eb5") +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_size_continuous(range = c(2, 8), name = "# matches") +
  labs(title = "Reliability diagram — home win probability",
       subtitle = "Points on the dashed line = perfect calibration",
       x = "Predicted probability", y = "Observed frequency") +
  theme_minimal(base_size = 13)

ggsave("report/reliability_diagram.png", p, width = 6, height = 5, dpi = 150)
cat("\nSaved report/reliability_diagram.png\n")