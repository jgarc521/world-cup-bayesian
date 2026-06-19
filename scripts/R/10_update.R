# 10_update.R
# Matchday update wrapper. Run this after each matchday to:
#   1. Pull new results (automatic — martj42 dataset updates within ~24hrs)
#   2. Refit the Bayesian model on updated data
#   3. Resimulate the bracket (Bayesian + ensemble)
#   4. Snapshot the new probabilities
#   5. Regenerate all plots
#
# Usage: source("scripts/R/10_update.R")
# Expected runtime: ~10-15 mins (dominated by Stan refit)

library(tidyverse)

# ---- helpers ----
section <- function(msg) {
  cat(sprintf("\n%s\n%s\n", msg, strrep("=", nchar(msg))))
}

elapsed <- function(start) {
  secs <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  sprintf("%.1f min", secs / 60)
}

total_start <- Sys.time()

# ---- check for new results before doing anything expensive ----
section("Checking for new results")

RESULTS_URL <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"
raw <- read_csv(RESULTS_URL, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(tournament == "FIFA World Cup",
         date >= as.Date("2026-06-01"),
         !is.na(home_score), !is.na(away_score))

prev_snapshot <- tryCatch(
  readRDS("data/processed/snapshots.rds") %>%
    filter(snapshot_date == max(snapshot_date)) %>%
    slice(1) %>%
    pull(snapshot_date),
  error = function(e) as.Date("2026-06-01")
)

new_results <- raw %>% filter(date > prev_snapshot)
cat(sprintf("Last snapshot : %s\n", prev_snapshot))
cat(sprintf("New results   : %d matches since last snapshot\n", nrow(new_results)))

if (nrow(new_results) > 0) {
  cat("New matches:\n")
  new_results %>%
    transmute(date, match = sprintf("%s %d-%d %s",
                                   home_team, home_score, away_score, away_team)) %>%
    print(n = 20)
} else {
  cat("No new results found. martj42 dataset may not have updated yet.\n")
  cat("Check https://github.com/martj42/international_results and rerun later.\n")
  cat("Continuing anyway to regenerate plots from current posterior.\n")
}

# ---- 1. rebuild data ----
section("Step 1/5 — Rebuilding data")
t <- Sys.time()
source("scripts/R/01_build_data.R")
cat(sprintf("Done in %s\n", elapsed(t)))

# ---- 2. refit Stan model ----
section("Step 2/5 — Refitting Bayesian model")
t <- Sys.time()
source("scripts/R/02_fit.R")
cat(sprintf("Done in %s\n", elapsed(t)))

# ---- 3. Bayesian simulator ----
section("Step 3/5 — Simulating bracket (Bayesian)")
t <- Sys.time()
source("scripts/R/03_simulate.R")
cat(sprintf("Done in %s\n", elapsed(t)))

# ---- 4. Ensemble simulator ----
section("Step 4/5 — Simulating bracket (ensemble)")
t <- Sys.time()
source("scripts/R/09_simulate_ensemble.R")
cat(sprintf("Done in %s\n", elapsed(t)))

# ---- 5. Snapshot + plots ----
section("Step 5/5 — Snapshotting and regenerating plots")
t <- Sys.time()
source("scripts/R/06_snapshot.R")
source("scripts/R/05_visualize.R")
cat(sprintf("Done in %s\n", elapsed(t)))

# ---- summary ----
section("Update complete")
snapshots <- readRDS("data/processed/snapshots.rds")
n_snaps   <- n_distinct(snapshots$snapshot_date)

cat(sprintf("Total runtime : %s\n", elapsed(total_start)))
cat(sprintf("Snapshots     : %d (dates: %s)\n",
            n_snaps,
            paste(sort(unique(snapshots$snapshot_date)), collapse = ", ")))

# print top 10 from ensemble for a quick sanity check
cat("\nTop 10 ensemble win probabilities:\n")
readRDS("data/processed/progression_ensemble.rds") %>%
  filter(team %in% unlist(map(
    c(A="Mexico", B="Canada", C="Brazil", D="United States",
      E="Germany", F="Netherlands", G="Belgium", H="Spain",
      I="France", J="Argentina", K="Portugal", L="England"),
    function(a) {
      wc <- read_csv(RESULTS_URL, show_col_types = FALSE) %>%
        mutate(date = as.Date(date)) %>%
        filter(tournament == "FIFA World Cup",
               date >= as.Date("2026-06-01"),
               date <= as.Date("2026-06-27"))
      opp <- wc %>% filter(home_team == a | away_team == a) %>%
        mutate(o = if_else(home_team == a, away_team, home_team)) %>%
        pull(o) %>% unique()
      c(a, opp)
    }
  ))) %>%
  arrange(desc(P_Win)) %>%
  select(team, P_Win, P_Final, P_SF) %>%
  head(10) %>%
  mutate(across(where(is.numeric), ~ scales::percent(.x, accuracy = 0.1))) %>%
  print(n = 10)

cat("\nPlots saved to report/\n")
cat("Run git add -A && git commit -m 'Matchday update' && git push to publish.\n")