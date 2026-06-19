# 05_visualize.R
# Produces four portfolio-ready plots:
#   1. Championship win probabilities (top 20, horizontal bar)
#   2. Full progression heatmap (all rounds, top 24 teams)
#   3. Team strength trajectories (attack + defense over time, top 12)
#   4. Win probability evolution across snapshots (updating story)

library(tidyverse)
library(lubridate)

dir.create("report", showWarnings = FALSE)

progression  <- readRDS("data/processed/progression.rds")
strength_all <- readRDS("data/processed/strength_current.rds")
stan_data    <- readRDS("data/processed/stan_data.rds")
team_lookup  <- readRDS("data/processed/team_lookup.rds")
post         <- readRDS("data/processed/posterior_slim.rds")

# WC teams only for display
wc_teams <- c(
  "Argentina","Australia","Austria","Belgium","Brazil","Canada","Cape Verde",
  "Colombia","Croatia","Curaçao","DR Congo","Ecuador","Egypt","England",
  "France","Germany","Ghana","Iran","Iraq","Ivory Coast","Japan","Jordan",
  "Mexico","Morocco","Netherlands","New Zealand","Norway","Panama",
  "Paraguay","Portugal","Saudi Arabia","Senegal","Spain","Sweden",
  "Switzerland","Tunisia","Turkey","United States","Uruguay","Uzbekistan",
  "Algeria","South Korea"
)

wc_progression <- progression %>% filter(team %in% wc_teams)
W  <- stan_data$W
Tn <- nrow(team_lookup)

theme_wc <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey40", size = 10),
        plot.caption  = element_text(colour = "grey55", size = 8))

# -----------------------------------------------------------------------
# Plot 1: Championship win probabilities — top 20
# -----------------------------------------------------------------------
top20 <- wc_progression %>%
  arrange(desc(P_Win)) %>%
  slice(1:20) %>%
  mutate(team = fct_reorder(team, P_Win))

p1 <- ggplot(top20, aes(P_Win, team)) +
  geom_col(fill = "#1a6eb5", width = 0.7) +
  geom_text(aes(label = scales::percent(P_Win, accuracy = 0.1)),
            hjust = -0.1, size = 3.2, colour = "grey30") +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "2026 World Cup — Championship win probabilities",
       subtitle = "Dynamic Bayesian model, coin-flip penalty resolution · 2,000 simulations",
       x = "P(Win tournament)", y = NULL,
       caption  = sprintf("Model snapshot: %s", Sys.Date())) +
  theme_wc

ggsave("report/01_win_probs.png", p1, width = 8, height = 7, dpi = 150)
cat("Saved 01_win_probs.png\n")

# -----------------------------------------------------------------------
# Plot 2: Progression heatmap — all rounds, top 24 teams by P_Win
# -----------------------------------------------------------------------
top24 <- wc_progression %>%
  arrange(desc(P_Win)) %>%
  slice(1:24) %>%
  mutate(team = fct_reorder(team, P_Win))

heat_long <- top24 %>%
  pivot_longer(cols = c(P_R32, P_R16, P_QF, P_SF, P_Final, P_Win),
               names_to = "round", values_to = "prob") %>%
  mutate(round = factor(round,
                        levels = c("P_R32","P_R16","P_QF","P_SF","P_Final","P_Win"),
                        labels = c("R32","R16","QF","SF","Final","Win")))

p2 <- ggplot(heat_long, aes(round, team, fill = prob)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = scales::percent(prob, accuracy = 1)),
            size = 2.8, colour = "white") +
  scale_fill_gradient(low = "#d4e9f7", high = "#0d3d6e",
                      labels = scales::percent, name = "Probability") +
  labs(title    = "2026 World Cup — Tournament progression probabilities",
       subtitle = "Dynamic Bayesian model · 2,000 simulations",
       x = "Round", y = NULL,
       caption  = sprintf("Model snapshot: %s", Sys.Date())) +
  theme_wc +
  theme(axis.text.x = element_text(face = "bold"))

ggsave("report/02_progression_heatmap.png", p2, width = 9, height = 8, dpi = 150)
cat("Saved 02_progression_heatmap.png\n")

# -----------------------------------------------------------------------
# Plot 3: Team strength trajectories — top 12 WC teams
# -----------------------------------------------------------------------
top12_teams <- wc_progression %>%
  arrange(desc(P_Win)) %>%
  slice(1:12) %>%
  pull(team)

top12_ids <- team_lookup %>%
  filter(team %in% top12_teams) %>%
  pull(team_id)

# extract posterior mean att and def at every knot for top 12
knot_seq <- 1:W
traj <- map_dfr(top12_ids, function(tid) {
  tname <- team_lookup$team[team_lookup$team_id == tid]
  map_dfr(knot_seq, function(w) {
    att_draws <- post[[sprintf("att[%d,%d]", tid, w)]]
    def_draws <- post[[sprintf("def[%d,%d]", tid, w)]]
    if (is.null(att_draws)) return(NULL)
    tibble(team  = tname,
           knot  = w,
           att   = mean(att_draws),
           def   = mean(def_draws),
           att_lo = quantile(att_draws, 0.1),
           att_hi = quantile(att_draws, 0.9),
           rating = mean(att_draws) + mean(def_draws))
  })
})

# map knots to approximate dates using stan_data
# knots are monthly; W knots span from START_DATE
knot_dates <- seq.Date(from = as.Date("2024-01-01"),
                       by   = "month",
                       length.out = W)
traj <- traj %>% mutate(date = knot_dates[knot])

p3 <- ggplot(traj, aes(date, att, colour = team, group = team)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_ribbon(aes(ymin = att_lo, ymax = att_hi, fill = team),
              alpha = 0.08, colour = NA) +
  scale_colour_viridis_d(option = "turbo", name = NULL) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title    = "Attack strength trajectories — top 12 WC teams",
       subtitle = "Posterior mean ± 80% credible interval",
       x = NULL, y = "Latent attack strength",
       caption  = "Dynamic Bayesian state-space model") +
  theme_wc +
  theme(legend.position = "right")

ggsave("report/03_strength_trajectories.png", p3, width = 10, height = 6, dpi = 150)
cat("Saved 03_strength_trajectories.png\n")

# -----------------------------------------------------------------------
# Plot 4: Win probability evolution across snapshots
# -----------------------------------------------------------------------
snapshot_file <- "data/processed/snapshots.rds"

if (file.exists(snapshot_file)) {
  snapshots <- readRDS(snapshot_file)
  n_dates   <- n_distinct(snapshots$snapshot_date)

  if (n_dates >= 2) {
    # top 10 teams by latest P_Win
    latest_top10 <- snapshots %>%
      filter(snapshot_date == max(snapshot_date)) %>%
      arrange(desc(P_Win)) %>%
      slice(1:10) %>%
      pull(team)

    snap_long <- snapshots %>%
      filter(team %in% latest_top10)

    p4 <- ggplot(snap_long, aes(snapshot_date, P_Win,
                                colour = team, group = team)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::percent) +
      scale_colour_viridis_d(option = "turbo", name = NULL) +
      labs(title    = "2026 World Cup — Win probability evolution",
           subtitle = "Updated after each matchday as results arrive",
           x = NULL, y = "P(Win tournament)",
           caption  = "Dynamic Bayesian model, posterior updated with each new result") +
      theme_wc +
      theme(legend.position = "right")

    ggsave("report/04_probability_evolution.png", p4,
           width = 10, height = 6, dpi = 150)
    cat("Saved 04_probability_evolution.png\n")
  } else {
    cat("Only 1 snapshot so far — run after more matchdays for evolution plot\n")
  }
} else {
  cat("No snapshots file yet — run 06_snapshot.R first\n")
}

cat("\nAll plots saved to report/\n")