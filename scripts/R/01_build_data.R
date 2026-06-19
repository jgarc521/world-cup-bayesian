# 01_build_data.R
# Build Stan inputs from the martj42 international results dataset.
# Adds recency weighting: matches decay toward WEIGHT_FLOOR as they age,
# so recent form dominates without hard-cutting the training window.

library(tidyverse)
library(lubridate)

RESULTS_URL  <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"
START_DATE   <- as.Date("2024-01-01")   # post-Qatar; widen/narrow as needed
MIN_MATCHES  <- 5                        # drop teams with fewer games in window
HALF_LIFE    <- 365                      # days; weight halves every ~1 year
WEIGHT_FLOOR <- 0.2                      # floor so old matches still contribute something
TODAY        <- Sys.Date()

raw <- read_csv(RESULTS_URL, show_col_types = FALSE) %>%
  mutate(date    = as.Date(date),
         neutral = as.integer(as.logical(neutral)))

window <- raw %>% filter(date >= START_DATE)

played <- window %>%
  filter(!is.na(home_score), !is.na(away_score), date <= TODAY) %>%
  mutate(home_score = as.integer(home_score),
         away_score = as.integer(away_score))

# ---- recency weights ----
# w(t) = WEIGHT_FLOOR + (1 - WEIGHT_FLOOR) * exp(-log(2) * age_days / HALF_LIFE)
# Recent match (age=0): weight=1.0. Match from HALF_LIFE days ago: weight=0.6.
played <- played %>%
  mutate(age_days = as.numeric(TODAY - date),
         weight   = WEIGHT_FLOOR + (1 - WEIGHT_FLOOR) *
                    exp(-log(2) * age_days / HALF_LIFE))

# ---- drop ultra-rare teams ----
appearances <- bind_rows(
  played %>% transmute(team = home_team),
  played %>% transmute(team = away_team)
) %>% dplyr::count(team, name = "n")

keep_teams <- appearances %>% filter(n >= MIN_MATCHES) %>% pull(team)
played <- played %>% filter(home_team %in% keep_teams, away_team %in% keep_teams)

# ---- team id map ----
team_lookup <- tibble(team = sort(unique(c(played$home_team, played$away_team)))) %>%
  mutate(team_id = row_number())
id_of <- function(x) team_lookup$team_id[match(x, team_lookup$team)]

# ---- monthly time knots ----
played <- played %>% mutate(ym = floor_date(date, "month"))
knots <- tibble(ym = sort(unique(played$ym))) %>%
  mutate(knot = row_number(),
         gap  = as.numeric(ym - lag(ym)) / 30.44,
         dt   = if_else(is.na(gap), 1, pmax(1, round(gap))))
played <- played %>% left_join(knots, by = "ym")

# ---- matches frame ----
matches <- played %>%
  mutate(is_wc = as.integer(tournament == "FIFA World Cup")) %>%
  transmute(home_id = id_of(home_team),
            away_id = id_of(away_team),
            knot, hg = home_score, ag = away_score,
            neutral, weight, is_wc)

# ---- stan data ----
stan_data <- list(
  T       = nrow(team_lookup),
  G       = nrow(matches),
  W       = nrow(knots),
  home    = matches$home_id,
  away    = matches$away_id,
  knot    = matches$knot,
  neutral = matches$neutral,
  is_wc   = matches$is_wc,
  y_home  = matches$hg,
  y_away  = matches$ag,
  dt      = knots$dt,
  weights = matches$weight        # new: recency weights
)

# ---- upcoming fixtures ----
fixtures <- window %>%
  filter(date > TODAY, is.na(home_score) | is.na(away_score)) %>%
  transmute(date, home_team, away_team, tournament, neutral)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(stan_data,   "data/processed/stan_data.rds")
saveRDS(team_lookup, "data/processed/team_lookup.rds")
saveRDS(fixtures,    "data/processed/fixtures.rds")

cat(sprintf("Teams: %d | Games: %d | Knots: %d | Upcoming: %d\n",
            stan_data$T, stan_data$G, stan_data$W, nrow(fixtures)))
cat(sprintf("Weight range: %.2f – %.2f (mean %.2f)\n",
            min(matches$weight), max(matches$weight), mean(matches$weight)))