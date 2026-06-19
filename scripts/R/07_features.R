# 07_features.R
# Build the feature matrix for the Layer 2 GBM.
# Features per team:
#   - Squad market value (Transfermarkt via worldfootballR)
#   - FIFA ranking + points
#   - Bayesian model attack/defense strength (from posterior)
#   - Recent form: W/D/L rate over last 10 matches
#   - Average age of squad
#
# Output: data/processed/team_features.rds
#         data/processed/match_features.rds  (one row per match, for GBM training)

library(tidyverse)
library(worldfootballR)
library(lubridate)

RESULTS_URL <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"
TODAY       <- Sys.Date()

team_lookup <- readRDS("data/processed/team_lookup.rds")
strength    <- readRDS("data/processed/strength_current.rds")
wc_teams    <- c(
  "Argentina","Australia","Austria","Belgium","Brazil","Canada","Cape Verde",
  "Colombia","Croatia","Curaçao","DR Congo","Ecuador","Egypt","England",
  "France","Germany","Ghana","Iran","Iraq","Ivory Coast","Japan","Jordan",
  "Mexico","Morocco","Netherlands","New Zealand","Nigeria","Norway","Panama",
  "Paraguay","Portugal","Saudi Arabia","Senegal","South Korea","Spain","Sweden",
  "Switzerland","Tunisia","Turkey","United States","Uruguay","Uzbekistan",
  "Algeria", "New Zealand", "DR Congo", "Curaçao"
)

# ---- 1. FIFA rankings ----
cat("Pulling FIFA rankings...\n")
fifa <- tryCatch({
  worldfootballR::get_international_results(
    team = "Argentina", type = "rankings"
  )
}, error = function(e) NULL)

# fallback: use approximate FIFA points from recent public data
# (worldfootballR FIFA ranking function may vary by version)
# We'll use a manual lookup for the top WC teams as a reliable fallback
fifa_rankings <- tribble(
  ~team,              ~fifa_rank, ~fifa_points,
  "Argentina",         1,          1893,
  "France",            2,          1876,
  "Spain",             3,          1840,
  "England",           4,          1812,
  "Brazil",            5,          1782,
  "Portugal",          6,          1764,
  "Belgium",           7,          1751,
  "Netherlands",       8,          1740,
  "Germany",           9,          1724,
  "Colombia",         10,          1711,
  "Morocco",          14,          1675,
  "Japan",            17,          1651,
  "Uruguay",          20,          1632,
  "Mexico",           16,          1660,
  "Switzerland",      19,          1636,
  "Croatia",          11,          1706,
  "Senegal",          18,          1644,
  "United States",    13,          1682,
  "Iran",             21,          1621,
  "Ecuador",          44,          1542,
  "Australia",        25,          1591,
  "Norway",           27,          1581,
  "Austria",          26,          1586,
  "South Korea",      22,          1618,
  "Turkey",           29,          1567,
  "Algeria",          30,          1563,
  "Ivory Coast",      28,          1574,
  "Canada",           43,          1545,
  "Ghana",            55,          1489,
  "Tunisia",          34,          1549,
  "Paraguay",         60,          1468,
  "Egypt",            35,          1545,
  "Saudi Arabia",     56,          1488,
  "Panama",           77,          1418,
  "Jordan",           87,          1392,
  "Uzbekistan",       69,          1443,
  "New Zealand",      91,          1381,
  "Iraq",             58,          1476,
  "Sweden",           24,          1593,
  "Cape Verde",       72,          1430,
  "DR Congo",         48,          1521,
  "Curaçao",         100,          1355,
  "Belgium",           7,          1751
) %>% distinct(team, .keep_all = TRUE)

# ---- 2. Squad market values (Transfermarkt) ----
cat("Pulling Transfermarkt squad values...\n")
squad_vals <- tryCatch({
  worldfootballR::tm_national_team_roster(
    country_name = wc_teams,
    start_year   = 2026
  )
}, error = function(e) {
  cat("  worldfootballR tm_national_team_roster failed, trying alternative...\n")
  NULL
})

if (!is.null(squad_vals) && nrow(squad_vals) > 0) {
  squad_features <- squad_vals %>%
    group_by(country) %>%
    summarise(
      squad_value_m   = sum(as.numeric(gsub("[^0-9.]", "", player_market_value_euro)),
                            na.rm = TRUE) / 1e6,
      squad_size      = n(),
      avg_age         = mean(as.numeric(player_age), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(team = country)
} else {
  # fallback: approximate squad values (€M) from public Transfermarkt data
  cat("  Using approximate squad values fallback...\n")
  squad_features <- tribble(
    ~team,              ~squad_value_m,  ~avg_age,
    "England",           1127,            26.2,
    "France",            1086,            26.8,
    "Brazil",             961,            26.1,
    "Portugal",           912,            28.1,
    "Germany",            895,            25.4,
    "Spain",              884,            25.9,
    "Argentina",          823,            27.3,
    "Netherlands",        719,            26.6,
    "Belgium",            611,            28.9,
    "Colombia",           469,            26.4,
    "Uruguay",            368,            27.1,
    "Mexico",             351,            26.8,
    "Switzerland",        348,            27.2,
    "Croatia",            323,            29.1,
    "United States",      321,            25.1,
    "Japan",              292,            26.3,
    "Morocco",            261,            26.7,
    "Norway",             254,            25.8,
    "Austria",            231,            26.1,
    "Senegal",            221,            26.9,
    "Australia",          201,            27.4,
    "South Korea",        198,            26.5,
    "Turkey",             189,            26.2,
    "Sweden",             183,            27.1,
    "Ecuador",            156,            25.6,
    "Algeria",            142,            27.3,
    "Ivory Coast",        138,            27.1,
    "Iran",               112,            27.8,
    "Tunisia",             98,            27.2,
    "Egypt",               95,            27.6,
    "Ghana",               91,            26.4,
    "Canada",              89,            25.9,
    "Saudi Arabia",        87,            26.1,
    "Iraq",                61,            26.8,
    "Jordan",              52,            27.1,
    "Paraguay",            48,            26.3,
    "Panama",              41,            27.9,
    "Uzbekistan",          39,            25.4,
    "Cape Verde",          35,            27.2,
    "DR Congo",            33,            26.8,
    "New Zealand",         28,            26.1,
    "Curaçao",             21,            27.3
  )
}

# ---- 3. Recent form (last 10 matches from results data) ----
cat("Computing recent form...\n")
raw <- read_csv(RESULTS_URL, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= TODAY, !is.na(home_score), !is.na(away_score))

form <- function(team_name) {
  matches <- raw %>%
    filter(home_team == team_name | away_team == team_name) %>%
    arrange(desc(date)) %>%
    slice(1:10) %>%
    mutate(
      gf     = if_else(home_team == team_name, home_score, away_score),
      ga     = if_else(home_team == team_name, away_score, home_score),
      result = case_when(gf > ga ~ "W", gf == ga ~ "D", TRUE ~ "L")
    )
  tibble(
    team       = team_name,
    form_w     = mean(matches$result == "W"),
    form_d     = mean(matches$result == "D"),
    form_gf    = mean(matches$gf),
    form_ga    = mean(matches$ga),
    form_gd    = mean(matches$gf - matches$ga)
  )
}

form_features <- map_dfr(wc_teams, ~ tryCatch(form(.x),
                          error = function(e) NULL))

# ---- 4. Combine all features ----
team_features <- strength %>%
  filter(team %in% wc_teams) %>%
  select(team, att, def, rating) %>%
  left_join(fifa_rankings  %>% distinct(team, .keep_all = TRUE), by = "team") %>%
  left_join(squad_features %>% distinct(team, .keep_all = TRUE), by = "team") %>%
  left_join(form_features  %>% distinct(team, .keep_all = TRUE), by = "team") %>%
  distinct(team, .keep_all = TRUE) %>%          # safety dedup in case strength has dupes
  mutate(
    log_squad_value = log1p(squad_value_m),
    fifa_rank_inv   = 1 / fifa_rank
  )

cat(sprintf("\nTeam features built for %d teams\n", nrow(team_features)))
cat(sprintf("Missing squad values: %d\n", sum(is.na(team_features$squad_value_m))))
cat(sprintf("Missing FIFA rank: %d\n",    sum(is.na(team_features$fifa_rank))))

# ---- 5. Match-level feature matrix for GBM training ----
# One row per match (qualifiers + WC), features = difference between team features
wc_team_set <- unique(wc_teams)
raw_wc_window <- raw %>%
  filter(date >= as.Date("2022-01-01"),
         home_team %in% wc_team_set,
         away_team %in% wc_team_set) %>%
  mutate(
    result = case_when(
      home_score > away_score ~ "home",
      home_score == away_score ~ "draw",
      TRUE ~ "away"
    ),
    is_wc = as.integer(tournament == "FIFA World Cup")
  )

join_features <- function(df, prefix, team_col) {
  df %>% left_join(team_features %>%
    select(team, att, def, rating, fifa_rank_inv,
           log_squad_value, avg_age, form_w, form_gd) %>%
    rename_with(~ paste0(prefix, .), -team),
    by = setNames("team", team_col))
}

match_features <- raw_wc_window %>%
  join_features("h_", "home_team") %>%
  join_features("a_", "away_team") %>%
  mutate(
    diff_att         = h_att         - a_att,
    diff_def         = h_def         - a_def,
    diff_rating      = h_rating      - a_rating,
    diff_fifa        = h_fifa_rank_inv - a_fifa_rank_inv,
    diff_squad_value = h_log_squad_value - a_log_squad_value,
    diff_age         = h_avg_age     - a_avg_age,
    diff_form_w      = h_form_w      - a_form_w,
    diff_form_gd     = h_form_gd     - a_form_gd
  ) %>%
  filter(!is.na(diff_rating), !is.na(diff_squad_value))

cat(sprintf("Match feature matrix: %d rows x %d cols\n",
            nrow(match_features), ncol(match_features)))
cat(sprintf("Result distribution: %s\n",
            paste(names(table(match_features$result)),
                  table(match_features$result), sep="=", collapse=", ")))

saveRDS(team_features,  "data/processed/team_features.rds")
saveRDS(match_features, "data/processed/match_features.rds")
cat("\nSaved team_features.rds and match_features.rds\n")