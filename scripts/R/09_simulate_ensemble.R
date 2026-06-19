# 09_simulate_ensemble.R
# Tournament simulator using the stacked ensemble probabilities.
# For each match the outcome probabilities come from:
#   w_bay * Bayesian(att, def, mu, mu_wc) + w_gbm * GBM(squad, form, ranking, ...)
#
# Structure mirrors 03_simulate.R but replaces the raw Poisson draw with a
# stacked probability draw. Knockout matches still simulate scorelines for
# the "did it go to penalties" realism, but the win probability uses the ensemble.
#
# Outputs:
#   data/processed/progression_ensemble.rds
#   report/07_ensemble_win_probs.png
#   report/08_model_comparison.png   (Bayesian vs ensemble side by side)

library(tidyverse)
library(xgboost)
library(lubridate)

RESULTS_URL <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"
N_SIMS      <- 2000
PENALTY_BIAS <- 0.5

stan_data     <- readRDS("data/processed/stan_data.rds")
team_lookup   <- readRDS("data/processed/team_lookup.rds")
post          <- readRDS("data/processed/posterior_slim.rds")
team_features <- readRDS("data/processed/team_features.rds")
gbm           <- readRDS("data/processed/gbm_model.rds")
stack_weights <- readRDS("data/processed/stack_weights.rds")
progression_bay <- readRDS("data/processed/progression.rds")

W  <- stan_data$W
Tn <- nrow(team_lookup)
id <- setNames(team_lookup$team_id, team_lookup$team)

w_bay <- stack_weights$w_bay
w_gbm <- stack_weights$w_gbm

cat(sprintf("Stacking weights — Bayesian: %.3f | GBM: %.3f\n", w_bay, w_gbm))

# ---- posterior matrices ----
att_mat <- matrix(0, nrow(post), Tn)
def_mat <- matrix(0, nrow(post), Tn)
for (t in 1:Tn) {
  att_mat[, t] <- post[[sprintf("att[%d,%d]", t, W)]]
  def_mat[, t] <- post[[sprintf("def[%d,%d]", t, W)]]
}
mu_vec    <- post$mu
mu_wc_vec <- post$mu_wc
n_draw    <- nrow(post)

# ---- feature lookup for GBM ----
FEATURE_COLS <- c("diff_att", "diff_def", "diff_rating",
                  "diff_fifa", "diff_squad_value",
                  "diff_age", "diff_form_w", "diff_form_gd",
                  "is_wc", "neutral")

tf <- team_features %>%
  select(team, att, def, rating, fifa_rank_inv,
         log_squad_value, avg_age, form_w, form_gd)

gbm_match_probs <- function(h, a, is_wc_flag = 1L, neutral_flag = 1L) {
  hf <- tf %>% filter(team == h)
  af <- tf %>% filter(team == a)
  if (nrow(hf) == 0 || nrow(af) == 0) return(NULL)
  feats <- matrix(c(
    hf$att         - af$att,
    hf$def         - af$def,
    hf$rating      - af$rating,
    hf$fifa_rank_inv - af$fifa_rank_inv,
    hf$log_squad_value - af$log_squad_value,
    hf$avg_age     - af$avg_age,
    hf$form_w      - af$form_w,
    hf$form_gd     - af$form_gd,
    is_wc_flag, neutral_flag
  ), nrow = 1, dimnames = list(NULL, FEATURE_COLS))
  raw <- predict(gbm, xgb.DMatrix(feats))   # flat: away, draw, home
  list(p_away = raw[1], p_draw = raw[2], p_home = raw[3])
}

# ---- stacked win prob for a match (home, away) ----
stacked_probs <- function(d, h, a, is_wc_flag = 1L, neutral_flag = 1L) {
  # Bayesian component
  base  <- mu_vec[d] + mu_wc_vec[d] * is_wc_flag
  lam_h <- exp(base + att_mat[d, id[h]] - def_mat[d, id[a]])
  lam_a <- exp(base + att_mat[d, id[a]] - def_mat[d, id[h]])
  # simulate many scorelines for home/draw/away split
  gh <- rpois(200, lam_h); ga <- rpois(200, lam_a)
  p_home_bay <- mean(gh > ga)
  p_draw_bay <- mean(gh == ga)
  p_away_bay <- mean(gh < ga)

  # GBM component
  gbm_p <- gbm_match_probs(h, a, is_wc_flag, neutral_flag)
  if (is.null(gbm_p)) {
    # fallback to pure Bayesian if team not in features
    return(list(p_home = p_home_bay, p_draw = p_draw_bay, p_away = p_away_bay,
                lam_h = lam_h[1], lam_a = lam_a[1]))
  }

  list(
    p_home = w_bay * p_home_bay + w_gbm * gbm_p$p_home,
    p_draw = w_bay * p_draw_bay + w_gbm * gbm_p$p_draw,
    p_away = w_bay * p_away_bay + w_gbm * gbm_p$p_away,
    lam_h  = mean(lam_h),
    lam_a  = mean(lam_a)
  )
}

# ---- simulate a group-stage match ----
sim_score_ensemble <- function(d, h, a) {
  sp <- stacked_probs(d, h, a)
  r  <- sample(c("home","draw","away"), 1,
               prob = c(sp$p_home, sp$p_draw, sp$p_away))
  if (r == "home") {
    hg <- rpois(1, sp$lam_h); ag <- max(0L, hg - 1L - rpois(1, 0.5))
  } else if (r == "away") {
    ag <- rpois(1, sp$lam_a); hg <- max(0L, ag - 1L - rpois(1, 0.5))
  } else {
    g  <- rpois(1, (sp$lam_h + sp$lam_a) / 2); hg <- g; ag <- g
  }
  c(as.integer(hg), as.integer(ag))
}

# ---- knockout match ----
koff_ensemble <- function(d, a, b) {
  sp <- stacked_probs(d, a, b)
  r  <- sample(c("home","draw","away"), 1,
               prob = c(sp$p_home, sp$p_draw, sp$p_away))
  if (r == "home") return(a)
  if (r == "away") return(b)
  # draw -> ET/penalties
  p_a <- PENALTY_BIAS * sp$p_home / (sp$p_home + sp$p_away) +
         (1 - PENALTY_BIAS) * 0.5
  if (runif(1) < p_a) a else b
}

# ---- reconstruct groups ----
anchors <- c(A="Mexico", B="Canada", C="Brazil", D="United States",
             E="Germany", F="Netherlands", G="Belgium", H="Spain",
             I="France", J="Argentina", K="Portugal", L="England")

wc <- read_csv(RESULTS_URL, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(tournament == "FIFA World Cup",
         date >= as.Date("2026-06-01"), date <= as.Date("2026-06-27"))

group_teams <- map(anchors, function(a) {
  opp <- wc %>% filter(home_team == a | away_team == a) %>%
    mutate(o = if_else(home_team == a, away_team, home_team)) %>%
    pull(o) %>% unique()
  c(a, opp)
})

group_matches <- map(group_teams, function(ts) {
  wc %>% filter(home_team %in% ts, away_team %in% ts) %>%
    transmute(home_team, away_team,
              hg = as.integer(home_score), ag = as.integer(away_score))
})

all_wc_teams <- unlist(group_teams, use.names = FALSE)

# ---- third-place bracket ----
third_elig <- list(
  "74"=c("A","B","C","D","F"), "77"=c("C","D","F","G","H"),
  "79"=c("C","E","F","H","I"), "80"=c("E","H","I","J","K"),
  "81"=c("B","E","F","I","J"), "82"=c("A","E","H","I","J"),
  "85"=c("E","F","G","I","J"), "87"=c("D","E","I","J","L")
)

assign_thirds <- function(qual) {
  slots <- names(third_elig)
  opts  <- lapply(third_elig, function(e) intersect(e, qual))
  ord   <- slots[order(lengths(opts))]
  res   <- setNames(rep(NA_character_, length(slots)), slots)
  used  <- character(0)
  rec   <- function(i) {
    if (i > length(ord)) return(TRUE)
    s <- ord[i]
    for (g in setdiff(opts[[s]], used)) {
      res[[s]] <<- g; used <<- c(used, g)
      if (rec(i + 1)) return(TRUE)
      res[[s]] <<- NA; used <<- setdiff(used, g)
    }
    FALSE
  }
  rec(1); res
}

standings <- function(d, gm, teams) {
  tbl <- tibble(team = teams, pts = 0, gf = 0, ga = 0)
  acc <- function(tbl, tm, gf, ga, pts) {
    i <- match(tm, tbl$team)
    tbl$gf[i] <- tbl$gf[i] + gf
    tbl$ga[i] <- tbl$ga[i] + ga
    tbl$pts[i] <- tbl$pts[i] + pts
    tbl
  }
  for (k in seq_len(nrow(gm))) {
    h <- gm$home_team[k]; a <- gm$away_team[k]
    hg <- gm$hg[k]; ag <- gm$ag[k]
    if (is.na(hg)) {
      sc <- sim_score_ensemble(d, h, a); hg <- sc[1]; ag <- sc[2]
    }
    ph <- if (hg > ag) 3 else if (hg == ag) 1 else 0
    pa <- if (ag > hg) 3 else if (hg == ag) 1 else 0
    tbl <- acc(tbl, h, hg, ag, ph)
    tbl <- acc(tbl, a, ag, hg, pa)
  }
  tbl %>% mutate(gd = gf - ga) %>%
    arrange(desc(pts), desc(gd), desc(gf))
}

play_once <- function(d) {
  winners <- character(); runners <- character()
  thirds  <- tibble()
  for (lt in names(anchors)) {
    s <- standings(d, group_matches[[lt]], group_teams[[lt]])
    winners[lt] <- s$team[1]; runners[lt] <- s$team[2]
    thirds <- bind_rows(thirds,
                        tibble(group = lt, team = s$team[3],
                               pts = s$pts[3], gd = s$gd[3], gf = s$gf[3]))
  }
  thirds <- thirds %>% arrange(desc(pts), desc(gd), desc(gf))
  qual3  <- thirds %>% slice(1:8)
  slot   <- assign_thirds(qual3$group)
  t3     <- setNames(qual3$team, qual3$group)
  T3     <- function(s) t3[[ slot[[s]] ]]

  w <- character()
  w["73"]  <- koff_ensemble(d, runners["A"],   runners["B"])
  w["74"]  <- koff_ensemble(d, winners["E"],   T3("74"))
  w["75"]  <- koff_ensemble(d, winners["F"],   runners["C"])
  w["76"]  <- koff_ensemble(d, winners["C"],   runners["F"])
  w["77"]  <- koff_ensemble(d, winners["I"],   T3("77"))
  w["78"]  <- koff_ensemble(d, runners["E"],   runners["I"])
  w["79"]  <- koff_ensemble(d, winners["A"],   T3("79"))
  w["80"]  <- koff_ensemble(d, winners["L"],   T3("80"))
  w["81"]  <- koff_ensemble(d, winners["D"],   T3("81"))
  w["82"]  <- koff_ensemble(d, winners["G"],   T3("82"))
  w["83"]  <- koff_ensemble(d, runners["K"],   runners["L"])
  w["84"]  <- koff_ensemble(d, winners["H"],   runners["J"])
  w["85"]  <- koff_ensemble(d, winners["B"],   T3("85"))
  w["86"]  <- koff_ensemble(d, winners["J"],   runners["H"])
  w["87"]  <- koff_ensemble(d, winners["K"],   T3("87"))
  w["88"]  <- koff_ensemble(d, runners["D"],   runners["G"])
  r32 <- c(winners, runners, qual3$team)

  w["89"]  <- koff_ensemble(d, w["74"], w["77"])
  w["90"]  <- koff_ensemble(d, w["73"], w["75"])
  w["91"]  <- koff_ensemble(d, w["76"], w["78"])
  w["92"]  <- koff_ensemble(d, w["79"], w["80"])
  w["93"]  <- koff_ensemble(d, w["83"], w["84"])
  w["94"]  <- koff_ensemble(d, w["81"], w["82"])
  w["95"]  <- koff_ensemble(d, w["86"], w["88"])
  w["96"]  <- koff_ensemble(d, w["85"], w["87"])

  w["97"]  <- koff_ensemble(d, w["89"], w["90"])
  w["98"]  <- koff_ensemble(d, w["93"], w["94"])
  w["99"]  <- koff_ensemble(d, w["91"], w["92"])
  w["100"] <- koff_ensemble(d, w["95"], w["96"])

  sf1   <- koff_ensemble(d, w["97"], w["98"])
  sf2   <- koff_ensemble(d, w["99"], w["100"])
  champ <- koff_ensemble(d, sf1, sf2)

  list(r32   = unname(r32),
       r16   = unname(w[as.character(73:88)]),
       qf    = unname(w[as.character(89:96)]),
       sf    = unname(w[as.character(97:100)]),
       final = c(sf1, sf2),
       champ = champ)
}

# ---- run simulations ----
cat(sprintf("Simulating %d tournaments (ensemble)...\n", N_SIMS))
count <- function() setNames(rep(0, length(all_wc_teams)), all_wc_teams)
hit <- list(R32=count(), R16=count(), QF=count(),
            SF=count(), Final=count(), Win=count())
add <- function(slot, teams) hit[[slot]][teams] <<- hit[[slot]][teams] + 1

for (s in 1:N_SIMS) {
  if (s %% 500 == 0) cat(sprintf("  sim %d / %d\n", s, N_SIMS))
  d <- sample.int(n_draw, 1)
  r <- play_once(d)
  add("R32", r$r32); add("R16", r$r16); add("QF", r$qf)
  add("SF",  r$sf);  add("Final", r$final); add("Win", r$champ)
}

progression_ens <- tibble(
  team    = all_wc_teams,
  P_R32   = hit$R32   / N_SIMS,
  P_R16   = hit$R16   / N_SIMS,
  P_QF    = hit$QF    / N_SIMS,
  P_SF    = hit$SF    / N_SIMS,
  P_Final = hit$Final / N_SIMS,
  P_Win   = hit$Win   / N_SIMS
) %>% arrange(desc(P_Win))

cat("\nEnsemble progression (top 20):\n")
print(head(progression_ens, 20), n = 20)

saveRDS(progression_ens, "data/processed/progression_ensemble.rds")

# -----------------------------------------------------------------------
# Plot 1: Ensemble win probabilities
# -----------------------------------------------------------------------
wc_teams_vec <- unlist(group_teams, use.names = FALSE)

top20_ens <- progression_ens %>%
  filter(team %in% wc_teams_vec) %>%
  arrange(desc(P_Win)) %>%
  slice(1:20) %>%
  mutate(team = fct_reorder(team, P_Win))

p1 <- ggplot(top20_ens, aes(P_Win, team)) +
  geom_col(fill = "#e87722", width = 0.7) +
  geom_text(aes(label = scales::percent(P_Win, accuracy = 0.1)),
            hjust = -0.1, size = 3.2, colour = "grey30") +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "2026 World Cup — Ensemble win probabilities",
       subtitle = sprintf("Bayesian (%.0f%%) + GBM (%.0f%%) stacked ensemble · 2,000 simulations",
                          w_bay * 100, w_gbm * 100),
       x = "P(Win tournament)", y = NULL,
       caption  = sprintf("Model snapshot: %s", Sys.Date())) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor  = element_blank(),
        plot.title        = element_text(face = "bold"),
        plot.subtitle     = element_text(colour = "grey40", size = 10))

ggsave("report/07_ensemble_win_probs.png", p1, width = 8, height = 7, dpi = 150)
cat("Saved report/07_ensemble_win_probs.png\n")

# -----------------------------------------------------------------------
# Plot 2: Bayesian vs Ensemble comparison (top 15)
# -----------------------------------------------------------------------
top15_ens  <- progression_ens %>%
  filter(team %in% wc_teams_vec) %>%
  arrange(desc(P_Win)) %>% slice(1:15) %>% pull(team)

compare <- bind_rows(
  progression_bay %>% filter(team %in% top15_ens) %>%
    mutate(model = "Bayesian only"),
  progression_ens %>% filter(team %in% top15_ens) %>%
    mutate(model = "Stacked ensemble")
) %>%
  mutate(team = factor(team, levels = top15_ens))

p2 <- ggplot(compare, aes(P_Win, team, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Bayesian only" = "#1a6eb5",
                               "Stacked ensemble" = "#e87722"),
                    name = NULL) +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title    = "Bayesian model vs stacked ensemble",
       subtitle = "Squad value + form + ranking added in Layer 2",
       x = "P(Win tournament)", y = NULL,
       caption  = sprintf("Snapshot: %s", Sys.Date())) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold"),
        legend.position  = "bottom")

ggsave("report/08_model_comparison.png", p2, width = 8, height = 7, dpi = 150)
cat("Saved report/08_model_comparison.png\n")
cat("\nDone. Check report/08_model_comparison.png for the Bayesian vs ensemble story.\n")