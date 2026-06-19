# 03_simulate.R
# Monte Carlo simulation of the remaining 2026 World Cup over the model posterior.
# Conditions on group games already played; simulates the rest. Encodes the real
# FIFA bracket (matches 73-104) including the 8-best-third-place slotting.

library(tidyverse)
library(lubridate)

RESULTS_URL <- "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"
N_SIMS      <- 2000

stan_data   <- readRDS("data/processed/stan_data.rds")
team_lookup <- readRDS("data/processed/team_lookup.rds")
post        <- readRDS("data/processed/posterior_slim.rds")
W <- stan_data$W
Tn <- nrow(team_lookup)
id <- setNames(team_lookup$team_id, team_lookup$team)

# ---- pre-extract posterior strengths at the final knot (fast numeric indexing) ----
att_mat <- matrix(0, nrow(post), Tn)
def_mat <- matrix(0, nrow(post), Tn)
for (t in 1:Tn) {
  att_mat[, t] <- post[[sprintf("att[%d,%d]", t, W)]]
  def_mat[, t] <- post[[sprintf("def[%d,%d]", t, W)]]
}
mu_vec    <- post$mu
mu_wc_vec <- post$mu_wc   # WC-specific intercept; all simulator matches are WC
n_draw <- nrow(post)

# ---- reconstruct the 12 groups from the fixture data ----
# Anchors = the known pot-1 / host seed in each group (one per group, unambiguous).
anchors <- c(A="Mexico", B="Canada", C="Brazil", D="United States",
             E="Germany", F="Netherlands", G="Belgium", H="Spain",
             I="France", J="Argentina", K="Portugal", L="England")

wc <- read_csv(RESULTS_URL, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(tournament == "FIFA World Cup",
         date >= as.Date("2026-06-01"), date <= as.Date("2026-06-27"))

# each group's four teams = anchor + its three group-stage opponents
group_teams <- map(anchors, function(a) {
  opp <- wc %>% filter(home_team == a | away_team == a) %>%
    mutate(o = if_else(home_team == a, away_team, home_team)) %>% pull(o) %>% unique()
  c(a, opp)
})
stopifnot(all(lengths(group_teams) == 4))

# each group's six matches (played carry scores; upcoming are NA)
group_matches <- map(group_teams, function(ts) {
  wc %>% filter(home_team %in% ts, away_team %in% ts) %>%
    transmute(home_team, away_team,
              hg = as.integer(home_score), ag = as.integer(away_score))
})

all_wc_teams <- unlist(group_teams, use.names = FALSE)
missing <- setdiff(all_wc_teams, names(id))
if (length(missing)) warning("Teams not in posterior (check name match): ",
                             paste(missing, collapse = ", "))

# ---- bracket definition (FIFA official, 2026) ----
# third-place slots and which groups each can draw from
third_elig <- list(
  "74" = c("A","B","C","D","F"), "77" = c("C","D","F","G","H"),
  "79" = c("C","E","F","H","I"), "80" = c("E","H","I","J","K"),
  "81" = c("B","E","F","I","J"), "82" = c("A","E","H","I","J"),
  "85" = c("E","F","G","I","J"), "87" = c("D","E","I","J","L")
)

assign_thirds <- function(qual) {            # qual = 8 group letters whose 3rd qualified
  slots <- names(third_elig)
  opts  <- lapply(third_elig, function(e) intersect(e, qual))
  ord   <- slots[order(lengths(opts))]       # constrain hardest slots first
  res <- setNames(rep(NA_character_, length(slots)), slots); used <- character(0)
  rec <- function(i) {
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

# ---- match engines ----
sim_score <- function(d, h, a) {
  base  <- mu_vec[d] + mu_wc_vec[d]   # all simulated matches are WC
  lam_h <- exp(base + att_mat[d, id[h]] - def_mat[d, id[a]])
  lam_a <- exp(base + att_mat[d, id[a]] - def_mat[d, id[h]])
  c(rpois(1, lam_h), rpois(1, lam_a))
}
koff <- function(d, a, b) {                  # knockout: returns winner (no draws)
  base  <- mu_vec[d] + mu_wc_vec[d]
  lam_a <- exp(base + att_mat[d, id[a]] - def_mat[d, id[b]])
  lam_b <- exp(base + att_mat[d, id[b]] - def_mat[d, id[a]])
  ga <- rpois(1, lam_a); gb <- rpois(1, lam_b)
  if (ga != gb) return(if (ga > gb) a else b)
  # PENALTY_BIAS=1.0: stronger side favoured by lambda ratio (~60-65%)
  # PENALTY_BIAS=0.5: coin flip (historically accurate for WC shootouts)
  # PENALTY_BIAS=0.0: weaker side favoured (sanity check only)
  p_a <- PENALTY_BIAS * lam_a / (lam_a + lam_b) + (1 - PENALTY_BIAS) * 0.5
  if (runif(1) < p_a) a else b
}

standings <- function(d, gm, teams) {
  tbl <- tibble(team = teams, pts = 0, gf = 0, ga = 0)
  acc <- function(tbl, tm, gf, ga, pts) {
    i <- match(tm, tbl$team)
    tbl$gf[i] <- tbl$gf[i] + gf; tbl$ga[i] <- tbl$ga[i] + ga; tbl$pts[i] <- tbl$pts[i] + pts
    tbl
  }
  for (k in seq_len(nrow(gm))) {
    h <- gm$home_team[k]; a <- gm$away_team[k]; hg <- gm$hg[k]; ag <- gm$ag[k]
    if (is.na(hg)) { sc <- sim_score(d, h, a); hg <- sc[1]; ag <- sc[2] }
    ph <- if (hg > ag) 3 else if (hg == ag) 1 else 0
    pa <- if (ag > hg) 3 else if (hg == ag) 1 else 0
    tbl <- acc(tbl, h, hg, ag, ph); tbl <- acc(tbl, a, ag, hg, pa)
  }
  tbl %>% mutate(gd = gf - ga) %>%
    arrange(desc(pts), desc(gd), desc(gf))   # (head-to-head / fair-play tiebreaks omitted)
}

# ---- one full tournament ----
play_once <- function(d) {
  winners <- character(); runners <- character(); thirds <- tibble()
  for (lt in names(anchors)) {
    s <- standings(d, group_matches[[lt]], group_teams[[lt]])
    winners[lt] <- s$team[1]; runners[lt] <- s$team[2]
    thirds <- bind_rows(thirds, tibble(group = lt, team = s$team[3],
                                       pts = s$pts[3], gd = s$gd[3], gf = s$gf[3]))
  }
  thirds <- thirds %>% arrange(desc(pts), desc(gd), desc(gf))
  qual3  <- thirds %>% slice(1:8)            # 8 best third-placed teams
  slot   <- assign_thirds(qual3$group)       # slot -> group letter
  t3 <- setNames(qual3$team, qual3$group)     # group letter -> third team
  T3 <- function(s) t3[[ slot[[s]] ]]         # third team in a given R32 slot

  w <- character()                            # winners by match number
  w["73"] <- koff(d, runners["A"], runners["B"])
  w["74"] <- koff(d, winners["E"], T3("74"))
  w["75"] <- koff(d, winners["F"], runners["C"])
  w["76"] <- koff(d, winners["C"], runners["F"])
  w["77"] <- koff(d, winners["I"], T3("77"))
  w["78"] <- koff(d, runners["E"], runners["I"])
  w["79"] <- koff(d, winners["A"], T3("79"))
  w["80"] <- koff(d, winners["L"], T3("80"))
  w["81"] <- koff(d, winners["D"], T3("81"))
  w["82"] <- koff(d, winners["G"], T3("82"))
  w["83"] <- koff(d, runners["K"], runners["L"])
  w["84"] <- koff(d, winners["H"], runners["J"])
  w["85"] <- koff(d, winners["B"], T3("85"))
  w["86"] <- koff(d, winners["J"], runners["H"])
  w["87"] <- koff(d, winners["K"], T3("87"))
  w["88"] <- koff(d, runners["D"], runners["G"])
  r32 <- c(winners, runners, qual3$team)      # the 32 qualifiers

  w["89"] <- koff(d, w["74"], w["77"]); w["90"] <- koff(d, w["73"], w["75"])
  w["91"] <- koff(d, w["76"], w["78"]); w["92"] <- koff(d, w["79"], w["80"])
  w["93"] <- koff(d, w["83"], w["84"]); w["94"] <- koff(d, w["81"], w["82"])
  w["95"] <- koff(d, w["86"], w["88"]); w["96"] <- koff(d, w["85"], w["87"])
  r16 <- w[as.character(73:88)]

  w["97"] <- koff(d, w["89"], w["90"]); w["98"] <- koff(d, w["93"], w["94"])
  w["99"] <- koff(d, w["91"], w["92"]); w["100"] <- koff(d, w["95"], w["96"])
  qf <- w[as.character(89:96)]

  sf1 <- koff(d, w["97"], w["98"]); sf2 <- koff(d, w["99"], w["100"])
  champ <- koff(d, sf1, sf2)

  list(r32 = unname(r32), r16 = unname(w[as.character(73:88)]),
       qf = unname(w[as.character(89:96)]), sf = unname(w[as.character(97:100)]),
       final = c(sf1, sf2), champ = champ)
}

# ---- sensitivity: run at three PENALTY_BIAS levels ----
# 1.0 = stronger side favoured by lambda ratio (original)
# 0.5 = coin flip (historically accurate for WC shootouts)  <- default going forward
# 0.0 = coin flip with slight underdog tilt (sanity check)
bias_levels <- c(1.0, 0.5)
sensitivity  <- list()

run_sims <- function(bias) {
  PENALTY_BIAS <<- bias
  cat(sprintf("\nSimulating %d tournaments at PENALTY_BIAS=%.1f...\n", N_SIMS, bias))
  count <- function() setNames(rep(0, length(all_wc_teams)), all_wc_teams)
  hit <- list(R32=count(),R16=count(),QF=count(),SF=count(),Final=count(),Win=count())
  add <- function(slot, teams) hit[[slot]][teams] <<- hit[[slot]][teams] + 1
  for (s in 1:N_SIMS) {
    d <- sample.int(n_draw, 1)
    r <- play_once(d)
    add("R32",r$r32); add("R16",r$r16); add("QF",r$qf)
    add("SF",r$sf);   add("Final",r$final); add("Win",r$champ)
  }
  tibble(team=all_wc_teams, penalty_bias=bias,
         P_R32=hit$R32/N_SIMS, P_R16=hit$R16/N_SIMS, P_QF=hit$QF/N_SIMS,
         P_SF=hit$SF/N_SIMS, P_Final=hit$Final/N_SIMS, P_Win=hit$Win/N_SIMS)
}

sensitivity <- map(bias_levels, run_sims) %>% bind_rows()

# ---- sensitivity comparison: top 10 by championship prob at coin-flip setting ----
options(pillar.sigfig = 3)
cat("\n--- Sensitivity: championship probability by penalty assumption ---\n")
sensitivity %>%
  select(team, penalty_bias, P_Win) %>%
  pivot_wider(names_from = penalty_bias, values_from = P_Win,
              names_prefix = "bias_") %>%
  arrange(desc(bias_0.5)) %>%
  head(10) %>%
  print(n = 10)

# ---- use coin-flip as the main result (more realistic) ----
progression <- sensitivity %>%
  filter(penalty_bias == 0.5) %>%
  select(-penalty_bias) %>%
  arrange(desc(P_Win))

cat("\nFull progression table (coin-flip penalties, top 20):\n")
print(head(progression, 20), n = 20)

saveRDS(progression, "data/processed/progression.rds")
cat("\nSaved data/processed/progression.rds\n")
saveRDS(sensitivity, "data/processed/sensitivity.rds")
cat("\nSaved sensitivity.rds\n")