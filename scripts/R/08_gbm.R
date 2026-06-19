# 08_gbm.R
# Layer 2: fit a multinomial XGBoost model on match features, then combine
# with the Bayesian model via Bayesian stacking (loo-weighted).
#
# Outputs:
#   data/processed/gbm_model.rds        - trained xgboost model
#   data/processed/gbm_probs.rds        - predicted probs for all WC matches
#   data/processed/stacked_probs.rds    - ensemble (stacked) probabilities
#   data/processed/stack_weights.rds    - loo-derived stacking weights
#   report/05_stacking_weights.png      - visual of ensemble weights

library(tidyverse)
library(xgboost)
library(lubridate)

match_features <- readRDS("data/processed/match_features.rds")
team_features  <- readRDS("data/processed/team_features.rds")
cal_pred       <- readRDS("data/processed/calibration_pred.rds")  # Bayesian probs on WC games

set.seed(42)

# -----------------------------------------------------------------------
# 1. Prep training data
# -----------------------------------------------------------------------
FEATURE_COLS <- c("diff_att", "diff_def", "diff_rating",
                  "diff_fifa", "diff_squad_value",
                  "diff_age", "diff_form_w", "diff_form_gd",
                  "is_wc", "neutral")

# outcome: 0=away, 1=draw, 2=home  (xgboost needs 0-indexed integer)
result_map <- c("away" = 0L, "draw" = 1L, "home" = 2L)

train <- match_features %>%
  filter(!is.na(diff_squad_value), !is.na(diff_fifa)) %>%
  mutate(label = result_map[result])

X_train <- train %>% select(all_of(FEATURE_COLS)) %>% as.matrix()
y_train <- train$label

dtrain <- xgb.DMatrix(X_train, label = y_train)

# -----------------------------------------------------------------------
# 2. Cross-validated tuning (5-fold)
# -----------------------------------------------------------------------
cat("Running 5-fold CV to find optimal rounds...\n")
params <- list(
  objective        = "multi:softprob",
  num_class        = 3,
  eval_metric      = "mlogloss",
  eta              = 0.05,
  max_depth        = 3,          # shallower: fewer params, better with n=534
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 5,          # raised: avoids splits on tiny leaf nodes
  gamma            = 0.1
)

cv <- xgb.cv(
  params                = params,
  data                  = dtrain,
  nrounds               = 300,
  nfold                 = 5,
  early_stopping_rounds = 20,
  verbose               = 1,     # show CV progress so we can see what's happening
  print_every_n         = 50
)

# best_iteration can be NULL/0 if early stopping fires on round 1 (model
# can't improve over the null; usually a data or label issue)
best_rounds <- cv$best_iteration
if (is.null(best_rounds) || length(best_rounds) == 0 || best_rounds < 1) {
  best_rounds <- which.min(cv$evaluation_log$test_mlogloss_mean)
  cat(sprintf("early_stopping returned empty; using manual min at round %d\n", best_rounds))
}
if (best_rounds < 1) best_rounds <- 50L   # hard fallback

cat(sprintf("Best rounds: %d | CV log-loss: %.4f\n",
            best_rounds,
            cv$evaluation_log$test_mlogloss_mean[best_rounds]))

# -----------------------------------------------------------------------
# 3. Fit final model
# -----------------------------------------------------------------------
cat("Fitting final GBM...\n")
gbm <- xgb.train(
  params  = params,
  data    = dtrain,
  nrounds = best_rounds,
  verbose = 0
)

# -----------------------------------------------------------------------
# 4. Feature importance
# -----------------------------------------------------------------------
imp <- xgb.importance(model = gbm, feature_names = FEATURE_COLS)
cat("\nFeature importance:\n")
print(imp)

p_imp <- ggplot(imp, aes(Gain, fct_reorder(Feature, Gain))) +
  geom_col(fill = "#1a6eb5", width = 0.7) +
  labs(title = "GBM feature importance (gain)",
       x = "Gain", y = NULL) +
  theme_minimal(base_size = 12)
ggsave("report/05_feature_importance.png", p_imp, width = 7, height = 5, dpi = 150)

# -----------------------------------------------------------------------
# 5. Predicted probabilities on WC group-stage matches (for stacking)
# -----------------------------------------------------------------------
# The calibration_pred has the Bayesian probs on played WC games.
# We need GBM probs on the same matches.

wc_matches <- match_features %>%
  filter(is_wc == 1, !is.na(diff_squad_value))

X_wc <- wc_matches %>%
  select(all_of(FEATURE_COLS)) %>%
  as.matrix()

gbm_raw_vec <- predict(gbm, X_wc)               # flat vector: n*3
gbm_raw     <- matrix(gbm_raw_vec, ncol = 3, byrow = TRUE)   # n x 3: [away, draw, home]
colnames(gbm_raw) <- c("p_away_gbm", "p_draw_gbm", "p_home_gbm")

gbm_probs <- bind_cols(
  wc_matches %>% select(date, home_team, away_team, result),
  as_tibble(gbm_raw)
)

saveRDS(gbm, "data/processed/gbm_model.rds")
saveRDS(gbm_probs, "data/processed/gbm_probs.rds")
cat(sprintf("\nGBM probs saved for %d WC matches\n", nrow(gbm_probs)))

# -----------------------------------------------------------------------
# 6. Bayesian stacking weights via log-score
# -----------------------------------------------------------------------
# We have two models' predicted probabilities on the same played WC matches.
# Stacking weight = argmax sum_i log(w1*p1_i + w2*p2_i) subject to w >= 0, sum=1.
# This is the Yao/Vehtari stacking approach applied to discrete outcomes.

# align Bayesian probs to the same matches
bay_probs <- cal_pred %>%
  select(date, home = home, away = away, result,
         p_home_bay = p_home, p_draw_bay = p_draw, p_away_bay = p_away)

stack_data <- gbm_probs %>%
  inner_join(bay_probs,
             by = c("home_team" = "home", "away_team" = "away", "result")) %>%
  mutate(
    I_home = as.integer(result == "home"),
    I_draw = as.integer(result == "draw"),
    I_away = as.integer(result == "away")
  )

cat(sprintf("Matched %d matches for stacking\n", nrow(stack_data)))

# log-score objective: maximise sum_i log(w*p_bay_i + (1-w)*p_gbm_i)
# optimise over scalar w in [0,1]
log_score <- function(w) {
  p_home <- w * stack_data$p_home_bay + (1 - w) * stack_data$p_home_gbm
  p_draw <- w * stack_data$p_draw_bay + (1 - w) * stack_data$p_draw_gbm
  p_away <- w * stack_data$p_away_bay + (1 - w) * stack_data$p_away_gbm
  ll <- stack_data$I_home * log(p_home + 1e-10) +
        stack_data$I_draw * log(p_draw + 1e-10) +
        stack_data$I_away * log(p_away + 1e-10)
  -sum(ll)   # negative because optim minimises
}

opt <- optimise(log_score, interval = c(0, 1))
w_bay <- opt$minimum
w_gbm <- 1 - w_bay

cat(sprintf("\nStacking weights:\n  Bayesian model : %.3f\n  GBM            : %.3f\n",
            w_bay, w_gbm))

stack_weights <- list(w_bay = w_bay, w_gbm = w_gbm)
saveRDS(stack_weights, "data/processed/stack_weights.rds")

# stacked probabilities on the evaluation matches
stacked <- stack_data %>%
  mutate(
    p_home_stack = w_bay * p_home_bay + w_gbm * p_home_gbm,
    p_draw_stack = w_bay * p_draw_bay + w_gbm * p_draw_gbm,
    p_away_stack = w_bay * p_away_bay + w_gbm * p_away_gbm
  )

# Brier score comparison
brier <- stacked %>%
  mutate(
    bs_bay   = (p_home_bay   - I_home)^2 + (p_draw_bay   - I_draw)^2 + (p_away_bay   - I_away)^2,
    bs_gbm   = (p_home_gbm   - I_home)^2 + (p_draw_gbm   - I_draw)^2 + (p_away_gbm   - I_away)^2,
    bs_stack = (p_home_stack - I_home)^2 + (p_draw_stack - I_draw)^2 + (p_away_stack - I_away)^2
  ) %>%
  summarise(across(starts_with("bs_"), mean))

cat(sprintf("\nBrier scores (lower = better):\n"))
cat(sprintf("  Bayesian model : %.4f\n", brier$bs_bay))
cat(sprintf("  GBM            : %.4f\n", brier$bs_gbm))
cat(sprintf("  Stacked        : %.4f\n", brier$bs_stack))

saveRDS(stacked, "data/processed/stacked_probs.rds")

# -----------------------------------------------------------------------
# 7. Stacking weights plot
# -----------------------------------------------------------------------
weights_df <- tibble(
  model  = c("Bayesian (dynamic)", "GBM (squad/form/ranking)"),
  weight = c(w_bay, w_gbm)
)

p_w <- ggplot(weights_df, aes(weight, fct_reorder(model, weight))) +
  geom_col(fill = c("#1a6eb5", "#e87722"), width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", weight * 100)),
            hjust = -0.1, size = 4) +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Bayesian stacking weights",
       subtitle = "Optimised by log-score on played WC group-stage matches",
       x = "Weight", y = NULL) +
  theme_minimal(base_size = 13)

ggsave("report/06_stacking_weights.png", p_w, width = 7, height = 3.5, dpi = 150)
cat("\nSaved report/06_stacking_weights.png\n")
cat("Done. Next: run 09_simulate_ensemble.R to get stacked tournament probabilities.\n")