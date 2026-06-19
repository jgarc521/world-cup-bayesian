// Dynamic hierarchical negative-binomial model for football match outcomes.
// v3 additions:
//   - mu_wc: separate tournament intercept for World Cup matches, fixing the
//     systematic under-prediction of scoring rate vs qualifier-heavy training data
//   - phi: negative binomial overdispersion
//   - recency weights on likelihood contributions

data {
  int<lower=1> T;
  int<lower=1> G;
  int<lower=1> W;

  array[G] int<lower=1, upper=T> home;
  array[G] int<lower=1, upper=T> away;
  array[G] int<lower=1, upper=W> knot;
  array[G] int<lower=0, upper=1> neutral;
  array[G] int<lower=0, upper=1> is_wc;   // 1 for World Cup matches, 0 otherwise

  array[G] int<lower=0> y_home;
  array[G] int<lower=0> y_away;

  vector<lower=0>[W] dt;
  vector<lower=0>[G] weights;
}

parameters {
  real mu;                  // baseline log scoring rate (qualifiers / friendlies)
  real mu_wc;               // WC-specific intercept shift (expected positive ~0.2-0.4)
  real home_adv;
  real<lower=0> phi;
  real<lower=0> sigma_team;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;

  matrix[T, W] z_att;
  matrix[T, W] z_def;
}

transformed parameters {
  matrix[T, W] att;
  matrix[T, W] def;

  att[, 1] = sigma_team * z_att[, 1];
  def[, 1] = sigma_team * z_def[, 1];

  for (w in 2:W) {
    att[, w] = att[, w - 1] + sigma_att * sqrt(dt[w]) * z_att[, w];
    def[, w] = def[, w - 1] + sigma_def * sqrt(dt[w]) * z_def[, w];
  }
}

model {
  mu         ~ normal(0.1, 0.5);
  mu_wc      ~ normal(0.3, 0.3);   // prior: WC scoring ~exp(0.3)=1.35x higher than baseline
  home_adv   ~ normal(0.2, 0.2);
  phi        ~ normal(0, 5);
  sigma_team ~ normal(0, 0.5);
  sigma_att  ~ normal(0, 0.1);
  sigma_def  ~ normal(0, 0.1);
  to_vector(z_att) ~ std_normal();
  to_vector(z_def) ~ std_normal();

  for (w in 1:W) {
    sum(att[, w]) ~ normal(0, 0.001 * T);
    sum(def[, w]) ~ normal(0, 0.001 * T);
  }

  for (g in 1:G) {
    int h = home[g];
    int a = away[g];
    int w = knot[g];
    real base  = mu + is_wc[g] * mu_wc;
    real eta_h = base + (1 - neutral[g]) * home_adv + att[h, w] - def[a, w];
    real eta_a = base                               + att[a, w] - def[h, w];
    target += weights[g] * neg_binomial_2_log_lpmf(y_home[g] | eta_h, phi);
    target += weights[g] * neg_binomial_2_log_lpmf(y_away[g] | eta_a, phi);
  }
}

generated quantities {
  vector[2 * G] log_lik;
  array[G] int y_home_rep;
  array[G] int y_away_rep;

  for (g in 1:G) {
    int h = home[g];
    int a = away[g];
    int w = knot[g];
    real base  = mu + is_wc[g] * mu_wc;
    real eta_h = base + (1 - neutral[g]) * home_adv + att[h, w] - def[a, w];
    real eta_a = base                               + att[a, w] - def[h, w];

    log_lik[g]     = neg_binomial_2_log_lpmf(y_home[g] | eta_h, phi);
    log_lik[G + g] = neg_binomial_2_log_lpmf(y_away[g] | eta_a, phi);
    y_home_rep[g]  = neg_binomial_2_log_rng(eta_h, phi);
    y_away_rep[g]  = neg_binomial_2_log_rng(eta_a, phi);
  }
}
