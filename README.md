# World Cup 2026 — Dynamic Bayesian Match Prediction

Predicting 2026 FIFA World Cup outcomes with a dynamic hierarchical Bayesian model
whose team-strength estimates update as matches are played, combined in a stacked
ensemble and propagated through a Monte Carlo of the tournament bracket.

## Approach
- **Core model:** dynamic Baio–Blangiardo — hierarchical Poisson goals model with team
  attack/defense strengths evolving as a state-space random walk (Stan).
- **Ensemble:** Bayesian stacking (`loo`) over the dynamic model, an Elo-style rating,
  and a gradient-boosted model on match features.
- **Updating:** posterior strengths refit as group-stage and knockout results arrive.
- **Simulation:** posterior draws propagated through the 2026 bracket
  (12 groups, 8 best third-placed teams, round of 32).

## Stack
- **R / Stan** — modeling, inference, tournament simulation (`cmdstanr`, `loo`, `worldfootballR`)
- **Python** — data acquisition where it's stronger (`soccerdata`), handed to R as parquet

## Status
In development.
