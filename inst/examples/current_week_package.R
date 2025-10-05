# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# CURRENT WEEK FIT - Using nflendzoneModel Package
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#
# This script reproduces current_week_fit.R functionality using the
# nflendzoneModel package as an external user would.
#
# Prerequisites:
# - Package installed: devtools::install() or install_github()
# - Required data packages: nflendzone (or nflreadr for testing)

library(posterior)
library(tidybayes)
library(tidyverse)

library(nflverse)
library(nflendzoneModel)
library(nflendzonePipeline)
library(nflendzone)

# Replace these with your actual data loading functions from nflendzone package
# library(nflendzone)
# library(nflendzoneData)

set.seed(52)

# ============================================================================ #
# 0. Load Data
# ============================================================================ #

# Get teams and seasons
# Replace with: teams <- nflendzone::load_teams(current = TRUE)$team_abbr
teams <- nflreadr::load_teams(current = TRUE)$team_abbr
all_seasons <- 2002:nflreadr::get_current_season()
current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

# Load game data
# Replace with: game_data_full <- nflendzone::load_game_data(seasons = all_seasons)
game_data_full <- nflendzone::load_game_data(seasons = all_seasons)
# filter(!is.na(result))
# transmute(
#   game_id,
#   season,
#   week,
#   game_type,
#   home_team,
#   away_team,
#   location = if_else(location == "Home", "Home", "Away"),
#   home_score,
#   away_score,
#   result,
#   total,
#   spread_line,
#   home_spread_prob,
#   away_spread_prob
# )

# ============================================================================ #
# 1. Prepare Data for Fitting
# ============================================================================ #

# Prepare full schedule with indices (needed for GQ)
schedule_idx <- prepare_schedule_indices(
  game_data_full,
  teams
)

# Filter to training data (before current season, only completed games)
training_data <- game_data_full |>
  filter(season < current_season, !is.na(result))

# Create Stan data
fit_stan_data <- prepare_stan_data(
  game_data = training_data,
  teams = teams,
  verbose = TRUE
)

# Roll forward to include current season up to current_week - 1
fit_stan_data <- roll_forward_fit_stan_data(
  fit_stan_data,
  schedule_idx,
  weeks_ahead = current_week - 1
)

# ============================================================================ #
# 2. Prepare GQ Data for Predictions
# ============================================================================ #

# Get targets for next week
gq_targets <- next_week_targets(fit_stan_data, horizon = 1L)

# Prepare GQ data
gq_stan_data <- prepare_gq_data(
  fit_stan_data = fit_stan_data,
  schedule_df = schedule_idx,
  target_weeks = gq_targets
)

# ============================================================================ #
# 3. Fit Model
# ============================================================================ #

# Globals
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = min(fit_chains, parallel::detectCores() - 1)
fit_warm = 1000
fit_samps = 1000
fit_thin = 1
fit_adapt_delta = 0.90
fit_max_treedepth = 10

cat("\n=== Fitting Model ===\n")

fit <- fit_team_strength_model(
  stan_data = fit_stan_data,
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

# Optional: Save fit
# fit$save_object(file = sprintf("current_fit_%d.rds", gq_targets))

# ============================================================================ #
# 4. Generate Predictions
# ============================================================================ #

cat("\n=== Generating Predictions ===\n")

gq <- predict_team_strength(
  draws = fit,
  gq_data = gq_stan_data,
  parallel_chains = fit_parallel,
  seed = fit_seed,
  sig_figs = fit_sig_figs
)

# ============================================================================ #
# 5. Extract Predictions
# ============================================================================ #

# Get OOS games
oos_games <- schedule_idx |>
  filter(week_idx %in% gq_targets)

# Extract predictions
predictions <- extract_game_predictions(
  gq = gq,
  schedule_df = oos_games
)

# ============================================================================ #
# 6. Build Prediction DataFrame (like original current_week_fit.R)
# ============================================================================ #

# Extract team strengths with rvars
gq_draws <- gq$draws()
gq_rvars <- as_draws_rvars(gq_draws)

gq_strengths <- gq_rvars |>
  spread_rvars(
    predicted_team_strength[week_idx, team],
    predicted_team_hfa[week_idx, team],
    sigma_pred
  ) |>
  mutate(
    week_idx = gq_targets[week_idx],
    team = teams[team]
  )

# Build full prediction dataframe
pred_df <- oos_games |>
  filter(week_idx %in% gq_targets) |>
  select(
    game_id,
    season_idx,
    week_idx,
    home_idx,
    away_idx,
    season,
    week,
    home_team,
    away_team,
    hfa,
    home_score,
    away_score,
    result,
    total,
    spread_line,
    home_spread_prob,
    away_spread_prob
  ) |>
  left_join(
    gq_strengths,
    by = c("week_idx", "home_team" = "team")
  ) |>
  rename(
    home_strength = predicted_team_strength,
    home_hfa = predicted_team_hfa
  ) |>
  left_join(
    gq_strengths |> select(week_idx, team, predicted_team_strength),
    by = c("week_idx", "away_team" = "team")
  ) |>
  rename(
    away_strength = predicted_team_strength
  ) |>
  relocate(home_strength, away_strength, home_hfa, .after = spread_line) |>
  mutate(
    # Predicted point differential
    mu_pred = home_strength - away_strength + home_hfa * hfa,
    # Predictive distribution with observation noise
    y_pred = rvar_rng(rnorm, 1, mu_pred, sigma_pred),
    .by = game_id
  ) |>
  mutate(
    # Cover probabilities
    p_mu_home_cover = Pr(mu_pred > spread_line),
    p_mu_away_cover = Pr(mu_pred < spread_line),
    p_y_home_cover = Pr(y_pred > spread_line),
    p_y_away_cover = Pr(y_pred < spread_line)
  ) |>
  mutate(
    # Predicted covers
    mu_cover = case_when(
      p_mu_home_cover > p_mu_away_cover ~ home_team,
      p_mu_home_cover < p_mu_away_cover ~ away_team,
      TRUE ~ NA_character_
    ),
    y_cover = case_when(
      p_y_home_cover > p_y_away_cover ~ home_team,
      p_y_home_cover < p_y_away_cover ~ away_team,
      TRUE ~ NA_character_
    )
  ) |>
  relocate(home_team, away_team, spread_line, .before = mu_pred)

# ============================================================================ #
# 7. Display Results
# ============================================================================ #

cat("\n=== Predictions ===\n")
print(pred_df, n = 20)

# Optional: Save predictions
# saveRDS(pred_df, sprintf("predictions_week_%d.rds", current_week))

cat("\n=== Complete ===\n")
