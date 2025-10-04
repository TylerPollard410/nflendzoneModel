library(magrittr)
library(tictoc)
library(plotly)
library(smplot2)
library(patchwork)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(Metrics)
library(broom.mixed)
library(tidybayes)

library(nflverse)
library(tidyverse)

# detach("package:nflendzonePipeline",unload = TRUE, force = TRUE)
# install.packages(".", repos = NULL, type = "source")
# pak::pak("TylerPollard410/nflendzone")
library(nflendzonePipeline)
library(nflendzone)

set.seed(52)

source("Model Fitting/off_def_stan/stan_helpers.R")

# 0. Globals ----
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = min(fit_chains, parallel::detectCores() - 1)
fit_warm = 1000
fit_samps = 2500
fit_thin = 1
fit_adapt_delta = 0.90
fit_max_treedepth = 10

# 1. Prepare schedule indices once ----
teams <- load_teams(current = TRUE)$team_abbr
all_seasons <- 2002:get_current_season()
schedule_idx <- prepare_schedule_indices(seasons = all_seasons, teams = teams)

# 2. Compile models ----
file_root <- "Model Fitting/off_def_stan"
fit_path <- file.path(file_root, "mod_fit.stan")
gq_path <- file.path(file_root, "mod_gq.stan")

fit_mod <- cmdstan_model(
  fit_path,
  compile_model_methods = TRUE,
  force_recompile = FALSE,
  pedantic = TRUE
)
gq_mod <- cmdstan_model(
  gq_path,
  compile_model_methods = TRUE,
  force_recompile = FALSE,
  pedantic = TRUE
)

# 3. Load and prepare data ----
fit_stan_data <- create_stan_data(
  before_season = get_current_season()
)
fit_stan_data <- roll_forward_fit_stan_data(
  fit_stan_data,
  schedule_idx,
  weeks_ahead = get_current_week() - 1
)

gq_targets <- next_week_targets(fit_stan_data, horizon = 1L)
gq_stan_data <- prepare_gq_data(
  fit_stan_data,
  schedule_idx,
  targets = gq_targets
)

# 4. Fit model ----
{
  timer <- .print_time(start = TRUE, msg = "Fitting Model")

  fit <- fit_mod$sample(
    data = fit_stan_data,
    seed = fit_seed,
    init = 0,
    sig_figs = fit_sig_figs,
    chains = 4,
    parallel_chains = fit_parallel,
    iter_warmup = fit_warm,
    iter_sampling = fit_samps,
    thin = fit_thin,
    adapt_delta = fit_adapt_delta,
    max_treedepth = fit_max_treedepth
  )

  .print_time(start = FALSE, timer = timer)
}

fit$save_object(
  file = file.path(file_root, paste0("current_fit", gq_targets, ".rds"))
)

# 5. Generate predictions ----
{
  timer <- .print_time(start = TRUE, msg = "Generating Predictions")

  gq <- gq_mod$generate_quantities(
    fitted_params = fit,
    data = gq_stan_data,
    seed = fit_seed,
    sig_figs = fit_sig_figs,
    parallel_chains = fit_parallel
  )

  .print_time(start = FALSE, timer = timer)
}

# 6. Extract and save results ----
fit_draws <- fit$draws()
fit_draws_df <- fit_draws |> as_draws_df()

gq_draws <- gq$draws()
gq_draws_df <- gq_draws |> as_draws_df()
gq_rvars <- gq_draws |> as_draws_rvars()
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

# 7. Make prediction dataframe ----
pred_df2 <- schedule_idx |>
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
  # mutate(
  #   spread_line = ifelse(
  #     game_id == "2025_04_GB_DAL",
  #     -6.5,
  #     spread_line
  #   )
  # ) |>
  left_join(
    gq_strengths,
    by = c("week_idx", "home_team" = "team")
  ) |>
  rename(
    home_strength = predicted_team_strength,
    home_hfa = predicted_team_hfa
  ) |>
  left_join(
    gq_strengths |>
      select(week_idx, team, predicted_team_strength),
    by = c("week_idx", "away_team" = "team")
  ) |>
  rename(
    away_strength = predicted_team_strength
  ) |>
  relocate(
    home_strength,
    away_strength,
    home_hfa,
    .after = spread_line
  ) |>
  mutate(
    mu_pred = home_strength - away_strength + home_hfa * hfa,
    y_pred = rvar_rng(rnorm, 1, mu_pred, sigma_pred),
    .by = game_id
  ) |>
  mutate(
    p_mu_home_cover = Pr(mu_pred > spread_line),
    p_mu_away_cover = Pr(mu_pred < spread_line),
    p_y_home_cover = Pr(y_pred > spread_line),
    p_y_away_cover = Pr(y_pred < spread_line)
  ) |>
  mutate(
    mu_cover = case_when(
      p_mu_home_cover > p_mu_away_cover ~ home_team,
      p_mu_home_cover < p_mu_away_cover ~ away_team,
    ),
    y_cover = case_when(
      p_y_home_cover > p_y_away_cover ~ home_team,
      p_y_home_cover < p_y_away_cover ~ away_team,
    )
  ) |>
  relocate(
    home_team,
    away_team,
    spread_line,
    .before = mu_pred
  )
