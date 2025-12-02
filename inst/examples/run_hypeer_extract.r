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
# devtools::install_github("TylerPollard410/nflendzoneModel")

library(arrow)
library(lubridate)
library(piggyback)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)

library(posterior)
library(tidybayes)

library(nflreadr)
library(nflfastR)
library(nflseedR)

library(nflendzoneModel)
library(nflendzonePipeline)
library(nflendzone)

library(KFAS)
library(bssm)
library(tictoc)

set.seed(52)

# ============================================================================ #
# 0. Load Data ----
# ============================================================================ #

# Global variables
github_data_repo <- "TylerPollard410/nflendzoneData"
github_releases_base_url <- paste0(
  "https://github.com/",
  github_data_repo,
  "/releases/download/"
)

# Get teams and seasons
# Replace with: teams <- nflendzone::load_teams(current = TRUE)$team_abbr
teams <- nflreadr::load_teams(current = TRUE)$team_abbr
all_seasons <- 2002:nflreadr::get_current_season()
current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

# Stan fit uses data through this season
fit_through_season <- current_season - 1L

# Load game data
game_data_full <- nflendzone::load_game_data(seasons = all_seasons)

tic(paste0("Total Time for End of Season Fits"))
# Loop through each season to fit and predict
for (fit_season in 2006:current_season) {
  cat(paste0(
    "\n%%%%%%%%%%%%%%% Running for Season ",
    fit_season,
    " %%%%%%%%%%%%%%% \n\n"
  ))
  tic(paste0("Total Time for Season ", fit_season))

  # ============================================================================ #
  # 1. Prepare Data for Fitting ----
  # ============================================================================ #

  # Prepare full schedule with indices (needed for GQ)
  schedule_idx <- prepare_schedule_indices(
    game_data_full,
    teams
  )

  # Filter to training data (before current season, only completed games)
  training_data <- schedule_idx |>
    filter(
      season < fit_season,
      !is.na(result)
    )

  # Create Stan data
  fit_stan_data <- prepare_stan_data(
    game_data = training_data,
    teams = teams,
    verbose = TRUE
  )

  # ============================================================================ #
  # 2. Prepare GQ Data for Predictions ----
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
  # 3. Fit Model ----
  # ============================================================================ #

  # Globals
  fit_seed = 52
  fit_init = 0
  fit_sig_figs = 10
  fit_chains = 4
  fit_parallel = parallel::detectCores()
  fit_warm = 1000
  fit_samps = 1000
  fit_thin = 1
  fit_adapt_delta = 0.95
  fit_max_treedepth = 10

  cat("\n=== Fitting Model ===\n")

  tictoc::tic("Model Fit Time")
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
  tictoc::toc()

  # ============================================================================ #
  # 4. Generate Predictions ----
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
  # 5. Extract Prediction Data ----
  # ============================================================================ #

  # Get OOS games
  oos_games <- schedule_idx |>
    filter(week_idx %in% gq_targets)

  # Extract predictions
  predictions <- extract_game_predictions(
    gq = gq,
    schedule_df = oos_games
  )

  # ID variables
  filter_week_idx <- fit_stan_data$N_weeks
  filter_season <- schedule_idx |>
    filter(week_idx == filter_week_idx) |>
    pull(season) |>
    unique()
  filter_week <- schedule_idx |>
    filter(week_idx == filter_week_idx) |>
    pull(week) |>
    unique()

  predict_week_idx <- gq_targets[1]
  predict_season <- schedule_idx |>
    filter(week_idx == predict_week_idx) |>
    pull(season) |>
    unique()
  predict_week <- schedule_idx |>
    filter(week_idx == predict_week_idx) |>
    pull(week) |>
    unique()

  # ============================================================================ #
  # 6. Extract Team Strengths ----
  # ============================================================================ #

  # Stan fit uses data through this season
  ## Hyperparameter Diagnostics ----
  fit_meta <- fit$metadata()
  fit_hyperparams <- fit$summary(
    variables = str_subset(fit_meta$stan_variables, "phi|sigma")
  ) |>
    mutate(
      season = filter_season,
      week = filter_week,
      week_idx = filter_week_idx,
      .before = 1
    )

  ## Draws ----
  # Extract team strengths with rvars
  fit_draws <- fit$draws()
  fit_rvars <- as_draws_rvars(fit_draws)

  gq_draws <- gq$draws()
  gq_rvars <- as_draws_rvars(gq_draws)

  ## Filtered ----
  ### Strength ----
  filtered_strengths <- gq_rvars |>
    spread_rvars(
      filtered_team_strength[team],
      filtered_team_hfa[team]
    ) |>
    mutate(
      week_idx = filter_week_idx,
      team = teams[team]
    ) |>
    left_join(
      schedule_idx |>
        select(season_idx, week_idx, season, week) |>
        distinct(),
      by = "week_idx"
    ) |>
    relocate(season_idx, week_idx, season, week, .before = 1)

  ### League HFA ----
  filtered_league_hfa <- gq_rvars |>
    spread_rvars(
      filtered_league_hfa
    ) |>
    mutate(
      week_idx = filter_week_idx
    ) |>
    left_join(
      schedule_idx |>
        select(season_idx, week_idx, season, week) |>
        distinct(),
      by = "week_idx"
    ) |>
    relocate(season_idx, week_idx, season, week, .before = 1)

  ### Results ----
  # filtered_result <- schedule_idx |>
  #   filter(week_idx == fit_stan_data$N_weeks) |>
  #   select(
  #     game_idx,
  #     game_id,
  #     season_idx,
  #     week_idx,
  #     season,
  #     week,
  #     hfa,
  #     home_team,
  #     away_team
  #   ) |>
  #   left_join(
  #     fit_rvars |>
  #       spread_rvars(
  #         mu[game_idx],
  #         sigma_obs
  #       ),
  #     by = "game_idx"
  #   ) |>
  #   mutate(
  #     y_obs = rvar_rng(rnorm, 1, mu, sigma_obs),
  #     .by = game_id
  #   ) |>
  #   as_tibble()

  filtered_result <- schedule_idx |>
    filter(week_idx == filter_week_idx) |>
    select(
      game_idx,
      game_id,
      season_idx,
      week_idx,
      season,
      week,
      hfa,
      home_team,
      away_team
    ) |>
    left_join(
      filtered_strengths |>
        rename(
          home_strength = filtered_team_strength,
          home_hfa = filtered_team_hfa
        ),
      by = join_by(season_idx, week_idx, season, week, home_team == team)
    ) |>
    left_join(
      filtered_strengths |>
        select(-filtered_team_hfa) |>
        rename(
          away_strength = filtered_team_strength
        ),
      by = join_by(season_idx, week_idx, season, week, away_team == team)
    ) |>
    mutate(
      sigma = gq_rvars$sigma_pred,
      mu = home_strength - away_strength + home_hfa * hfa,
      y = rvar_rng(rnorm, 1, mu, sigma),
      .by = game_id
    )

  ## Predicted ----
  ### Strength ----
  predicted_strengths <- gq_rvars |>
    spread_rvars(
      predicted_team_strength[week_idx, team],
      predicted_team_hfa[week_idx, team]
    ) |>
    mutate(
      week_idx = gq_targets[week_idx],
      team = teams[team]
    )

  ### League HFA ----
  predicted_league_hfa <- gq_rvars |>
    spread_rvars(
      predicted_league_hfa
    ) |>
    mutate(
      season_idx = gq_stan_data$future_week_to_season[1],
      week_idx = predict_week_idx
    ) |>
    left_join(
      schedule_idx |>
        select(season_idx, week_idx, season, week) |>
        distinct(),
      by = c("season_idx", "week_idx")
    ) |>
    relocate(season_idx, week_idx, season, week, .before = 1)

  ### Results ----
  if (nrow(oos_games) == 0) {
    stop("No out-of-sample games to predict.")
  } else {
    predicted_result <- schedule_idx |>
      filter(week_idx == predict_week_idx) |>
      select(
        game_idx,
        game_id,
        season_idx,
        week_idx,
        season,
        week,
        hfa,
        home_team,
        away_team
      ) |>
      left_join(
        predicted_strengths |>
          rename(
            home_strength = predicted_team_strength,
            home_hfa = predicted_team_hfa
          ),
        by = join_by(week_idx, home_team == team)
      ) |>
      left_join(
        predicted_strengths |>
          select(-predicted_team_hfa) |>
          rename(
            away_strength = predicted_team_strength
          ),
        by = join_by(week_idx, away_team == team)
      ) |>
      mutate(
        sigma = gq_rvars$sigma_pred,
        mu = home_strength - away_strength + home_hfa * hfa,
        y = rvar_rng(rnorm, 1, mu, sigma),
        .by = game_id
      )
  }

  # ============================================================================ #
  # 7. Build Prediction DataFrame ----
  # ============================================================================ #

  # ============================================================================ #
  # 8. Save Output to Release ----
  # ============================================================================ #

  cat("\n=== Save Release ===\n")

  # Create release tags
  model_tags <- c(
    "team_strength_fit_hyperparams",
    "team_strength_filter",
    "league_hfa_filter",
    "result_filter",
    "team_strength_predict",
    "league_hfa_predict",
    "result_predict",
    "team_strength_fit",
    "team_strength_gq"
  )

  # Create new release for each tag (if not exists)
  suppressWarnings({
    purrr::walk(
      model_tags,
      ~ piggyback::pb_new_release(repo = github_data_repo, tag = .x)
    )
  })

  # ---------------------------------------------------------------------------- #
  ## Upload Fit Hyperparameters ----

  upload_model_output(
    data = fit_hyperparams,
    tag = "team_strength_fit_hyperparams",
    season = filter_season,
    week = filter_week,
    week_idx = filter_week_idx,
    repo = github_data_repo
  )
  gc()

  # ---------------------------------------------------------------------------- #
  ## Upload Filtered Estimates ----

  upload_model_output(
    data = filtered_strengths,
    tag = "team_strength_filter",
    season = filter_season,
    week = filter_week,
    week_idx = filter_week_idx,
    repo = github_data_repo
  )
  gc()

  upload_model_output(
    data = filtered_league_hfa,
    tag = "league_hfa_filter",
    season = filter_season,
    week = filter_week,
    week_idx = filter_week_idx,
    repo = github_data_repo
  )
  gc()

  upload_model_output(
    data = filtered_result,
    tag = "result_filter",
    season = filter_season,
    week = filter_week,
    week_idx = filter_week_idx,
    repo = github_data_repo
  )
  gc()

  # ---------------------------------------------------------------------------- #
  ## Upload Predicted Estimates ----

  upload_model_output(
    data = predicted_strengths,
    tag = "team_strength_predict",
    season = predict_season,
    week = predict_week,
    week_idx = predict_week_idx,
    repo = github_data_repo
  )
  gc()

  upload_model_output(
    data = predicted_league_hfa,
    tag = "league_hfa_predict",
    season = predict_season,
    week = predict_week,
    week_idx = predict_week_idx,
    repo = github_data_repo
  )
  gc()

  upload_model_output(
    data = predicted_result,
    tag = "result_predict",
    season = predict_season,
    week = predict_week,
    week_idx = predict_week_idx,
    repo = github_data_repo
  )
  gc()

  # ---------------------------------------------------------------------------- #
  toc()
  cat(
    "\n%%%%%%%%%%%%%%% Completed Season ",
    fit_season,
    " %%%%%%%%%%%%%%% \n\n\n\n\n\n"
  )
}
toc()
