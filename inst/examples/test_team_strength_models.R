# CURRENT WEEK FIT - Using nflendzoneModel Package
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
library(tictoc)

library(posterior)
library(tidybayes)
#library(cmdstanr)

library(KFAS)
library(bssm)

library(nflreadr)
library(nflfastR)
library(nflseedR)

library(nflendzoneModel)
library(nflendzonePipeline)
library(nflendzone)

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

#current_season <- 2006
#current_week <- 1

# Load game data
#game_data_full <- nflendzone::load_game_data(seasons = all_seasons)
game_data_full <- nflendzone::load_game_data(
  seasons = (current_season - 3):current_season
)

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
    season < current_season |
      (season == current_season & week < current_week),
    !is.na(result)
  )

# Create Stan data
fit_stan_data <- prepare_stan_data(
  game_data = training_data,
  teams = teams,
  verbose = TRUE
)

# Roll forward to include current season up to current_week - 1
# fit_stan_data <- roll_forward_fit_stan_data(
#   fit_stan_data,
#   schedule_idx,
#   weeks_ahead = current_week - 1
# )

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
fit_warm = 500
fit_samps = 500
fit_thin = 1
fit_adapt_delta = 0.95
fit_max_treedepth = 10

cat("\n=== Fitting Model ===\n")

## 3.1 Fit Model ----
fit_univar_normal <- fit_team_strength_model(
  model = "team_strength_fit",
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

fit_univar_student <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_univar_student",
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

fit_bivar_normal <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_bivar_normal",
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

fit_bivar_poisson <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_bivar_poisson",
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

fit_bivar_negbinom <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_bivar_negbinom",
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

fit_names <- c(
  "univar_normal",
  "univar_student",
  "bivar_normal",
  "bivar_poisson",
  "bivar_negbinom"
)

fit_list <- list(
  fit_univar_normal,
  fit_univar_student,
  fit_bivar_normal,
  fit_bivar_poisson,
  fit_bivar_negbinom
) |>
  set_names(fit_names)

## 3.2 Save Fit Outputs ----
fit_list |>
  imap(
    \(.fit, .name) {
      cat("\n=== Saving Fit Outputs for", .name, "===\n")
      tic()
      .fit$save_object(
        file = paste0("artifacts/model-archive/team_strength/", .name, ".rds")
      )
      toc()
    }
  )

fit_loo <- fit_list |>
  map(\(.fit) .fit$loo())
fit_loo |> loo::loo_compare()
fit_loo |> loo::loo_model_weights()

# ============================================================================ #
# 4. Generate Predictions ----
# ============================================================================ #

# cat("\n=== Generating Predictions ===\n")

# gq <- predict_team_strength(
#   draws = fit,
#   gq_data = gq_stan_data,
#   parallel_chains = fit_parallel,
#   seed = fit_seed,
#   sig_figs = fit_sig_figs
# )

# gq_files <- gq$save_output_files(
#   dir = "artifacts/model-archive/team_strength",
#   basename = "team_strength_gq",
#   timestamp = FALSE,
#   random = FALSE
# )

# ============================================================================ #
# 5. Extract Prediction Data ----
# ============================================================================ #

# Get OOS games
oos_games <- schedule_idx |>
  filter(week_idx %in% gq_targets)

# Extract predictions
# predictions <- extract_game_predictions(
#   gq = gq,
#   schedule_df = oos_games
# )

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

## Draws ----
# Extract team strengths with rvars
#fit_draws <- fit$draws()
#fit_rvars <- as_draws_rvars(fit_draws)

fit_rvars_list <- fit_list |>
  map(
    \(.fit) {
      .fit$draws() |>
        as_draws_rvars()
    }
  )

# gq_draws <- gq$draws()
# gq_rvars <- as_draws_rvars(gq_draws)

## Hyperparameters + helpers (apples-to-apples) ----
# Robust summariser (avoid quantile helper issues)
summarize_hparams <- function(draws) {
  posterior::summarize_draws(draws, "mean", "sd", "median", "mad") |>
    dplyr::select(variable, mean, sd, median, mad)
}

has_var <- function(draws, var) {
  any(stringr::str_detect(
    posterior::variables(draws),
    paste0("^", var, "(\\\[|$)")
  ))
}

only_elem_cols <- function(df, base) {
  nm <- names(df)
  nm[stringr::str_detect(nm, paste0("^", base, "\\\\[\\d+\\\\]$"))]
}

extract_hyperparams <- function(fit, model_label) {
  dr <- fit$draws()
  vars <- c(
    # League/Team HFA process
    "phi_league_hfa",
    "sigma_league_hfa_innovation",
    "league_hfa_init",
    "sigma_team_hfa",
    # Strength init scales
    "sigma_team_strength_init",
    "sigma_team_off_init",
    "sigma_team_def_init",
    # Weekly persistence + scales
    "phi_weekly_team_strength_innovation",
    "sigma_weekly_team_strength_innovation",
    "phi_weekly_off",
    "phi_weekly_def",
    "sigma_weekly_off_innov",
    "sigma_weekly_def_innov",
    # Season persistence + scales
    "phi_season_team_strength_innovation",
    "sigma_season_team_strength_innovation",
    "phi_season_off",
    "phi_season_def",
    "sigma_season_off_innov",
    "sigma_season_def_innov",
    # Observation/intercepts
    "nu_obs",
    "sigma_obs", # univar student
    "alpha_score_points", # bivar normal
    "alpha_score", # bivar poisson (log-scale)
    "alpha_score_raw",
    "log_phi_home",
    "log_phi_away",
    "log_sigma_eps_week" # negbinom
  )
  present <- vars[purrr::map_lgl(vars, ~ has_var(dr, .x))]
  if (length(present) == 0) {
    return(dplyr::tibble(
      model = model_label,
      variable = character(),
      mean = numeric()
    ))
  }
  dr |>
    posterior::subset_draws(variables = present) |>
    summarize_hparams() |>
    dplyr::mutate(model = model_label, .before = 1)
}

fit_hyperparams <- fit_list |>
  purrr::imap(~ extract_hyperparams(.x, .y)) |>
  dplyr::bind_rows() |>
  dplyr::mutate(model = factor(model, levels = fit_names)) |>
  dplyr::arrange(variable, model)

fit_strengths_list <- fit_rvars_list |>
  purrr::map(\(x) {
    keep_at(x, stringr::str_subset(names(x), "filtered|predicted"))
  })

possible_exprs <- c(
  "filtered_team_strength[team]",
  "filtered_team_off_strength[team]",
  "filtered_team_def_strength[team]",
  "filtered_team_hfa[team]",
  "filtered_league_hfa",
  "predicted_team_strength[team]",
  "predicted_team_off_strength[team]",
  "predicted_team_def_strength[team]",
  "predicted_team_hfa[team]",
  "predicted_league_hfa"
)

# fit_strengths_list |>
#   imap(
#     \(.rvars, .name) {
#       rvars_expr <- str_subset(
#         possible_exprs,
#         str_c(names(.rvars), collapse = "|")
#       )
#       .rvars |>
#         spread_rvars(
#           enquote(rvars_expr)
#         ) |>
# mutate(
#   model = .name,
#   season = current_season,
#   team = teams[team],
#   .before = 1
# )
#   }
# )

fit_strengths <- fit_strengths_list |>
  purrr::imap(
    \(.rvars, .name) {
      if (stringr::str_detect(.name, "univar")) {
        .rvars |>
          spread_rvars(
            filtered_team_strength[team],
            filtered_team_hfa[team],
            filtered_league_hfa[season],
            predicted_team_strength[team],
            predicted_team_hfa[team],
            predicted_league_hfa[season]
          ) |>
          dplyr::mutate(
            model = .name,
            season = current_season,
            team = teams[team],
            .before = 1
          )
      } else {
        .rvars |>
          spread_rvars(
            filtered_team_off_strength[team],
            filtered_team_def_strength[team],
            filtered_team_hfa[team],
            filtered_league_hfa[season],
            predicted_team_off_strength[team],
            predicted_team_def_strength[team],
            predicted_team_hfa[team],
            predicted_league_hfa[season]
          ) |>
          dplyr::mutate(
            filtered_team_strength = filtered_team_off_strength +
              filtered_team_def_strength,
            predicted_team_strength = predicted_team_off_strength +
              predicted_team_def_strength,
            model = .name,
            season = current_season,
            team = teams[team],
            .before = 1
          )
      }
    }
  ) |>
  list_rbind() |>
  dplyr::mutate(model = factor(model, levels = fit_names)) |>
  dplyr::select(
    model,
    season,
    team,
    contains("filtered"),
    contains("predicted")
  ) |>
  dplyr::relocate(
    "filtered_team_off_strength",
    "filtered_team_def_strength",
    .before = filtered_team_strength
  ) |>
  dplyr::relocate(
    "predicted_team_off_strength",
    "predicted_team_def_strength",
    .before = predicted_team_strength
  ) |>
  dplyr::arrange(team, model)


possible_exprs <- c(
  "filtered_team_strength[team]",
  "filtered_team_off_strength[team]",
  "filtered_team_def_strength[team]",
  "filtered_team_hfa[team]",
  "filtered_league_hfa",
  "predicted_team_strength[team]",
  "predicted_team_off_strength[team]",
  "predicted_team_def_strength[team]",
  "predicted_team_hfa[team]",
  "predicted_league_hfa"
)


comp_list <- fit_rvars_list |>
  keep_at(c("univar_normal", "bivar_negbinom"))
comp_list |>
  map_depth(2, \(x) attr(x, "dims") <- dim(x))

comp_fits <- comp_list |>
  imap(
    \(.rvars, .name) {
      rvars_expr <- str_subset(
        possible_exprs,
        str_c(names(.rvars), collapse = "|")
      ) |>
        rlang::parse_exprs()

      .rvars |>
        spread_rvars(
          !!!rvars_expr
        )
    }
  ) |>
  list_rbind(names_to = "model") |>
  mutate(
    team = teams[team],
    model = factor(model, levels = fit_names),
    filtered_team_strength = if_else(
      model == "bivar_negbinom",
      exp(filtered_team_off_strength + filtered_team_def_strength),
      filtered_team_strength
    ),
    predicted_team_strength = if_else(
      model == "bivar_negbinom",
      exp(predicted_team_off_strength + predicted_team_def_strength),
      filtered_team_strength
    )
  ) |>
  arrange(team, model)
