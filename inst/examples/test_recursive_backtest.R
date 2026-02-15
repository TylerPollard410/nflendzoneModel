# Recursive Backtest Test Script
# Runs a weekly recursive update for 2024 and 2025 seasons

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(cmdstanr)
library(posterior)
library(arrow)
library(nflreadr)
library(nflendzoneModel)
library(nflendzone)
library(tictoc)

# --- Configuration ---
fit_2023_path <- "artifacts/model-archive/team_strength/bivar_negbinom_2023_eos.rds"
output_dir <- "artifacts/model-archive/recursive_test"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Helper for loading installed package helpers without restarting
# (Since we just installed it, they should be available via namespace)
# source("R/recursive_utils.R") # Uncomment if not using installed package version

# ==============================================================================
# 1. Setup Initial State (End of 2023)
# ==============================================================================
cat("Loading 2023 End-of-Season Fit...\n")
fit_2023 <- readRDS(fit_2023_path)

# Correct the class if needed (sometimes saving/loading R6 objects can be tricky,
# but cmdstanr usually handles it if the CSVs are present.
# Warning: If temporary CSVs are gone, this fails.
# The user's script saved it with save_object(), which usually persists data.
# If it fails, we might need to rely on the fact that we just ran it in the session
# or look for the CSVs?)
#
# Note: In the previous turn, I ran the fit. If the session is fresh, fit_2023
# variable might be gone, but the file exists.
# CmdStanR `save_object` saves the draws and summary, so it does not need CSVs
# for summary/extraction.

teams_data <- nflreadr::load_teams(current = TRUE)
teams <- teams_data$team_abbr
# Create team map for later
team_map <- tibble::tibble(
  team_idx = seq_along(teams),
  team_abbr = teams
)

cat("Extracting Globals...\n")
# Extract fixed hyperparameters (process noise, AR terms)
globals <- extract_recursive_globals(fit_2023)
print(globals)

cat("Extracting Priors (Posterior 2023 EOS)...\n")
# Extract estimated team strengths at end of 2023
priors_2023_eos <- extract_recursive_priors(fit_2023, teams)

# ==============================================================================
# 2. Season Transition (2023 -> 2024)
# ==============================================================================
cat("Applying Season Transition (2023 -> 2024)...\n")
priors_2024_w01 <- transition_season_priors(priors_2023_eos, fit_2023)

# ==============================================================================
# 3. Recursive Loop 2024
# ==============================================================================
cat("Loading 2024 Game Data...\n")
games_2024 <- nflendzone::load_game_data(seasons = 2024) |>
  dplyr::filter(season == 2024)
idx_2024 <- prepare_schedule_indices(games_2024, teams)

# Storage for results
recursive_history <- list()
current_priors <- priors_2024_w01

weeks_to_run <- sort(unique(idx_2024$week))
# Remove weeks with no results yet (if any) or handle incomplete weeks
# We only filter completed games in the loop.

cat("Starting Recursive Loop for 2024...\n")

{
  tictoc::tic("2024 Recursive Backtest")
  for (w in weeks_to_run) {
    cat(sprintf("Processing 2024 Week %d... ", w))

    # A. Filter Data
    week_data <- idx_2024 |>
      dplyr::filter(week == w, !is.na(result))

    if (nrow(week_data) == 0) {
      cat("No games. Skipping.\n")
      next
    }

    # B. Prepare Stan Data
    # Note: team_strength_recursive expects array inputs
    # We construct the list manually because prepare_stan_data is for the full model
    # and heavily entangled with history/seasons.
    # The recursive model is simple: 1 week, N games.

    stan_data <- list(
      N_games = nrow(week_data),
      N_teams = length(teams),
      N_weeks = 1,

      home_team = as.array(week_data$home_idx),
      away_team = as.array(week_data$away_idx),
      week_idx = as.array(rep(1, nrow(week_data))), # Local index 1
      hfa = as.array(week_data$hfa), # Assume neutral sites handle elsewhere, or default 1

      home_score = as.array(week_data$home_score),
      away_score = as.array(week_data$away_score),

      # Priors
      prior_off_mean = current_priors$prior_off_mean,
      prior_off_sd = current_priors$prior_off_sd,
      prior_def_mean = current_priors$prior_def_mean,
      prior_def_sd = current_priors$prior_def_sd,
      prior_team_hfa_mean = current_priors$prior_team_hfa_mean,
      prior_team_hfa_sd = current_priors$prior_team_hfa_sd,
      prior_alpha_log_mean = current_priors$prior_alpha_log_mean,
      prior_alpha_log_sd = current_priors$prior_alpha_log_sd,

      # Globals
      sigma_weekly_off = globals$sigma_weekly_off,
      sigma_weekly_def = globals$sigma_weekly_def,
      phi_weekly_off = globals$phi_weekly_off,
      phi_weekly_def = globals$phi_weekly_def,
      phi_home = globals$phi_home,
      phi_away = globals$phi_away
    )

    # C. Fit Recursive Model
    # We use the installed model via nflendzoneModel::fit_team_strength_model helper?
    # Or direct cmdstanr for speed/custom model name?
    # The helper requires the model name to be in the package.
    fit_w <- nflendzoneModel::fit_team_strength_model(
      stan_data = stan_data,
      model = "team_strength_recursive",
      seed = 52,
      chains = 4, # Parallel chains
      parallel_chains = 4,
      iter_warmup = 500, # Fast warmup
      iter_sampling = 1000,
      refresh = 0 # Silent
    )

    # D. Extract Next Priors (Predicted State for W+1)
    # The model's generated quantities `predicted_*` contain the forecast state
    # (Current + Evolution).

    pred_off <- fit_w$summary("predicted_team_off_strength")
    pred_def <- fit_w$summary("predicted_team_def_strength")
    pred_hfa <- fit_w$summary("predicted_team_hfa") # Usually static copy
    pred_alp <- fit_w$summary("predicted_alpha_log") # Usually static copy

    # Update current_priors for next iteration
    current_priors <- list(
      prior_off_mean = pred_off$mean,
      prior_off_sd = pred_off$sd,
      prior_def_mean = pred_def$mean,
      prior_def_sd = pred_def$sd,
      prior_team_hfa_mean = pred_hfa$mean,
      prior_team_hfa_sd = pred_hfa$sd,
      prior_alpha_log_mean = pred_alp$mean,
      prior_alpha_log_sd = pred_alp$sd
    )

    # Save Summary Artifact
    summary_df <- fit_w$summary(
      variables = c(
        "team_off_strength",
        "team_def_strength",
        "team_hfa",
        "alpha_log"
      )
    ) |>
      dplyr::mutate(season = 2024L, week = w)

    recursive_history[[paste0("2024_W", w)]] <- summary_df
    cat("Done.\n")
  }
  tictoc::toc()
}

# ==============================================================================
# 4. Save 2024 Results
# ==============================================================================
saveRDS(recursive_history, file.path(output_dir, "recursive_history_2024.rds"))


# UPDATED SCRIPT ------
# Recursive Backtest Test Script
# Runs a weekly recursive update for 2024 and 2025 seasons

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(cmdstanr)
library(posterior)
library(arrow)
library(nflreadr)
library(nflendzoneModel)
library(nflendzone)
library(tictoc)

# --- Configuration ---
fit_2023_path <- "artifacts/model-archive/team_strength/bivar_negbinom_2023_eos.rds"
output_dir <- "artifacts/model-archive/recursive_test"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. Setup Initial State (End of 2023)
# ==============================================================================
cat("Loading 2023 End-of-Season Fit...\n")
fit_2023 <- readRDS(fit_2023_path)

teams_data <- nflreadr::load_teams(current = TRUE)
teams <- teams_data$team_abbr
# Create team map for later
team_map <- tibble::tibble(
  team_idx = seq_along(teams),
  team_abbr = teams
)

cat("Extracting Globals...\n")
globals <- extract_recursive_globals(fit_2023)
print(globals)

cat("Extracting Priors (Posterior 2023 EOS)...\n")
priors_2023_eos <- extract_recursive_priors(fit_2023, teams)
print(priors_2023_eos)

# ==============================================================================
# 2. Season Transition (2023 -> 2024)
# ==============================================================================
cat("Applying Season Transition (2023 -> 2024)...\n")
priors_2024_w01 <- transition_season_priors(priors_2023_eos, fit_2023)
print(priors_2024_w01)

# ==============================================================================
# 3. Recursive Loop 2024
# ==============================================================================
cat("Loading 2024 Game Data...\n")
games_2024 <- nflendzone::load_game_data(seasons = 2024) |>
  dplyr::filter(season == 2024)
idx_2024 <- prepare_schedule_indices(games_2024, teams)

recursive_history <- list()
current_priors <- priors_2024_w01

weeks_to_run <- sort(unique(idx_2024$week))

cat("Starting Recursive Loop for 2024...\n")

{
  tictoc::tic("2024 Recursive Backtest")
  for (w in weeks_to_run) {
    cat(sprintf("Processing 2024 Week %d... ", w))

    # A. Filter Data
    week_data <- idx_2024 |>
      dplyr::filter(week == w, !is.na(result))

    if (nrow(week_data) == 0) {
      cat("No games. Skipping.\n")
      next
    }

    # B. Prepare Stan Data
    stan_data <- list(
      N_games = nrow(week_data),
      N_teams = length(teams),
      N_weeks = 1,

      home_team = as.array(week_data$home_idx),
      away_team = as.array(week_data$away_idx),
      week_idx = as.array(rep(1, nrow(week_data))),
      hfa = as.array(week_data$hfa),

      home_score = as.array(week_data$home_score),
      away_score = as.array(week_data$away_score),

      # Priors
      prior_off_mean = current_priors$prior_off_mean,
      prior_off_sd = current_priors$prior_off_sd,
      prior_def_mean = current_priors$prior_def_mean,
      prior_def_sd = current_priors$prior_def_sd,
      prior_team_hfa_mean = current_priors$prior_team_hfa_mean,
      prior_team_hfa_sd = current_priors$prior_team_hfa_sd,
      prior_alpha_log_mean = current_priors$prior_alpha_log_mean,
      prior_alpha_log_sd = current_priors$prior_alpha_log_sd,

      # Globals
      sigma_weekly_off = globals$sigma_weekly_off,
      sigma_weekly_def = globals$sigma_weekly_def,
      phi_weekly_off = globals$phi_weekly_off,
      phi_weekly_def = globals$phi_weekly_def,
      phi_home = globals$phi_home,
      phi_away = globals$phi_away
    )

    # C. Fit Recursive Model
    fit_w <- nflendzoneModel::fit_team_strength_model(
      stan_data = stan_data,
      model = "team_strength_recursive",
      seed = 52,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = 500,
      iter_sampling = 1000,
      refresh = 0
    )

    # D. Extract Estimates (Filtered & Predicted)

    # Define variables of interest
    vars_filtered <- c(
      "team_off_strength",
      "team_def_strength",
      "team_hfa",
      "alpha_log"
    )
    vars_predicted <- paste0("predicted_", vars_filtered)

    # Get summaries using CmdStanR/posterior
    summary_w <- fit_w$summary(variables = c(vars_filtered, vars_predicted)) |>
      dplyr::mutate(
        # Identify type
        type = dplyr::if_else(
          stringr::str_detect(variable, "^predicted_"),
          "predicted",
          "filtered"
        ),
        # 1. Strip prediction prefix
        temp_var = stringr::str_remove(variable, "^predicted_"),
        # 2. Extract strictly the parameter name (before brackets)
        parameter = stringr::str_extract(temp_var, "^[^\\[]+"),
        # 3. Extract indices string (between brackets)
        indices_str = stringr::str_extract(temp_var, "(?<=\\[).*(?=\\])"),

        # Set season and week
        season = 2024L,
        # If predicted, it represents the state for the NEXT week
        week = dplyr::if_else(type == "predicted", w + 1L, w)
      ) |>
      dplyr::select(-temp_var)

    # Store in history
    recursive_history[[paste0("2024_W", w)]] <- summary_w

    # E. Update Priors for Next Loop
    # We use the 'predicted' Means and SDs as priors for W+1

    # Helper to extract vector of stats ordered by team index
    get_prior_vec <- function(df, param_name, stat) {
      df |>
        dplyr::filter(type == "predicted", parameter == param_name) |>
        dplyr::mutate(
          # Extract last number in indices string as team index
          # e.g. "1,5" -> 5; "5" -> 5
          idx = as.integer(stringr::str_extract(indices_str, "\\d+$"))
        ) |>
        dplyr::arrange(idx) |>
        dplyr::pull(.data[[stat]])
    }

    get_prior_scalar <- function(df, param_name, stat) {
      val <- df |>
        dplyr::filter(type == "predicted", parameter == param_name) |>
        dplyr::pull(.data[[stat]])
      if (length(val) == 0) {
        return(0)
      } # Safety
      return(val)
    }

    prior_off_mean <- get_prior_vec(summary_w, "team_off_strength", "mean")
    prior_off_sd <- get_prior_vec(summary_w, "team_off_strength", "sd")

    prior_def_mean <- get_prior_vec(summary_w, "team_def_strength", "mean")
    prior_def_sd <- get_prior_vec(summary_w, "team_def_strength", "sd")

    prior_hfa_mean <- get_prior_vec(summary_w, "team_hfa", "mean")
    prior_hfa_sd <- get_prior_vec(summary_w, "team_hfa", "sd")

    # Scalar alpha
    prior_alpha_mean <- get_prior_scalar(summary_w, "alpha_log", "mean")
    prior_alpha_sd <- get_prior_scalar(summary_w, "alpha_log", "sd")

    current_priors <- list(
      prior_off_mean = prior_off_mean,
      prior_off_sd = prior_off_sd,
      prior_def_mean = prior_def_mean,
      prior_def_sd = prior_def_sd,
      prior_team_hfa_mean = prior_hfa_mean,
      prior_team_hfa_sd = prior_hfa_sd,
      prior_alpha_log_mean = prior_alpha_mean,
      prior_alpha_log_sd = prior_alpha_sd
    )

    cat("Done.\n")
  }
  tictoc::toc()
}


# ==============================================================================
# 4. Clean and Save Results
# ==============================================================================
cat("Processing results into tidy dataframe...\n")

history_df <- dplyr::bind_rows(recursive_history) |>
  tibble::as_tibble()

# Create team mapping
if (!exists("teams")) {
  teams_data <- nflreadr::load_teams(current = TRUE)
  teams <- teams_data$team_abbr
}

team_map <- tibble::tibble(
  team_idx = seq_along(teams),
  team_abbr = teams
)

clean_history_df <- history_df |>
  dplyr::mutate(
    # Extract Team Index from the pre-parsed indices_str
    # logic: if indices_str is "5", team is 5. If "1,5", team is 5.
    team_idx = dplyr::if_else(
      is.na(indices_str),
      NA_integer_,
      as.integer(stringr::str_extract(indices_str, "\\d+$"))
    )
  ) |>
  # Join Team Abbr
  dplyr::left_join(team_map, by = "team_idx") |>
  # Select and Arrange
  dplyr::select(
    season,
    week,
    type,
    parameter,
    team_abbr,
    mean,
    sd,
    q5,
    q95,
    rhat,
    ess_bulk
  ) |>
  dplyr::arrange(season, week, type, parameter, team_abbr)

# Preview Results
print(head(clean_history_df))
print(tail(clean_history_df))

# Check for unmapped team parameters
unmapped_check <- clean_history_df |>
  dplyr::filter(
    is.na(team_abbr),
    stringr::str_detect(parameter, "team") # if param name implies team
  )

if (nrow(unmapped_check) > 0) {
  warning("Some team parameters were not mapped to abbreviations. Checking:")
  print(head(unmapped_check))
} else {
  cat("All team parameters successfully mapped.\n")
}

# Save
saveRDS(
  clean_history_df,
  file.path(output_dir, "recursive_history_2024_clean.rds")
)
cat(
  "Saved cleaned history to:",
  file.path(output_dir, "recursive_history_2024_clean.rds"),
  "\n"
)

# ==============================================================================
# 5. Transition to 2025 (If data exists)
# ==============================================================================
# For 2025, we take the POSTERIOR of 2024 Last Week (not the predicted next week)
# and apply Season Transition.
# However, our loop ended with `current_priors` containing the `predicted` (Weekly evolved)
# state.
# We need to backtrack: Take "Posterior W_Last" -> "Season Transition" -> "Prior 2025 W1".

# Re-extract posterior of the last fit
last_fit <- fit_w
priors_2024_eos <- extract_recursive_priors(last_fit, teams)
priors_2025_w01 <- transition_season_priors(priors_2024_eos, fit_2023) # Use same global params from 2023 fit

# ... (Repeat loop logic for 2025 if needed, or stop here for the test) ...
cat("Recursive test for 2024 complete. Saved history.\n")
