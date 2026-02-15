# End of Season Fits (2023 & 2024)
# Used to establish baselines for recursive backtesting

library(arrow)
library(dplyr)
library(cmdstanr)
library(posterior)
library(tidybayes)
library(nflreadr)
library(nflendzoneModel)
library(nflendzone)

# Ensure directory exists
dir.create(
  "artifacts/model-archive/team_strength",
  recursive = TRUE,
  showWarnings = FALSE
)

# Helper function to run a specific end-of-season fit
run_eos_fit <- function(target_season, window_size = 5) {
  cat(sprintf("\n\n==================================================\n"))
  cat(sprintf("Running End-of-Season Fit for: %d\n", target_season))
  cat(sprintf(
    "Data Window: %d - %d\n",
    target_season - window_size + 1,
    target_season
  ))
  cat(sprintf("==================================================\n"))

  # 1. Define Seasons
  seasons_seq <- (target_season - window_size + 1):target_season

  # 2. Load Data
  teams_data <- nflreadr::load_teams(current = TRUE)
  teams <- teams_data$team_abbr

  # Load game data for the specific window
  game_data <- nflendzone::load_game_data(seasons = seasons_seq)

  # 3. Prepare Data
  schedule_idx <- prepare_schedule_indices(game_data, teams)

  # Filter for all completed games within the window
  training_data <- schedule_idx |>
    dplyr::filter(
      season %in% seasons_seq,
      !is.na(result)
    )

  cat(sprintf("Training Data: %d games\n", nrow(training_data)))

  fit_stan_data <- prepare_stan_data(
    game_data = training_data,
    teams = teams,
    verbose = TRUE
  )

  # 4. Fit Model
  # Using parameters similar to the example script
  fit <- fit_team_strength_model(
    stan_data = fit_stan_data,
    model = "team_strength_bivar_negbinom",
    seed = 52,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    adapt_delta = 0.95,
    max_treedepth = 10
  )

  # 5. Save Object
  filename <- sprintf(
    "artifacts/model-archive/team_strength/bivar_negbinom_%d_eos.rds",
    target_season
  )
  cat(sprintf("\nSaving model object to: %s\n", filename))
  fit$save_object(file = filename)

  # 6. Diagnostics & Inspection
  cat("\n--- Diagnostics ---\n")
  print(fit$diagnostic_summary())

  cat("\n--- CmdStan Diagnose ---\n")
  fit$cmdstan_diagnose()

  cat("\n--- Parameter Summaries (Key Globals) ---\n")
  # Inspect key evolution parameters
  fit$summary(
    variables = c(
      "phi_weekly_off",
      "phi_weekly_def",
      "sigma_weekly_off_std",
      "sigma_weekly_def_std",
      "phi_season_off",
      "phi_season_def",
      "sigma_season_off_std",
      "sigma_season_def_std",
      "alpha_score_std",
      "log_phi_league_std"
    )
  ) |>
    print()

  return(fit)
}

# --- Execution ---

# 1. Run 2023 End of Season
fit_2023 <- run_eos_fit(target_season = 2023)

# 2. Run 2024 End of Season
fit_2024 <- run_eos_fit(target_season = 2024)
