# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# WORKFLOW FUNCTIONS ----
# High-level user-facing functions for complete modeling workflows
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

#' Fit NFL Team Strength Model with Predictions
#'
#' @description
#' Main workflow function for fitting the NFL team strength state-space model
#' and optionally generating predictions. This function wraps the complete
#' workflow from data preparation through model fitting and prediction generation.
#'
#' Inspired by `stan_foot()` from the footBayes package, this provides a clean
#' interface for Bayesian NFL modeling using pre-compiled Stan models.
#'
#' @param game_data Data frame of NFL games with required columns:
#'   game_id, season, week, home_team, away_team, location, home_score,
#'   away_score, result, total, game_type
#' @param teams Character vector of team abbreviations in canonical order.
#'   If NULL (default), will be inferred from game_data.
#' @param predict Logical; generate predictions after fitting? (default TRUE)
#' @param target_week Integer; specific week index to predict. If NULL,
#'   predicts the next week after the training data.
#' @param horizon Integer; number of weeks ahead to predict (default 1).
#'   Only used if target_week is NULL.
#' @param schedule_full Full schedule dataframe for predictions. If NULL and
#'   predict = TRUE, uses game_data. Should include future games if predicting
#'   beyond game_data.
#' @param fit_args Named list of arguments to pass to the Stan sampler.
#'   Common options:
#'   \itemize{
#'     \item chains: Number of MCMC chains (default 4)
#'     \item parallel_chains: Chains to run in parallel (default 4)
#'     \item iter_warmup: Warmup iterations (default 1000)
#'     \item iter_sampling: Sampling iterations (default 2000)
#'     \item adapt_delta: Target acceptance rate (default 0.9)
#'     \item max_treedepth: Maximum tree depth (default 10)
#'     \item seed: Random seed (default 52)
#'   }
#' @param gq_args Named list of arguments for generate_quantities().
#'   Typically only needs: parallel_chains, seed.
#' @param keep_fit Logical; keep the full fit object in the result? (default TRUE).
#'   If FALSE, only posterior draws are kept (saves memory). You can still
#'   use predict_team_strength() with the draws later.
#' @param verbose Logical; print progress messages (default TRUE)
#'
#' @return An S3 object of class `nflendzoneFit` with components:
#'   \itemize{
#'     \item fit: CmdStanMCMC object from fitting (if keep_fit = TRUE)
#'     \item draws: Posterior draws array (always included)
#'     \item gq: CmdStanGQ object from predictions (if predict = TRUE)
#'     \item predictions: Tibble of game predictions (if predict = TRUE)
#'     \item team_strengths: Tibble of team strength estimates (if predict = TRUE)
#'     \item stan_data: Stan data list used for fitting
#'     \item gq_data: Stan data list used for GQ (if predict = TRUE)
#'     \item teams: Team abbreviations vector
#'     \item model_type: Character string indicating model type ("team_strength")
#'     \item call: The original function call
#'   }
#'
#'   Note: If keep_fit = FALSE, the fit object is not stored to save memory.
#'   You can still generate predictions later using predict_team_strength()
#'   with the draws component.
#'
#' @export
#' @family workflow
#'
#' @examples
#' \dontrun{
#' library(nflverse)
#'
#' # Load game data (assuming you have helper functions from another package)
#' games <- load_nfl_games(seasons = 2020:2023)
#'
#' # Fit model and predict next week
#' result <- nfl_team_strength(
#'   game_data = games,
#'   predict = TRUE,
#'   fit_args = list(
#'     chains = 4,
#'     iter_sampling = 2000,
#'     seed = 42
#'   )
#' )
#'
#' # Access components
#' result$fit              # CmdStanMCMC fit object
#' result$predictions      # Game predictions
#' result$team_strengths   # Team strength estimates
#'
#' # Plot team strengths
#' library(ggplot2)
#' result$team_strengths %>%
#'   filter(parameter == "strength") %>%
#'   ggplot(aes(x = mean, y = reorder(team, mean))) +
#'   geom_point() +
#'   geom_errorbarh(aes(xmin = q5, xmax = q95))
#' }
nfl_team_strength <- function(
    game_data,
    teams = NULL,
    predict = TRUE,
    target_week = NULL,
    horizon = 1L,
    schedule_full = NULL,
    fit_args = list(),
    gq_args = list(),
    keep_fit = TRUE,
    verbose = TRUE) {
  # Capture call
  match_call <- match.call()

  # Input validation
  required_cols <- c(
    "game_id", "season", "week", "home_team", "away_team",
    "location", "home_score", "away_score", "result", "total", "game_type"
  )
  missing_cols <- setdiff(required_cols, names(game_data))
  if (length(missing_cols) > 0) {
    stop(
      "game_data missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Infer teams if not provided
  if (is.null(teams)) {
    teams <- sort(unique(c(game_data$home_team, game_data$away_team)))
    if (verbose) {
      cat("Inferred", length(teams), "teams from game_data\n")
    }
  }

  # Set default fit arguments
  fit_defaults <- list(
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    adapt_delta = 0.9,
    max_treedepth = 10,
    seed = 52
  )
  fit_args <- utils::modifyList(fit_defaults, fit_args)

  # Set default GQ arguments
  gq_defaults <- list(
    parallel_chains = fit_args$parallel_chains,
    seed = fit_args$seed
  )
  gq_args <- utils::modifyList(gq_defaults, gq_args)

  if (verbose) {
    cat("\n=== NFL Team Strength Model ===\n")
    cat("Preparing data...\n")
  }

  # Prepare Stan data
  stan_data <- prepare_stan_data(
    game_data = game_data,
    teams = teams,
    verbose = verbose
  )

  if (verbose) {
    cat("\nFitting model...\n")
    cat("  Chains:", fit_args$chains, "\n")
    cat("  Sampling iterations:", fit_args$iter_sampling, "\n")
  }

  # Fit model
  fit <- fit_team_strength_model(
    stan_data = stan_data,
    chains = fit_args$chains,
    parallel_chains = fit_args$parallel_chains,
    iter_warmup = fit_args$iter_warmup,
    iter_sampling = fit_args$iter_sampling,
    adapt_delta = fit_args$adapt_delta,
    max_treedepth = fit_args$max_treedepth,
    seed = fit_args$seed
  )

  # Extract draws (always needed for predictions)
  draws <- fit$draws()

  # Initialize result object
  result <- list(
    draws = draws,
    stan_data = stan_data,
    teams = teams,
    model_type = "team_strength",
    call = match_call
  )

  # Only keep fit object if requested (can be very large)
  if (keep_fit) {
    result$fit <- fit
  } else {
    if (verbose) {
      cat("  Fit object discarded (keep_fit = FALSE). Draws retained.\n")
    }
  }

  class(result) <- c("nflendzoneFit", "list")

  # Generate predictions if requested
  if (predict) {
    if (verbose) {
      cat("\nGenerating predictions...\n")
    }

    # Prepare schedule for predictions
    if (is.null(schedule_full)) {
      schedule_full <- game_data
      if (verbose) {
        cat("  Using game_data as schedule\n")
      }
    }

    # Add indices to schedule
    schedule_indexed <- prepare_schedule_indices(schedule_full, teams)

    # Prepare GQ data
    gq_data <- prepare_gq_data(
      fit_stan_data = stan_data,
      schedule_df = schedule_indexed,
      target_weeks = target_week,
      horizon = horizon
    )

    # Generate quantities using the standalone predict function
    # This works whether we kept the fit object or just the draws
    gq <- predict_team_strength(
      draws = if (keep_fit) fit else draws,
      gq_data = gq_data,
      parallel_chains = gq_args$parallel_chains,
      seed = gq_args$seed
    )

    # Extract predictions
    if (gq_data$N_oos > 0) {
      oos_schedule <- schedule_indexed |>
        dplyr::filter(
          week_idx %in% unique(gq_data$oos_week_idx)
        )

      predictions <- extract_game_predictions(
        gq = gq,
        schedule_df = oos_schedule
      )

      if (verbose) {
        cat("  Extracted predictions for", nrow(predictions), "games\n")
      }
    } else {
      predictions <- tibble::tibble()
      if (verbose) {
        cat("  No out-of-sample games to predict\n")
      }
    }

    # Extract team strengths
    team_strengths <- extract_team_strengths(
      object = gq,
      teams = teams,
      type = "predicted"
    )

    # Add to result
    result$gq <- gq
    result$gq_data <- gq_data
    result$predictions <- predictions
    result$team_strengths <- team_strengths
  }

  if (verbose) {
    cat("\n=== Fitting Complete ===\n")
  }

  result
}

#' Print Method for nflendzoneFit Objects
#'
#' @param x An nflendzoneFit object
#' @param ... Additional arguments (unused)
#' @export
print.nflendzoneFit <- function(x, ...) {
  model_name <- switch(
    x$model_type,
    "team_strength" = "Team Strength State-Space Model",
    "bivariate_poisson" = "Bivariate Poisson Model",
    "student_t" = "Student-t Model",
    sprintf("Model: %s", x$model_type)
  )

  cat("nflendzone Model Fit\n")
  cat("====================\n\n")
  cat("Model Type:", model_name, "\n\n")

  cat("Data:\n")
  cat("  Games:", x$stan_data$N_games, "\n")
  cat("  Teams:", x$stan_data$N_teams, "\n")
  cat("  Seasons:", x$stan_data$N_seasons, "\n")
  cat("  Weeks:", x$stan_data$N_weeks, "\n\n")

  cat("Model Fit:\n")
  if (!is.null(x$fit)) {
    fit_summary <- x$fit$summary()
    n_divergences <- sum(x$fit$sampler_diagnostics()[, , "divergent__"])
    cat("  Sampler: CmdStan NUTS\n")
    cat("  Draws:", nrow(x$fit$draws()), "\n")
    cat("  Divergences:", n_divergences, "\n\n")
  }

  if (!is.null(x$predictions)) {
    cat("Predictions:\n")
    cat("  Games:", nrow(x$predictions), "\n")
    cat("  Weeks:", paste(unique(x$predictions$week), collapse = ", "), "\n\n")
  }

  if (!is.null(x$team_strengths)) {
    cat("Team Strengths Available: Yes\n")
    cat("  Top 3 teams by strength:\n")
    top3 <- x$team_strengths |>
      dplyr::filter(parameter == "strength") |>
      dplyr::slice_head(n = 3)
    for (i in seq_len(nrow(top3))) {
      cat(sprintf(
        "    %s: %.2f (90%% CI: [%.2f, %.2f])\n",
        top3$team[i],
        top3$mean[i],
        top3$q5[i],
        top3$q95[i]
      ))
    }
  }

  invisible(x)
}

#' Summary Method for nflendzoneFit Objects
#'
#' @param object An nflendzoneFit object
#' @param ... Additional arguments (unused)
#' @export
summary.nflendzoneFit <- function(object, ...) {
  model_name <- switch(
    object$model_type,
    "team_strength" = "Team Strength State-Space Model",
    "bivariate_poisson" = "Bivariate Poisson Model",
    "student_t" = "Student-t Model",
    sprintf("Model: %s", object$model_type)
  )

  cat("nflendzone Model Summary\n")
  cat("========================\n\n")
  cat("Model Type:", model_name, "\n\n")

  # Model diagnostics
  if (!is.null(object$fit)) {
    cat("MCMC Diagnostics:\n")
    diag <- object$fit$diagnostic_summary()
    print(diag)
    cat("\n")
  }

  # Parameter summary
  if (!is.null(object$fit)) {
    cat("Key Parameters:\n")
    param_summary <- object$fit$summary(
      variables = c("sigma_obs", "sigma_weekly", "sigma_carryover")
    )
    print(param_summary)
    cat("\n")
  }

  # Team strengths
  if (!is.null(object$team_strengths)) {
    cat("Team Strengths:\n")
    print(object$team_strengths |> dplyr::filter(parameter == "strength"))
  }

  invisible(object)
}
