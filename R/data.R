# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# DATA PREPARATION FUNCTIONS ----
# User-facing functions for preparing data for Stan models
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

#' Prepare Stan Data for Team Strength Model
#'
#' @description
#' Prepares NFL game data for the team strength state-space Stan model.
#' This function takes raw game data and converts it into the format required
#' by the Stan models.
#'
#' @param game_data Data frame of NFL games with columns: game_id, season, week,
#'   home_team, away_team, location, home_score, away_score, result, total, game_type
#' @param teams Character vector of team abbreviations in canonical order.
#'   Must match the teams in game_data.
#' @param verbose Logical; print progress messages (default FALSE)
#'
#' @return List suitable for Stan model with elements:
#'   \itemize{
#'     \item N_games: Number of games
#'     \item N_teams: Number of teams
#'     \item N_seasons: Number of seasons
#'     \item N_weeks: Number of unique weeks
#'     \item season_idx: Season indices
#'     \item week_idx: Global week indices
#'     \item home_team, away_team: Team indices
#'     \item hfa: Home field advantage indicators
#'     \item home_score, away_score, result, total: Outcomes
#'     \item fw_season_idx, lw_season_idx: Season boundary indicators
#'   }
#'
#' @export
#' @family data-preparation
#'
#' @examples
#' \dontrun{
#' # Assuming you have game data from nflverse or similar
#' stan_data <- prepare_stan_data(
#'   game_data = my_games,
#'   teams = unique(c(my_games$home_team, my_games$away_team))
#' )
#' }
prepare_stan_data <- function(game_data, teams, verbose = FALSE) {
  if (verbose) {
    cat("Preparing Stan data...\n")
    cat("  Games:", nrow(game_data), "\n")
    cat("  Teams:", length(teams), "\n")
    cat("  Seasons:", paste(range(game_data$season), collapse = "-"), "\n")
  }

  # Prepare indices
  indexed_data <- prepare_schedule_indices(game_data, teams)

  # Select and transform variables for Stan
  stan_df <- indexed_data |>
    dplyr::select(
      season_idx,
      week_idx,
      fw_season_idx,
      lw_season_idx,
      home_team = home_idx,
      away_team = away_idx,
      hfa,
      home_score,
      away_score,
      result,
      total
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.integer)
    )

  # Create Stan data list using tidybayes::compose_data
  stan_data <- stan_df |>
    tidybayes::compose_data(
      .n_name = tidybayes::n_prefix("N"),
      N_games = nrow(stan_df),
      N_teams = length(teams),
      N_seasons = length(unique(indexed_data$season)),
      N_weeks = length(unique(stan_df$week_idx))
    )

  if (verbose) {
    cat("\nStan data summary:\n")
    cat("  N_games:", stan_data$N_games, "\n")
    cat("  N_teams:", stan_data$N_teams, "\n")
    cat("  N_seasons:", stan_data$N_seasons, "\n")
    cat("  N_weeks:", stan_data$N_weeks, "\n")
  }

  stan_data
}

#' Prepare GQ Data for Predictions
#'
#' @description
#' Prepares data for Stan's generated quantities block to make predictions
#' for out-of-sample games.
#'
#' @param fit_stan_data Training data list used in the original fit
#' @param schedule_df Full schedule dataframe with indices (from prepare_schedule_indices)
#' @param target_weeks Integer vector of week indices to predict.
#'   If NULL, uses next week after fit data.
#' @param horizon Integer; number of weeks ahead to predict (default 1).
#'   Only used if target_weeks is NULL.
#'
#' @return Stan data list suitable for generate_quantities() with added:
#'   \itemize{
#'     \item N_oos: Number of out-of-sample games
#'     \item oos_home_team, oos_away_team: OOS team indices
#'     \item oos_season_idx, oos_week_idx: OOS time indices
#'     \item oos_hfa: OOS home field advantage indicators
#'     \item N_future_weeks: Number of future weeks to forecast
#'     \item future_is_first_week, future_week_to_season: Future week metadata
#'   }
#'
#' @export
#' @family data-preparation
#'
#' @examples
#' \dontrun{
#' # Predict next week
#' gq_data <- prepare_gq_data(
#'   fit_stan_data = my_stan_data,
#'   schedule_df = full_schedule
#' )
#'
#' # Predict specific weeks
#' gq_data <- prepare_gq_data(
#'   fit_stan_data = my_stan_data,
#'   schedule_df = full_schedule,
#'   target_weeks = c(250, 251, 252)
#' )
#' }
prepare_gq_data <- function(
    fit_stan_data,
    schedule_df,
    target_weeks = NULL,
    horizon = 1L) {
  # Determine target weeks
  if (is.null(target_weeks)) {
    target_weeks <- next_week_targets(fit_stan_data, horizon = horizon)
  }

  if (is.null(target_weeks) || length(target_weeks) == 0) {
    # No predictions requested
    oos <- build_oos_from_schedule(schedule_df[0, ])
    fut <- build_future_meta(fit_stan_data, schedule_df, horizon = 0L)
    return(c(fit_stan_data, oos, fut))
  }

  # Calculate horizon for future metadata
  last_w <- get_last_fitted_week(fit_stan_data)
  actual_horizon <- max(0L, max(target_weeks) - last_w)

  # Get OOS games
  oos_df <- schedule_df |>
    dplyr::filter(week_idx %in% target_weeks) |>
    dplyr::arrange(week_idx)

  oos <- build_oos_from_schedule(oos_df)
  fut <- build_future_meta(fit_stan_data, schedule_df, horizon = actual_horizon)

  # Combine with fit data
  c(fit_stan_data, oos, fut)
}
