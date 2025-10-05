# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# INTERNAL UTILITY FUNCTIONS ----
# Not exported - used internally by package functions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

#' Prepare Schedule with Indices for Stan
#'
#' @description
#' Prepares a complete schedule dataframe with all necessary indices for Stan
#' modeling (team indices, season indices, week indices, etc.). This is needed
#' for preparing GQ data and rolling forward fit data.
#'
#' @param game_data Data frame of NFL games with columns: season, week, home_team,
#'   away_team, location, home_score, away_score, result, total, game_type
#' @param teams Character vector of team abbreviations in canonical order
#' @return Data frame with added index columns for Stan
#' @export
#' @family data-preparation
prepare_schedule_indices <- function(game_data, teams) {
  game_data |>
    dplyr::arrange(season, week) |>
    dplyr::mutate(
      game_idx = dplyr::row_number(),
      season_idx = as.integer(as.factor(season)),
      # Use existing week_seq if available, otherwise create it
      week_idx = if ("week_seq" %in% names(game_data)) {
        as.integer(week_seq)
      } else {
        as.integer(dplyr::dense_rank(paste(season, week, sep = "_")))
      },
      # Season boundary indicators
      fw_season_idx = as.integer(ifelse(week == 1, 1, 0)),
      lw_season_idx = as.integer(ifelse(game_type == "SB", 1, 0)),
      # Team indices
      home_idx = match(home_team, teams),
      away_idx = match(away_team, teams),
      # Home field advantage indicator
      hfa = as.integer(ifelse(location == "Home", 1, 0))
    )
}

#' Get Last Fitted Week from Stan Data
#'
#' @description
#' Internal helper to extract the last week index from a Stan data list.
#'
#' @param fit_stan_data List created for Stan fitting (must contain week_idx)
#' @return Integer - maximum week_idx in the data
#' @keywords internal
#' @noRd
get_last_fitted_week <- function(fit_stan_data) {
  if (!is.list(fit_stan_data) || is.null(fit_stan_data$week_idx)) {
    stop("fit_stan_data must be a list with week_idx element")
  }
  max(as.integer(fit_stan_data$week_idx), na.rm = TRUE)
}

#' Compute Next Week Targets for Prediction
#'
#' @description
#' Computes which week indices to predict based on the last fitted week.
#' Used to determine target weeks for out-of-sample predictions.
#'
#' @param fit_stan_data Training data list used in fit
#' @param horizon Integer number of weeks ahead (default 1)
#' @return Integer vector of global week_idx targets
#' @export
#' @family data-preparation
next_week_targets <- function(fit_stan_data, horizon = 1L) {
  last_w <- get_last_fitted_week(fit_stan_data)
  if (is.null(horizon) || horizon <= 0) {
    return(integer(0))
  }
  seq.int(from = last_w + 1L, length.out = as.integer(horizon))
}

#' Roll Forward Fit Stan Data by Weeks
#'
#' @description
#' Extends Stan fit data by including additional weeks from the schedule.
#' Used for sequential updating when new game results become available.
#'
#' @param fit_stan_data Current Stan data list
#' @param schedule_df Full schedule with indices from prepare_schedule_indices()
#' @param weeks_ahead Number of weeks to extend
#' @return New Stan data list extended by weeks_ahead
#' @export
#' @family data-preparation
roll_forward_fit_stan_data <- function(
  fit_stan_data,
  schedule_df,
  weeks_ahead = 1L
) {
  last_w <- get_last_fitted_week(fit_stan_data)
  target_w <- last_w + as.integer(weeks_ahead)

  df <- schedule_df |>
    dplyr::filter(week_idx <= target_w) |>
    dplyr::arrange(week_idx)

  # Ensure outcomes exist
  if (!all(c("result", "home_score", "away_score", "total") %in% names(df))) {
    stop(
      "schedule_df must include outcome columns: ",
      "result, home_score, away_score, total"
    )
  }

  if (any(is.na(df$result))) {
    stop(
      "Missing outcomes detected in games through week ",
      target_w,
      ". Cannot include future games in fit data."
    )
  }

  stan_df <- df |>
    dplyr::transmute(
      season_idx = as.integer(season_idx),
      week_idx = as.integer(week_idx),
      fw_season_idx = as.integer(fw_season_idx),
      lw_season_idx = as.integer(lw_season_idx),
      home_team = as.integer(home_idx),
      away_team = as.integer(away_idx),
      hfa = as.integer(hfa),
      home_score = as.integer(home_score),
      away_score = as.integer(away_score),
      result = as.integer(result),
      total = as.integer(total)
    )

  N_teams <- if (!is.null(fit_stan_data$N_teams)) {
    as.integer(fit_stan_data$N_teams)
  } else {
    length(unique(c(df$home_idx, df$away_idx)))
  }

  # Use tidybayes::compose_data for clean Stan list creation
  stan_df |>
    tidybayes::compose_data(
      .n_name = tidybayes::n_prefix("N"),
      N_games = nrow(stan_df),
      N_teams = N_teams,
      N_seasons = max(stan_df$season_idx),
      N_weeks = length(unique(stan_df$week_idx))
    )
}

#' Build Future Week Metadata for Multi-Step Forecasts
#'
#' @description
#' Internal helper to create future week metadata arrays for Stan GQ block.
#'
#' @param fit_stan_data List passed to Stan fit
#' @param schedule_df Full schedule with week_idx, season_idx, fw_season_idx
#' @param horizon Number of future weeks to cover
#' @return Named list with N_future_weeks and metadata arrays
#' @keywords internal
#' @noRd
build_future_meta <- function(fit_stan_data, schedule_df, horizon) {
  if (is.null(horizon) || horizon <= 0) {
    return(list(
      N_future_weeks = 0L,
      future_is_first_week = integer(0),
      future_week_to_season = integer(0)
    ))
  }

  last_w <- get_last_fitted_week(fit_stan_data)
  future_weeks <- seq.int(from = last_w + 1L, length.out = horizon)

  fut <- schedule_df |>
    dplyr::filter(week_idx %in% future_weeks) |>
    dplyr::group_by(week_idx) |>
    dplyr::summarise(
      future_is_first_week = as.integer(any(fw_season_idx == 1)),
      future_week_to_season = as.integer(dplyr::first(season_idx)),
      .groups = "drop"
    ) |>
    dplyr::arrange(week_idx)

  # Ensure contiguity and fill any missing rows
  if (nrow(fut) != horizon) {
    fut <- tibble::tibble(week_idx = future_weeks) |>
      dplyr::left_join(fut, by = "week_idx") |>
      tidyr::fill(future_week_to_season, .direction = "down") |>
      dplyr::mutate(
        future_is_first_week = ifelse(
          is.na(future_is_first_week),
          0L,
          future_is_first_week
        ),
        future_week_to_season = as.integer(ifelse(
          is.na(future_week_to_season),
          max(schedule_df$season_idx, na.rm = TRUE),
          future_week_to_season
        ))
      )
  }

  list(
    N_future_weeks = as.integer(horizon),
    future_is_first_week = array(as.integer(fut$future_is_first_week), dim = horizon),
    future_week_to_season = array(as.integer(fut$future_week_to_season), dim = horizon)
  )
}

#' Build OOS Arrays from Schedule Subset
#'
#' @description
#' Internal helper to create out-of-sample game arrays for Stan GQ.
#'
#' @param schedule_df Subset of schedule with games to predict
#' @return Named list with N_oos and oos_* arrays
#' @keywords internal
#' @noRd
build_oos_from_schedule <- function(schedule_df) {
  if (!nrow(schedule_df)) {
    return(list(
      N_oos = 0L,
      oos_home_team = integer(0),
      oos_away_team = integer(0),
      oos_season_idx = integer(0),
      oos_week_idx = integer(0),
      oos_hfa = integer(0)
    ))
  }
  list(
    N_oos = as.integer(nrow(schedule_df)),
    oos_home_team = as.integer(schedule_df$home_idx),
    oos_away_team = as.integer(schedule_df$away_idx),
    oos_season_idx = as.integer(schedule_df$season_idx),
    oos_week_idx = as.integer(schedule_df$week_idx),
    oos_hfa = as.integer(schedule_df$hfa)
  )
}
