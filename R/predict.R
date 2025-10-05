# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# PREDICTION AND EXTRACTION FUNCTIONS ----
# Functions for extracting predictions and states from fitted models
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

#' Extract Game Predictions from GQ Object
#'
#' @description
#' Extracts posterior predictions for out-of-sample games from a
#' CmdStanGQ object. Returns posterior means and standard deviations
#' for both the mean prediction (mu_pred) and the predictive distribution (y_pred).
#'
#' @param gq CmdStanGQ object from generate_team_predictions()
#' @param schedule_df Schedule dataframe with game information for the OOS games
#'
#' @return Tibble with columns:
#'   \itemize{
#'     \item oos_row: Row number in OOS data
#'     \item game_id: Game identifier (if in schedule_df)
#'     \item week_idx, season: Time indices
#'     \item home_team, away_team: Team identifiers
#'     \item mu_mean, mu_sd: Posterior mean prediction (point spread)
#'     \item y_mean, y_sd: Predictive distribution (includes observation noise)
#'   }
#'
#' @export
#' @family prediction
#'
#' @examples
#' \dontrun{
#' predictions <- extract_game_predictions(
#'   gq = my_gq_fit,
#'   schedule_df = next_week_schedule
#' )
#' }
extract_game_predictions <- function(gq, schedule_df) {
  if (!inherits(gq, "CmdStanGQ")) {
    stop("gq must be a CmdStanGQ object from generate_team_predictions()")
  }

  # Extract mu_pred and y_pred
  mu <- posterior::as_draws_matrix(gq$draws(variables = "mu_pred"))
  y <- posterior::as_draws_matrix(gq$draws(variables = "y_pred"))

  k <- ncol(mu)
  if (k == 0) {
    return(tibble::tibble())
  }

  # Build prediction tibble
  pred_tbl <- tibble::tibble(
    oos_row = seq_len(k),
    mu_mean = colMeans(mu),
    mu_sd = apply(mu, 2, stats::sd),
    y_mean = colMeans(y),
    y_sd = apply(y, 2, stats::sd)
  )

  # Join with schedule info if available
  if (!is.null(schedule_df) && nrow(schedule_df) >= k) {
    schedule_subset <- schedule_df[seq_len(k), ]
    pred_tbl <- pred_tbl |>
      dplyr::mutate(
        game_id = schedule_subset$game_id,
        week_idx = schedule_subset$week_idx,
        season = schedule_subset$season,
        week = schedule_subset$week,
        home_team = schedule_subset$home_team,
        away_team = schedule_subset$away_team
      ) |>
      dplyr::relocate(game_id, season, week, home_team, away_team, .after = oos_row)
  }

  pred_tbl
}

#' Extract Team Strengths from Fit or GQ Object
#'
#' @description
#' Extracts team strength and home field advantage parameters from a
#' fitted model (CmdStanMCMC) or generated quantities (CmdStanGQ) object.
#'
#' @param object CmdStanMCMC or CmdStanGQ object
#' @param teams Character vector of team names/abbreviations in index order
#' @param type Character; "filtered" (from fit) or "predicted" (from GQ).
#'   Default "predicted".
#'
#' @return Tibble with team strength summaries containing:
#'   \itemize{
#'     \item team: Team abbreviation
#'     \item variable: Variable name (e.g., "predicted_team_strength")
#'     \item mean, sd, median: Posterior summaries
#'     \item q5, q95: 90% credible interval bounds
#'   }
#'
#' @export
#' @family prediction
#'
#' @examples
#' \dontrun{
#' # Extract predicted strengths from GQ
#' team_strengths <- extract_team_strengths(
#'   object = my_gq_fit,
#'   teams = nfl_teams,
#'   type = "predicted"
#' )
#'
#' # Extract filtered strengths from fit
#' current_strengths <- extract_team_strengths(
#'   object = my_fit,
#'   teams = nfl_teams,
#'   type = "filtered"
#' )
#' }
extract_team_strengths <- function(object, teams, type = "predicted") {
  if (!inherits(object, c("CmdStanMCMC", "CmdStanGQ"))) {
    stop("object must be a CmdStanMCMC or CmdStanGQ object")
  }

  type <- match.arg(type, c("filtered", "predicted"))

  # Determine variable names based on type
  if (type == "predicted") {
    strength_var <- "predicted_team_strength_next_mean"
    hfa_var <- "predicted_team_hfa_next_mean"
  } else {
    strength_var <- "filtered_team_strength_last"
    hfa_var <- "filtered_team_hfa_last"
  }

  # Extract draws
  tryCatch(
    {
      strength_draws <- object$draws(variables = strength_var)
      hfa_draws <- object$draws(variables = hfa_var)

      # Summarize
      strength_summary <- posterior::summarise_draws(strength_draws)
      hfa_summary <- posterior::summarise_draws(hfa_draws)

      # Combine and add team names
      result <- dplyr::bind_rows(
        strength_summary |> dplyr::mutate(parameter = "strength"),
        hfa_summary |> dplyr::mutate(parameter = "hfa")
      )

      # Extract team index from variable name
      result <- result |>
        dplyr::mutate(
          team_idx = as.integer(gsub(".*\\[([0-9]+)\\].*", "\\1", variable)),
          team = teams[team_idx]
        ) |>
        dplyr::select(team, parameter, mean, sd, median, q5, q95) |>
        dplyr::arrange(parameter, dplyr::desc(mean))

      return(result)
    },
    error = function(e) {
      warning(
        "Could not extract team strengths. Variables may not exist in object.\n",
        "Error: ", e$message
      )
      return(tibble::tibble())
    }
  )
}

#' Extract Full Posterior Draws for Team Strengths
#'
#' @description
#' Extracts the full posterior draws (not just summaries) for team strengths.
#' Useful for custom posterior analysis or visualization.
#'
#' @param gq CmdStanGQ object from generate_team_predictions()
#' @param teams Character vector of team names in index order
#'
#' @return Data frame in tidy format with columns:
#'   \itemize{
#'     \item .chain, .iteration, .draw: MCMC identifiers
#'     \item week_idx: Week index
#'     \item team: Team abbreviation
#'     \item predicted_team_strength: Draw value for strength
#'     \item predicted_team_hfa: Draw value for home field advantage
#'   }
#'
#' @export
#' @family prediction
#'
#' @examples
#' \dontrun{
#' # Get full draws for custom analysis
#' strength_draws <- extract_strength_draws(
#'   gq = my_gq_fit,
#'   teams = nfl_teams
#' )
#'
#' # Use with tidybayes for plotting
#' library(tidybayes)
#' strength_draws %>%
#'   ggplot(aes(x = predicted_team_strength, y = team)) +
#'   stat_halfeye()
#' }
extract_strength_draws <- function(gq, teams) {
  if (!inherits(gq, "CmdStanGQ")) {
    stop("gq must be a CmdStanGQ object")
  }

  # Check if tidybayes is available for spread_draws
  if (!requireNamespace("tidybayes", quietly = TRUE)) {
    stop(
      "Package 'tidybayes' is required for extract_strength_draws(). ",
      "Install with: install.packages('tidybayes')"
    )
  }

  tryCatch(
    {
      gq_rvars <- posterior::as_draws_rvars(gq$draws())

      # Use tidybayes to extract in tidy format
      draws <- gq_rvars |>
        tidybayes::spread_rvars(
          predicted_team_strength[week_idx, team],
          predicted_team_hfa[week_idx, team]
        ) |>
        dplyr::mutate(
          team = teams[team]
        )

      return(draws)
    },
    error = function(e) {
      warning(
        "Could not extract strength draws. ",
        "Ensure GQ includes predicted_team_strength and predicted_team_hfa.\n",
        "Error: ", e$message
      )
      return(tibble::tibble())
    }
  )
}
