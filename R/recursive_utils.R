# Recursive Workflow Utilities
# Helpers for efficient weekly updating using `team_strength_recursive.stan`

#' @title Extract Global Hyperparameters for Recursive Updates
#' @description Extracts posterior means of global evolution parameters and scales them
#' to the physical scale expected by the recursive Stan model.
#' @param fit A CmdStanFit object from the full `team_strength_bivar_negbinom` model.
#' @return A list of scalars.
#' @export
extract_recursive_globals <- function(fit) {
  # Hardcoded scales from team_strength_bivar_negbinom.stan
  SCALE_HFA <- 0.1
  SCALE_WEEK <- 0.05
  SCALE_SEAS <- 0.1
  SCALE_PHI <- 0.1

  # Extract summaries
  # We use the mean of the posterior for "Fixed" globals
  vars <- c(
    "phi_weekly_off",
    "phi_weekly_def",
    "sigma_weekly_off_std",
    "sigma_weekly_def_std",
    "log_phi_league_std",
    "sigma_log_phi_std"
  )

  sum_stats <- fit$summary(variables = vars) |>
    dplyr::select(variable, mean) |>
    tibble::deframe()

  # Calculate derived physical parameters
  sigma_weekly_off <- sum_stats[["sigma_weekly_off_std"]] * SCALE_WEEK
  sigma_weekly_def <- sum_stats[["sigma_weekly_def_std"]] * SCALE_WEEK

  # Dispersion (simplify to league average for now, or handle home/away carefully)
  # The recursive model asks for phi_home, phi_away.
  # We'll use the league mean dispersion for simplicity in the fixed-global approximation.
  # log_phi_league = 2.5 + 0.5 * log_phi_league_std
  log_phi_league <- 2.5 + 0.5 * sum_stats[["log_phi_league_std"]]
  phi_val <- exp(log_phi_league)

  list(
    phi_weekly_off = sum_stats[["phi_weekly_off"]],
    phi_weekly_def = sum_stats[["phi_weekly_def"]],
    sigma_weekly_off = sigma_weekly_off,
    sigma_weekly_def = sigma_weekly_def,
    phi_home = phi_val,
    phi_away = phi_val
  )
}

#' @title Extract Last State as Priors
#' @description Extracts the final week's team strengths and the season's HFA/Alpha
#' to serve as priors for the next recursive step.
#' @param fit A CmdStanFit object (either full history or recursive).
#' @param teams Character vector of team abbreviations (to ensure alignment).
#' @return A list of vectors (mean/sd) for recursive priors.
#' @export
extract_recursive_priors <- function(fit, teams) {
  # Helper to get mean/sd for a vector variable, ordered by team
  get_team_stats <- function(var_pattern) {
    fit$summary(variables = var_pattern) |> # Use NULL to scan all, or better: perform regex
      #dplyr::filter(stringr::str_detect(variable, var_pattern)) |>
      dplyr::mutate(
        idx = as.numeric(stringr::str_extract(variable, "\\d+"))
      ) |>
      dplyr::arrange(idx) |>
      dplyr::select(mean, sd)
  }

  # Check if this is a "full" fit (has filtered_*) or "recursive" fit
  # Full fit variables: filtered_team_off_strength[t], filtered_alpha_log
  # Recursive fit variables: team_off_strength[w, t], alpha_log

  meta <- fit$metadata()
  is_full <- "filtered_team_off_strength" %in% meta$stan_variables

  if (is_full) {
    # Full Model Extraction
    off <- get_team_stats("filtered_team_off_strength")
    def <- get_team_stats("filtered_team_def_strength")
    hfa <- get_team_stats("filtered_team_hfa")

    alpha_sum <- fit$summary("filtered_alpha_log")
  } else {
    # Recursive Model Extraction
    # We want the LAST week in the window.
    # team_off_strength is [N_weeks, N_teams]
    # We find max N_weeks
    max_w <- meta$stan_variable_sizes$team_off_strength[1]

    # Regex to capture [max_w, t]
    # e.g. team_off_strength[1, 1]

    # Note: If N_weeks=1, it's just [1, t]

    patt_off <- paste0("team_off_strength[", max_w, ",", seq_along(teams), "]")
    patt_def <- paste0("team_def_strength[", max_w, ",", seq_along(teams), "]")

    # HFA and Alpha are static in recursive window
    patt_hfa <- "team_hfa"

    off <- get_team_stats(patt_off)
    def <- get_team_stats(patt_def)
    hfa <- get_team_stats(patt_hfa)

    alpha_sum <- fit$summary("alpha_log")
  }

  # Validation
  if (nrow(off) != length(teams)) {
    warning("Mismatch in team count for Offense")
  }

  list(
    prior_off_mean = off$mean,
    prior_off_sd = off$sd,
    prior_def_mean = def$mean,
    prior_def_sd = def$sd,
    prior_team_hfa_mean = hfa$mean,
    prior_team_hfa_sd = hfa$sd,
    prior_alpha_log_mean = alpha_sum$mean,
    prior_alpha_log_sd = alpha_sum$sd
  )
}

#' @title Transition State Across Seasons
#' @description Applies the Season-to-Season AR(1) transition to the means and SDs.
#' Used when moving from Week 22 (Super Bowl) to Week 1 of next season.
#' @param priors List of priors (means/sds) from the end of previous season.
#' @param fit_full Full model fit to extract Seasonal AR parameters.
#' @return Updated list of priors.
#' @export
transition_season_priors <- function(priors, fit_full) {
  # Extract Season AR params
  # Note: Requires same scaling logic
  SCALE_SEAS <- 0.1

  sum_stats <- fit_full$summary(
    variables = c(
      "phi_season_off",
      "phi_season_def",
      "sigma_season_off_std",
      "sigma_season_def_std"
    )
  ) |>
    dplyr::select(variable, mean) |>
    tibble::deframe()

  phi_off <- sum_stats[["phi_season_off"]]
  phi_def <- sum_stats[["phi_season_def"]]
  sigma_off <- sum_stats[["sigma_season_off_std"]] * SCALE_SEAS
  sigma_def <- sum_stats[["sigma_season_def_std"]] * SCALE_SEAS

  # AR(1) Moment Propagation:
  # Mean_new = phi * Mean_old
  # Var_new  = phi^2 * Var_old + sigma^2

  new_off_mean <- phi_off * priors$prior_off_mean
  new_off_sd <- sqrt((phi_off^2) * (priors$prior_off_sd^2) + sigma_off^2)

  new_def_mean <- phi_def * priors$prior_def_mean
  new_def_sd <- sqrt((phi_def^2) * (priors$prior_def_sd^2) + sigma_def^2)

  # For HFA and Alpha, we usually treat them as AR(1) as well in the full model.
  # Full model:
  # alpha_log_dev[s] = phi_alpha_log * alpha_log_dev[s - 1] + z...
  # league_hfa[s] = phi_league_hfa * league_hfa[s - 1] + ...

  # Simplification for MVP: Pass them through with inflated variance or similar logic.
  # Let's check coefficients in fit_full.

  extra_stats <- fit_full$summary(
    variables = c(
      "phi_alpha_log",
      "sigma_alpha_log_std",
      "phi_league_hfa",
      "sigma_league_hfa_std"
    )
  ) |>
    dplyr::select(variable, mean) |>
    tibble::deframe()

  # Alpha (intercept)
  # Ideally we need the 'dev' part to apply phi. But we have the absolute level.
  # Approx: Mean_new = Mean_old (Random Walk) or shrink to baseline?
  # Given phi ~ 1 usually, RW is fine.
  phi_alpha <- extra_stats[["phi_alpha_log"]]
  sigma_alpha <- extra_stats[["sigma_alpha_log_std"]] * SCALE_SEAS

  # Note: Applying phi to the *total* alpha is wrong if alpha is centered on 22 (mean ~ 3.1).
  # The model centers alpha_dev on 0.
  # If we only have the total alpha (approx 3.1), applying phi < 1 shrinks it to 0 (very wrong).
  # Strategy: Treat Alpha transition as Random Walk (Mean = Mean, Var += sigma^2)
  # This implies Phi=1 for the total level.

  new_alpha_mean <- priors$prior_alpha_log_mean
  new_alpha_sd <- sqrt(priors$prior_alpha_log_sd^2 + sigma_alpha^2)

  # Team HFA
  # Full model: team_hfa = league + dev.
  # We have the total team_hfa.
  # Strategy: Random Walk with inflated variance.
  sigma_hfa <- extra_stats[["sigma_league_hfa_std"]] * 0.1 # SCALE_HFA

  new_hfa_mean <- priors$prior_team_hfa_mean
  new_hfa_sd <- sqrt(priors$prior_team_hfa_sd^2 + sigma_hfa^2)

  list(
    prior_off_mean = new_off_mean,
    prior_off_sd = new_off_sd,
    prior_def_mean = new_def_mean,
    prior_def_sd = new_def_sd,
    prior_team_hfa_mean = new_hfa_mean,
    prior_team_hfa_sd = new_hfa_sd,
    prior_alpha_log_mean = new_alpha_mean,
    prior_alpha_log_sd = new_alpha_sd
  )
}
