# ============================================================================ #
# CURRENT WEEK / END-OF-SEASON FIT + KFAS IN-SEASON FILTER
# Using nflendzoneModel + CmdStan + KFAS
# ============================================================================ #
#
# This script:
#  1. Performs an end-of-season full Stan fit of the team-strength state-space
#     model using nflendzoneModel.
#  2. Extracts hyperparameters and end-of-season state snapshots, including
#     predicted next-season team strength and HFA components.
#  3. Builds a KFAS-based in-season state-space filter for:
#       - 32 team strengths (dynamic, weekly AR(1))
#       - 1 league HFA (static within season, seasonal AR handled in prior)
#       - 32 team HFA deviations (static within season, deviations from league)
#  4. Produces per-game latent features for betting models:
#       theta_home, theta_away, theta_diff,
#       team_hfa_home, league_hfa,
#       mu_pred_result, sd_pred_result.
#
# Prerequisites:
# - nflendzoneModel, nflendzonePipeline, nflendzone installed
# - CmdStan configured
# - KFAS installed
#
# devtools::install_github("TylerPollard410/nflendzoneModel")
# devtools::install_github("TylerPollard410/nflendzonePipeline")
# devtools::install_github("TylerPollard410/nflendzone")
# ============================================================================ #

# ============================================================================ #
# 0. Libraries & Global Options ----
# ============================================================================ #

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
# 1. Load Game Data & Define Seasons ----
# ============================================================================ #

# Global variables (for reference; not strictly required if using local files)
github_data_repo <- "TylerPollard410/nflendzoneData"
github_releases_base_url <- paste0(
  "https://github.com/",
  github_data_repo,
  "/releases/download/"
)

# Teams (you can swap to nflendzone::load_teams if you prefer)
teams <- nflreadr::load_teams(current = TRUE)$team_abbr

all_seasons <- 2002:nflreadr::get_current_season()
current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

# For reproducible dev / testing
current_season <- 2025
current_week <- 12

# Stan fit uses data through this season
fit_through_season <- current_season - 1L

# Load full game data (all seasons)
game_data_full <- nflendzone::load_game_data(seasons = all_seasons)

# ============================================================================ #
# 2. Prepare Schedule & Stan Data (End-of-Season Fit) ----
# ============================================================================ #

# Prepare full schedule with indices (teams mapped to 1..32)
schedule_idx <- prepare_schedule_indices(
  game_data_full,
  teams
)

# Training data:
#   - all completed games strictly before current_season
training_data <- schedule_idx |>
  filter(
    season < current_season,
    !is.na(result)
  )

# Create Stan data list for team-strength model
fit_stan_data <- prepare_stan_data(
  game_data = training_data,
  teams = teams,
  verbose = TRUE
)

# ============================================================================ #
# 3. Fit Team Strength Model (CmdStan via nflendzoneModel) ----
# ============================================================================ #

fit_seed <- 52
fit_init <- 0
fit_sig_figs <- 10
fit_chains <- 4
fit_parallel <- parallel::detectCores()
fit_warm <- 1000
fit_samps <- 1000
fit_thin <- 1
fit_adapt_delta <- 0.95
fit_max_treedepth <- 10

cat(
  "\n=== Fitting Team Strength Model (through season",
  fit_through_season,
  ") ===\n"
)

tic("Model Fit Time")
fit <- fit_team_strength_model(
  stan_data = fit_stan_data,
  seed = fit_seed,
  init = fit_init,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)
toc()

# -------------------------------------------------------------------------- #
## 3.1 Hyperparameter Diagnostics (phi, sigma, etc.) ----
# -------------------------------------------------------------------------- #

fit_meta <- fit$metadata()
fit_meta$stan_variables

fit_hyperparams <- fit$summary(
  variables = str_subset(fit_meta$stan_variables, "phi|sigma")
)
print(fit_hyperparams)

# Named vector of posterior means for hyperparameters
hyper_means <- fit_hyperparams |>
  select(variable, mean) |>
  tibble::deframe()

cat("\n=== Hyperparameter posterior means ===\n")
print(hyper_means)

# ============================================================================ #
# 4. Extract State Snapshots (Strength & HFA) ----
# ============================================================================ #
#
# We assume team_strength_fit.stan / team_strength_gq.stan define:
#   - filtered_team_strength[N_teams]
#   - filtered_team_hfa[N_teams]        (team-specific HFA for last season)
#   - filtered_league_hfa               (league HFA for last season)
#   - predicted_team_strength[N_teams]  (one-step-ahead; next season start)
#   - predicted_team_hfa[N_teams]       (one-step-ahead; next season start)
#   - predicted_league_hfa              (one-step-ahead; next season start)
#
# Note: predicted_team_hfa likely encodes *full team HFA*; we treat deviations
#       as (team_hfa - league_hfa).
# ============================================================================ #

state_vars <- c(
  "filtered_team_strength",
  "filtered_team_hfa",
  "filtered_league_hfa",
  "predicted_team_strength",
  "predicted_team_hfa",
  "predicted_league_hfa"
)

state_summary <- fit$summary(variables = state_vars)

extract_vector_param <- function(summary_df, prefix, metric) {
  summary_df |>
    filter(str_starts(variable, prefix)) |> #paste0(prefix, "\\["))) |>
    arrange(str_order(variable, numeric = TRUE)) |>
    pull(metric)
}

# filtered_team_strength <- extract_vector_param(
#   state_summary,
#   "filtered_team_strength",
#   "mean"
# )
# filtered_team_hfa <- extract_vector_param(
#   state_summary,
#   "filtered_team_hfa",
#   "mean"
# )
# filtered_league_hfa <- extract_vector_param(
#   state_summary,
#   "filtered_league_hfa",
#   "mean"
# )

# predicted_team_strength <- extract_vector_param(
#   state_summary,
#   "predicted_team_strength",
#   "mean"
# )
# predicted_team_hfa <- extract_vector_param(
#   state_summary,
#   "predicted_team_hfa",
#   "mean"
# )
# predicted_league_hfa <- extract_vector_param(
#   state_summary,
#   "predicted_league_hfa",
#   "mean"
# )

state_summary_means <- state_vars |>
  set_names() |>
  map(\(var_name) extract_vector_param(state_summary, var_name, "mean"))

# Deviations for the next season: team_pred_dev = team_pred - league_pred
predicted_team_hfa_dev <- state_summary_means$predicted_team_hfa -
  state_summary_means$predicted_league_hfa

# Approximate prior sds for HFA states (optional; used in P1)
filtered_team_hfa_sd <- extract_vector_param(
  state_summary,
  "filtered_team_hfa",
  "sd"
)

filtered_league_hfa_sd <- extract_vector_param(
  state_summary,
  "filtered_league_hfa",
  "sd"
)

end_of_season_state <- list(
  season_fit_through = fit_through_season,
  teams = teams,
  hyper_means = hyper_means,
  # filtered_team_strength = filtered_team_strength,
  # filtered_team_hfa = filtered_team_hfa,
  # filtered_league_hfa = filtered_league_hfa,
  # predicted_team_strength = predicted_team_strength,
  # predicted_team_hfa = predicted_team_hfa,
  # predicted_league_hfa = predicted_league_hfa,
  state_summary_means,
  predicted_team_hfa_dev = predicted_team_hfa_dev,
  filtered_team_hfa_sd = filtered_team_hfa_sd,
  filtered_league_hfa_sd = filtered_league_hfa_sd
) |>
  list_flatten()

cat("\n=== Extracted end-of-season state snapshots ===\n")
str(end_of_season_state)

# Optionally save
# saveRDS(
#   end_of_season_state,
#   file = file.path("model_artifacts",
#                    paste0("team_strength_state_through_", fit_through_season, ".rds"))
# )

# ============================================================================ #
# 5. KFAS In-Season Filtering: Strength + Seasonal HFA ----
# ============================================================================ #
#
# Within a single season, to match "refit Stan every week":
#   - Team strengths are dynamic (AR(1) over games/weeks).
#   - League HFA for that season is a static unknown parameter:
#       - prior mean & variance come from last season via seasonal AR(1).
#       - updated as games are observed.
#   - Team HFA deviations for that season are static unknowns centered on
#     league HFA; KFAS updates them as data accumulate.
#
# State vector α_t for a given season:
#
#   α_t = [
#       STR_1(t), ..., STR_32(t),       (dynamic, AR(1))
#       HFA_league(s),                  (static within season)
#       HFA_dev_1(s), ..., HFA_dev_32(s) (static within season)
#   ]
#
# Dimension:
#   m = 32 (strength) + 1 (league HFA) + 32 (team HFA deviations) = 65
#
# Transition:
#   - STR block: AR(1) with phi_week, sigma_week
#   - HFA blocks: identity with zero process noise (static)
#
# Observation for game g at time t (season s):
#
#   result_g = (STR_home(t) - STR_away(t))
#              + HFA_league(s) * I[hfa_g == 1]
#              + HFA_dev_home(s) * I[hfa_g == 1]
#              + eps_g
#
# Z_g encodes this as a 1 x 65 row.
# ============================================================================ #

# -------------------------------------------------------------------------- #
## 5.1 Helper: Build KFAS Model for a Given Season ----
# -------------------------------------------------------------------------- #

build_kfas_model_for_season <- function(
  season_schedule,
  teams,
  hyper_means,
  init_team_strength,
  init_league_hfa,
  init_team_hfa_dev,
  prior_sd_league_hfa,
  prior_sd_team_hfa_dev
) {
  n_teams <- length(teams)
  n_games <- nrow(season_schedule)

  stopifnot(
    length(init_team_strength) == n_teams,
    length(init_team_hfa_dev) == n_teams
  )

  # State index bookkeeping
  idx_str <- seq_len(n_teams)
  idx_league <- n_teams + 1L
  idx_team_hfa <- (n_teams + 2L):(2L * n_teams + 1L)
  m <- 2L * n_teams + 1L # total state dimension

  home_idx <- season_schedule$home_idx
  away_idx <- season_schedule$away_idx
  hfa_ind <- season_schedule$hfa # 1 = true home, 0 = neutral

  # ---------------------------------------------------------------------- #
  # Hyperparameters from Stan
  # ---------------------------------------------------------------------- #

  phi_week <- hyper_means[["phi_weekly_team_strength_innovation"]]
  sigma_week <- hyper_means[["sigma_weekly_team_strength_innovation"]]
  sigma_obs <- hyper_means[["sigma_obs"]]

  # Initial team strength prior scale
  sigma_team_init <- hyper_means[["sigma_team_strength_init"]]
  if (is.na(sigma_team_init)) {
    sigma_team_init <- sigma_week
  }

  # League HFA AR(1) across seasons: for P1 we approximate stationary var
  phi_league <- hyper_means[["phi_league_hfa"]]
  sigma_league_innov <- hyper_means[["sigma_league_hfa_innovation"]]

  # Stationary variance as a fallback if we don't trust filtered sd
  if (!is.na(phi_league) && abs(phi_league) < 1) {
    sigma_league_stationary <- sigma_league_innov / sqrt(1 - phi_league^2)
  } else {
    sigma_league_stationary <- sigma_league_innov
  }

  # Team HFA deviations prior scale
  sigma_team_hfa_prior <- hyper_means[["sigma_team_hfa"]]

  # ---------------------------------------------------------------------- #
  # Initial state mean a1 and covariance P1
  # ---------------------------------------------------------------------- #

  a1 <- numeric(m)
  a1[idx_str] <- init_team_strength
  a1[idx_league] <- init_league_hfa
  a1[idx_team_hfa] <- init_team_hfa_dev

  P1 <- diag(0, m)

  # Team strengths prior variance
  diag(P1)[idx_str] <- sigma_team_init^2

  # League HFA prior variance:
  #   if we have a filtered sd from last season, use that;
  #   else use stationary variance from AR(1) as a fallback.
  if (!is.null(prior_sd_league_hfa) && !is.na(prior_sd_league_hfa)) {
    diag(P1)[idx_league] <- prior_sd_league_hfa^2
  } else {
    diag(P1)[idx_league] <- sigma_league_stationary^2
  }

  # Team HFA deviations prior variance:
  #   if we have filtered sds from last season, use those;
  #   else use sigma_team_hfa as global prior scale.
  if (!is.null(prior_sd_team_hfa_dev)) {
    diag(P1)[idx_team_hfa] <- pmax(prior_sd_team_hfa_dev^2, 1e-6)
  } else {
    diag(P1)[idx_team_hfa] <- sigma_team_hfa_prior^2
  }

  # ---------------------------------------------------------------------- #
  # Observation design Z: 1 x m x n_games
  # ---------------------------------------------------------------------- #

  Z <- array(0, dim = c(1, m, n_games))

  for (g in seq_len(n_games)) {
    z_g <- numeric(m)

    # Strength: home - away
    z_g[idx_str[home_idx[g]]] <- 1
    z_g[idx_str[away_idx[g]]] <- -1

    # HFA contribution if true home
    if (hfa_ind[g] == 1L) {
      z_g[idx_league] <- 1
      z_g[idx_team_hfa[home_idx[g]]] <- 1
    }
    Z[1, , g] <- z_g
  }

  # ---------------------------------------------------------------------- #
  # State transition T and innovation covariance Q
  # ---------------------------------------------------------------------- #

  # Per-game updates ---

  # T <- array(0, dim = c(m, m, n_games))
  # Q <- array(0, dim = c(m, m, n_games))

  # for (g in seq_len(n_games)) {
  #   T_block <- diag(1, m)

  #   # Strength block: AR(1) with phi_week
  #   T_block[idx_str, idx_str] <- diag(phi_week, n_teams)
  #   # League HFA & team HFA deviations: static within season (T = 1)

  #   T[,, g] <- T_block

  #   Q_block <- matrix(0, nrow = m, ncol = m)
  #   diag(Q_block)[idx_str] <- sigma_week^2

  #   Q[,, g] <- Q_block
  # }

  # Per-week updates ---
  # T <- array(0, dim = c(m, m, n_games))
  # Q <- array(0, dim = c(m, m, n_games))

  # week_vec <- season_schedule$week_idx # or week_idx if you prefer

  # for (g in seq_len(n_games)) {
  #   T_block <- diag(1, m)
  #   Q_block <- matrix(0, nrow = m, ncol = m)

  #   is_first_game_of_season <- (g == 1L)
  #   is_new_week <- is_first_game_of_season || (week_vec[g] != week_vec[g - 1L])

  #   if (is_new_week) {
  #     # Apply WEEKLY AR(1) step
  #     T_block[idx_str, idx_str] <- diag(phi_week, n_teams)
  #     diag(Q_block)[idx_str] <- sigma_week^2
  #   } else {
  #     # Same week: NO evolution of strengths
  #     # T_block[idx_str, idx_str] already = I
  #     # diag(Q_block)[idx_str] already = 0
  #   }

  #   # League HFA + team HFA deviations are static within season
  #   # => T = 1 on those diagonals, Q = 0 (already set by diag(1) and zeros)

  #   T[,, g] <- T_block
  #   Q[,, g] <- Q_block
  # }

  # Per-day updates ---
  T <- array(0, dim = c(m, m, n_games))
  Q <- array(0, dim = c(m, m, n_games))

  # Make sure season_schedule is sorted by gameday, then time, then game_idx
  # (You probably already did something like arrange(week, game_idx);
  #  for day-level you may want arrange(gameday, gametime, game_idx) upstream.)

  day_vec <- as.Date(season_schedule$gameday)

  for (g in seq_len(n_games)) {
    T_block <- diag(1, m)
    Q_block <- matrix(0, nrow = m, ncol = m)

    is_first_game_of_season <- (g == 1L)
    is_new_day <- is_first_game_of_season || (day_vec[g] != day_vec[g - 1L])

    if (is_new_day) {
      # Apply AR(1) step once per *day* instead of once per week
      T_block[idx_str, idx_str] <- diag(phi_week, n_teams)
      diag(Q_block)[idx_str] <- sigma_week^2
    } else {
      # Same day: no evolution of strengths
      # T_block[idx_str, idx_str] stays as identity
      # Q_block[idx_str] stays at 0
    }

    # League + team HFA still static within season → already handled by diag(1) and zeros

    T[,, g] <- T_block
    Q[,, g] <- Q_block
  }

  # ---------------------------------------------------------------------- #
  # Disturbance selector R (must be 3D: m x m x n_games)
  # ---------------------------------------------------------------------- #

  R <- replicate(n_games, diag(m), simplify = "array")

  # ---------------------------------------------------------------------- #
  # Observation noise variance
  # ---------------------------------------------------------------------- #

  # Observation noise variance (must be 3D to match time-varying model)
  H <- array(sigma_obs^2, dim = c(1, 1, n_games))

  # Build model, then set initial state
  model <- SSModel(
    season_schedule$result ~ -1 +
      SSMcustom(
        Z = Z,
        T = T,
        R = R,
        Q = Q,
        a1 = a1,
        P1 = P1
      ),
    H = H
  )

  #model$a1 <- a1
  #model$P1 <- P1

  model
}


# -------------------------------------------------------------------------- #
## 5.2 Helper: Run KFAS Filter and Extract In-Season Features ----
# -------------------------------------------------------------------------- #

run_inseason_kfas <- function(
  season_schedule,
  teams,
  end_of_season_state
) {
  hyper_means <- end_of_season_state$hyper_means

  n_teams <- length(teams)
  n_games <- nrow(season_schedule)

  # Initial means from end-of-season Stan fit (one-step-ahead predictions)
  init_team_strength <- end_of_season_state$predicted_team_strength
  init_league_hfa <- end_of_season_state$predicted_league_hfa
  init_team_hfa_dev <- end_of_season_state$predicted_team_hfa_dev

  # Prior sds for HFA states from previous season's posterior
  prior_sd_league_hfa <- end_of_season_state$filtered_league_hfa_sd
  prior_sd_team_hfa_dev <- end_of_season_state$filtered_team_hfa_sd

  # Build KFAS model
  model <- build_kfas_model_for_season(
    season_schedule = season_schedule,
    teams = teams,
    hyper_means = hyper_means,
    init_team_strength = init_team_strength,
    init_league_hfa = init_league_hfa,
    init_team_hfa_dev = init_team_hfa_dev,
    prior_sd_league_hfa = prior_sd_league_hfa,
    prior_sd_team_hfa_dev = prior_sd_team_hfa_dev
  )

  # Filtering only (no smoothing)
  kf <- KFS(model, filtering = "state", smoothing = "none")

  # Extract Z and H
  Z <- model$Z
  H <- model$H

  # Handle 2D vs 3D H
  H_dims <- dim(H)
  if (length(H_dims) == 2) {
    sigma_obs_vec <- rep(sqrt(H[1, 1]), n_games)
  } else if (length(H_dims) == 3) {
    sigma_obs_vec <- sqrt(H[1, 1, seq_len(n_games)])
  } else {
    stop("Unexpected dimension for model$H: ", paste(H_dims, collapse = " x "))
  }

  # State dimension
  m <- ncol(kf$a)

  # State indices
  idx_str <- seq_len(n_teams)
  idx_league <- n_teams + 1L
  idx_team_hfa <- (n_teams + 2L):(2L * n_teams + 1L)

  # Game indices
  home_idx <- season_schedule$home_idx
  away_idx <- season_schedule$away_idx

  # Allocate result vectors
  theta_home <- numeric(n_games)
  theta_away <- numeric(n_games)
  theta_diff <- numeric(n_games)
  sd_theta_home <- numeric(n_games)
  sd_theta_away <- numeric(n_games)
  sd_theta_diff <- numeric(n_games)

  team_hfa_home <- numeric(n_games)
  league_hfa_vec <- numeric(n_games)
  sd_team_hfa_home <- numeric(n_games)
  sd_league_hfa <- numeric(n_games)

  y_pred <- numeric(n_games)
  sd_pred <- numeric(n_games)

  # ================================
  # MAIN GAME LOOP
  # ================================

  for (g in seq_len(n_games)) {
    # State mean and covariance BEFORE observing game g
    alpha_pred <- kf$a[g, ] # m-length vector
    P_pred <- kf$P[,, g] # m x m covariance

    z_g <- Z[1, , g, drop = TRUE] # design vector
    sigma_obs_g2 <- sigma_obs_vec[g]^2

    # -----------------------------
    # Predicted result
    # -----------------------------
    y_g <- as.numeric(z_g %*% alpha_pred)
    var_g <- as.numeric(z_g %*% P_pred %*% z_g + sigma_obs_g2)

    y_pred[g] <- y_g
    sd_pred[g] <- sqrt(var_g)

    # -----------------------------
    # Latent team strength means
    # -----------------------------
    i_home <- idx_str[home_idx[g]]
    i_away <- idx_str[away_idx[g]]

    theta_home[g] <- alpha_pred[i_home]
    theta_away[g] <- alpha_pred[i_away]
    theta_diff[g] <- theta_home[g] - theta_away[g]

    # SDs
    sd_theta_home[g] <- sqrt(P_pred[i_home, i_home])
    sd_theta_away[g] <- sqrt(P_pred[i_away, i_away])

    var_diff <-
      P_pred[i_home, i_home] +
      P_pred[i_away, i_away] -
      2 * P_pred[i_home, i_away]

    sd_theta_diff[g] <- sqrt(var_diff)

    # -----------------------------
    # HFA components
    # -----------------------------
    league_hfa_vec[g] <- alpha_pred[idx_league]
    sd_league_hfa[g] <- sqrt(P_pred[idx_league, idx_league])

    # Team HFA home = league + team_dev
    i_hfa_home <- idx_team_hfa[home_idx[g]]
    team_hfa_dev_home <- alpha_pred[i_hfa_home]

    team_hfa_home[g] <- league_hfa_vec[g] + team_hfa_dev_home

    var_hfa_home <-
      P_pred[idx_league, idx_league] +
      P_pred[i_hfa_home, i_hfa_home] +
      2 * P_pred[idx_league, i_hfa_home]

    sd_team_hfa_home[g] <- sqrt(var_hfa_home)
  }

  # ================================
  # RETURN RESULTS
  # ================================

  season_schedule |>
    mutate(
      theta_home = theta_home,
      sd_theta_home = sd_theta_home,
      theta_away = theta_away,
      sd_theta_away = sd_theta_away,
      theta_diff = theta_diff,
      sd_theta_diff = sd_theta_diff,

      team_hfa_home = team_hfa_home,
      sd_team_hfa_home = sd_team_hfa_home,

      league_hfa = league_hfa_vec,
      sd_league_hfa = sd_league_hfa,

      y_pred_result = y_pred,
      sd_pred_result = sd_pred
    )
}


# ============================================================================ #
# 6. Run In-Season KFAS for Current Season ----
# ============================================================================ #

current_season_schedule <- schedule_idx |>
  filter(
    season == current_season #, !is.na(result)
  ) |>
  arrange(week, game_idx)

model <- build_kfas_model_for_season(
  season_schedule = current_season_schedule,
  teams = teams,
  hyper_means = end_of_season_state$hyper_means,
  init_team_strength = end_of_season_state$predicted_team_strength,
  init_league_hfa = end_of_season_state$predicted_league_hfa,
  init_team_hfa_dev = end_of_season_state$predicted_team_hfa_dev,
  prior_sd_league_hfa = end_of_season_state$filtered_league_hfa_sd,
  prior_sd_team_hfa_dev = end_of_season_state$filtered_team_hfa_sd
)

model_test <- model
KFAS::is.SSModel(model_test)

cat("\n--- SSModel Structural Check ---\n")

cat("Z dims: ", paste(dim(model_test$Z), collapse = " x "), "\n")
cat("T dims: ", paste(dim(model_test$T), collapse = " x "), "\n")
cat("R dims: ", paste(dim(model_test$R), collapse = " x "), "\n")
cat("Q dims: ", paste(dim(model_test$Q), collapse = " x "), "\n")
cat("H dims: ", paste(dim(model_test$H), collapse = " x "), "\n")

cat("a1 diims ", paste(dim(model_test$a1), collapse = " x "), "\n")
cat("P1 dims: ", paste(dim(model_test$P1), collapse = " x "), "\n")

kf <- KFS(model, filtering = "state", smoothing = "none")

cat("\n=== Running KFAS in-season filter for season", current_season, "===\n")

inseason_results <- run_inseason_kfas(
  season_schedule = current_season_schedule,
  teams = teams,
  end_of_season_state = end_of_season_state
)

inseason_results <- inseason_results |>
  rowwise() |>
  mutate(
    home_strength = rvar(rnorm(4000, theta_home, sd_theta_home)),
    away_strength = rvar(rnorm(4000, theta_away, sd_theta_away)),
    home_hfa = rvar(rnorm(4000, team_hfa_home, sd_team_hfa_home)),
    mu = home_strength - away_strength + home_hfa,
    y = rvar(rnorm(4000, y_pred_result, sd_pred_result))
  ) |>
  ungroup()

cat("\n=== Example of in-season latent features ===\n")
print(
  inseason_results |>
    select(
      season,
      week,
      game_id,
      theta_home,
      theta_away,
      theta_diff,
      team_hfa_home,
      league_hfa,
      mu_pred_result,
      sd_pred_result
    ) |>
    filter(season == 2025, week == 1) |>
    data.frame()
)

# ============================================================================ #
# 7. Next Steps (Betting Layer, outside this script) ----
# ============================================================================ #
#
# - Join `inseason_results` with Vegas lines and covariates.
#   Example:
#     betting_df <- inseason_results |>
#       left_join(spread_data, by = "game_id") |>
#       mutate(Y = result - spread_line)
#
# - Fit a Bayesian regression (e.g. brms) using:
#       Y ~ theta_diff + team_hfa_home + league_hfa + covariates
#
# - Use posterior_predict() to get draws for Y, so:
#       P(home covers) = mean(Y_draws > 0)
#       P(away covers) = mean(Y_draws < 0)
#
# - Use these probabilities to build ROI tables and evaluate betting strategies.
#
# This script gives you:
#   - End-of-season full Bayesian fit (Stan),
#   - Hyperparameters for the state-space process,
#   - Season-specific, time-updated latent team strengths and HFA via KFAS,
#   - Per-game predictive distributions for score differential.
# ============================================================================ #

perf_kfa_df <- inseason_results |>
  mutate(
    bet_prob_home_win = Pr(y > 0),
    bet_prob_away_win = Pr(y < 0),
    bet_winner = case_when(
      bet_prob_home_win > bet_prob_away_win ~ home_team,
      bet_prob_home_win < bet_prob_away_win ~ away_team,
      TRUE ~ NA_character_
    ),
    bet_winner_correct = case_when(
      bet_winner == winner ~ TRUE,
      bet_winner != winner ~ FALSE,
      TRUE ~ NA
    ),
    bet_prob_home_cover = Pr(y > spread_line),
    bet_prob_away_cover = Pr(y < spread_line),
    bet_cover = case_when(
      bet_prob_home_cover > bet_prob_away_cover ~ home_team,
      bet_prob_home_cover < bet_prob_away_cover ~ away_team,
      TRUE ~ NA_character_
    ),
    bet_vegas_cover = case_when(
      bet_prob_home_cover > home_spread_prob ~ home_team,
      bet_prob_away_cover > away_spread_prob ~ away_team,
      TRUE ~ NA_character_
    ),
    bet_vegas_thresh_cover = case_when(
      (bet_prob_home_cover > home_spread_prob) &
        (abs(bet_prob_home_cover - home_spread_prob) < 0.08) ~ home_team,
      (bet_prob_away_cover > away_spread_prob) &
        (abs(bet_prob_away_cover - away_spread_prob) < 0.08) ~ away_team,
      #bet_prob_away_cover > away_spread_prob ~ away_team,
      TRUE ~ NA_character_
    ),
    result_cover = case_when(
      result > spread_line ~ home_team,
      result < spread_line ~ away_team,
      TRUE ~ NA_character_
    ),
    bet_cover_correct = case_when(
      bet_cover == result_cover ~ TRUE,
      bet_cover != result_cover ~ FALSE,
      TRUE ~ NA
    ),
    bet_vegas_cover_correct = case_when(
      bet_vegas_cover == result_cover ~ TRUE,
      bet_vegas_cover != result_cover ~ FALSE,
      TRUE ~ NA
    ),
    bet_vegas_thresh_cover_correct = case_when(
      bet_vegas_thresh_cover == result_cover ~ TRUE,
      bet_vegas_thresh_cover != result_cover ~ FALSE,
      TRUE ~ NA
    )
  )

perf_kfa_results <- perf_kfa_df |>
  summarise(
    total_games = sum(!is.na(result)),
    home_wins = sum(winner == home_team, na.rm = TRUE),
    away_wins = sum(winner == away_team, na.rm = TRUE),
    bet_home_wins = sum(bet_winner == home_team, na.rm = TRUE),
    bet_away_wins = sum(bet_winner == away_team, na.rm = TRUE),
    bet_wins_correct = sum(bet_winner_correct, na.rm = TRUE),
    bet_home_covers = sum(bet_cover == home_team, na.rm = TRUE),
    bet_away_covers = sum(bet_cover == away_team, na.rm = TRUE),
    bet_covers_correct = sum(bet_cover_correct, na.rm = TRUE),
    bet_vegas_covers_correct = sum(
      bet_vegas_cover_correct,
      na.rm = TRUE
    ),
    bet_vegas_thresh_covers_correct = sum(
      bet_vegas_thresh_cover_correct,
      na.rm = TRUE
    ),
    pct_home_wins = home_wins / total_games,
    pct_away_wins = away_wins / total_games,
    pct_bet_wins_correct = mean(bet_winner_correct, na.rm = TRUE),
    pct_home_covers = sum(result_cover == home_team, na.rm = TRUE) /
      total_games,
    pct_away_covers = sum(result_cover == away_team, na.rm = TRUE) /
      total_games,
    pct_bet_covers_correct = mean(bet_cover_correct, na.rm = TRUE),
    pct_vegas_covers_correct = mean(bet_vegas_cover_correct, na.rm = TRUE),
    pct_vegas_thresh_covers_correct = mean(
      bet_vegas_thresh_cover_correct,
      na.rm = TRUE
    )
  )
data.frame(perf_kfa_results)

tic()
hyper_means_list <- list()
end_of_season_state_list <- list()
inseason_results_list <- list()
for (fit_season in 2006:2025) {
  fit_hyperparams_files <- pb_list(
    repo = github_data_repo,
    tag = "team_strength_fit_hyperparams"
  ) |>
    pull(file_name)
  fit_hyperparams_season_file <- str_subset(
    fit_hyperparams_files,
    pattern = paste0(fit_season - 1)
  )
  fit_hyperparams <- pb_read(
    file = fit_hyperparams_season_file,
    repo = github_data_repo,
    tag = "team_strength_fit_hyperparams"
  )
  hyper_means <- fit_hyperparams |>
    select(variable, mean) |>
    tibble::deframe()
  hyper_means_list[[as.character(fit_season)]] <- hyper_means

  team_strength_filter_file <- pb_list(
    repo = github_data_repo,
    tag = "team_strength_filter"
  ) |>
    pull(file_name) |>
    str_sort(numeric = TRUE) |>
    str_subset(pattern = paste0(fit_season - 1)) |>
    last()
  filtered_team_strengths <- pb_read(
    team_strength_filter_file,
    repo = github_data_repo,
    tag = "team_strength_filter"
  )

  filtered_team_strength <- E(filtered_team_strengths$filtered_team_strength)
  filtered_team_hfa <- E(filtered_team_strengths$filtered_team_hfa)
  filtered_team_hfa_sd <- sd(filtered_team_strengths$filtered_team_hfa)

  league_hfa_filter_file <- pb_list(
    repo = github_data_repo,
    tag = "league_hfa_filter"
  ) |>
    pull(file_name) |>
    str_sort(numeric = TRUE) |>
    str_subset(pattern = paste0(fit_season - 1)) |>
    last()
  filtered_league_hfas <- pb_read(
    league_hfa_filter_file,
    repo = github_data_repo,
    tag = "league_hfa_filter"
  )

  filtered_league_hfa <- E(filtered_league_hfas$filtered_league_hfa)
  filtered_team_hfa_dev <- filtered_team_hfa - filtered_league_hfa
  filtered_league_hfa_sd <- sd(filtered_league_hfas$filtered_league_hfa)

  team_strength_predict_file <- pb_list(
    repo = github_data_repo,
    tag = "team_strength_predict"
  ) |>
    pull(file_name) |>
    str_sort(numeric = TRUE) |>
    str_subset(pattern = paste0(fit_season)) |>
    first()
  predicted_team_strengths <- pb_read(
    team_strength_predict_file,
    repo = github_data_repo,
    tag = "team_strength_predict"
  )

  predicted_team_strength <- E(predicted_team_strengths$predicted_team_strength)
  predicted_team_hfa <- E(predicted_team_strengths$predicted_team_hfa)
  predicted_team_hfa_sd <- sd(predicted_team_strengths$predicted_team_hfa)

  league_hfa_predict_file <- pb_list(
    repo = github_data_repo,
    tag = "league_hfa_predict"
  ) |>
    pull(file_name) |>
    str_sort(numeric = TRUE) |>
    str_subset(pattern = paste0(fit_season)) |>
    first()
  predicted_league_hfas <- pb_read(
    league_hfa_predict_file,
    repo = github_data_repo,
    tag = "league_hfa_predict"
  )

  predicted_league_hfa <- E(predicted_league_hfas$predicted_league_hfa)
  predicted_team_hfa_dev <- predicted_team_hfa - predicted_league_hfa
  predicted_league_hfa_sd <- sd(predicted_league_hfas$predicted_league_hfa)

  end_of_season_state <- list(
    season_fit_through = fit_season - 1,
    teams = teams,
    hyper_means = hyper_means,
    filtered_team_strength = filtered_team_strength,
    filtered_team_hfa = filtered_team_hfa,
    filtered_league_hfa = filtered_league_hfa,
    predicted_team_strength = predicted_team_strength,
    predicted_team_hfa = predicted_team_hfa,
    predicted_league_hfa = predicted_league_hfa,
    filtered_team_hfa_dev = filtered_team_hfa_dev,
    predicted_team_hfa_dev = predicted_team_hfa_dev,
    filtered_team_hfa_sd = filtered_team_hfa_sd,
    filtered_league_hfa_sd = filtered_league_hfa_sd,
    predicted_team_hfa_sd = predicted_team_hfa_sd,
    predicted_league_hfa_sd = predicted_league_hfa_sd
  )

  end_of_season_state_list[[as.character(fit_season)]] <- end_of_season_state

  current_season_schedule <- schedule_idx |>
    filter(
      season == fit_season #, !is.na(result)
    ) |>
    arrange(week, game_idx)

  inseason_results <- run_inseason_kfas(
    season_schedule = current_season_schedule,
    teams = teams,
    end_of_season_state = end_of_season_state
  )
  inseason_results_list[[as.character(fit_season)]] <- inseason_results
}
toc()

inseason_results_df <- inseason_results_list |>
  list_rbind()

tic()
inseason_results_df <- inseason_results_df |>
  #rowwise() |>
  mutate(
    # home_strength = rvar(rnorm(4000, theta_home, sd_theta_home)),
    # away_strength = rvar(rnorm(4000, theta_away, sd_theta_away)),
    # home_hfa = rvar(rnorm(4000, team_hfa_home, sd_team_hfa_home)),
    # mu = home_strength - away_strength + home_hfa,
    # y = rvar(rnorm(4000, y_pred_result, sd_pred_result))
    home_strength = rvar_rng(
      rnorm,
      nrow(inseason_results_df),
      theta_home,
      sd_theta_home
    ),
    away_strength = rvar_rng(
      rnorm,
      nrow(inseason_results_df),
      theta_away,
      sd_theta_away
    ),
    home_hfa = rvar_rng(
      rnorm,
      nrow(inseason_results_df),
      team_hfa_home,
      sd_team_hfa_home
    ),
    mu = home_strength - away_strength + home_hfa,
    y = rvar_rng(
      rnorm,
      nrow(inseason_results_df),
      y_pred_result,
      sd_pred_result
    )
  ) #|>
#ungroup()
toc()

perf_kfa_df_all <- inseason_results_df |>
  mutate(
    bet_prob_home_win = Pr(y > 0),
    bet_prob_away_win = Pr(y < 0),
    bet_winner = case_when(
      bet_prob_home_win > bet_prob_away_win ~ home_team,
      bet_prob_home_win < bet_prob_away_win ~ away_team,
      TRUE ~ NA_character_
    ),
    bet_winner_correct = case_when(
      bet_winner == winner ~ TRUE,
      bet_winner != winner ~ FALSE,
      TRUE ~ NA
    ),
    bet_prob_home_cover = Pr(y > spread_line),
    bet_prob_away_cover = Pr(y < spread_line),
    bet_cover = case_when(
      bet_prob_home_cover > bet_prob_away_cover ~ home_team,
      bet_prob_home_cover < bet_prob_away_cover ~ away_team,
      TRUE ~ NA_character_
    ),
    bet_vegas_cover = case_when(
      bet_prob_home_cover > home_spread_prob ~ home_team,
      bet_prob_away_cover > away_spread_prob ~ away_team,
      TRUE ~ NA_character_
    ),
    bet_vegas_thresh_cover = case_when(
      (bet_prob_home_cover > home_spread_prob) &
        (abs(bet_prob_home_cover - home_spread_prob) < 0.08) ~ home_team,
      (bet_prob_away_cover > away_spread_prob) &
        (abs(bet_prob_away_cover - away_spread_prob) < 0.08) ~ away_team,
      #bet_prob_away_cover > away_spread_prob ~ away_team,
      TRUE ~ NA_character_
    ),
    result_cover = case_when(
      result > spread_line ~ home_team,
      result < spread_line ~ away_team,
      TRUE ~ NA_character_
    ),
    bet_cover_correct = case_when(
      bet_cover == result_cover ~ TRUE,
      bet_cover != result_cover ~ FALSE,
      TRUE ~ NA
    ),
    bet_vegas_cover_correct = case_when(
      bet_vegas_cover == result_cover ~ TRUE,
      bet_vegas_cover != result_cover ~ FALSE,
      TRUE ~ NA
    ),
    bet_vegas_thresh_cover_correct = case_when(
      bet_vegas_thresh_cover == result_cover ~ TRUE,
      bet_vegas_thresh_cover != result_cover ~ FALSE,
      TRUE ~ NA
    )
  )

perf_kfa_results_season <- perf_kfa_df_all |>
  group_by(season) |>
  summarise(
    total_games = sum(!is.na(result)),
    home_wins = sum(winner == home_team, na.rm = TRUE),
    away_wins = sum(winner == away_team, na.rm = TRUE),
    bet_home_wins = sum(bet_winner == home_team, na.rm = TRUE),
    bet_away_wins = sum(bet_winner == away_team, na.rm = TRUE),
    bet_wins_correct = sum(bet_winner_correct, na.rm = TRUE),
    bet_home_covers = sum(bet_cover == home_team, na.rm = TRUE),
    bet_away_covers = sum(bet_cover == away_team, na.rm = TRUE),
    bet_covers_correct = sum(bet_cover_correct, na.rm = TRUE),
    bet_vegas_covers_correct = sum(
      bet_vegas_cover_correct,
      na.rm = TRUE
    ),
    bet_vegas_thresh_covers_correct = sum(
      bet_vegas_thresh_cover_correct,
      na.rm = TRUE
    ),
    pct_home_wins = home_wins / total_games,
    pct_away_wins = away_wins / total_games,
    pct_bet_wins_correct = mean(bet_winner_correct, na.rm = TRUE),
    pct_home_covers = sum(result_cover == home_team, na.rm = TRUE) /
      total_games,
    pct_away_covers = sum(result_cover == away_team, na.rm = TRUE) /
      total_games,
    pct_bet_covers_correct = mean(bet_cover_correct, na.rm = TRUE),
    pct_vegas_covers_correct = mean(bet_vegas_cover_correct, na.rm = TRUE),
    pct_vegas_thresh_covers_correct = mean(
      bet_vegas_thresh_cover_correct,
      na.rm = TRUE
    )
  )
data.frame(perf_kfa_results)
data.frame(perf_kfa_results_all)
data.frame(perf_kfa_results_season)


## ============================================================================ #

bs_model <- as_bssm(model)
bs_kfilter <- kfilter(bs_model)


# ============================================================================ #
# Build full-history bssm model for team strength + HFA ----
# ============================================================================ #

build_team_strength_bssm <- function(schedule_tbl, teams) {
  # ------------------------------------------------------------------------ #
  # 1. Order and filter games
  # ------------------------------------------------------------------------ #

  schedule <- schedule_tbl |>
    filter(!is.na(result)) |>
    arrange(season, week, gameday, gametime)

  y <- schedule$result
  n_games <- length(y)

  n_teams <- length(teams)
  m <- 2L * n_teams + 1L # 32 strengths + 1 league_hfa + 32 team HFA devs

  # State index bookkeeping
  idx_str <- seq_len(n_teams)
  idx_league <- n_teams + 1L
  idx_hfa <- (n_teams + 2L):(2L * n_teams + 1L)

  home_idx <- schedule$home_idx
  away_idx <- schedule$away_idx
  hfa_ind <- schedule$hfa # 1 = real home, 0 = neutral
  season <- schedule$season

  # Flag for first game of each season
  is_new_season <- c(TRUE, diff(season) != 0L)

  # ------------------------------------------------------------------------ #
  # 2. Static observation design Z (does NOT depend on hyperparameters)
  #    Z_mat: n_games x m; internally bssm will use m x n
  # ------------------------------------------------------------------------ #

  Z_mat <- matrix(0, nrow = n_games, ncol = m)

  for (t in seq_len(n_games)) {
    z_t <- numeric(m)

    # Strengths: home - away
    z_t[idx_str[home_idx[t]]] <- 1
    z_t[idx_str[away_idx[t]]] <- -1

    # HFA contribution if true home
    if (hfa_ind[t] == 1L) {
      z_t[idx_league] <- 1
      z_t[idx_hfa[home_idx[t]]] <- 1
    }

    Z_mat[t, ] <- z_t
  }

  # bssm expects Z as m x n matrix for ssm_ulg
  Z_base <- t(Z_mat)

  # ------------------------------------------------------------------------ #
  # 3. Placeholder system matrices for defining dimensions
  #    (actual values will be overwritten by update_fn)
  # ------------------------------------------------------------------------ #

  # T, R can be time-varying: m x m x n
  T_base <- array(diag(1, m), dim = c(m, m, n_games))
  R_base <- array(0, dim = c(m, m, n_games))

  # Observation std devs (H is vector of SDs)
  H_base <- rep(1.0, n_games) # placeholder, updated by theta

  # Initial state mean and covariance
  a1_base <- numeric(m)
  P1_base <- diag(1, m)

  # State names (optional, but helpful)
  state_names <- c(
    paste0("str_", teams),
    "league_hfa",
    paste0("hfa_dev_", teams)
  )

  # Store data needed inside update_fn via closure
  env_data <- list(
    n_games = n_games,
    n_teams = n_teams,
    m = m,
    idx_str = idx_str,
    idx_league = idx_league,
    idx_hfa = idx_hfa,
    is_new_season = is_new_season
  )

  # ------------------------------------------------------------------------ #
  # 4. Hyperparameter mapping: theta -> system matrices
  # ------------------------------------------------------------------------ #
  #
  # theta:
  #  1 = phi_week
  #  2 = sigma_week
  #  3 = sigma_obs
  #  4 = sigma_team_init
  #  5 = phi_league
  #  6 = sigma_league
  #  7 = sigma_team_hfa
  #
  # NOTE: We keep phi's unconstrained (can be >1) with priors that strongly
  #       favour |phi| < 1. sigmas are constrained to > 0 via prior.
  # ------------------------------------------------------------------------ #

  update_fn <- function(theta) {
    with(env_data, {
      phi_week <- theta[1]
      sigma_week <- theta[2]
      sigma_obs <- theta[3]
      sigma_team_init <- theta[4]
      phi_league <- theta[5]
      sigma_league <- theta[6]
      sigma_team_hfa <- theta[7]

      # enforce small lower bound on sigmas to avoid numerical issues
      sigma_week <- pmax(sigma_week, 1e-6)
      sigma_obs <- pmax(sigma_obs, 1e-6)
      sigma_team_init <- pmax(sigma_team_init, 1e-6)
      sigma_league <- pmax(sigma_league, 1e-6)
      sigma_team_hfa <- pmax(sigma_team_hfa, 1e-6)

      # T: m x m x n
      T_upd <- array(0, dim = c(m, m, n_games))
      # R: m x m x n (diagonal innovations)
      R_upd <- array(0, dim = c(m, m, n_games))

      for (t in seq_len(n_games)) {
        T_block <- diag(1, m)
        R_block <- matrix(0, m, m)

        # Team strengths: AR(1) with phi_week
        T_block[idx_str, idx_str] <- diag(phi_week, n_teams)
        diag(R_block)[idx_str] <- sigma_week

        # League HFA: static within season; gets AR(1) innovation at first game of season
        if (is_new_season[t]) {
          T_block[idx_league, idx_league] <- phi_league
          diag(R_block)[idx_league] <- sigma_league
        } else {
          T_block[idx_league, idx_league] <- 1
          # no process noise for league_hfa within season
        }

        # Team HFA deviations: static random effects (no process noise)
        # T_block[idx_hfa, idx_hfa] <- diag(1, n_teams) already via diag(1, m)
        # diag(R_block)[idx_hfa] <- 0

        T_upd[,, t] <- T_block
        R_upd[,, t] <- R_block
      }

      # H: vector of std devs
      H_upd <- rep(sigma_obs, n_games)

      # Prior for states:
      a1_upd <- numeric(m)
      P1_upd <- diag(0, m)

      # Team strengths prior variance
      diag(P1_upd)[idx_str] <- sigma_team_init^2

      # League HFA prior variance: stationary approx
      if (abs(phi_league) < 1) {
        var_league <- sigma_league^2 / (1 - phi_league^2)
      } else {
        var_league <- sigma_league^2 * 10
      }
      diag(P1_upd)[idx_league] <- var_league

      # Team HFA deviations prior variance
      diag(P1_upd)[idx_hfa] <- sigma_team_hfa^2

      list(
        T = T_upd,
        R = R_upd,
        H = H_upd,
        a1 = a1_upd,
        P1 = P1_upd
      )
    })
  }

  # ------------------------------------------------------------------------ #
  # 5. Prior for theta
  # ------------------------------------------------------------------------ #
  #
  # You can tune these to roughly match your Stan posteriors.
  # Here we use:
  #   - phi_week      ~ N(0.9, 0.2^2)
  #   - sigma_week    ~ half-N(0, 2)
  #   - sigma_obs     ~ half-N(0, 10)
  #   - sigma_team_init ~ half-N(0, 10)
  #   - phi_league    ~ N(0.8, 0.3^2)
  #   - sigma_league  ~ half-N(0, 2)
  #   - sigma_team_hfa ~ half-N(0, 5)
  # ------------------------------------------------------------------------ #

  prior_fn <- function(theta) {
    phi_week <- theta[1]
    sigma_week <- theta[2]
    sigma_obs <- theta[3]
    sigma_team_init <- theta[4]
    phi_league <- theta[5]
    sigma_league <- theta[6]
    sigma_team_hfa <- theta[7]

    # sigmas must be > 0
    if (
      any(
        c(
          sigma_week,
          sigma_obs,
          sigma_team_init,
          sigma_league,
          sigma_team_hfa
        ) <=
          0
      )
    ) {
      return(-Inf)
    }

    lp <- 0

    # phi priors (Gaussian, unconstrained)
    lp <- lp + dnorm(phi_week, mean = 0.9, sd = 0.2, log = TRUE)
    lp <- lp + dnorm(phi_league, mean = 0.8, sd = 0.3, log = TRUE)

    # sigma priors: half-normal via truncation at 0
    lp <- lp + dnorm(sigma_week, 0, 2, log = TRUE)
    lp <- lp + dnorm(sigma_obs, 0, 10, log = TRUE)
    lp <- lp + dnorm(sigma_team_init, 0, 10, log = TRUE)
    lp <- lp + dnorm(sigma_league, 0, 2, log = TRUE)
    lp <- lp + dnorm(sigma_team_hfa, 0, 5, log = TRUE)

    lp
  }

  # ------------------------------------------------------------------------ #
  # 6. Initial hyperparameter values (theta_init)
  #    You can seed these with your Stan posterior means if you like.
  # ------------------------------------------------------------------------ #

  theta_init <- c(
    phi_week = 0.99,
    sigma_week = 0.8,
    sigma_obs = 12.5,
    sigma_team_init = 4.5,
    phi_league = 0.95,
    sigma_league = 0.4,
    sigma_team_hfa = 1.2
  )

  # ------------------------------------------------------------------------ #
  # 7. Build ssm_ulg model
  # ------------------------------------------------------------------------ #

  model <- ssm_ulg(
    y = y,
    Z = Z_base,
    H = H_base,
    T = T_base,
    R = R_base,
    a1 = a1_base,
    P1 = P1_base,
    init_theta = theta_init,
    state_names = state_names,
    update_fn = update_fn,
    prior_fn = prior_fn
  )

  model
}


# Get input data
schedule_tbl <- schedule_idx

# Build the model
bssm_model <- build_team_strength_bssm(schedule_tbl, teams)

bssm_kfilter <- kfilter(bssm_model)

# Run MCMC on hyperparameters + states
# (tune nsim/burnin/thin as needed; these are just example numbers)
fit_bssm <- run_mcmc(
  bssm_model,
  iter = 20000,
  burnin = 5000,
  thin = 10,
  mcmc_type = "approx", # exact LG, so this is fast
  verbose = TRUE
)

# Quick diagnostics for theta
summary(fit_bssm, variable = "theta", return_se = TRUE)

# Get posterior means of states (α_t) at each game
state_summary <- summary(fit_bssm, variable = "states")

# state_summary is a list; $mean is a matrix:
# rows = states (65), cols = time (games)
str(state_summary$mean)

state_means <- state_summary$mean

n_teams <- length(teams)
idx_str <- seq_len(n_teams)
idx_league <- n_teams + 1L
idx_hfa <- (n_teams + 2L):(2L * n_teams + 1L)

# For each game t, compute theta_home, theta_away, theta_diff, etc.
schedule_ordered <- schedule_idx |>
  filter(!is.na(result)) |>
  arrange(season, week, game_idx)

home_idx <- schedule_ordered$home_idx
away_idx <- schedule_ordered$away_idx

n_games <- nrow(schedule_ordered)

theta_home_mean <- numeric(n_games)
theta_away_mean <- numeric(n_games)
theta_diff_mean <- numeric(n_games)
league_hfa_mean <- numeric(n_games)
team_hfa_home_mean <- numeric(n_games)

for (t in seq_len(n_games)) {
  alpha_t <- state_means[, t]

  theta_home_mean[t] <- alpha_t[idx_str[home_idx[t]]]
  theta_away_mean[t] <- alpha_t[idx_str[away_idx[t]]]
  theta_diff_mean[t] <- theta_home_mean[t] - theta_away_mean[t]

  league_hfa_mean[t] <- alpha_t[idx_league]
  team_hfa_dev_home <- alpha_t[idx_hfa[home_idx[t]]]
  team_hfa_home_mean[t] <- league_hfa_mean[t] + team_hfa_dev_home
}

latent_df <- schedule_ordered |>
  mutate(
    theta_home = theta_home_mean,
    theta_away = theta_away_mean,
    theta_diff = theta_diff_mean,
    league_hfa = league_hfa_mean,
    team_hfa_home = team_hfa_home_mean
  )
