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
library(bssm) # not used in this script but available if you experiment later
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

# Stan fit used to use data through this season; here it's just a reference
fit_through_season <- current_season - 1L

# Load full game data (all seasons)
game_data_full <- nflendzone::load_game_data(seasons = all_seasons)

# ============================================================================ #
# 2. Prepare Schedule Indices ----
# ============================================================================ #
#
# We construct schedule_idx with integer team indices etc.
# This is the main input to the ML + KFAS state-space model.
# ============================================================================ #

# Prepare full schedule with indices (teams mapped to 1..K)
schedule_idx <- prepare_schedule_indices(
  game_data_full,
  teams
)

# Optional: training data used previously for Stan (not required for KFAS)
training_data <- schedule_idx |>
  filter(
    season < current_season,
    !is.na(result)
  )

# ============================================================================ #
# 3. Build ML-Based Two-Layer Team Strength KFAS Model ----
# ============================================================================ #
#
# State vector α_t (dimension m = 3 * K + 1, where K = number of teams):
#
#   [ season_strength_1..K,          (slow AR(1) across seasons)
#     weekly_strength_1..K,          (fast AR(1) within season)
#     league_hfa,                    (AR(1) across seasons, static within)
#     team_hfa_dev_1..K ]            (static random effects per team)
#
# Observation equation at game t:
#
#   result_t =
#     (season_home + week_home) -
#     (season_away + week_away) +
#     I(hfa_t==1) * (league_hfa + team_hfa_dev_home) +
#     eps_t,   eps_t ~ N(0, sigma_obs^2)
#
# Hyperparameters (estimated by ML via fitSSM):
#
#   pars[1] = phi_week_raw      → phi_week   in (-1, 1)
#   pars[2] = log_sigma_week    → sigma_week > 0
#   pars[3] = phi_season_raw    → phi_season in (-1, 1)
#   pars[4] = log_sigma_season  → sigma_season > 0
#   pars[5] = log_sigma_obs     → sigma_obs  > 0
#   pars[6] = phi_league_raw    → phi_league in (-1, 1)
#   pars[7] = log_sigma_league  → sigma_league > 0
#   pars[8] = log_sigma_hfa     → sigma_team_hfa > 0
# ============================================================================ #

build_team_strength_kfas_ml <- function(schedule_idx, teams) {
  # ---------------------------------------------------------------------- #
  # 0. Preprocess & dimensions
  # ---------------------------------------------------------------------- #
  schedule <- schedule_idx |>
    dplyr::filter(!is.na(result)) |>
    dplyr::arrange(season, week, game_idx)

  y <- schedule$result
  n_games <- nrow(schedule)
  K <- length(teams)

  # State indices
  idx_season <- 1:K
  idx_week <- (K + 1):(2 * K)
  idx_league <- 2 * K + 1
  idx_team_hfa <- (2 * K + 2):(3 * K + 1)

  m <- 3 * K + 1 # state dimension
  k <- 2 * K + 1 # disturbance dimension

  home_idx <- schedule$home_idx
  away_idx <- schedule$away_idx
  hfa_ind <- schedule$hfa
  is_new_season <- c(TRUE, diff(schedule$season) != 0)
  season_inds <- which(is_new_season)

  # ---------------------------------------------------------------------- #
  # 1. Initial state
  # ---------------------------------------------------------------------- #
  a1 <- numeric(m)
  P1 <- diag(0, m)
  diag(P1)[idx_season] <- 10^2
  diag(P1)[idx_week] <- 5^2
  P1[idx_league, idx_league] <- 5^2
  diag(P1)[idx_team_hfa] <- 2^2

  # ---------------------------------------------------------------------- #
  # 2. Z: 1 × m × n_games
  # ---------------------------------------------------------------------- #
  Z <- array(0, dim = c(1, m, n_games))
  for (g in seq_len(n_games)) {
    z_g <- numeric(m)

    z_g[idx_season[home_idx[g]]] <- 1
    z_g[idx_season[away_idx[g]]] <- -1
    z_g[idx_week[home_idx[g]]] <- 1
    z_g[idx_week[away_idx[g]]] <- -1

    if (hfa_ind[g] == 1L) {
      z_g[idx_league] <- 1
      z_g[idx_team_hfa[home_idx[g]]] <- 1
    }

    Z[1, , g] <- z_g
  }

  # ---------------------------------------------------------------------- #
  # 3. R: m × k × n
  # ---------------------------------------------------------------------- #
  R0 <- array(0, dim = c(m, k, n_games))

  # season innovations
  R0[cbind(idx_season, 1:K, 1)] <- 1
  for (j in seq_len(K)) {
    R0[idx_season[j], j, ] <- 1
  }

  # weekly innovations
  for (j in seq_len(K)) {
    R0[idx_week[j], K + j, ] <- 1
  }

  # league HFA innovation
  R0[idx_league, 2 * K + 1, ] <- 1

  # ---------------------------------------------------------------------- #
  # 4. T0 and Q0: placeholders
  # ---------------------------------------------------------------------- #
  T0 <- array(0, dim = c(m, m, n_games))
  for (g in seq_len(n_games)) {
    T0[,, g] <- diag(1, m)
  }

  Q0 <- array(0, dim = c(k, k, n_games))

  # ---------------------------------------------------------------------- #
  # 5. H
  # ---------------------------------------------------------------------- #
  H0 <- array(1, dim = c(1, 1, n_games))

  # ---------------------------------------------------------------------- #
  # 6. Build SSModel
  # ---------------------------------------------------------------------- #
  model0 <- KFAS::SSModel(
    schedule$result ~ -1 +
      SSMcustom(
        Z = Z,
        T = T0,
        R = R0,
        Q = Q0,
        a1 = a1,
        P1 = P1
      ),
    H = H0
  )

  # ---------------------------------------------------------------------- #
  # 7. meta for updatefn
  # ---------------------------------------------------------------------- #
  meta <- list(
    n_games = n_games,
    K = K,
    k = k,
    idx_season = idx_season,
    idx_week = idx_week,
    idx_league = idx_league,
    idx_team_hfa = idx_team_hfa,
    is_new_season = is_new_season,
    season_inds = season_inds
  )

  # ---------------------------------------------------------------------- #
  # 8. VECTORIZED updatefn (major speedup)
  # ---------------------------------------------------------------------- #
  updatefn <- local(function(pars, model, ...) {
    u <- meta
    n_games <- u$n_games
    K <- u$K
    k <- u$k
    idx_season <- u$idx_season
    idx_week <- u$idx_week
    idx_league <- u$idx_league
    idx_team_hfa <- u$idx_team_hfa
    season_inds <- u$season_inds

    m <- length(model$a1)

    # transforms
    phi_week <- 2 * plogis(pars[1]) - 1
    sigma_week <- exp(pars[2])
    phi_season <- 2 * plogis(pars[3]) - 1
    sigma_season <- exp(pars[4])
    sigma_obs <- exp(pars[5])
    phi_league <- 2 * plogis(pars[6]) - 1
    sigma_league <- exp(pars[7])
    sigma_team_hfa <- exp(pars[8])

    # ============================================================ #
    # VECTORIZE T updates
    # ============================================================ #

    # weekly AR(1) (applies to ALL games)
    model$T[idx_week, idx_week, ] <- phi_week

    # season AR(1) only at season boundaries
    if (length(season_inds) > 0) {
      model$T[idx_season, idx_season, season_inds] <- phi_season
      model$T[idx_league, idx_league, season_inds] <- phi_league
    }

    # ============================================================ #
    # VECTORIZE Q updates
    # ============================================================ #

    # weekly innovations always active
    model$Q[(K + 1):(2 * K), (K + 1):(2 * K), ] <- sigma_week^2

    # season + league disturbances only at boundaries
    if (length(season_inds) > 0) {
      model$Q[1:K, 1:K, season_inds] <- sigma_season^2
      model$Q[2 * K + 1, 2 * K + 1, season_inds] <- sigma_league^2
    }

    # ============================================================ #
    # Observation noise H
    # ============================================================ #
    model$H[1, 1, ] <- sigma_obs^2

    # ============================================================ #
    # Initial covariance (does NOT depend on time)
    # ============================================================ #
    P1_new <- diag(0, m)
    diag(P1_new)[idx_season] <- 10^2
    diag(P1_new)[idx_week] <- 5^2
    P1_new[idx_league, idx_league] <- 5^2
    diag(P1_new)[idx_team_hfa] <- sigma_team_hfa^2

    model$P1[,] <- P1_new
    model$a1[] <- 0

    model
  })

  list(
    model0 = model0,
    updatefn = updatefn,
    schedule = schedule,
    teams = teams
  )
}


# ============================================================================ #
# 4. Fit ML KFAS Model (fitSSM + KFS) ----
# ============================================================================ #
#
# This step:
#   - Runs ML estimation of hyperparameters via fitSSM()
#   - Performs Kalman filtering to obtain one-step-ahead states (a, P)
# ============================================================================ #

fit_team_strength_kfas_ml <- function(
  schedule_idx,
  teams,
  init_pars = NULL,
  method = "BFGS",
  ...
) {
  built <- build_team_strength_kfas_ml(schedule_idx, teams)
  model0 <- built$model0
  updatefn <- built$updatefn
  schedule <- built$schedule

  # Stan-based hyperparam guesses
  if (is.null(init_pars)) {
    init_pars <- c(
      qlogis((0.994 + 1) / 2), # phi_week_raw
      log(0.803), # log_sigma_week
      qlogis((0.679 + 1) / 2), # phi_season_raw
      log(3.35), # log_sigma_season
      log(12.5), # log_sigma_obs
      qlogis((0.940 + 1) / 2), # phi_league_raw
      log(0.385), # log_sigma_league
      log(1.21) # log_sigma_hfa
    )
  }

  cat("\n=== Fitting ML KFAS Team Strength Model (full history) ===\n")
  tic("ML KFAS fit time")

  fit_ml <- KFAS::fitSSM(
    model = model0,
    inits = init_pars,
    updatefn = updatefn,
    method = method,
    ...
  )

  toc()

  cat("\n=== Running Kalman Filter (one-step-ahead states) ===\n")
  kf <- KFAS::KFS(
    fit_ml$model,
    filtering = "state",
    smoothing = "none"
  )

  list(
    fit = fit_ml,
    kf = kf,
    schedule = schedule,
    teams = teams
  )
}


# ============================================================================ #
# 5. Extract Latent Features from KFAS Fit ----
# ============================================================================ #
#
# Uses predicted states (a, P) or smoothed states (alphahat, V) to build:
#   - theta_home, theta_away, theta_diff
#   - league_hfa, team_hfa_home
#   - sd_* for all above
#   - mu_pred_result, sd_pred_result
#
# These outputs are ready to be joined into your betting model data.
# ============================================================================ #

extract_latent_features_kfas <- function(
  fit_obj,
  use_smoothed = FALSE # set TRUE if you re-run KFS with smoothing="state"
) {
  kf <- fit_obj$kf
  model <- fit_obj$fit$model
  schedule <- fit_obj$schedule
  teams <- fit_obj$teams

  n_games <- nrow(schedule)

  meta <- model$u
  K <- meta$K
  idx_season <- meta$idx_season
  idx_week <- meta$idx_week
  idx_league <- meta$idx_league
  idx_hfa <- meta$idx_hfa

  # Choose which states to use
  if (use_smoothed && !is.null(kf$alphahat)) {
    alpha <- kf$alphahat
    V <- kf$V
  } else {
    alpha <- kf$a
    V <- kf$P
  }

  home_idx <- schedule$home_idx
  away_idx <- schedule$away_idx

  # Preallocate
  theta_home <- numeric(n_games)
  theta_away <- numeric(n_games)
  theta_diff <- numeric(n_games)
  league_hfa <- numeric(n_games)
  team_hfa_home <- numeric(n_games)

  sd_theta_home <- numeric(n_games)
  sd_theta_away <- numeric(n_games)
  sd_theta_diff <- numeric(n_games)
  sd_league_hfa <- numeric(n_games)
  sd_team_hfa_home <- numeric(n_games)

  mu_pred_result <- numeric(n_games)
  sd_pred_result <- numeric(n_games)

  Z <- model$Z
  H <- model$H

  # Handle H dims (2D vs 3D)
  H_dims <- dim(H)
  if (length(H_dims) == 2) {
    sigma_obs_vec <- rep(sqrt(H[1, 1]), n_games)
  } else {
    sigma_obs_vec <- sqrt(H[1, 1, seq_len(n_games)])
  }

  for (t in seq_len(n_games)) {
    a_t <- alpha[t, ]
    P_t <- V[,, t]

    h <- home_idx[t]
    a <- away_idx[t]

    # ---- Overall team strengths: season + weekly ----
    season_home <- a_t[idx_season[h]]
    weekly_home <- a_t[idx_week[h]]
    season_away <- a_t[idx_season[a]]
    weekly_away <- a_t[idx_week[a]]

    theta_home[t] <- season_home + weekly_home
    theta_away[t] <- season_away + weekly_away
    theta_diff[t] <- theta_home[t] - theta_away[t]

    league_hfa[t] <- a_t[idx_league]
    hfa_dev_home <- a_t[idx_hfa[h]]
    team_hfa_home[t] <- league_hfa[t] + hfa_dev_home

    # ---- SDs of latent quantities via linear forms ----

    # theta_home: w_home = e_season_home + e_week_home
    w_home <- numeric(length(a_t))
    w_home[idx_season[h]] <- 1
    w_home[idx_week[h]] <- 1

    # theta_away
    w_away <- numeric(length(a_t))
    w_away[idx_season[a]] <- 1
    w_away[idx_week[a]] <- 1

    # theta_diff = theta_home - theta_away
    w_diff <- w_home - w_away

    var_theta_home <- as.numeric(t(w_home) %*% P_t %*% w_home)
    var_theta_away <- as.numeric(t(w_away) %*% P_t %*% w_away)
    var_theta_diff <- as.numeric(t(w_diff) %*% P_t %*% w_diff)

    sd_theta_home[t] <- sqrt(max(var_theta_home, 0))
    sd_theta_away[t] <- sqrt(max(var_theta_away, 0))
    sd_theta_diff[t] <- sqrt(max(var_theta_diff, 0))

    # league_hfa variance
    sd_league_hfa[t] <- sqrt(max(P_t[idx_league, idx_league], 0))

    # team_hfa_home = league_hfa + hfa_dev_home
    w_hfa <- numeric(length(a_t))
    w_hfa[idx_league] <- 1
    w_hfa[idx_hfa[h]] <- 1

    var_team_hfa_home <- as.numeric(t(w_hfa) %*% P_t %*% w_hfa)
    sd_team_hfa_home[t] <- sqrt(max(var_team_hfa_home, 0))

    # ---- Predictive distribution for result ----
    z_t <- Z[1, , t, drop = TRUE]
    mu_t <- sum(z_t * a_t)
    var_t <- as.numeric(t(z_t) %*% P_t %*% z_t + sigma_obs_vec[t]^2)

    mu_pred_result[t] <- mu_t
    sd_pred_result[t] <- sqrt(max(var_t, 0))
  }

  schedule |>
    mutate(
      theta_home = theta_home,
      theta_away = theta_away,
      theta_diff = theta_diff,
      league_hfa = league_hfa,
      team_hfa_home = team_hfa_home,
      sd_theta_home = sd_theta_home,
      sd_theta_away = sd_theta_away,
      sd_theta_diff = sd_theta_diff,
      sd_league_hfa = sd_league_hfa,
      sd_team_hfa_home = sd_team_hfa_home,
      mu_pred_result = mu_pred_result,
      sd_pred_result = sd_pred_result
    )
}

# ============================================================================ #
# 6. Run Full-History ML KFAS Fit & Generate Latent Features ----
# ============================================================================ #
#
# This section runs the model on all available games (2002-current),
# and returns per-game latent team strengths + HFA features that can
# be fed directly into your betting models (brms, cmdstanr, etc.).
# ============================================================================ #

test_built <- build_team_strength_kfas_ml(
  schedule_idx = schedule_idx |>
    filter(season <= 2005),
  teams = teams
)

model_test <- test_built$model0

{
  cat("\n--- SSModel Structural Check ---\n\n")

  cat("y dims: ", "\n")
  cat("  Required: ", "A n x p matrix containing the observations", "\n")
  cat("  Actual:   ", paste(dim(model_test$y), collapse = " x "), "\n")

  cat("Z dims: ", "\n")
  cat(
    "  Required: ",
    "A p x m x 1 or p x m x n array corresponding to the system matrix of observation equation",
    "\n"
  )
  cat("  Actual:   ", paste(dim(model_test$Z), collapse = " x "), "\n")

  cat("H dims: ", "\n")
  cat(
    "  Required: ",
    "A p x p x 1 or p x p x n array corresponding to the covariance matrix \n",
    "            of observational disturbances epsilo",
    "\n"
  )
  cat("  Actual:   ", paste(dim(model_test$H), collapse = " x "), "\n")

  cat("T dims: ", "\n")
  cat(
    "  Required: ",
    "A m x m x 1 or m x m x n array corresponding to the first system matrix \n",
    "            of state equatio",
    "\n"
  )
  cat("  Actual:   ", paste(dim(model_test$T), collapse = " x "), "\n")

  cat("R dims: ", "\n")
  cat(
    "  Required: ",
    "A m x k x 1 or m x k x n array corresponding to the second system matrix \n",
    "            of state equation",
    "\n"
  )
  cat("  Actual:   ", paste(dim(model_test$R), collapse = " x "), "\n")

  cat("Q dims: ", "\n")
  cat(
    "  Required: ",
    "A k x k x 1 or k x k x n array corresponding to the covariance matrix of \n",
    "            state disturbances eta",
    "\n"
  )
  cat("  Actual:   ", paste(dim(model_test$Q), collapse = " x "), "\n")

  cat("a1 dims: ", "\n")
  cat(
    "  Required: ",
    "A m x 1 matrix containing the expected values of the initial states",
    "\n"
  )
  cat("  Actual:   ", paste(dim(model_test$a1), collapse = " x "), "\n")

  cat("P1 dims: ", "\n")
  cat(
    "  Required: ",
    "A m x m matrix containing the covariance matrix of the nondiffuse part of \n",
    "            the initial state vector",
    "\n"
  )
  cat("  Actual:   ", paste(dim(model_test$P1), collapse = " x "), "\n\n")

  cat("\nIs SSModel? ", KFAS::is.SSModel(model_test), "\n")
}


cat("\n=== Starting full-history ML KFAS pipeline ===\n")

updatefn_test <- test_built$updatefn

init_pars_test <- c(
  qlogis((0.994 + 1) / 2), # phi_week_raw
  log(0.803), # log_sigma_week
  qlogis((0.679 + 1) / 2), # phi_season_raw
  log(3.35), # log_sigma_season
  log(12.5), # log_sigma_obs
  qlogis((0.940 + 1) / 2), # phi_league_raw
  log(0.385), # log_sigma_league
  log(1.21) # log_sigma_hfa
)

tic("Test ML KFAS fit time")
fit_ml_test <- KFAS::fitSSM(
  model = model_test,
  inits = init_pars_test,
  updatefn = updatefn_test,
  method = "BFGS",
  trace = 6
)
toc()

fit_kfas <- fit_team_strength_kfas_ml(
  schedule_idx = schedule_idx,
  teams = teams,
  trace = 6
)

latent_features <- extract_latent_features_kfas(fit_kfas)

cat("\n=== Example of latent features for current season ===\n")

latent_features |>
  filter(season == current_season) |>
  arrange(week, game_idx) |>
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
  head() |>
  print()
