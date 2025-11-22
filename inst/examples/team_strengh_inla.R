library(dplyr)
library(tidyr)
library(INLA)
library(tictoc)
library(posterior)
library(tidybayes)
library(tibble)
library(stringr)

# ============================================================================ #
# 1. Prepare Data for Fitting ----
# ============================================================================ #

teams_tbl <- nflreadr::load_teams(current = TRUE)
teams <- teams_tbl$team_abbr

all_seasons <- 2002:nflreadr::get_current_season()
current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

game_data_full <- nflendzone::load_game_data(seasons = all_seasons)

# Add a persistent row index so we can match game_idx like Stan
game_data_full <- game_data_full |>
  mutate(game_idx = dplyr::row_number())

# 1) Encode teams and seasons
games_prep <- game_data_full |>
  mutate(
    team_home = as.integer(factor(home_team)),
    team_away = as.integer(factor(away_team)),
    season_id = as.integer(factor(season)) # 1..N_seasons
  )

n_teams <- max(games_prep$team_home)
n_seasons <- max(games_prep$season_id)

# Team name levels (mapping team_id -> team_abbr)
team_levels <- levels(factor(games_prep$home_team))

# 2) Create a global-week index across all seasons
games_prep <- games_prep |>
  arrange(season, week, gameday, gametime) |>
  mutate(
    global_week = dplyr::cur_group_id(),
    .by = c(season, week)
  )

n_global_weeks <- max(games_prep$global_week)

# 3) Team-season indices for HFA deviations
games_prep <- games_prep |>
  mutate(
    team_season_home = (season_id - 1L) * n_teams + team_home,
    team_season_away = (season_id - 1L) * n_teams + team_away
  )

n_team_season <- n_teams * n_seasons

# 4) Build INLA-specific columns
games_inla <- games_prep |>
  mutate(
    y = result,
    idx_week_home = global_week, # RW1 time index for home
    idx_week_away = global_week, # same time index, copied for away
    season_id_league = season_id
  )

glimpse(games_inla)

# ============================================================================ #
# 2. INLA Model Formula (Gaussian result model with RW1 team strength) ----
# ============================================================================ #

formula_inla <- y ~ 0 +
  # Team strength: RW1 over global weeks, replicated per team (home side)
  # f(idx_week_home, model = "rw1", replicate = team_home, constr = TRUE) +
  f(idx_week_home, model = "ar1", replicate = team_home, constr = TRUE) +
  # Team strength: same latent field, copied with scale -1 for away side
  f(idx_week_away, copy = "idx_week_home", fixed = FALSE, param = c(-1, 0)) +
  # League-wide HFA: AR1 over seasons
  f(season_id_league, model = "ar1") +
  # Team-season HFA deviations (for home team only), sum-to-zero across all
  f(team_season_home, model = "iid", constr = TRUE)

# Alternative formula that more closely mirrors the Stan structure (kept commented
# out for now; swap in to experiment).
# formula_inla_ar1 <- y ~ 0 +
#   f(
#     idx_week_home,
#     model = "ar1",
#     replicate = team_home,
#     constr = TRUE,
#     hyper = list(
#       rho = list(prior = "beta", param = c(9, 1)),
#       prec = list(prior = "pc.prec", param = c(1, 0.05))
#     )
#   ) +
#   f(
#     idx_week_away,
#     copy = "idx_week_home",
#     fixed = FALSE,
#     param = c(-1, 0)
#   ) +
#   f(
#     season_id_league,
#     model = "ar1",
#     constr = TRUE,
#     hyper = list(
#       rho = list(prior = "beta", param = c(8, 2)),
#       prec = list(prior = "pc.prec", param = c(1, 0.05))
#     )
#   ) +
#   f(
#     team_season_home,
#     model = "iid",
#     constr = TRUE,
#     hyper = list(
#       prec = list(prior = "pc.prec", param = c(1, 0.05))
#     )
#   )

# ============================================================================ #
# 3. Fit on 2002-2024 and Predict 2025 Week 1 (GQ-style) ----
# ============================================================================ #

s <- 2025
w <- 1

message(
  "Fitting INLA model for training up to season ",
  s - 1,
  " and predicting season ",
  s,
  " week ",
  w
)

# Training data: all seasons < s (2002-2024)
train_data <- games_inla |>
  filter(
    season < s | (season == s & week < w),
    !is.na(y) # only completed games for fitting
  )

# Last observed week in training (filtered state): should be 2024 W22
last_train_global_week <- max(train_data$global_week)
last_train_season <- max(train_data$season[
  train_data$global_week == last_train_global_week
])
last_train_week <- max(train_data$week[
  train_data$global_week == last_train_global_week
])
last_train_season_id <- max(train_data$season_id[
  train_data$global_week == last_train_global_week
])

# Games we want to predict this iteration (season s, week w)
new_data <- games_inla |>
  filter(season == s, week == w)

if (nrow(new_data) == 0L) {
  stop("No games found for season ", s, " week ", w, " in games_inla.")
}

# store actual result for later comparison, but set y=NA for prediction
new_data <- new_data |>
  mutate(
    y_true = y,
    y = NA_real_
  )

pred_global_week <- unique(new_data$global_week)
pred_season_id <- unique(new_data$season_id)
pred_season <- unique(new_data$season)
pred_week <- unique(new_data$week)

# Combined data: training + prediction rows
combined_data <- bind_rows(train_data, new_data)
n_train <- nrow(train_data)
n_total <- nrow(combined_data)

# Indices of prediction rows (2025 W1)
pred_idx <- seq.int(n_train + 1L, n_total)

# ============================================================================ #
# 4. Single INLA fit with config=TRUE (enables posterior sampling) ----
# ============================================================================ #

tic("INLA fit (training + 2025W1 as NA)")
fit_inla <- inla(
  formula_inla,
  data = combined_data,
  family = "gaussian",
  verbose = TRUE,
  control.predictor = list(
    compute = TRUE,
    link = 1
  ),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = FALSE,
    config = TRUE # <-- enables inla.posterior.sample()
  )
)
toc()

fit_inla_sum <- summary(fit_inla, digits = 4)
print(fit_inla_sum, digits = 4)

# For reference, show stan model summary of similar variables
fit_stan_meta <- fit$metadata()
fit$print(variables = str_subset(fit_stan_meta$stan_variables, "phi|sigma"))

# ============================================================================ #
# 5. Posterior sampling ----
# ============================================================================ #

nsamp <- 4000 # choose similar to Stan
tic("INLA posterior sampling")
samples <- inla.posterior.sample(nsamp, fit_inla)
toc()

latent_names <- rownames(samples[[1]]$latent)
str(latent_names)
unique(sub("^([^:]+):.*", "\\1", latent_names))

hyper_names <- names(samples[[1]]$hyperpar)
hyper_names

# helper: extract draws for a subset of latent nodes
extract_latent_draws <- function(mask) {
  idx <- which(mask)
  out <- matrix(NA_real_, nrow = nsamp, ncol = length(idx))
  for (i in seq_len(nsamp)) {
    out[i, ] <- samples[[i]]$latent[idx]
  }
  colnames(out) <- latent_names[idx]
  out
}

# ============================================================================ #
# 6. League HFA (AR1) as rvar, filtered & predicted ----
# ============================================================================ #

league_mask <- grepl("^season_id_league", latent_names)
league_draws <- extract_latent_draws(league_mask)

# parse season index from latent names: "season_id_league:k"
season_idx_tbl <- tibble(
  latent_name = colnames(league_draws)
) |>
  tidyr::extract(
    latent_name,
    into = c("field", "season_idx_raw"),
    regex = "^(season_id_league):(\\d+)$",
    remove = FALSE,
    convert = TRUE
  ) |>
  mutate(season_idx = season_idx_raw)

league_hfa_rvar <- posterior::rvar(league_draws)

# filtered (last train season = 2024)
filtered_league_col <- which(season_idx_tbl$season_idx == last_train_season_id)
filtered_league_hfa_inla <- tibble(
  season_idx = last_train_season_id,
  week_idx = last_train_global_week,
  season = last_train_season,
  week = last_train_week,
  filtered_league_hfa = league_hfa_rvar[filtered_league_col]
)

# predicted (next season = 2025)
pred_league_col <- which(season_idx_tbl$season_idx == pred_season_id)
predicted_league_hfa_inla <- tibble(
  season_idx = pred_season_id,
  week_idx = pred_global_week,
  season = pred_season,
  week = pred_week,
  predicted_league_hfa = league_hfa_rvar[pred_league_col]
)

# ============================================================================ #
# 7. Team HFA deviations per team & season as rvar ----
# ============================================================================ #

team_hfa_mask <- grepl("^team_season_home", latent_names)
team_hfa_draws <- extract_latent_draws(team_hfa_mask)

team_hfa_idx_tbl <- tibble(
  latent_name = colnames(team_hfa_draws)
) |>
  tidyr::extract(
    latent_name,
    into = c("field", "ts_idx"),
    regex = "^(team_season_home):(\\d+)$",
    remove = FALSE,
    convert = TRUE
  ) |>
  mutate(
    season_id = ((ts_idx - 1L) %/% n_teams) + 1L,
    team_id = ((ts_idx - 1L) %% n_teams) + 1L,
    team = team_levels[team_id]
  )

team_hfa_rvar <- posterior::rvar(team_hfa_draws)

# filtered team HFA at last_train_season_id
filtered_hfa_rows <- team_hfa_idx_tbl |>
  filter(season_id == last_train_season_id) |>
  arrange(team_id)

filtered_team_hfa_rvars <- lapply(filtered_hfa_rows$latent_name, function(nm) {
  col_idx <- which(colnames(team_hfa_draws) == nm)
  team_hfa_rvar[col_idx]
})

filtered_team_hfa_inla <- tibble(
  season_idx = last_train_season_id,
  week_idx = last_train_global_week,
  season = last_train_season,
  week = last_train_week,
  team = filtered_hfa_rows$team,
  filtered_team_hfa = do.call(c, filtered_team_hfa_rvars)
)

# predicted team HFA at pred_season_id
pred_hfa_rows <- team_hfa_idx_tbl |>
  filter(season_id == pred_season_id) |>
  arrange(team_id)

pred_team_hfa_rvars <- lapply(pred_hfa_rows$latent_name, function(nm) {
  col_idx <- which(colnames(team_hfa_draws) == nm)
  team_hfa_rvar[col_idx]
})

predicted_team_hfa_inla <- tibble(
  season_idx = pred_season_id,
  week_idx = pred_global_week,
  season = pred_season,
  week = pred_week,
  team = pred_hfa_rows$team,
  predicted_team_hfa = do.call(c, pred_team_hfa_rvars)
)

# ============================================================================ #
# 8. RW1 team strengths per (team, global_week) as rvar ----
# ============================================================================ #

# RW1 field with replicate => names typically "idx_week_home:global_week:replicate"
strength_mask <- grepl("^idx_week_home", latent_names)
strength_draws <- extract_latent_draws(strength_mask)
str(strength_draws)

strength_idx_tbl <- tibble(
  latent_name = colnames(strength_draws)
) |>
  mutate(
    latent_idx = as.integer(sub("^idx_week_home:", "", latent_name)),
    global_week = ((latent_idx - 1L) %% n_global_weeks) + 1L,
    team_id = ((latent_idx - 1L) %/% n_global_weeks) + 1L,
    team = team_levels[team_id]
  )
strength_idx_tbl

strength_rvar <- posterior::rvar(strength_draws)
str(strength_rvar)

# FILTERED strengths: last_train_global_week & last_train_season_id
filtered_strength_rows <- strength_idx_tbl |>
  filter(global_week == last_train_global_week) |>
  arrange(team_id)

filtered_strength_rvars <- lapply(
  filtered_strength_rows$latent_name,
  function(nm) {
    col_idx <- which(colnames(strength_draws) == nm)
    strength_rvar[col_idx]
  }
)

filtered_strengths_inla <- tibble(
  season_idx = last_train_season_id,
  week_idx = last_train_global_week,
  season = last_train_season,
  week = last_train_week,
  team = filtered_strength_rows$team,
  filtered_team_strength = do.call(c, filtered_strength_rvars)
) |>
  left_join(
    filtered_team_hfa_inla |> select(team, filtered_team_hfa),
    by = "team"
  )
filtered_strengths_inla

# PREDICTED strengths: pred_global_week & pred_season_id
pred_strength_rows <- strength_idx_tbl |>
  filter(global_week == pred_global_week) |>
  arrange(team_id)

pred_strength_rvars <- lapply(pred_strength_rows$latent_name, function(nm) {
  col_idx <- which(colnames(strength_draws) == nm)
  strength_rvar[col_idx]
})

predicted_strengths_inla <- tibble(
  week_idx = pred_global_week,
  team = pred_strength_rows$team,
  predicted_team_strength = do.call(c, pred_strength_rvars)
) |>
  left_join(predicted_team_hfa_inla, by = c("week_idx", "team")) |>
  mutate(
    season_idx = pred_season_id,
    season = pred_season,
    week = pred_week
  ) |>
  relocate(season_idx, week_idx, season, week, .before = team)
predicted_strengths_inla

# ============================================================================ #
# 9. Linear predictor & predictive distributions for games ----
# ============================================================================ #

# Linear predictor nodes "Predictor:i"
lp_mask <- grepl("^Predictor", latent_names)
lp_draws <- extract_latent_draws(lp_mask)
lp_rvar <- posterior::rvar(lp_draws) # [draw, row_of_combined_data]

# Observation noise (sigma_obs)
obs_prec_idx <- grep("Precision for the Gaussian observations", hyper_names)
obs_prec_draws <- sapply(samples, function(s) s$hyperpar[obs_prec_idx])
obs_sigma_draws <- 1 / sqrt(obs_prec_draws)

## Filtered_result for last observed week (2024 W22) ----

# rows in combined_data corresponding to last training week
last_week_rows <- which(
  combined_data$global_week == last_train_global_week &
    combined_data$season == last_train_season
)
stopifnot(all(last_week_rows <= n_train))

# helper: make rvar column from lp_rvar per row
mu_last_list <- lapply(last_week_rows, function(i_row) {
  lp_rvar[i_row]
})

# predictive y = mu + eps, eps ~ N(0, sigma_obs)
set.seed(123)
y_last_matrix <- matrix(NA_real_, nrow = nsamp, ncol = length(last_week_rows))
for (i in seq_len(nsamp)) {
  mu_i <- as.numeric(lp_draws[i, last_week_rows])
  y_last_matrix[i, ] <- rnorm(
    length(mu_i),
    mean = mu_i,
    sd = obs_sigma_draws[i]
  )
}
y_last_rvars <- lapply(seq_along(last_week_rows), function(j) {
  posterior::rvar(y_last_matrix[, j])
})

# build filtered_result tibble
last_week_games <- combined_data[last_week_rows, ] |>
  mutate(
    home_team = team_levels[team_home],
    away_team = team_levels[team_away]
  )

# match strengths & HFA by home/away team
filtered_strengths_inla_expanded <- filtered_strengths_inla |>
  select(team, filtered_team_strength, filtered_team_hfa)

filtered_result_inla <- last_week_games |>
  mutate(row_id = row_number()) |>
  left_join(filtered_strengths_inla_expanded, by = c("home_team" = "team")) |>
  rename(
    home_strength = filtered_team_strength,
    home_hfa = filtered_team_hfa
  ) |>
  left_join(filtered_strengths_inla_expanded, by = c("away_team" = "team")) |>
  rename(
    away_strength = filtered_team_strength
  ) |>
  mutate(
    sigma = posterior::rvar(matrix(obs_sigma_draws, ncol = 1)),
    mu = do.call(c, mu_last_list),
    y = do.call(c, y_last_rvars)
  ) |>
  transmute(
    game_idx,
    game_id,
    season_idx = last_train_season_id,
    week_idx = last_train_global_week,
    season = last_train_season,
    week = last_train_week,
    hfa = dplyr::if_else(location == "Home", 1L, 0L, missing = 0L),
    home_team,
    away_team,
    home_strength,
    home_hfa,
    away_strength,
    sigma,
    mu,
    y
  )

## Predicted_result for 2025 W1 ----

# rows for 2025 W1 predictions
pred_rows <- pred_idx

mu_pred_list <- lapply(pred_rows, function(i_row) {
  lp_rvar[i_row]
})

# predictive y = mu + eps for 2025 W1
set.seed(456)
y_pred_matrix <- matrix(NA_real_, nrow = nsamp, ncol = length(pred_rows))
for (i in seq_len(nsamp)) {
  mu_i <- as.numeric(lp_draws[i, pred_rows])
  y_pred_matrix[i, ] <- rnorm(
    length(mu_i),
    mean = mu_i,
    sd = obs_sigma_draws[i]
  )
}
y_pred_rvars <- lapply(seq_along(pred_rows), function(j) {
  posterior::rvar(y_pred_matrix[, j])
})

pred_week_games <- combined_data[pred_rows, ] |>
  mutate(
    home_team = team_levels[team_home],
    away_team = team_levels[team_away]
  )

predicted_strengths_inla_expanded <- predicted_strengths_inla |>
  select(week_idx, team, predicted_team_strength, predicted_team_hfa)

predicted_result_inla <- pred_week_games |>
  mutate(row_id = row_number()) |>
  left_join(
    predicted_strengths_inla_expanded,
    by = c("global_week" = "week_idx", "home_team" = "team")
  ) |>
  rename(
    home_strength = predicted_team_strength,
    home_hfa = predicted_team_hfa
  ) |>
  left_join(
    predicted_strengths_inla_expanded,
    by = c("global_week" = "week_idx", "away_team" = "team")
  ) |>
  rename(
    away_strength = predicted_team_strength
  ) |>
  mutate(
    sigma = posterior::rvar(matrix(obs_sigma_draws, ncol = 1)),
    mu = do.call(c, mu_pred_list),
    y = do.call(c, y_pred_rvars)
  ) |>
  transmute(
    game_idx,
    game_id,
    season_idx = pred_season_id,
    week_idx = pred_global_week,
    season = pred_season,
    week = pred_week,
    hfa = dplyr::if_else(location == "Home", 1L, 0L, missing = 0L),
    home_team,
    away_team,
    home_strength,
    home_hfa,
    away_strength,
    sigma,
    mu,
    y
  )

# ============================================================================ #
# 10. Final objects analogous to Stan output ----
# ============================================================================ #

filtered_league_hfa_inla # 1-row tibble with rvar league HFA at last week
filtered_strengths_inla # 32-row tibble with filtered_team_strength & filtered_team_hfa
filtered_result_inla # game-level tibble for last observed week

predicted_league_hfa_inla # 1-row tibble with rvar league HFA for 2025 W1
predicted_strengths_inla # 32-row tibble with predicted_team_strength & predicted_team_hfa
predicted_result_inla # game-level tibble for 2025 W1 predictions


# ============================================================================ #
# 11. Final objects FROM Stan output ----
# ============================================================================ #
# Available from previously running update_team_strength_model.r

print(filtered_league_hfa, n = Inf)
print(filtered_strengths, n = Inf)
print(data.frame(filtered_result))

print(predicted_league_hfa, n = Inf)
print(predicted_strengths, n = Inf)
print(data.frame(predicted_result))


# ============================================================================ #
# 12. Compare INLA vs Stan outputs ----
# ============================================================================ #

stan_objects <- c(
  "filtered_league_hfa",
  "filtered_strengths",
  "filtered_result",
  "predicted_league_hfa",
  "predicted_strengths",
  "predicted_result"
)

have_stan_objects <- all(vapply(stan_objects, exists, logical(1)))

if (!have_stan_objects) {
  warning(
    "Stan objects not found in this session. ",
    "Run update_team_strength_model.r before comparing outputs."
  )
} # else {
rvar_mean_sd <- function(rv) {
  draws <- posterior::draws_of(rv)
  c(
    mean = base::mean(draws),
    sd = stats::sd(draws)
  )
}

structure_check_tbl <- tibble(
  object = c(
    "filtered_league_hfa",
    "filtered_strengths",
    "filtered_result",
    "predicted_league_hfa",
    "predicted_strengths",
    "predicted_result"
  ),
  inla_rows = c(
    nrow(filtered_league_hfa_inla),
    nrow(filtered_strengths_inla),
    nrow(filtered_result_inla),
    nrow(predicted_league_hfa_inla),
    nrow(predicted_strengths_inla),
    nrow(predicted_result_inla)
  ),
  stan_rows = c(
    nrow(filtered_league_hfa),
    nrow(filtered_strengths),
    nrow(filtered_result),
    nrow(predicted_league_hfa),
    nrow(predicted_strengths),
    nrow(predicted_result)
  ),
  columns_match = c(
    identical(
      names(filtered_league_hfa_inla),
      names(filtered_league_hfa)
    ),
    identical(
      names(filtered_strengths_inla),
      names(filtered_strengths)
    ),
    identical(
      names(filtered_result_inla),
      names(filtered_result)
    ),
    identical(
      names(predicted_league_hfa_inla),
      names(predicted_league_hfa)
    ),
    identical(
      names(predicted_strengths_inla),
      names(predicted_strengths)
    ),
    identical(
      names(predicted_result_inla),
      names(predicted_result)
    )
  )
)
print(structure_check_tbl)

missing_filtered_teams <- base::setdiff(
  filtered_strengths$team,
  filtered_strengths_inla$team
)
missing_predicted_teams <- base::setdiff(
  predicted_strengths$team,
  predicted_strengths_inla$team
)

if (length(missing_filtered_teams) > 0) {
  message(
    "Teams missing from filtered_strengths_inla: ",
    paste(missing_filtered_teams, collapse = ", ")
  )
}
if (length(missing_predicted_teams) > 0) {
  message(
    "Teams missing from predicted_strengths_inla: ",
    paste(missing_predicted_teams, collapse = ", ")
  )
}

compare_single_rvar <- function(tbl, col, method_label) {
  col_quo <- enquo(col)
  stats_vec <- rvar_mean_sd(dplyr::pull(tbl, !!col_quo))

  tbl |>
    summarise(
      season_idx = dplyr::first(season_idx),
      week_idx = dplyr::first(week_idx),
      season = dplyr::first(season),
      week = dplyr::first(week)
    ) |>
    mutate(
      mean = stats_vec["mean"],
      sd = stats_vec["sd"],
      method = method_label,
      .before = 1
    )
}

filtered_league_compare <- bind_rows(
  compare_single_rvar(
    filtered_league_hfa_inla,
    filtered_league_hfa,
    "inla"
  ),
  compare_single_rvar(
    filtered_league_hfa,
    filtered_league_hfa,
    "stan"
  )
) |>
  tidyr::pivot_wider(
    names_from = method,
    values_from = c(mean, sd),
    names_glue = "{.value}_{method}"
  ) |>
  mutate(
    mean_diff = mean_inla - mean_stan,
    sd_diff = sd_inla - sd_stan
  )

predicted_league_compare <- bind_rows(
  compare_single_rvar(
    predicted_league_hfa_inla,
    predicted_league_hfa,
    "inla"
  ),
  compare_single_rvar(
    predicted_league_hfa,
    predicted_league_hfa,
    "stan"
  )
) |>
  tidyr::pivot_wider(
    names_from = method,
    values_from = c(mean, sd),
    names_glue = "{.value}_{method}"
  ) |>
  mutate(
    mean_diff = mean_inla - mean_stan,
    sd_diff = sd_inla - sd_stan
  )

summarise_strength_tbl <- function(tbl, strength_col, hfa_col, prefix) {
  tbl |>
    rowwise() |>
    mutate(
      strength_mean = as.numeric(rvar_mean_sd({{ strength_col }})["mean"]),
      strength_sd = as.numeric(rvar_mean_sd({{ strength_col }})["sd"]),
      hfa_mean = as.numeric(rvar_mean_sd({{ hfa_col }})["mean"]),
      hfa_sd = as.numeric(rvar_mean_sd({{ hfa_col }})["sd"])
    ) |>
    ungroup() |>
    select(
      any_of(c("season_idx", "week_idx", "season", "week")),
      team,
      strength_mean,
      strength_sd,
      hfa_mean,
      hfa_sd
    ) |>
    rename_with(
      ~ paste(prefix, ., sep = "_"),
      c("strength_mean", "strength_sd", "hfa_mean", "hfa_sd")
    )
}

filtered_strengths_compare <- full_join(
  summarise_strength_tbl(
    filtered_strengths_inla,
    filtered_team_strength,
    filtered_team_hfa,
    "inla"
  ),
  summarise_strength_tbl(
    filtered_strengths,
    filtered_team_strength,
    filtered_team_hfa,
    "stan"
  ),
  by = c("season_idx", "week_idx", "season", "week", "team")
) |>
  mutate(
    strength_mean_diff = inla_strength_mean - stan_strength_mean,
    hfa_mean_diff = inla_hfa_mean - stan_hfa_mean
  )

predicted_strengths_compare <- full_join(
  summarise_strength_tbl(
    predicted_strengths_inla,
    predicted_team_strength,
    predicted_team_hfa,
    "inla"
  ),
  summarise_strength_tbl(
    predicted_strengths |>
      mutate(
        season_idx = pred_season_id,
        season = pred_season,
        week = pred_week,
        .before = week_idx
      ),
    predicted_team_strength,
    predicted_team_hfa,
    "stan"
  ),
  by = c("season_idx", "week_idx", "season", "week", "team")
) |>
  mutate(
    strength_mean_diff = inla_strength_mean - stan_strength_mean,
    hfa_mean_diff = inla_hfa_mean - stan_hfa_mean
  )

summarise_result_tbl <- function(tbl, prefix) {
  tbl |>
    rowwise() |>
    mutate(
      mu_mean = as.numeric(rvar_mean_sd(mu)["mean"]),
      mu_sd = as.numeric(rvar_mean_sd(mu)["sd"]),
      y_mean = as.numeric(rvar_mean_sd(y)["mean"]),
      y_sd = as.numeric(rvar_mean_sd(y)["sd"]),
      sigma_mean = as.numeric(rvar_mean_sd(sigma)["mean"]),
      sigma_sd = as.numeric(rvar_mean_sd(sigma)["sd"])
    ) |>
    ungroup() |>
    select(
      game_idx,
      game_id,
      season_idx,
      week_idx,
      season,
      week,
      home_team,
      away_team,
      mu_mean,
      mu_sd,
      y_mean,
      y_sd,
      sigma_mean,
      sigma_sd
    ) |>
    rename_with(
      ~ paste(prefix, ., sep = "_"),
      c("mu_mean", "mu_sd", "y_mean", "y_sd", "sigma_mean", "sigma_sd")
    )
}

filtered_result_compare <- full_join(
  summarise_result_tbl(filtered_result_inla, "inla"),
  summarise_result_tbl(filtered_result, "stan"),
  by = c(
    "game_idx",
    "game_id",
    "season_idx",
    "week_idx",
    "season",
    "week",
    "home_team",
    "away_team"
  )
) |>
  mutate(
    mu_mean_diff = inla_mu_mean - stan_mu_mean,
    y_mean_diff = inla_y_mean - stan_y_mean,
    sigma_mean_diff = inla_sigma_mean - stan_sigma_mean
  )

predicted_result_compare <- full_join(
  summarise_result_tbl(predicted_result_inla, "inla"),
  summarise_result_tbl(predicted_result, "stan"),
  by = c(
    "game_idx",
    "game_id",
    "season_idx",
    "week_idx",
    "season",
    "week",
    "home_team",
    "away_team"
  )
) |>
  mutate(
    mu_mean_diff = inla_mu_mean - stan_mu_mean,
    y_mean_diff = inla_y_mean - stan_y_mean,
    sigma_mean_diff = inla_sigma_mean - stan_sigma_mean
  )
# }

filtered_league_compare
predicted_league_compare
filtered_strengths_compare
predicted_strengths_compare
filtered_result_compare
predicted_result_compare
