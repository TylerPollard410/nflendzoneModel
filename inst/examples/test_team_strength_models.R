# CURRENT WEEK FIT - Using nflendzoneModel Package
#
# This script reproduces current_week_fit.R functionality using the
# nflendzoneModel package as an external user would.
#
# Prerequisites:
# - Package installed: devtools::install() or install_github()
# - Required data packages: nflendzone (or nflreadr for testing)
# devtools::install_github("TylerPollard410/nflendzoneModel")

library(arrow)
library(lubridate)
library(piggyback)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(tictoc)

library(posterior)
library(tidybayes)
library(cmdstanr)
library(bayesplot)

library(KFAS)
library(bssm)

library(nflreadr)
library(nflfastR)
library(nflseedR)

library(nflendzoneModel)
library(nflendzonePipeline)
library(nflendzone)

set.seed(52)

# ============================================================================ #
# 0. Load Data ----
# ============================================================================ #

# Global variables
github_data_repo <- "TylerPollard410/nflendzoneData"
github_releases_base_url <- paste0(
  "https://github.com/",
  github_data_repo,
  "/releases/download/"
)

# Get teams and seasons
# Replace with: teams <- nflendzone::load_teams(current = TRUE)$team_abbr
teams <- nflreadr::load_teams(current = TRUE)$team_abbr
all_seasons <- 2002:nflreadr::get_current_season()
current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

#current_season <- 2006
#current_week <- 1

# Load game data
# game_data_full <- nflendzone::load_game_data(seasons = all_seasons)
game_data_full <- nflendzone::load_game_data(
  seasons = (current_season - 5):current_season
)

# ============================================================================ #
# 1. Prepare Data for Fitting ----
# ============================================================================ #

# Prepare full schedule with indices (needed for GQ)
schedule_idx <- prepare_schedule_indices(
  game_data_full,
  teams
)

# Filter to training data (before current season, only completed games)
training_data <- schedule_idx |>
  filter(
    season < current_season |
      (season == current_season & week < current_week),
    !is.na(result)
  )

# Create Stan data
fit_stan_data <- prepare_stan_data(
  game_data = training_data,
  teams = teams,
  verbose = TRUE
)

# Roll forward to include current season up to current_week - 1
# fit_stan_data <- roll_forward_fit_stan_data(
#   fit_stan_data,
#   schedule_idx,
#   weeks_ahead = current_week - 1
# )

# ============================================================================ #
# 2. Prepare GQ Data for Predictions ----
# ============================================================================ #

# Get targets for next week
gq_targets <- next_week_targets(fit_stan_data, horizon = 1L)

# Prepare GQ data
gq_stan_data <- prepare_gq_data(
  fit_stan_data = fit_stan_data,
  schedule_df = schedule_idx,
  target_weeks = gq_targets
)

# ============================================================================ #
# 3. Fit Model ----
# ============================================================================ #

## 3.0 previous fitting parameters ----
historic_hyperparams <- pb_list(
  repo = github_data_repo,
  tag = "team_strength_fit_hyperparams"
) |>
  pull(file_name) |>
  str_subset(pattern = paste0("team_strength_fit_hyperparams_[:digit:]")) |>
  map(\(x) {
    pb_read(
      file = x,
      repo = github_data_repo,
      tag = "team_strength_fit_hyperparams"
    )
  }) |>
  list_rbind() |>
  arrange(variable, season)

## 3.1 Fit Model ----
# Globals
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = parallel::detectCores()
fit_warm = 1000
fit_samps = 1000
fit_thin = 1
fit_adapt_delta = 0.90
fit_max_treedepth = 10

cat("\n=== Fitting Model ===\n")

fit_univar_normal <- fit_team_strength_model(
  model = "team_strength_fit",
  stan_data = fit_stan_data,
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit_univar_student <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_univar_student",
  seed = fit_seed,
  # init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit_bivar_normal <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_bivar_normal",
  seed = fit_seed,
  # init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit_bivar_poisson <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_bivar_poisson",
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

fit_bivar_negbinom <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_bivar_negbinom",
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)

mod_nb2 <- cmdstan_model("src/stan/team_strength_bivar_negbinom.stan")
fit_bivar_negbinom2 <- mod_nb2$sample(
  data = fit_stan_data,
  # model = "team_strength_bivar_negbinom",
  seed = fit_seed,
  init = 0,
  sig_figs = fit_sig_figs,
  chains = fit_chains,
  parallel_chains = fit_parallel,
  iter_warmup = fit_warm,
  iter_sampling = fit_samps,
  thin = fit_thin,
  adapt_delta = fit_adapt_delta,
  max_treedepth = fit_max_treedepth
)
fit_bivar_negbinom2$diagnostic_summary()
fit_bivar_negbinom2$cmdstan_diagnose()

fit_names <- c(
  "univar_normal",
  "univar_student",
  "bivar_normal",
  "bivar_poisson",
  "bivar_negbinom",
  "bivar_negbinom2"
)

fit_list <- list(
  fit_univar_normal,
  fit_univar_student,
  fit_bivar_normal,
  fit_bivar_poisson,
  fit_bivar_negbinom,
  fit_bivar_negbinom2
) |>
  set_names(fit_names)

## 3.2 Save Fit Outputs ----
fit_list |>
  imap(
    \(.fit, .name) {
      cat("\n=== Saving Fit Outputs for", .name, "===\n")
      tic()
      .fit$save_object(
        file = paste0("artifacts/model-archive/team_strength/", .name, ".rds")
      )
      toc()
    }
  )
fit_bivar_negbinom2$save_object(
  file = paste0(
    "artifacts/model-archive/team_strength/",
    "bivar_negbinom2",
    ".rds"
  )
)

fit_loo <- fit_list |>
  map(\(.fit) .fit$loo())
fit_loo |> loo::loo_compare()
fit_loo |> loo::loo_model_weights()

# ============================================================================ #
# 4. Generate Predictions ----
# ============================================================================ #

# cat("\n=== Generating Predictions ===\n")

# gq <- predict_team_strength(
#   draws = fit,
#   gq_data = gq_stan_data,
#   parallel_chains = fit_parallel,
#   seed = fit_seed,
#   sig_figs = fit_sig_figs
# )

# gq_files <- gq$save_output_files(
#   dir = "artifacts/model-archive/team_strength",
#   basename = "team_strength_gq",
#   timestamp = FALSE,
#   random = FALSE
# )

# ============================================================================ #
# 5. Extract Prediction Data ----
# ============================================================================ #

# Get OOS games
oos_games <- schedule_idx |>
  filter(week_idx %in% gq_targets)

# Extract predictions
# predictions <- extract_game_predictions(
#   gq = gq,
#   schedule_df = oos_games
# )

# ID variables
filter_week_idx <- fit_stan_data$N_weeks
filter_season <- schedule_idx |>
  filter(week_idx == filter_week_idx) |>
  pull(season) |>
  unique()
filter_week <- schedule_idx |>
  filter(week_idx == filter_week_idx) |>
  pull(week) |>
  unique()

predict_week_idx <- gq_targets[1]
predict_season <- schedule_idx |>
  filter(week_idx == predict_week_idx) |>
  pull(season) |>
  unique()
predict_week <- schedule_idx |>
  filter(week_idx == predict_week_idx) |>
  pull(week) |>
  unique()

# ============================================================================ #
# 6. Extract Team Strengths ----
# ============================================================================ #

## Draws ----
# Extract team strengths with rvars
#fit_draws <- fit$draws()
#fit_rvars <- as_draws_rvars(fit_draws)

fit_rvars_list <- fit_list |>
  map(
    \(.fit) {
      .fit$draws() |>
        as_draws_rvars()
    }
  )

# gq_draws <- gq$draws()
# gq_rvars <- as_draws_rvars(gq_draws)

## Hyperparameters + helpers (apples-to-apples) ----
extract_hyperparams <- c(
  # League/Team HFA process
  "phi_league_hfa",
  "sigma_league_hfa_innovation",
  "league_hfa_init",
  "sigma_team_hfa",
  # Strength init scales
  "sigma_team_strength_init",
  "sigma_team_off_init",
  "sigma_team_def_init",
  # Weekly persistence + scales
  "phi_weekly_team_strength_innovation",
  "sigma_weekly_team_strength_innovation",
  "phi_weekly_off",
  "phi_weekly_def",
  "sigma_weekly_off_innov",
  "sigma_weekly_def_innov",
  # Season persistence + scales
  "phi_season_team_strength_innovation",
  "sigma_season_team_strength_innovation",
  "phi_season_off",
  "phi_season_def",
  "sigma_season_off_innov",
  "sigma_season_def_innov",
  # Observation/intercepts
  "nu_obs",
  "sigma_obs", # univar student
  "alpha_score_points", # bivar normal
  "alpha_score", # bivar poisson (log-scale)
  "alpha_score_raw",
  "log_phi_home",
  "log_phi_away",
  "sigma_alpha_log_dev_innovation",
  "phi_alpha_log"
)

fit_hyperparams_list <- fit_rvars_list |>
  purrr::map(\(x) {
    keep_at(x, stringr::str_subset(names(x), "phi|sigma|alpha"))
  })

fit_hyperparams <- fit_hyperparams_list |>
  imap(\(.rvars, .name) {
    .rvars |>
      summarise_draws() #|>
    # mutate(model = .name, .before = 1)
  }) |>
  list_rbind(names_to = "model") |>
  mutate(model = factor(model, levels = fit_names)) |>
  arrange(model, variable)

## Team Strengths ----
fit_strengths_list <- fit_rvars_list |>
  purrr::map(\(x) {
    keep_at(x, stringr::str_subset(names(x), "filtered|predicted"))
  })

fit_strengths <- fit_strengths_list |>
  purrr::imap(
    \(.rvars, .name) {
      if (stringr::str_detect(.name, "univar")) {
        .rvars |>
          spread_rvars(
            filtered_team_strength[team],
            filtered_team_hfa[team],
            filtered_league_hfa[season],
            predicted_team_strength[team],
            predicted_team_hfa[team],
            predicted_league_hfa[season]
          ) |>
          dplyr::mutate(
            model = .name,
            season = current_season,
            team = teams[team],
            .before = 1
          )
      } else {
        .rvars |>
          spread_rvars(
            filtered_team_off_strength[team],
            filtered_team_def_strength[team],
            filtered_team_hfa[team],
            filtered_league_hfa[season],
            predicted_team_off_strength[team],
            predicted_team_def_strength[team],
            predicted_team_hfa[team],
            predicted_league_hfa[season]
          ) |>
          dplyr::mutate(
            filtered_team_strength = filtered_team_off_strength +
              filtered_team_def_strength,
            predicted_team_strength = predicted_team_off_strength +
              predicted_team_def_strength,
            model = .name,
            season = current_season,
            team = teams[team],
            .before = 1
          )
      }
    }
  ) |>
  list_rbind() |>
  dplyr::mutate(model = factor(model, levels = fit_names)) |>
  dplyr::select(
    model,
    season,
    team,
    contains("filtered"),
    contains("predicted")
  ) |>
  dplyr::relocate(
    "filtered_team_off_strength",
    "filtered_team_def_strength",
    .before = filtered_team_strength
  ) |>
  dplyr::relocate(
    "predicted_team_off_strength",
    "predicted_team_def_strength",
    .before = predicted_team_strength
  ) |>
  dplyr::arrange(team, model)


possible_exprs <- c(
  "alpha_log[season]",
  "filtered_team_strength[team]",
  "filtered_team_off_strength[team]",
  "filtered_team_def_strength[team]",
  "filtered_team_hfa[team]",
  "filtered_league_hfa",
  "predicted_team_strength[team]",
  "predicted_team_off_strength[team]",
  "predicted_team_def_strength[team]",
  "predicted_team_hfa[team]",
  "predicted_league_hfa"
)

## Compare Univar vs Bivar ----
comp_list <- fit_rvars_list |>
  keep_at(c("univar_normal", "bivar_negbinom", "bivar_negbinom2"))
comp_list |>
  map_depth(2, \(x) attr(x, "dims") <- dim(x))

comp_fits <- comp_list |>
  imap(
    \(.rvars, .name) {
      rvars_expr <- str_subset(
        possible_exprs,
        str_c(names(.rvars), collapse = "|")
      ) |>
        rlang::parse_exprs()

      out <- .rvars |>
        spread_rvars(
          !!!rvars_expr
        )

      if (str_detect(.name, "bivar_negbinom")) {
        # Global log-scale intercept for expected scoring
        #alpha_log <- .rvars$alpha_log

        # Neutral-field expected points vs league-average opponent
        out <- out |>
          arrange(season) |>
          mutate(alpha_log = tail(alpha_log, n = 1)) |>
          filter(season == max(season)) |>
          select(-season) |>
          mutate(
            filtered_off_points_neutral = exp(
              alpha_log + filtered_team_off_strength
            ),
            filtered_def_points_neutral = exp(
              alpha_log - filtered_team_def_strength
            ),
            predicted_off_points_neutral = exp(
              alpha_log + predicted_team_off_strength
            ),
            predicted_def_points_neutral = exp(
              alpha_log - predicted_team_def_strength
            ),

            # Margin (points) comparable to univar_normal
            filtered_team_strength = filtered_off_points_neutral -
              filtered_def_points_neutral,
            predicted_team_strength = predicted_off_points_neutral -
              predicted_def_points_neutral,

            # Convert HFA rvars from log-rate to point increments (home-only effect)
            # Neutral baseline: exp(alpha_log); increment: exp(alpha_log + hfa) - exp(alpha_log)
            filtered_team_hfa = exp(alpha_log + filtered_team_hfa) -
              exp(alpha_log),
            predicted_team_hfa = exp(alpha_log + predicted_team_hfa) -
              exp(alpha_log),
            filtered_league_hfa = exp(alpha_log + filtered_league_hfa) -
              exp(alpha_log),
            predicted_league_hfa = exp(alpha_log + predicted_league_hfa) -
              exp(alpha_log)
          )
      }
      out
    }
  ) |>
  list_rbind(names_to = "model") |>
  mutate(
    team = teams[team],
    model = factor(model, levels = fit_names)
  ) |>
  select(
    model,
    team,
    everything()
    #-season
  ) |>
  arrange(team, model)
comp_fits |>
  glimpse()

check <- comp_fits |>
  filter(str_detect(as.character(model), "bivar_negbinom")) |>
  mutate(
    approx_strength_linear = exp(alpha_log) *
      (filtered_team_off_strength + filtered_team_def_strength),
    diff = filtered_team_strength - approx_strength_linear
  ) |>
  group_by(model, team) |>
  summarise(
    approx_mae = mean(abs(diff)),
    approx_mean = mean(diff)
  )

check |>
  arrange(desc(approx_mae)) |>
  head(10)


library(dplyr)
library(tidyr)
library(posterior)
library(ggplot2)

cmp <- comp_fits |>
  filter(model %in% c("univar_normal", "bivar_negbinom2")) |>
  transmute(
    model,
    team,
    filt_mean = mean(filtered_team_strength),
    filt_sd = sd(filtered_team_strength),
    pred_mean = mean(predicted_team_strength),
    pred_sd = sd(predicted_team_strength)
  ) |>
  pivot_wider(
    names_from = model,
    values_from = c(filt_mean, filt_sd, pred_mean, pred_sd)
  )
cmp

# ---- Shrinkage diagnostics ----
fit_filt <- lm(filt_mean_bivar_negbinom2 ~ filt_mean_univar_normal, data = cmp)
fit_pred <- lm(pred_mean_bivar_negbinom2 ~ pred_mean_univar_normal, data = cmp)

cat(
  "\nFiltered: cor =",
  cor(cmp$filt_mean_univar_normal, cmp$filt_mean_bivar_negbinom2),
  "\n"
)
print(summary(fit_filt)$coef)

cat(
  "\nPredicted: cor =",
  cor(cmp$pred_mean_univar_normal, cmp$pred_mean_bivar_negbinom2),
  "\n"
)
print(summary(fit_pred)$coef)

# biggest disagreements
cmp |>
  mutate(
    filt_abs_diff = abs(filt_mean_bivar_negbinom2 - filt_mean_univar_normal)
  ) |>
  arrange(desc(filt_abs_diff)) |>
  select(
    team,
    filt_mean_univar_normal,
    filt_mean_bivar_negbinom2,
    filt_abs_diff
  ) |>
  head(10) |>
  print(n = 10)

dir.create("artifacts/compare", recursive = TRUE, showWarnings = FALSE)

# ---- Plot 1: filtered means scatter (univar vs negbinom2) ----
p1 <- ggplot(cmp, aes(filt_mean_univar_normal, filt_mean_bivar_negbinom2)) +
  geom_hline(yintercept = 0, color = "grey85") +
  geom_vline(xintercept = 0, color = "grey85") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "red") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_text(aes(label = team), check_overlap = TRUE, nudge_y = 0.1, size = 3) +
  labs(
    title = "Filtered team strength: univar vs negbinom2",
    x = "Univar filtered strength (posterior mean)",
    y = "Negbinom2 filtered strength (posterior mean)"
  ) +
  theme_minimal()
p1
ggsave(
  "artifacts/compare/strength_filtered_scatter.png",
  p1,
  width = 9,
  height = 7,
  dpi = 150
)

# ---- Plot 2: predicted means scatter ----
p2 <- ggplot(cmp, aes(pred_mean_univar_normal, pred_mean_bivar_negbinom2)) +
  geom_hline(yintercept = 0, color = "grey85") +
  geom_vline(xintercept = 0, color = "grey85") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "red") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_text(aes(label = team), check_overlap = TRUE, nudge_y = 0.1, size = 3) +
  labs(
    title = "Predicted team strength: univar vs negbinom2",
    x = "Univar predicted strength (posterior mean)",
    y = "Negbinom2 predicted strength (posterior mean)"
  ) +
  theme_minimal()
p2
ggsave(
  "artifacts/compare/strength_predicted_scatter.png",
  p2,
  width = 9,
  height = 7,
  dpi = 150
)

# ---- Plot 3: “shrinkage” (difference vs univar) ----
p3 <- ggplot(
  cmp,
  aes(
    filt_mean_univar_normal,
    filt_mean_bivar_negbinom2 - filt_mean_univar_normal
  )
) +
  geom_hline(yintercept = 0, linetype = 2, color = "red") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_text(aes(label = team), check_overlap = TRUE, nudge_y = 0.1, size = 3) +
  labs(
    title = "Filtered shrinkage: (negbinom2 - univar) vs univar",
    x = "Univar filtered strength (posterior mean)",
    y = "Difference in means"
  ) +
  theme_minimal()
p3
ggsave(
  "artifacts/compare/strength_filtered_shrinkage.png",
  p3,
  width = 9,
  height = 7,
  dpi = 150
)

# 7. Prediction Evaluation ----
filtered_results_nb2 <- fit_rvars_list |>
  pluck("bivar_negbinom2") |>
  keep_at(\(x) stringr::str_subset(x, "sim")) |>
  spread_rvars(
    c(sim_home_score, sim_away_score, sim_result, sim_total, sim_home_win)[
      game_id
    ]
  )

# Extract OOS predictions manually for bivar_negbinom2
pred_cols <- c(
  "alpha_log",
  "log_phi_home",
  "log_phi_away",
  "phi_home",
  "phi_away",
  "predicted_team_off_strength",
  "predicted_team_def_strength",
  "predicted_team_strength",
  "predicted_team_hfa",
  "predicted_league_hfa"
)

pred_exprs <- c(
  "alpha_log[season]",
  "log_phi_home",
  "log_phi_away",
  "phi_home",
  "phi_away",
  "predicted_team_off_strength[team]",
  "predicted_team_def_strength[team]",
  "predicted_team_strength[team]",
  "predicted_team_hfa[team]",
  "predicted_league_hfa"
)
# extract everything before first[
str_extract(pred_exprs, "^\\w+")

predicted_results_nb2 <- fit_rvars_list |>
  pluck("bivar_negbinom2") |>
  keep_at(str_extract(pred_exprs, "^\\w+")) |>
  as_draws_df() |>
  spread_rvars(!!!rlang::parse_exprs(pred_exprs))
#ungroup() |>
filter(season == max(season)) |>
  select(-season)

oos_nb2 <- oos_games |>
  left_join(
    predicted_results_nb2 |>
      filter(season == max(season)) |>
      select(
        team,
        alpha_log,
        log_phi_home,
        log_phi_away,
        phi_home,
        phi_away,
        home_predicted_team_off_strength = predicted_team_off_strength,
        home_predicted_team_def_strength = predicted_team_def_strength,
        home_predicted_team_strength = predicted_team_strength,
        home_predicted_team_hfa = predicted_team_hfa
      ),
    by = c("home_idx" = "team")
  ) |>
  left_join(
    predicted_results_nb2 |>

      filter(season == max(season)) |>
      select(
        team,
        away_predicted_team_off_strength = predicted_team_off_strength,
        away_predicted_team_def_strength = predicted_team_def_strength,
        away_predicted_team_strength = predicted_team_strength
      ),
    by = c("away_idx" = "team")
  ) |>
  mutate(
    eta_home = alpha_log +
      home_predicted_team_off_strength -
      away_predicted_team_def_strength +
      (hfa * home_predicted_team_hfa),
    eta_away = alpha_log +
      away_predicted_team_off_strength -
      home_predicted_team_def_strength,
    mu_home = exp(eta_home),
    mu_away = exp(eta_away),
    mu_result = mu_home - mu_away,
    mu_total = mu_home + mu_away
  )

pred_dat <- oos_nb2 |>
  as_draws_df() |>
  gather_draws(
    "mu_home[..]"
  )

oos_mu_draws <- draws_rvars(
  mu_home = oos_nb2$mu_home,
  mu_away = oos_nb2$mu_away,
  mu_result = oos_nb2$mu_result,
  mu_total = oos_nb2$mu_total
) |>
  spread_draws(
    mu_home[game_idx],
    mu_away[game_idx],
    mu_result[game_idx],
    mu_total[game_idx]
  ) |>
  mutate(
    game_id = oos_games$game_id[game_idx],
    .after = game_idx
  )

p <- oos_mu_draws |>
  ungroup() |>
  dplyr::filter(game_idx == 1)
mcmc_hex(
  p,
  pars = c("mu_away", "mu_home"),
  bins = 10
)

# or, override the fill scale
oos_mu_draws |>
  dplyr::ungroup() |>
  dplyr::filter(game_idx == 1) |>
  bayesplot::mcmc_hex(pars = c("mu_away", "mu_home")) +
  ggplot2::scale_fill_gradientn(
    breaks = scales::breaks_pretty(n = 6)(c(0, NA)), # supply your range here
    labels = scales::label_number()
  )

# simplest: avoid mcmc_hex; use ggplot2 hexbin directly
oos_mu_draws |>
  #dplyr::ungroup() |>
  dplyr::filter(game_idx == 1) |>
  ggplot2::ggplot(ggplot2::aes(mu_away, mu_home)) +
  ggplot2::stat_bin_hex(bins = 30) +
  ggplot2::scale_fill_viridis_c(
    breaks = scales::breaks_pretty(6),
    labels = scales::label_number()
  ) +
  theme_minimal()

oos_mu_draws |>
  #dplyr::ungroup() |>
  #dplyr::filter(game_idx == 1) |>
  ggplot2::ggplot(ggplot2::aes(mu_result, mu_total)) +
  ggplot2::stat_bin_hex(bins = 20) +
  facet_wrap(~game_id) +
  ggplot2::scale_fill_viridis_c(
    breaks = scales::breaks_pretty(6),
    labels = scales::label_number()
  ) +
  theme_minimal()

library(dplyr)
library(tidybayes)
library(posterior)

rv <- fit_rvars_list$bivar_negbinom2

# keep your current "last alpha_log" approach
alpha_last <- rv |>
  spread_rvars(alpha_log[season]) |>
  arrange(season) |>
  slice_tail(n = 1) |>
  pull(alpha_log)

phi_home <- exp(rv$log_phi_home)
phi_away <- exp(rv$log_phi_away)

# build etas (HOME-ONLY HFA, matching your current Stan model)
eta_home <- alpha_last +
  rv$predicted_team_off_strength[oos_games$home_idx] -
  rv$predicted_team_def_strength[oos_games$away_idx] +
  (oos_games$hfa * rv$predicted_team_hfa[oos_games$home_idx])

eta_away <- alpha_last +
  rv$predicted_team_off_strength[oos_games$away_idx] -
  rv$predicted_team_def_strength[oos_games$home_idx]

mu_home <- exp(eta_home)
mu_away <- exp(eta_away)

# expected scores (posterior means)
oos_exp <- oos_games |>
  mutate(
    exp_home = mu_home,
    exp_away = mu_away,
    exp_total = exp_home + exp_away,
    exp_result = exp_home - exp_away
  )

# simulated scores: convert rvars -> draws matrix, then rnbinom, then back to rvars
eta_home_mat <- as.matrix(as_draws_matrix(eta_home))
eta_away_mat <- as.matrix(as_draws_matrix(eta_away))

phi_home_vec <- as.numeric(as.matrix(as_draws_matrix(phi_home)))
phi_away_vec <- as.numeric(as.matrix(as_draws_matrix(phi_away)))

n_draws <- nrow(eta_home_mat)
n_games <- ncol(eta_home_mat)

phi_home_mat <- matrix(phi_home_vec, nrow = n_draws, ncol = n_games)
phi_away_mat <- matrix(phi_away_vec, nrow = n_draws, ncol = n_games)

set.seed(52)
sim_home_mat <- matrix(
  rnbinom(
    n_draws * n_games,
    size = as.vector(phi_home_mat),
    mu = exp(as.vector(eta_home_mat))
  ),
  nrow = n_draws,
  ncol = n_games
)
sim_away_mat <- matrix(
  rnbinom(
    n_draws * n_games,
    size = as.vector(phi_away_mat),
    mu = exp(as.vector(eta_away_mat))
  ),
  nrow = n_draws,
  ncol = n_games
)

oos_sim <- oos_exp |>
  mutate(
    sim_home_score = rvar(sim_home_mat),
    sim_away_score = rvar(sim_away_mat),
    sim_total = sim_home_score + sim_away_score,
    sim_result = sim_home_score - sim_away_score
  )

library(hexbin)

m <- as.matrix(draws_xy[, c("sim_away_score", "sim_home_score")])


nb_rvars <- oos_sim |>
  as_draws()
