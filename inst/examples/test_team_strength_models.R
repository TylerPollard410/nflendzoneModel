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
#game_data_full <- nflendzone::load_game_data(seasons = all_seasons)
game_data_full <- nflendzone::load_game_data(
  seasons = (current_season - 3):current_season
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

fit_new_team_strength_model <- function(stan_data, model_name, ...) {
  stopifnot(is.list(stan_data))
  model <- instantiate::stan_package_model(
    name = model_name,
    package = "nflendzoneModel"
  )
  fit <- model$sample(data = stan_data, ...)
  fit
}

# Globals
fit_seed = 52
fit_init = 0
fit_sig_figs = 10
fit_chains = 4
fit_parallel = parallel::detectCores()
fit_warm = 1000
fit_samps = 1000
fit_thin = 1
fit_adapt_delta = 0.95
fit_max_treedepth = 10

cat("\n=== Fitting Model ===\n")

## 3.1 Fit Model ----
fit_univar_normal <- fit_team_strength_model(
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

fit_univar_student <- fit_new_team_strength_model(
  stan_data = fit_stan_data,
  model_name = "team_strength_univar_student",
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

fit_bivar_normal <- fit_new_team_strength_model(
  stan_data = fit_stan_data,
  model_name = "team_strength_bivar_normal",
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

fit_bivar_poisson <- fit_new_team_strength_model(
  stan_data = fit_stan_data,
  model_name = "team_strength_bivar_poisson",
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

fit_bivar_negbinom <- fit_new_team_strength_model(
  stan_data = fit_stan_data,
  model_name = "team_strength_bivar_negbinom",
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

fit_names <- c(
  "univar_normal",
  "univar_student",
  "bivar_normal",
  "bivar_poisson",
  "bivar_negbinom"
)

fit_list <- list(
  fit_univar_normal,
  fit_univar_student,
  fit_bivar_normal,
  fit_bivar_poisson,
  fit_bivar_negbinom
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
# fit_bivar_negbinom$save_object(
#   file = paste0(
#     "artifacts/model-archive/team_strength/",
#     "bivar_negbinom",
#     ".rds"
#   )
# )

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

fit_draws_list <- fit_list |>
  map(
    \(.fit) {
      .fit$draws() |>
        as_draws_rvars()
    }
  )

# gq_draws <- gq$draws()
# gq_rvars <- as_draws_rvars(gq_draws)

## Hypeerparameters ----
fit_hyperparams <- fit_list |>
  imap(
    \(.fit, .name) {
      .fit$summary(
        variables = str_subset(.fit$metadata()$stan_variables, "phi|sigma")
      ) |>
        mutate(model = .name, .before = 1)
    }
  ) |>
  bind_rows() |>
  mutate(
    model = factor(
      model,
      levels = fit_names
    )
  ) |>
  arrange(variable, model)

fit_strengths_list <- fit_draws_list |>
  map(\(x) keep_at(x, str_subset(names(x), "filtered|predicted")))

possible_exprs <- c(
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

# fit_strengths_list |>
#   imap(
#     \(.rvars, .name) {
#       rvars_expr <- str_subset(
#         possible_exprs,
#         str_c(names(.rvars), collapse = "|")
#       )
#       .rvars |>
#         spread_rvars(
#           enquote(rvars_expr)
#         ) |>
# mutate(
#   model = .name,
#   season = current_season,
#   team = teams[team],
#   .before = 1
# )
#   }
# )

fit_strengths <- fit_strengths_list |>
  imap(
    \(.rvars, .name) {
      #rvars_expr <- str_subset(possible_exprs, names(.rvars))
      if (str_detect(.name, "univar")) {
        .rvars |>
          spread_rvars(
            filtered_team_strength[team],
            # filter_team_off_strength[team],
            # filter_team_def_strength[team],
            filtered_team_hfa[team],
            filtered_league_hfa[season],
            predicted_team_strength[team],
            # filter_team_off_strength[team],
            # filter_team_def_strength[team],
            predicted_team_hfa[team],
            predicted_league_hfa[season]
          ) |>
          mutate(
            model = .name,
            season = current_season,
            team = teams[team],
            .before = 1
          )
      } else if (str_detect(.name, "bivar_normal")) {
        .rvars |>
          spread_rvars(
            # filtered_team_strength[team],
            filtered_team_off_strength[team],
            filtered_team_def_strength[team],
            filtered_team_hfa[team],
            filtered_league_hfa[season],
            # predicted_team_strength[team],
            predicted_team_off_strength[team],
            predicted_team_def_strength[team],
            predicted_team_hfa[team],
            predicted_league_hfa[season]
          ) |>
          mutate(
            model = .name,
            season = current_season,
            team = teams[team],
            .before = 1
          )
      } else {
        .rvars |>
          spread_rvars(
            # filtered_team_strength[team],
            filtered_team_off_strength[team],
            filtered_team_def_strength[team],
            filtered_team_hfa[team],
            filtered_league_hfa[season]
            # predicted_team_strength[team],
            # predicted_team_off_strength[team],
            # predicted_team_def_strength[team],
            # predicted_team_hfa[team],
            # predicted_league_hfa[season]
          ) |>
          mutate(
            model = .name,
            season = current_season,
            team = teams[team],
            .before = 1
          )
      }
    }
  ) |>
  list_rbind() |>
  mutate(
    model = factor(
      model,
      levels = fit_names
    )
  ) |>
  select(
    model,
    season,
    team,
    contains("filtered"),
    contains("predicted")
  ) |>
  relocate(
    "filtered_team_off_strength",
    "filtered_team_def_strength",
    .before = filtered_team_strength
  ) |>
  relocate(
    "predicted_team_off_strength",
    "predicted_team_def_strength",
    .before = predicted_team_strength
  ) |>
  arrange(team, model)

fit_strengths_list2 <- fit_strengths_list |> #compact()
  reduce(union_all)

## Extract posteriors by model ----
post_strengths_univar_normal <- fit_draws_list |>
  pluck("univar_normal") |>
  spread_rvars(
    filtered_team_strength[team],
    filtered_team_hfa[team],
    filtered_league_hfa,
    predicted_team_strength[team],
    predicted_team_hfa[team],
    predicted_league_hfa
  ) |>
  mutate(
    model = "univar_normal",
    season = current_season,
    team = teams[team],
    .before = 1
  )

post_strengths_univar_student <- fit_draws_list |>
  pluck("univar_student") |>
  spread_rvars(
    filtered_team_strength[team],
    filtered_team_hfa[team],
    filtered_league_hfa,
    predicted_team_strength[team],
    predicted_team_hfa[team],
    predicted_league_hfa
  ) |>
  mutate(
    model = "univar_student",
    season = current_season,
    team = teams[team],
    .before = 1
  )

post_strengths_bivar_normal <- fit_draws_list |>
  pluck("bivar_normal") |>
  spread_rvars(
    # filtered_team_strength[team],
    filtered_team_off_strength[team],
    filtered_team_def_strength[team],
    filtered_team_hfa[team],
    filtered_league_hfa[season],
    # predicted_team_strength[team],
    predicted_team_off_strength[team],
    predicted_team_def_strength[team],
    predicted_team_hfa[team],
    predicted_league_hfa[season]
  ) |>
  mutate(
    model = "bivar_normal",
    season = current_season,
    team = teams[team],
    .before = 1
  )

post_strengths_bivar_poisson <- fit_draws_list |>
  pluck("bivar_poisson") |>
  spread_rvars(
    # filtered_team_strength[team],
    filtered_team_off_strength[team],
    filtered_team_def_strength[team],
    filtered_team_hfa[team],
    filtered_league_hfa[season],
    # predicted_team_strength[team],
    predicted_team_off_strength[team],
    predicted_team_def_strength[team],
    predicted_team_hfa[team],
    predicted_league_hfa[season]
  ) |>
  mutate(
    model = "bivar_poisson",
    season = current_season,
    team = teams[team],
    .before = 1
  )

post_strengths_bivar_negbinom <- fit_draws_list |>
  pluck("bivar_negbinom") |>
  spread_rvars(
    # filtered_team_strength[team],
    filtered_team_off_strength[team],
    filtered_team_def_strength[team],
    filtered_team_hfa[team],
    filtered_league_hfa[season],
    # predicted_team_strength[team],
    predicted_team_off_strength[team],
    predicted_team_def_strength[team],
    predicted_team_hfa[team],
    predicted_league_hfa[season]
  ) |>
  mutate(
    model = "bivar_negbinom",
    season = current_season,
    team = teams[team],
    .before = 1
  )

# ## Filtered ----
# ### Strength ----
# filtered_strengths <- gq_rvars |>
#   spread_rvars(
#     filtered_team_strength[team],
#     filtered_team_hfa[team]
#   ) |>
#   mutate(
#     week_idx = filter_week_idx,
#     team = teams[team]
#   ) |>
#   left_join(
#     schedule_idx |>
#       select(season_idx, week_idx, season, week) |>
#       distinct(),
#     by = "week_idx"
#   ) |>
#   relocate(season_idx, week_idx, season, week, .before = 1)

# ### League HFA ----
# filtered_league_hfa <- gq_rvars |>
#   spread_rvars(
#     filtered_league_hfa
#   ) |>
#   mutate(
#     week_idx = filter_week_idx
#   ) |>
#   left_join(
#     schedule_idx |>
#       select(season_idx, week_idx, season, week) |>
#       distinct(),
#     by = "week_idx"
#   ) |>
#   relocate(season_idx, week_idx, season, week, .before = 1)

# ### Results ----
# # filtered_result <- schedule_idx |>
# #   filter(week_idx == fit_stan_data$N_weeks) |>
# #   select(
# #     game_idx,
# #     game_id,
# #     season_idx,
# #     week_idx,
# #     season,
# #     week,
# #     hfa,
# #     home_team,
# #     away_team
# #   ) |>
# #   left_join(
# #     fit_rvars |>
# #       spread_rvars(
# #         mu[game_idx],
# #         sigma_obs
# #       ),
# #     by = "game_idx"
# #   ) |>
# #   mutate(
# #     y_obs = rvar_rng(rnorm, 1, mu, sigma_obs),
# #     .by = game_id
# #   ) |>
# #   as_tibble()

# filtered_result <- schedule_idx |>
#   filter(week_idx == filter_week_idx) |>
#   select(
#     game_idx,
#     game_id,
#     season_idx,
#     week_idx,
#     season,
#     week,
#     hfa,
#     home_team,
#     away_team
#   ) |>
#   left_join(
#     filtered_strengths |>
#       rename(
#         home_strength = filtered_team_strength,
#         home_hfa = filtered_team_hfa
#       ),
#     by = join_by(season_idx, week_idx, season, week, home_team == team)
#   ) |>
#   left_join(
#     filtered_strengths |>
#       select(-filtered_team_hfa) |>
#       rename(
#         away_strength = filtered_team_strength
#       ),
#     by = join_by(season_idx, week_idx, season, week, away_team == team)
#   ) |>
#   mutate(
#     sigma = gq_rvars$sigma_pred,
#     mu = home_strength - away_strength + home_hfa * hfa,
#     y = rvar_rng(rnorm, 1, mu, sigma),
#     .by = game_id
#   )

# ## Predicted ----
# ### Strength ----
# predicted_strengths <- gq_rvars |>
#   spread_rvars(
#     predicted_team_strength[week_idx, team],
#     predicted_team_hfa[week_idx, team]
#   ) |>
#   mutate(
#     week_idx = gq_targets[week_idx],
#     team = teams[team]
#   )

# ### League HFA ----
# predicted_league_hfa <- gq_rvars |>
#   spread_rvars(
#     predicted_league_hfa
#   ) |>
#   mutate(
#     season_idx = gq_stan_data$future_week_to_season[1],
#     week_idx = predict_week_idx
#   ) |>
#   left_join(
#     schedule_idx |>
#       select(season_idx, week_idx, season, week) |>
#       distinct(),
#     by = c("season_idx", "week_idx")
#   ) |>
#   relocate(season_idx, week_idx, season, week, .before = 1)

# ### Results ----
# if (nrow(oos_games) == 0) {
#   stop("No out-of-sample games to predict.")
# } else {
#   predicted_result <- schedule_idx |>
#     filter(week_idx == predict_week_idx) |>
#     select(
#       game_idx,
#       game_id,
#       season_idx,
#       week_idx,
#       season,
#       week,
#       hfa,
#       home_team,
#       away_team
#     ) |>
#     left_join(
#       predicted_strengths |>
#         rename(
#           home_strength = predicted_team_strength,
#           home_hfa = predicted_team_hfa
#         ),
#       by = join_by(week_idx, home_team == team)
#     ) |>
#     left_join(
#       predicted_strengths |>
#         select(-predicted_team_hfa) |>
#         rename(
#           away_strength = predicted_team_strength
#         ),
#       by = join_by(week_idx, away_team == team)
#     ) |>
#     mutate(
#       sigma = gq_rvars$sigma_pred,
#       mu = home_strength - away_strength + home_hfa * hfa,
#       y = rvar_rng(rnorm, 1, mu, sigma),
#       .by = game_id
#     )
# }

# # ============================================================================ #
# # 7. Save Output for Fit ----
# # ============================================================================ #

# fit_list <- list(
#   fit_rvars,
#   fit_hyperparams,
#   filtered_strengths,
#   filtered_league_hfa,
#   filtered_result,
#   predicted_strengths,
#   predicted_league_hfa,
#   predicted_result
# )

# ============================================================================ #
# 8. Save Output to Release ----
# ============================================================================ #

cat("\n=== Save Release ===\n")

# Create release tags
model_tags <- c(
  "team_strength_filter",
  "league_hfa_filter",
  "result_filter",
  "team_strength_predict",
  "league_hfa_predict",
  "result_predict",
  "team_strength_fit",
  "team_strength_gq"
)

# Create new release for each tag (if not exists)
suppressWarnings({
  purrr::walk(
    model_tags,
    ~ piggyback::pb_new_release(repo = github_data_repo, tag = .x)
  )
})

# ---------------------------------------------------------------------------- #
## Upload Filtered Estimates ----

upload_model_output(
  data = filtered_strengths,
  tag = "team_strength_filter",
  season = filter_season,
  week = filter_week,
  week_idx = filter_week_idx,
  repo = github_data_repo
)

upload_model_output(
  data = filtered_league_hfa,
  tag = "league_hfa_filter",
  season = filter_season,
  week = filter_week,
  week_idx = filter_week_idx,
  repo = github_data_repo
)

upload_model_output(
  data = filtered_result,
  tag = "result_filter",
  season = filter_season,
  week = filter_week,
  week_idx = filter_week_idx,
  repo = github_data_repo
)

# ---------------------------------------------------------------------------- #
## Upload Predicted Estimates ----

upload_model_output(
  data = predicted_strengths,
  tag = "team_strength_predict",
  season = predict_season,
  week = predict_week,
  week_idx = predict_week_idx,
  repo = github_data_repo
)

upload_model_output(
  data = predicted_league_hfa,
  tag = "league_hfa_predict",
  season = predict_season,
  week = predict_week,
  week_idx = predict_week_idx,
  repo = github_data_repo
)

upload_model_output(
  data = predicted_result,
  tag = "result_predict",
  season = predict_season,
  week = predict_week,
  week_idx = predict_week_idx,
  repo = github_data_repo
)

# ---------------------------------------------------------------------------- #
## Upload Model Objects ----

cat("\n=== Upload Model Objects ===\n")

### Team Strength Fit ----
fit_tag <- "team_strength_fit"
fit_timestamp <- c(
  season = filter_season,
  week = filter_week,
  week_idx = filter_week_idx
)

## Upload
upload_cmdstan_outputs(
  files = fit_files,
  tag = fit_tag,
  repo = github_data_repo,
  timestamp = fit_timestamp
)

### Team Strength GQ ----
gq_tag <- "team_strength_gq"
predict_timestamp <- c(
  season = predict_season,
  week = predict_week,
  week_idx = predict_week_idx
)
predict_metadata <- NULL
if (!any(is.na(predict_timestamp))) {
  predict_metadata <- list(
    predict_timestamp = as.list(predict_timestamp)
  )
}

## Upload
upload_cmdstan_outputs(
  files = gq_files,
  tag = gq_tag,
  repo = github_data_repo,
  timestamp = fit_timestamp,
  metadata = predict_metadata
)

cat("\n=== Complete ===\n")


# Pathfinder ----
library(cmdstanr)
library(stringr)

mod <- cmdstan_model(
  "src/stan/team_strength_fit.stan",
  compile = TRUE
)

opt <- mod$optimize(
  data = fit_stan_data,
  seed = fit_seed,
  #init = fit_init,
  sig_figs = fit_sig_figs,
  iter = 50000,
  jacobian = TRUE
)

mod_path2 <- mod$pathfinder(
  data = fit_stan_data,
  seed = fit_seed,
  init = fit_init,
  sig_figs = fit_sig_figs,
  num_paths = 10,
  max_lbfgs_iters = 100,
  single_path_draws = 200,
  draws = 200,
  #num_elbo_draws = 50,
  #psis_resample = FALSE,
  #calculate_lp = FALSE,
  history_size = 50
)


fit_stan_meta <- fit$metadata()

fit$print(variables = str_subset(fit_stan_meta$stan_variables, "phi|sigma"))
opt$print(variables = str_subset(fit_stan_meta$stan_variables, "phi|sigma"))
# mod_path$print(
#   variables = str_subset(fit_stan_meta$stan_variables, "phi|sigma")
# )
mod_path2$print(
  variables = str_subset(fit_stan_meta$stan_variables, "phi|sigma")
)
