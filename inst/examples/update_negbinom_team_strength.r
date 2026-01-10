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

library(ggplot2)
library(scales)
library(hexbin)
library(ggside)
library(ggnewscale)
library(patchwork)
library(bayesplot)

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
teams_data <- nflreadr::load_teams(current = TRUE)
teams <- teams_data$team_abbr
all_seasons <- 2002:nflreadr::get_current_season()
current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

#current_season <- 2006
#current_week <- 8

# Load game data
# game_data_full <- nflendzone::load_game_data(seasons = all_seasons)
game_data_full <- nflendzone::load_game_data(
  seasons = (current_season - 4):current_season
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
gc()

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
fit_adapt_delta = 0.95
fit_max_treedepth = 10

cat("\n=== Fitting Model ===\n")

fit_bivar_negbinom <- fit_team_strength_model(
  stan_data = fit_stan_data,
  model = "team_strength_bivar_negbinom",
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
fit_bivar_negbinom$diagnostic_summary()
fit_bivar_negbinom$cmdstan_diagnose()

fit_bivar_negbinom$save_object(
  file = paste0(
    "artifacts/model-archive/team_strength/",
    "bivar_negbinom",
    ".rds"
  )
)

## 3.3 Loo ----
fit_loo <- fit_bivar_negbinom$loo()
fit_loo |> print()
fit_loo |> loo::loo_compare()
fit_loo |> loo::loo_model_weights()

# ============================================================================ #
# 4. Generate Quantities ----
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
# 6. Posteriors ----
# ============================================================================ #

## 6.1 Draws & rvars ----
# Extract team strengths with rvars

### 6.1.1 Negative Binomial Draws ----
nb_draws <- fit_bivar_negbinom$draws()
nb_rvars <- as_draws_rvars(nb_draws)

week_vars <- c(
  "phi_home",
  "phi_away",
  "filtered_alpha_log",
  "filtered_team_off_strength[team]",
  "filtered_team_def_strength[team]",
  "filtered_team_hfa[team]",
  "filtered_league_hfa",
  "predicted_alpha_log",
  "predicted_team_off_strength[team]",
  "predicted_team_def_strength[team]",
  "predicted_team_hfa[team]",
  "predicted_league_hfa"
)

nb_sum <- fit_bivar_negbinom$summary(
  variables = str_extract(week_vars, "^\\w+")
) |>
  mutate(team = str_extract(variable, "[:digit:]+"), .before = 1) |>
  mutate(
    team = teams[as.numeric(team)],
    filtered_season = filter_season,
    filtered_week = filter_week,
    predicted_season = predict_season,
    predicted_week = predict_week,
    .before = 1
  )

# piggyback::pb_release_create(
#   repo = github_data_repo,
#   tag = "team_strength_negbinom_summary",
#   name = "team_strength_negbinom_summary",
#   body = paste("Data release for", "team_strength_negbinom_summary")
# )
# arrow::write_feather(
#   nb_sum,
#   paste0(
#     "artifacts/model-reports/team_strength/nb_sum_",
#     filter_season,
#     ".arrow"
#   )
# )
pb_write(
  x = nb_sum,
  file = paste0(
    "team_strength_negbinom_summary",
    "_",
    filter_season,
    ".arrow" # ".rds", ".parquet", ".arrow"
  ),
  write_function = arrow::write_feather,
  repo = github_data_repo,
  tag = "team_strength_negbinom_summary"
)
gc()


nb_rvars <- nb_rvars |>
  spread_rvars(
    phi_home,
    phi_away,
    filtered_alpha_log,
    filtered_team_off_strength[team],
    filtered_team_def_strength[team],
    filtered_team_hfa[team],
    filtered_league_hfa,
    predicted_alpha_log,
    predicted_team_off_strength[team],
    predicted_team_def_strength[team],
    predicted_team_hfa[team],
    predicted_league_hfa
  ) |>
  mutate(
    team = teams[team],
    filtered_season = filter_season,
    filtered_week = filter_week,
    predicted_season = predict_season,
    predicted_week = predict_week,
    .before = 1
  ) |>
  relocate(team, .after = predicted_season)

dir.create(
  "artifacts/model-reports/team_strength",
  recursive = TRUE,
  showWarnings = FALSE
)
saveRDS(nb_rvars, "artifacts/model-reports/team_strength/nb_rvars.rds")

## 6.2 Hyperparameters ----
nb2_hyperparams_list <- fit_bivar_negbinom$summary(
  variables = str_subset(
    fit_bivar_negbinom$metadata()$stan_variables,
    "phi|sigma|alpha"
  )
)

### Plot hyperparameter summaries ----
nb2_hyperparams_plots <- nb2_hyperparams_list |>
  names() |>
  set_names() |>
  imap(\(.rvar, .names) {
    var_len <- nb2_hyperparams_list |> pluck(.rvar) |> length()
    if (var_len > 1) {
      nvar <- paste0(.rvar, "[", 1:var_len, "]")
      mcmc_combo(
        nb2_hyperparams_list,
        pars = nvar,
        combo = c("hist", "trace")
      )
    } else {
      mcmc_combo(
        nb2_hyperparams_list,
        pars = .rvar,
        combo = c("hist", "trace")
      )
    }
  })
nb2_hyperparams_plots


## 6.3 Team Strengths ----

# 8. Probabilistic Predictions ----
# load nb_rvars
#nb_rvars <- readRDS("artifacts/model-reports/team_strength/nb_rvars.rds")
nb_rvars <- readRDS("nb_rvars.rds")
base_repo_url <- "https://github.com/TylerPollard410/nflendzoneData/releases/download/"
tag <- "team_strength_negbinom_summary"
nb_sums_github_url <- paste0(
  base_repo_url,
  tag,
  "/",
  tag,
  "_",
  filter_season,
  ".arrow"
)

nb_sums_github_url2 <- pb_download_url()

system.time(
  nb_sum_github <- arrow::read_feather(nb_sums_github_url)
)


nb_sum_github2 <- load_from_url(
  nb_sums_github_url
)

nb_rvars_github <- nb_sum_github |>
  #filter(is.na(team) | team == "BAL") |>
  ungroup() |>
  mutate(
    rvariable = rvar_rng(
      rnorm,
      n = n(),
      mean = mean,
      sd = sd,
      ndraws = 4000
    ),
    .after = variable
  ) |>
  # mutate(team = as.numeric(factor(team))) |>
  # mutate(
  #   variable = ifelse(!is.na(team), paste0(variable, "[", team, "]"), variable)
  # ) |>
  select(variable, rvariable) |>
  pivot_wider(
    names_from = variable,
    values_from = rvariable
    # id_cols = c(
    #   "filtered_season",
    #   "filtered_week",
    #   "predicted_season",
    #   "predicted_week"
    # )
  ) |>
  unnest_rvars() |>
  spread_rvars(!!!rlang::parse_exprs(week_vars)) |>
  relocate(team, .before = 1) |>
  mutate(
    team = teams[team],
    filtered_season = filter_season,
    filtered_week = filter_week,
    predicted_season = predict_season,
    predicted_week = predict_week,
    .before = 1
  )

nb_rvars_plot <- nb_rvars
nb_rvars_plot <- nb_rvars_github

# nfl data metadata
teams_data <- nflreadr::load_teams(current = TRUE)
teams <- teams_data$team_abbr
filter_season <- unique(nb_rvars_plot$filtered_season)
filter_week <- unique(nb_rvars_plot$filtered_week)
predict_season <- unique(nb_rvars_plot$predicted_season)
predict_week <- unique(nb_rvars_plot$predicted_week)

team_colors <- setNames(teams_data$team_color, teams_data$team_abbr)
team_colors_light <- colorspace::lighten(team_colors, amount = 0.25)

result_fill_values <- c(team_colors_light, Push = "grey70")
total_fill_values <- c(Under = "steelblue3", Over = "orange2", Push = "grey70")

# Load game data
game_data_full <- nflendzone::load_game_data(
  seasons = sort(unique(filter_season, predict_season))
)

# Prepare full schedule with indices (needed for GQ)
schedule_idx <- prepare_schedule_indices(
  game_data_full,
  teams
)

# Get OOS games
oos_games <- schedule_idx |>
  filter(season %in% predict_season, week %in% predict_week)

team_strengths <- nb_rvars_plot |>
  select(
    season = filtered_season,
    week = filtered_week,
    team,
    filtered_alpha_log,
    phi_home,
    phi_away,
    filtered_team_off_strength,
    filtered_team_def_strength,
    filtered_team_hfa,
    filtered_league_hfa
  ) |>
  mutate(
    team_off_strength = exp(filtered_alpha_log + filtered_team_off_strength) -
      exp(filtered_alpha_log),
    team_def_strength = exp(filtered_alpha_log + filtered_team_def_strength) -
      exp(filtered_alpha_log),
    team_strength = team_off_strength + team_def_strength,
    team_hfa = exp(filtered_alpha_log + filtered_team_hfa) -
      exp(filtered_alpha_log),
    league_hfa = exp(filtered_alpha_log + filtered_league_hfa) -
      exp(filtered_alpha_log)
  ) |>
  mutate(
    n = ndraws(filtered_team_off_strength)
  )

pred_rvars <- oos_games |>
  left_join(
    nb_rvars_plot |>
      # filter(season == max(season)) |>
      select(
        season = predicted_season,
        week = predicted_week,
        team,
        predicted_alpha_log,
        phi_home,
        phi_away,
        home_predicted_team_off_strength = predicted_team_off_strength,
        home_predicted_team_def_strength = predicted_team_def_strength,
        home_predicted_team_hfa = predicted_team_hfa
      ),
    by = c("season", "week", "home_team" = "team")
  ) |>
  left_join(
    nb_rvars_plot |>
      # filter(season == max(season)) |>
      select(
        season = predicted_season,
        week = predicted_week,
        team,
        away_predicted_team_off_strength = predicted_team_off_strength,
        away_predicted_team_def_strength = predicted_team_def_strength
      ),
    by = c("season", "week", "away_team" = "team")
  ) |>
  mutate(
    eta_home = predicted_alpha_log +
      home_predicted_team_off_strength -
      away_predicted_team_def_strength +
      (hfa * (home_predicted_team_hfa / 2)),
    eta_away = predicted_alpha_log +
      away_predicted_team_off_strength -
      home_predicted_team_def_strength -
      (hfa * (home_predicted_team_hfa / 2)),
    mu_home = exp(eta_home),
    mu_away = exp(eta_away),
    mu_result = mu_home - mu_away,
    mu_total = mu_home + mu_away
  ) |>
  mutate(
    y_home = rvar_rng(
      rnbinom,
      n = nrow(oos_games),
      mu = mu_home,
      size = phi_home
    ),
    y_away = rvar_rng(
      rnbinom,
      n = nrow(oos_games),
      mu = mu_away,
      size = phi_away
    ),
    y_result = y_home - y_away,
    y_total = y_home + y_away
  )

# # mu_result vs mu_total as proportion
# oos_pred_draws |>
#   ggplot(ggplot2::aes(mu_result, mu_total)) +
#   stat_bin_hex(
#     bins = 20,
#     aes(fill = after_stat(count / sum(count)))
#   ) +
#   facet_wrap(~game_id) +
#   scale_fill_viridis_c(
#     #limits = c(0, 1),
#     labels = scales::label_percent(accuracy = 1),
#     name = "Proportion"
#   ) +
#   theme_minimal()

# # y_result vs y_total as proportion
# oos_pred_draws |>
#   ggplot(ggplot2::aes(mu_result, mu_total)) +
#   stat_bin_hex(
#     bins = 20,
#     #aes(fill = after_stat(count / sum(count)))
#   ) +
#   facet_wrap(~game_id) +
#   ggplot2::scale_fill_viridis_c(
#     breaks = scales::breaks_pretty(6),
#     labels = scales::label_number()
#   ) +
#   theme_bw()

# # install.packages("ggside") # if needed

# # Hexbin of mu_result vs mu_total with side histograms (counts)
# oos_pred_draws |>
#   ggplot2::ggplot(ggplot2::aes(mu_result, mu_total)) +
#   ggplot2::stat_bin_hex(bins = 20) +
#   ggside::geom_xsidehistogram(bins = 30, fill = "grey45", alpha = 0.7) +
#   ggside::geom_ysidehistogram(bins = 30, fill = "grey45", alpha = 0.7) +
#   facet_wrap(~game_id) +
#   ggplot2::scale_fill_viridis_c(
#     breaks = scales::breaks_pretty(6),
#     labels = scales::label_number(),
#     name = "Count"
#   ) +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(ggside.panel.scale = 0.25)

## 8.1 One Game Test ----
game_id_sel <- pred_rvars |>
  filter(home_team == "BAL" | away_team == "BAL") |>
  pull(game_id)

# 1) Get rvar row for the game (to use Pr) and the per-draw data (to plot)
one_game_rvars <- pred_rvars |>
  dplyr::filter(game_id == game_id_sel)

one_game_draws <- one_game_rvars |>
  unnest_rvars()

# # 2) Quadrant + marginal probabilities using Pr() on rvars
# probs <- one_game_rvars |>
#   summarise(
#     p_home_over = Pr(mu_result > spread_line & mu_total > total_line),
#     p_home_under = Pr(mu_result > spread_line & mu_total < total_line),
#     p_away_over = Pr(mu_result < spread_line & mu_total > total_line),
#     p_away_under = Pr(mu_result < spread_line & mu_total < total_line),
#     p_home_cover = Pr(mu_result > spread_line),
#     p_away_cover = Pr(mu_result < spread_line),
#     p_over = Pr(mu_total > total_line),
#     p_under = Pr(mu_total < total_line),
#     spread_line = dplyr::first(spread_line),
#     total_line = dplyr::first(total_line),
#     home_team = dplyr::first(home_team),
#     away_team = dplyr::first(away_team)
#   )

# # 3) Label positions inside the main panel
# xr <- range(one_game_draws$mu_result, na.rm = TRUE)
# yr <- range(one_game_draws$mu_total, na.rm = TRUE)
# x_off <- diff(xr) * 0.25
# y_off <- diff(yr) * 0.25
# x_off_min <- quantile(one_game_draws$mu_result, probs = 0.005, na.rm = TRUE)
# x_off_max <- quantile(one_game_draws$mu_result, probs = 0.995, na.rm = TRUE)
# y_off_min <- quantile(one_game_draws$mu_total, probs = 0.005, na.rm = TRUE)
# y_off_max <- quantile(one_game_draws$mu_total, probs = 0.995, na.rm = TRUE)

# quad_labels <- tibble::tibble(
#   # x = probs$spread_line + c(x_off, x_off, -x_off, -x_off),
#   # y = probs$total_line + c(y_off, -y_off, y_off, -y_off),
#   x = c(x_off_max, x_off_max, x_off_min, x_off_min),
#   y = c(y_off_max, y_off_min, y_off_max, y_off_min),
#   label = c(
#     paste0(
#       probs$home_team,
#       " cover & Over:  ",
#       percent(probs$p_home_over, 0.1)
#     ),
#     paste0(
#       probs$home_team,
#       " cover & Under: ",
#       percent(probs$p_home_under, 0.1)
#     ),
#     paste0(
#       probs$away_team,
#       " cover & Over:  ",
#       percent(probs$p_away_over, 0.1)
#     ),
#     paste0(
#       probs$away_team,
#       " cover & Under: ",
#       percent(probs$p_away_under, 0.1)
#     )
#   )
# )

# # 4) (Optional) side-panel text labels; if your ggside lacks sidetext geoms, remove these two layers
# xside_labels <- tibble::tibble(
#   # x = probs$spread_line + c(-x_off, x_off),
#   # y = Inf,
#   x = c(x_off_min, x_off_max),
#   y = Inf,
#   label = c(
#     paste0(probs$away_team, " cover: ", percent(probs$p_away_cover, 0.1)),
#     paste0(probs$home_team, " cover: ", percent(probs$p_home_cover, 0.1))
#   )
# )
# yside_labels <- tibble::tibble(
#   # y = probs$total_line + c(-y_off, y_off),
#   # x = Inf,
#   y = c(y_off_min, y_off_max),
#   x = Inf,
#   label = c(
#     paste0("Under: ", percent(probs$p_under, 0.1)),
#     paste0("Over:  ", percent(probs$p_over, 0.1))
#   )
# )

# # 5) Plot: hexbin counts, decision lines, in-panel quadrant labels, side marginals
# one_game_draws |>
#   mutate(
#     home_team = probs$home_team,
#     away_team = probs$away_team,
#     spread_line = probs$spread_line,
#     total_line = probs$total_line,
#     .after = game_id
#   ) |>
#   mutate(
#     result_bin = case_when(
#       mu_result > spread_line ~ home_team,
#       mu_result < spread_line ~ away_team,
#       TRUE ~ "Push"
#     ),
#     total_bin = case_when(
#       mu_total > total_line ~ "Over",
#       mu_total < total_line ~ "Under",
#       TRUE ~ "Push"
#     )
#   ) |>
#   # mutate(
#   #   result_bin = factor(
#   #     result_bin,
#   #     levels = c(probs$away_team, "Push", probs$home_team)
#   #   ),
#   #   total_bin = factor(total_bin, levels = c("Under", "Push", "Over"))
#   # ) |>
#   ggplot(aes(mu_result, mu_total)) +
#   # main joint hex counts
#   stat_bin_hex(
#     #bins = 20,
#     binwidth = c(1, 1),
#   ) +
#   scale_fill_viridis_c(
#     breaks = breaks_pretty(6),
#     labels = label_number(),
#     name = "Count"
#   ) +
#   # decision lines
#   geom_vline(
#     data = probs,
#     aes(xintercept = spread_line),
#     linetype = 2,
#     color = "red"
#   ) +
#   geom_hline(
#     data = probs,
#     aes(yintercept = total_line),
#     linetype = 2,
#     color = "red"
#   ) +
#   # quadrant probabilities (in-panel)
#   geom_label(
#     data = quad_labels,
#     aes(x = x, y = y, label = label),
#     inherit.aes = FALSE,
#     size = 3
#   ) +
#   # side marginals (counts)
#   new_scale_fill() +
#   ggside::geom_xsidehistogram(
#     aes(y = after_stat(count), fill = result_bin),
#     #bins = 30,
#     binwidth = 1,
#     boundary = probs$spread_line,
#     alpha = 0.7
#   ) +
#   scale_fill_manual(
#     values = result_fill_values
#     #breaks = c(probs$away_team, "Push", probs$home_team),
#     #name = "Result bins"
#     #drop = FALSE
#   ) +
#   new_scale_fill() +
#   ggside::geom_ysidehistogram(
#     aes(x = after_stat(count), fill = total_bin),
#     #bins = 30,
#     binwidth = 1,
#     boundary = probs$total_line,
#     alpha = 0.7
#   ) +
#   # separate side-panel fill scales from main fill
#   scale_fill_manual(
#     values = total_fill_values
#     #breaks = c(probs$away_team, "Push", probs$home_team),
#     #name = "Result bins"
#     #drop = FALSE
#   ) +
#   # ggside::scale_yfill_manual(
#   #   values = total_fill_values,
#   #   breaks = c("Under", "Push", "Over"),
#   #   name = "Total bins",
#   #   drop = FALSE
#   # ) +
#   # side-panel probability labels (comment out if unavailable in your ggside)
#   ggside::geom_xsidetext(
#     data = xside_labels,
#     aes(x = x, y = Inf, label = label),
#     inherit.aes = FALSE,
#     vjust = 1.1,
#     size = 3
#   ) +
#   ggside::geom_ysidetext(
#     data = yside_labels,
#     aes(x = Inf, y = y, label = label),
#     inherit.aes = FALSE,
#     hjust = 1.1,
#     size = 3
#   ) +
#   facet_wrap(~game_id) +
#   labs(
#     x = "Result",
#     y = "Total"
#   ) +
#   theme_minimal() +
#   theme_ggside_void() +
#   theme(
#     ggside.panel.scale = 0.25,
#     ggside.axis.line = element_blank(),
#     ggside.axis.ticks = element_blank(),
#     ggside.axis.text = element_blank(),
#     legend.position = "none"
#   )

# one_game <- pred_rvars$game_id[3]
# one_game_rvars <- pred_rvars |>
#   dplyr::filter(game_id == one_game)
# one_game_draws <- one_game_rvars |>
#   tidybayes::unnest_rvars()

spread_line_use <- one_game_rvars$spread_line
total_line_use <- one_game_rvars$total_line

# joint_y_prob_plot <-
# shiny::req(input$team_game)
# shiny::req(spread_line_rv(), total_line_rv())

# game_id_sel <- input$team_game

# one_game_rvars <- pred_rvars |>
#   dplyr::filter(game_id == game_id_sel)

# one_game_draws <- one_game_rvars |>
#   tidybayes::unnest_rvars()

one_game_rvars <- one_game_rvars |>
  dplyr::mutate(
    p_home_over = posterior::Pr(
      y_result > spread_line_use & y_total > total_line_use
    ),
    p_home_under = posterior::Pr(
      y_result > spread_line_use & y_total < total_line_use
    ),
    p_away_over = posterior::Pr(
      y_result < spread_line_use & y_total > total_line_use
    ),
    p_away_under = posterior::Pr(
      y_result < spread_line_use & y_total < total_line_use
    ),
    p_home_cover = posterior::Pr(y_result > spread_line_use),
    p_away_cover = posterior::Pr(y_result < spread_line_use),
    p_over = posterior::Pr(y_total > total_line_use),
    p_under = posterior::Pr(y_total < total_line_use)
  )

x_off_min <- stats::quantile(
  one_game_rvars$y_result,
  probs = 0.005,
  na.rm = TRUE
)
x_off_max <- stats::quantile(
  one_game_rvars$y_result,
  probs = 0.995,
  na.rm = TRUE
)
y_off_min <- stats::quantile(
  one_game_rvars$y_total,
  probs = 0.005,
  na.rm = TRUE
)
y_off_max <- stats::quantile(
  one_game_rvars$y_total,
  probs = 0.995,
  na.rm = TRUE
)

quad_labels <- tibble::tibble(
  x = c(x_off_max, x_off_max, x_off_min, x_off_min),
  y = c(y_off_max, y_off_min, y_off_max, y_off_min),
  label = c(
    paste0(
      one_game_rvars$home_team,
      " cover & Over:  ",
      scales::percent(one_game_rvars$p_home_over, 0.1)
    ),
    paste0(
      one_game_rvars$home_team,
      " cover & Under: ",
      scales::percent(one_game_rvars$p_home_under, 0.1)
    ),
    paste0(
      one_game_rvars$away_team,
      " cover & Over:  ",
      scales::percent(one_game_rvars$p_away_over, 0.1)
    ),
    paste0(
      one_game_rvars$away_team,
      " cover & Under: ",
      scales::percent(one_game_rvars$p_away_under, 0.1)
    )
  )
)

xside_labels <- tibble::tibble(
  x = c(x_off_min, x_off_max),
  y = Inf,
  label = c(
    paste0(
      one_game_rvars$away_team,
      " cover: ",
      scales::percent(one_game_rvars$p_away_cover, 0.1)
    ),
    paste0(
      one_game_rvars$home_team,
      " cover: ",
      scales::percent(one_game_rvars$p_home_cover, 0.1)
    )
  )
)

yside_labels <- tibble::tibble(
  y = c(y_off_min, y_off_max),
  x = Inf,
  label = c(
    paste0("Under: ", scales::percent(one_game_rvars$p_under, 0.1)),
    paste0("Over:  ", scales::percent(one_game_rvars$p_over, 0.1))
  )
)

joint_y_prob_plot <- one_game_draws |>
  dplyr::mutate(
    result_bin = dplyr::case_when(
      y_result > spread_line_use ~ home_team,
      y_result < spread_line_use ~ away_team,
      TRUE ~ "Push"
    ),
    total_bin = dplyr::case_when(
      y_total > total_line_use ~ "Over",
      y_total < total_line_use ~ "Under",
      TRUE ~ "Push"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(y_result, y_total)) +
  ggplot2::stat_bin_hex(binwidth = c(2, 2)) +
  ggplot2::scale_fill_viridis_c(
    breaks = scales::breaks_pretty(6),
    labels = scales::label_number(),
    name = "Count"
  ) +
  ggplot2::geom_vline(
    xintercept = spread_line_use,
    linetype = 2,
    color = "red"
  ) +
  ggplot2::geom_hline(
    yintercept = total_line_use,
    linetype = 2,
    color = "red"
  ) +
  ggplot2::geom_label(
    data = quad_labels,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3
  ) +
  ggnewscale::new_scale_fill() +
  ggside::geom_xsidehistogram(
    ggplot2::aes(y = ggplot2::after_stat(count), fill = result_bin),
    binwidth = 1,
    boundary = spread_line_use,
    alpha = 0.7
  ) +
  ggplot2::scale_fill_manual(values = result_fill_values) +
  ggnewscale::new_scale_fill() +
  ggside::geom_ysidehistogram(
    ggplot2::aes(x = ggplot2::after_stat(count), fill = total_bin),
    binwidth = 1,
    boundary = total_line_use,
    alpha = 0.7
  ) +
  ggplot2::scale_fill_manual(values = total_fill_values) +
  ggside::geom_xsidetext(
    data = xside_labels,
    ggplot2::aes(x = x, y = Inf, label = label),
    inherit.aes = FALSE,
    vjust = 1.1,
    size = 3
  ) +
  ggside::geom_ysidetext(
    data = yside_labels,
    ggplot2::aes(x = Inf, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1.1,
    size = 3
  ) +
  ggplot2::scale_x_continuous(minor_breaks = seq(-20, 20, by = 1)) +
  # ggplot2::coord_cartesian(
  #   xlim = c(-21, 21),
  #   ylim = c(20, 70)
  # ) +
  ggplot2::facet_wrap(~game_id) +
  ggplot2::labs(x = "Result", y = "Total") +
  ggplot2::theme_minimal() +
  ggside::theme_ggside_void() +
  ggplot2::theme(
    ggside.panel.scale = 0.25,
    ggside.axis.line = ggplot2::element_blank(),
    ggside.axis.ticks = ggplot2::element_blank(),
    ggside.axis.text = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_line(
      color = "grey90",
      linewidth = 0.3
    ),
    legend.position = "none"
  )
joint_y_prob_plot
plotly::ggplotly(joint_y_prob_plot)
joint_y_prob_plot_build <- ggplot2::ggplot_build(joint_y_prob_plot)
joint_y_prob_plot_build_data <- joint_y_prob_plot_build@data
