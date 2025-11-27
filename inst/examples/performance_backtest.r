library(arrow)
library(lubridate)
library(piggyback)
library(purrr)
library(dplyr)
library(stringr)

library(Metrics)
library(posterior)
library(tidybayes)

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

# Load game data
# Replace with: game_data_full <- nflendzone::load_game_data(seasons = all_seasons)
game_data_full <- nflendzone::load_game_data(seasons = all_seasons)
# filter(!is.na(result))
# transmute(
#   game_id,
#   season,
#   week,
#   game_type,
#   home_team,
#   away_team,
#   location = if_else(location == "Home", "Home", "Away"),
#   home_score,
#   away_score,
#   result,
#   total,
#   spread_line,
#   home_spread_prob,
#   away_spread_prob
# )

# ============================================================================ #
# 1. Prepare Data for Fitting ----
# ============================================================================ #

# Prepare full schedule with indices (needed for GQ)
schedule_idx <- prepare_schedule_indices(
  game_data_full,
  teams
)

# Filter to training data (before current season, only completed games)
perf_data <- schedule_idx |>
  filter(
    #season == current_season |
    (season == current_season & week <= current_week),
    !is.na(result)
  )

perf_idx_tbl <- perf_data |>
  distinct(
    season,
    week,
    week_idx
  )

perf_pb_list <- pb_list(
  repo = github_data_repo,
  tag = "result_predict"
) |>
  filter(str_detect(file_name, "timestamp", negate = TRUE))

pb_download_url(
  file = perf_pb_list$file_name,
  repo = github_data_repo,
  tag = "result_predict",
  url_type = "browser"
)

test <- pb_read(
  file = perf_pb_list$file_name[1],
  repo = github_data_repo,
  tag = "result_predict"
)

perf_tbls <- perf_idx_tbl |>
  mutate(
    season_week = paste0(season, "_", week)
  ) |>
  pull(season_week) |>
  set_names() |>
  map(\(x) {
    url <- pb_read(
      file = paste0("result_predict_", x, ".rds"),
      repo = github_data_repo,
      tag = "result_predict"
    )
  })

perf_pb_data <- perf_tbls |>
  list_rbind()

perf_df <- perf_data |>
  left_join(
    perf_pb_data,
    by = join_by(
      game_id,
      season,
      week,
      away_team,
      home_team,
      game_idx,
      season_idx,
      week_idx,
      hfa
    )
  ) |>
  arrange(gameday, gametime) |>
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

perf_results <- perf_df |>
  summarise(
    total_games = n(),
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
data.frame(perf_results)


make_init <- function(prev_fit, new_data) {
  nd <- new_data
  d <- posterior::as_draws_df(prev_fit$draws(1)) # single draw
  w_old <- prev_fit$metadata()$data()$N_weeks
  w_new <- nd$N_weeks
  Tm <- nd$N_teams
  init_list <- list(
    phi_league_hfa = d$phi_league_hfa,
    sigma_league_hfa_innovation = d$sigma_league_hfa_innovation,
    phi_weekly_team_strength_innovation = d$phi_weekly_team_strength_innovation,
    sigma_weekly_team_strength_innovation = d$sigma_weekly_team_strength_innovation,
    phi_season_team_strength_innovation = d$phi_season_team_strength_innovation,
    sigma_season_team_strength_innovation = d$sigma_season_team_strength_innovation,
    sigma_team_strength_init = d$sigma_team_strength_init,
    sigma_obs = d$sigma_obs,
    league_hfa_init = d$league_hfa_init,
    z_team_strength_init = array(
      dplyr::last(dplyr::select(d, starts_with("z_team_strength_init"))) %||% 0,
      dim = c(Tm, 1)
    ),
    z_weekly_team_strength_innovation = array(0, dim = c(w_new - 1, Tm))
  )
  # copy old innovations
  z_old <- posterior::draws_of(prev_fit$draws(
    "z_weekly_team_strength_innovation"
  ))[1, , , drop = FALSE]
  n_copy <- min(dim(z_old)[2], w_new - 1)
  if (n_copy > 0) {
    init_list$z_weekly_team_strength_innovation[1:n_copy, ] <- z_old[
      1,
      1:n_copy,
    ]
  }
  # fill any new weeks with small noise
  if (w_new - 1 > n_copy) {
    init_list$z_weekly_team_strength_innovation[(n_copy + 1):(w_new - 1), ] <-
      rnorm((w_new - 1 - n_copy) * Tm, 0, 0.1)
  }
  init_list
}

make_init(fit, gq_stan_data)
