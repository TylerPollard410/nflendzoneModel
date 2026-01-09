library(duckdb)
library(duckdbfs)
library(stringr)
library(dbplyr)
library(dplyr)
library(nflreadr)
library(piggyback)


# Global variables
github_data_repo <- "TylerPollard410/nflendzoneData"
github_releases_base_url <- paste0(
  "https://github.com/",
  github_data_repo,
  "/releases/download/"
)

github_release_url <- function(tag, file = "rds") {
  paste0(
    "https://github.com/TylerPollard410/nflendzoneData/releases/download/",
    tag,
    "/",
    tag,
    ".",
    file
  )
}

# github_release_url("season_standings", "rds")

# system.time(
#   t <- load_from_url(
#     url = github_release_url("team_features", "parquet"),
#     seasons = 2025
#   ) |>
#     filter(SRS > 5) |>
#     group_by(team) |>
#     summarise(mean_SRS = mean(SRS, na.rm = TRUE))
# )
# object.size(t)

repo_files_list <- pb_list(repo = github_data_repo)
repo_tags <- repo_files_list |>
  distinct(tag) |>
  pull(tag)
repo_files <- repo_files_list |>
  filter(str_detect(file_name, "timestamp|metadata", negate = TRUE))
repo_files_rds <- repo_files |>
  filter(str_detect(file_name, ".rds$"))
repo_files_parquet <- repo_files |>
  filter(str_detect(file_name, ".parquet$"))

nfl_st_files <- pb_download_url(
  file = repo_files_parquet |>
    pull(file_name) |>
    str_subset(pattern = "nfl_stats_season_team_regpost_"),
  repo = github_data_repo,
  tag = "nfl_stats_season_team_regpost"
)

nfl_st_ds <- open_dataset(
  nfl_st_files,
  format = "parquet",
  filename = TRUE
)
t_names <- nfl_st_ds |> colnames()
t <- nfl_st_ds |>
  filter(season == 2025) |>
  group_by(team) |>
  summarize(mean_pyards = mean(passing_yards, na.rm = TRUE)) |>
  #ungroup() |>
  arrange(desc(mean_pyards))
t <- t |> collect()
t
