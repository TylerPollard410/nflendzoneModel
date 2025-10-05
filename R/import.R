#' Import CmdStan model output files from a GitHub release
#'
#' Downloads CmdStan CSV output files from a GitHub release and reads them
#' into a CmdStanMCMC object. Automatically detects the number of chains
#' available in the release.
#'
#' @param tag Release tag name.
#' @param repo GitHub repo (username/repo).
#' @param dest Directory to download files to (default: tempdir()).
#' @param cleanup Logical: remove downloaded files after reading? (default: TRUE)
#' @return A CmdStanMCMC object with all available chains.
#' @export
import_cmdstan_model <- function(
  tag,
  repo,
  dest = tempdir(),
  cleanup = TRUE
) {
  # Get list of files in the release
  release_files <- piggyback::pb_list(repo = repo, tag = tag)

  # Filter for CSV files matching the tag pattern
  csv_pattern <- paste0("^", tag, "-(\\d+)\\.csv$")
  csv_files <- release_files$file_name[grepl(
    csv_pattern,
    release_files$file_name
  )]

  if (length(csv_files) == 0L) {
    stop(
      sprintf(
        "No CmdStan CSV files found for tag '%s' in repo '%s'",
        tag,
        repo
      ),
      call. = FALSE
    )
  }

  # Download all CSV files
  purrr::walk(
    csv_files,
    ~ piggyback::pb_download(
      file = .x,
      repo = repo,
      tag = tag,
      dest = dest,
      overwrite = TRUE
    )
  )

  # Read into CmdStanMCMC object
  csv_paths <- file.path(dest, csv_files)
  model <- cmdstanr::as_cmdstan_fit(files = csv_paths)

  # Cleanup if requested
  if (cleanup) {
    purrr::walk(csv_paths, unlink)
  }

  cat(sprintf(
    "[%s] Imported %d chain%s from '%s'\n",
    tag,
    length(csv_files),
    if (length(csv_files) == 1) "" else "s",
    repo
  ))

  return(model)
}
