# PACKAGE UTILITY FUNCTIONS ----
# Helpers for interacting with the installed package and its Stan models

#' @title List available Stan models in the package
#' @description Returns a tibble of Stan models available in the installed package.
#' @param package Name of the package, defaults to "nflendzoneModel".
#' @return A tibble with columns: `model_name`, `file_path`, and `compiled` (logical).
#' @export
#' @family package-utils
#' @examples
#' \dontrun{
#'   list_package_models()
#' }
list_package_models <- function(package = "nflendzoneModel") {
  path <- system.file("bin", "stan", package = package)
  if (path == "") {
    warning(
      "No 'bin/stan' directory found for package '",
      package,
      "'. Is it installed?"
    )
    return(tibble::tibble(
      model_name = character(0),
      file_path = character(0),
      compiled = logical(0)
    ))
  }

  files <- list.files(path, pattern = "\\.stan$", full.names = TRUE)

  # Check for executables (no extension on Unix, .exe on Windows)
  ext <- if (.Platform$OS.type == "windows") ".exe" else ""
  exec_paths <- file.path(
    dirname(files),
    paste0(tools::file_path_sans_ext(basename(files)), ext)
  )
  compiled_status <- file.exists(exec_paths)

  tibble::tibble(
    model_name = tools::file_path_sans_ext(basename(files)),
    file_path = files,
    compiled = compiled_status
  )
}

#' @title Print Stan model code
#' @description Prints the Stan code for a specific model in the package.
#' @param model Name of the model (without .stan extension).
#' @param package Name of the package, defaults to "nflendzoneModel".
#' @export
#' @family package-utils
#' @examples
#' \dontrun{
#'   print_model_code("team_strength_fit")
#' }
print_model_code <- function(model, package = "nflendzoneModel") {
  models <- list_package_models(package = package)

  if (!model %in% models$model_name) {
    stop(
      "Model '",
      model,
      "' not found in package '",
      package,
      "'. Available models: ",
      paste(models$model_name, collapse = ", ")
    )
  }

  path <- models$file_path[models$model_name == model]
  cat(readLines(path), sep = "\n")
  invisible(path)
}
