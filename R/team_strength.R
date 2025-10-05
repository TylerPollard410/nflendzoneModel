#' @title Fit the NFL team strength model
#' @export
#' @family models
#' @description Fit the NFL team strength state-space Stan model.
#' @param stan_data List of data for Stan model (from prepare_stan_data helpers).
#' @param ... Named arguments to the `sample()` method of CmdStan model objects.
#' @return A CmdStanMCMC fit object.
#' @examples
#' \dontrun{
#'   if (instantiate::stan_cmdstan_exists()) {
#'     # Assuming you have prepared stan_data
#'     fit <- fit_team_strength_model(
#'       stan_data = my_stan_data,
#'       chains = 4,
#'       parallel_chains = 4,
#'       iter_warmup = 1000,
#'       iter_sampling = 2000
#'     )
#'   }
#' }
fit_team_strength_model <- function(stan_data, ...) {
  stopifnot(is.list(stan_data))
  model <- instantiate::stan_package_model(
    name = "team_strength_fit",
    package = "nflendzoneModel"
  )
  fit <- model$sample(data = stan_data, ...)
  fit
}

#' @title Generate predictions from fitted model
#' @export
#' @family models
#' @description Generate predictions using the team strength GQ model.
#' @param fit A CmdStanMCMC fit object from fit_team_strength_model().
#' @param gq_data List of data for GQ model (includes OOS games and future metadata).
#' @param ... Named arguments to the `generate_quantities()` method.
#' @return A CmdStanGQ object with predictions.
#' @examples
#' \dontrun{
#'   if (instantiate::stan_cmdstan_exists()) {
#'     gq <- generate_team_predictions(
#'       fit = my_fit,
#'       gq_data = my_gq_data,
#'       parallel_chains = 4
#'     )
#'   }
#' }
generate_team_predictions <- function(fit, gq_data, ...) {
  stopifnot(inherits(fit, "CmdStanMCMC"))
  stopifnot(is.list(gq_data))
  model <- instantiate::stan_package_model(
    name = "team_strength_gq",
    package = "nflendzoneModel"
  )
  gq <- model$generate_quantities(fitted_params = fit, data = gq_data, ...)
  gq
}

#' @title Predict from team strength model (standalone)
#' @export
#' @family models
#' @description
#' Standalone prediction function that uses the GQ model without requiring
#' the full fit object. This is memory-efficient as you can save just the
#' posterior draws and discard the large fit object.
#'
#' Following the brms pattern, this allows you to:
#' 1. Fit the model and extract draws
#' 2. Save only the draws (much smaller than full fit)
#' 3. Use this function to generate predictions later
#'
#' @param draws Posterior draws from a previous fit. Can be:
#'   \itemize{
#'     \item CmdStanMCMC object (will extract draws automatically)
#'     \item draws_array, draws_matrix, or draws_df from posterior package
#'     \item Path to saved draws (.rds or .csv)
#'   }
#' @param gq_data List of data for GQ model (from prepare_gq_data()).
#' @param ... Additional arguments passed to generate_quantities() (e.g., parallel_chains, seed).
#' @return A CmdStanGQ object with predictions.
#'
#' @examples
#' \dontrun{
#' # Fit model
#' fit <- fit_team_strength_model(stan_data)
#'
#' # Option 1: Use fit object directly
#' gq <- predict_team_strength(fit, gq_data)
#'
#' # Option 2: Save draws and discard fit (memory-efficient)
#' draws <- fit$draws()
#' saveRDS(draws, "model_draws.rds")
#' rm(fit)  # Free memory
#'
#' # Later: Load draws and predict
#' draws <- readRDS("model_draws.rds")
#' gq <- predict_team_strength(draws, gq_data)
#' }
predict_team_strength <- function(draws, gq_data, ...) {
  stopifnot(is.list(gq_data))

  # Load GQ model
  gq_model <- instantiate::stan_package_model(
    name = "team_strength_gq",
    package = "nflendzoneModel"
  )

  # Handle different input types for draws
  if (inherits(draws, "CmdStanMCMC")) {
    # If passed a fit object, use it directly
    gq <- gq_model$generate_quantities(fitted_params = draws, data = gq_data, ...)
  } else if (is.character(draws) && file.exists(draws)) {
    # If passed a file path, load it
    if (grepl("\\.rds$", draws)) {
      loaded_draws <- readRDS(draws)
      gq <- gq_model$generate_quantities(fitted_params = loaded_draws, data = gq_data, ...)
    } else if (grepl("\\.csv$", draws)) {
      loaded_draws <- cmdstanr::read_cmdstan_csv(draws)
      gq <- gq_model$generate_quantities(fitted_params = loaded_draws, data = gq_data, ...)
    } else {
      stop("File must be .rds or .csv format")
    }
  } else {
    # Assume it's already draws in a format cmdstan can use
    gq <- gq_model$generate_quantities(fitted_params = draws, data = gq_data, ...)
  }

  gq
}
