#' @title Get the team strength fit model
#' @export
#' @family models
#' @description Access the pre-compiled team strength fit Stan model.
#' @return A CmdStanModel object for the team strength fit model.
#' @examples
#' if (instantiate::stan_cmdstan_exists()) {
#'   mod <- get_team_strength_fit_model()
#' }
get_team_strength_fit_model <- function() {
  instantiate::stan_package_model(
    name = "team_strength_fit",
    package = "nflendzoneModel"
  )
}

#' @title Get the team strength generated quantities model
#' @export
#' @family models
#' @description Access the pre-compiled team strength GQ Stan model for predictions.
#' @return A CmdStanModel object for the team strength GQ model.
#' @examples
#' if (instantiate::stan_cmdstan_exists()) {
#'   mod <- get_team_strength_gq_model()
#' }
get_team_strength_gq_model <- function() {
  instantiate::stan_package_model(
    name = "team_strength_gq",
    package = "nflendzoneModel"
  )
}

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
  model <- get_team_strength_fit_model()
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
  model <- get_team_strength_gq_model()
  gq <- model$generate_quantities(fitted_params = fit, data = gq_data, ...)
  gq
}
