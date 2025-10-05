#' nflendzoneModel: NFL Bayesian Modeling Package
#' @name nflendzoneModel-package
#' @description NFL Bayesian state-space models for team strength estimation
#'   and game prediction using pre-compiled Stan models.
#' @family help
#' @importFrom instantiate stan_package_model
#' @importFrom dplyr filter mutate select arrange group_by summarise bind_rows left_join transmute across row_number slice_head
#' @importFrom tibble tibble
#' @importFrom tidyr fill
#' @importFrom posterior as_draws_matrix as_draws_rvars summarise_draws
#' @importFrom tidybayes compose_data n_prefix spread_rvars
#' @importFrom stats sd
#' @importFrom utils modifyList
"_PACKAGE"
