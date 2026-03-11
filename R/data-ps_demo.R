#' Faux poststratification frame for the BeSD demo dataset
#'
#' A synthetic census-style data frame bundled with **rbesd** for demonstrating
#' the poststratification pipeline. It is derived from [data_demo] by counting
#' the observed combinations of country × gender × age, then scaling each cell
#' count by a random integer in \[800, 1200\] to produce census-plausible
#' population numbers that deliberately deviate from the raw survey proportions,
#' so that MrP estimates differ visibly from unweighted survey estimates.
#'
#' Use this frame with [besd_poststrat_frame()], [besd_fitted_probs()], and
#' [besd_poststratify()] to run a full MrP workflow on the demo dataset.
#'
#' **Important:** these are synthetic population counts for illustration only.
#' They should not be used for serious or representative analysis.
#'
#' @format A data frame with 4 columns:
#' \describe{
#'   \item{country}{Country (Brazil, Mexico, Senegal, Philippines, The UK).}
#'   \item{dem_gen}{Gender of the cell (matches levels in [data_demo]).}
#'   \item{dem_age}{Age group of the cell (matches levels in [data_demo]).}
#'   \item{n_pop}{Synthetic population cell count (observed count × random
#'     scalar in \[800, 1200\], seed 42).}
#' }
#'
#' @details
#' Rows where `dem_gen` or `dem_age` was `NA` or a sparse category
#' (`"Other"`, `"Prefer not to say"`) in [data_demo] are excluded, reflecting
#' the convention that a census frame contains only well-defined demographic
#' cells. Only common predictors are included; context-specific predictors
#' (those with country-varying levels) are not supported in
#' [besd_poststrat_frame()] and must not appear here.
#'
#' @seealso [data_demo], [besd_poststrat_frame()], [besd_poststratify()]
#'
#' @examples
#' data("ps_demo", package = "rbesd")
#' head(ps_demo)
#'
#' # Pass to besd_poststrat_frame() after fitting a model on data_demo:
#' # frame <- besd_poststrat_frame(ps_demo, fit = my_fit, pop_col = "n_pop")
#'
#' @source
#' Derived from [data_demo], which is sampled from IMMS survey data:
#' \url{https://osf.io/6pxhk/}.
"ps_demo"
