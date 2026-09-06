#' BeSD survey demo dataset
#'
#' A small, **non-representative** demo dataset bundled with **rbesd** for examples,
#' tests, and vignettes. It is derived from IMMS survey data and has been **sampled and
#' recoded** into the harmonised BeSD variable names used by this package.
#'
#' The BeSD item columns (`tf_*`, `sp_*`, `pr_*`) correspond to entries in
#' [besd_dictionary()]. The socio-demographic columns (`dem_*`) correspond to
#' [dem_dictionary()].
#'
#' **Important:** this is a convenience dataset for demonstrating package functionality.
#' It should not be used for serious or representative analysis.
#'
#' @format A data frame with 22 columns:
#' \describe{
#'   \item{ADM1}{Country (Brazil, Canada, Senegal, Philippines, The UK).}
#'
#'   \item{tf_benefits}{Thinking & feeling (ordinal): confidence in vaccine benefits.}
#'   \item{tf_safety}{Thinking & feeling (ordinal): confidence in vaccine safety.}
#'   \item{tf_hcws}{Thinking & feeling (ordinal): confidence in health workers.}
#'
#'   \item{so_peer}{Social processes (binary): peer norms.}
#'   \item{so_family}{Social processes (binary): family norms.}
#'   \item{so_religious}{Social processes (binary): religious leader norms.}
#'   \item{so_community}{Social processes (binary): community leader norms.}
#'   \item{so_hcw_rec}{Social processes (binary): health worker recommendation.}
#'   \item{so_travel_autonomy}{Social processes (binary): mother's travel autonomy 
#'    (reverse-coded in dictionary).}
#'
#'   \item{pr_recall}{Practical issues (binary): received recall.}
#'   \item{pr_know_where}{Practical issues (binary): know where to go.}
#'   \item{pr_took_child}{Practical issues (binary): took child for vaccination.}
#'   \item{pr_turned_away}{Practical issues (binary): turned away (reverse-coded in 
#'    dictionary).}
#'   \item{pr_ease_access}{Practical issues (ordinal): ease of access.}
#'   \item{pr_afford}{Practical issues (ordinal): affordability.}
#'   \item{pr_service_satisfaction}{Practical issues (ordinal): service satisfaction.}
#'
#'   \item{pr_reasons_ease_access}{Practical issues (multichoice): reasons for low ease 
#'    of access (packed format).}
#'   \item{pr_service_quality}{Practical issues (multichoice): service quality issues 
#'    (packed format).}
#'
#'   \item{dem_gen}{Socio-demographic predictor: gender.}
#'   \item{dem_age}{Socio-demographic predictor: age group.}
#'   \item{dem_eth}{Socio-demographic predictor: ethnicity.}
#' }
#'
#' @details
#' The "dictionary truth" (question text, response levels, reverse-coding) lives in
#' [besd_dictionary()] and [dem_dictionary()]. Use those to inspect what each variable 
#' means.
#'
#' @examples
#' # Load rbesd demo dataset
#' data("data_demo", package = "rbesd")
#'
#' x <- as_besd(data_demo, country_col = "ADM1", dem_dict = dem_dictionary())
#' besd_info(x)
#'
#' @source
#' Derived and sampled from IMMS survey data hosted on OSF: \url{https://osf.io/6pxhk/}.
"data_demo"
