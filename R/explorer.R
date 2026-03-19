#' Prepare data files for the BeSD Explorer Shiny app
#'
#' Computes and saves the four data files that the Explorer needs. Run this
#' once after creating your \code{besd_data} object, and again whenever your
#' underlying data changes.
#'
#' The function works through four steps and reports progress as it goes:
#'
#' \enumerate{
#'   \item \strong{besd_sum} (required) — overall response distributions, computed
#'     via \code{summary()}.
#'   \item \strong{demo_sum} (required) — sample composition by demographic group,
#'     computed via \code{besd_summary_demographics()}.
#'   \item \strong{topbox_all} (required) — top-box scores used by the map and
#'     ranked-items view, computed via \code{besd_topbox()}.
#'   \item \strong{breakdown_sum} (optional) — response distributions broken down
#'     by each demographic variable, computed via \code{besd_summary_by()}.
#'     This powers the \emph{BeSD by Demographic} tab. It is skipped
#'     automatically if your \code{besd_data} object has no demographic
#'     variables (\code{dem_dict = NULL}), and the tab will simply be empty.
#'     You can also skip it intentionally by passing \code{dem_vars = character(0)}.
#' }
#'
#' @param besd_data A \code{besd_data} object created by \code{\link{as_besd}}.
#' @param data_dir Directory where the \code{.rds} files will be saved.
#'   Defaults to the standard user data directory for rbesd
#'   (\code{tools::R_user_dir("rbesd", "data")}), which persists across
#'   package updates. The directory is created if it does not exist.
#' @param conf_level Numeric. Confidence/credible interval width passed to
#'   \code{summary()} and \code{besd_summary_by()}. Default \code{0.95}.
#' @param exclude_missing Logical. Passed as \code{exclude_missing_tokens} to
#'   summary functions. Default \code{TRUE}.
#' @param dem_vars Character vector of demographic variable IDs to use for
#'   breakdown summaries (step 4). If \code{NULL} (default), the function
#'   reads them from the \code{dem_dict} attribute of \code{besd_data}. Pass
#'   \code{character(0)} to skip the breakdown step entirely.
#' @param overwrite Logical. If \code{FALSE}, existing files are left in place
#'   and a message is printed instead of overwriting. Default \code{TRUE}.
#'
#' @return Invisibly returns \code{data_dir}.
#'
#' @seealso \code{\link{launch_explorer}}
#' @export
prepare_explorer_data <- function(
    besd_data,
    data_dir        = tools::R_user_dir("rbesd", which = "data"),
    conf_level      = 0.95,
    exclude_missing = TRUE,
    dem_vars        = NULL,
    overwrite       = TRUE) {

  if (!inherits(besd_data, "besd_data"))
    stop("`besd_data` must be a besd_data object created by as_besd().",
         call. = FALSE)

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message("Created data directory: ", data_dir)
  }

  .save_rds <- function(obj, filename) {
    path <- file.path(data_dir, filename)
    if (!overwrite && file.exists(path)) {
      message("  Skipping ", filename, " (already exists; use overwrite = TRUE to replace)")
      return(invisible(NULL))
    }
    saveRDS(obj, path)
    message("  Saved: ", path)
  }

  # ── Step 1: besd_sum (required) ─────────────────────────────────────────────
  message("Step 1/4: Computing BeSD summary (besd_sum) ...")
  besd_sum <- summary(besd_data,
                      conf_level             = conf_level,
                      exclude_missing_tokens = exclude_missing)
  .save_rds(besd_sum, "besd_sum.rds")

  # ── Step 2: demo_sum (required) ─────────────────────────────────────────────
  message("Step 2/4: Computing demographic composition summary (demo_sum) ...")
  demo_sum <- besd_summary_demographics(besd_sum)
  .save_rds(demo_sum, "demo_sum.rds")

  # ── Step 3: topbox_all (required) ───────────────────────────────────────────
  message("Step 3/4: Computing top-box scores (topbox_all) ...")
  topbox_all <- besd_topbox(besd_sum)
  .save_rds(topbox_all, "topbox_all.rds")

  # ── Step 4: breakdown_sum (optional) ────────────────────────────────────────
  message("Step 4/4: Computing demographic breakdowns (breakdown_sum) ...")
  message("  This powers the 'BeSD by Demographic' tab. ",
          "Skipped automatically if no demographic variables are present.")

  if (is.null(dem_vars)) {
    dem_dict <- attr(besd_data, "dem_dict")
    dem_vars <- if (!is.null(dem_dict)) unique(dem_dict$item_id) else character(0)
  }

  if (length(dem_vars) == 0L) {
    message("  No demographic variables found — saving empty breakdown_sum.")
    breakdown_sum <- tibble::tibble()
  } else {
    message("  Variables: ", paste(dem_vars, collapse = ", "))
    breakdown_sum <- dplyr::bind_rows(lapply(dem_vars, function(v) {
      tryCatch(
        besd_summary_by(besd_data, by_col = v,
                        conf_level             = conf_level,
                        exclude_missing_tokens = exclude_missing),
        error = function(e) {
          message("  Skipped '", v, "': ", conditionMessage(e))
          NULL
        }
      )
    }))
  }
  .save_rds(breakdown_sum, "breakdown_sum.rds")

  message("\nDone. All data files saved to:\n  ", data_dir)
  message("Launch the Explorer with:\n  launch_explorer()")
  invisible(data_dir)
}


#' Launch the BeSD Explorer Shiny app
#'
#' Opens the interactive BeSD Explorer dashboard in your default browser.
#'
#' If \code{data_dir} is not supplied, the function first looks for user data
#' prepared by \code{\link{prepare_explorer_data}} in the default location
#' (\code{tools::R_user_dir("rbesd", "data")}). If none is found there, it
#' falls back to the bundled demo dataset so you can explore the app
#' immediately.
#'
#' @param data_dir Path to a directory containing the \code{.rds} files
#'   produced by \code{\link{prepare_explorer_data}}. If \code{NULL}
#'   (default), the standard user data directory is tried first, then the
#'   bundled demo data.
#'
#' @seealso \code{\link{prepare_explorer_data}}
#' @export
launch_explorer <- function(data_dir = NULL) {
  app_dir <- system.file("shiny/explorer", package = "rbesd")
  if (!nzchar(app_dir))
    stop("Could not find the BeSD Explorer app. ",
         "Make sure rbesd is installed correctly.", call. = FALSE)

  if (is.null(data_dir)) {
    user_dir <- tools::R_user_dir("rbesd", which = "data")
    if (dir.exists(user_dir) &&
        file.exists(file.path(user_dir, "besd_sum.rds"))) {
      data_dir <- user_dir
      message("Using your data from: ", data_dir)
    } else {
      data_dir <- file.path(app_dir, "data")
      message("No user data found. Launching with bundled demo data.")
      message("To use your own data, run: prepare_explorer_data(your_besd_object)")
    }
  }

  required <- c("besd_sum.rds", "demo_sum.rds", "topbox_all.rds")
  missing_files <- required[!file.exists(file.path(data_dir, required))]
  if (length(missing_files))
    stop(
      "Required data file(s) not found in '", data_dir, "':\n",
      paste0("  - ", missing_files, collapse = "\n"), "\n",
      "Run prepare_explorer_data(your_besd_object) to generate them.",
      call. = FALSE
    )

  shiny::shinyOptions(besd_data_dir = data_dir)
  shiny::runApp(app_dir, display.mode = "normal")
}
