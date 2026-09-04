# BeSD dictionary items used as regression predictors (item-on-item models,
# e.g. tf_benefits ~ tf_safety). Ordinal items are stored as ordered factors by
# as_besd(); they must be dummy-coded against a reference level, not fitted with
# polynomial contrasts, so every coefficient decodes in tidy_model().

.mk_item_pred_data <- function() {
  sim <- make_besd_sim(n_countries = 4, n_per_cell = 10)
  suppressWarnings(
    rbesd::as_besd(
      sim$survey,
      country_col = "country",
      dem_dict    = rbesd::dem_dictionary()
    )
  )
}

test_that("an ordinal BeSD predictor is dummy-coded and mapped", {
  x    <- .mk_item_pred_data()
  levs <- rbesd::besd_dictionary()$levels[[
    match("tf_safety", rbesd::besd_dictionary()$item_id)
  ]]

  prep <- suppressWarnings(
    rbesd:::besd_prepare(
      x,
      predictors = list(common = "tf_safety", context = character(0)),
      scope      = "multilevel",
      engine     = "frequentist"
    )
  )

  v <- prep$df[["tf_safety"]]
  expect_true(is.factor(v))
  expect_false(is.ordered(v))
  expect_true(all(grepl("^__[0-9]{2}$", levels(v))))

  mp <- prep$level_map[["tf_safety"]]
  expect_false(is.null(mp))
  expect_equal(mp$label, levs)
  expect_equal(mp$code, sprintf("__%02d", seq_along(levs)))

  terms <- paste0("tf_safety__", sprintf("%02d", seq_along(levs)))
  expect_true(all(terms %in% names(prep$term_map)))
  expect_equal(
    vapply(prep$term_map[terms], function(e) e$level_label, character(1)),
    stats::setNames(levs, terms)
  )
})

test_that("an ordinal BeSD predictor works as a context predictor", {
  x <- .mk_item_pred_data()

  prep <- suppressWarnings(
    rbesd:::besd_prepare(
      x,
      predictors = list(common = "dem_gen", context = "tf_safety"),
      scope      = "multilevel",
      engine     = "frequentist"
    )
  )

  expect_true(length(prep$added_ctx) > 0L)
  expect_true(all(grepl("^ctx_tf_safety_C[0-9]+_[0-9]+$", prep$added_ctx)))
  expect_true(all(!is.na(vapply(
    prep$term_map[prep$added_ctx],
    function(e) e$level_label %||% NA_character_,
    character(1)
  ))))
})

test_that("besd_regress rejects self-prediction and multichoice predictors", {
  x <- .mk_item_pred_data()

  expect_error(
    rbesd::besd_regress(x, outcome = "tf_safety",
                        predictors = list(common = "tf_safety"),
                        scope = "multilevel", engine = "frequentist"),
    "cannot predict itself"
  )

  raw <- make_multichoice_raw(n = 200)
  raw$sp_peer <- sample(c("Yes", "No"), nrow(raw), replace = TRUE)
  xm <- suppressWarnings(rbesd::as_besd(
    raw,
    country_col = "country",
    mapping = c(raw_reason_1 = "pr_reasons_ease_access",
                raw_reason_2 = "pr_reasons_ease_access",
                raw_reason_3 = "pr_reasons_ease_access"),
    multichoice_specs = list(
      pr_reasons_ease_access = list(encoding = "text_prefix", not_prefix = "Not ")
    )
  ))

  expect_error(
    rbesd::besd_regress(xm, outcome = "sp_peer",
                        predictors = list(common = "pr_reasons_ease_access"),
                        scope = "multilevel", engine = "frequentist"),
    "Multichoice item"
  )
})

test_that("tidy_model decodes an ordinal BeSD predictor end to end", {
  skip_if_not_installed("lme4")
  x <- .mk_item_pred_data()

  warnings_seen <- character()
  fit <- withCallingHandlers(
    rbesd::besd_regress(
      x,
      outcome    = "sp_peer",
      predictors = list(common = "tf_safety", context = character(0)),
      scope      = "multilevel",
      engine     = "frequentist"
    ),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  tid <- withCallingHandlers(
    rbesd::tidy_model(fit),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("could not be decoded", warnings_seen)))

  slopes <- tid[tid$param_type == "slope", , drop = FALSE]
  expect_true(nrow(slopes) > 0L)
  expect_true(all(!is.na(slopes$variable)))
  expect_true(all(slopes$variable == "Confidence in vaccine safety"))

  levs <- rbesd::besd_dictionary()$levels[[
    match("tf_safety", rbesd::besd_dictionary()$item_id)
  ]]
  expect_true(all(slopes$level %in% levs))
})

test_that("ref_levels override is reflected in the reported baseline", {
  x <- .mk_item_pred_data()

  prep <- suppressWarnings(
    rbesd:::besd_prepare(
      x,
      predictors = list(common = "tf_safety", context = character(0)),
      scope      = "multilevel",
      engine     = "frequentist",
      ref_levels = list(tf_safety = "Not at all safe")
    )
  )

  mp   <- prep$level_map[["tf_safety"]]
  want <- mp$code[[match("Not at all safe", mp$label)]]

  # The model contrasts against this level ...
  expect_equal(levels(prep$df[["tf_safety"]])[[1]], want)
  # ... and prep$ref_code, which drives the baseline table, agrees.
  expect_equal(prep$ref_code[["tf_safety"]], want)
})
