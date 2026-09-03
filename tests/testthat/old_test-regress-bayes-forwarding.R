
test_that("besd_regress forwards brms args and uses Laplace-friendly random effects syntax", {
  df <- make_dummy_besd_df(n_countries = 4, n_per_country = 40)
  x <- rbesd::as_besd(df, dem_dict = rbesd::dem_dictionary())

  captured <- list()

  fit <- testthat::with_mocked_bindings(
    {
      rbesd::besd_regress(
        x,
        outcome = "sp_peer",
        predictors = list(common = c("dem_gen"), context = c("dem_age")),
        scope = "multilevel",
        engine = "bayes",
        random_slopes = TRUE,
        correlated_re = FALSE,
        algorithm = "laplace"
      )
    },
    .fit_model = function(dat, formula, y_type, engine, multilevel, ...) {
      captured <<- list(
        formula = formula,
        y_type = y_type,
        engine = engine,
        multilevel = multilevel,
        dots = list(...)
      )
      structure(list(dummy = TRUE, formula = formula), class = "dummy_fit")
    },
    .package = "rbesd"
  )

  expect_s3_class(fit, "besd_fit")
  expect_equal(captured$engine, "bayes")
  expect_equal(captured$y_type, "binary")
  expect_true(isTRUE(captured$multilevel))
  expect_equal(captured$dots$algorithm, "laplace")

  # When bayes + random_slopes + correlated_re=FALSE, formula uses `||` (uncorrelated RE)
  f_chr <- paste(deparse(captured$formula), collapse = " ")
  expect_true(grepl("\\|\\|", f_chr))

  # The stored fit is our dummy
  expect_s3_class(fit$fits[["sp_peer"]], "dummy_fit")
})

test_that("multichoice outcomes are expanded to multiple binary outcomes", {
  df <- make_dummy_multichoice_raw_df(n = 80)

  mapping <- c(
    raw_reason_1 = "pr_reasons_ease_access",
    raw_reason_2 = "pr_reasons_ease_access",
    raw_reason_3 = "pr_reasons_ease_access"
  )

  x <- suppressWarnings(rbesd::as_besd(
    df,
    mapping = mapping,
    multichoice_specs = list(
      pr_reasons_ease_access = list(encoding = "text_prefix", not_prefix = "Not ")
    ),
    dem_dict = rbesd::dem_dictionary()
  ))

  n_calls <- 0L
  out_names <- character(0)

  fit <- testthat::with_mocked_bindings(
    {
      rbesd::besd_regress(
        x,
        outcome = "pr_reasons_ease_access",
        predictors = list(common = c("dem_gen"), context = character()),
        scope = "multilevel",
        engine = "bayes",
        algorithm = "laplace"
      )
    },
    .fit_model = function(dat, formula, y_type, engine, multilevel, ...) {
      n_calls <<- n_calls + 1L
      out_names <<- unique(c(out_names, all.vars(formula)[1]))
      structure(list(dummy = TRUE), class = "dummy_fit")
    },
    .package = "rbesd"
  )

  expect_s3_class(fit, "besd_fit")
  expect_equal(fit$meta$y_type, "binary")
  # Multichoice has >1 derived outcomes
  expect_true(length(fit$meta$outcomes) > 1)
  expect_equal(n_calls, length(fit$meta$outcomes))
  expect_true(all(fit$meta$outcomes %in% out_names))
})
