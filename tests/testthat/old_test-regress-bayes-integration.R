
test_that("Bayesian multilevel regression runs with brms + Laplace (opt-in)", {
  testthat::skip_if_not_installed("brms")
  testthat::skip_if_not(identical(Sys.getenv("RBESD_RUN_BAYES_TESTS"), "true"))

  df <- make_dummy_besd_df(n_countries = 4, n_per_country = 40, seed = 999)
  x <- rbesd::as_besd(df, dem_dict = rbesd::dem_dictionary())

  fit <- rbesd::besd_regress(
    x,
    outcome = "so_peer",
    predictors = list(common = c("dem_gen"), context = c("dem_age")),
    scope = "multilevel",
    engine = "bayes",
    algorithm = "laplace",
    chains = 1,
    iter = 400,
    warmup = 200,
    refresh = 0
  )

  expect_s3_class(fit, "besd_fit")
  expect_equal(fit$meta$engine, "bayes")
  expect_true(!is.null(fit$fits[["so_peer"]]))
})
