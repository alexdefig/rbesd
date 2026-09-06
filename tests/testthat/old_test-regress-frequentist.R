
test_that("besd_regress fits by-country frequentist logistic models", {
  df <- make_dummy_besd_df(n_countries = 4, n_per_country = 60)
  x <- rbesd::as_besd(df, dem_dict = rbesd::dem_dictionary())

  fit <- rbesd::besd_regress(
    x,
    outcome = "so_peer",
    predictors = c("dem_gen", "dem_age"),
    scope = "by_country",
    engine = "frequentist"
  )

  expect_s3_class(fit, "besd_fit_by_country")
  expect_equal(fit$meta$scope, "by_country")
  expect_equal(fit$meta$engine, "frequentist")
  expect_equal(fit$meta$outcome, "so_peer")
  expect_true(length(fit$fits) >= 2)

  # At least one fitted model exists and is a glm
  some_glm <- FALSE
  for (cc in names(fit$fits)) {
    m <- fit$fits[[cc]][["so_peer"]]
    if (!is.null(m) && inherits(m, "glm")) {
      some_glm <- TRUE
      break
    }
  }
  expect_true(some_glm)
})

test_that("besd_regress fits multilevel frequentist models when lme4 is available", {
  skip_if_not_installed("lme4")

  df <- make_dummy_besd_df(n_countries = 5, n_per_country = 50)
  x <- rbesd::as_besd(df, dem_dict = rbesd::dem_dictionary())

  fit <- rbesd::besd_regress(
    x,
    outcome = "so_peer",
    predictors = list(common = c("dem_gen"), context = c("dem_age")),
    scope = "multilevel",
    engine = "frequentist"
  )

  expect_s3_class(fit, "besd_fit")
  expect_equal(fit$meta$scope, "multilevel")
  expect_equal(fit$meta$engine, "frequentist")

  m <- fit$fits[["so_peer"]]
  expect_true(inherits(m, "glmerMod") || inherits(m, "merMod"))
})
