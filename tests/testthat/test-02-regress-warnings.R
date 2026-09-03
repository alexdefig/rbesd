test_that("besd_regress does not duplicate multilevel complete-case warnings", {
  skip_if_not_installed("lme4")

  set.seed(20260903)
  countries <- paste0("C", 1:4)
  country <- rep(countries, each = 80)
  n <- length(country)

  dem_gen_full <- sample(c("Man", "Woman"), n, replace = TRUE)
  country_shift <- stats::setNames(c(-0.5, -0.1, 0.2, 0.5), countries)
  eta <- -0.2 + 0.6 * (dem_gen_full == "Woman") + country_shift[country]
  sp_peer <- ifelse(stats::runif(n) < stats::plogis(eta), "Yes", "No")

  df <- data.frame(
    country = country,
    sp_peer = sp_peer,
    dem_gen = dem_gen_full,
    stringsAsFactors = FALSE
  )
  df$dem_gen[seq_len(24)] <- NA_character_

  x <- suppressWarnings(rbesd::as_besd(df, dem_dict = rbesd::dem_dictionary()))

  warnings_seen <- character()
  fit <- withCallingHandlers(
    rbesd::besd_regress(
      x,
      outcome = "sp_peer",
      predictors = list(common = "dem_gen"),
      scope = "multilevel",
      engine = "frequentist"
    ),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(fit, "besd_fit")
  expect_equal(
    sum(grepl("^Complete-case deletion \\(multilevel\\)", warnings_seen)),
    1L
  )
})
