
test_that("as_besd handles missing tokens and unknown values as configured", {
  df <- make_dummy_besd_df(n_countries = 3, n_per_country = 30)

  # Inject missing tokens and unknown values
  df$sp_peer[1:5] <- "Don't know"
  df$dem_gen[6:8] <- "Prefer not to say"  # unknown to dictionary

  # unknown_action = "error" should stop on unknown
  expect_error(
    rbesd::as_besd(
      df,
      dem_dict = rbesd::dem_dictionary(),
      missing_tokens = c("Don't know"),
      missing_action = "na",
      unknown_action = "error"
    ),
    "not in dictionary levels"
  )

  # unknown_action = "na" should coerce unknown to NA (with a warning by default)
  expect_warning(
    x <- rbesd::as_besd(
      df,
      dem_dict = rbesd::dem_dictionary(),
      missing_tokens = c("Don't know"),
      missing_action = "na",
      unknown_action = "na",
      warn_on_unknown = TRUE
    ),
    "Dropping|Coercing|unknown"
  )

  # missing tokens were turned into NA
  expect_true(all(is.na(x$sp_peer[1:5])))

  # unknown dem_gen values become NA
  expect_true(all(is.na(x$dem_gen[6:8])))

  # missing_action = "keep" keeps the token as an explicit level
  x2 <- rbesd::as_besd(
    df,
    dem_dict = rbesd::dem_dictionary(),
    missing_tokens = c("Don't know"),
    missing_action = "keep",
    unknown_action = "na",
    warn_on_unknown = FALSE
  )
  expect_true("Don't know" %in% levels(x2$sp_peer))
  expect_equal(as.character(x2$sp_peer[1]), "Don't know")
})
