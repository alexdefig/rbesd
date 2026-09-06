
test_that("as_besd can map and pack multichoice items from multiple raw columns", {
  df <- make_dummy_multichoice_raw_df(n = 100)

  mapping <- c(
    raw_reason_1 = "pr_reasons_ease_access",
    raw_reason_2 = "pr_reasons_ease_access",
    raw_reason_3 = "pr_reasons_ease_access"
  )

  expect_warning(
  x <- rbesd::as_besd(
    df,
    besd_dict = rbesd::besd_dictionary(),
    dem_dict  = rbesd::dem_dictionary(),
    mapping = mapping,
    multichoice_specs = list(
      pr_reasons_ease_access = list(encoding = "text_prefix", not_prefix = "Not ")
    ),
    country_col = "country",
    keep_original = FALSE
  ),
  "Some multichoice dictionary level\\(s\\) not observed"
)

  expect_s3_class(x, "besd_data")
  expect_true("pr_reasons_ease_access" %in% names(x))

  # Packed string uses the package separator when multiple options are selected
  sep <- rbesd:::.BESD_SEP
  packed <- as.character(x$pr_reasons_ease_access)
  any_multi <- any(grepl(sep, packed, fixed = TRUE), na.rm = TRUE)
  expect_true(any_multi)

  # It should validate (warnings about unseen levels are acceptable)
  expect_true(rbesd::besd_validate(x, strict = TRUE))
})

test_that("mapping multiple raw columns to a non-multichoice item errors", {
  df <- make_dummy_multichoice_raw_df(n = 10)

  mapping <- c(
    raw_reason_1 = "so_peer",
    raw_reason_2 = "so_peer"
  )

  expect_error(
    rbesd::as_besd(
      df,
      mapping = mapping,
      dem_dict = rbesd::dem_dictionary()
    ),
    "Multiple raw cols mapped"
  )
})
