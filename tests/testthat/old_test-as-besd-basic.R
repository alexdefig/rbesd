
test_that("as_besd creates a valid besd_data object with expected attributes", {
  df <- make_dummy_besd_df()

  x <- rbesd::as_besd(
    df,
    besd_dict = rbesd::besd_dictionary(),
    dem_dict  = rbesd::dem_dictionary(),
    country_col = "country"
  )

  expect_s3_class(x, "besd_data")

  info <- rbesd::besd_info(x)
  expect_true(is.data.frame(info$besd_dict))
  expect_true(is.data.frame(info$dem_dict))
  expect_equal(info$country_col, "country")

  # Columns were coerced to appropriate types
  expect_true(is.factor(x$so_peer))
  expect_false(is.ordered(x$so_peer))
  expect_true(is.factor(x$tf_safety))
  expect_true(is.ordered(x$tf_safety))

  # Items registered
  expect_true("so_peer" %in% info$besd_items)
  expect_true(all(c("dem_gen", "dem_age") %in% info$dem_items))

  # Object validates
  expect_true(rbesd::besd_validate(x, strict = TRUE))
})
