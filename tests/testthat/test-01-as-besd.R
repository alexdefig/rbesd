# as_besd() ingest contract: types, levels, design columns, multichoice
# packing, and the unknown/missing token policies.

sim <- make_besd_sim(n_countries = 4, n_per_cell = 5)
lv  <- sim$params$levels

# Dictionary genders include levels the sample never contains (Nonbinary,
# Other), which as_besd() warns about; that is expected, not a failure.
build <- function(df, ...) suppressWarnings(as_besd(df, dem_dict = dem_dictionary(), ...))


test_that("as_besd() applies dictionary types and levels", {
  x <- build(sim$survey)

  expect_s3_class(x, "besd_data")

  # Ordinal items become ordered factors; binary ones do not.
  expect_true(is.ordered(x$tf_safety))
  expect_false(is.ordered(x$sp_peer))
  expect_true(is.factor(x$sp_peer))

  # Levels come from the dictionary, in dictionary order.
  expect_identical(levels(x$tf_safety), lv$safe)
  expect_identical(levels(x$sp_peer),   lv$peer)

  # Items are registered and the object validates.
  info <- besd_info(x)
  expect_setequal(info$besd_items, c("sp_peer", "tf_safety"))
  expect_setequal(info$dem_items,  c("dem_gen", "dem_age"))
  expect_true(besd_validate(x, strict = TRUE))
})


test_that("as_besd() round-trips the survey design columns", {
  x <- build(sim$survey, stratum_col = "stratum", psu_col = "psu",
             weight_col = "wt", id_col = "rid")
  info <- besd_info(x)

  expect_identical(info$country_col, "country")
  expect_identical(info$stratum_col, "stratum")
  expect_identical(info$psu_col,     "psu")
  expect_identical(info$weight_col,  "wt")
  expect_identical(info$id_col,      "rid")
  expect_true(all(c("stratum", "psu", "wt", "rid") %in% names(x)))

  # Unclaimed columns are dropped unless keep_original = TRUE.
  y <- build(sim$survey)
  expect_false(any(c("stratum", "psu", "wt", "rid") %in% names(y)))
  expect_true(all(c("stratum", "psu", "wt", "rid") %in% names(build(sim$survey, keep_original = TRUE))))
})


test_that("as_besd() packs multiple raw columns into one multichoice item", {
  raw  <- make_multichoice_raw()
  opts <- besd_dictionary()$levels[[match("pr_reasons_ease_access", besd_dictionary()$item_id)]][1:3]

  # The fixture uses 3 of the 12 dictionary options, which as_besd() warns about.
  x <- suppressWarnings(as_besd(
    raw,
    mapping = c(raw_reason_1 = "pr_reasons_ease_access",
                raw_reason_2 = "pr_reasons_ease_access",
                raw_reason_3 = "pr_reasons_ease_access"),
    multichoice_specs = list(
      pr_reasons_ease_access = list(encoding = "text_prefix", not_prefix = "Not ")
    )
  ))

  expect_true("pr_reasons_ease_access" %in% names(x))
  expect_type(x$pr_reasons_ease_access, "character")

  # Selected options are packed into one string separated by .BESD_SEP, and
  # every unpacked token is a real dictionary option.
  packed <- x$pr_reasons_ease_access[!is.na(x$pr_reasons_ease_access)]
  tokens <- unlist(strsplit(packed, rbesd:::.BESD_SEP, fixed = TRUE))
  expect_true(all(setdiff(tokens, "") %in% opts))
  expect_true(any(grepl(rbesd:::.BESD_SEP, packed, fixed = TRUE)))
})


test_that("unknown values error or become NA per unknown_action", {
  bad <- sim$survey
  bad$sp_peer[1:3] <- "Maybe"

  expect_error(build(bad, unknown_action = "error"), "unknown value")

  x <- build(bad, unknown_action = "na")
  expect_true(all(is.na(x$sp_peer[1:3])))
  expect_false("Maybe" %in% levels(x$sp_peer))
})


test_that("missing tokens are kept as levels or NA'd per missing_action", {
  tok <- sim$survey
  tok$sp_peer[1:4] <- "Prefer not to say"

  keep <- build(tok, missing_tokens = "Prefer not to say", missing_action = "keep")
  expect_true("Prefer not to say" %in% levels(keep$sp_peer))
  expect_false(any(is.na(keep$sp_peer[1:4])))

  nad <- build(tok, missing_tokens = "Prefer not to say", missing_action = "na")
  expect_false("Prefer not to say" %in% levels(nad$sp_peer))
  expect_true(all(is.na(nad$sp_peer[1:4])))
})
