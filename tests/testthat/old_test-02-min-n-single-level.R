
# test-min-n-single-level.R
#
# Tests the behaviour when a predictor has only two levels in a country but
# one of them falls below min_n, leaving a single effective level.
#
# There are two things to verify:
#
#   Test 1 (.prep_predictors): the rare level's observations are correctly
#   NA-ed in the affected country and only that country.
#
#   Test 2 (besd_regress pipeline): the variable is silently dropped from the
#   affected country's model with no error. This documents an intentional
#   behaviour: it is the user's responsibility to notice via tidy_model() that
#   a country is missing a predictor. The test pins this behaviour so that if
#   it ever changes (e.g. to a warning) the change is deliberate.


# ── Test 1: .prep_predictors NAs the rare level in the correct country only ───
#
# Country A: LevelX=50, LevelY=5  — LevelY below min_n=10, should be NA-ed
# Country B: LevelX=40, LevelY=30 — both above min_n=10, neither affected
#
# After prep:
#   - all LevelY rows in country A should be NA
#   - LevelY rows in country B should be intact
#   - LevelX rows in both countries should be unaffected

test_that(".prep_predictors NAs a rare level in the affected country only", {
  
  df <- data.frame(
    country  = c(rep("A", 55), rep("B", 70)),
    groupvar = c(
      rep("LevelX", 50), rep("LevelY",  5),   # A: LevelY rare
      rep("LevelX", 40), rep("LevelY", 30)    # B: both fine
    ),
    stringsAsFactors = FALSE
  )
  
  result <- rbesd:::.prep_predictors(
    df,
    predictors = "groupvar",
    group_col  = "country",
    min_n      = 10L,
    ref_rule   = "mode"
  )
  
  lm        <- result$level_map[["groupvar"]]
  y_code    <- lm$code[match("LevelY", lm$label)]
  x_code    <- lm$code[match("LevelX", lm$label)]
  encoded   <- as.character(result$df$groupvar)
  is_A      <- df$country == "A"
  is_B      <- df$country == "B"
  
  # LevelY observations in country A are all NA
  expect_true(
    all(is.na(encoded[is_A & df$groupvar == "LevelY"])),
    label = "LevelY rows in country A are NA after min_n filtering"
  )
  expect_equal(
    sum(is.na(encoded[is_A])), 5L,
    label = "exactly 5 observations NA-ed in country A"
  )
  
  # LevelY observations in country B are intact
  expect_equal(
    sum(!is.na(encoded[is_B]) & encoded[is_B] == y_code), 30L,
    label = "LevelY rows in country B are unaffected"
  )
  
  # LevelX is intact in both countries
  expect_equal(
    sum(!is.na(encoded[is_A]) & encoded[is_A] == x_code), 50L,
    label = "LevelX rows in country A unaffected"
  )
  expect_equal(
    sum(!is.na(encoded[is_B]) & encoded[is_B] == x_code), 40L,
    label = "LevelX rows in country B unaffected"
  )
})


# ── Test 2: besd_regress silently drops the single-effective-level variable ───
#
# Country A: groupvar has only LevelX after min_n filtering — single level,
#            no contrast possible, variable must be absent from model terms.
# Country B: groupvar has both levels — variable present in model terms.
#
# The test also verifies no error is thrown: the pipeline should complete
# successfully even though one country cannot fit the predictor.

test_that("besd_regress silently drops predictor reduced to one level by min_n", {
  
  set.seed(1)
  
  n_A <- 100; n_B <- 100
  df <- data.frame(
    country  = c(rep("A", n_A), rep("B", n_B)),
    groupvar = c(
      # Country A: LevelY has only 6 obs — below min_n=10
      sample(c(rep("LevelX", 94), rep("LevelY", 6))),
      # Country B: both levels well represented
      sample(c(rep("LevelX", 55), rep("LevelY", 45)))
    ),
    # Binary outcome with genuine signal so models are estimable
    outcome  = c(
      sample(c("Yes", "No"), n_A, replace = TRUE, prob = c(0.6, 0.4)),
      sample(c("Yes", "No"), n_B, replace = TRUE, prob = c(0.4, 0.6))
    ),
    stringsAsFactors = FALSE
  )
  
  # Build a minimal besd_data-like structure
  # Use a plain as_besd call with a hand-rolled dem_dict
  dem_dict <- tibble::tibble(
    item_id        = "groupvar",
    question_short = "Group variable",
    question       = "Group variable",
    levels         = list(c("LevelX", "LevelY")),
    item_type      = "categorical",
    reverse        = FALSE
  )
  besd_dict <- tibble::tibble(
    item_id        = "outcome",
    question_short = "Outcome",
    question       = "Outcome",
    levels         = list(c("Yes", "No")),
    item_type      = "binary",
    reverse        = FALSE,
    domain         = NA_character_
  )
  
  x <- rbesd::as_besd(
    df,
    country_col = "country",
    besd_dict   = besd_dict,
    dem_dict    = dem_dict
  )
  
  # Should complete without error
  expect_no_error({
    fit <- rbesd::besd_regress(
      x,
      outcome    = "outcome",
      predictors = "groupvar",
      scope      = "by_country",
      engine     = "frequentist"
    )
  })
  
  # Country A: model exists but groupvar coefficient should be absent
  # (variable dropped because only one level after min_n filtering)
  model_A <- fit$fits[["A"]][["outcome"]]
  if (!is.null(model_A)) {
    coef_names_A <- names(stats::coef(model_A))
    has_groupvar_A <- any(grepl("groupvar", coef_names_A))
    expect_false(
      has_groupvar_A,
      label = "groupvar absent from country A model (single level after min_n)"
    )
  }
  
  # Country B: groupvar coefficient should be present
  model_B <- fit$fits[["B"]][["outcome"]]
  expect_false(
    is.null(model_B),
    label = "country B model was fitted"
  )
  coef_names_B <- names(stats::coef(model_B))
  has_groupvar_B <- any(grepl("groupvar", coef_names_B))
  expect_true(
    has_groupvar_B,
    label = "groupvar present in country B model (both levels retained)"
  )
})
