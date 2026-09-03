# test-predictor-encoding.R
#
# Tests that .prep_predictors() encodes factor levels faithfully and that
# .parse_terms() decodes them back to the correct (readable) labels. The following tests
# are performed:
#   1  Common-predictor encoding
#   2  Context-predictor encoding
#   3 


# Test 1: common predictor encoding (ungrouped) -------------------------------------
#
# Scenario: a gender variable with three levels where "Other" is the modal category.
# This exercises both the basic encoding logic AND the catch-all avoidance added to 
# .pick_ref(): "Other" shoudl never be the reference even though it has the most 
# observations.
#
# What we are asserting:
#   (a) level_map is created with one row per level, codes in __01/__02/... format, 
#       labels matching the originals exactly
#   (b) the factor stored in the returned df contains only codes (not labels)
#   (c) NA values in the input are preserved as NA in the output
#   (d) the reference level is NOT "Other" — it should be the modal non-catch-all
#       level ("Woman", 40 obs vs "Man", 30 obs)

test_that(".prep_predictors encodes a common (ungrouped) factor correctly", {
  
  df <- data.frame(
    # "Other" is deliberately the most common — should never be chosen as ref
    gender = c(rep("Other", 60), rep("Woman", 40), rep("Man", 30), NA, NA),
    stringsAsFactors = FALSE
  )
  
  result <- rbesd:::.prep_predictors(
    df,
    predictors = "gender",
    group_col  = NULL,
    min_n      = 0L,
    ref_rule   = "mode"
  )
  
  lm <- result$level_map[["gender"]]
  
  # (a) level_map exists and has the right structure
  expect_true(
    !is.null(lm),
    label = "level_map entry created for 'gender'"
  )
  expect_equal(
    sort(lm$label),
    sort(c("Man", "Other", "Woman")),
    label = "level_map labels match original levels"
  )
  expect_true(
    all(grepl("^__[0-9]{2,}$", lm$code)),
    label = "level_map codes follow __NN format"
  )
  expect_equal(
    nrow(lm), 3L,
    label = "one level_map row per level"
  )
  
  # (b) returned df factor contains only codes, not original labels
  encoded_vals <- as.character(result$df$gender)
  expect_true(
    all(encoded_vals[!is.na(encoded_vals)] %in% lm$code),
    label = "encoded column values are all codes, not original labels"
  )
  expect_false(
    any(c("Man", "Woman", "Other") %in% encoded_vals),
    label = "original labels are absent from encoded column"
  )
  
  # (c) NAs are preserved
  expect_equal(
    sum(is.na(result$df$gender)),
    2L,
    label = "NA values in input are preserved as NA in output"
  )
  
  # (d) reference level is not "Other" (catch-all avoidance)
  ref_code  <- result$ref_code[["gender"]]
  ref_label <- lm$label[match(ref_code, lm$code)]
  expect_false(
    identical(ref_label, "Other"),
    label = "catch-all 'Other' is not chosen as reference"
  )
  # Modal non-catch-all is "Woman" (40 obs > "Man" 30 obs)
  expect_equal(
    ref_label, "Woman",
    label = "modal non-catch-all level 'Woman' is chosen as reference"
  )
  # Reference level is first in the encoded factor
  expect_equal(
    levels(result$df$gender)[[1L]],
    ref_code,
    label = "reference code is the first factor level in encoded column"
  )
})


# ── Test 2: context predictor encoding (grouped, per-country) ─────────────────
#
# Scenario: an ethnicity variable across two countries.
#   Country A: Akan=50, Ewe=30, Other=25  — all above min_n=10
#   Country B: Yoruba=40, Hausa=35, Igbo=8 — Igbo is below min_n=10
#
# What we are asserting:
#   (a) per-country reference for A is "Akan" (modal non-catch-all), NOT "Other"
#   (b) per-country reference for B is "Yoruba" (modal non-catch-all)
#   (c) "Igbo" observations in country B are NA-ed out (below min_n)
#   (d) "Other" observations in country A are NOT NA-ed (count >= min_n)
#   (e) observations in country A are unaffected by country B's rare-level rule

test_that(".prep_predictors applies per-country refs and min_n correctly", {
  
  df <- data.frame(
    country   = c(rep("A", 105), rep("B", 83)),
    ethnicity = c(
      # Country A: Akan=50, Ewe=30, Other=25
      rep("Akan",  50), rep("Ewe", 30), rep("Other", 25),
      # Country B: Yoruba=40, Hausa=35, Igbo=8
      rep("Yoruba", 40), rep("Hausa", 35), rep("Igbo", 8)
    ),
    stringsAsFactors = FALSE
  )
  
  result <- rbesd:::.prep_predictors(
    df,
    predictors = "ethnicity",
    group_col  = "country",
    min_n      = 10L,
    ref_rule   = "mode"
  )
  
  lm   <- result$level_map[["ethnicity"]]
  refs <- result$ref_code_by_group[["ethnicity"]]
  
  # (a) country A reference is "Akan", not "Other"
  ref_A_label <- lm$label[match(refs[["A"]], lm$code)]
  expect_equal(
    ref_A_label, "Akan",
    label = "country A ref is modal non-catch-all 'Akan', not 'Other'"
  )
  
  # (b) country B reference is "Yoruba"
  ref_B_label <- lm$label[match(refs[["B"]], lm$code)]
  expect_equal(
    ref_B_label, "Yoruba",
    label = "country B ref is modal non-catch-all 'Yoruba'"
  )
  
  # (c) Igbo observations in country B are NA (below min_n = 10)
  b_rows      <- result$df[df$country == "B", ]
  igbo_code   <- lm$code[match("Igbo", lm$label)]
  igbo_in_B   <- as.character(b_rows$ethnicity)
  expect_false(
    any(!is.na(igbo_in_B) & igbo_in_B == igbo_code),
    label = "Igbo observations in country B are NA after min_n filtering"
  )
  expect_equal(
    sum(is.na(igbo_in_B)), 8L,
    label = "exactly 8 Igbo observations NA-ed in country B"
  )
  
  # (d) "Other" observations in country A are NOT NA-ed (count 25 >= min_n 10)
  a_rows       <- result$df[df$country == "A", ]
  other_code   <- lm$code[match("Other", lm$label)]
  other_in_A   <- as.character(a_rows$ethnicity)
  expect_equal(
    sum(!is.na(other_in_A) & other_in_A == other_code), 25L,
    label = "'Other' observations in country A retained (count >= min_n)"
  )
  
  # (e) Akan/Ewe counts in country A are fully intact
  akan_code  <- lm$code[match("Akan", lm$label)]
  expect_equal(
    sum(!is.na(other_in_A) & other_in_A == akan_code), 50L,
    label = "country A Akan count unaffected by country B min_n rule"
  )
})


# ── Test 3: decoding — .parse_terms() recovers correct labels ─────────────────
#
# Scenario: given a known level_map (constructed by hand, not from a model run),
# check that .parse_terms() translates internal codes back to human-readable
# labels correctly for every case the function handles:
#   (a) a regular coded slope term  -> correct variable + level label
#   (b) all three levels of the same variable decode to distinct correct labels
#   (c) "(Intercept)" is recognised as param_type = "intercept", not a slope
#   (d) an ordinal cutpoint term is recognised as param_type = "cutpoint"
#   (e) a term not in any level_map falls back to using the raw term as variable

test_that(".parse_terms decodes coded terms back to correct labels", {
  
  # Build a minimal meta object — exactly what tidy_model() would construct
  meta <- list(
    term_map  = NULL,
    level_map = list(
      dem_gen = tibble::tibble(
        code  = c("__01", "__02", "__03"),
        label = c("Man",  "Woman", "Other")
      ),
      dem_age = tibble::tibble(
        code  = c("__01", "__02", "__03"),
        label = c("18-24 years old", "25-34 years old", "35-44 years old")
      )
    ),
    dem_dict = NULL
  )
  
  # The terms as they appear in model coefficient names: varname + level code
  terms <- c(
    "dem_gen__02",         # Woman
    "dem_gen__01",         # Man  (the reference — would normally be absent but
    "dem_gen__03",         #       parse_terms still needs to handle it)
    "dem_age__02",         # 25-34 years old
    "(Intercept)",         # special: intercept
    "Intercept[2]",        # ordinal cutpoint
    "an_unknown_var"       # fallback: not in any level_map
  )
  
  result <- rbesd:::.parse_terms(terms, meta)
  
  # (a) first term: dem_gen__02 -> variable="dem_gen" (or label), level="Woman"
  expect_equal(
    result$level[[1]], "Woman",
    label = "dem_gen__02 decodes to level 'Woman'"
  )
  
  # (b) all three dem_gen levels decode correctly and distinctly
  gen_levels <- result$level[1:3]
  expect_equal(
    sort(gen_levels), sort(c("Woman", "Man", "Other")),
    label = "all three dem_gen levels decode to distinct correct labels"
  )
  
  # (c) "(Intercept)" has param_type = "intercept"
  expect_equal(
    result$param_type[[5]], "intercept",
    label = "'(Intercept)' recognised as intercept, not slope"
  )
  
  # (d) ordinal cutpoint "Intercept[2]" has param_type = "cutpoint"
  expect_equal(
    result$param_type[[6]], "cutpoint",
    label = "'Intercept[2]' recognised as cutpoint"
  )
  
  # (e) unknown variable falls back gracefully — variable is non-NA, no error
  expect_false(
    is.na(result$variable[[7]]),
    label = "unknown term falls back to non-NA variable name"
  )
  expect_true(
    nzchar(result$variable[[7]]),
    label = "unknown term fallback variable label is non-empty"
  )
})