# Poststratification: per-outcome types and top-box collapsing.

# Fit once and reuse: the outcome list deliberately puts the ordinal item
# first, so a regression to reading the scalar `meta$y_type` would silently
# drop the binary outcome.
ps_fixture <- local({
  sim <- make_besd_sim(n_countries = 4, n_per_cell = 20)
  x <- suppressWarnings(as_besd(
    sim$survey, dem_dict = dem_dictionary(), country_col = "country",
    weight_col = "wt", stratum_col = "stratum", psu_col = "psu"
  ))
  fit <- suppressWarnings(suppressMessages(besd_regress(
    x, outcome = c("tf_safety", "sp_peer"),
    predictors = c("dem_gen", "dem_age"),
    scope = "by_country", engine = "frequentist"
  )))
  cells <- unique(sim$cells[, c("country", "dem_gen", "dem_age", "n_pop")])
  frame <- besd_poststrat_frame(cells, fit = fit, pop_col = "n_pop")
  list(x = x, frame = frame, fitted = besd_fitted_probs(fit, newdata = frame))
})

.toplevs_of <- function(item) {
  d <- besd_dictionary()
  d$toplevs[[match(item, d$item_id)]]
}

test_that("mixed binary and ordinal outcomes are both poststratified", {
  ps <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame)

  # meta$y_type is only the first outcome's type; both must still appear.
  expect_equal(ps_fixture$fitted$meta$y_type, "ordinal")
  expect_setequal(unique(ps$item_id), c("tf_safety", "sp_peer"))
  expect_equal(sort(unique(ps$item_type[ps$item_id == "sp_peer"])), "binary")
  expect_true(all(ps$pct >= 0 & ps$pct <= 100))
})

test_that("combine_top collapses ordinal items to a single top-box row", {
  ps    <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame)
  ps_tb <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                             combine_top = TRUE)

  tl  <- .toplevs_of("tf_safety")
  ord <- ps_tb[ps_tb$item_id == "tf_safety", ]

  expect_equal(nrow(ord), length(unique(ps_tb$country)))
  expect_equal(unique(ord$response), paste(tl, collapse = ", "))

  # The collapsed estimate is the sum of its member categories. (Exact here
  # because the frequentist engine carries a single draw; for Bayesian models
  # the point estimate is a median of summed draws, tested separately.)
  member <- ps[ps$item_id == "tf_safety" & ps$response %in% tl, ]
  manual <- tapply(member$pct, member$country, sum)
  expect_equal(ord$pct[order(ord$country)],
               as.numeric(manual[order(names(manual))]),
               tolerance = 1e-10)
})

test_that("combine_top leaves binary outcomes unchanged", {
  ps    <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame)
  ps_tb <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                             combine_top = TRUE)

  a <- ps[ps$item_id == "sp_peer", c("country", "response", "pct")]
  b <- ps_tb[ps_tb$item_id == "sp_peer", c("country", "response", "pct")]
  expect_equal(as.data.frame(a), as.data.frame(b))
})

test_that("combine_top labels and columns match summary(combine_top = TRUE)", {
  ps_tb <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                             combine_top = TRUE)
  s <- suppressMessages(
    summary(ps_fixture$x, combine_top = TRUE, ci = FALSE,
            include_demographics = FALSE)
  )

  expect_setequal(unique(ps_tb$response), unique(s$response))
  expect_true(all(names(s) %in% names(ps_tb)))
  expect_true(all(ps_tb$estimator == "mrp"))
  expect_true(inherits(ps_tb, "besd_summary_tbl"))
})

test_that("combine_top respects by and overall grouping", {
  ps_tb <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                             by = "dem_gen", overall = TRUE,
                             combine_top = TRUE)

  expect_true("dem_gen" %in% names(ps_tb))
  expect_setequal(unique(ps_tb$dem_gen), c("Man", "Woman"))
  # One overall row per item x by-group.
  expect_equal(sum(ps_tb$country == "Overall"),
               length(unique(ps_tb$item_id)) * 2L)
})

test_that("combine_top returns collapsed posterior draws", {
  out <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                           combine_top = TRUE, post_probs = TRUE)
  tl <- paste(.toplevs_of("tf_safety"), collapse = ", ")

  expect_named(out, c("estimates", "post_probs"))
  expect_true(tl %in% out$post_probs$response)
  expect_equal(nrow(out$post_probs), nrow(out$estimates))
})

test_that(".ps_collapse_cats sums categories within each draw", {
  set.seed(42)
  n_draws <- 200; n_cells <- 5; n_k <- 4
  arr <- array(runif(n_draws * n_cells * n_k), c(n_draws, n_cells, n_k))
  idx <- seq_len(n_cells)

  # Summing within a draw, not across summarised categories.
  expect_equal(.ps_collapse_cats(arr, idx, c(3L, 4L)),
               arr[, idx, 3] + arr[, idx, 4])

  # A single category is the plain slice, as before.
  expect_equal(.ps_collapse_cats(arr, idx, 2L),
               matrix(as.numeric(arr[, idx, 2, drop = FALSE]), nrow = n_draws))

  # Interval comes from the distribution of the sum, not the sum of intervals.
  w   <- c(.1, .2, .3, .25, .15)
  got <- .ps_summarise_draws(
    .ps_weighted_draws(.ps_collapse_cats(arr, idx, c(3L, 4L)), w), .95, "bayes"
  )
  ref <- as.numeric((arr[, idx, 3] + arr[, idx, 4]) %*% w)
  expect_equal(got$estimate, stats::median(ref))
  expect_equal(got$lower, unname(stats::quantile(ref, .025)))
  expect_equal(got$upper, unname(stats::quantile(ref, .975)))
})

test_that("combine_top requires toplevs for non-binary outcomes", {
  im <- list(a = list(item_id = "a", toplevs = c("Yes")),
             b = list(item_id = "b", toplevs = NULL))

  expect_silent(.ps_assert_toplevs("a", list(a = "ordinal"), im))
  # Binary outcomes are exempt: they already carry the top-box label.
  expect_silent(.ps_assert_toplevs("b", list(b = "binary"), im))
  expect_error(.ps_assert_toplevs("b", list(b = "ordinal"), im),
               "requires non-empty `toplevs`")
})

test_that(".ps_top_indices falls back to positional category labels", {
  im <- list(toplevs = c("Moderately safe", "Very safe"),
             levels  = c("Not at all safe", "A little safe",
                         "Moderately safe", "Very safe"))

  expect_equal(.ps_top_indices(im$levels, im, "tf_safety"), c(3L, 4L))
  # Engines that return positional labels rather than the factor levels.
  expect_equal(.ps_top_indices(c("1", "2", "3", "4"), im, "tf_safety"), c(3L, 4L))
  expect_error(.ps_top_indices(c("x", "y"), im, "tf_safety"), "could not match")
})
