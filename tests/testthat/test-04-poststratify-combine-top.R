# Poststratification: Bayesian-only guard, per-outcome types, top-box collapsing.
#
# The fitted object is synthesised rather than sampled: MrP is Bayesian-only,
# and a real brms fit would make this file slow and dependent on a Stan
# toolchain. A hand-built posterior also lets the interval assertions compare
# against an exact reference computed from the same draws.

.mk_ps_fixture <- function(n_draws = 400L, seed = 99L) {
  set.seed(seed)
  sim <- make_besd_sim(n_countries = 4, n_per_cell = 20)
  x <- suppressWarnings(as_besd(
    sim$survey, dem_dict = dem_dictionary(), country_col = "country",
    weight_col = "wt", stratum_col = "stratum", psu_col = "psu"
  ))
  # A frequentist fit is fine here: it is only used to encode the frame.
  fit <- suppressWarnings(suppressMessages(besd_regress(
    x, outcome = c("tf_safety", "sp_peer"),
    predictors = c("dem_gen", "dem_age"),
    scope = "by_country", engine = "frequentist"
  )))
  cells <- unique(sim$cells[, c("country", "dem_gen", "dem_age", "n_pop")])
  frame <- besd_poststrat_frame(cells, fit = fit, pop_col = "n_pop")

  n_cells <- nrow(frame)
  cats    <- besd_dictionary()$levels[[
    match("tf_safety", besd_dictionary()$item_id)]]

  # Ordinal: rows normalised to a simplex across categories, as posterior_epred
  # would return. Binary: a plain [n_draws x n_cells] probability matrix.
  raw <- array(stats::rexp(n_draws * n_cells * length(cats)),
               c(n_draws, n_cells, length(cats)))
  ord <- raw / array(apply(raw, c(1, 2), sum),
                     c(n_draws, n_cells, length(cats)))
  bin <- matrix(stats::runif(n_draws * n_cells), nrow = n_draws)

  fitted <- structure(
    list(
      draws = list(tf_safety = ord, sp_peer = bin),
      meta  = list(
        engine     = "bayes",
        scope      = "by_country",
        outcomes   = c("tf_safety", "sp_peer"),
        y_type     = "ordinal",   # first outcome only, as besd_fitted_probs sets it
        y_types    = list(tf_safety = "ordinal", sp_peer = "binary"),
        n_sample   = n_draws,
        categories = list(tf_safety = cats, sp_peer = NULL),
        besd_dict  = fit$prep$dict,
        dem_dict   = fit$prep$dem_dict,
        outcome_label_map  = list(),
        outcome_parent_map = list()
      ),
      row_ids = tibble::tibble(country = as.character(frame$country),
                               .row_id = seq_len(n_cells))
    ),
    class = "besd_fitted"
  )

  list(x = x, fit = fit, frame = frame, fitted = fitted,
       ord = ord, cats = cats, n_draws = n_draws)
}

ps_fixture <- .mk_ps_fixture()

.toplevs_of <- function(item) {
  d <- besd_dictionary()
  d$toplevs[[match(item, d$item_id)]]
}

# ── Bayesian-only guard ────────────────────────────────────────────────────────

test_that("poststratifying a frequentist fit is refused", {
  freq <- ps_fixture$fitted
  freq$meta$engine <- "frequentist"

  expect_error(besd_poststratify(freq, ps_fixture$frame),
               "requires a Bayesian model fit")
  expect_error(besd_poststratify(freq, ps_fixture$frame), "engine = \"bayes\"")
})

test_that("predicting onto a poststrat frame is refused for frequentist fits", {
  expect_error(
    besd_fitted_probs(ps_fixture$fit, newdata = ps_fixture$frame),
    "requires a Bayesian model fit"
  )
})

test_that("newdata is required: there is no training-data prediction path", {
  expect_error(besd_fitted_probs(ps_fixture$fit), "`newdata` is required")
  expect_error(besd_fitted_probs(ps_fixture$fit), "MrP pipeline")
})

test_that("newdata must be a poststrat frame", {
  expect_error(besd_fitted_probs(ps_fixture$fit, newdata = data.frame(a = 1)),
               "besd_poststrat_frame")
})

# ── Per-outcome types ──────────────────────────────────────────────────────────

test_that("mixed binary and ordinal outcomes are both poststratified", {
  ps <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame)

  # meta$y_type is only the first outcome's type; both must still appear.
  expect_equal(ps_fixture$fitted$meta$y_type, "ordinal")
  expect_setequal(unique(ps$item_id), c("tf_safety", "sp_peer"))
  expect_equal(unique(ps$item_type[ps$item_id == "sp_peer"]), "binary")
  expect_true(all(ps$pct >= 0 & ps$pct <= 100))
  expect_true(all(is.finite(ps$lcl)) && all(is.finite(ps$ucl)))
})

# ── combine_top ────────────────────────────────────────────────────────────────

test_that("combine_top collapses ordinal items to a single top-box row", {
  ps_tb <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                             combine_top = TRUE)
  tl  <- .toplevs_of("tf_safety")
  ord <- ps_tb[ps_tb$item_id == "tf_safety", ]

  expect_equal(nrow(ord), length(unique(ps_tb$country)))
  expect_equal(unique(ord$response), paste(tl, collapse = ", "))
})

test_that("combine_top estimates match an exact reference on the same draws", {
  ps_tb <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                             combine_top = TRUE)

  frame <- ps_fixture$frame
  tl    <- .toplevs_of("tf_safety")
  k_top <- match(tl, ps_fixture$cats)
  cty   <- as.character(frame$country)[[1]]
  idx   <- which(as.character(frame$country) == cty)

  w   <- frame$n_pop[idx]; w <- w / sum(w)
  ref <- as.numeric(apply(ps_fixture$ord[, idx, k_top, drop = FALSE], c(1, 2), sum) %*% w)

  got <- ps_tb[ps_tb$item_id == "tf_safety" & ps_tb$country == cty, ]
  expect_equal(got$pct, 100 * stats::median(ref))
  expect_equal(got$lcl, 100 * unname(stats::quantile(ref, 0.025)))
  expect_equal(got$ucl, 100 * unname(stats::quantile(ref, 0.975)))

  # The interval must describe the summed quantity, not the sum of the
  # per-category intervals, which is materially wider.
  per_cat <- lapply(k_top, function(k) {
    d <- as.numeric(ps_fixture$ord[, idx, k] %*% w)
    unname(stats::quantile(d, c(0.025, 0.975)))
  })
  naive_width <- sum(vapply(per_cat, diff, numeric(1)))
  expect_lt(got$ucl - got$lcl, 100 * naive_width)
})

test_that("combine_top leaves binary outcomes unchanged", {
  ps    <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame)
  ps_tb <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                             combine_top = TRUE)

  a <- ps[ps$item_id == "sp_peer", c("country", "response", "pct", "lcl", "ucl")]
  b <- ps_tb[ps_tb$item_id == "sp_peer", c("country", "response", "pct", "lcl", "ucl")]
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
  expect_s3_class(ps_tb, "besd_summary_tbl")
})

test_that("combine_top respects by and overall grouping", {
  ps_tb <- besd_poststratify(ps_fixture$fitted, ps_fixture$frame,
                             by = "dem_gen", overall = TRUE, combine_top = TRUE)

  expect_true("dem_gen" %in% names(ps_tb))
  expect_setequal(unique(ps_tb$dem_gen), c("Man", "Woman"))
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
  expect_length(out$post_probs$draws[[1]], ps_fixture$n_draws)
  # Draws stay on the probability scale.
  expect_true(all(out$post_probs$draws[[1]] >= 0 & out$post_probs$draws[[1]] <= 1))
})

# ── Internal helpers ───────────────────────────────────────────────────────────

test_that(".ps_collapse_cats sums categories within each draw", {
  set.seed(42)
  n_draws <- 200; n_cells <- 5; n_k <- 4
  arr <- array(stats::runif(n_draws * n_cells * n_k), c(n_draws, n_cells, n_k))
  idx <- seq_len(n_cells)

  expect_equal(.ps_collapse_cats(arr, idx, c(3L, 4L)),
               arr[, idx, 3] + arr[, idx, 4])
  expect_equal(.ps_collapse_cats(arr, idx, 2L),
               matrix(as.numeric(arr[, idx, 2, drop = FALSE]), nrow = n_draws))

  w   <- c(.1, .2, .3, .25, .15)
  got <- .ps_summarise_draws(
    .ps_weighted_draws(.ps_collapse_cats(arr, idx, c(3L, 4L)), w), .95
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
  expect_silent(.ps_assert_toplevs("b", list(b = "binary"), im))
  expect_error(.ps_assert_toplevs("b", list(b = "ordinal"), im),
               "requires non-empty `toplevs`")
})

test_that(".ps_top_indices falls back to positional category labels", {
  im <- list(toplevs = c("Moderately safe", "Very safe"),
             levels  = c("Not at all safe", "A little safe",
                         "Moderately safe", "Very safe"))

  expect_equal(.ps_top_indices(im$levels, im, "tf_safety"), c(3L, 4L))
  expect_equal(.ps_top_indices(c("1", "2", "3", "4"), im, "tf_safety"), c(3L, 4L))
  expect_error(.ps_top_indices(c("x", "y"), im, "tf_safety"), "could not match")
})
