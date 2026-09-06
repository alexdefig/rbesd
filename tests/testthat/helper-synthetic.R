# Synthetic BeSD data with known parameters.
#
# One generator feeds the whole suite. Levels are pulled from the shipped
# dictionaries so simulated data can never drift from besd_dictionary().

.sim_levels <- function() {
  besd <- rbesd::besd_dictionary()
  dem  <- rbesd::dem_dictionary()
  list(
    gen  = dem$levels[[match("dem_gen",   dem$item_id)]],
    age  = dem$levels[[match("dem_age",   dem$item_id)]],
    peer = besd$levels[[match("so_peer",   besd$item_id)]],
    safe = besd$levels[[match("tf_safety", besd$item_id)]]
  )
}

# Simulate a multilevel BeSD survey plus the census frame it was drawn from.
#
# The binary outcome `so_peer` follows a known logistic model with country
# random intercepts. Women are deliberately oversampled, so the raw survey
# estimate is biased away from the population truth and MrP has something to
# correct.
#
# Returns:
#   survey - respondent-level data.frame, ready for as_besd()
#   cells  - census frame: one row per country x gender x age, with n_pop and
#            the true cell probability p
#   truth  - population-weighted true prevalence of "Yes" per country (0-100)
#   params - the planted parameters, for recovery assertions
make_besd_sim <- function(n_countries = 12,
                          n_per_cell  = 30,
                          seed        = 2026,
                          b0          = -0.3,
                          sd_country  = 0.5,
                          b_gen       = 0.7,
                          b_age       = c(0, 0.2, 0.4, 0.6, 0.8),
                          oversample  = c(Man = 1, Woman = 3),
                          pop_range   = c(200L, 5000L)) {
  set.seed(seed)
  lv     <- .sim_levels()
  gen_lv <- lv$gen[1:2]          # Man, Woman
  age_lv <- lv$age               # 5 age bands
  names(b_age) <- age_lv

  countries <- sprintf("C%02d", seq_len(n_countries))
  u <- stats::setNames(stats::rnorm(n_countries, 0, sd_country), countries)

  # Census frame with known cell probabilities
  cells <- expand.grid(country = countries, dem_gen = gen_lv, dem_age = age_lv,
                       KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  cells$p <- stats::plogis(
    b0 + u[cells$country] + b_gen * (cells$dem_gen == "Woman") + b_age[cells$dem_age]
  )
  cells$n_pop <- sample(seq.int(pop_range[1], pop_range[2]), nrow(cells), replace = TRUE)

  # Population truth: the quantity MrP should recover
  num   <- tapply(cells$p * cells$n_pop, cells$country, sum)
  den   <- tapply(cells$n_pop,           cells$country, sum)
  truth <- data.frame(country  = names(num),
                      true_pct = 100 * as.numeric(num / den),
                      row.names = NULL, stringsAsFactors = FALSE)

  # Draw the (deliberately non-proportional) sample
  n_samp <- n_per_cell * unname(oversample[cells$dem_gen])
  idx    <- rep(seq_len(nrow(cells)), n_samp)
  n      <- length(idx)

  survey <- data.frame(
    country = cells$country[idx],
    stratum = paste0(cells$country[idx], "-", cells$dem_age[idx]),
    psu     = paste0(cells$country[idx], "-", cells$dem_gen[idx]),
    wt      = 1 / unname(oversample[cells$dem_gen[idx]]),
    rid     = seq_len(n),
    so_peer = ifelse(stats::runif(n) < cells$p[idx], "Yes", "No"),
    dem_gen = cells$dem_gen[idx],
    dem_age = cells$dem_age[idx],
    stringsAsFactors = FALSE
  )

  # Ordinal item sharing the same country effects, for summary()-side tests
  z <- u[survey$country] + b_gen * (survey$dem_gen == "Woman") + stats::rnorm(n)
  survey$tf_safety <- as.character(
    cut(z, breaks = c(-Inf, -0.5, 0.3, 1.1, Inf), labels = lv$safe)
  )

  list(
    survey = survey,
    cells  = cells,
    truth  = truth,
    params = list(b0 = b0, sd_country = sd_country, b_gen = b_gen, b_age = b_age,
                  u = u, countries = countries, gen_levels = gen_lv,
                  age_levels = age_lv, levels = lv)
  )
}

# Raw multichoice columns in "text_prefix" encoding: each column holds either
# the option label (selected) or "Not <option>".
make_multichoice_raw <- function(n = 120, seed = 456) {
  set.seed(seed)
  besd <- rbesd::besd_dictionary()
  opts <- besd$levels[[match("pr_reasons_ease_access", besd$item_id)]][1:3]

  pick <- function(opt) ifelse(stats::runif(n) < 0.35, opt, paste0("Not ", opt))
  data.frame(
    country      = sample(sprintf("C%02d", 1:4), n, replace = TRUE),
    raw_reason_1 = pick(opts[[1]]),
    raw_reason_2 = pick(opts[[2]]),
    raw_reason_3 = pick(opts[[3]]),
    stringsAsFactors = FALSE
  )
}
