
# Helper functions for tests (new suite)
# These helpers create synthetic BeSD-like data using dictionary labels.

make_dummy_besd_df <- function(n_countries = 6, n_per_country = 80, seed = 123) {
  set.seed(seed)

  dict_besd <- rbesd::besd_dictionary()
  dict_dem  <- rbesd::dem_dictionary()

  # Pull label levels from dictionaries
  lv_peer <- dict_besd$levels[[match("so_peer", dict_besd$item_id)]]
  lv_safe <- dict_besd$levels[[match("tf_safety", dict_besd$item_id)]]

  lv_gen  <- dict_dem$levels[[match("dem_gen", dict_dem$item_id)]]
  lv_age  <- dict_dem$levels[[match("dem_age", dict_dem$item_id)]]

  countries <- paste0("C", seq_len(n_countries))
  country <- rep(countries, each = n_per_country)
  n <- length(country)

  dem_gen <- sample(lv_gen, n, replace = TRUE, prob = c(0.46, 0.52, 0.01, 0.01))
  dem_age <- sample(lv_age, n, replace = TRUE, prob = c(0.18, 0.28, 0.24, 0.18, 0.12))

  # Create a binary outcome with country + gender signal so regressions have variance
  # Logistic: intercept varies by country
  cc_idx <- as.integer(factor(country, levels = countries))
  country_re <- stats::rnorm(n_countries, 0, 0.6)[cc_idx]
  gen_eff <- ifelse(dem_gen == "Woman", 0.3, ifelse(dem_gen == "Man", -0.1, 0))
  age_eff <- ifelse(dem_age %in% c("35-44 years old", "45-54 years old", "55+ years"), 0.15, 0)

  eta <- -0.2 + country_re + gen_eff + age_eff
  p <- 1 / (1 + exp(-eta))
  so_peer <- ifelse(stats::runif(n) < p, "Yes", "No")

  # Ordinal outcome from a latent variable
  z <- 0.0 + country_re + 0.25 * (dem_gen == "Woman") + stats::rnorm(n)
  # cut into 4 ordered categories matching dictionary labels
  q <- stats::quantile(z, probs = c(0.25, 0.5, 0.75))
  tf_safety <- cut(
    z,
    breaks = c(-Inf, q[[1]], q[[2]], q[[3]], Inf),
    labels = lv_safe,
    ordered_result = TRUE
  )
  tf_safety <- as.character(tf_safety)

  data.frame(
    country = country,
    so_peer = so_peer,
    tf_safety = tf_safety,
    dem_gen = dem_gen,
    dem_age = dem_age,
    stringsAsFactors = FALSE
  )
}

make_dummy_multichoice_raw_df <- function(n = 120, seed = 456) {
  set.seed(seed)

  dict_besd <- rbesd::besd_dictionary()
  dict_dem  <- rbesd::dem_dictionary()

  lv_mc <- dict_besd$levels[[match("pr_reasons_ease_access", dict_besd$item_id)]]
  lv_gen <- dict_dem$levels[[match("dem_gen", dict_dem$item_id)]]

  # We'll use the first 3 multichoice options for compact tests
  opts <- lv_mc[1:3]
  not_prefix <- "Not "

  country <- sample(paste0("C", 1:4), n, replace = TRUE)
  dem_gen <- sample(lv_gen, n, replace = TRUE)

  # Raw columns: each column is either selected (option label) or "Not <option>"
  pick <- function(opt) ifelse(stats::runif(n) < 0.35, opt, paste0(not_prefix, opt))
  raw1 <- pick(opts[[1]])
  raw2 <- pick(opts[[2]])
  raw3 <- pick(opts[[3]])

  data.frame(
    country = country,
    dem_gen = dem_gen,
    raw_reason_1 = raw1,
    raw_reason_2 = raw2,
    raw_reason_3 = raw3,
    stringsAsFactors = FALSE
  )
}
