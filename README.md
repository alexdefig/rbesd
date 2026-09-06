# rbesd

<img src="man/figures/img_rbesd.jpg" align="right" width="140"/>

`rbesd` is an R package providing a standardized workflow for WHO/UNICEF BeSD quantitative toolkit data:

- **Preparation & validation**: harmonise raw variables to a standard item dictionary, validate codings, and store metadata.
- **Visualisation**: item bars/stacked bars, radar profiles, and cross-country comparisons.
- **Regression modelling**: wrappers for binary/ordinal outcomes; frequentist/Bayesian; single-level/multilevel; optional survey weights.
- **Scoring**: compute domain/subdomain scores on a 0–1 scale.

---

[Installation](#installation) · [Data Preparation](#data-preparation) · [Data Quality](#data-quality) · [Regression](#regression) · [Poststratification (MrP)](#poststratification-mrp) · [BeSD Explorer](#besd-explorer-interactive-shiny-app)

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("alexdefig/rbesd")

# Contributors: install from a local clone
# devtools::install_local("path/to/rbesd")
```

---

## Data Preparation

The core object is a `besd_data` created by `as_besd()`. It stores your survey data alongside a validated item dictionary and optional demographic metadata.

### 1. Load the bundled demo dataset

```r
library(rbesd)

data("data_demo", package = "rbesd")
```

`data_demo` is a 3,000-respondent slice of IMMS 2025 data across five countries (Brazil, Mexico, Senegal, Philippines, The UK), already mapped to BeSD item names.

### 2. Build a demographic dictionary

`dem_dictionary()` covers age and gender. Add custom variables (e.g. ethnicity) with `modify_dictionary()`:

```r
dem_eth_df <- tibble::tibble(
  item_id        = "dem_eth",
  domain         = "predictor",
  item_type      = "categorical",
  levels         = list(sort(unique(data_demo$dem_eth))),
  reverse        = NA,
  question       = "What is your ethnicity?",
  question_short = "Ethnicity"
)

dem_dict <- modify_dictionary(
  df      = dem_eth_df,
  dict    = dem_dictionary(),
  replace = FALSE
)
```

### 3. Create the `besd_data` object

Provide a named mapping from your raw column names to BeSD item IDs. Declare any free-text missing tokens so they are recoded to `NA`:

```r
mapping <- c(
  B1  = "tf_benefits",
  B2  = "tf_safety",
  B4  = "tf_hcws",
  B5  = "so_peer",
  B6  = "so_family",
  B7  = "so_religious",
  B3  = "so_hcw_rec",
  B9  = "pr_recall",
  B10 = "so_travel_autonomy",
  B11 = "pr_know_where",
  # multichoice: many source columns → one item ID
  setNames(rep("pr_reasons_ease_access", 12), paste0("A_B16_", 1:12)),
  S3_RECODE = "dem_age",
  S2        = "dem_gen",
  S7        = "dem_eth"
)

y <- as_besd(
  df             = data_demo,
  country_col    = "country",
  dem_dict       = dem_dict,
  mapping        = mapping,
  missing_tokens = list(.all = c("Prefer not to say", "Don't Know")),
  missing_action = "na"
)
```

`missing_tokens` accepts `.all` (applies to every item) or per-item overrides. `missing_action = "na"` recodes matched tokens to `NA`; `"keep"` retains them as a response level.

Multichoice items (multiple source columns that share a single item ID) are detected automatically. For items with text-prefix encoding, pass `multichoice_specs`:

```r
y <- as_besd(
  ...,
  multichoice_specs = list(
    pr_reasons_ease_access = list(encoding = "text_prefix", not_prefix = "Not ")
  )
)
```

---

## Data Quality

### Inspect the object

```r
print(y)
# besd_data: 3000 rows · 5 countries · 17 BeSD items · 3 demographic variables
```

### Missing data summary

```r
besd_missing_summary(y)
```

Reports per-item missingness and the joint listwise fraction (how many rows complete-case deletion will remove).

### Response distributions

```r
smry <- summary(y)
print(smry, n = 20)

# Demographic composition
besd_summary_demographics(smry)
```

### Check for rare factor levels

Thin cells within a country can cause near-separation in regression models. Inspect before fitting:

```r
rl <- besd_rare_levels(y, predictors = c("dem_eth", "dem_gen", "dem_age"))
print(rl, n = Inf)
```

### Recode rare or unwanted levels

```r
# Recode a level to NA (and drop it from the factor)
y <- besd_recode_missing(
  y,
  tokens      = list(dem_gen = "Other"),
  drop_levels = TRUE
)

# Merge sparse ethnicity categories into "Other"
y <- besd_recode_levels(
  y,
  col    = "dem_eth",
  recode = c(
    "Oriental"   = "Other",
    "Indigenous" = "Other"
  )
)
```

---

## Regression

All regression is handled by `besd_regress()`. Choose `scope` (by-country or multilevel) and `engine` (frequentist or Bayesian). Results are extracted with `tidy_model()`.

### By-country, frequentist (binary outcome)

Fits a separate logistic regression per country:

```r
fit_bc <- besd_regress(
  y,
  outcome    = "so_peer",
  predictors = c("dem_gen", "dem_age"),
  scope      = "by_country",
  engine     = "frequentist"
)

# Tidy fixed effects as odds ratios
tidy_model(fit_bc, exponentiate = TRUE)

# Include baseline reference rows
tidy_model(fit_bc, return_baseline = TRUE)
```

### Multilevel, frequentist (binary outcome)

A single model with country as the random-effect grouping variable. `predictors` can be split into `common` (pooled across countries) and `context` (country-specific dummies, dropped when cell count < `min_n_context`):

```r
fit_ml <- besd_regress(
  y,
  outcome       = "so_peer",
  predictors    = list(common = c("dem_gen", "dem_age"), context = "dem_eth"),
  scope         = "multilevel",
  engine        = "frequentist",
  min_n_context = 10L
)

# Fixed effects + random SD parameters + country BLUPs
tidy_model(fit_ml, include_random = TRUE)
```

### Ordinal outcome

`besd_regress()` detects the item type from the dictionary and uses a cumulative logit model automatically:

```r
fit_ord <- besd_regress(
  y,
  outcome    = "tf_benefits",
  predictors = c("dem_gen", "dem_age"),
  scope      = "by_country",
  engine     = "frequentist"
)

tidy_model(fit_ord)
```

### Multichoice outcome

Multichoice items are split into one binary column per response option; `besd_regress()` fits a separate logistic model for each. The `level` column in `tidy_model()` output contains the response label:

```r
fit_mc <- besd_regress(
  y,
  outcome    = "pr_reasons_ease_access",
  predictors = c("dem_gen", "dem_age"),
  scope      = "by_country",
  engine     = "frequentist"
)

tidy_model(fit_mc)
```

### Bayesian multilevel

Requires `brms` and a configured Stan toolchain. First-time model compilation takes ~1–2 minutes.

```r
fit_bayes <- besd_regress(
  y,
  outcome       = "so_peer",
  predictors    = list(common = c("dem_gen", "dem_age"), context = "dem_eth"),
  scope         = "multilevel",
  engine        = "bayes",
  chains        = 2,
  iter          = 2000,
  cores         = 2,
  control       = list(adapt_delta = 0.95)
)

td <- tidy_model(fit_bayes)

# Check convergence: Rhat < 1.01 and ESS > 400
td[, c("variable", "level", "country", "rhat", "ess")]
```

**Random slopes** allow the effect of common predictors to vary by country. `correlated_re = FALSE` fits independent variances (more stable with few countries):

```r
fit_rs <- besd_regress(
  y,
  outcome       = "so_peer",
  predictors    = list(common = c("dem_gen", "dem_age"), context = "dem_eth"),
  scope         = "multilevel",
  engine        = "bayes",
  random_slopes = TRUE,
  correlated_re = FALSE,
  chains        = 2,
  iter          = 2000,
  cores         = 2,
  control       = list(adapt_delta = 0.99)
)
```

For large models, the `cmdstanr` backend is faster and more memory-efficient than `rstan`:

```r
fit_cmd <- besd_regress(
  ...,
  backend = "cmdstanr",
  threads = brms::threading(4)
)
```

### Looping over multiple outcomes

```r
outcomes <- c("so_peer", "so_family", "pr_recall")

fits <- lapply(outcomes, function(yy) {
  besd_regress(
    y,
    outcome    = yy,
    predictors = list(common = c("dem_gen", "dem_age"), context = "dem_eth"),
    scope      = "multilevel",
    engine     = "frequentist"
  )
})
names(fits) <- outcomes

results <- dplyr::bind_rows(lapply(fits, tidy_model))
```

---

## Poststratification (MrP)

Multilevel regression with poststratification (MrP) reweights model predictions to match a population census frame, correcting for non-representative samples.

### 1. Load the poststratification frame

`ps_demo` is a bundled census-style data frame of population cell counts for each country × gender × age combination, designed to pair with `data_demo`:

```r
data("ps_demo", package = "rbesd")
head(ps_demo)
# country   dem_gen  dem_age    n_pop
# Brazil    Man      18-34     145600
# ...
```

### 2. Fit a Bayesian multilevel model

MrP requires a model that generates cell-level predicted probabilities. Fit a Bayesian multilevel model as described in the [Regression](#regression) section. Note: context-specific predictors are not supported for MrP — use `common` predictors only:

```r
fit <- besd_regress(
  y,
  outcome       = "so_peer",
  predictors    = list(common = c("dem_gen", "dem_age")),
  scope         = "multilevel",
  engine        = "bayes",
  random_slopes = TRUE,
  correlated_re = FALSE,
  backend       = "cmdstanr",
  chains        = 2,
  iter          = 1000,
  cores         = 2,
  threads       = brms::threading(4),
  control       = list(adapt_delta = 0.99)
)
```

### 3. Build the poststrat frame and get fitted probabilities

```r
frame  <- besd_poststrat_frame(ps_demo, fit = fit, pop_col = "n_pop")
fitted <- besd_fitted_probs(fit, newdata = frame)
```

### 4. Poststratify

Population-weighted national estimates (one row per country):

```r
mrp_national <- besd_poststratify(fitted, frame)
```

Estimates broken down by demographic subgroup:

```r
mrp_by_dem <- besd_poststratify(fitted, frame, by = c("dem_gen", "dem_age"))
```

Add an overall (pooled across countries) estimate with `overall = TRUE`:

```r
mrp_overall <- besd_poststratify(fitted, frame, overall = TRUE)
```

For Bayesian models, retrieve the full posterior draw distribution per cell:

```r
out <- besd_poststratify(fitted, frame, post_probs = TRUE)
out$estimates   # median + 95% credible interval
out$post_probs  # list-column of posterior draws per group
```

---

## BeSD Explorer (interactive Shiny app)

The package includes an interactive dashboard for exploring BeSD results across countries, demographic breakdowns, and item profiles.

### Try it immediately with demo data

```r
library(rbesd)
launch_explorer()  # launches with the bundled demo dataset
```

### Use your own data

Prepare your data once using `prepare_explorer_data()`, then launch as normal.
Your data is saved outside the package so it survives package updates.

```r
library(rbesd)

# Step 1: create a besd object from your survey data
dat <- as_besd(
  df          = your_dataframe,
  country_col = "your_country_column",
  dem_dict    = dem_dictionary()   # omit if you have no demographic variables
)

# Step 2: compute and save the Explorer data files
prepare_explorer_data(dat)

# Step 3: launch
launch_explorer()
```

`prepare_explorer_data()` works through four steps and prints progress as it goes:

| Step | File | Required? | Powers |
|------|------|-----------|--------|
| 1 | `besd_sum.rds` | Yes | All tabs |
| 2 | `demo_sum.rds` | Yes | Country Profile — sample composition |
| 3 | `topbox_all.rds` | Yes | Map, ranked items |
| 4 | `breakdown_sum.rds` | No | BeSD by Demographic tab |

Step 4 (demographic breakdowns) is skipped automatically if your `besd_data`
object has no demographic variables. The tab will simply be empty in that case.

If you have multiple datasets or want to store files in a specific location:

```r
prepare_explorer_data(dat, data_dir = "~/my_project/besd_data")
launch_explorer(data_dir = "~/my_project/besd_data")
```

### What the Explorer shows

- **Global Overview** — world map and country rankings for any BeSD item
- **Country Profile** — response distributions, radar profile, and sample composition for a selected country
- **BeSD by Demographic** — response breakdowns by demographic subgroup across one or more countries
