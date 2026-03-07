# rbesd

<img src="man/figures/img_rbesd.jpg" align="right" width="140"/>

`rbesd` is an R package providing a standardized workflow for WHO/UNICEF BeSD quantitative toolkit data:

- **Preparation & validation**: harmonise raw variables to a standard item dictionary, validate codings, and store metadata.
- **Visualisation**: item bars/stacked bars, radar profiles, and cross-country comparisons.
- **Regression modelling**: wrappers for binary/ordinal outcomes; frequentist/Bayesian; single-level/multilevel; optional survey weights.
- **Scoring**: compute domain/subdomain scores on a 0–1 scale.

## Installation (development)

```r
# install.packages("devtools")
devtools::install_local("path/to/rbesd")
```

## Quick start

```r
library(rbesd)

# df_raw: your survey dataset
obj <- as_besd(
  df_raw,
  country_col = "country",
  weight_col  = "weight",
  mapping     = c(
    "Q1_trust_vax" = "conf_trust",
    "Q2_safety"    = "conf_safety"
  )
)

besd_validate(obj)
besd_plot_radar(obj)
fit <- besd_regress(obj, outcomes = c("conf_trust","conf_safety"), predictors = c("age","sex"))
besd_tidy(fit)
```

See `vignettes/` for a more complete workflow.

## BeSD Explorer (interactive Shiny app)

The package includes an interactive dashboard for exploring BeSD results across countries, demographic breakdowns, and item profiles.

### Try it with the built-in demo data

```r
library(rbesd)

# Run the prep script to generate the app data files using the bundled demo dataset
source(system.file("shiny/explorer/prep_data.R", package = "rbesd"))

# The script saves the data files and calls launch_explorer() automatically
```

### Use your own data

Open `prep_data.R` (found in `inst/shiny/explorer/`) and replace the demo data block with your own `as_besd()` call. The script walks through:

1. Creating a `besd` object with `as_besd()`
2. Computing `besd_sum` via `summary()`
3. Computing demographic breakdowns via `besd_summary_by()` (optional — needed for the "BeSD by Demographic" tab)
4. Saving the data files and launching the app

```r
# After editing prep_data.R with your own data:
source(system.file("shiny/explorer/prep_data.R", package = "rbesd"))
```

### What the Explorer shows

- **Global Overview** — world map and country rankings for any BeSD item
- **Country Profile** — response distributions, radar profile, and sample composition for a selected country
- **BeSD by Demographic** — response breakdowns by demographic subgroup across one or more countries
