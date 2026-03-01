# rbesd

<img src="man/figures/img_rbesd.jpeg" align="right" width="140"/>

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
