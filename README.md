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
