# BeSD Explorer — Shiny App

Interactive dashboard for exploring WHO/UNICEF BeSD survey results.

## Running locally

```r
# Install required packages (first time only)
install.packages(c("shiny", "bslib", "leaflet", "reactable",
                   "rnaturalearth", "rnaturalearthdata", "sf"))

# Load the package and launch
devtools::load_all()
launch_explorer()
```

## Preparing the data

Before running the app you must save your pre-computed summary objects
into `data/`.  Run the prep script once (and whenever your data changes):

```r
source("dev/prep_explorer_data.R")
```

This produces:
- `data/besd_sum.rds`  — output of `summary(besd_data_object)`
- `data/demo_sum.rds`  — output of `besd_summary_demographics(besd_sum)`

## Deploying to a Shiny server (handoff to IT)

### What to hand over

A zip file or git repository containing this entire folder
(`inst/shiny/explorer/`) plus an `renv.lock` file.

### Server setup (IT instructions)

1. Install R (≥ 4.1) and [Shiny Server](https://posit.co/products/open-source/shinyserver/)
   or Posit Connect.
2. Clone / unzip the app folder into `/srv/shiny-server/besd-explorer/`.
3. Install R dependencies:
   ```r
   install.packages("renv")
   renv::restore()   # uses renv.lock to reproduce exact package versions
   ```
4. The app will be live at `https://your-server.org/besd-explorer/`.
5. Embed on a webpage with a single line of HTML:
   ```html
   <iframe src="https://your-server.org/besd-explorer/"
           width="100%" height="900px" frameborder="0"></iframe>
   ```

### Generating the renv.lock

Run this once on your development machine before handing over:

```r
renv::init()
renv::snapshot()
```

Commit the resulting `renv.lock` alongside the app.

## Updating the data

When you collect new data:
1. Re-run `dev/prep_explorer_data.R`
2. Re-deploy (or ask IT to restart the Shiny app process)
