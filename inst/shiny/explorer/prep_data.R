# prep_data.R
#
# Template script: prepare the data files required by the BeSD Explorer Shiny app.
#
# Run this script once to generate the three .rds files the Explorer needs,
# then call launch_explorer() to open the app in your browser.
#
# The example below uses `data_demo`, the small non-representative dataset
# bundled with rbesd, so you can try the Explorer immediately without your own
# data. Replace Section 1 with your own data loading and preparation steps.
# ─────────────────────────────────────────────────────────────────────────────

library(rbesd)

# ── 1. Create a besd object ───────────────────────────────────────────────────

# --- Example: built-in demo data ---------------------------------------------
data("data_demo", package = "rbesd")

dat_besd <- as_besd(
  df          = data_demo,
  country_col = "ADM1",
  dem_dict    = dem_dictionary()
)

# --- Your own data (replace the block above with something like this) ---------
# dat_besd <- as_besd(
#   df          = your_dataframe,
#   country_col = "your_country_column",
#   dem_dict    = your_dem_dict,    # optional; omit if no demographic variables
#   mapping     = your_mapping,     # optional; named vector mapping raw col → item_id
#   missing_tokens = list(.all = c("Don't know", "Prefer not to say")),
#   missing_action = "na"
# )

# ── 2. Compute summaries ──────────────────────────────────────────────────────
besd_sum <- summary(dat_besd, conf_level = 0.95, exclude_missing_tokens = TRUE)
demo_sum <- besd_summary_demographics(besd_sum)

# ── 3. Compute demographic breakdowns ─────────────────────────────────────────
# This powers the "BeSD by Demographic" tab. Skip (or set breakdown_sum <- NULL)
# if you have no demographic variables — the tab will simply be empty.

dem_vars <- unique(dem_dictionary()$item_id)  # or supply your own character vector

breakdown_sum <- dplyr::bind_rows(lapply(dem_vars, function(v) {
  tryCatch(
    besd_summary_by(dat_besd, by_col = v,
                    conf_level             = 0.95,
                    exclude_missing_tokens = TRUE),
    error = function(e) {
      message("Skipped '", v, "': ", conditionMessage(e))
      NULL
    }
  )
}))

# ── 4. Save to the app data directory ─────────────────────────────────────────
out_dir <- system.file("shiny/explorer/data", package = "rbesd")

topbox_all <- rbesd::besd_topbox(besd_sum)

saveRDS(besd_sum,      file.path(out_dir, "besd_sum.rds"))
saveRDS(demo_sum,      file.path(out_dir, "demo_sum.rds"))
saveRDS(breakdown_sum, file.path(out_dir, "breakdown_sum.rds"))
saveRDS(topbox_all,    file.path(out_dir, "topbox_all.rds"))

message("Saved besd_sum.rds, demo_sum.rds, and breakdown_sum.rds to ", out_dir)

# ── 5. Launch the Explorer ────────────────────────────────────────────────────
launch_explorer()
