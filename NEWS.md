# rbesd (development version)

## Bug fixes
- `tidy_model()`: ordinal cutpoints from `clm()` (label-based `A|B` names) were
  misidentified as slopes, triggering spurious "could not be decoded" warnings.
  Cutpoints now appear in the tidy table with `variable = "(Cutpoint)"` and
  `level` showing the full threshold label.
- `tidy_model()`: intercept rows now show `variable = "(Intercept)"` rather than
  `NA` for clarity.
- `tidy_model()`: profile CI matrix was aligned by position rather than name,
  causing CIs to be off by one row when the intercept was present. Now aligned
  by rowname with Wald fallback for any unmatched terms.
- `tidy_model()`: "Waiting for profiling to be done..." messages from
  `stats::confint()` are now suppressed.
- `tidy_model()`: baseline reference codes removed from output columns.
- `.fit_model()`: missing `...` in function signature caused
  `'...' used in an incorrect context` error when extra arguments were passed
  via `besd_regress()`.
- `besd_regress()`: constant predictors (single unique non-NA value) are now
  detected before fitting and excluded with an informative message rather than
  causing a model error.
- `besd_regress()`: reference levels are no longer set to catch-all categories
  such as "Other" or "Prefer not to say".
- `besd_recode_missing()`: did not update `dict$levels` after setting values to
  `NA`, causing `summary()` to emit ghost rows for dropped tokens that existed
  as dictionary levels.
- `modify_dictionary()`: factor-valued levels (e.g. from
  `sort(unique(factor_col))`) were stored as integer codes rather than labels,
  corrupting dictionary levels after any `c(factor, character)` operation
  downstream.
- `as_besd()`: unknown-value errors now report all problematic columns at once
  rather than stopping on the first. Item name is propagated into all
  unknown-value errors and warnings for easier diagnosis.
- `as_besd()`: fixed `grepl` pattern in `.coerce_dict_items()` that incorrectly
  muffled unknown-value warnings.
- `besd_prepare()`: added missing token guard to prevent factor levels outside
  the dictionary from reaching the model fitter.

## Improvements
- `besd_regress()` now accepts multiple outcomes, fitting each independently with
  shared predictor preparation.
- `besd_rare_levels()` added as a pre-flight diagnostic for inspecting sparse
  factor levels before fitting.
- Common predictor reference levels in by_country models are now selected from
  the full pooled dataset rather than independently per country.
- Missing data diagnostics and complete-case warnings added to the regression
  pipeline.
- `.update_dict_levels()` extracted into `dictionary.R` as a shared internal
  helper used by both `besd_recode_levels()` and `besd_recode_missing()` to keep
  dictionary levels consistent with the data. Also fixes a spurious warning in
  `besd_recode_levels()` when `NA_character_` was passed as a recode target.
- `besd_recode_missing()` moved from `besd-as_besd.R` to `missing.R` alongside
  the other user-facing data cleaning helpers.
- `lme4` and `ordinal` moved from `Suggests` to `Imports` so they are installed
  automatically with the package.
- `.assert_is_scalar_number()` added to `utils-assert.R`.
- Regression tidy dispatch rearchitected by model class for cleaner extensibility.
- Predictor encoding edge cases covered by new tests.

# rbesd 0.1.0.9000
- Initial release with data preparation, validation, summaries, plotting, and
  regression wrappers.