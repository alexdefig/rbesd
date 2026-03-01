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
- `.fit_model()`: missing `...` in function signature caused `'...' used in an
  incorrect context` error when extra arguments were passed via `besd_regress()`.
- `modify_dictionary()`: factor-valued levels (e.g. from `sort(unique(factor_col))`)
  were stored as integer codes rather than labels, corrupting dictionary levels
  after any `c(factor, character)` operation downstream.
- `besd_recode_missing()`: did not update `dict$levels` after setting values to
  `NA`, causing `summary()` to emit ghost rows for dropped tokens that existed
  as dictionary levels.

## Improvements
- `.update_dict_levels()` extracted into `dictionary.R` as a shared internal
  helper used by both `besd_recode_levels()` and `besd_recode_missing()` to keep
  dictionary levels consistent with the data. Also fixes a spurious warning in
  `besd_recode_levels()` when `NA_character_` was passed as a recode target.
- `besd_recode_missing()` moved from `besd-as_besd.R` to `missing.R` alongside
  the other user-facing data cleaning helpers.
- Common predictor reference levels in by_country models are now selected from
  the full pooled dataset rather than independently within each country.
- `lme4` and `ordinal` moved from `Suggests` to `Imports` so they are installed
  automatically with the package.
- `.assert_is_scalar_number()` added to `utils-assert.R`.
- General QoL improvements: 90-char line maximum, consistent code style.

# rbesd 0.1.0.9000
- Initial release with data preparation, validation, summaries, plotting, and 
  regression wrappers.