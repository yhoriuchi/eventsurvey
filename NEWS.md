# eventsurvey 0.3.0

* Changes the default to `calibration = "estimand"` in both estimation
  interfaces. Daily effects use matching elapsed forecast horizons; the
  window-average effect uses signed average errors within placebo windows.
  Use `calibration = "pooled"` to reproduce the previous calibration and
  rolling-window construction. Point estimates are unchanged.
* Uses common complete reference windows with the final elapsed forecast
  horizons, including on sparse schedules. Insufficient matching history
  produces an informative error; there is no automatic pooled fallback.
* Propagates reference-average sampling uncertainty and covariance with the
  effect estimate, including shared fits and forecast covariate means.
  Daily intervals remain pointwise. Validity requires the corresponding
  population bias restriction and sampling assumptions.
* Exposes signed window-average references and their standard errors in
  `reference_average`; daily results include horizon, estimated bias bound,
  and reference count. Console output and HTML reports identify calibration.
* Adds an inference derivation, independent covariance and compatibility
  tests, and a reproducible simulation/stratified-bootstrap validation script.

* Regenerates published-application reports and figures with estimand-specific
  calibration as the main method and explicitly labeled pooled comparisons.
  Tutorials, including sparse schedules, follow the same convention.

# eventsurvey 0.2.0

* Adds additive respondent-level covariates using `outcome ~ time + x1 + x2`.
  Numeric, factor, character, and logical covariates are supported. Every
  fitting window uses OLS and predictions use each forecast period's covariates.
* Implements period-stratified respondent influence functions for adjusted
  uncertainty, including estimated covariate means and cross-window overlap.
  Adjusted models require at least two complete responses per used period and
  full-rank fits with residual degrees of freedom. The time-only estimator
  and period-summary interface retain their existing calculations.
* Updates adjusted counterfactual plots, inner effect intervals, HTML reports,
  and documentation, including a covariate-adjustment article. Adds numerical,
  regression, and simulation validation of the extension.

# eventsurvey 0.1.0

* Initial release of `eventsurvey()` for respondent-level data frames or
  tibbles and `eventsurvey_summary()` for period-level counts, means, and
  variances. Both
  interfaces estimate short-run event effects by extrapolating a linear
  pre-event trend and constructing honest bounded-misspecification intervals
  from matched rolling forecasts.
* Supports sparse survey schedules, numeric periods, dates and timestamps,
  explicit event-day rules, and automatic diagnostics for calendar coverage,
  period sample sizes, variance information, reference windows, and elapsed
  fitting and forecasting spans.
* Provides standard fitted-object methods, publication-ready plots, and
  `eventsurvey_report()` for creating a self-contained HTML report containing
  estimates, diagnostics, settings, figures, and interpretation guidance.
* Includes the ready-to-use respondent-level `sample_data` dataset and
  period-level summaries for reproducing the Epifanio, Giani, and Ivandic
  application. The Bateson and Weintraub example includes a local preparation
  workflow for authorized users of the licensed source data.
* Provides articles covering data preparation, methodology and assumptions,
  diagnostics, sensitivity analysis, sparse schedules, period-level data,
  published-study examples, and interpretation of HTML reports.
