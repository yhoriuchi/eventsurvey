# eventsurvey 0.3.0.9000

* Supports estimand-specific calibration only in both estimation interfaces.
* Calibrates each daily effect using matching elapsed forecast horizons, then
  aggregates daily effects and bias bounds with equal period weights.
  Signed forecast errors are no longer averaged across days for calibration.
* Constructs conservative average honest intervals by averaging daily endpoints
  computed at significance level `alpha / window` (Bonferroni). Displayed daily
  intervals remain pointwise. Average sampling standard errors retain the full
  cross-day covariance induced by the shared pre-event regression.
* Exposes daily `std_error`, `average_components` (all target days and the
  intervals used for aggregation), and `inference$day_covariance` in both
  interfaces. Removes `reference_average`; `estimate$n_reference` now counts
  all daily reference contrasts contributing to aggregation.
* Updates website examples, generated reports, and calibration verification.

# eventsurvey 0.3.0

* Introduced matching-horizon calibration and common complete reference
  windows, including for sparse schedules, with reference-error uncertainty.
  Its period-average calculation has been superseded by the daily-first
  aggregation described above.
* Added an inference derivation, covariance tests, and calibration validation.

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
