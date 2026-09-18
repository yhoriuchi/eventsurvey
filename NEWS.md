# eventsurvey 0.0.0.9000

* Sparse survey schedules are now supported by default with
  `pre_periods = "observed"`. Fitted objects report missing pre-event and
  forecast periods and elapsed rolling-window spans; strict consecutive-period
  validation remains available with `pre_periods = "consecutive"`.
* Observed-period reference windows now mirror an excluded event period by
  skipping one observed period between fitting and forecasting windows.
* Added a simulation article comparing complete daily fieldwork with a sparse
  schedule, including coverage, counterfactual, gap, and estimand diagnostics.
* Initialized the R package, testing, continuous-integration, and pkgdown
  website environment.
* Added the formula-based `eventsurvey()` estimator with analytic
  bounded-misspecification-error inference, period-specific estimates,
  rolling forecast diagnostics, Date support, and explicit event-day rules.
* Added `simulate_eventsurvey()` and four `ggplot2` visualizations.
* Added complete getting-started, data-preparation, example, methodology,
  sensitivity, and FAQ articles.
