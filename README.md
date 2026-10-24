<p align="center" style="padding-top: 2rem;">
  <img src="man/figures/logo.svg" width="190" alt="eventsurvey logo" />
</p>

<p align="center">
  <a href="https://github.com/yhoriuchi/eventsurvey/releases/latest"><img src="https://img.shields.io/github/v/release/yhoriuchi/eventsurvey?sort=semver" alt="Latest GitHub release" /></a>
  <a href="https://github.com/yhoriuchi/eventsurvey/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/yhoriuchi/eventsurvey/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check status" /></a>
  <img src="https://img.shields.io/badge/CRAN-not%20published-lightgrey" alt="CRAN not published" />
  <img src="https://img.shields.io/badge/downloads-not%20available-lightgrey" alt="Downloads not available" />
</p>

## Causal inference for events during survey fieldwork

`eventsurvey` estimates how survey outcomes change when an event occurs during
fieldwork. It learns the pre-event trend, projects what would have happened
without the event, and compares that counterfactual with the observed
post-event outcomes. The package provides estimates, uncertainty intervals,
automatic diagnostics, figures, and a shareable HTML report.

## Installation

Install version 0.4.1, which uses daily estimand-specific
calibration followed by aggregation for period averages:

```r
install.packages("remotes")
remotes::install_github("yhoriuchi/eventsurvey", ref = "v0.4.1")
```

This command pins the installation to the `v0.4.1` release for reproducibility.

Check your installed package version with `packageVersion("eventsurvey")`.
The release badge links to the [latest published release](https://github.com/yhoriuchi/eventsurvey/releases/latest);
the website's navigation bar shows the version used to build its documentation.

## Quick start

```r
library(eventsurvey)
fit <- eventsurvey(y ~ day, sample_data)
```

In `sample_data`, `y` is a five-category survey response coded from 1 (lowest)
to 5 (highest), and `day` is the interview day relative to the event: negative
values are before the event, 0 is the event day, and positive values are after
the event.
By default, the estimator excludes day 0 because respondents' exposure is
often unclear during the event period; forecasting begins on day 1.

Create a complete, self-contained HTML report with one additional command:

```r
eventsurvey_report(fit)
```

The fitted object checks the data and design automatically, and the HTML report
brings the main estimates, diagnostics, and figures together in one file.

## Calibration and a five-day average

Calibration matches each day's placebo errors to its elapsed forecast horizon.
For a period average, the package calibrates each day first, then averages the
estimates and bias bounds with equal day weights. It does not average signed
prediction errors across days before calibration.

```r
# Average effect over Days 1–5, excluding the event day
fit <- eventsurvey(y ~ day, sample_data, window = 5, schedule = "consecutive")
fit$estimate
fit$days
```

The average standard error includes covariance between daily estimates sharing
the pre-event regression. The average honest interval aggregates daily intervals
with a Bonferroni correction; displayed daily intervals remain pointwise.
Inspect `fit$average_components` for the intervals used in aggregation.
See [Methodology](articles/methodology.html) for assumptions and inference.

## Add pre-event covariates

With respondent-level data, put time first and add baseline characteristics:

```r
adjusted <- eventsurvey(outcome ~ day + age + education, survey_data)
```

Numeric covariates enter linearly; factors and character columns enter as
categorical indicators. Each pre-event window fits its own OLS model. Forecasts
average predictions over each forecast period's respondents, and uncertainty
includes estimated covariate means and overlapping fits. Use characteristics
unaffected by the event, and assess forecasting performance before the event.
See [Covariate Adjustment](articles/covariate-adjustment.html) for a runnable
example, assumptions, and limitations. Summary-only data remain time-only.

## Learn more

- [Getting Started](articles/getting-started.html) — Fit a model and
  create a report.
- [Reading an HTML Report](articles/reading-html-reports.html) — Interpret the
  estimates, diagnostics, and figures.
- [Methodology](articles/methodology.html) — Understand the estimand,
  assumptions, and uncertainty procedure.
- [Examples](articles/published-applications.html) — See the
  workflow applied to published studies.

Browse the [complete article library](articles/index.html) or the
[function reference](reference/index.html).

## Additional Info

### Upcoming features

The current version estimates the effect of one event occurring during one
survey fieldwork period. Future versions may extend the method to repeated
events and to pooling comparable events across fieldwork periods—for example,
designs studying multiple high-level visits. These extensions require new
work on the estimand, dependence across events, and uncertainty.

Other potential features include alternative trend
specifications, survey weights, clustered uncertainty, and richer workflows
for multiple outcomes and subgroups. Each requires additional methodological
development and validation before becoming a public package option.

### Comments, questions, or suggestions?

Please check the [existing GitHub issues](https://github.com/yhoriuchi/eventsurvey/issues).
If your question or suggestion is not already covered, please open a new issue.
