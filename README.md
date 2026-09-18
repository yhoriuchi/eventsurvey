# eventsurvey <img src="man/figures/logo.svg" align="right" width="150" alt="eventsurvey logo" />

`eventsurvey` estimates short-run causal effects when an event occurs while a
survey is in the field. It extrapolates the observed pre-event trend, compares
that no-event counterfactual with post-event survey means, and uses matched
rolling forecasts to account for extrapolation error.

## Installation

The repository is being prepared for publication. Once it is public, install
the development version with:

```r
install.packages("remotes")
remotes::install_github("yhoriuchi/eventsurvey")
```

## Quick start

The interface needs a respondent-level outcome, an equally spaced time
variable, an event time, and one window length.

```r
library(eventsurvey)

survey_data <- simulate_eventsurvey(seed = 10)
fit <- eventsurvey(y ~ day, survey_data, event_time = 0, window = 6)

fit
plot(fit)
plot(fit, type = "effects")
plot(fit, type = "diagnostics")
```

The `window` argument is used symmetrically: each model is fit to that many
consecutive pre-event periods and forecasts the next `window` periods. At
least `2 * window` consecutive pre-event periods are required.
Skipping the event period requires one additional pre-event period so the
rolling diagnostic can reproduce the same one-period gap.

## What the estimate means

The post-event mean is observed. The missing quantity is the mean that would
have been observed on the same dates without the event. `eventsurvey`
estimates that path with a local linear extrapolation. Its honest interval
combines sampling variation with a data-driven bound based on the largest
matched pre-event forecast error.

The current release implements the manuscript's validated time-only linear
specification. Covariate adjustment and alternative trends are not exposed as
public options until their joint uncertainty calculations are validated.

When respondent-level records cannot be redistributed,
`eventsurvey_summary()` accepts one row per period with the response count,
mean, and within-period variance. The package includes documented summary data
for the CC0-licensed Epifanio--Giani--Ivandic application. For the
Bateson--Weintraub application, the website provides a preparation recipe that
authorized AmericasBarometer users can run locally without redistributing the
source data.

## Learn more

Start with [Getting Started](https://yhoriuchi.github.io/eventsurvey/articles/getting-started.html),
then see the [complete workflow](https://yhoriuchi.github.io/eventsurvey/articles/example-workflow.html),
[published applications](https://yhoriuchi.github.io/eventsurvey/articles/published-applications.html),
[methodology](https://yhoriuchi.github.io/eventsurvey/articles/methodology.html),
and [sensitivity analysis](https://yhoriuchi.github.io/eventsurvey/articles/sensitivity.html).
