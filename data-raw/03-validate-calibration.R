# Validate daily calibration followed by equal-day aggregation.
# What this code is about: Check coverage under daily bias restrictions and
# compare covariance-aware standard errors with a stratified bootstrap.
# Inputs: Simulated independent period means and respondent samples.
# Outputs: Local CSV validation results in output/ (excluded from releases).

# Initial settings --------------------------------------------------------
library(dplyr)
library(purrr)
devtools::load_all(quiet = TRUE)
dir.create("output", showWarnings = FALSE)
replications <- 1000L
bootstrap_replications <- 400L
set.seed(20261024)
# Matrix indexing and direct access to internal design/S3 components below
# keep the covariance algebra explicit; ordinary summaries use dplyr.

# Fixed-design coverage ---------------------------------------------------
scenarios <- c("linear", "positive_boundary", "negative_boundary", "mixed_boundary", "one_window")
results <- list()
for (scenario in scenarios) {
  days <- c(if (scenario == "one_window") -11:-1 else -20:-1, 1:5)
  counts <- rep(c(60, 100, 180), length.out = length(days))
  variances <- rep(c(0.5, 1, 2), length.out = length(days))
  ps <- tibble(
    relative_period = days, n = counts, mean = 0,
    variance = variances, mean_variance = variances / counts
  )
  built <- eventsurvey:::build_design(ps, 5, 1:5)
  pre <- days < 0
  post <- days > 0
  population <- 2 + 0.08 * days
  if (scenario != "linear") population[pre] <- population[pre] + 0.025 * days[pre]^2
  population_errors <- as.vector(built$reference_rows %*% population)
  daily_bounds <- map_dbl(2:6, function(h) {
    max(abs(population_errors[built$reference_map$horizon == h]))
  })
  bias <- switch(scenario,
    linear = rep(0, 5),
    negative_boundary = -daily_bounds,
    mixed_boundary = daily_bounds * c(1, -1, 1, -1, 1),
    daily_bounds
  )
  population[post] <- as.vector(built$counterfactual_rows %*% population) + bias
  treatment <- c(0.8, 0.6, 0.4, 0.2, 0)
  population[post] <- population[post] + treatment
  truth <- c(mean(treatment), treatment)
  draws <- vector("list", replications)
  for (b in seq_len(replications)) {
    noisy <- ps |>
      mutate(
        mean = population + rnorm(length(days), sd = sqrt(variances / counts)),
        mean_variance = variances * rchisq(length(days), counts - 1) / (counts - 1) / counts
      )
    fit <- eventsurvey:::estimate_design(built, noisy, 0.05)
    low <- c(fit$estimate$conf_low, fit$days$conf_low)
    high <- c(fit$estimate$conf_high, fit$days$conf_high)
    draws[[b]] <- tibble(
      target = c("average", paste0("day", 1:5)),
      covered = low <= truth & high >= truth, width = high - low
    )
  }
  results[[scenario]] <- list_rbind(draws) |>
    group_by(target) |>
    summarise(coverage = mean(covered), mean_width = mean(width), .groups = "drop") |>
    mutate(
      scenario = scenario, replications = replications,
      mc_se = sqrt(coverage * (1 - coverage) / replications)
    )
  cat("Completed coverage scenario:", scenario, "\n")
}
coverage <- list_rbind(results)
write.csv(coverage, "output/daily-first-coverage.csv", row.names = FALSE)
stopifnot(all(pull(coverage, coverage) > 0.95 - 4 * sqrt(0.95 * 0.05 / replications)))

# Adjusted model bootstrap ------------------------------------------------
set.seed(8275)
dat <- tibble(day = rep(c(-15:-1, 1:5), each = 150)) |>
  mutate(
    x = rnorm(n(), mean = 0.3 * sin(day / 3)),
    y = 2 + 0.1 * day + 1.8 * x + (day > 0) * 0.7 + rnorm(n(), sd = 0.8)
  )
fit <- eventsurvey(y ~ day + x, dat, window = 5)
# Independent lm calculation; the bootstrap refits the same pre-event model.
get_effects <- function(.dataframe) {
  model <- lm(y ~ day + x, filter(.dataframe, day %in% -5:-1))
  daily <- map_dbl(1:5, function(target) {
    forecast <- filter(.dataframe, day == target)
    mean(pull(forecast, y) - predict(model, forecast))
  })
  c(mean(daily), daily)
}
bootstrap <- replicate(bootstrap_replications, {
  sampled <- dat |>
    group_by(day) |>
    slice_sample(prop = 1, replace = TRUE) |>
    ungroup()
  get_effects(sampled)
})
comparison <- tibble(
  component = c("average", paste0("day", 1:5)),
  analytic_se = c(fit$estimate$std_error, fit$days$std_error),
  bootstrap_se = apply(bootstrap, 1, sd)
) |>
  mutate(ratio = analytic_se / bootstrap_se)
write.csv(comparison, "output/daily-first-bootstrap.csv", row.names = FALSE)
stopifnot(all(pull(comparison, ratio) > 0.8 & pull(comparison, ratio) < 1.2))
print(coverage)
print(comparison)
cat("Daily-first calibration validation passed.\n")
