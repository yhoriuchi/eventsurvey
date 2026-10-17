# eventsurvey: coverage, covariance, and bootstrap checks for calibration.
# Run from the package root: Rscript data-raw/03-validate-calibration.R
# Outputs are local and excluded from package builds and Git.

# Initial settings --------------------------------------------------------
devtools::load_all(quiet = TRUE)
dir.create("output", showWarnings = FALSE)
replications <- 2000L
adjusted_replications <- 300L
bootstrap_replications <- 500L
set.seed(20261016)

# Fixed-design normal experiments -----------------------------------------
# Fix counts and population paths. A normal mean and independent chi-squared
# sample variance reproduce sufficient statistics from independent normal
# respondents. Stress cases deliberately violate a stated assumption.
scenarios <- c(
  "linear", "curvature", "positive_boundary", "negative_boundary",
  "average_cancellation", "bound_violation", "correlated_period_shocks",
  "one_window_positive", "one_window_negative"
)
results <- list()
for (scenario in scenarios) {
  cat("Normal experiment:", scenario, "\n")
  day <- c(if (grepl("one_window", scenario)) -9:-1 else -18:-1, 1:4)
  n <- rep(if (grepl("one_window", scenario)) 1000 else c(60, 100, 180), length.out = length(day))
  sigma2 <- rep(c(0.5, 1, 2), length.out = length(day))
  ps <- data.frame(
    relative_period = day, n, mean = 0, variance = sigma2,
    mean_variance = sigma2 / n
  )
  built <- lapply(c("estimand", "pooled"), function(method) {
    eventsurvey:::build_design(ps, 4, 1:4, calibration = method)
  })
  names(built) <- c("estimand", "pooled")
  base <- built$estimand
  pre <- day < 0
  post <- day > 0
  mu <- 2 + 0.08 * day
  if (scenario %in% c("curvature", "positive_boundary", "negative_boundary", "one_window_positive", "one_window_negative")) {
    mu[pre] <- mu[pre] + 0.025 * day[pre]^2
  }
  if (scenario == "average_cancellation") mu[pre] <- mu[pre] + sin(day[pre] / 2)
  reference_population <- as.vector(base$reference_rows %*% mu)
  average_population <- as.vector(eventsurvey:::average_reference_rows(base) %*% mu)
  daily_bounds <- vapply(2:5, function(h) {
    max(abs(reference_population[base$reference_map$horizon == h]))
  }, numeric(1))
  average_bound <- max(abs(average_population))
  prediction <- as.vector(base$counterfactual_rows %*% mu)
  bias <- rep(0, 4)
  if (scenario == "curvature") bias <- rep(0.8 * min(c(daily_bounds, average_bound)), 4)
  if (scenario %in% c("positive_boundary", "negative_boundary", "one_window_positive", "one_window_negative")) {
    # Put the average bias exactly at its population bound; individual
    # targets are labeled valid/invalid separately, without assuming nesting.
    bias <- rep(if (scenario %in% c("positive_boundary", "one_window_positive")) average_bound else -average_bound, 4)
  }
  if (scenario == "average_cancellation") {
    # Average bound holds although individual daily bounds can fail.
    bias <- c(8, -8, 8, -8)
  }
  if (scenario == "bound_violation") bias <- rep(3, 4)
  mu[post] <- prediction + bias
  treatment <- c(0.8, 0.6, 0.4, 0.2)
  truth <- c(mean(treatment), treatment)
  population <- mu + ifelse(post, c(rep(0, sum(pre)), treatment), 0)
  draws <- vector("list", replications * 2L)
  for (b in seq_len(replications)) {
    noisy <- ps
    noise <- rnorm(length(day), sd = sqrt(sigma2 / n))
    if (scenario == "correlated_period_shocks") {
      noise <- noise + as.vector(arima.sim(list(ar = 0.7), n = length(day), sd = 0.6))
    }
    noisy$mean <- population + noise
    noisy$mean_variance <- sigma2 * rchisq(length(day), n - 1) / (n - 1) / n
    for (k in seq_along(built)) {
      average <- eventsurvey:::estimate_design(built[[k]], noisy, 0.05)$estimate
      daily <- eventsurvey:::estimate_days(built[[k]], noisy, 0.05)
      low <- c(average$conf_low, daily$conf_low)
      high <- c(average$conf_high, daily$conf_high)
      draws[[(b - 1L) * 2L + k]] <- data.frame(
        method = names(built)[k], target = c("average", paste0("day", 1:4)),
        coverage = low <= truth & high >= truth, width = high - low
      )
    }
  }
  draws <- do.call(rbind, draws)
  for (method in names(built)) {
    bounds <- if (method == "estimand") c(average_bound, daily_bounds) else rep(max(abs(reference_population)), 5)
    valid <- abs(c(mean(bias), bias)) <= bounds + 1e-10
    for (h in seq_along(truth)) {
      target <- c("average", paste0("day", 1:4))[h]
      d <- draws[draws$method == method & draws$target == target, ]
      coverage <- mean(d$coverage)
      results[[length(results) + 1L]] <- data.frame(
        scenario, method, target, replications,
        population_bias = c(mean(bias), bias)[h], population_bound = bounds[h],
        bound_holds = valid[h], independent_sampling = scenario != "correlated_period_shocks",
        coverage, mc_se = sqrt(coverage * (1 - coverage) / replications),
        mean_width = mean(d$width)
      )
    }
  }
}
results <- do.call(rbind, results)
write.csv(results, "output/calibration-coverage.csv", row.names = FALSE)
print(results, digits = 4, row.names = FALSE)
valid <- subset(results, bound_holds & independent_sampling)
# A Monte Carlo tolerance, not a proof of uniform asymptotic coverage.
stopifnot(all(valid$coverage > 0.95 - 4 * sqrt(0.95 * 0.05 / replications)))

# End-to-end adjusted model coverage ---------------------------------------
generate_adjusted <- function(seed, n = 120L) {
  set.seed(seed)
  day <- rep(c(-14:-1, 1:3), each = n)
  x <- rnorm(length(day), mean = 0.3 * sin(day / 3))
  y <- 2 + 0.1 * day + 1.8 * x + (day > 0) * 0.7 +
    rnorm(length(day), sd = 0.6 + 0.2 * abs(x))
  data.frame(y, day, x)
}
adjusted <- list()
for (b in seq_len(adjusted_replications)) {
  if (b %% 50L == 0L) cat("Adjusted Monte Carlo:", b, "\n")
  dat <- generate_adjusted(83000L + b)
  for (method in c("estimand", "pooled")) {
    fit <- eventsurvey(y ~ day + x, dat, window = 3, calibration = method)
    low <- c(fit$estimate$conf_low, fit$days$conf_low)
    high <- c(fit$estimate$conf_high, fit$days$conf_high)
    adjusted[[length(adjusted) + 1L]] <- data.frame(
      method,
      target = c("average", paste0("day", 1:3)),
      covered = low <= 0.7 & high >= 0.7, width = high - low
    )
  }
}
adjusted <- do.call(rbind, adjusted)
adjusted_summary <- aggregate(cbind(covered, width) ~ method + target, adjusted, mean)
adjusted_summary$replications <- adjusted_replications
adjusted_summary$mc_se <- sqrt(adjusted_summary$covered * (1 - adjusted_summary$covered) / adjusted_replications)
write.csv(adjusted_summary, "output/calibration-adjusted-coverage.csv", row.names = FALSE)
stopifnot(all(adjusted_summary$covered > 0.95 - 4 * sqrt(0.95 * 0.05 / adjusted_replications)))

# Independent stratified bootstrap for average references -----------------
dat <- generate_adjusted(1742, n = 200)
fit <- eventsurvey(y ~ day + x, dat, window = 3)
stacked <- function(d) {
  model <- lm(y ~ day + x, d[d$day %in% -3:-1, ])
  daily <- vapply(1:3, function(t) {
    x <- d[d$day == t, ]
    mean(x$y - predict(model, x))
  }, numeric(1))
  references <- vapply(fit$windows$fit_start, function(start) {
    model <- lm(y ~ day + x, d[d$day %in% start:(start + 2L), ])
    targets <- fit$reference$forecast_period[fit$reference$fit_start == start]
    mean(vapply(targets, function(t) {
      x <- d[d$day == t, ]
      mean(x$y - predict(model, x))
    }, numeric(1)))
  }, numeric(1))
  c(mean(daily), references)
}
set.seed(52719)
strata <- split(seq_len(nrow(dat)), dat$day)
bootstrap <- replicate(bootstrap_replications, {
  i <- unlist(lapply(strata, function(x) sample(x, length(x), replace = TRUE)))
  stacked(dat[i, ])
})
bootstrap_se <- apply(bootstrap, 1, sd)
analytic_se <- c(fit$estimate$std_error, fit$reference_average$std_error)
comparison <- data.frame(
  component = c("average", paste0("reference_average_", seq_len(nrow(fit$windows)))),
  analytic_se, bootstrap_se, ratio = analytic_se / bootstrap_se
)
combined <- do.call(rbind, lapply(c(-1, 1), function(sign) {
  analytic <- sqrt(fit$estimate$std_error^2 + fit$inference$average_reference_variance +
    2 * sign * rowMeans(fit$inference$average_reference_day_covariance))
  empirical <- apply(sweep(sign * bootstrap[-1, , drop = FALSE], 2, bootstrap[1, ], "+"), 1, sd)
  data.frame(sign,
    reference = seq_along(analytic), analytic_se = analytic,
    bootstrap_se = empirical, ratio = analytic / empirical
  )
}))
write.csv(comparison, "output/calibration-bootstrap.csv", row.names = FALSE)
write.csv(combined, "output/calibration-bootstrap-combined.csv", row.names = FALSE)
stopifnot(
  all(comparison$ratio > 0.8 & comparison$ratio < 1.2),
  all(combined$ratio > 0.8 & combined$ratio < 1.2)
)
cat("Adjusted coverage:\n")
print(adjusted_summary, digits = 4, row.names = FALSE)
cat("Bootstrap SE ratio range:", range(comparison$ratio), "\n")
cat("Combined (+/- reference) SE ratio range:", range(combined$ratio), "\n")
cat("Calibration validation passed.\n")
