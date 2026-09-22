# eventsurvey: reproducible simulation and bootstrap validation of covariates.
# Run from the package root with Rscript data-raw/01-validate-covariates.R.

# Initial settings --------------------------------------------------------
devtools::load_all(quiet = TRUE)
dir.create("output", showWarnings = FALSE)
replications <- 300L
bootstrap_replications <- 400L

# Simulate data -----------------------------------------------------------
generate_data <- function(seed, scenario, n = 100L) {
  set.seed(seed)
  day <- rep(c(-18:-1, 1:4), each = n)
  shift <- if (scenario == "composition") 0.8 * sin(day / 3) else 0
  x <- rnorm(length(day), mean = shift)
  noise_sd <- if (scenario == "heteroskedastic") 0.5 + 0.7 * abs(x) else 1
  nonlinear <- if (scenario == "quadratic") 0.025 * day^2 else 0
  effect <- 1.5 + if (scenario == "heterogeneous") 0.6 * x else 0
  y <- 5 + 0.08 * day + 3 * x + nonlinear + (day > 0) * effect +
    rnorm(length(day), sd = noise_sd)
  data.frame(day, x, y)
}

# Monte Carlo -------------------------------------------------------------
scenarios <- c("linear", "composition", "heteroskedastic", "heterogeneous", "quadratic")
results <- list()
for (scenario in scenarios) {
  cat("Monte Carlo:", scenario, "\n")
  draws <- lapply(seq_len(replications), function(b) {
    dat <- generate_data(10000L + b, scenario)
    fits <- list(
      time_only = eventsurvey(y ~ day, dat, window = 4),
      adjusted = eventsurvey(y ~ day + x, dat, window = 4)
    )
    do.call(rbind, lapply(names(fits), function(name) {
      e <- fits[[name]]$estimate
      data.frame(
        model = name, estimate = e$effect, se = e$std_error,
        conventional = e$conventional_low <= 1.5 & e$conventional_high >= 1.5,
        honest = e$conf_low <= 1.5 & e$conf_high >= 1.5,
        width = e$conf_high - e$conf_low,
        reference_rmse = sqrt(mean(fits[[name]]$reference$error^2))
      )
    }))
  })
  draws <- do.call(rbind, draws)
  for (name in unique(draws$model)) {
    d <- draws[draws$model == name, ]
    results[[length(results) + 1L]] <- data.frame(
      scenario,
      model = name, replications,
      bias = mean(d$estimate) - 1.5,
      rmse = sqrt(mean((d$estimate - 1.5)^2)),
      empirical_sd = sd(d$estimate), rms_se = sqrt(mean(d$se^2)),
      conventional_coverage = mean(d$conventional),
      honest_coverage = mean(d$honest), mean_honest_width = mean(d$width),
      reference_rmse = mean(d$reference_rmse)
    )
  }
}
results <- do.call(rbind, results)
write.csv(results, "output/covariate-monte-carlo.csv", row.names = FALSE)
print(results, digits = 4, row.names = FALSE)

# Independent stratified bootstrap ---------------------------------------
# Refit with stats::lm(), without using the package covariance implementation.
dat <- generate_data(7391, "heteroskedastic", n = 200)
fit <- eventsurvey(y ~ day + x, dat, window = 4)
reference <- fit$reference
stacked_estimates <- function(d) {
  model <- lm(y ~ day + x, data = d[d$day %in% -4:-1, ])
  day_effects <- vapply(1:4, function(t) {
    target <- d[d$day == t, ]
    mean(target$y - predict(model, target))
  }, numeric(1))
  errors <- numeric(nrow(reference))
  for (start in unique(reference$fit_start)) {
    model <- lm(y ~ day + x, data = d[d$day %in% start:(start + 3L), ])
    indices <- which(reference$fit_start == start)
    errors[indices] <- vapply(reference$forecast_period[indices], function(t) {
      target <- d[d$day == t, ]
      mean(target$y - predict(model, target))
    }, numeric(1))
  }
  c(mean(day_effects), errors, day_effects)
}
set.seed(1917)
strata <- split(seq_len(nrow(dat)), dat$day)
cat("Independent bootstrap:", bootstrap_replications, "replicates\n")
bootstrap <- replicate(bootstrap_replications, {
  idx <- unlist(lapply(strata, function(i) sample(i, length(i), replace = TRUE)))
  stacked_estimates(dat[idx, ])
})
bootstrap_covariance <- cov(t(bootstrap))
nr <- nrow(reference)
analytic_variance <- c(
  fit$estimate$std_error^2,
  fit$inference$reference_variance, diag(fit$inference$day_covariance)
)
bootstrap_comparison <- data.frame(
  component = c("average", paste0("reference_", seq_len(nr)), paste0("day_", 1:4)),
  analytic_se = sqrt(analytic_variance),
  bootstrap_se = sqrt(diag(bootstrap_covariance))
)
bootstrap_comparison$ratio <- bootstrap_comparison$analytic_se / bootstrap_comparison$bootstrap_se
write.csv(bootstrap_comparison, "output/covariate-bootstrap.csv", row.names = FALSE)
analytic_combined <- sqrt(fit$estimate$std_error^2 +
  fit$inference$reference_variance + 2 * rowMeans(fit$inference$reference_day_covariance))
bootstrap_combined <- vapply(seq_len(nr), function(r) sd(bootstrap[1, ] + bootstrap[1 + r, ]), numeric(1))
cat("Average analytic / bootstrap SE:", bootstrap_comparison$ratio[1], "\n")
cat("Reference SE ratio range:", range(bootstrap_comparison$ratio[2:(nr + 1L)]), "\n")
cat("Combined effect + reference SE ratio range:", range(analytic_combined / bootstrap_combined), "\n")
write.csv(data.frame(
  reference = seq_len(nr), analytic_se = analytic_combined,
  bootstrap_se = bootstrap_combined
), "output/covariate-bootstrap-combined.csv", row.names = FALSE)

# Verification ------------------------------------------------------------
correct <- subset(results, scenario != "quadratic" & model == "adjusted")
stopifnot(
  all(abs(correct$bias) < 0.1),
  all(correct$rms_se / correct$empirical_sd > 0.8),
  all(correct$rms_se / correct$empirical_sd < 1.2),
  all(correct$conventional_coverage > 0.89),
  all(correct$honest_coverage > 0.94),
  all(bootstrap_comparison$ratio > 0.75),
  all(bootstrap_comparison$ratio < 1.25)
)
cat("Simulation and bootstrap validation passed.\n")
