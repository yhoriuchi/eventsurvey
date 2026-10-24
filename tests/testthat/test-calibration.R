# What this code is about: Verify the package API and inference numerically.
# Direct component/matrix access is intentional when testing the object contract.

# Regression checks ------------------------------------------------------
calibration_summary_data <- function() {
  day <- c(-20:-1, 0:5)
  data.frame(
    day,
    n = rep(c(30, 80, 50), length.out = length(day)),
    mean = 3 + 0.1 * day + sin(day / 2) + (day >= 0) * 0.8,
    variance = rep(c(1, 2), length.out = length(day))
  )
}

# Independent WLS prediction coefficients, using lm rather than package helpers.
calibration_contrast <- function(d, fit_days, target) {
  train <- which(d$day %in% fit_days)
  model <- lm(mean ~ day, d[train, ], weights = n)
  x <- model.matrix(model)
  row <- numeric(nrow(d))
  row[train] <- -as.vector(c(1, target) %*%
    solve(crossprod(x, x * d$n[train]), t(x) * rep(d$n[train], each = 2)))
  row[match(target, d$day)] <- 1
  row
}

calibration_independent_interval <- function(point_row, rows, d, alpha = 0.05) {
  # Stack both signs, with the full covariance of period means.
  a <- rbind(sweep(rows, 2, point_row, "+"), sweep(-rows, 2, point_row, "+"))
  point <- as.vector(a %*% d$mean)
  se <- sqrt(diag(a %*% diag(d$variance / d$n) %*% t(a)))
  c(
    min(point - qnorm(1 - alpha / 2) * se),
    max(point + qnorm(1 - alpha / 2) * se)
  )
}

test_that("daily-first calibration and full covariance agree with independent WLS", {
  d <- calibration_summary_data()
  for (event_day in c("exclude", "include")) {
    fit <- eventsurvey_summary(mean ~ day, d, window = 5, event_day = event_day)
    targets <- fit$fitted$relative_period
    rows <- t(vapply(seq_len(nrow(fit$reference)), function(i) {
      start <- fit$reference$fit_start[i]
      calibration_contrast(d, start:(start + 4L), fit$reference$forecast_period[i])
    }, numeric(nrow(d))))
    point_rows <- t(vapply(targets, function(t) {
      calibration_contrast(d, -5:-1, t)
    }, numeric(nrow(d))))
    covariance <- point_rows %*% diag(d$variance / d$n) %*% t(point_rows)
    expect_equal(fit$inference$day_covariance, covariance, tolerance = 1e-10)
    expect_equal(fit$days$std_error, sqrt(diag(covariance)), tolerance = 1e-10)
    expect_equal(fit$estimate$std_error, sqrt(sum(covariance)) / 5, tolerance = 1e-10)
    expect_gt(fit$estimate$std_error, sqrt(sum(diag(covariance))) / 5)
    expect_equal(fit$estimate$effect, mean(fit$days$effect))
    expect_equal(as.vector(rows %*% d$mean), fit$reference$error, tolerance = 1e-10)
    components <- matrix(0, length(targets), 2)
    for (h in seq_along(targets)) {
      i <- which(fit$reference$horizon == targets[h] + 1)
      expected <- calibration_independent_interval(point_rows[h, ], rows[i, , drop = FALSE], d)
      expect_equal(c(fit$days$conf_low[h], fit$days$conf_high[h]), expected, tolerance = 1e-10)
      components[h, ] <- calibration_independent_interval(point_rows[h, ], rows[i, , drop = FALSE], d, alpha = 0.05 / 5)
      expect_equal(fit$days$n_reference[h], length(i))
      expect_equal(fit$days$bias_bound[h], max(abs(fit$reference$error[i])))
    }
    expect_equal(as.matrix(fit$average_components[c("conf_low", "conf_high")]),
      components,
      ignore_attr = TRUE, tolerance = 1e-10
    )
    expect_equal(unname(confint(fit)[1, ]), colMeans(components), tolerance = 1e-10)
    expect_equal(fit$estimate$bias_bound, mean(fit$days$bias_bound))
    expect_equal(fit$estimate$n_reference, sum(fit$days$n_reference))
    signed_average_bound <- max(abs(aggregate(error ~ fit_start, fit$reference, mean)$error))
    expect_gt(fit$estimate$bias_bound, signed_average_bound)
    expect_null(fit$reference_average)
  }
})

test_that("sparse calibration matches elapsed offsets and rejects absent matches", {
  d <- calibration_summary_data()
  d <- d[!d$day %in% c(-9, -1, 2), ]
  fit <- eventsurvey_summary(mean ~ day, d, window = 4)
  expected_horizons <- c(3, 5, 6, 7)
  expect_equal(fit$days$horizon, expected_horizons)
  for (start in fit$windows$fit_start) {
    r <- fit$reference[fit$reference$fit_start == start, ]
    expect_equal(r$horizon, expected_horizons)
    expect_true(all(r$forecast_period < 0))
  }
  expect_equal(fit$estimate$effect, mean(fit$days$effect))
  expect_error(eventsurvey_summary(mean ~ day, d, window = 4, schedule = "consecutive"), "must be consecutive")
  d$day[d$day < 0] <- 10 * d$day[d$day < 0]
  expect_error(eventsurvey_summary(mean ~ day, d, window = 4), "No complete pre-event reference windows")
})

test_that("one reference window and hidden event day retain every average component", {
  d <- calibration_summary_data()
  for (event_day in c("exclude", "include")) {
    min_day <- if (event_day == "exclude") -9 else -8
    fit <- eventsurvey_summary(mean ~ day, d[d$day >= min_day, ],
      window = 4,
      event_day = event_day, report_event_day = FALSE
    )
    expect_equal(fit$estimate$n_reference, 4L)
    expect_true(all(fit$days$n_reference == 1L))
    expect_false(0 %in% fit$days$relative_period)
    expect_equal(nrow(fit$average_components), 4)
    expect_equal(fit$estimate$effect, mean(fit$average_components$effect))
    expect_equal(fit$estimate$bias_bound, mean(fit$average_components$bias_bound))
    expect_equal(fit$estimate$conf_low, mean(fit$average_components$conf_low))
    expect_equal(fit$estimate$conf_high, mean(fit$average_components$conf_high))
  }
})

test_that("only estimand calibration is accepted and inner intervals retain it", {
  fit <- eventsurvey(y ~ day, sample_data)
  other <- eventsurvey(y ~ day, sample_data, alpha = 0.1)
  inner <- plot(fit, "effects", inner_level = 0.9)$layers[[4]]$data
  expect_equal(inner$conf_low, other$days$conf_low)
  expect_equal(inner$conf_high, other$days$conf_high)
  expect_match(paste(capture.output(fit), collapse = "\n"), "Calibration: estimand")
  for (method in c("pooled", "unknown")) {
    expect_error(eventsurvey(y ~ day, sample_data, calibration = method), "arg")
    expect_error(eventsurvey_summary(mean ~ day, calibration_summary_data(), calibration = method), "arg")
    dat <- sample_data
    set.seed(19)
    dat$x <- rnorm(nrow(dat))
    expect_error(eventsurvey(y ~ day + x, dat, calibration = method), "arg")
  }
})
