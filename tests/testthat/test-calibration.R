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

test_that("daily and average intervals match independent full covariance calculations", {
  d <- calibration_summary_data()
  for (event_day in c("exclude", "include")) {
    for (calibration in c("estimand", "pooled")) {
      fit <- eventsurvey_summary(mean ~ day, d,
        window = 4,
        event_day = event_day, calibration = calibration
      )
      rows <- t(vapply(seq_len(nrow(fit$reference)), function(i) {
        start <- fit$reference$fit_start[i]
        calibration_contrast(d, start:(start + 3L), fit$reference$forecast_period[i])
      }, numeric(nrow(d))))
      targets <- fit$fitted$relative_period
      point_rows <- t(vapply(targets, function(t) {
        calibration_contrast(d, -4:-1, t)
      }, numeric(nrow(d))))
      groups <- split(seq_len(nrow(rows)), fit$reference$fit_start)
      average_rows <- t(vapply(groups, function(i) colMeans(rows[i, , drop = FALSE]), numeric(nrow(d))))
      selected <- if (calibration == "estimand") average_rows else rows
      expect_equal(as.vector(rows %*% d$mean), fit$reference$error, tolerance = 1e-10)
      expect_equal(as.vector(average_rows %*% d$mean), fit$reference_average$error, tolerance = 1e-10)
      expect_equal(sqrt(as.vector(average_rows^2 %*% (d$variance / d$n))),
        fit$reference_average$std_error,
        tolerance = 1e-10
      )
      expected <- calibration_independent_interval(colMeans(point_rows), selected, d)
      expect_equal(unname(confint(fit)[1, ]), expected, tolerance = 1e-10)
      for (h in seq_along(targets)) {
        i <- if (calibration == "estimand") which(fit$reference$horizon == targets[h] + 1) else seq_len(nrow(rows))
        expected <- calibration_independent_interval(point_rows[h, ], rows[i, , drop = FALSE], d)
        expect_equal(c(fit$days$conf_low[h], fit$days$conf_high[h]), expected, tolerance = 1e-10)
        expect_equal(fit$days$n_reference[h], length(i))
        expect_equal(fit$days$bias_bound[h], max(abs(fit$reference$error[i])))
      }
    }
  }
})

test_that("signed averaging and common-window nesting are preserved", {
  d <- calibration_summary_data()
  specific <- eventsurvey_summary(mean ~ day, d, window = 4)
  pooled <- eventsurvey_summary(mean ~ day, d, window = 4, calibration = "pooled")
  averages <- aggregate(error ~ fit_start, specific$reference, mean)
  expect_equal(specific$settings$calibration, "estimand")
  expect_equal(specific$reference, pooled$reference)
  expect_equal(specific$reference_average$error, averages$error)
  expect_equal(specific$estimate$bias_bound, max(abs(averages$error)))
  expect_lt(specific$estimate$bias_bound, max(abs(specific$reference$error)))
  mean_absolute <- aggregate(abs(error) ~ fit_start, specific$reference, mean)[[2]]
  expect_true(any(abs(averages$error) < mean_absolute - 1e-8))
  expect_equal(specific$estimate$effect, pooled$estimate$effect)
  expect_equal(specific$estimate$std_error, pooled$estimate$std_error)
  expect_equal(specific$estimate$n_reference, nrow(specific$windows))
  expect_equal(pooled$estimate$n_reference, nrow(pooled$reference))
  expect_gte(specific$estimate$conf_low, pooled$estimate$conf_low - 1e-10)
  expect_lte(specific$estimate$conf_high, pooled$estimate$conf_high + 1e-10)
  expect_true(all(specific$days$conf_low >= pooled$days$conf_low - 1e-10))
  expect_true(all(specific$days$conf_high <= pooled$days$conf_high + 1e-10))
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
  expect_equal(
    fit$reference_average$error,
    aggregate(error ~ fit_start, fit$reference, mean)$error
  )
  # Sufficient observed periods, but no calendar-horizon match.
  d$day[d$day < 0] <- 10 * d$day[d$day < 0]
  expect_error(
    eventsurvey_summary(mean ~ day, d, window = 4),
    "No complete pre-event reference windows"
  )
  expect_s3_class(eventsurvey_summary(mean ~ day, d, window = 4, calibration = "pooled"), "eventsurvey")
})

test_that("one reference window and included hidden event day use correct targets", {
  d <- calibration_summary_data()
  for (event_day in c("exclude", "include")) {
    min_day <- if (event_day == "exclude") -9 else -8
    fit <- eventsurvey_summary(mean ~ day, d[d$day >= min_day, ],
      window = 4,
      event_day = event_day, report_event_day = FALSE
    )
    expect_equal(fit$estimate$n_reference, 1L)
    expect_true(all(fit$days$n_reference == 1L))
    expect_false(0 %in% fit$days$relative_period)
    expect_equal(fit$estimate$effect, mean(fit$fitted$observed - fit$fitted$counterfactual))
  }
})

test_that("calibration is validated and retained when plotting inner intervals", {
  for (method in c("estimand", "pooled")) {
    fit <- eventsurvey(y ~ day, sample_data, calibration = method)
    other <- eventsurvey(y ~ day, sample_data, calibration = method, alpha = 0.1)
    inner <- plot(fit, "effects", inner_level = 0.9)$layers[[4]]$data
    expect_equal(inner$conf_low, other$days$conf_low)
    expect_equal(inner$conf_high, other$days$conf_high)
    expect_match(paste(capture.output(fit), collapse = "\n"), paste("Calibration:", method))
    # Pre-0.3 serialized fits lack calibration metadata and used pooled.
    if (method == "pooled") {
      fit$settings$calibration <- NULL
      old_inner <- plot(fit, "effects", inner_level = 0.9)$layers[[4]]$data
      expect_equal(old_inner$conf_low, other$days$conf_low)
    }
  }
  expect_error(eventsurvey(y ~ day, sample_data, calibration = "unknown"), "arg")
  expect_error(eventsurvey_summary(mean ~ day, calibration_summary_data(), calibration = "unknown"), "arg")
})
