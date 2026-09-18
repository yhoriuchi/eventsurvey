test_that("the core estimator returns coherent results", {
  dat <- simulate_eventsurvey(seed = 10, pre = 24, post = 7, respondents = 30)
  fit <- eventsurvey(y ~ day, dat, event_time = 0, window = 6)

  expect_s3_class(fit, "eventsurvey")
  expect_equal(nrow(fit$days), 6)
  expect_equal(fit$estimate$n_windows, 13)
  expect_equal(fit$estimate$n_reference, 78)
  expect_lt(fit$estimate$conf_low, fit$estimate$effect)
  expect_gt(fit$estimate$conf_high, fit$estimate$effect)
  expect_gte(fit$estimate$conf_low, -Inf)
  expect_equal(max(abs(fit$reference$error)), fit$estimate$bias_bound)
})

test_that("linear period means are extrapolated exactly", {
  dat <- expand.grid(day = -12:5, respondent = 1:4)
  dat$y <- 3 + 0.5 * dat$day + c(-1, -0.5, 0.5, 1)[dat$respondent]
  fit <- eventsurvey(y ~ day, dat, event_time = 0, window = 6)

  expect_equal(fit$estimate$effect, 0, tolerance = 1e-10)
  expect_equal(fit$estimate$bias_bound, 0, tolerance = 1e-10)
  expect_equal(fit$fitted$observed, fit$fitted$counterfactual, tolerance = 1e-10)
})

test_that("Date inputs and excluded event periods work", {
  dat <- simulate_eventsurvey(seed = 11, pre = 20, post = 8)
  dat$date <- as.Date("2026-01-15") + dat$day
  fit <- eventsurvey(y ~ date, dat, event_time = as.Date("2026-01-15"),
                     window = 5, event_day = "exclude")

  expect_equal(fit$days$relative_period, 1:5)
  expect_equal(sort(unique(fit$reference$horizon)), 2:6)
})

test_that("the event period can be forecast but hidden from day results", {
  dat <- simulate_eventsurvey(seed = 12, pre = 20, post = 8)
  fit <- eventsurvey(y ~ day, dat, event_time = 0, window = 5,
                     report_event_day = FALSE)

  expect_false(0 %in% fit$days$relative_period)
  expect_true(0 %in% fit$fitted$relative_period)
})

test_that("calendar gaps and insufficient history fail clearly", {
  dat <- simulate_eventsurvey(seed = 13, pre = 12, post = 6)
  expect_error(eventsurvey(y ~ day, dat[dat$day != -7, ], 0, window = 4),
               "must be consecutive")
  expect_error(eventsurvey(y ~ day, dat[dat$day >= -7, ], 0, window = 4),
               "At least 8")
  expect_error(eventsurvey(y ~ day, dat[dat$day >= -8, ], 0, window = 4,
                           event_day = "exclude"), "At least 9")
  expect_error(eventsurvey(y ~ day, dat[dat$day != 2, ], 0, window = 4),
               "Every forecast period")
  expect_error(eventsurvey(y ~ day, dat[dat$day >= 0, ], 0, window = 4),
               "no pre-event")
})

test_that("plot methods return ggplot objects", {
  dat <- simulate_eventsurvey(seed = 14, pre = 16, post = 6)
  fit <- eventsurvey(y ~ day, dat, 0, window = 4)

  for (type in c("counterfactual", "effects", "diagnostics", "daily", "counts")) {
    expect_s3_class(plot(fit, type = type), "ggplot")
  }
  expect_s3_class(plot(fit, type = "effects", inner_level = 0.90), "ggplot")
  expect_error(plot(fit, type = "effects", inner_level = 0.99),
               "between zero and the fitted confidence level")
})

test_that("summary data reproduce respondent-level estimates", {
  dat <- simulate_eventsurvey(seed = 15, pre = 20, post = 6)
  split_y <- split(dat$y, dat$day)
  daily <- data.frame(
    day = as.integer(names(split_y)),
    mean = vapply(split_y, mean, numeric(1)),
    n = vapply(split_y, length, integer(1)),
    variance = vapply(split_y, stats::var, numeric(1))
  )
  individual <- eventsurvey(y ~ day, dat, 0, window = 5)
  summarized <- eventsurvey_summary(mean ~ day, daily,
    n = n, variance = variance, event_time = 0, window = 5)

  expect_equal(summarized$estimate, individual$estimate, tolerance = 1e-12)
  expect_equal(summarized$days, individual$days, tolerance = 1e-12)
})

test_that("published application summaries reproduce manuscript tables", {
  data("published_examples", package = "eventsurvey")
  privacy <- subset(published_examples, grepl("privacy", outcome))
  fit_p <- eventsurvey_summary(mean ~ day, privacy,
    n = n, variance = variance, event_time = 0, window = 5,
    report_event_day = FALSE, pre_periods = "observed")
  expect_equal(round(fit_p$days$effect, 3), c(-0.115, -0.050, -0.512, -0.050))
  expect_equal(round(fit_p$estimate$bias_bound, 3), 0.780)
  expect_equal(fit_p$estimate$n_windows, 7)

  procedural <- subset(published_examples, grepl("procedural", outcome))
  fit_r <- eventsurvey_summary(mean ~ day, procedural,
    n = n, variance = variance, event_time = 0, window = 7,
    report_event_day = FALSE, pre_periods = "observed")
  expect_equal(round(fit_r$days$effect, 3),
               c(-0.079, -0.046, -0.444, -0.033, -0.017, -0.070))
  expect_equal(round(fit_r$estimate$bias_bound, 3), 0.402)
  expect_equal(fit_r$estimate$n_windows, 4)
})

test_that("observed-period mode is explicit when pre-event dates are empty", {
  dat <- simulate_eventsurvey(seed = 16, pre = 16, post = 5)
  dat <- dat[dat$day != -12, ]
  expect_error(eventsurvey(y ~ day, dat, 0, window = 4), "must be consecutive")
  fit <- eventsurvey(y ~ day, dat, 0, window = 4, pre_periods = "observed")
  expect_s3_class(fit, "eventsurvey")
  expect_equal(fit$settings$pre_periods, "observed")
})
