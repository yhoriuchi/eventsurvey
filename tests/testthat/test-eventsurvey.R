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

  for (type in c("counterfactual", "effects", "diagnostics", "daily")) {
    expect_s3_class(plot(fit, type = type), "ggplot")
  }
})
