test_that("sample_data is ready for a two-line first analysis", {
  expect_s3_class(sample_data, "data.frame")
  expect_named(sample_data, c("day", "y"))
  expect_type(sample_data$y, "integer")
  expect_setequal(unique(sample_data$y), 1:5)

  simulated <- simulate_eventsurvey(seed = 10)
  expected <- data.frame(
    day = simulated$day,
    y = as.integer(cut(
      simulated$y,
      breaks = c(-Inf, 45, 48, 51, 54, Inf),
      labels = FALSE
    ))
  )
  expect_equal(sample_data, expected)

  fit <- eventsurvey(y ~ day, sample_data)
  expect_s3_class(fit, "eventsurvey")
})

test_that("the core estimator returns coherent results", {
  dat <- simulate_eventsurvey(seed = 10, pre = 24, post = 7, respondents = 30)
  fit <- eventsurvey(y ~ day, dat, event_time = 0, window = 6)

  expect_s3_class(fit, "eventsurvey")
  expect_equal(nrow(fit$days), 6)
  expect_equal(fit$days$relative_period, 1:6)
  expect_equal(fit$estimate$n_windows, 12)
  expect_equal(fit$estimate$n_reference, 72)
  expect_lt(fit$estimate$conf_low, fit$estimate$effect)
  expect_gt(fit$estimate$conf_high, fit$estimate$effect)
  expect_gte(fit$estimate$conf_low, -Inf)
  expect_equal(max(abs(fit$reference$error)), fit$estimate$bias_bound)
})

test_that("compact printing omits the formula environment", {
  fit <- eventsurvey(y ~ day, sample_data)
  output <- capture.output(print(fit))

  expect_true(any(output == "Formula: y ~ day"))
  expect_false(any(grepl("<environment:", output, fixed = TRUE)))
})

test_that("core estimators require a simple outcome-by-time formula", {
  dat <- simulate_eventsurvey(seed = 23, pre = 20, post = 7)
  expect_error(
    eventsurvey(log(y) ~ day, dat),
    "untransformed column names"
  )
  expect_error(
    eventsurvey(y ~ I(day^2), dat),
    "untransformed column names"
  )
  expect_error(
    eventsurvey(y ~ day + counterfactual, dat),
    "untransformed column names"
  )
  expect_error(
    eventsurvey(not_a_variable ~ day, dat),
    "not found"
  )

  split_y <- split(dat$y, dat$day)
  daily <- data.frame(
    day = as.integer(names(split_y)),
    mean = vapply(split_y, mean, numeric(1)),
    n = vapply(split_y, length, integer(1)),
    variance = vapply(split_y, stats::var, numeric(1))
  )
  expect_error(
    eventsurvey_summary(mean ~ I(day^2), daily),
    "untransformed column names"
  )
  expect_error(
    eventsurvey_summary(log(mean) ~ day, daily),
    "untransformed column names"
  )
})

test_that("fitted objects record input and represented response counts", {
  dat <- simulate_eventsurvey(seed = 24, pre = 20, post = 7)
  dat$y[1L] <- NA_real_
  expect_warning(
    fit <- eventsurvey(y ~ day, dat),
    "1 row\\(s\\).*omitted"
  )
  expect_equal(fit$settings$data_level, "Respondent")
  expect_equal(fit$settings$n_input_rows, nrow(dat))
  expect_equal(fit$settings$n_used_rows, nrow(dat) - 1L)
  expect_equal(fit$settings$n_omitted_rows, 1L)
  expect_equal(fit$settings$total_responses, nrow(dat) - 1L)

  complete <- dat[!is.na(dat$y), ]
  split_y <- split(complete$y, complete$day)
  daily <- data.frame(
    day = as.integer(names(split_y)),
    mean = vapply(split_y, mean, numeric(1)),
    n = vapply(split_y, length, integer(1)),
    variance = vapply(split_y, stats::var, numeric(1))
  )
  summary_fit <- eventsurvey_summary(mean ~ day, daily)
  expect_equal(summary_fit$settings$data_level, "Period summary")
  expect_equal(summary_fit$settings$n_input_rows, nrow(daily))
  expect_equal(summary_fit$settings$n_used_rows, nrow(daily))
  expect_equal(summary_fit$settings$n_omitted_rows, 0L)
  expect_equal(summary_fit$settings$total_responses, sum(daily$n))
})

test_that("standard extractors return the fitted effect and intervals", {
  fit <- eventsurvey(y ~ day, sample_data)
  expect_equal(
    coef(fit),
    stats::setNames(fit$estimate$effect, "window_average_effect")
  )
  expect_equal(
    unname(confint(fit)),
    matrix(c(fit$estimate$conf_low, fit$estimate$conf_high), nrow = 1L)
  )
  expect_equal(
    unname(confint(fit, type = "conventional")),
    matrix(
      c(fit$estimate$conventional_low, fit$estimate$conventional_high),
      nrow = 1L
    )
  )
  expect_error(confint(fit, parm = "other"), "parm")
  expect_error(confint(fit, level = 0.90), "fitted confidence level")
})

test_that("core estimators exclude the event period by default", {
  dat <- simulate_eventsurvey(seed = 19, pre = 20, post = 7)
  respondent_default <- eventsurvey(y ~ day, dat)
  respondent_explicit <- eventsurvey(
    y ~ day,
    dat,
    event_time = 0,
    window = 6,
    event_day = "exclude",
    report_event_day = TRUE,
    alpha = 0.05,
    schedule = "observed"
  )

  split_y <- split(dat$y, dat$day)
  daily <- data.frame(
    day = as.integer(names(split_y)),
    mean = vapply(split_y, mean, numeric(1)),
    n = vapply(split_y, length, integer(1)),
    variance = vapply(split_y, stats::var, numeric(1))
  )
  summary_default <- eventsurvey_summary(mean ~ day, daily)
  summary_explicit <- eventsurvey_summary(
    mean ~ day,
    daily,
    n = n,
    variance = variance,
    event_time = 0,
    window = 6,
    event_day = "exclude",
    report_event_day = TRUE,
    alpha = 0.05,
    schedule = "observed"
  )

  expect_equal(respondent_default$estimate, respondent_explicit$estimate)
  expect_equal(respondent_default$settings, respondent_explicit$settings)
  expect_equal(summary_default$estimate, summary_explicit$estimate)
  expect_equal(summary_default$settings, summary_explicit$settings)
  expect_equal(respondent_default$settings$event_day, "exclude")
  expect_equal(summary_default$settings$event_day, "exclude")
})

test_that("linear period means are extrapolated exactly", {
  dat <- expand.grid(day = -13:6, respondent = 1:4)
  dat$y <- 3 + 0.5 * dat$day + c(-1, -0.5, 0.5, 1)[dat$respondent]
  fit <- eventsurvey(y ~ day, dat, event_time = 0, window = 6)

  expect_equal(fit$estimate$effect, 0, tolerance = 1e-10)
  expect_equal(fit$estimate$bias_bound, 0, tolerance = 1e-10)
  expect_equal(fit$fitted$observed, fit$fitted$counterfactual, tolerance = 1e-10)
})

test_that("Date inputs and excluded event periods work", {
  dat <- simulate_eventsurvey(seed = 11, pre = 20, post = 8)
  dat$date <- as.Date("2026-01-15") + dat$day
  fit <- eventsurvey(y ~ date, dat,
    event_time = as.Date("2026-01-15"),
    window = 5, event_day = "exclude"
  )

  expect_equal(fit$days$relative_period, 1:5)
  expect_equal(sort(unique(fit$reference$horizon)), 2:6)
})

test_that("the event period can be forecast but hidden from day results", {
  dat <- simulate_eventsurvey(seed = 12, pre = 20, post = 8)
  fit <- eventsurvey(y ~ day, dat,
    event_time = 0, window = 5,
    event_day = "include",
    report_event_day = FALSE
  )

  expect_false(0 %in% fit$days$relative_period)
  expect_true(0 %in% fit$fitted$relative_period)
})

test_that("calendar gaps are diagnosed and strict mode remains available", {
  dat <- simulate_eventsurvey(seed = 13, pre = 12, post = 6)
  gapped <- dat[dat$day != -3, ]
  fit <- eventsurvey(y ~ day, gapped, 0, window = 4)
  expect_equal(fit$settings$schedule, "observed")
  expect_equal(fit$settings$missing_pre_periods, -3L)
  expect_equal(fit$settings$n_missing_pre_periods, 1L)
  expect_equal(fit$settings$pre_event_span, 12L)
  expect_equal(fit$settings$fit_span, 5L)
  expect_equal(nrow(fit$windows), fit$estimate$n_windows)
  expect_true(any(fit$windows$forecast_span > 4L))
  expect_match(
    paste(capture.output(fit), collapse = "\n"),
    "missing pre/forecast periods: 1 / 0"
  )
  expect_match(
    paste(capture.output(summary(fit)), collapse = "\n"),
    "Missing pre-event periods: 1 \\(-3\\)"
  )
  expect_error(
    eventsurvey(y ~ day, gapped, 0,
      window = 4,
      schedule = "consecutive"
    ),
    "must be consecutive"
  )
})

test_that("insufficient history fails clearly", {
  dat <- simulate_eventsurvey(seed = 13, pre = 12, post = 6)
  expect_error(
    eventsurvey(
      y ~ day, dat[dat$day >= -7, ], 0, window = 4,
      event_day = "include"
    ),
    "At least 8"
  )
  expect_error(
    eventsurvey(y ~ day, dat[dat$day >= -8, ], 0, window = 4),
    "At least 9"
  )
  expect_error(
    eventsurvey(y ~ day, dat[dat$day >= 0, ], 0, window = 4),
    "no pre-event"
  )
})

test_that("sparse forecast dates are diagnosed and strict mode rejects them", {
  dat <- simulate_eventsurvey(seed = 18, pre = 12, post = 6)
  gapped <- dat[dat$day != 2, ]
  fit <- eventsurvey(y ~ day, gapped, 0, window = 4)

  expect_equal(fit$days$relative_period, c(1L, 3L, 4L, 5L))
  expect_equal(fit$settings$missing_forecast_periods, 2L)
  expect_equal(fit$settings$n_missing_forecast_periods, 1L)
  expect_equal(fit$settings$forecast_span, 5L)
  expect_match(
    paste(capture.output(summary(fit)), collapse = "\n"),
    "Missing forecast periods: 1 \\(2\\)"
  )
  expect_error(
    eventsurvey(y ~ day, gapped, 0,
      window = 4,
      schedule = "consecutive"
    ),
    "Every forecast period"
  )
})

test_that("plot methods return ggplot objects", {
  dat <- simulate_eventsurvey(seed = 14, pre = 16, post = 6)
  fit <- eventsurvey(y ~ day, dat, 0, window = 4)

  for (type in c(
    "counterfactual", "effects", "diagnostics", "coverage", "daily", "counts"
  )) {
    expect_s3_class(plot(fit, type = type), "ggplot")
  }
  counterfactual_plot <- plot(fit, type = "counterfactual")
  expected_periods <- sort(unique(c(0,
    fit$settings$fit_periods,
    fit$fitted$relative_period
  )))
  expect_equal(
    counterfactual_plot$scales$get_scales("x")$breaks,
    expected_periods
  )
  coverage_plot <- plot(fit, type = "coverage")
  expect_length(coverage_plot$layers, 4L)
  expect_s3_class(coverage_plot$layers[[2L]]$geom, "GeomRect")

  diagnostic_plot <- plot(fit, type = "diagnostics")
  expect_true(0 %in% diagnostic_plot$scales$get_scales("x")$breaks)
  time_plots <- lapply(
    c("counterfactual", "effects", "diagnostics", "coverage", "daily", "counts"),
    function(type) ggplot2::ggplot_build(plot(fit, type = type))$data[[1L]]
  )
  expect_true(all(vapply(time_plots, function(layer) {
    isTRUE(all.equal(unique(layer$xintercept), 0)) &&
      identical(unique(layer$linetype), "dashed") &&
      identical(unique(layer$colour), "#337AB7") &&
      isTRUE(all.equal(unique(layer$linewidth), 0.45))
  }, logical(1))))

  expect_s3_class(plot(fit, type = "effects", inner_level = 0.90), "ggplot")
  expect_error(
    plot(fit, type = "effects", inner_level = 0.99),
    "between zero and the fitted confidence level"
  )
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
    n = n, variance = variance, event_time = 0, window = 5
  )

  expect_equal(summarized$estimate, individual$estimate, tolerance = 1e-12)
  expect_equal(summarized$days, individual$days, tolerance = 1e-12)
})

test_that("both estimation interfaces accept tibbles", {
  skip_if_not_installed("dplyr")

  respondents <- dplyr::as_tibble(sample_data)
  daily <- respondents |>
    dplyr::group_by(day) |>
    dplyr::summarise(
      mean = mean(y),
      n = dplyr::n(),
      variance = stats::var(y),
      .groups = "drop"
    )

  individual <- eventsurvey(y ~ day, respondents)
  summarized <- eventsurvey_summary(mean ~ day, daily)

  expect_s3_class(respondents, "tbl_df")
  expect_s3_class(daily, "tbl_df")
  expect_equal(summarized$estimate, individual$estimate, tolerance = 1e-12)
  expect_equal(summarized$days, individual$days, tolerance = 1e-12)
})

test_that("published application summaries reproduce manuscript tables", {
  data("published_examples", package = "eventsurvey")
  privacy <- subset(published_examples, grepl("privacy", outcome))
  fit_p <- eventsurvey_summary(mean ~ day, privacy,
    n = n, variance = variance, event_time = 0, window = 5,
    event_day = "include", report_event_day = FALSE,
    schedule = "observed"
  )
  expect_equal(round(fit_p$days$effect, 3), c(-0.115, -0.050, -0.512, -0.050))
  expect_equal(round(fit_p$estimate$bias_bound, 3), 0.780)
  expect_equal(fit_p$estimate$n_windows, 7)

  procedural <- subset(published_examples, grepl("procedural", outcome))
  fit_r <- eventsurvey_summary(mean ~ day, procedural,
    n = n, variance = variance, event_time = 0, window = 7,
    event_day = "include", report_event_day = FALSE,
    schedule = "observed"
  )
  expect_equal(
    round(fit_r$days$effect, 3),
    c(-0.079, -0.046, -0.444, -0.033, -0.017, -0.070)
  )
  expect_equal(round(fit_r$estimate$bias_bound, 3), 0.402)
  expect_equal(fit_r$estimate$n_windows, 4)
})

test_that("observed-period mode is the default when pre-event dates are empty", {
  dat <- simulate_eventsurvey(seed = 16, pre = 16, post = 5)
  dat <- dat[dat$day != -12, ]
  fit <- eventsurvey(y ~ day, dat, 0, window = 4)
  expect_s3_class(fit, "eventsurvey")
  expect_equal(fit$settings$schedule, "observed")
  expect_error(eventsurvey(y ~ day, dat, 0,
    window = 4,
    schedule = "consecutive"
  ), "must be consecutive")
})

test_that("observed mode mirrors an excluded event period in reference windows", {
  dat <- simulate_eventsurvey(seed = 17, pre = 17, post = 6)
  fit <- eventsurvey(y ~ day, dat, 0,
    window = 4,
    event_day = "exclude"
  )

  expect_equal(fit$estimate$n_windows, 9L)
  expect_true(all(fit$windows$forecast_start - fit$windows$fit_end == 2L))
})

test_that("automatic diagnostics identify design and data issues", {
  complete <- simulate_eventsurvey(seed = 22, pre = 20, post = 7)
  complete_fit <- eventsurvey(y ~ day, complete)
  expect_equal(nrow(complete_fit$diagnostics), 5L)
  expect_true(all(complete_fit$diagnostics$status == "OK"))

  sparse <- complete[complete$day != -3, ]
  sparse_fit <- eventsurvey(y ~ day, sparse)
  review_checks <- sparse_fit$diagnostics$check[
    sparse_fit$diagnostics$status == "Review"
  ]
  expect_contains(review_checks, "Calendar coverage")
  expect_contains(review_checks, "Elapsed window spans")

  limited_fit <- eventsurvey(y ~ day, complete[complete$day >= -13, ])
  expect_equal(limited_fit$estimate$n_windows, 1L)
  expect_equal(
    limited_fit$diagnostics$status[
      limited_fit$diagnostics$check == "Reference windows"
    ],
    "Review"
  )

  split_y <- split(complete$y, complete$day)
  daily <- data.frame(
    day = as.integer(names(split_y)),
    mean = vapply(split_y, mean, numeric(1)),
    n = vapply(split_y, length, integer(1)),
    variance = vapply(split_y, stats::var, numeric(1))
  )
  daily$n[daily$day == -2] <- 1L
  daily$variance[daily$day == -2] <- NA_real_
  summary_fit <- eventsurvey_summary(mean ~ day, daily)
  review_checks <- summary_fit$diagnostics$check[
    summary_fit$diagnostics$status == "Review"
  ]
  expect_contains(review_checks, "Responses per period")
  expect_contains(review_checks, "Variance information")
  expect_true(summary_fit$daily$variance_imputed[summary_fit$daily$relative_period == -2])

  printed <- paste(capture.output(summary(sparse_fit)), collapse = "\n")
  expect_match(printed, "Automatic diagnostics")
  expect_match(printed, "[Review] Calendar coverage", fixed = TRUE)
})
