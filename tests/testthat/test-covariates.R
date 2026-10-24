# What this code is about: Verify the package API and inference numerically.
# Direct component/matrix access is intentional when testing the object contract.

# Regression checks ------------------------------------------------------
covariate_test_data <- function(seed = 127, n = 40L) {
  set.seed(seed)
  day <- rep(c(-18:0, 1:6), each = n)
  x <- rnorm(length(day), mean = 0.03 * day)
  group <- factor(rep(rep(c("a", "b"), length.out = n), length(unique(day))))
  y <- 2 + 0.1 * day + 3 * x + 0.8 * (group == "b") +
    (day >= 0) * (1 + 0.15 * day) + rnorm(length(day), sd = 0.7)
  data.frame(day, x, group, y)
}

test_that("adjusted forecasts and rolling errors agree with independent lm fits", {
  dat <- covariate_test_data()
  for (schedule in c("observed", "consecutive")) {
    for (event_day in c("include", "exclude")) {
      fit <- eventsurvey(y ~ day + x + group, dat,
        window = 4,
        schedule = schedule, event_day = event_day
      )
      model <- lm(y ~ day + x + group, dat, subset = day %in% fit$settings$fit_periods)
      prediction <- vapply(fit$fitted$relative_period, function(t) {
        mean(predict(model, newdata = dat[dat$day == t, ]))
      }, numeric(1))
      expect_equal(fit$fitted$counterfactual, prediction, tolerance = 1e-10)
      expect_equal(fit$estimate$effect, mean(fit$fitted$observed - prediction))
      for (j in seq_len(nrow(fit$windows))) {
        win <- fit$windows[j, ]
        ref <- fit$reference[fit$reference$fit_start == win$fit_start, ]
        model <- lm(y ~ day + x + group, dat,
          subset = day >= win$fit_start & day <= win$fit_end
        )
        expected <- vapply(ref$forecast_period, function(t) {
          d <- dat[dat$day == t, ]
          mean(d$y - predict(model, newdata = d))
        }, numeric(1))
        expect_equal(ref$error, expected, tolerance = 1e-10)
      }
    }
  }
})

test_that("influence covariance reduces exactly to the original time-only formula", {
  dat <- covariate_test_data()
  ps <- eventsurvey:::period_stats(data.frame(day = dat$day, y = dat$y))
  built <- eventsurvey:::build_design(ps, 4, 1:4)
  # Exercise the general influence engine with just intercept and time.
  extended <- eventsurvey:::estimate_covariates(dat[c("y", "day")], dat$day, built, ps, 0.05)
  original <- eventsurvey(y ~ day, dat, window = 4)
  expect_equal(extended$estimate, original$estimate, tolerance = 1e-10)
  expect_equal(extended$days, original$days, tolerance = 1e-10)
})

test_that("joint covariance matches an independent stratified score calculation", {
  dat <- covariate_test_data(n = 20)
  fit <- eventsurvey(y ~ day + x, dat, window = 4)
  score <- function(fit_days, target) {
    train <- which(dat$day %in% fit_days)
    forecast <- which(dat$day == target)
    model <- lm(y ~ day + x, data = dat[train, ])
    z <- model.matrix(model)
    znew <- model.matrix(~ day + x, dat[forecast, ])
    zbar <- colMeans(znew)
    s <- numeric(nrow(dat))
    s[train] <- -as.vector(z %*% solve(crossprod(z), zbar)) * residuals(model)
    # Residualizing the outcome also includes the random covariate-mean term.
    s[forecast] <- (dat$y[forecast] - as.vector(znew %*% coef(model))) / length(forecast)
    for (idx in split(seq_len(nrow(dat)), dat$day)) {
      s[idx] <- (s[idx] - mean(s[idx])) * sqrt(length(idx) / (length(idx) - 1))
    }
    s
  }
  daily <- vapply(1:4, function(t) score(-4:-1, t), numeric(nrow(dat)))
  reference <- vapply(seq_len(nrow(fit$reference)), function(r) {
    start <- fit$reference$fit_start[r]
    score(start:(start + 3L), fit$reference$forecast_period[r])
  }, numeric(nrow(dat)))
  expect_equal(fit$inference$day_covariance, crossprod(daily), tolerance = 1e-10)
  expect_equal(fit$inference$reference_variance, colSums(reference^2), tolerance = 1e-10)
  expect_equal(fit$inference$reference_day_covariance, crossprod(reference, daily), tolerance = 1e-10)
  combined <- outer(rowMeans(daily), rep(1, ncol(reference))) + reference
  expected_se <- sqrt(colSums(combined^2))
  calculated_se <- sqrt(fit$estimate$std_error^2 +
    fit$inference$reference_variance + 2 * rowMeans(fit$inference$reference_day_covariance))
  expect_equal(calculated_se, expected_se, tolerance = 1e-10)
  expect_gt(max(abs(crossprod(reference, daily))), 0)
})

test_that("covariate predictions handle dates, sparse schedules, names and row order", {
  dat <- covariate_test_data()
  dat <- dat[!dat$day %in% c(-9, -2, 2), ]
  dat$date <- as.Date("2020-01-01") + dat$day
  dat$`baseline score` <- dat$x
  dat$group <- as.character(dat$group)
  numeric_fit <- eventsurvey(y ~ day + x + group, dat, window = 4)
  dated <- eventsurvey(y ~ date + `baseline score` + group, dat,
    event_time = as.Date("2020-01-01"), window = 4
  )
  shuffled <- eventsurvey(y ~ day + x + group, dat[sample(nrow(dat)), ], window = 4)
  expect_equal(dated$estimate, numeric_fit$estimate, tolerance = 1e-10)
  expect_equal(shuffled$estimate, numeric_fit$estimate, tolerance = 1e-10)
  expect_equal(numeric_fit$days$relative_period, c(1L, 3L, 4L, 5L))
  expect_error(eventsurvey(y ~ day + x, dat, window = 4, schedule = "consecutive"), "consecutive")
  expect_equal(dated$settings$covariates, c("baseline score", "group"))
})

test_that("validation catches unsupported or unidentified adjusted models", {
  dat <- covariate_test_data()
  expect_error(eventsurvey(y ~ day * x, dat), "untransformed")
  expect_error(eventsurvey(y ~ day + I(x^2), dat), "untransformed")
  expect_error(eventsurvey(y ~ ., dat), "untransformed")
  expect_error(eventsurvey(y ~ day + day, dat), "distinct")
  expect_error(eventsurvey(y ~ day + y, dat), "distinct")
  expect_error(eventsurvey(y ~ day + missing_column, dat), "not found")
  dat$constant <- 1
  dat$duplicate <- dat$x
  expect_error(eventsurvey(y ~ day + constant, dat), "constant")
  expect_error(eventsurvey(y ~ day + x + duplicate, dat), "rank deficient")
  dat$x[1] <- Inf
  expect_error(eventsurvey(y ~ day + x, dat), "finite numeric")
  dat <- covariate_test_data()
  dat$group <- factor(ifelse(dat$day < -6, "a", "b"))
  expect_error(eventsurvey(y ~ day + group, dat), "rank deficient.*final")
  dat$group <- factor(ifelse(dat$day <= -13, "a", rep(c("a", "b"), length.out = nrow(dat))))
  expect_error(eventsurvey(y ~ day + group, dat), "rank deficient.*rolling")
  dat$group <- factor(ifelse(dat$day >= 0, "new", "old"))
  expect_error(eventsurvey(y ~ day + group, dat), "rank deficient")
  singleton <- dat[dat$day != -1 | !duplicated(dat$day), ]
  expect_error(eventsurvey(y ~ day + x, singleton), "at least two complete")
  expect_error(eventsurvey_summary(y ~ day + x, dat), "respondent-level")
  tiny <- covariate_test_data(n = 2)
  expect_error(eventsurvey(y ~ day + x + group, tiny, window = 2), "residual degrees")
})

test_that("complete-case accounting and event-day display remain correct", {
  dat <- covariate_test_data()
  dat$x[1:3] <- NA_real_
  expect_warning(fit <- eventsurvey(y ~ day + x, dat, window = 4), "3 row.*covariates")
  complete <- eventsurvey(y ~ day + x, dat[complete.cases(dat), ], window = 4)
  expect_equal(fit$estimate, complete$estimate)
  expect_equal(fit$settings$n_omitted_rows, 3L)
  dat <- dat[complete.cases(dat), ]
  shown <- eventsurvey(y ~ day + x, dat, window = 4, event_day = "include")
  hidden <- eventsurvey(y ~ day + x, dat,
    window = 4,
    event_day = "include", report_event_day = FALSE
  )
  expect_equal(hidden$estimate, shown$estimate)
  expect_false(0 %in% hidden$days$relative_period)
  expect_s3_class(plot(hidden, "effects", inner_level = 0.9), "ggplot")
})

test_that("plots and extraction use adjusted results including inner intervals", {
  dat <- covariate_test_data()
  fit <- eventsurvey(y ~ day + x + group, dat, window = 4)
  for (type in c("counterfactual", "effects", "diagnostics", "coverage", "daily", "counts")) {
    expect_s3_class(ggplot2::ggplot_build(plot(fit, type)), "ggplot_built")
  }
  path <- plot(fit)$layers[[2L]]$data
  expect_equal(path$counterfactual[match(1:4, path$relative_period)], fit$fitted$counterfactual)
  inner_fit <- eventsurvey(y ~ day + x + group, dat, window = 4, alpha = 0.1)
  inner <- plot(fit, "effects", inner_level = 0.9)$layers[[4L]]$data
  expect_equal(inner$conf_low, inner_fit$days$conf_low)
  expect_equal(inner$conf_high, inner_fit$days$conf_high)
  expect_equal(unname(coef(fit)), fit$estimate$effect)
  expect_equal(unname(confint(fit)[1, ]), c(fit$estimate$conf_low, fit$estimate$conf_high))
  expect_match(paste(capture.output(summary(fit)), collapse = "\n"), "Covariates: x, group")
})

test_that("post-event outcomes cannot change pre-event fits or reference errors", {
  dat <- covariate_test_data()
  before <- eventsurvey(y ~ day + x, dat, window = 4)
  dat$y[dat$day > 0] <- dat$y[dat$day > 0] + 10
  after <- eventsurvey(y ~ day + x, dat, window = 4)
  expect_equal(after$fitted$counterfactual, before$fitted$counterfactual)
  expect_equal(after$reference, before$reference)
  expect_equal(after$estimate$effect, before$estimate$effect + 10)
  expect_equal(after$estimate$std_error, before$estimate$std_error)
})

test_that("exact linear data recover known effects with unequal period sizes", {
  set.seed(18)
  day <- rep(c(-18:-1, 1:4), times = c(sample(12:50, 18), 10, 20, 40, 80))
  dat <- data.frame(day, x = rnorm(length(day)))
  dat$y <- 5 + 0.2 * day + 3 * dat$x + ifelse(day > 0, 1 + day / 2, 0)
  fit <- eventsurvey(y ~ day + x, dat, window = 4)
  expect_equal(fit$days$effect, 1 + (1:4) / 2, tolerance = 1e-10)
  expect_equal(fit$estimate$effect, 2.25, tolerance = 1e-10)
  expect_lt(max(abs(fit$reference$error)), 1e-10)
  expect_equal(fit$settings$total_responses, nrow(dat))
})

test_that("categorical coding, units and irrelevant periods do not change results", {
  dat <- covariate_test_data()
  first <- eventsurvey(y ~ day + x + group, dat, window = 4)
  dat$group <- ordered(dat$group)
  ordered_fit <- eventsurvey(y ~ day + x + group, dat, window = 4)
  expect_equal(ordered_fit$estimate, first$estimate, tolerance = 1e-10)
  dat$group <- dat$group == "b"
  dat$x <- 1e6 + 1e3 * dat$x
  units_fit <- eventsurvey(y ~ day + x + group, dat, window = 4)
  expect_equal(units_fit$estimate, first$estimate, tolerance = 1e-9)
  dat$group <- as.character(dat$group)
  dat$group[dat$day == 0 | dat$day > 4] <- "unused level"
  outside <- eventsurvey(y ~ day + x + group, dat, window = 4)
  expect_equal(outside$estimate, units_fit$estimate, tolerance = 1e-9)
})

test_that("adjusted daily aggregation retains all sampling covariance", {
  dat <- covariate_test_data(n = 25)
  fit <- eventsurvey(y ~ day + x, dat, window = 4)
  # Independent lm score implementation, in the original covariate units.
  score <- function(fit_days, target) {
    train <- which(dat$day %in% fit_days)
    forecast <- which(dat$day == target)
    model <- lm(y ~ day + x, dat[train, ])
    x <- model.matrix(model)
    z <- model.matrix(~ day + x, dat[forecast, ])
    out <- numeric(nrow(dat))
    out[train] <- -as.vector(x %*% solve(crossprod(x), colMeans(z))) * residuals(model)
    out[forecast] <- (dat$y[forecast] - as.vector(z %*% coef(model))) / length(forecast)
    for (i in split(seq_len(nrow(dat)), dat$day)) {
      out[i] <- (out[i] - mean(out[i])) * sqrt(length(i) / (length(i) - 1))
    }
    out
  }
  daily <- vapply(1:4, function(t) score(-4:-1, t), numeric(nrow(dat)))
  reference <- vapply(seq_len(nrow(fit$reference)), function(j) {
    start <- fit$reference$fit_start[j]
    score(start:(start + 3L), fit$reference$forecast_period[j])
  }, numeric(nrow(dat)))
  expect_equal(fit$inference$day_covariance, unname(crossprod(daily)), tolerance = 1e-10)
  expect_equal(fit$estimate$std_error, sqrt(sum(rowMeans(daily)^2)), tolerance = 1e-10)
  expect_gt(abs(fit$estimate$std_error^2 - sum(colSums(daily^2)) / 16), 1e-5)
  components <- t(vapply(1:4, function(h) {
    i <- which(fit$reference$horizon == h + 1)
    combined <- cbind(
      sweep(reference[, i, drop = FALSE], 1, daily[, h], "+"),
      sweep(-reference[, i, drop = FALSE], 1, daily[, h], "+")
    )
    point <- fit$days$effect[h] + c(fit$reference$error[i], -fit$reference$error[i])
    se <- sqrt(colSums(combined^2))
    c(min(point - qnorm(1 - 0.05 / 8) * se), max(point + qnorm(1 - 0.05 / 8) * se))
  }, numeric(2)))
  expect_equal(unname(confint(fit)[1, ]), colMeans(components), tolerance = 1e-10)
  expect_equal(fit$estimate$bias_bound, mean(fit$days$bias_bound))
})

test_that("both covariance engines agree on sparse data", {
  dat <- covariate_test_data()
  dat <- dat[!dat$day %in% c(-9, -1, 2), ]
  ps <- eventsurvey:::period_stats(data.frame(day = dat$day, y = dat$y))
  for (event_day in c("exclude", "include")) {
    target <- if (event_day == "exclude") c(1L, 3L, 4L, 5L) else c(0L, 1L, 3L, 4L)
    built <- eventsurvey:::build_design(ps, 4, target,
      event_day = event_day
    )
    influence <- eventsurvey:::estimate_covariates(dat[c("y", "day")], dat$day, built, ps, 0.05)
    analytic <- eventsurvey(y ~ day, dat,
      window = 4,
      event_day = event_day
    )
    expect_equal(influence$estimate, analytic$estimate, tolerance = 1e-10)
    expect_equal(influence$days, analytic$days, tolerance = 1e-10)
    expect_equal(influence$average_components, analytic$average_components, tolerance = 1e-10)
  }
})
