validate_scalar <- function(x, name, lower = -Inf, upper = Inf,
                            integer = FALSE, open = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x)
  if (integer) ok <- ok && x == as.integer(x)
  if (open) ok <- ok && x > lower && x < upper else ok <- ok && x >= lower && x <= upper
  if (!ok) stop("`", name, "` has an invalid value.", call. = FALSE)
  invisible(x)
}

relative_period <- function(time, event_time) {
  if (inherits(time, "POSIXt")) time <- as.Date(time)
  if (inherits(event_time, "POSIXt")) event_time <- as.Date(event_time)
  if (inherits(time, "Date")) {
    if (!inherits(event_time, "Date") || length(event_time) != 1L) {
      stop("For a Date time variable, `event_time` must be one Date.", call. = FALSE)
    }
    out <- as.numeric(time - event_time)
  } else if (is.numeric(time)) {
    if (!is.numeric(event_time) || length(event_time) != 1L || !is.finite(event_time)) {
      stop("For a numeric time variable, `event_time` must be one finite number.", call. = FALSE)
    }
    out <- time - event_time
  } else {
    stop("Time must be integer-like numeric, Date, or POSIXt.", call. = FALSE)
  }
  if (any(!is.finite(out)) || any(abs(out - round(out)) > sqrt(.Machine$double.eps))) {
    stop("Time must identify whole, equally spaced periods (normally calendar days).",
         call. = FALSE)
  }
  as.integer(round(out))
}

period_stats <- function(data) {
  split_y <- split(data$y, data$day)
  days <- as.integer(names(split_y))
  ord <- order(days)
  days <- days[ord]
  split_y <- split_y[ord]
  n <- vapply(split_y, length, integer(1))
  ybar <- vapply(split_y, mean, numeric(1))
  s2 <- vapply(split_y, function(z) if (length(z) > 1L) stats::var(z) else NA_real_, numeric(1))
  ok <- n > 1L & is.finite(s2)
  if (!any(ok)) stop("At least one period must contain two or more responses.", call. = FALSE)
  pooled <- sum((n[ok] - 1) * s2[ok]) / sum(n[ok] - 1)
  sigma2 <- ifelse(ok, s2, pooled)
  data.frame(relative_period = days, n = n, mean = ybar,
             variance = sigma2, mean_variance = sigma2 / n)
}

summary_period_stats <- function(day, n, mean, variance) {
  if (!is.numeric(n) || !is.numeric(mean) || !is.numeric(variance)) {
    stop("`n`, the period mean, and `variance` must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(day)) || any(day != as.integer(day))) {
    stop("Summary time values must identify whole, equally spaced periods.", call. = FALSE)
  }
  if (any(!is.finite(n)) || any(n < 1) || any(n != as.integer(n))) {
    stop("`n` must contain positive integer counts.", call. = FALSE)
  }
  if (any(!is.finite(mean))) {
    stop("Period means must contain only finite values.", call. = FALSE)
  }
  if (any(is.finite(variance) & variance < 0)) {
    stop("`variance` cannot contain negative values.", call. = FALSE)
  }
  if (anyDuplicated(day)) {
    stop("Summary data must contain exactly one row per period.", call. = FALSE)
  }
  ok <- n > 1L & is.finite(variance)
  if (!any(ok)) {
    stop("At least one period must have a finite variance and two or more responses.",
         call. = FALSE)
  }
  invalid <- !is.finite(variance) & n > 1L
  if (any(invalid)) {
    stop("Missing variances are permitted only for periods with one response.",
         call. = FALSE)
  }
  pooled <- sum((n[ok] - 1) * variance[ok]) / sum(n[ok] - 1)
  sigma2 <- ifelse(ok, variance, pooled)
  out <- data.frame(relative_period = as.integer(day), n = as.integer(n),
                    mean = as.numeric(mean), variance = sigma2,
                    mean_variance = sigma2 / n)
  out[order(out$relative_period), , drop = FALSE]
}

prediction_weights <- function(fit_days, fit_n, target_day) {
  design <- cbind(1, fit_days)
  cross <- t(design * fit_n)
  as.numeric(c(1, target_day) %*% solve(cross %*% design, cross))
}

build_design <- function(ps, window, target,
                         pre_periods = c("consecutive", "observed")) {
  pre_periods <- match.arg(pre_periods)
  days <- ps$relative_period
  pre <- days[days < 0]
  index <- function(x) match(x, days)
  final_fit <- if (pre_periods == "consecutive") -window:-1L else utils::tail(pre, window)
  if (anyNA(index(final_fit))) stop("The final fitting window has missing periods.", call. = FALSE)
  m <- nrow(ps)
  rows <- list()
  map <- list()
  k <- 0L
  if (pre_periods == "consecutive") {
    forecast_offsets <- target - max(final_fit)
    last_start <- -window - max(forecast_offsets)
    starts <- seq.int(min(pre), last_start)
    fit_windows <- lapply(starts, function(start) seq.int(start, length.out = window))
    forecast_windows <- lapply(fit_windows, function(fit) max(fit) + forecast_offsets)
  } else {
    n_windows <- length(pre) - 2L * window + 1L
    starts <- seq_len(n_windows)
    fit_windows <- lapply(starts, function(j) pre[j:(j + window - 1L)])
    forecast_windows <- lapply(starts, function(j) pre[(j + window):(j + 2L * window - 1L)])
  }
  for (j in seq_along(fit_windows)) {
    fit <- fit_windows[[j]]
    forecast <- forecast_windows[[j]]
    fit_n <- ps$n[index(fit)]
    for (target_day in forecast) {
      weights <- prediction_weights(fit, fit_n, target_day)
      row <- numeric(m)
      row[index(target_day)] <- 1
      row[index(fit)] <- row[index(fit)] - weights
      k <- k + 1L
      rows[[k]] <- row
      map[[k]] <- c(fit_start = fit[1L], forecast_period = target_day,
                    horizon = target_day - max(fit))
    }
  }
  reference_rows <- do.call(rbind, rows)
  reference_map <- as.data.frame(do.call(rbind, map))
  fit_n <- ps$n[index(final_fit)]
  effect_rows <- counterfactual_rows <- matrix(0, nrow = length(target), ncol = m)
  for (j in seq_along(target)) {
    weights <- prediction_weights(final_fit, fit_n, target[j])
    counterfactual_rows[j, index(final_fit)] <- weights
    effect_rows[j, ] <- -counterfactual_rows[j, ]
    effect_rows[j, index(target[j])] <- effect_rows[j, index(target[j])] + 1
  }
  list(reference_rows = reference_rows, reference_map = reference_map,
       effect_rows = effect_rows, counterfactual_rows = counterfactual_rows,
       average_row = colMeans(effect_rows), final_fit = final_fit,
       target = target, n_windows = length(fit_windows))
}

honest_interval <- function(point_row, point, reference_rows, reference_errors,
                            variance, alpha) {
  z <- stats::qnorm(1 - alpha / 2)
  low <- Inf
  high <- -Inf
  for (r in seq_len(nrow(reference_rows))) {
    for (sign in c(-1, 1)) {
      combined <- point_row + sign * reference_rows[r, ]
      se <- sqrt(sum(combined^2 * variance))
      adjusted <- point + sign * reference_errors[r]
      low <- min(low, adjusted - z * se)
      high <- max(high, adjusted + z * se)
    }
  }
  c(low = low, high = high)
}

estimate_design <- function(built, ps, alpha) {
  ybar <- ps$mean
  variance <- ps$mean_variance
  reference_errors <- as.numeric(built$reference_rows %*% ybar)
  effect <- sum(built$average_row * ybar)
  se <- sqrt(sum(built$average_row^2 * variance))
  z <- stats::qnorm(1 - alpha / 2)
  ci <- honest_interval(built$average_row, effect, built$reference_rows,
                        reference_errors, variance, alpha)
  estimate <- data.frame(effect = effect,
                         std_error = se,
                         conventional_low = effect - z * se,
                         conventional_high = effect + z * se,
                         bias_bound = max(abs(reference_errors)),
                         conf_low = unname(ci[1L]), conf_high = unname(ci[2L]),
                         n_windows = built$n_windows,
                         n_reference = length(reference_errors))
  reference <- cbind(built$reference_map, error = reference_errors)
  fitted <- data.frame(relative_period = built$target,
                       observed = ybar[match(built$target, ps$relative_period)],
                       counterfactual = as.numeric(built$counterfactual_rows %*% ybar))
  list(estimate = estimate, reference = reference, fitted = fitted)
}

estimate_days <- function(built, ps, alpha) {
  ybar <- ps$mean
  variance <- ps$mean_variance
  reference_errors <- as.numeric(built$reference_rows %*% ybar)
  effects <- as.numeric(built$effect_rows %*% ybar)
  intervals <- t(vapply(seq_along(effects), function(j) {
    honest_interval(built$effect_rows[j, ], effects[j], built$reference_rows,
                    reference_errors, variance, alpha)
  }, numeric(2)))
  data.frame(relative_period = built$target,
             observed = ybar[match(built$target, ps$relative_period)],
             counterfactual = as.numeric(built$counterfactual_rows %*% ybar),
             effect = effects, conf_low = intervals[, 1L], conf_high = intervals[, 2L])
}
