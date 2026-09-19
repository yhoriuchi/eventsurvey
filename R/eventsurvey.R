#' Estimate effects of an event during survey fieldwork
#'
#' `eventsurvey()` extrapolates the pre-event outcome trend into a short
#' post-event window. It uses rolling pre-event forecasts to bound possible
#' misspecification and constructs an honest confidence interval using the
#' union--intersection method.
#'
#' The formula must contain untransformed column names for one outcome and one
#' time variable, for example `support ~ interview_date`. Time may be an
#' integer-like numeric variable, a `Date`, or a `POSIXt` value. The current
#' implementation deliberately uses a time-only linear model; transformations,
#' interactions, and covariates are not yet supported.
#'
#' Every fitted object contains automatic diagnostics. A check receives
#' `"Review"` status when calendar periods are missing, any observed period has
#' fewer than 10 responses, a within-period variance must be pooled, fewer than
#' five rolling reference windows are available, or an observed-period window
#' spans more calendar periods than requested. These descriptive flags do not
#' alter estimates or invalidate an analysis.
#'
#' @param formula A two-sided formula of the form `outcome ~ time`, using two
#'   untransformed column names.
#' @param data A data frame or tibble with one row per respondent.
#' @param event_time The event period, in the same scale and class as the time
#'   variable. Defaults to `0`, which is appropriate when time is coded
#'   relative to the event. Supply the event date or period explicitly when
#'   using an absolute time scale.
#' @param window Integer length of both the final pre-event fitting window and
#'   the post-event forecast horizon. Must be at least 2.
#' @param event_day Whether the event period is excluded so forecasting begins
#'   one period later (`"exclude"`, the default) or included in the forecast
#'   horizon (`"include"`). Exclusion is the safer default because exposure is
#'   often ambiguous for respondents interviewed during the event period.
#' @param report_event_day If `FALSE`, an included event period remains in the
#'   window-average estimate but is omitted from the printed day-specific
#'   results and plots. This argument applies only when
#'   `event_day = "include"`.
#' @param schedule How rolling fitting and forecasting windows are formed. The default,
#'   `"observed"`, forms fitting and forecasting windows from ordered periods
#'   containing responses and supports sparse survey schedules. `"consecutive"`
#'   requires every period on the time scale to be represented. In either mode,
#'   models use the actual time values rather than compressing gaps.
#' @param alpha Significance level for confidence intervals.
#' @return An object of class `eventsurvey` containing the estimate, honest and
#'   conventional intervals, day-specific results, reference forecast errors,
#'   rolling-window diagnostics, automatic design and data checks, daily
#'   summaries, and analysis settings.
#' @seealso `vignette("automatic-diagnostics", package = "eventsurvey")` for
#'   examples of every automatic check and guidance on `Review` statuses.
#' @rdname estimate_eventsurvey
#' @export
#' @examples
#' fit <- eventsurvey(y ~ day, sample_data)
#' fit
#' summary(fit)
eventsurvey <- function(formula, data, event_time = 0, window = 6L,
                        event_day = c("exclude", "include"),
                        report_event_day = TRUE, alpha = 0.05,
                        schedule = c("observed", "consecutive")) {
  cl <- match.call()
  event_day <- match.arg(event_day)
  schedule <- match.arg(schedule)
  validate_scalar(window, "window", lower = 2, integer = TRUE)
  validate_scalar(alpha, "alpha", lower = 0, upper = 1, open = TRUE)
  if (!is.logical(report_event_day) || length(report_event_day) != 1L ||
    is.na(report_event_day)) {
    stop("`report_event_day` must be TRUE or FALSE.", call. = FALSE)
  }
  validate_simple_formula(formula, data)
  n_input_rows <- nrow(data)
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  y <- stats::model.response(mf)
  time <- mf[[2L]]
  if (!is.numeric(y)) stop("The outcome must be numeric.", call. = FALSE)
  if (nrow(mf) < nrow(data)) {
    warning(nrow(data) - nrow(mf), " row(s) with missing outcome or time were omitted.",
      call. = FALSE
    )
  }

  day <- relative_period(time, event_time)
  if (any(!is.finite(y))) stop("The outcome must contain only finite values.", call. = FALSE)
  analysis_data <- data.frame(day = day, y = as.numeric(y))
  ps <- period_stats(analysis_data)
  if (!any(ps$relative_period < 0)) {
    stop("The data contain no pre-event periods.", call. = FALSE)
  }
  post <- if (event_day == "include") {
    ps$relative_period[ps$relative_period >= 0L]
  } else {
    ps$relative_period[ps$relative_period > 0L]
  }
  target <- if (schedule == "observed") {
    utils::head(post, window)
  } else if (event_day == "include") {
    0:(window - 1L)
  } else {
    seq_len(window)
  }
  if (length(target) < window) {
    stop("At least ", window, " observed forecast periods are required.",
      call. = FALSE
    )
  }
  required_pre <- seq.int(min(ps$relative_period[ps$relative_period < 0]), -1L)
  missing_pre <- setdiff(required_pre, ps$relative_period)
  forecast_start <- if (event_day == "include") 0L else 1L
  required_post <- seq.int(forecast_start, max(target))
  missing_post <- setdiff(required_post, ps$relative_period)
  if (length(missing_pre) && schedule == "consecutive") {
    stop("Pre-event periods must be consecutive. Missing relative period(s): ",
      paste(missing_pre, collapse = ", "), ". Restrict `data` to a consecutive analysis span.",
      call. = FALSE
    )
  }
  if (length(missing_post) && schedule == "consecutive") {
    stop("Every forecast period must contain at least one response. Missing relative period(s): ",
      paste(missing_post, collapse = ", "), ".",
      call. = FALSE
    )
  }
  minimum_pre <- 2L * window + if (event_day == "exclude") 1L else 0L
  if (sum(ps$relative_period < 0) < minimum_pre) {
    stop("At least ", minimum_pre, " observed pre-event periods are required for window = ",
      window, " with event_day = \"", event_day, "\".",
      call. = FALSE
    )
  }
  built <- build_design(ps, window, target, schedule, event_day)
  result <- estimate_design(built, ps, alpha)
  day_results <- estimate_days(built, ps, alpha)
  if (event_day == "include" && !report_event_day) {
    day_results <- day_results[day_results$relative_period != 0, , drop = FALSE]
  }
  settings <- list(
    window = as.integer(window), alpha = alpha,
    event_time = event_time, event_day = event_day,
    report_event_day = report_event_day,
    schedule = schedule,
    missing_pre_periods = missing_pre,
    n_missing_pre_periods = length(missing_pre),
    missing_forecast_periods = missing_post,
    n_missing_forecast_periods = length(missing_post),
    pre_event_span = max(required_pre) - min(required_pre) + 1L,
    fit_span = max(built$final_fit) - min(built$final_fit) + 1L,
    forecast_span = max(target) - min(target) + 1L,
    fit_periods = built$final_fit,
    data_level = "Respondent",
    n_input_rows = n_input_rows,
    n_used_rows = nrow(mf),
    n_omitted_rows = n_input_rows - nrow(mf),
    total_responses = sum(ps$n),
    outcome = names(mf)[1L], time = names(mf)[2L]
  )
  diagnostics <- analysis_diagnostics(ps, settings, built)
  structure(list(
    estimate = result$estimate, days = day_results,
    reference = result$reference, windows = built$windows, daily = ps,
    fitted = result$fitted, settings = settings, diagnostics = diagnostics,
    call = cl, formula = formula
  ), class = "eventsurvey")
}

#' Estimate event effects from period-level summaries
#'
#' `eventsurvey_summary()` is the sufficient-statistics counterpart to
#' [eventsurvey()]. It is useful when respondent-level observations cannot be
#' redistributed but each period's count, mean, and sample variance are
#' available. For a time-only linear model, it produces the same estimates as
#' the respondent-level function.
#'
#' @param formula A two-sided formula of the form `period_mean ~ time`, using
#'   two untransformed column names.
#' @param data A data frame or tibble with one row per observed period.
#' @param n The unquoted name of the column containing respondent counts.
#'   Defaults to a column named `n`.
#' @param variance The unquoted name of the column containing the within-period
#'   sample variance of the outcome. Defaults to a column named `variance`. It
#'   may be `NA` when `n` is one; the pooled within-period variance is then used
#'   for uncertainty calculations.
#' @inheritParams eventsurvey
#' @return An object of class `eventsurvey`; see [eventsurvey()].
#' @seealso `vignette("summary-data", package = "eventsurvey")` for a complete
#'   period-level workflow and an equivalence demonstration.
#' @export
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   daily <- sample_data |>
#'     dplyr::group_by(day) |>
#'     dplyr::summarise(
#'       mean = mean(y),
#'       n = dplyr::n(),
#'       variance = stats::var(y),
#'       .groups = "drop"
#'     )
#'   fit <- eventsurvey_summary(mean ~ day, daily)
#' }
eventsurvey_summary <- function(formula, data, n = n, variance = variance,
                                event_time = 0,
                                window = 6L,
                                event_day = c("exclude", "include"),
                                report_event_day = TRUE, alpha = 0.05,
                                schedule = c("observed", "consecutive")) {
  cl <- match.call()
  event_day <- match.arg(event_day)
  schedule <- match.arg(schedule)
  validate_scalar(window, "window", lower = 2, integer = TRUE)
  validate_scalar(alpha, "alpha", lower = 0, upper = 1, open = TRUE)
  if (!is.logical(report_event_day) || length(report_event_day) != 1L ||
    is.na(report_event_day)) {
    stop("`report_event_day` must be TRUE or FALSE.", call. = FALSE)
  }
  validate_simple_formula(formula, data)
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  means <- stats::model.response(mf)
  time <- mf[[2L]]
  n_values <- eval(substitute(n), data, parent.frame())
  variance_values <- eval(substitute(variance), data, parent.frame())
  if (length(n_values) != nrow(data) || length(variance_values) != nrow(data)) {
    stop("`n` and `variance` must each identify one column in `data`.", call. = FALSE)
  }
  day <- relative_period(time, event_time)
  ps <- summary_period_stats(day, n_values, means, variance_values)
  if (!any(ps$relative_period < 0)) stop("The data contain no pre-event periods.", call. = FALSE)
  post <- if (event_day == "include") {
    ps$relative_period[ps$relative_period >= 0L]
  } else {
    ps$relative_period[ps$relative_period > 0L]
  }
  target <- if (schedule == "observed") {
    utils::head(post, window)
  } else if (event_day == "include") {
    0:(window - 1L)
  } else {
    seq_len(window)
  }
  if (length(target) < window) {
    stop("At least ", window, " observed forecast periods are required.",
      call. = FALSE
    )
  }
  required_pre <- seq.int(min(ps$relative_period[ps$relative_period < 0]), -1L)
  missing_pre <- setdiff(required_pre, ps$relative_period)
  forecast_start <- if (event_day == "include") 0L else 1L
  required_post <- seq.int(forecast_start, max(target))
  missing_post <- setdiff(required_post, ps$relative_period)
  if (length(missing_pre) && schedule == "consecutive") {
    stop("Pre-event periods must be consecutive. Missing relative period(s): ",
      paste(missing_pre, collapse = ", "), ". Use `schedule = \"observed\"` to analyze an irregular survey schedule.",
      call. = FALSE
    )
  }
  if (length(missing_post) && schedule == "consecutive") {
    stop("Every forecast period must contain at least one response. Missing relative period(s): ",
      paste(missing_post, collapse = ", "), ".",
      call. = FALSE
    )
  }
  minimum_pre <- 2L * window + if (event_day == "exclude") 1L else 0L
  if (sum(ps$relative_period < 0) < minimum_pre) {
    stop("At least ", minimum_pre, " pre-event periods are required.", call. = FALSE)
  }
  built <- build_design(ps, window, target, schedule, event_day)
  result <- estimate_design(built, ps, alpha)
  day_results <- estimate_days(built, ps, alpha)
  if (event_day == "include" && !report_event_day) {
    day_results <- day_results[day_results$relative_period != 0, , drop = FALSE]
  }
  settings <- list(
    window = as.integer(window), alpha = alpha,
    event_time = event_time, event_day = event_day,
    report_event_day = report_event_day,
    schedule = schedule,
    missing_pre_periods = missing_pre,
    n_missing_pre_periods = length(missing_pre),
    missing_forecast_periods = missing_post,
    n_missing_forecast_periods = length(missing_post),
    pre_event_span = max(required_pre) - min(required_pre) + 1L,
    fit_span = max(built$final_fit) - min(built$final_fit) + 1L,
    forecast_span = max(target) - min(target) + 1L,
    fit_periods = built$final_fit,
    data_level = "Period summary",
    n_input_rows = nrow(data),
    n_used_rows = nrow(data),
    n_omitted_rows = 0L,
    total_responses = sum(n_values),
    outcome = names(mf)[1L], time = names(mf)[2L]
  )
  diagnostics <- analysis_diagnostics(ps, settings, built)
  structure(list(
    estimate = result$estimate, days = day_results,
    reference = result$reference, windows = built$windows, daily = ps,
    fitted = result$fitted, settings = settings, diagnostics = diagnostics,
    call = cl, formula = formula
  ), class = "eventsurvey")
}

#' Simulate an event-during-survey dataset
#'
#' Creates a reproducible respondent-level dataset with a linear no-event
#' trend and heterogeneous post-event effects. It is intended for examples,
#' teaching, and smoke tests rather than power analysis.
#'
#' @param seed Random-number seed.
#' @param pre Number of pre-event periods.
#' @param post Number of event/post-event periods.
#' @param respondents Approximate respondents per period.
#' @return A data frame with `day`, `y`, `counterfactual`, and `effect`.
#' @export
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   dat <- simulate_eventsurvey()
#'   dat |>
#'     dplyr::group_by(day) |>
#'     dplyr::summarise(mean = mean(y), .groups = "drop")
#' }
simulate_eventsurvey <- function(seed = 2026L, pre = 30L, post = 8L,
                                 respondents = 40L) {
  validate_scalar(pre, "pre", lower = 4, integer = TRUE)
  validate_scalar(post, "post", lower = 1, integer = TRUE)
  validate_scalar(respondents, "respondents", lower = 2, integer = TRUE)
  set.seed(seed)
  days <- seq.int(-pre, post - 1L)
  n <- pmax(2L, stats::rpois(length(days), respondents))
  day <- rep(days, n)
  counterfactual <- 50 + 0.18 * day
  effect_by_day <- ifelse(day < 0, 0, 2.8 * exp(-day / 4) - 0.45 * day)
  y <- counterfactual + effect_by_day + stats::rnorm(length(day), sd = 3.5)
  data.frame(
    day = day, y = y, counterfactual = counterfactual,
    effect = effect_by_day
  )
}

#' Print an event-during-survey analysis
#'
#' Displays a compact console overview containing the formula, window and
#' schedule settings, average effect, honest confidence interval, and number
#' of automatic diagnostic checks requiring review.
#'
#' @param x An object returned by [eventsurvey()] or [eventsurvey_summary()].
#' @param ... Unused.
#'
#' @return Invisibly, `x`.
#' @seealso [summary.eventsurvey()] for detailed numerical output,
#'   [plot.eventsurvey()] for figures, and [eventsurvey_report()] for a
#'   self-contained HTML report.
#' @export
#'
#' @examples
#' fit <- eventsurvey(y ~ day, sample_data)
#' print(fit)
print.eventsurvey <- function(x, ...) {
  est <- x$estimate
  cat("Event-during-survey estimate\n")
  cat("Formula: ", paste(deparse(x$formula), collapse = " "), "\n", sep = "")
  cat(
    "Window:", x$settings$window, "periods | reference forecasts:",
    est$n_reference, "\n"
  )
  cat(
    "Period schedule:", x$settings$schedule,
    "| missing pre/forecast periods:",
    x$settings$n_missing_pre_periods, "/",
    x$settings$n_missing_forecast_periods, "\n"
  )
  cat(sprintf("Average effect: %.3f\n", est$effect))
  cat(sprintf(
    "Honest %.0f%% CI: [%.3f, %.3f]\n",
    100 * (1 - x$settings$alpha), est$conf_low, est$conf_high
  ))
  n_review <- sum(x$diagnostics$status == "Review")
  cat("Diagnostics:", n_review, "of", nrow(x$diagnostics), "checks need review\n")
  invisible(x)
}

#' Summarize an event-during-survey analysis
#'
#' Collects and displays the fitted call, analysis settings, automatic
#' diagnostics, window-average estimate, period-specific estimates, and the
#' range and largest absolute value of the rolling reference forecast errors.
#'
#' @param object An object returned by [eventsurvey()] or
#'   [eventsurvey_summary()].
#' @param ... Unused.
#'
#' @return An object of class `summary.eventsurvey` containing the fitted call,
#'   settings, estimates, diagnostics, and reference-error summaries. Printing
#'   the returned object displays the complete numerical summary.
#' @seealso [print.eventsurvey()] for a compact console overview,
#'   [plot.eventsurvey()] for figures, and [eventsurvey_report()] for a
#'   self-contained HTML report.
#' @export
#'
#' @examples
#' fit <- eventsurvey(y ~ day, sample_data)
#' summary(fit)
summary.eventsurvey <- function(object, ...) {
  out <- list(
    call = object$call, settings = object$settings,
    estimate = object$estimate, days = object$days,
    diagnostics = object$diagnostics,
    reference_range = range(object$reference$error),
    max_absolute_reference_error = max(abs(object$reference$error))
  )
  class(out) <- "summary.eventsurvey"
  out
}

#' Extract the window-average event effect and confidence interval
#'
#' `coef()` returns the estimated window-average event effect. `confint()`
#' returns its honest confidence interval by default; set
#' `type = "conventional"` to deliberately request the interval that accounts
#' for sampling uncertainty but not the estimated misspecification bound.
#'
#' @param object An object returned by [eventsurvey()] or
#'   [eventsurvey_summary()].
#' @param parm The coefficient to select. It may be omitted or set to
#'   `"window_average_effect"`.
#' @param level Confidence level. By default this is the level used to fit
#'   `object`. Stored intervals cannot be returned at a different level; refit
#'   the model with another `alpha` to change it.
#' @param type Which interval to return: the default honest interval or the
#'   conventional interval.
#' @param ... Unused.
#'
#' @return `coef()` returns a named numeric vector. `confint()` returns a
#'   one-row matrix with lower and upper confidence limits.
#' @name extract.eventsurvey
#' @examples
#' fit <- eventsurvey(y ~ day, sample_data)
#' coef(fit)
#' confint(fit)
#' confint(fit, type = "conventional")
NULL

#' @rdname extract.eventsurvey
#' @export
coef.eventsurvey <- function(object, ...) {
  stats::setNames(object$estimate$effect, "window_average_effect")
}

#' @rdname extract.eventsurvey
#' @export
confint.eventsurvey <- function(object, parm, level = 1 - object$settings$alpha,
                                type = c("honest", "conventional"), ...) {
  coefficient_name <- "window_average_effect"
  if (!missing(parm) && !is.null(parm) &&
    !identical(as.character(parm), coefficient_name)) {
    stop("`parm` must be \"window_average_effect\".", call. = FALSE)
  }
  fitted_level <- 1 - object$settings$alpha
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
    !isTRUE(all.equal(level, fitted_level))) {
    stop(
      "`level` must match the fitted confidence level (",
      format(fitted_level), "). Refit with another `alpha` to change it.",
      call. = FALSE
    )
  }
  type <- match.arg(type)
  limits <- if (type == "honest") {
    c(object$estimate$conf_low, object$estimate$conf_high)
  } else {
    c(object$estimate$conventional_low, object$estimate$conventional_high)
  }
  tail_probability <- (1 - level) / 2
  column_names <- paste0(
    formatC(100 * c(tail_probability, 1 - tail_probability),
      format = "fg", digits = 6
    ),
    " %"
  )
  out <- matrix(limits, nrow = 1L, dimnames = list(coefficient_name, column_names))
  out
}

#' @export
print.summary.eventsurvey <- function(x, ...) {
  cat("Event-during-survey analysis\n\n")
  print(x$call)
  cat("\nSettings\n")
  cat("  Fit and forecast window:", x$settings$window, "periods\n")
  cat("  Event period:", x$settings$event_day, "\n")
  cat("  Period schedule:", x$settings$schedule, "\n")
  cat("  Missing pre-event periods:", x$settings$n_missing_pre_periods)
  if (x$settings$n_missing_pre_periods > 0L) {
    cat(" (", paste(x$settings$missing_pre_periods, collapse = ", "), ")", sep = "")
  }
  cat("\n")
  cat("  Missing forecast periods:", x$settings$n_missing_forecast_periods)
  if (x$settings$n_missing_forecast_periods > 0L) {
    cat(" (", paste(x$settings$missing_forecast_periods, collapse = ", "), ")", sep = "")
  }
  cat("\n")
  cat(
    "  Final fit:", x$settings$window, "observed periods spanning",
    x$settings$fit_span, "period(s) on the time scale\n"
  )
  cat(
    "  Final forecast:", x$settings$window, "observed periods spanning",
    x$settings$forecast_span, "period(s) on the time scale\n"
  )
  cat("  Confidence level:", 100 * (1 - x$settings$alpha), "%\n\n")
  cat("Automatic diagnostics\n")
  for (i in seq_len(nrow(x$diagnostics))) {
    cat(
      "  [", x$diagnostics$status[i], "] ",
      x$diagnostics$check[i], ": ", x$diagnostics$detail[i], "\n",
      sep = ""
    )
  }
  cat("\n")
  cat("Window-average effect\n")
  print(x$estimate, row.names = FALSE)
  cat("\nPeriod-specific effects\n")
  print(x$days, row.names = FALSE)
  invisible(x)
}
