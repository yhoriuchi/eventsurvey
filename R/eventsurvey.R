#' Estimate effects of an event during survey fieldwork
#'
#' `eventsurvey()` extrapolates the pre-event outcome trend into a short
#' post-event window. It uses rolling pre-event forecasts to bound possible
#' misspecification and constructs an honest confidence interval using the
#' union--intersection method.
#'
#' The formula must contain one outcome and one time variable, for example
#' `support ~ interview_date`. Time may be an integer-like numeric variable,
#' a `Date`, or a `POSIXt` value. The current implementation deliberately uses
#' a time-only linear model; covariates are not yet supported.
#'
#' @param formula A two-sided formula of the form `outcome ~ time`.
#' @param data A data frame with one row per respondent.
#' @param event_time The event period, in the same scale and class as the time
#'   variable.
#' @param window Integer length of both the final pre-event fitting window and
#'   the post-event forecast horizon. Must be at least 2.
#' @param event_day Whether the event period is included in the forecast
#'   horizon (`"include"`) or skipped so forecasting begins one period later
#'   (`"exclude"`).
#' @param report_event_day If `FALSE`, an included event period remains in the
#'   window-average estimate but is omitted from the printed day-specific
#'   results and plots. This is useful when exposure during the event period is
#'   ambiguous.
#' @param pre_periods How rolling pre-event windows are formed. The default,
#'   `"observed"`, forms fitting and forecasting windows from ordered periods
#'   containing responses and supports sparse survey schedules. `"consecutive"`
#'   requires every period on the time scale to be represented. In either mode,
#'   models use the actual time values rather than compressing gaps.
#' @param alpha Significance level for confidence intervals.
#' @return An object of class `eventsurvey` containing the estimate, honest and
#'   conventional intervals, day-specific results, reference forecast errors,
#'   rolling-window diagnostics, daily summaries, and analysis settings.
#' @rdname estimate_eventsurvey
#' @export
#' @examples
#' dat <- simulate_eventsurvey(seed = 10)
#' fit <- eventsurvey(y ~ day, dat, event_time = 0, window = 6)
#' fit
#' summary(fit)
eventsurvey <- function(formula, data, event_time, window = 6L,
                        event_day = c("include", "exclude"),
                        report_event_day = TRUE, alpha = 0.05,
                        pre_periods = c("observed", "consecutive")) {
  cl <- match.call()
  event_day <- match.arg(event_day)
  pre_periods <- match.arg(pre_periods)
  validate_scalar(window, "window", lower = 2, integer = TRUE)
  validate_scalar(alpha, "alpha", lower = 0, upper = 1, open = TRUE)
  if (!is.logical(report_event_day) || length(report_event_day) != 1L ||
    is.na(report_event_day)) {
    stop("`report_event_day` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("`formula` must have the form outcome ~ time.", call. = FALSE)
  }
  terms_obj <- stats::terms(formula, data = data)
  if (attr(terms_obj, "intercept") != 1L || length(attr(terms_obj, "term.labels")) != 1L) {
    stop("The current implementation requires exactly one time variable: outcome ~ time.",
      call. = FALSE
    )
  }
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  if (ncol(mf) != 2L) {
    stop("The current implementation requires exactly one time variable: outcome ~ time.",
      call. = FALSE
    )
  }
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
  target <- if (pre_periods == "observed") {
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
  if (length(missing_pre) && pre_periods == "consecutive") {
    stop("Pre-event periods must be consecutive. Missing relative period(s): ",
      paste(missing_pre, collapse = ", "), ". Restrict `data` to a consecutive analysis span.",
      call. = FALSE
    )
  }
  if (length(missing_post) && pre_periods == "consecutive") {
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
  built <- build_design(ps, window, target, pre_periods, event_day)
  result <- estimate_design(built, ps, alpha)
  day_results <- estimate_days(built, ps, alpha)
  if (event_day == "include" && !report_event_day) {
    day_results <- day_results[day_results$relative_period != 0, , drop = FALSE]
  }
  settings <- list(
    window = as.integer(window), alpha = alpha,
    event_time = event_time, event_day = event_day,
    report_event_day = report_event_day,
    pre_periods = pre_periods,
    missing_pre_periods = missing_pre,
    n_missing_pre_periods = length(missing_pre),
    missing_forecast_periods = missing_post,
    n_missing_forecast_periods = length(missing_post),
    pre_event_span = max(required_pre) - min(required_pre) + 1L,
    fit_span = max(built$final_fit) - min(built$final_fit) + 1L,
    forecast_span = max(target) - min(target) + 1L,
    fit_periods = built$final_fit,
    outcome = names(mf)[1L], time = names(mf)[2L]
  )
  structure(list(
    estimate = result$estimate, days = day_results,
    reference = result$reference, windows = built$windows, daily = ps,
    fitted = result$fitted, settings = settings,
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
#' @param formula A two-sided formula of the form `period_mean ~ time`.
#' @param data A data frame with one row per observed period.
#' @param n The unquoted name of the column containing respondent counts.
#' @param variance The unquoted name of the column containing the within-period
#'   sample variance of the outcome. It may be `NA` when `n` is one; the
#'   pooled within-period variance is then used for uncertainty calculations.
#' @inheritParams eventsurvey
#' @return An object of class `eventsurvey`; see [eventsurvey()].
#' @export
#' @examples
#' dat <- simulate_eventsurvey(seed = 10)
#' daily <- aggregate(y ~ day, dat, function(z) {
#'   c(mean = mean(z), n = length(z), variance = var(z))
#' })
#' daily <- data.frame(
#'   day = daily$day, mean = daily$y[, "mean"],
#'   n = daily$y[, "n"], variance = daily$y[, "variance"]
#' )
#' fit <- eventsurvey_summary(mean ~ day, daily,
#'   n = n, variance = variance,
#'   event_time = 0, window = 6
#' )
eventsurvey_summary <- function(formula, data, n, variance, event_time,
                                window = 6L,
                                event_day = c("include", "exclude"),
                                report_event_day = TRUE, alpha = 0.05,
                                pre_periods = c("observed", "consecutive")) {
  cl <- match.call()
  event_day <- match.arg(event_day)
  pre_periods <- match.arg(pre_periods)
  validate_scalar(window, "window", lower = 2, integer = TRUE)
  validate_scalar(alpha, "alpha", lower = 0, upper = 1, open = TRUE)
  if (!is.logical(report_event_day) || length(report_event_day) != 1L ||
    is.na(report_event_day)) {
    stop("`report_event_day` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("`formula` must have the form period_mean ~ time.", call. = FALSE)
  }
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  if (ncol(mf) != 2L) {
    stop("The current implementation requires exactly one time variable.", call. = FALSE)
  }
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
  target <- if (pre_periods == "observed") {
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
  if (length(missing_pre) && pre_periods == "consecutive") {
    stop("Pre-event periods must be consecutive. Missing relative period(s): ",
      paste(missing_pre, collapse = ", "), ". Use `pre_periods = \"observed\"` to analyze an irregular survey schedule.",
      call. = FALSE
    )
  }
  if (length(missing_post) && pre_periods == "consecutive") {
    stop("Every forecast period must contain at least one response. Missing relative period(s): ",
      paste(missing_post, collapse = ", "), ".",
      call. = FALSE
    )
  }
  minimum_pre <- 2L * window + if (event_day == "exclude") 1L else 0L
  if (sum(ps$relative_period < 0) < minimum_pre) {
    stop("At least ", minimum_pre, " pre-event periods are required.", call. = FALSE)
  }
  built <- build_design(ps, window, target, pre_periods, event_day)
  result <- estimate_design(built, ps, alpha)
  day_results <- estimate_days(built, ps, alpha)
  if (event_day == "include" && !report_event_day) {
    day_results <- day_results[day_results$relative_period != 0, , drop = FALSE]
  }
  settings <- list(
    window = as.integer(window), alpha = alpha,
    event_time = event_time, event_day = event_day,
    report_event_day = report_event_day,
    pre_periods = pre_periods,
    missing_pre_periods = missing_pre,
    n_missing_pre_periods = length(missing_pre),
    missing_forecast_periods = missing_post,
    n_missing_forecast_periods = length(missing_post),
    pre_event_span = max(required_pre) - min(required_pre) + 1L,
    fit_span = max(built$final_fit) - min(built$final_fit) + 1L,
    forecast_span = max(target) - min(target) + 1L,
    fit_periods = built$final_fit,
    outcome = names(mf)[1L], time = names(mf)[2L]
  )
  structure(list(
    estimate = result$estimate, days = day_results,
    reference = result$reference, windows = built$windows, daily = ps,
    fitted = result$fitted, settings = settings,
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
#' dat <- simulate_eventsurvey()
#' aggregate(y ~ day, dat, mean)
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

#' @export
print.eventsurvey <- function(x, ...) {
  est <- x$estimate
  cat("Event-during-survey estimate\n")
  cat("Formula: ")
  print(x$formula)
  cat(
    "Window:", x$settings$window, "periods | reference forecasts:",
    est$n_reference, "\n"
  )
  cat(
    "Period sequence:", x$settings$pre_periods,
    "| missing pre/forecast periods:",
    x$settings$n_missing_pre_periods, "/",
    x$settings$n_missing_forecast_periods, "\n"
  )
  cat(sprintf("Average effect: %.3f\n", est$effect))
  cat(sprintf(
    "Honest %.0f%% CI: [%.3f, %.3f]\n",
    100 * (1 - x$settings$alpha), est$conf_low, est$conf_high
  ))
  invisible(x)
}

#' @export
summary.eventsurvey <- function(object, ...) {
  out <- list(
    call = object$call, settings = object$settings,
    estimate = object$estimate, days = object$days,
    reference_range = range(object$reference$error),
    max_absolute_reference_error = max(abs(object$reference$error))
  )
  class(out) <- "summary.eventsurvey"
  out
}

#' @export
print.summary.eventsurvey <- function(x, ...) {
  cat("Event-during-survey analysis\n\n")
  print(x$call)
  cat("\nSettings\n")
  cat("  Fit and forecast window:", x$settings$window, "periods\n")
  cat("  Event period:", x$settings$event_day, "\n")
  cat("  Pre-event period sequence:", x$settings$pre_periods, "\n")
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
  cat("Window-average effect\n")
  print(x$estimate, row.names = FALSE)
  cat("\nPeriod-specific effects\n")
  print(x$days, row.names = FALSE)
  invisible(x)
}
