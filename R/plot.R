#' Plot an event-during-survey analysis
#'
#' @param x An `eventsurvey` object.
#' @param type Plot type: the observed and extrapolated outcome path,
#'   period-specific effects, rolling reference errors, all period means, or
#'   respondent counts by period.
#' @param inner_level Optional inner confidence level for an effects plot, such
#'   as `0.90` when the fitted object contains 95% intervals. The outer
#'   interval uses the confidence level supplied when fitting the object.
#' @param ... Unused.
#' @return A `ggplot2` object.
#' @export
plot.eventsurvey <- function(x,
                             type = c("counterfactual", "effects", "diagnostics", "daily", "counts"),
                             inner_level = NULL,
                             ...) {
  type <- match.arg(type)
  event_color <- "#006D77"
  observed_color <- "#263238"
  fit_color <- "#E07A5F"

  if (type == "effects") {
    dat <- x$days
    out <- ggplot2::ggplot(dat, ggplot2::aes(x = relative_period, y = effect)) +
      ggplot2::geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low, ymax = conf_high),
        width = 0.16, color = "grey60", linewidth = 2.2
      )
    if (!is.null(inner_level)) {
      outer_level <- 1 - x$settings$alpha
      if (!is.numeric(inner_level) || length(inner_level) != 1L ||
        !is.finite(inner_level) || inner_level <= 0 ||
        inner_level >= outer_level) {
        stop("`inner_level` must be between zero and the fitted confidence level.",
          call. = FALSE
        )
      }
      target <- x$fitted$relative_period
      built <- build_design(
        x$daily, x$settings$window, target,
        x$settings$pre_periods, x$settings$event_day
      )
      inner <- estimate_days(built, x$daily, alpha = 1 - inner_level)
      if (x$settings$event_day == "include" &&
        !x$settings$report_event_day) {
        inner <- inner[inner$relative_period != 0, , drop = FALSE]
      }
      out <- out +
        ggplot2::geom_errorbar(
          data = inner,
          ggplot2::aes(ymin = conf_low, ymax = conf_high),
          width = 0, color = observed_color, linewidth = 0.8
        )
    }
    return(out +
      ggplot2::geom_point(size = 2.4, color = observed_color) +
      ggplot2::scale_x_continuous(breaks = dat$relative_period) +
      ggplot2::labs(x = "Period relative to event", y = "Estimated event effect") +
      ggthemes::theme_few())
  }

  if (type == "diagnostics") {
    dat <- x$reference
    return(ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = forecast_period, y = error,
        group = factor(fit_start)
      )
    ) +
      ggplot2::geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
      ggplot2::geom_line(color = "grey75", linewidth = 0.35) +
      ggplot2::geom_point(ggplot2::aes(color = horizon), size = 1.8) +
      ggplot2::scale_color_gradient(low = "#83C5BE", high = event_color) +
      ggplot2::labs(
        x = "Pre-event forecast period", y = "Reference forecast error",
        color = "Horizon"
      ) +
      ggthemes::theme_few())
  }

  daily <- x$daily
  if (type == "counts") {
    return(ggplot2::ggplot(daily, ggplot2::aes(x = relative_period, y = n)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = event_color) +
      ggplot2::geom_col(fill = "grey65", color = "grey45", width = 0.8) +
      ggplot2::labs(x = "Period relative to event", y = "Responses") +
      ggthemes::theme_few())
  }
  if (type == "daily") {
    return(ggplot2::ggplot(daily, ggplot2::aes(x = relative_period, y = mean)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = event_color) +
      ggplot2::geom_line(color = "grey65", linewidth = 0.45) +
      ggplot2::geom_point(ggplot2::aes(size = n), color = observed_color, alpha = 0.8) +
      ggplot2::scale_size_continuous(range = c(1.5, 4)) +
      ggplot2::labs(
        x = "Period relative to event", y = "Observed period mean",
        size = "Responses"
      ) +
      ggthemes::theme_few())
  }

  fit_days <- x$settings$fit_periods
  fit_data <- daily[daily$relative_period %in% fit_days, , drop = FALSE]
  model <- stats::lm(mean ~ relative_period, data = fit_data, weights = n)
  line_days <- sort(c(fit_days, x$fitted$relative_period))
  line_data <- data.frame(relative_period = line_days)
  line_data$counterfactual <- stats::predict(model, newdata = line_data)
  observed <- daily[daily$relative_period %in% line_days, , drop = FALSE]
  ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = event_color) +
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(x = relative_period, y = counterfactual),
      color = fit_color, linewidth = 0.85
    ) +
    ggplot2::geom_segment(
      data = x$fitted,
      ggplot2::aes(
        x = relative_period, xend = relative_period,
        y = counterfactual, yend = observed
      ),
      color = event_color, linewidth = 0.65
    ) +
    ggplot2::geom_point(
      data = observed,
      ggplot2::aes(x = relative_period, y = mean),
      color = observed_color, size = 2.4
    ) +
    ggplot2::geom_point(
      data = x$fitted,
      ggplot2::aes(x = relative_period, y = counterfactual),
      shape = 21, fill = "white", color = fit_color, size = 2.4
    ) +
    ggplot2::labs(x = "Period relative to event", y = "Outcome mean") +
    ggthemes::theme_few()
}

utils::globalVariables(c(
  "relative_period", "effect", "conf_low", "conf_high",
  "forecast_period", "error", "fit_start", "horizon",
  "mean", "n", "counterfactual", "observed"
))
