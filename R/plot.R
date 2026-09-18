#' Plot an event-during-survey analysis
#'
#' @param x An `eventsurvey` object.
#' @param type Plot type: the observed and extrapolated outcome path,
#'   period-specific effects, rolling reference errors, combined outcome and
#'   response coverage, all period means, or respondent counts by period. In
#'   the combined coverage plot, gray bars are scaled relative to the largest
#'   period sample and placed in a strip below the observed outcome series.
#'   A blue dashed vertical line marks event period 0 in every time-axis plot.
#' @param inner_level Optional inner confidence level for an effects plot, such
#'   as `0.90` when the fitted object contains 95% intervals. The outer
#'   interval uses the confidence level supplied when fitting the object.
#' @param ... Unused.
#' @return A `ggplot2` object.
#' @export
plot.eventsurvey <- function(x,
                             type = c("counterfactual", "effects", "diagnostics", "coverage", "daily", "counts"),
                             inner_level = NULL,
                             ...) {
  type <- match.arg(type)
  event_color <- "#337AB7"
  event_linetype <- "dashed"
  event_linewidth <- 0.45
  observed_color <- "#212529"
  fit_color <- "#6C757D"

  if (type == "effects") {
    dat <- x$days
    out <- ggplot2::ggplot(dat, ggplot2::aes(x = relative_period, y = effect)) +
      ggplot2::geom_vline(
        xintercept = 0, linetype = event_linetype, color = event_color,
        linewidth = event_linewidth
      ) +
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
        x$settings$schedule, x$settings$event_day
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
      ggplot2::scale_x_continuous(
        breaks = sort(unique(c(0, dat$relative_period)))
      ) +
      ggplot2::labs(x = "Period relative to event", y = "Estimated event effect") +
      ggthemes::theme_few())
  }

  if (type == "diagnostics") {
    dat <- x$reference
    forecast_periods <- sort(unique(c(dat$forecast_period, 0)))
    break_index <- if (length(forecast_periods) <= 9L) {
      seq_along(forecast_periods)
    } else {
      unique(round(seq(1, length(forecast_periods), length.out = 9L)))
    }
    diagnostic_breaks <- forecast_periods[break_index]
    return(ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = forecast_period, y = error,
        group = factor(fit_start)
      )
    ) +
      ggplot2::geom_vline(
        xintercept = 0, linetype = event_linetype, color = event_color,
        linewidth = event_linewidth
      ) +
      ggplot2::geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
      ggplot2::geom_line(color = "grey75", linewidth = 0.35) +
      ggplot2::geom_point(ggplot2::aes(color = horizon), size = 1.8) +
      ggplot2::scale_color_gradient(low = "#D9E6F2", high = event_color) +
      ggplot2::scale_x_continuous(breaks = diagnostic_breaks) +
      ggplot2::labs(
        x = "Pre-event forecast period", y = "Reference forecast error",
        color = "Horizon"
      ) +
      ggthemes::theme_few())
  }

  daily <- x$daily
  if (type == "coverage") {
    outcome_limits <- range(daily$mean, na.rm = TRUE)
    outcome_span <- diff(outcome_limits)
    if (!is.finite(outcome_span) || outcome_span <= 0) {
      outcome_span <- max(abs(outcome_limits), 1, na.rm = TRUE) * 0.2
    }

    plot_min <- outcome_limits[1L] - 0.30 * outcome_span
    plot_max <- outcome_limits[2L] + 0.05 * outcome_span
    bar_base <- outcome_limits[1L] - 0.27 * outcome_span
    bar_height <- 0.15 * outcome_span
    observed_periods <- sort(unique(daily$relative_period))
    period_gaps <- diff(observed_periods)
    bar_width <- if (length(period_gaps)) {
      0.8 * min(period_gaps[period_gaps > 0])
    } else {
      0.8
    }
    bar_data <- data.frame(
      xmin = daily$relative_period - bar_width / 2,
      xmax = daily$relative_period + bar_width / 2,
      ymin = bar_base,
      ymax = bar_base + bar_height * daily$n / max(daily$n, na.rm = TRUE)
    )

    return(ggplot2::ggplot(daily, ggplot2::aes(x = relative_period)) +
      ggplot2::geom_vline(
        xintercept = 0, linetype = event_linetype, color = event_color,
        linewidth = event_linewidth
      ) +
      ggplot2::geom_rect(
        data = bar_data,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE, fill = "grey82", color = NA
      ) +
      ggplot2::geom_line(ggplot2::aes(y = mean),
        color = "grey55", linewidth = 0.55
      ) +
      ggplot2::geom_point(ggplot2::aes(y = mean),
        color = observed_color, size = 2.2
      ) +
      ggplot2::coord_cartesian(ylim = c(plot_min, plot_max)) +
      ggplot2::labs(
        x = "Period relative to event", y = "Observed period mean"
      ) +
      ggthemes::theme_few())
  }
  if (type == "counts") {
    return(ggplot2::ggplot(daily, ggplot2::aes(x = relative_period, y = n)) +
      ggplot2::geom_vline(
        xintercept = 0, linetype = event_linetype, color = event_color,
        linewidth = event_linewidth
      ) +
      ggplot2::geom_col(fill = "grey65", color = "grey45", width = 0.8) +
      ggplot2::labs(x = "Period relative to event", y = "Responses") +
      ggthemes::theme_few())
  }
  if (type == "daily") {
    return(ggplot2::ggplot(daily, ggplot2::aes(x = relative_period, y = mean)) +
      ggplot2::geom_vline(
        xintercept = 0, linetype = event_linetype, color = event_color,
        linewidth = event_linewidth
      ) +
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
    ggplot2::geom_vline(
      xintercept = 0, linetype = event_linetype, color = event_color,
      linewidth = event_linewidth
    ) +
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
    ggplot2::scale_x_continuous(breaks = sort(unique(c(0, line_days)))) +
    ggplot2::labs(x = "Period relative to event", y = "Outcome mean") +
    ggthemes::theme_few()
}

utils::globalVariables(c(
  "relative_period", "effect", "conf_low", "conf_high",
  "forecast_period", "error", "fit_start", "horizon",
  "mean", "n", "counterfactual", "observed",
  "xmin", "xmax", "ymin", "ymax"
))
