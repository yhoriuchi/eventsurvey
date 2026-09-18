# Bateson--Weintraub website figures --------------------------------------
#
# Run from the package root after setting EVENTSURVEY_BATESON_DATA. The input
# remains local under the LAPOP/CGD terms; only statistical figures are saved.

source("data-raw/prepare-bateson-local.R")
devtools::load_all(".", quiet = TRUE)

fit_bateson <- eventsurvey_summary(
  mean ~ day,
  data = bateson,
  n = n,
  variance = variance,
  event_time = 0,
  window = 7,
  event_day = "include",
  report_event_day = FALSE
)

figure_dir <- file.path("vignettes", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

save_figure <- function(plot, filename, width = 7, height = 4.2) {
  ggplot2::ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 180,
    bg = "white"
  )
}

save_figure(
  plot(fit_bateson, type = "daily") +
    ggplot2::labs(y = "Mean trust in the U.S. government"),
  "bateson-daily-means.png"
)
save_figure(plot(fit_bateson, type = "counts"), "bateson-daily-counts.png")
save_figure(
  plot(fit_bateson, type = "counterfactual") +
    ggplot2::labs(y = "Mean trust in the U.S. government"),
  "bateson-counterfactual.png"
)
save_figure(
  plot(fit_bateson, type = "effects", inner_level = 0.90) +
    ggplot2::labs(y = "Estimated effect on trust"),
  "bateson-effects.png"
)
save_figure(plot(fit_bateson, type = "diagnostics"), "bateson-diagnostics.png")
