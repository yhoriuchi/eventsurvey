# What this code is about: Regenerate public reports and estimate tables
# using daily calibration and equal-day aggregation, keeping licensed inputs private.
# Published-application HTML reports --------------------------------------
#
# Run from the package root. The Bateson--Weintraub report requires a local,
# licensed source file identified by EVENTSURVEY_BATESON_DATA. The script
# saves self-contained reports and an estimate-only results table; it never
# copies source records or period-level input data into the repository.

# Initial settings --------------------------------------------------------

devtools::load_all(".", quiet = TRUE)
report_dir <- file.path("pkgdown", "assets", "reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

# Bateson and Weintraub (2022) -------------------------------------------

source("data-raw/prepare-bateson-local.R")

fit_bateson <- eventsurvey_summary(
  mean ~ day,
  data = bateson,
  event_time = 0,
  window = 7,
  event_day = "include",
  report_event_day = FALSE
)

# Epifanio, Giani, and Ivandic (2023) ------------------------------------

data("published_examples", package = "eventsurvey")
epifanio <- published_examples |>
  dplyr::filter(grepl("Epifanio", study))
privacy <- epifanio |>
  dplyr::filter(grepl("privacy", outcome))
procedural <- epifanio |>
  dplyr::filter(grepl("procedural", outcome))

fit_privacy <- eventsurvey_summary(
  mean ~ day,
  data = privacy,
  event_time = 0,
  window = 5,
  event_day = "include",
  report_event_day = FALSE,
  schedule = "observed"
)

fit_procedural <- eventsurvey_summary(
  mean ~ day,
  data = procedural,
  event_time = 0,
  window = 7,
  event_day = "include",
  report_event_day = FALSE,
  schedule = "observed"
)

# Verify daily calibration and aggregate targets -------------------------

main_fits <- list(
  "Bateson: trust" = fit_bateson,
  "Epifanio: privacy rights" = fit_privacy,
  "Epifanio: procedural rights" = fit_procedural
)
for (fit in main_fits) {
  stopifnot(
    fit$settings$calibration == "estimand",
    isTRUE(all.equal(fit$estimate$effect, mean(fit$average_components$effect))),
    isTRUE(all.equal(fit$estimate$bias_bound, mean(fit$average_components$bias_bound))),
    isTRUE(all.equal(
      fit$estimate$std_error^2,
      sum(fit$inference$day_covariance) / nrow(fit$fitted)^2
    )),
    all(is.finite(c(fit$estimate$conf_low, fit$estimate$conf_high)))
  )
}

# Save estimates only for the website ------------------------------------
results <- do.call(rbind, lapply(seq_along(main_fits), function(i) {
  fit <- main_fits[[i]]
  data.frame(
    outcome = names(main_fits)[i],
    target_periods = paste(fit$fitted$relative_period, collapse = ", "),
    effect = fit$estimate$effect, std_error = fit$estimate$std_error,
    bias_bound = fit$estimate$bias_bound,
    conf_low = fit$estimate$conf_low, conf_high = fit$estimate$conf_high,
    n_windows = fit$estimate$n_windows
  )
}))
write.csv(results, file.path("vignettes", "figures", "application-calibration.csv"), row.names = FALSE)
print(results, row.names = FALSE)

# Render application reports --------------------------------------------
eventsurvey_report(
  fit_bateson,
  file = file.path(report_dir, "bateson-weintraub.html"),
  open = FALSE, overwrite = TRUE,
  title = "Bateson and Weintraub (2022)"
)
eventsurvey_report(
  list("Privacy rights" = fit_privacy, "Procedural rights" = fit_procedural),
  file = file.path(report_dir, "epifanio-giani-ivandic.html"),
  open = FALSE, overwrite = TRUE,
  title = "Epifanio, Giani, and Ivandic (2023)"
)
