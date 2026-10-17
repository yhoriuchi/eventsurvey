# Published-application HTML reports --------------------------------------
#
# Run from the package root. The Bateson--Weintraub report requires a local,
# licensed source file identified by EVENTSURVEY_BATESON_DATA. The script
# saves self-contained reports and an estimate-only comparison table; it never
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
  calibration = "estimand",
  report_event_day = FALSE
)

fit_bateson_pooled <- eventsurvey_summary(
  mean ~ day,
  data = bateson,
  event_time = 0,
  window = 7,
  event_day = "include",
  calibration = "pooled",
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
  calibration = "estimand",
  report_event_day = FALSE,
  schedule = "observed"
)

fit_procedural <- eventsurvey_summary(
  mean ~ day,
  data = procedural,
  event_time = 0,
  window = 7,
  event_day = "include",
  calibration = "estimand",
  report_event_day = FALSE,
  schedule = "observed"
)

fit_privacy_pooled <- eventsurvey_summary(
  mean ~ day,
  data = privacy,
  event_time = 0,
  window = 5,
  event_day = "include",
  calibration = "pooled",
  report_event_day = FALSE,
  schedule = "observed"
)

fit_procedural_pooled <- eventsurvey_summary(
  mean ~ day,
  data = procedural,
  event_time = 0,
  window = 7,
  event_day = "include",
  calibration = "pooled",
  report_event_day = FALSE,
  schedule = "observed"
)

# Verify calibration and unchanged targets -------------------------------

main_fits <- list(
  "Bateson: trust" = fit_bateson,
  "Epifanio: privacy rights" = fit_privacy,
  "Epifanio: procedural rights" = fit_procedural
)
pooled_fits <- list(fit_bateson_pooled, fit_privacy_pooled, fit_procedural_pooled)

for (i in seq_along(main_fits)) {
  main <- main_fits[[i]]
  comparison <- pooled_fits[[i]]
  average_errors <- tapply(main$reference$error, main$reference$fit_start, mean)
  stopifnot(
    main$settings$calibration == "estimand",
    comparison$settings$calibration == "pooled",
    identical(main$fitted$relative_period, comparison$fitted$relative_period),
    isTRUE(all.equal(main$estimate$effect, comparison$estimate$effect)),
    isTRUE(all.equal(main$days$effect, comparison$days$effect)),
    isTRUE(all.equal(main$estimate$bias_bound, max(abs(average_errors)))),
    isTRUE(all.equal(
      comparison$estimate$bias_bound, max(abs(comparison$reference$error))
    )),
    all(is.finite(c(
      main$estimate$conf_low, main$estimate$conf_high,
      main$days$conf_low, main$days$conf_high
    ))),
    all(main$days$conf_low <= main$days$conf_high)
  )
  for (j in seq_len(nrow(main$days))) {
    matching <- main$reference$horizon == main$days$horizon[j]
    stopifnot(isTRUE(all.equal(
      main$days$bias_bound[j], max(abs(main$reference$error[matching]))
    )))
  }
}

# Save estimates only for the website comparison -------------------------

comparison_rows <- lapply(seq_along(main_fits), function(i) {
  pair <- list(main_fits[[i]], pooled_fits[[i]])
  do.call(rbind, lapply(seq_along(pair), function(j) {
    fit <- pair[[j]]
    data.frame(
      outcome = names(main_fits)[i],
      calibration = fit$settings$calibration,
      role = if (j == 1L) "Main" else "Comparison",
      target_periods = paste(fit$fitted$relative_period, collapse = ", "),
      effect = fit$estimate$effect,
      bias_bound = fit$estimate$bias_bound,
      conf_low = fit$estimate$conf_low,
      conf_high = fit$estimate$conf_high,
      n_windows = fit$estimate$n_windows
    )
  }))
})
comparison_table <- do.call(rbind, comparison_rows)
write.csv(
  comparison_table,
  file.path("vignettes", "figures", "application-calibration.csv"),
  row.names = FALSE
)
print(comparison_table, row.names = FALSE)

# Render main analyses first, then explicitly labeled comparisons --------

eventsurvey_report(
  list(
    "Main: estimand-specific calibration" = fit_bateson,
    "Comparison: pooled calibration" = fit_bateson_pooled
  ),
  file = file.path(report_dir, "bateson-weintraub.html"),
  open = FALSE,
  overwrite = TRUE,
  title = "Bateson and Weintraub (2022): main analysis and pooled comparison"
)

eventsurvey_report(
  list(
    "Main: privacy rights, estimand-specific calibration" = fit_privacy,
    "Main: procedural rights, estimand-specific calibration" = fit_procedural,
    "Comparison: privacy rights, pooled calibration" = fit_privacy_pooled,
    "Comparison: procedural rights, pooled calibration" = fit_procedural_pooled
  ),
  file = file.path(report_dir, "epifanio-giani-ivandic.html"),
  open = FALSE,
  overwrite = TRUE,
  title = "Epifanio, Giani, and Ivandic (2023): main analyses and pooled comparisons"
)
