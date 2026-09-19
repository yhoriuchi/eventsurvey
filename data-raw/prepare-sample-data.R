# Rebuild the ready-to-use respondent-level example data.
# Run from the package root after changing simulate_eventsurvey().

devtools::load_all(".")
simulated <- simulate_eventsurvey(seed = 10)
sample_data <- simulated |>
  dplyr::transmute(
    day,
    y = as.integer(cut(
      y,
      breaks = c(-Inf, 45, 48, 51, 54, Inf),
      labels = FALSE
    ))
  ) |>
  as.data.frame()
save(sample_data, file = "data/sample_data.rda", compress = "xz")
