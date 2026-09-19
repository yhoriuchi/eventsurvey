# Rebuild the ready-to-use respondent-level example data.
# Run from the package root after changing simulate_eventsurvey().

devtools::load_all(".")
simulated <- simulate_eventsurvey(seed = 10)
sample_data <- data.frame(
  day = simulated$day,
  y = as.integer(cut(
    simulated$y,
    breaks = c(-Inf, 45, 48, 51, 54, Inf),
    labels = FALSE
  ))
)
save(sample_data, file = "data/sample_data.rda", compress = "xz")
