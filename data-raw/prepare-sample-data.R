# Rebuild the ready-to-use respondent-level example data.
# Run from the package root after changing simulate_eventsurvey().

devtools::load_all(".")
sample_data <- simulate_eventsurvey(seed = 10)
save(sample_data, file = "data/sample_data.rda", compress = "xz")
