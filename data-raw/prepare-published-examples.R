# Prepare period-level sufficient statistics for the published applications.
#
# These summaries deliberately omit respondent-level records. Set the
# environment variable below to a local copy of the source file, then run this
# script from the package root with `Rscript`.
#
# Bateson & Weintraub (2022) use AmericasBarometer data governed by a license
# that prohibits redistribution. Their summaries are therefore not included
# in the package; see prepare-bateson-local.R for a local-only recipe.
# Epifanio, Giani & Ivandic (2023): the source analysis uses main_cleaned.csv.

epifanio_file <- Sys.getenv("EVENTSURVEY_EPIFANIO_DATA")
if (!file.exists(epifanio_file)) {
  stop("Set EVENTSURVEY_EPIFANIO_DATA to the source CSV file.")
}

summarise_periods <- function(day, outcome, study, outcome_name, event_date) {
  data.frame(day = day, value = outcome) |>
    dplyr::filter(is.finite(day), is.finite(value)) |>
    dplyr::group_by(day) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(value),
      variance = if (dplyr::n() > 1L) stats::var(value) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::arrange(day) |>
    dplyr::transmute(
      study = study,
      outcome = outcome_name,
      event_date = as.Date(event_date),
      date = as.Date(event_date) + day,
      day,
      n,
      mean,
      variance
    ) |>
    as.data.frame()
}

epifanio <- read.csv(epifanio_file, check.names = FALSE)
interview_date <- as.character(epifanio$intdate)
interview_date <- ifelse(nchar(interview_date) < 8L,
                         paste0("0", interview_date), interview_date)
interview_date <- as.Date(paste(substr(interview_date, 5L, 8L),
                                substr(interview_date, 3L, 4L),
                                substr(interview_date, 1L, 2L), sep = "-"))
epifanio_day <- as.numeric(interview_date - as.Date("2005-07-07"))
privacy_summary <- summarise_periods(
  epifanio_day, as.numeric(epifanio$privacy),
  "Epifanio, Giani, and Ivandic (2023)",
  "Support for restrictions on privacy rights", "2005-07-07"
)
procedural_summary <- summarise_periods(
  epifanio_day, as.numeric(epifanio$procedural),
  "Epifanio, Giani, and Ivandic (2023)",
  "Support for restrictions on procedural rights", "2005-07-07"
)

published_examples <- dplyr::bind_rows(privacy_summary, procedural_summary) |>
  as.data.frame()
rownames(published_examples) <- NULL
save(published_examples, file = "data/published_examples.rda", compress = "xz")
