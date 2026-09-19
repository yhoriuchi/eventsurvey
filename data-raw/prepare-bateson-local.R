# Prepare Bateson--Weintraub daily summaries for local analysis only.
#
# LAPOP/CGD requires each user to obtain AmericasBarometer data directly and
# agree to its current terms. This script does not download, copy, or save the
# source records into the package. Point EVENTSURVEY_BATESON_DATA to the local
# four-country analysis file named cleaned_main.csv, then source this script.

bateson_file <- Sys.getenv("EVENTSURVEY_BATESON_DATA")
if (!file.exists(bateson_file)) {
  stop("Set EVENTSURVEY_BATESON_DATA to your licensed source CSV file.")
}

bateson_source <- read.csv(bateson_file, check.names = FALSE)
trust_scale <- c(
  "Untrustworthy" = 1,
  "Not Very Trustworthy" = 2,
  "Somewhat Trustworthy" = 3,
  "Very Trustworthy" = 4
)
bateson_y <- unname(trust_scale[as.character(bateson_source$trustusgov)])
bateson <- data.frame(
  day = bateson_source$time_zero,
  y = bateson_y
) |>
  dplyr::filter(is.finite(day), is.finite(y)) |>
  dplyr::group_by(day) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(y),
    variance = if (dplyr::n() > 1L) stats::var(y) else NA_real_,
    .groups = "drop"
  ) |>
  dplyr::arrange(day) |>
  as.data.frame()
stopifnot(nrow(bateson) == 51L, min(bateson$day) == -26L)
