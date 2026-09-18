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
keep <- is.finite(bateson_source$time_zero) & is.finite(bateson_y)
split_y <- split(bateson_y[keep], bateson_source$time_zero[keep])

bateson <- data.frame(
  day = as.integer(names(split_y)),
  n = vapply(split_y, length, integer(1)),
  mean = vapply(split_y, mean, numeric(1)),
  variance = vapply(
    split_y,
    function(x) if (length(x) > 1L) stats::var(x) else NA_real_,
    numeric(1)
  )
)
bateson <- bateson[order(bateson$day), ]
stopifnot(nrow(bateson) == 51L, min(bateson$day) == -26L)
