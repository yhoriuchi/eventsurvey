# Published-application HTML reports --------------------------------------
#
# Run from the package root. The Bateson--Weintraub report requires a local,
# licensed source file identified by EVENTSURVEY_BATESON_DATA. The script
# saves only self-contained analytical reports; it never copies source records
# or period-level data into the repository.

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

eventsurvey_report(
  fit_bateson,
  file = file.path(report_dir, "bateson-weintraub.html"),
  open = FALSE,
  overwrite = TRUE,
  title = "Bateson and Weintraub (2022)"
)

# Epifanio, Giani, and Ivandic (2023) ------------------------------------

data("published_examples", package = "eventsurvey")
epifanio <- subset(published_examples, grepl("Epifanio", study))
privacy <- subset(epifanio, grepl("privacy", outcome))
procedural <- subset(epifanio, grepl("procedural", outcome))

fit_privacy <- eventsurvey_summary(
  mean ~ day,
  data = privacy,
  event_time = 0,
  window = 5,
  report_event_day = FALSE,
  schedule = "observed"
)

fit_procedural <- eventsurvey_summary(
  mean ~ day,
  data = procedural,
  event_time = 0,
  window = 7,
  report_event_day = FALSE,
  schedule = "observed"
)

eventsurvey_report(
  list(
    "Privacy-rights restrictions" = fit_privacy,
    "Procedural-rights restrictions" = fit_procedural
  ),
  file = file.path(report_dir, "epifanio-giani-ivandic.html"),
  open = FALSE,
  overwrite = TRUE,
  title = "Epifanio, Giani, and Ivandic (2023)"
)
