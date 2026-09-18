test_that("eventsurvey_report validates its inputs", {
  expect_error(
    eventsurvey_report(data.frame()),
    "eventsurvey object"
  )

  dat <- simulate_eventsurvey(seed = 20)
  fit <- eventsurvey(y ~ day, dat)
  expect_error(eventsurvey_report(fit, open = NA), "`open` must be")
  expect_error(eventsurvey_report(fit, overwrite = NA), "`overwrite` must be")
  expect_error(eventsurvey_report(fit, title = ""), "`title` must be")
  expect_error(eventsurvey_report(fit, file = "report.pdf"), "`.html`")
  expect_error(eventsurvey_report(list(), open = FALSE), "non-empty list")
  expect_error(
    eventsurvey_report(list(fit, data.frame()), open = FALSE),
    "non-empty list"
  )
})

test_that("eventsurvey_report creates a self-contained HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  dat <- simulate_eventsurvey(seed = 21)
  fit <- eventsurvey(y ~ day, dat)
  output <- tempfile(fileext = ".html")
  on.exit(unlink(output), add = TRUE)

  report <- eventsurvey_report(fit, file = output, open = FALSE)
  expect_true(file.exists(report))
  html <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(html, "Event Survey Report", fixed = TRUE)
  expect_match(html, "Main result", fixed = TRUE)
  expect_match(html, "Automatic diagnostics", fixed = TRUE)
  expect_match(html, "Forecast diagnostics", fixed = TRUE)
  expect_match(html, "Interpretation limits", fixed = TRUE)
  expect_match(html, "original units", fixed = TRUE)
  expect_match(html, "Input rows", fixed = TRUE)
  expect_match(html, "Rows omitted for missing outcome or time", fixed = TRUE)
  expect_match(html, "Total responses represented", fixed = TRUE)
  expect_error(
    eventsurvey_report(fit, file = output, open = FALSE),
    "already exists"
  )

  split_y <- split(dat$y, dat$day)
  daily <- data.frame(
    day = as.integer(names(split_y)),
    mean = vapply(split_y, mean, numeric(1)),
    n = vapply(split_y, length, integer(1)),
    variance = vapply(split_y, stats::var, numeric(1))
  )
  summary_fit <- eventsurvey_summary(mean ~ day, daily)
  summary_output <- tempfile(fileext = ".html")
  on.exit(unlink(summary_output), add = TRUE)

  summary_report <- eventsurvey_report(
    summary_fit,
    file = summary_output,
    open = FALSE
  )
  expect_true(file.exists(summary_report))
  summary_html <- paste(readLines(summary_report, warn = FALSE), collapse = "\n")
  expect_match(summary_html, "eventsurvey_summary", fixed = TRUE)

  combined_output <- tempfile(fileext = ".html")
  on.exit(unlink(combined_output), add = TRUE)
  combined_report <- eventsurvey_report(
    list("Respondent data" = fit, "Summary data" = summary_fit),
    file = combined_output,
    open = FALSE,
    title = "Two analyses"
  )
  combined_html <- paste(
    readLines(combined_report, warn = FALSE),
    collapse = "\n"
  )
  expect_match(combined_html, "Two analyses", fixed = TRUE)
  expect_match(combined_html, "Respondent data", fixed = TRUE)
  expect_match(combined_html, "Summary data", fixed = TRUE)
  expect_match(combined_html, "does not pool events or outcomes", fixed = TRUE)
  expect_match(combined_html, "multiple\\s+comparisons")
})
