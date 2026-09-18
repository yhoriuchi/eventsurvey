#' Create a complete event-survey report
#'
#' `eventsurvey_report()` turns one fitted [eventsurvey()] or
#' [eventsurvey_summary()] object, or a named list of fitted objects, into a
#' self-contained HTML report. Each analysis retains its own estimates,
#' diagnostics, settings, and figures.
#'
#' @param x An object of class `eventsurvey`, or a non-empty list of such
#'   objects. Names identify analyses in a multi-analysis report; missing names
#'   are replaced with `"Analysis 1"`, `"Analysis 2"`, and so on.
#' @param file Optional output path. When `NULL`, the report is written to a
#'   temporary file. A `.html` extension is added when none is supplied.
#' @param open Whether to open the completed report automatically. The default,
#'   `interactive()`, evaluates to `TRUE` when the function is called from an
#'   interactive session such as the RStudio console, and normally evaluates
#'   to `FALSE` in `Rscript`, tests, continuous integration, and other batch
#'   runs. The HTML file is created either way.
#' @param overwrite Whether an existing output file may be replaced.
#' @param title Report title. Defaults to `"Event Survey Report"`.
#'
#' @return Invisibly, the normalized path to the generated HTML report.
#' @export
#'
#' @examples
#' dat <- simulate_eventsurvey(seed = 10)
#'
#' if (requireNamespace("rmarkdown", quietly = TRUE) &&
#'     rmarkdown::pandoc_available()) {
#'   report_file <- eventsurvey(y ~ day, dat) |>
#'     eventsurvey_report(open = FALSE)
#' }
eventsurvey_report <- function(x, file = NULL, open = interactive(),
                               overwrite = FALSE,
                               title = "Event Survey Report") {
  if (inherits(x, "eventsurvey")) {
    fits <- list(x)
  } else if (is.list(x) && length(x) > 0L &&
    all(vapply(x, inherits, logical(1), what = "eventsurvey"))) {
    fits <- x
  } else {
    stop("`x` must be an eventsurvey object or a non-empty list of eventsurvey objects.",
      call. = FALSE
    )
  }
  if (!is.logical(open) || length(open) != 1L || is.na(open)) {
    stop("`open` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(title) || length(title) != 1L || is.na(title) ||
    !nzchar(title)) {
    stop("`title` must be one non-empty character string.", call. = FALSE)
  }

  fit_names <- names(fits)
  if (is.null(fit_names)) fit_names <- rep("", length(fits))
  missing_names <- !nzchar(fit_names)
  fit_names[missing_names] <- paste("Analysis", which(missing_names))
  names(fits) <- make.unique(fit_names)
  if (is.null(file)) {
    file <- tempfile("eventsurvey-report-", fileext = ".html")
  } else {
    if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
      stop("`file` must be one non-empty path or NULL.", call. = FALSE)
    }
    file <- path.expand(file)
    extension <- tolower(tools::file_ext(file))
    if (!nzchar(extension)) {
      file <- paste0(file, ".html")
    } else if (extension != "html") {
      stop("`file` must have an `.html` extension.", call. = FALSE)
    }
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Install the `rmarkdown` package to create an HTML report.",
      call. = FALSE
    )
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc is required to create an HTML report. Install RStudio or Pandoc and try again.",
      call. = FALSE
    )
  }

  output_dir <- dirname(file)
  if (!dir.exists(output_dir)) {
    stop("The report directory does not exist: ", output_dir, call. = FALSE)
  }
  if (file.exists(file) && !overwrite) {
    stop("The report already exists. Choose another path or set `overwrite = TRUE`.",
      call. = FALSE
    )
  }

  template <- system.file(
    "rmarkdown", "eventsurvey-report.Rmd",
    package = "eventsurvey"
  )
  if (!nzchar(template)) {
    stop("The eventsurvey report template could not be found.", call. = FALSE)
  }
  section_template <- system.file(
    "rmarkdown", "eventsurvey-report-section.Rmd",
    package = "eventsurvey"
  )
  if (!nzchar(section_template)) {
    stop("The eventsurvey report section template could not be found.",
      call. = FALSE
    )
  }

  report_environment <- new.env(parent = globalenv())
  report_environment$fits <- fits
  report_environment$section_template <- section_template
  rendered <- rmarkdown::render(
    input = template,
    output_file = basename(file),
    output_dir = output_dir,
    intermediates_dir = tempdir(),
    envir = report_environment,
    params = list(title = title),
    quiet = TRUE,
    clean = TRUE
  )
  rendered <- normalizePath(rendered, mustWork = TRUE)

  if (open) {
    utils::browseURL(rendered)
  }
  invisible(rendered)
}
