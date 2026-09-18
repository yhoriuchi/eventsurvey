#' Published application data at the period level
#'
#' Period-level sufficient statistics used to reproduce the `eventsurvey`
#' reanalysis of Epifanio, Giani, and Ivandic (2023). These are derived
#' summaries, not the original respondent-level replication file. Each row
#' contains a date's respondent count, outcome mean, and within-date sample
#' variance.
#'
#' The Bateson--Weintraub application uses AmericasBarometer data that the
#' package cannot redistribute. Its article instead provides a local data-
#' preparation recipe for authorized users. See the package articles for
#' source links, access conditions, outcome coding, and the distinction between
#' the original studies and these reanalyses.
#'
#' @format A data frame with 274 rows and 8 variables:
#' \describe{
#'   \item{study}{Published study.}
#'   \item{outcome}{Outcome summarized.}
#'   \item{event_date}{Date of the event.}
#'   \item{date}{Interview date.}
#'   \item{day}{Calendar day relative to the event.}
#'   \item{n}{Number of non-missing responses.}
#'   \item{mean}{Daily sample mean.}
#'   \item{variance}{Daily sample variance; `NA` when `n = 1`.}
#' }
#' @references
#' Bateson, Regina, and Michael Weintraub. 2022. "The 2016 Election and
#' America's Standing Abroad: Quasi-experimental Evidence of a Trump Effect."
#' *Journal of Politics* 84(4): 2300--2304. \doi{10.1086/718209}
#'
#' Epifanio, Mariaelisa, Marco Giani, and Ria Ivandic. 2023. "Wait and See?
#' Public Opinion Dynamics after Terrorist Attacks." *Journal of Politics*
#' 85(3): 843--859. \doi{10.1086/723020}
#' @source
#' Epifanio, Giani, and Ivandic replication materials:
#' \doi{10.7910/DVN/AUHSMD}.
#' @examples
#' data(published_examples)
#' subset(published_examples, grepl("privacy", outcome) & day >= -5 & day <= 4)
"published_examples"
