#' Functions for Audit Year(s) Determination
#'
#' @description
#' Functions to determine current audit year and generate sequential lists of
#' audit years following NPDA conventions.
#'
#' Audit years are formatted as "YYYY/YY" (e.g., "2024/25").
#'
#' \strong{Available Functions:}
#' \itemize{
#'   \item \code{\link{get_AuditYear}}: Determine audit year from date(s)
#'   \item \code{\link{get_AuditYears}}: Generate sequence of audit years
#' }
#'
#' @template AuditYear_quarters
#'
#' @section Key Features:
#' \itemize{
#'   \item Fully vectorized - works with single dates or date vectors
#'   \item Integrates seamlessly with dplyr pipelines
#'   \item Handles edge cases (leap years, century boundaries)
#'   \item Optional integer or formatted string output
#' }
#'
#' @family AuditYear functions
#' @name family_AuditYear
#' @aliases AuditYear AuditYears
NULL

# ========== 0. Internal Helper Function ==========

#' Format Numeric Year as Audit Year String
#'
#' @description
#' Internal helper function to convert a numeric year to NPDA audit year format.
#'
#' @param year Integer or numeric year (e.g., 2017).
#'
#' @return Character string in format "YYYY/YY" (e.g., "2017/18").
#'
#' @keywords internal
#' @noRd
#'
#' @family AuditYear functions
#' @examples
#' \dontrun{
#' .formatAuditYear(2017)  # Returns "2017/18"
#' .formatAuditYear(2024)  # Returns "2024/25"
#' }
.formatAuditYear <- function(year) {
  paste0(year, "/", sprintf("%02d", (year + 1) %% 100))
}

# ========== 1. Get Current Audit Year ==========
#' Get Current Audit Year
#'
#' @description
#' Determines the current NPDA audit year based on a given date. The NPDA audit
#' year runs from 1 April to 31 March, so dates from January-March fall into
#' the audit year that started in the previous calendar year.
#'
#' @param date Date object or string that can be coerced to Date. The date for
#'   which to determine the audit year. Default is \code{Sys.Date()} (today).
#' @param format Logical. If \code{TRUE} (default), returns audit year as formatted
#'   string "YYYY/YY" (e.g., "2024/25"). If \code{FALSE}, returns the start year
#'   as an integer (e.g., 2024).
#'
#' @return If \code{format = TRUE}, returns a character string in format "YYYY/YY".
#'   If \code{format = FALSE}, returns an integer representing the audit year's
#'   start year.
#'
#' @template AuditYear_quarters
#'
#' @family AuditYear functions
#' @export
#'
#' @examples
#' # Get current audit year (based on today's date)
#' get_AuditYear()
#'
#' # Get current audit year as integer
#' get_AuditYear(format = FALSE)
#'
#' # Determine audit year for specific dates
#' get_AuditYear(as.Date("2025-05-15"))  # Returns "2025/26"
#' get_AuditYear(as.Date("2025-02-15"))  # Returns "2024/25" (Q4)
#' get_AuditYear(as.Date("2025-04-01"))  # Returns "2025/26" (Q1 start)
#' get_AuditYear(as.Date("2025-03-31"))  # Returns "2024/26" (Q4 end)
#'
#' # Use in data processing
#' \dontrun{
#' library(dplyr)
#' df <- data.frame(
#'   admission_date = as.Date(c("2024-05-01", "2024-12-01", "2025-02-01"))
#' )
#'
#' df |> mutate(audit_year = get_AuditYear(admission_date))
#' }
get_AuditYear <- function(date = Sys.Date(), format = TRUE) {

  # 0. Preparation
  # Convert to Date if needed
  date <- as.Date(date)

  # Extract year and month
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))

  # 1. If month >= 4/April, current audit year starts this calendar year; else previous calendar year.
  # result <- if (m >= 4) y else y - 1
  # Vectorized properly with ifelse
  result <- ifelse(m >= 4, y, y - 1)

  # 2. If format==T, format as "YYYY/YY"
  if (format==TRUE) .formatAuditYear(result) else as.integer(result)

  # 3. Planned improvement: option to determine current audit quarter
}

# ========== 2. Generate Sequential List of Audit Years ==========
#' Generate Sequential List of Audit Years
#'
#' @description
#' Creates a sequential list of NPDA audit years between specified start and end years.
#' Useful for generating axis labels, filtering data by multiple audit years, or
#' creating comprehensive reports across multiple audit periods.
#'
#' @param startYear Integer. The first audit year to include in the sequence.
#'   Default is 2010 (when NPDA began).
#' @param endYear Integer. The last audit year to include in the sequence.
#'   Default is the current audit year (determined by \code{get_AuditYear(format = FALSE)}).
#' @param format Logical. If \code{TRUE} (default), returns audit years as formatted
#'   strings "YYYY/YY" (e.g., "2024/25"). If \code{FALSE}, returns integers
#'   representing each audit year's start year.
#'
#' @return If \code{format = TRUE}, returns a character vector of audit years in
#'   format "YYYY/YY". If \code{format = FALSE}, returns an integer vector of
#'   audit year start years.
#'
#' @details
#' This function generates all audit years from \code{startYear} to \code{endYear}
#' inclusive. It's particularly useful for:
#' \itemize{
#'   \item Creating dropdown menus or filters in Shiny applications
#'   \item Generating x-axis labels for time-series plots
#'   \item Iterating over multiple audit years in analysis pipelines
#'   \item Ensuring consistent audit year labeling across reports
#' }
#'
#' The NPDA began in the 2010/11 audit year,
#' hence the default start year of 2010.
#'
#' @seealso \code{\link{get_AuditYear}} for determining a single audit year from a date.
#'
#' @family AuditYear functions
#' @export
#'
#' @examples
#' # Get all audit years from 2010 to current (formatted)
#' get_AuditYears()
#'
#' # Get specific range of audit years
#' get_AuditYears(startYear = 2015, endYear = 2020)
#' # Returns: "2015/16" "2016/17" "2017/18" "2018/19" "2019/20" "2020/21"
#'
#' # Get unformatted (integer) audit years
#' get_AuditYears(startYear = 2018, endYear = 2020, format = FALSE)
#' # Returns: 2018 2019 2020
#'
#' # Use in plotting
#' \dontrun{
#' library(ggplot2)
#' audit_years <- get_AuditYears(2015, 2020)
#' ggplot(data.frame(year = audit_years, value = rnorm(6))) +
#'   aes(x = year, y = value) +
#'   geom_col() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#'
#' # Use in data filtering
#' library(dplyr)
#' recent_years <- get_AuditYears(startYear = 2020)
#' df |> filter(audit_year %in% recent_years)
#'
#' # Create lookup table
#' data.frame(
#'   audit_year = get_AuditYears(2010, 2015),
#'   audit_year_numeric = get_AuditYears(2010, 2015, format = FALSE)
#' )
#' #   audit_year audit_year_numeric
#' # 1    2010/11               2010
#' # 2    2011/12               2011
#' # 3    2012/13               2012
#' # 4    2013/14               2013
#' # 5    2014/15               2014
#' # 6    2015/16               2015
#' }
get_AuditYears <- function(startYear = 2010, endYear = get_AuditYear(format = FALSE), format = TRUE) {
  # 0. Input validation
  if (!is.numeric(startYear) || !is.numeric(endYear)) {
    stop("'startYear' and 'endYear' must be numeric values.")
  }

  if (startYear > endYear) {
    stop("'startYear' (", startYear, ") must be less than or equal to 'endYear' (", endYear, ").")
  }

  if (length(startYear) != 1 || length(endYear) != 1) {
    stop("'startYear' and 'endYear' must be single values, not vectors.")
  }

  # 1. Generate sequential list of audit year start years
  resultList <- seq.int(startYear, endYear)

  # 2. If format==T, format as "YYYY/YY"
  if (format == TRUE) {
    # vapply(resultList, .formatAuditYear, FUN.VALUE = character(1))
    .formatAuditYear(resultList)
  } else {
    as.integer(resultList)
  }
}
