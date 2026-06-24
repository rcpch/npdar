#' Functions for Audit Year(s) Determination
#'
#' @description
#' Functions to determine current audit year and generate sequential lists of
#' audit years following NPDA fiscal year structure or your defined conventions.
#'
#' Audit years are formatted as "YYYY/YY" (e.g., "2024/25").
#'
#' \strong{Available Functions:}
#' \itemize{
#'   \item \code{\link{get_auditYear}}: Determine audit year & quarter from date(s)
#'   \item \code{\link{get_auditYears}}: Generate sequence of audit years
#' }
#'
#' @template auditYear_quarters
#'
#' @section Key Features:
#' \itemize{
#'   \item Fully vectorised - works with single dates or date vectors
#'   \item Integrates seamlessly with dplyr pipelines
#'   \item Handles edge cases (leap years, century boundaries)
#'   \item Optional integer or formatted string output
#' }
#'
#' @family auditYear functions
#' @name family_auditYear
#' @aliases auditYear auditYears
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
#' @family auditYear functions
#' @examples
#' \dontrun{
#' .formatAuditYear(2017)  # Returns "2017/18"
#' .formatAuditYear(2024)  # Returns "2024/25"
#' }
.formatAuditYear <- function(year) {
  paste0(year, "/", sprintf("%02d", (year + 1) %% 100))
}

# ========== 1. Get Current Audit Year ==========
#' Get Current Audit Year (& Quarters)
#'
#' @description
#' Determines the current audit year (& quarter) based on a given date.
#' As an example, the default is NPDA audit year, which runs from 1 April to 31 March,
#' so dates from January-March fall into the audit year that started
#' in the previous calendar year.
#'
#' @param date Date object or string that can be coerced to Date. The date for
#'   which to determine the audit year. Default is \code{Sys.Date()} (today).
#' @param start_month Integer. The month (1-12) that the audit year starts.
#'   Default is 4 (April, which is the NPDA default).
#' @param format Logical. If \code{TRUE} (default), returns audit year & quarter
#' as formatted string "YYYY/YY Qx" (e.g., "2024/25 Q3").
#' If \code{FALSE}, returns the start year as an integer (e.g., 2024).
#'
#' @return If \code{format = TRUE}, returns a character string in format "YYYY/YY".
#'   If \code{format = FALSE}, returns an integer representing the audit year's
#'   start year.
#'
#' @template auditYear_quarters
#'
#' @family auditYear functions
#' @export
#'
#' @examples
#' # Get current audit year (based on today's date)
#' get_auditYear()
#'
#' # Get current audit year as integer
#' get_auditYear(format = FALSE)
#'
#' # Customise Q1 start month
#' get_auditYear(start_month = 1)
#'
#' # Customise Q1 start month
#' get_AuditYear(start_month = 1)
#'
#' # Determine audit year for specific dates
#' get_auditYear("2025-05-15")  # Returns "2025/26 Q1"
#' get_auditYear("2025-02-15")  # Returns "2024/25 Q4"
#' get_auditYear("2025-04-01")  # Returns "2025/26 Q1"
#' get_auditYear("2025-03-31")  # Returns "2024/26 Q4"
#'
#' # Use in data processing
#' \dontrun{
#' library(dplyr)
#' df <- data.frame(
#'   admission_date = as.Date(c("2024-05-01", "2024-12-01", "2025-02-01"))
#' )
#'
#' df |> mutate(audit_year = get_auditYear(admission_date))
#' }
get_auditYear <- function(date = Sys.Date(), start_month = 4, format = TRUE) {

  # Step 0. Preparation
  # Validate start_month
  if (!is.numeric(start_month) || length(start_month) != 1 ||
      start_month %% 1 != 0 || start_month < 1 || start_month > 12) {
    stop("`start_month` must be a single integer between 1 and 12.", call. = FALSE)
  }

  # Convert to Date if needed
  date <- as.Date(date)

  # Extract year and month
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))

  # Step 1. Determine audit year
  # If month >= start_month (4/April, NPDA default), current audit year starts this calendar year; else previous calendar year.
  # audit_year <- if (m >= 4) y else y - 1
  # Vectorized properly with ifelse
  audit_year <- ifelse(m >= start_month, y, y - 1)

  # Step 2. If format==T, determine audit quarter & format as "YYYY/YY Qx"; else return integer year
  if (format==TRUE) {
    # Step 2a. Format year as "YYYY/YY"
    audit_year <- .formatAuditYear(audit_year)
    # Step 2b. Determine audit quarter
    audit_quarter <- ceiling(((m - start_month + 12) %% 12 + 1) / 3)
    result <- paste0(audit_year, " Q", audit_quarter)
  } else {
    result <- as.integer(audit_year)
  }

  result
}

# ========== 2. Generate Sequential List of Audit Years ==========
#' Generate Sequential List of Audit Years
#'
#' @description
#' Creates a sequential list of NPDA audit years between specified
#' start and end years. Useful for generating axis labels, filtering data by
#' multiple audit years, or creating comprehensive reports across multiple audit periods.
#'
#' @param startYear Integer. The first audit year to include in the sequence.
#'   Default is 2010 (when NPDA began).
#' @param endYear Integer. The last audit year to include in the sequence.
#'   Default is the current audit year (determined by \code{get_auditYear(format = FALSE)}).
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
#' @seealso \code{\link{get_auditYear}} for determining a single audit year from a date.
#'
#' @family AuditYear functions
#' @export
#'
#' @examples
#' # Get all audit years from 2010 to current (formatted)
#' get_auditYears()
#'
#' # Get specific range of audit years
#' get_auditYears(startYear = 2015, endYear = 2020)
#' # Returns: "2015/16" "2016/17" "2017/18" "2018/19" "2019/20" "2020/21"
#'
#' # Get unformatted (integer) audit years
#' get_auditYears(startYear = 2018, endYear = 2020, format = FALSE)
#' # Returns: 2018 2019 2020
#'
#' # Use in plotting
#' \dontrun{
#' library(ggplot2)
#' audit_years <- get_auditYears(2015, 2020)
#' ggplot(data.frame(year = audit_years, value = rnorm(6))) +
#'   aes(x = year, y = value) +
#'   geom_col() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#'
#' # Use in data filtering
#' library(dplyr)
#' recent_years <- get_auditYears(startYear = 2020)
#' df |> filter(audit_year %in% recent_years)
#'
#' # Create lookup table
#' data.frame(
#'   audit_year = get_auditYears(2010, 2015),
#'   audit_year_numeric = get_auditYears(2010, 2015, format = FALSE)
#' )
#' #   audit_year audit_year_numeric
#' # 1    2010/11               2010
#' # 2    2011/12               2011
#' # 3    2012/13               2012
#' # 4    2013/14               2013
#' # 5    2014/15               2014
#' # 6    2015/16               2015
#' }
get_auditYears <- function(startYear = 2010, endYear = get_auditYear(format = FALSE), format = TRUE) {
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
