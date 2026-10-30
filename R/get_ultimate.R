#' Get the up-to-date/first/last mode or first/last entry from a vector
#'
#' @description This function finds the most recent (i.e., up-to-date) mode, the first/last mode, or
#' the first/last entry from a vector, with options to ignore specific values.
#' Particularly useful for finding the most current valid values in longitudinal data.
#' \code{NA} values are excluded by default (\code{na.rm = TRUE}); set \code{na.rm = FALSE}
#' when a missing value is meaningful and should be considered like any other value.
#'
#' @param x A vector of values to be evaluated. Can be numeric, character, but date (POSIXct) is currently not supported.
#' @param find Character string specifying what to find. Options are:
#'   \itemize{
#'     \item \code{"uptodate_mode"} - Among all modal values, returns the one that appears last in the vector
#'     \item \code{"first_mode"} - Among all modal values, returns the first mode in order of unique values
#'     \item \code{"last_mode"} - Among all modal values, returns the last mode in order of unique values
#'     \item \code{"first_entry"} - The first valid entry in the vector (after exclusions)
#'     \item \code{"last_entry"} - The last valid entry in the vector (after exclusions)
#'   }
#' @param exclude A value or vector of values to ignore/exclude from consideration.
#'   Can be a single value (e.g., 99, "Unknown") or multiple values (e.g., \code{c(99, "Unknown")}).
#'   Must not contain \code{NA} (an error is thrown); use \code{na.rm} to control how \code{NA}s are handled.
#'   Default is \code{NULL} (nothing is excluded).
#'
#' @param na.rm Logical. Whether to exclude \code{NA}s before evaluating.
#' Default is \code{TRUE}. If \code{FALSE}, \code{NA} will be considered
#' as a valid candidate and treated like any other value.
#'
#' @param warn Logical. Whether to display warning messages. Warnings are shown when
#'   multiple modal values exist, showing the remaining modes apart from the one designated in \code{find},
#'   or when \code{x} contains \code{NA}s and \code{na.rm} is not specified (they are removed by default).
#'   Default is \code{TRUE}.
#'
#' @returns The result based on the \code{find}, \code{exclude} and \code{na.rm} parameters.
#'   If no valid values remain after exclusions, returns \code{NA}. With \code{na.rm = FALSE},
#'   \code{NA} is also returned when it is itself the selected mode or entry.
#'   When multiple modes exist and \code{warn = TRUE}, a warning message lists the
#'   other modal values not returned.
#'
#' @export
#'
#' @examples
#' # Example 1: Exclude invalid entries before finding the designated value
#' x <- c(1, 1, 2, 2, 2, 1, 99, 99, 99, 99, NA, NA, NA, NA)
#' get_ultimate(x, find = "uptodate_mode")  # 99
#' get_ultimate(x, find = "uptodate_mode",
#'              exclude = 99)               # 1, NAs excluded by default
#' get_ultimate(x, find = "uptodate_mode",
#'              exclude = 99, na.rm=FALSE)      # NA, if NAs are meaningful
#' # When all values are excluded
#' x <- c(99, 99, 99, 99, NA, NA, NA, NA)
#' get_ultimate(x, find = 'uptodate_mode')  # 99 (BAD PRACTICE)
#' get_ultimate(x, find = 'uptodate_mode',
#'              exclude = 99)               # NA (GOOD PRACTICE)
#' get_ultimate(c(9,9,9,9,9), find='uptodate_mode',
#'              exclude = 9, warn = FALSE)        # NA
#' get_ultimate(c(9,9,9,9,9), find='last_entry',
#'              exclude = 9, warn = FALSE)        # NA
#'
#' # Example 2: Finding up-to-date mode with categorical data
#' x <- c("F", "F", "Other", "Other", "Unknown",
#'         "Other", "M", "M", "M", "M", "F", "F",
#'         "Other", "Unknown", "Unknown", "Unknown")
#' get_ultimate(x, find = "uptodate_mode",
#'              exclude = "Unknown")       # "Other" (last occurrence) (GOOD)
#' get_ultimate(x, find = "uptodate_mode") # "Unknown" (BAD)
#' get_ultimate(x, find = "first_mode",
#'              warn = FALSE)              # "F" (first among unique modes)
#' get_ultimate(x, find = "last_mode",
#'              warn = FALSE)           # "M" (last among unique modes)
#'
#' # Example 3: Finding first/last valid entry
#' get_ultimate(x, find = "last_entry",
#'              warn = FALSE)           # "Unknown" (BAD)
#' get_ultimate(x, find = "last_entry", exclude = "Unknown",
#'              warn = FALSE)           # "Other" (GOOD)
#' get_ultimate(x, find = "first_entry", exclude = "Unknown",
#'              warn = FALSE)           # "F"
#'
get_ultimate <- function(x, find, exclude = NULL, warn = TRUE, na.rm = TRUE) {
  if (anyNA(exclude)) {
    stop("`exclude` must not contain NA. Use `na.rm` to control how NAs are handled.")
  }

  # Extract unique values and their counts
  if (na.rm) {
    if (warn && missing(na.rm) && anyNA(x)) {
      warning("NAs are removed by default with `na.rm = TRUE`")
    }
    x <- x[!(is.na(x))]
  }

  x <- x[!(x %in% exclude)]

  ux <- unique(x)

  # If vector is empty after filtering, just return NA and that's it
  if (length(x) == 0) {
    return(NA)
  }

  if (find == 'last_entry') {                                                   # 1. Find the last/first entry
    # Find the last entry
    return_value <- x[length(x)]                                                # Equivalent to python [-1]
  } else if (find == 'first_entry') {
    # Find the first valid value
    return_value <- x[1]
  } else if (find %in% c("first_mode", "last_mode", "uptodate_mode")) {         # 2. Find the first/last/uptodate mode
    # Find all modes
    # Indices of modes
    nx <- tabulate(match(x, ux))
    modes <- ux[which(nx == max(nx))]

    if (length(modes) == 1) {
      # Find the only mode
      return_value <- modes
    } else {
      # Find first/last/up-to-date mode based on 'find' argument
      if (find == 'first_mode') {
        # First mode
        return_value <- modes[1]
      } else if (find == 'last_mode') {
        # Last mode
        return_value <- modes[length(modes)]                                    # Equivalent to python [-1]
      } else if (find == "uptodate_mode") {
        # Latest value that is also mode
        for (value in rev(x)) {
          if (value %in% modes) {
            return_value <- value
            break
          }}
      }

      if (warn) {
        remaining_modes <- modes[!(modes %in% return_value)]
        warning("There are multiple modes, the remaining modes are: ", paste(remaining_modes, collapse = ", "))
      }
    }
  } else {
    stop("Invalid 'find' argument. \n1. To find specific mode, use 'uptodate_mode', 'first_mode', or 'last_mode' \n2. To find specific entry, use 'last_entry' or 'first_entry'")
  }

  return(return_value)
}
