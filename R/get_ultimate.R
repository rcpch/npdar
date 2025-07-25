#' Get the up-to-date/first/last mode or first/last entry from a vector
#' @description This function finds the most recent (i.e., up-to-date) mode, the first/last mode, or
#' the first/last entry from a vector, with options to exclude specific values.
#' Particularly useful for finding the most current valid values in longitudinal data.
#' \code{NA} values are always excluded by default.
#'
#' @param x A vector of values to be evaluated. Can be numeric, character, but POSIXct or date variable is currently not supported.
#' @param find Character string specifying what to find. Options are:
#'   \itemize{
#'     \item \code{"uptodate_mode"} - Among all modal values, returns the one that appears last in the vector
#'     \item \code{"first_mode"} - Among all modal values, returns the first mode in order of unique values
#'     \item \code{"last_mode"} - Among all modal values, returns the last mode in order of unique values
#'     \item \code{"first_entry"} - The first valid entry in the vector (after exclusions)
#'     \item \code{"last_entry"} - The last valid entry in the vector (after exclusions)
#'   }
#' @param except A value or vector of values to exclude from consideration.
#'   \code{NA} values are always excluded automatically. Can be a single value (e.g., 99, "Unknown", NA)
#'   or multiple values (e.g., \code{c(99, "Unknown")}). If not specified, a warning is issued.
#'
#' @param warning Logical. Whether to display warning messages. Warnings are shown when
#'   multiple modal values exist, showing the remaining modes apart from the one designated in \code{find},
#'   or when \code{except} is not specified. Default is \code{TRUE}.
#'
#' @returns The result based on the \code{find} and \code{except} parameters.
#'   If no valid values remain after exclusions, returns \code{NA}.
#'   When multiple modes exist and \code{warning = TRUE}, a warning message lists the
#'   other modal values not returned.
#'
#' @export
#'
#' @examples
#' # Example 1: Exclude invalid entries before finding the designated value
#' x <- c(1, 1, 2, 2, 2, 1, 99, 99, 99, 99, NA, NA, NA, NA)
#' get_ultimate(x, find="uptodate_mode")                       # Returns 99 (BAD)
#' get_ultimate(x, find="uptodate_mode", except=99)            # Return 1 NAs are excluded by default (GOOD)
#' get_ultimate(x, find="uptodate_mode", except=c(99, NA))     # Return 1 (GOOD)
#' # When all values are excluded
#' x <- c(99, 99, 99, 99, NA, NA, NA, NA)
#' get_ultimate(x, find='uptodate_mode')                       # Return 99 (BAD PRACTICE)
#' get_ultimate(x, find='uptodate_mode', except=99)            # Return NA (GOOD PRACTICE)
#' get_ultimate(c(9,9,9,9,9), find='uptodate_mode', except=9, warning=F)         # Return NA
#' get_ultimate(c(9,9,9,9,9), find='last_entry', except=9, warning=F)            # Return NA
#'
#' # Example 2: Finding up-to-date mode with categorical data
#' x <- c("F", "F", "Other", "Other", "Unknown", "Other", "M", "M", "M", "M",
#'         "F", "F", "Other", "Unknown", "Unknown", "Unknown")
#' get_ultimate(x, find = "uptodate_mode", except = "Unknown") # Returns "Other" (last occurrence) (GOOD)
#' get_ultimate(x, find = "uptodate_mode")                     # Returns "Unknown" (BAD)
#' get_ultimate(x, find = "first_mode", warning = FALSE)       # Returns "F" (first among unique modes)
#' get_ultimate(x, find = "last_mode", warning = FALSE)        # Returns "Unknown" (last among unique modes)
#'
#' # Example 3: Finding first/last valid entry
#' get_ultimate(x, find = "last_entry", warning = FALSE)                      # Returns "Unknown" (BAD)
#' get_ultimate(x, find = "last_entry", except = "Unknown", warning = FALSE)  # Returns "Other" (GOOD)
#' get_ultimate(x, find = "first_entry", except = "Unknown", warning = FALSE) # Returns "F"
#'

get_ultimate <- function(x, find, except, warning = TRUE) {
  # Extract unique values and their counts
  x <- x[!(is.na(x))]                       # Exclude NA by default

  if (missing(except)) {
    if (warning) {
      warning("NAs are excluded by default, but it's still good practice to specify the invalid values to exclude.")
    }
  } else {
    x <- x[!(x %in% except)]
  }

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

      if (warning) {
        remaining_modes <- modes[!(modes %in% return_value)]
        warning("There are multiple modes, the remaining modes are: ", paste(remaining_modes, collapse = ", "))
      }
    }
  } else {
    stop("Invalid 'find' argument. \n1. To find specific mode, use 'uptodate_mode', 'first_mode', or 'last_mode' \n2. To find specific entry, use 'last_entry' or 'first_entry'")
  }

  return(return_value)
}
