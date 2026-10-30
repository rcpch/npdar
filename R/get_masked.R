#' Mask small numerators for sensitive data privacy
#'
#' @description Masks small numerators (counts) to protect patient privacy when the
#' corresponding denominator (the total) is also shown. It applies two rules:
#' (1) mask any numerator in the range (0, threshold), optionally together with zeros
#' and \code{NA}s, and (2) make sure at least two values are masked, at least one of
#' them positive, by also masking the next smallest positive value(s) if needed.
#'
#' Rule 2 is the reason this function exists. If a denominator is published, a single
#' masked numerator can be recovered by subtracting the visible numerators from the
#' total. Masking at least two values, one of them positive, leaves the individual
#' values undetermined. If the denominator is not shown, a simple \code{ifelse()} on the
#' threshold should suffice and there's no need to use this function.
#'
#' @param vec A numeric vector of count data (numerators) to be masked. Only
#'   values \code{>= 0} and \code{NA} are accepted; negative values and
#'   non-numeric input throw an error. Note that a mixed vector such as
#'   \code{c(1, 2, TRUE, FALSE)} is coerced to numeric by R
#'   before reaching the function, so it will be silently allowed.
#' @param threshold Numeric. The threshold value for masking. Values greater than 0
#'   and less than \code{threshold} will be masked. As an example, the NPDA usually uses 3.
#' @param mask_label Character string. The message to replace masked values with.
#'   Default is "masked".
#' @param mask_zero Logical. If \code{TRUE}, zeros are masked as well. Default is
#'   \code{FALSE}, because a zero conveys a meaningful and positive message
#'   (e.g. no patients at this hospital has the disease) and does not put privacy at risk.
#' @param mask_na Logical. If \code{TRUE}, \code{NA}s are masked as well. Default is
#'   \code{FALSE}, because an \code{NA} conveys a meaningful message (e.g. the hospital has
#'   unknown number of patients with the disease) and does not put privacy at risk.
#'
#' @return A character vector with masked values replaced by \code{mask_label}.
#'   If no values need masking, returns the original vector converted to character.
#'   Unmasked \code{NA}s are returned as \code{NA_character_}.
#'
#' @details
#' The function implements a two-step masking process:
#' \enumerate{
#'   \item \strong{Initial masking}: Any values in the range (0, threshold) are masked,
#'         plus zeros if \code{mask_zero = TRUE} and \code{NA}s if \code{mask_na = TRUE}.
#'   \item \strong{Secondary masking}: If anything was masked but fewer than two
#'         values were, or none of them is positive, the smallest unmasked positive
#'         value(s) are also masked, all occurrences at a time, until at least two
#'         values are masked and at least one is positive (or none are left).
#' }
#'
#' \strong{Masked zeros and \code{NA}s:}
#' When \code{mask_zero} and \code{mask_na} are \code{TRUE},
#' zeros and \code{NA}s count towards the minimum of two masked values but
#' cannot be the only ones. They add nothing to the total, so if only they were masked,
#' subtracting the visible numerators from the denominator would reveal that every
#' masked value is 0 or unknown, which is why the smallest positive value(s) must also be masked.
#'
#' @note
#' The function always returns a character vector to facilitate merging with other
#' masked datasets that may contain non-numeric mask messages.
#'
#' @export
#'
#' @examples
#' # Positive values less than the threshold and the second smallest positive value are masked
#' vec1 <- c(5, 6, 4, 5, 1, 0, 0, NA)
#' get_masked(vec1, threshold = 3)
#'
#' # Custom mask message
#' get_masked(vec1, threshold = 3, mask_label = "*")
#'
#' # No masking if all positive values are above the threshold
#' vec2 <- c(5, 6, 4, 5, 3, 0, 0, NA)
#' get_masked(vec2, mask_label = "*", threshold = 3)
#'
#' # Custom threshold
#' get_masked(vec2, mask_label = "*", threshold = 4)
#'
#' # Zeros are kept by default; mask_zero = TRUE masks them and the smallest positive values too
#' get_masked(vec1, threshold = 3, mask_label = "*", mask_zero = TRUE)
#'
#' # NAs are kept by default; mask_na = TRUE masks them and the smallest positive values too
#' get_masked(vec1, threshold = 3, mask_label = "*", mask_na = TRUE)
#' get_masked(vec1, threshold = 3, mask_label = "*", mask_zero = TRUE, mask_na = TRUE)
#'
#' # Realistic workflow example
#' \dontrun{
#' library(dplyr)
#' library(tibble)
#'
#' # Create example demographic data
#' set.seed(1234)
#' dat <- tibble(
#'   hospital_id = sample(8, size = 100, replace = TRUE),
#'   condition = sample(c("adhd", "asd", "dyslexia","prefer not to say"),
#'                      size = 100, replace = TRUE)
#' )
#' # Apply masking to grouped counts
#' results <- dat |>
#'   group_by(hospital_id, condition) |>
#'   summarise(numerator = n(), .groups = "drop") |>
#'   mutate(
#'     denomiantor = sum(numerator),
#'     percent = sprintf("%.1f%%", 100 * numerator / denomiantor),
#'     numerator_masked = get_masked(numerator, threshold = 3, mask_label = '*'),
#'     percent_masked = ifelse(numerator_masked == '*', '*', percent)
#'   )
#' }
get_masked <- function(vec, threshold, mask_label = 'masked', mask_zero = FALSE, mask_na = FALSE) {
  # Input validation
  if (!is.numeric(vec) || any(vec < 0, na.rm = TRUE)) {
    stop("Input 'vec' must be a numeric vector without any negative value")
  }
  
  if (missing(threshold) || !is.numeric(threshold) || length(threshold) != 1 || threshold <= 0) {
    stop("'threshold' must be a single positive numeric value")
  }
  if (!is.character(mask_label) || length(mask_label) != 1) {
    stop("'mask_label' must be a single character string")
  }
  if (!is.logical(mask_zero) || length(mask_zero) != 1 || is.na(mask_zero)) {
    stop("'mask_zero' must be TRUE or FALSE")
  }
  if (!is.logical(mask_na) || length(mask_na) != 1 || is.na(mask_na)) {
    stop("'mask_na' must be TRUE or FALSE")
  }

  is_known <- !is.na(vec)

  # Step 1: Mask positive values below the threshold, and zeros / NAs if requested
  is_positive <- is_known & vec > 0
  is_to_mask <- is_positive & vec < threshold
  if (mask_zero) {
    is_to_mask <- is_to_mask | (is_known & vec == 0)
  }
  if (mask_na) {
    is_to_mask <- is_to_mask | !is_known
  }

  # Step 2: Ensure at least two values are masked, at least one of them positive
  needs_more <- function() sum(is_to_mask) < 2 || !any(is_to_mask & is_positive)
  if (any(is_to_mask) && needs_more()) {
    for (value in sort(unique(vec[is_positive & !is_to_mask]))) {
      is_to_mask[is_known & vec == value] <- TRUE
      if (!needs_more()) break
    }
  }

  # Step 3: Apply masking
  vec_masked <- as.character(vec)
  vec_masked[is_to_mask] <- mask_label
  vec_masked
}
