#' Mask small numerators for sensitive data privacy
#'
#' @description This function masks small numerator values in sensitive data to protect privacy
#' by preventing potential identification of individuals. It applies two key rules:
#' (1) mask any numerators in the range (0, threshold), and (2) if only one numerator is masked,
#' also mask all occurrences of the second smallest value to prevent back-calculation.
#'
#' @param vec A numeric vector of count data (numerators) to be masked.
#' @param threshold Numeric. The threshold value for masking. Values greater than 0
#'   and less than \code{threshold} will be masked. Default is 3.
#' @param mask_label Character string. The message to replace masked values with.
#'   Default is "masked".
#'
#' @return A character vector with small values replaced by the mask message.
#'   If no values need masking, returns the original vector converted to character.
#'
#' @details
#' The function implements a two-step masking process:
#' \enumerate{
#'   \item \strong{Initial masking}: Any values in the range (0, threshold) are masked
#'   \item \strong{Secondary masking}: If only one value was masked in step 1,
#'         all occurrences of the second smallest value (excluding 0) are also masked
#'         to prevent back-calculation of the original masked value
#' }
#'
#' This approach prevents scenarios where having only one masked value would allow
#' users to calculate the masked value by subtraction from known totals.
#'
#' @note
#' Zero values are never masked as they typically represent legitimate absence of cases.
#' The function always returns a character vector to facilitate merging with other
#' masked datasets that may contain non-numeric mask messages.
#'
#' @export
#'
#' @examples
#' # Basic usage - mask values less than 3
#' example_vec1 <- c(4, 6, 2, 4, 0)
#' get_masked(example_vec1)  # Returns: "4" "6" "masked" "4" "0"
#'
#' # With different threshold
#' example_vec2 <- c(4, 6, 2, 4, 1)
#' get_masked(example_vec2, threshold = 3)  # Masks values 1 and 2, plus second smallest (4)
#'
#' # Custom mask message
#' example_vec3 <- c(4, 6, 2, 4, 3)
#' get_masked(example_vec3, threshold = 3, mask_label = "*")
#'
#' # Demonstrating secondary masking rule
#' example_vec4 <- c(4, 6, 3, 4, 3)
#' get_masked(example_vec4, threshold = 3)  # Only value 2 would be masked initially
#'
#' # Higher threshold
#' example_vec5 <- c(4, 6, 3, 4, 3)
#' get_masked(example_vec5, threshold = 5, mask_label = "*")  # Masks 3s and 4s
#'
#' # No masking needed
#' example_vec6 <- c(5, 6, 7, 8, 0)
#' get_masked(example_vec6)  # Returns original as characters: "5" "6" "7" "8" "0"
#'
#' # Realistic workflow example
#' \dontrun{
#' library(dplyr)
#' library(tibble)
#'
#' # Create example demographic data
#' set.seed(1234)
#' dat <- tibble(
#'   unit_number = sample(8, size = 100, replace = TRUE),
#'   gender = sample(c("boy", "girl", "non-binary", "prefer not to say"),
#'                   size = 100, replace = TRUE)
#' )
#'
#' # Apply masking to grouped counts
#' results <- dat |>
#'   group_by(unit_number, gender) |>
#'   summarise(numerator = n(), .groups = "drop") |>
#'   mutate(
#'     proportion = sprintf("%.1f%%", 100 * numerator / sum(numerator)),
#'     numerator_masked = get_masked(numerator, threshold = 3),
#'     proportion_masked = ifelse(numerator_masked == 'masked', 'masked', proportion)
#'   )
#' }
get_masked <- function(vec, threshold = 3, mask_label = 'masked', mask_zero = FALSE, mask_na = FALSE) {
  # Input validation
  if (!is.numeric(vec) || any(vec < 0, na.rm = TRUE)) {
    stop("Input 'vec' must be a numeric vector without any negative value")
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold <= 0) {
    stop("'threshold' must be a single positive numeric value")
  }
  if (!is.character(mask_label) || length(mask_label) != 1) {
    stop("'mask_label' must be a single character string")
  }
  if (!is.logical(mask_zero) || length(mask_zero) != 1 || is.na(mask_zero)) {
    stop("'mask_zero' must be TRUE or FALSE")
  }

  is_known <- !is.na(vec)

  # Step 1: Mask positive values below the threshold, and zeros / NAs if requested
  is_positive <- is_known & vec > 0
  is_to_mask <- is_positive & vec < threshold
  if (mask_zero) {
    is_to_mask <- is_to_mask | (is_known & vec == 0)
  }
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
