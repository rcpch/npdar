#' Mask small numerators for sensitive data privacy
#'
#' @description This function masks small numerator values in sensitive data to protect privacy
#' by preventing potential identification of individuals. It applies two key rules:
#' (1) mask any numerators in the range (0, maxNum), and (2) if only one numerator is masked,
#' also mask all occurrences of the second smallest value to prevent back-calculation.
#'
#' @param vec A numeric vector of count data (numerators) to be masked.
#' @param maxNum Numeric. The threshold value for masking. Values greater than 0
#'   and less than \code{maxNum} will be masked. Default is 3.
#' @param maskMessage Character string. The message to replace masked values with.
#'   Default is "masked".
#'
#' @return A character vector with small values replaced by the mask message.
#'   If no values need masking, returns the original vector converted to character.
#'
#' @details
#' The function implements a two-step masking process:
#' \enumerate{
#'   \item \strong{Initial masking}: Any values in the range (0, maxNum) are masked
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
#' mask_numerators(example_vec1)  # Returns: "4" "6" "masked" "4" "0"
#'
#' # With different threshold
#' example_vec2 <- c(4, 6, 2, 4, 1)
#' mask_numerators(example_vec2, maxNum = 3)  # Masks values 1 and 2, plus second smallest (4)
#'
#' # Custom mask message
#' example_vec3 <- c(4, 6, 2, 4, 3)
#' mask_numerators(example_vec3, maxNum = 3, maskMessage = "*")
#'
#' # Demonstrating secondary masking rule
#' example_vec4 <- c(4, 6, 3, 4, 3)
#' mask_numerators(example_vec4, maxNum = 3)  # Only value 2 would be masked initially
#'
#' # Higher threshold
#' example_vec5 <- c(4, 6, 3, 4, 3)
#' mask_numerators(example_vec5, maxNum = 5, maskMessage = "*")  # Masks 3s and 4s
#'
#' # No masking needed
#' example_vec6 <- c(5, 6, 7, 8, 0)
#' mask_numerators(example_vec6)  # Returns original as characters: "5" "6" "7" "8" "0"
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
#'     numerator_masked = mask_numerators(numerator, maxNum = 3),
#'     proportion_masked = ifelse(numerator_masked == 'masked', 'masked', proportion)
#'   )
#' }
mask_numerators <- function(vec, maxNum = 3, maskMessage = 'masked') {
  # Input validation
  if (!is.numeric(vec)) {
    stop("Input 'vec' must be a numeric vector")
  }
  if (!is.numeric(maxNum) || length(maxNum) != 1 || maxNum <= 0) {
    stop("'maxNum' must be a single positive numeric value")
  }
  if (!is.character(maskMessage) || length(maskMessage) != 1) {
    stop("'maskMessage' must be a single character string")
  }

  # Step 1: Mask any values within the range (0, maxNum)
  vec_masked <- ifelse(vec > 0 & vec < maxNum, maskMessage, vec)

  # Count how many values were masked currently
  num_masked <- sum(vec_masked == maskMessage)

  # Step 2: If no values within range (0, maxNum), return the original vector
  if (num_masked == 0) {
    return(as.character(vec)) # Convert to character for consistency
  }

  # Step 3: If only 1 value was masked, mask all occurrences of the second smallest
  if (num_masked == 1) {
    # Get unique values excluding 0
    unique_sorted <- unique(sort(vec)) # Sort and get all unique values
    unique_sorted <- unique_sorted[unique_sorted != 0] # Exclude 0

    # Check if there's a second smallest value to mask
    if (length(unique_sorted) >= 2) {
      # Get the second smallest value (excluding 0)
      second_smallest <- unique_sorted[2]
      # Mask all occurrences (use original vec, not vec_masked which is now character)
      vec_masked[vec == second_smallest] <- maskMessage
    }
  }

  return(vec_masked)
}
