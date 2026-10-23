# ========== Test Data Setup ==========
vec_easy <- c(5, 6, 2, 5, 1, 0, 0, NA)
vec_high <- c(5, 6, 4, 5, 3, 0, 0, NA)
vec_tricky <- c(5, 6, 4, 5, 1, 0, 0, NA)


# ========== Test basic usage ==========
test_that("get_masked handles basic masking correctly", {
  # Mask >= 2 positive values:
  # Mask positive values below the threshold (3)
  expect_equal(get_masked(vec_easy[1:5]),
               c("5", "6", "masked", "5", "masked"))
  # All values are above the 3 threshold, so no masking occurs
  expect_equal(get_masked(vec_high[1:5]),
               c("5", "6", "4", "5", "3"))
  # Secondary masking works:
  # Only 1 is positive and below the 3 threshold, so the second smallest value (4) is masked too
  expect_equal(get_masked(vec_tricky[1:5]),
               c("5", "6", "masked", "5", "masked"))
})

test_that("get_masked handles secondary masking with insufficient unique values", {
  # When there's only one unique positive value, no secondary masking possible
  result1 <- get_masked(c(0, 2, 2, 2), threshold = 3)
  expected1 <- c("0", "masked", "masked", "masked")
  expect_equal(result1, expected1)

  # When only zeros and one other value exist
  result2 <- get_masked(c(0, 0, 1), threshold = 3)
  expected2 <- c("0", "0", "masked") # Only 1 masked, no second smallest to mask
  expect_equal(result2, expected2)
})

test_that("get_masked handles edge cases", {
  # Single value
  expect_equal(get_masked(c(2)), c("masked"))
  expect_equal(get_masked(c(5)), c("5"))

  # Two values, one needs masking
  expect_equal(get_masked(c(1, 10), threshold = 3),
               c("masked", "masked"))

  # All same values below threshold
  expect_equal(get_masked(c(2, 2, 2), threshold = 3),
               c("masked", "masked", "masked"))

  # All same values above threshold
  expect_equal(get_masked(c(5, 5, 5), threshold = 3),
               c("5", "5", "5"))
})

test_that("get_masked always returns character vector", {
  # Even when no masking occurs, should return character
  result1 <- get_masked(c(10, 20, 30, NA))
  expect_type(result1, "character")
  expect_equal(result1, c("10", "20", "30", NA))

  # When masking occurs
  result2 <- get_masked(c(1, 20, 30, NA))
  expect_type(result2, "character")
  expect_true(all(is.character(result2)))
  expect_equal(result2, c("masked", "masked", "30", NA))
})

test_that("get_masked validates other input parameters", {
  # Non-numeric vector
  expect_error(get_masked(c("a", "b", "c")), "Input 'vec' must be a numeric vector without any negative value")
  expect_error(get_masked(c("a", 2, 3)), "Input 'vec' must be a numeric vector without any negative value")

  # Logical vector
  expect_error(get_masked(c(TRUE, FALSE)), "Input 'vec' must be a numeric vector without any negative value")

  # unfortunately, this mixed vector will be treated as numeric (TRUE = 1, FALSE = 0) and may cannot be caught
  # get_masked(c(1, 2, TRUE, FALSE))

  # Negative values
  expect_error(get_masked(c(1, -1, 5)), "Input 'vec' must be a numeric vector without any negative value")
  expect_error(get_masked(c(-0.5, NA, 5)), "Input 'vec' must be a numeric vector without any negative value")
})


# ========== Test mask label ==========
test_that("get_masked handles custom mask label", {
  expect_equal(get_masked(vec_easy[1:5], mask_label = "*"),
               c("5", "6", "*", "5", "*"))

  expect_equal(get_masked(vec_easy[1:5], mask_label = "<3"),
               c("5", "6", "<3", "5", "<3"))

  # Invalid mask_label
  expect_error(get_masked(vec_easy, mask_label = c("a", "b")), "'mask_label' must be a single character string")
  expect_error(get_masked(vec_easy, mask_label = 123), "'mask_label' must be a single character string")
})

# ========== Test mask thresholds ==========
test_that("get_masked handles different thresholds", {
  expect_equal(get_masked(vec_high[1:5], mask_label = "*", threshold = 3),
               c("5", "6", "4", "5", "3"))

  expect_equal(get_masked(vec_high[1:5], mask_label = "*", threshold = 4),
               c("5", "6", "*", "5", "*"))

  expect_equal(get_masked(vec_high[1:5], mask_label = "*", threshold = 5),
               c("5", "6", "*", "5", "*"))

  expect_equal(get_masked(vec_high[1:5], mask_label = "*", threshold = 6),
               c("*", "6", "*", "*", "*"))

  # Invalid threshold
  expect_error(get_masked(vec_easy, threshold = "1"), "'threshold' must be a single positive numeric value")
  expect_error(get_masked(vec_easy, threshold = 0), "'threshold' must be a single positive numeric value")
  expect_error(get_masked(vec_easy, threshold = c(2, 3)), "'threshold' must be a single positive numeric value")
  expect_error(get_masked(vec_easy, threshold = -1), "'threshold' must be a single positive numeric value")
})


# ========== Test zeros ==========
test_that("get_masked keeps zeros by default", {
  # Leaves zeros unmasked by default
  expect_equal(get_masked(vec_easy[1:6], mask_label = "*"),
               c("5", "6", "*", "5", "*", "0"))

  # Zero in secondary masking scenario
  expect_equal(get_masked(vec_easy[4:7], mask_label = "*"),
               c("*", "*", "0", "0"))
})

test_that("mask_zero = TRUE with only zeros has nothing else to mask", {
  expect_equal(get_masked(c(0, 0, 0), mask_label = "*", mask_zero = FALSE), c("0", "0", "0"))
  expect_equal(get_masked(c(0, 0, 0), mask_label = "*", mask_zero = TRUE), c("*", "*", "*"))
})

test_that("get_masked masks zeros when mask_zero = TRUE", {
  # mask_zero counts masked zeros towards the two masked values
  # A masked positive plus a masked zero is enough, no secondary masking
  expect_equal(get_masked(vec_easy[4:7], mask_label = "*", mask_zero = TRUE),
               c("5", "*", "*", "*"))

  expect_equal(get_masked(vec_easy[4:6], mask_label = "*", mask_zero = TRUE),
               c("5", "*", "*"))

  # Invalid mask_zero
  expect_error(get_masked(c(1, 2, 3), mask_zero = NA), "'mask_zero' must be TRUE or FALSE")
  expect_error(get_masked(c(1, 2, 3), mask_zero = "yes"), "'mask_zero' must be TRUE or FALSE")
  expect_error(get_masked(c(1, 2, 3), mask_zero = c(TRUE, FALSE)), "'mask_zero' must be TRUE or FALSE")
})

test_that("mask_zero = TRUE always masks at least one smallest positive value", {
  # When no positive below threshold: masked zeros alone would reveal the masked sum is 0,
  # so the smallest positive value is masked too
  # Single zero and a single smallest positive are enough
  expect_equal(get_masked(vec_high[1:7], mask_label = "*", mask_zero = TRUE),
               c("5", "6", "4", "5", "*", "*", "*"))
})

test_that("mask_zero = TRUE changes nothing when there are no zeros", {
  expect_equal(get_masked(vec_easy[1:5], mask_zero = TRUE),
               c("5", "6", "masked", "5", "masked"))
  expect_equal(get_masked(vec_high[1:5], mask_zero = TRUE),
               c("5", "6", "4", "5", "3"))
})

# ========== Test NAs ==========

test_that("get_masked leaves NAs unmasked by default", {
  # NAs do not affect secondary masking
  expect_equal(get_masked(vec1, mask_label = "*"),
               c("5", "6", "*", "5", "*", "0", "0", NA))
  expect_equal(get_masked(vec1, mask_label = "*", mask_zero = TRUE),
               c("5", "6", "4", "5", "*", "*", "*", NA))
})

test_that("mask_na = TRUE with only NAs has nothing else to mask", {
  expect_equal(get_masked(c(NA_real_, NA_real_), mask_label = "*", mask_na = FALSE),
               c(NA_character_, NA_character_))
  expect_equal(get_masked(c(NA_real_, NA_real_), mask_label = "*", mask_na = TRUE),
               c("*", "*"))
})

test_that("get_masked masks NAs when mask_na = TRUE", {
  expect_equal(get_masked(vec1, mask_label = "*", mask_na = TRUE),
               c("5", "6", "4", "5", "*", "0", "0", "*"))

  # Invalid mask_na
  expect_error(get_masked(c(1, 2, 3), mask_na = NA), "'mask_na' must be TRUE or FALSE")
  expect_error(get_masked(c(1, 2, 3), mask_na = "yes"), "'mask_na' must be TRUE or FALSE")
  expect_error(get_masked(c(1, 2, 3), mask_na = c(TRUE, FALSE)), "'mask_na' must be TRUE or FALSE")
})

test_that("mask_na counts masked NAs towards the two masked values", {
  # A masked positive plus a masked NA is enough, no secondary masking
  expect_equal(get_masked(c(1, 5, 6, 7, NA), mask_label = "*", mask_na = TRUE),
               c("*", "5", "6", "7", "*"))

  # No positive below threshold: masked NAs alone would reveal that nothing
  # is hidden, so the smallest positive value is masked too, however many NAs
  expect_equal(get_masked(c(NA, NA, 5, 6, 7), mask_label = "*", mask_na = TRUE),
               c("*", "*", "*", "6", "7"))

  # Same with zeros and NAs together
  expect_equal(get_masked(vec_high, mask_label = "*", mask_zero = TRUE, mask_na = TRUE),
               c("5", "6", "4", "5", "*", "*", "*", "*"))

  # Only one masked positive and a single masked zero or NA: no more is needed
  expect_equal(get_masked(c(0, NA, 2, 5, 8), mask_label = "*", mask_zero = TRUE, mask_na = TRUE),
               c("*", "*", "*", "5", "8"))
})

test_that("mask_na = TRUE changes nothing when there are no NAs", {
  expect_equal(get_masked(vec_easy[1:5], mask_na = TRUE),
               c("5", "6", "masked", "5", "masked"))
  expect_equal(get_masked(vec_high[1:5], mask_na = TRUE),
               c("5", "6", "4", "5", "3"))
})

