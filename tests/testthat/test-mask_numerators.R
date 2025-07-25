test_that("mask_numerators handles basic masking correctly", {
  # Example from documentation: c(4, 6, 2, 4, 0)
  # Only 2 should be masked (2 < 3 and > 0)
  result1 <- mask_numerators(c(4, 6, 2, 4, 0))
  expected1 <- c("masked", "6", "masked", "masked", "0")
  expect_equal(result1, expected1)

  # Test with custom threshold
  result2 <- mask_numerators(c(4, 6, 2, 4, 1), maxNum = 3)
  # Both 1 and 2 are masked, so no secondary masking needed
  expected2 <- c("4", "6", "masked", "4", "masked")
  expect_equal(result2, expected2)
})

test_that("mask_numerators applies secondary masking rule correctly", {
  # When only one value is masked, second smallest should also be masked
  # Example: c(5, 6, 2, 7, 8) with maxNum = 3
  # Only 2 is masked initially, so second smallest (5) should also be masked
  result1 <- mask_numerators(c(5, 6, 2, 7, 8), maxNum = 3)
  expected1 <- c("masked", "6", "masked", "7", "8")
  expect_equal(result1, expected1)

  # Another example: c(10, 1, 15, 20) with maxNum = 3
  # Only 1 is masked initially, so second smallest (10) should also be masked
  result2 <- mask_numerators(c(10, 1, 15, 20), maxNum = 3)
  expected2 <- c("masked", "masked", "15", "20")
  expect_equal(result2, expected2)
})

test_that("mask_numerators handles custom mask messages", {
  # Test with asterisk
  result1 <- mask_numerators(c(4, 6, 2, 4, 3), maxNum = 3, maskMessage = "*")
  expected1 <- c("4", "6", "*", "4", "*") # Both 2 and 3 are masked, no secondary masking
  expect_equal(result1, expected1)

  # Test with different message
  result2 <- mask_numerators(c(5, 1, 8), maxNum = 3, maskMessage = "<5")
  expected2 <- c("<5", "<5", "8") # 1 masked initially, then 5 (second smallest) masked
  expect_equal(result2, expected2)
})

test_that("mask_numerators returns original vector when no masking needed", {
  # All values >= maxNum or equal to 0
  result1 <- mask_numerators(c(5, 6, 7, 8, 0))
  expected1 <- c("5", "6", "7", "8", "0")
  expect_equal(result1, expected1)

  # All values are 0
  result2 <- mask_numerators(c(0, 0, 0))
  expected2 <- c("0", "0", "0")
  expect_equal(result2, expected2)

  # All values >= threshold
  result3 <- mask_numerators(c(10, 15, 20), maxNum = 5)
  expected3 <- c("10", "15", "20")
  expect_equal(result3, expected3)
})

test_that("mask_numerators handles zero values correctly", {
  # Zeros should never be masked
  result1 <- mask_numerators(c(0, 1, 2, 0, 3), maxNum = 3)
  # 1 and 2 are masked, no secondary masking needed
  expected1 <- c("0", "masked", "masked", "0", "3")
  expect_equal(result1, expected1)

  # Zero in secondary masking scenario
  result2 <- mask_numerators(c(0, 2, 5, 8), maxNum = 3)
  # Only 2 masked initially, second smallest excluding 0 is 5
  expected2 <- c("0", "masked", "masked", "8")
  expect_equal(result2, expected2)
})

test_that("mask_numerators handles edge cases", {
  # Single value
  expect_equal(mask_numerators(c(2)), c("masked"))
  expect_equal(mask_numerators(c(5)), c("5"))

  # Two values, one needs masking
  result1 <- mask_numerators(c(1, 10), maxNum = 3)
  expected1 <- c("masked", "masked") # 1 masked, then 10 (second smallest) masked
  expect_equal(result1, expected1)

  # All same values below threshold
  result2 <- mask_numerators(c(2, 2, 2), maxNum = 3)
  expected2 <- c("masked", "masked", "masked")
  expect_equal(result2, expected2)

  # All same values above threshold
  result3 <- mask_numerators(c(5, 5, 5), maxNum = 3)
  expected3 <- c("5", "5", "5")
  expect_equal(result3, expected3)
})

test_that("mask_numerators handles secondary masking with insufficient unique values", {
  # When there's only one unique non-zero value, no secondary masking possible
  result1 <- mask_numerators(c(0, 2, 2, 2), maxNum = 3)
  expected1 <- c("0", "masked", "masked", "masked")
  expect_equal(result1, expected1)

  # When only zeros and one other value exist
  result2 <- mask_numerators(c(0, 0, 1), maxNum = 3)
  expected2 <- c("0", "0", "masked") # Only 1 masked, no second smallest to mask
  expect_equal(result2, expected2)
})

test_that("mask_numerators validates input parameters", {
  # Non-numeric vector
  expect_error(mask_numerators(c("a", "b", "c")), "Input 'vec' must be a numeric vector")

  # Invalid maxNum
  expect_error(mask_numerators(c(1, 2, 3), maxNum = 0), "'maxNum' must be a single positive numeric value")
  expect_error(mask_numerators(c(1, 2, 3), maxNum = c(2, 3)), "'maxNum' must be a single positive numeric value")
  expect_error(mask_numerators(c(1, 2, 3), maxNum = -1), "'maxNum' must be a single positive numeric value")

  # Invalid maskMessage
  expect_error(mask_numerators(c(1, 2, 3), maskMessage = c("a", "b")), "'maskMessage' must be a single character string")
  expect_error(mask_numerators(c(1, 2, 3), maskMessage = 123), "'maskMessage' must be a single character string")
})

test_that("mask_numerators works with different maxNum thresholds", {
  vec <- c(1, 2, 3, 4, 5, 6)

  # maxNum = 2: only 1 gets masked, then second smallest (2) gets masked
  result1 <- mask_numerators(vec, maxNum = 2)
  expected1 <- c("masked", "masked", "3", "4", "5", "6")
  expect_equal(result1, expected1)

  # maxNum = 4: 1, 2, 3 get masked (multiple), no secondary masking
  result2 <- mask_numerators(vec, maxNum = 4)
  expected2 <- c("masked", "masked", "masked", "4", "5", "6")
  expect_equal(result2, expected2)

  # maxNum = 7: all values except 0 would be masked
  result3 <- mask_numerators(vec, maxNum = 7)
  expected3 <- c("masked", "masked", "masked", "masked", "masked", "masked")
  expect_equal(result3, expected3)
})

test_that("mask_numerators always returns character vector", {
  # Even when no masking occurs, should return character
  result1 <- mask_numerators(c(10, 20, 30))
  expect_type(result1, "character")
  expect_equal(result1, c("10", "20", "30"))

  # When masking occurs
  result2 <- mask_numerators(c(1, 10, 20))
  expect_type(result2, "character")
  expect_true(all(is.character(result2)))
})
