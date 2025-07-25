test_that("get_ultimate handles numeric data with exclusions correctly", {
  x <- c(1, 1, 2, 2, 2, 1, 99, 99, 99, 99, NA, NA, NA, NA)

  # Test uptodate_mode
  expect_equal(get_ultimate(x, find = "uptodate_mode", warning = FALSE), 99)
  expect_equal(get_ultimate(x, find = "uptodate_mode", except = 99, warning = FALSE), 2)
  expect_equal(get_ultimate(x, find = "uptodate_mode", except = c(99, NA), warning = FALSE), 2)

  # Test with all values excluded
  x2 <- c(99, 99, 99, 99, NA, NA, NA, NA)
  expect_equal(get_ultimate(x2, find = "uptodate_mode", except = 99, warning = FALSE), NA)

  # Test with single repeated value excluded
  expect_equal(get_ultimate(c(9,9,9,9,9), find = "uptodate_mode", except = 9, warning = FALSE), NA)
  expect_equal(get_ultimate(c(9,9,9,9,9), find = "last_entry", except = 9, warning = FALSE), NA)
})

test_that("get_ultimate handles categorical data correctly", {
  x3 <- c("F", "F", "Other", "Other", "Unknown", "Other", "M", "M", "M", "M",
          "F", "F", "Other", "Unknown", "Unknown", "Unknown")

  # Test different mode types
  expect_equal(get_ultimate(x3, find = "uptodate_mode", except = "Unknown", warning = FALSE), "Other")
  expect_equal(get_ultimate(x3, find = "uptodate_mode", warning = FALSE), "Unknown")
  expect_equal(get_ultimate(x3, find = "first_mode", warning = FALSE), "F")
  expect_equal(get_ultimate(x3, find = "last_mode", warning = FALSE), "Unknown")
})

test_that("get_ultimate handles first_entry and last_entry correctly", {
  x3 <- c("F", "F", "Other", "Other", "Unknown", "Other", "M", "M", "M", "M",
          "F", "F", "Other", "Unknown", "Unknown", "Unknown")

  # Test entry finding
  expect_equal(get_ultimate(x3, find = "last_entry", warning = FALSE), "Unknown")
  expect_equal(get_ultimate(x3, find = "last_entry", except = "Unknown", warning = FALSE), "Other")
  expect_equal(get_ultimate(x3, find = "first_entry", except = "Unknown", warning = FALSE), "F")
})

test_that("get_ultimate handles NA values correctly", {
  # Test that NAs are excluded by default
  x_with_na <- c(1, 2, 2, NA, NA)
  expect_equal(get_ultimate(x_with_na, find = "uptodate_mode", warning = FALSE), 2)
  expect_equal(get_ultimate(x_with_na, find = "last_entry", warning = FALSE), 2)
  expect_equal(get_ultimate(x_with_na, find = "first_entry", warning = FALSE), 1)

  # Test with only NAs
  expect_equal(get_ultimate(c(NA, NA, NA), find = "uptodate_mode", warning = FALSE), NA)
})

test_that("get_ultimate produces appropriate warnings", {
  # Test warning when except is not specified
  expect_warning(
    get_ultimate(c(1, 2, 3), find = "uptodate_mode"),
    "NAs are excluded by default, but it's still good practice to specify the invalid values to exclude."
  )

  # Test warning for multiple modes
  x_multi_mode <- c(1, 1, 2, 2, 3, 3)
  expect_warning(
    get_ultimate(x_multi_mode, find = "uptodate_mode", except = NA),
    "There are multiple modes, the remaining modes are:"
  )

  # Test no warning when warning = FALSE
  expect_silent(get_ultimate(c(1, 2, 3), find = "uptodate_mode", warning = FALSE))
  expect_silent(get_ultimate(x_multi_mode, find = "uptodate_mode", except = NA, warning = FALSE))
})

test_that("get_ultimate handles edge cases", {
  # Empty vector after filtering
  expect_equal(get_ultimate(c(1, 1, 1), find = "uptodate_mode", except = 1, warning = FALSE), NA)

  # Single value
  expect_equal(get_ultimate(c(5), find = "uptodate_mode", warning = FALSE), 5)
  expect_equal(get_ultimate(c(5), find = "first_entry", warning = FALSE), 5)
  expect_equal(get_ultimate(c(5), find = "last_entry", warning = FALSE), 5)

  # Single value after exclusions
  expect_equal(get_ultimate(c(1, 2, 2, 2), find = "uptodate_mode", except = 2, warning = FALSE), 1)
})

test_that("get_ultimate throws error for invalid find argument", {
  expect_error(
    get_ultimate(c(1, 2, 3), find = "invalid_option"),
    "Invalid 'find' argument"
  )

  expect_error(
    get_ultimate(c(1, 2, 3), find = "mode"),
    "Invalid 'find' argument"
  )
})

test_that("get_ultimate handles different data types", {
  # Test with factors
  x_factor <- factor(c("A", "B", "B", "C", "C", "C"))
  expect_equal(get_ultimate(x_factor, find = "uptodate_mode", warning = FALSE), factor("C", levels = c("A", "B", "C")))

  # Test with logical
  x_logical <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
  expect_equal(get_ultimate(x_logical, find = "uptodate_mode", warning = FALSE), TRUE)

  # Test with mixed numeric (integers and doubles)
  x_mixed <- c(1L, 2.0, 2.0, 1L)
  expect_equal(get_ultimate(x_mixed, find = "uptodate_mode", warning = FALSE), 1)
})

test_that("get_ultimate uptodate_mode logic works correctly", {
  # Test that uptodate_mode finds the modal value that appears last
  x_test <- c("A", "B", "A", "C", "B", "A", "C", "C")
  # Modes are A and C (both appear 3 times), but C appears last among modes
  expect_equal(get_ultimate(x_test, find = "uptodate_mode", warning = FALSE), "C")

  # Another test case
  x_test2 <- c(1, 2, 1, 3, 2, 1, 2, 3, 3)
  # All values appear 3 times, but 3 appears last among modes
  expect_equal(get_ultimate(x_test2, find = "uptodate_mode", warning = FALSE), 3)
}
