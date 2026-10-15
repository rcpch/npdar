# tests/testthat/test-audit_year.R

library(testthat)

# ========== Test audit_year() default ==========

test_that("audit_year uses current date by default", {
  # Test that default works (uses Sys.Date())
  result <- audit_year()
  expect_type(result, "character")
  expect_match(result, "^\\d{4}/\\d{2} Q\\d{1}$")  # Matches format YYYY/YY Qx

  # Test unformatted default
  result_int <- audit_year(format = FALSE)
  expect_type(result_int, "integer")
  expect_true(result_int >= 2010)  # Reasonable minimum
  expect_true(result_int <= as.integer(format(Sys.Date(), "%Y")))
})

# ========== Test audit_year() input types ==========
test_that("audit_year works with character date inputs", {
  # Character dates should be coerced to Date
  expect_equal(audit_year(as.Date("2026-06-15")), "2026/27 Q1")
  expect_equal(audit_year(as.POSIXct("2026-06-15")), "2026/27 Q1")
  expect_equal(audit_year("2024-05-15"), "2024/25 Q1")
  expect_equal(audit_year("2024/02/15"), "2023/24 Q4")
})

# ========== Test audit_year() format ==========

test_that("audit_year returns unformatted year correctly", {
  # Formatted (default)
  expect_equal(audit_year("2024-05-15", format = TRUE), "2024/25 Q1")

  # Unformatted (integer)
  expect_equal(audit_year("2024-05-15", format = FALSE), 2024)
  expect_equal(audit_year("2024-02-15", format = FALSE), 2023)
  expect_equal(audit_year("2024-04-01", format = FALSE), 2024)
  expect_equal(audit_year("2024-03-31", format = FALSE), 2023)

  # Check type
  expect_type(audit_year("2024-05-15", format = FALSE), "integer")
  expect_type(audit_year("2024-05-15", format = TRUE), "character")
})

# ========== Test audit_year() quarters ==========

test_that("audit_year correctly identifies audit years for dates in Q1-Q4", {
  # Q1: April-June (current calendar year)
  expect_equal(audit_year("2024-06-30"), "2024/25 Q1")
  # Q2: July-September (current calendar year)
  expect_equal(audit_year("2024-09-30"), "2024/25 Q2")
  # Q3: October-December (current calendar year)
  expect_equal(audit_year("2024-10-01"), "2024/25 Q3")
  # Q4: January-March (next calendar year, but previous audit year)
  expect_equal(audit_year("2025-01-01"), "2024/25 Q4")
  expect_equal(audit_year("2025-02-15"), "2024/25 Q4")
  expect_equal(audit_year("2025-03-31"), "2024/25 Q4")
})

test_that("audit_year handles audit year boundaries correctly", {
  # Last day of audit year
  expect_equal(audit_year("2020-03-31"), "2019/20 Q4")
  # First day of audit year
  expect_equal(audit_year("2024-04-01"), "2024/25 Q1")
})

# ========== Test audit_year() custom quarters ==========

test_that("audit_year only accept 1-12 custom quarters", {
  expect_error(
    audit_year("2024-06-30", start_month = 1.5),
    "`start_month` must be a single integer between 1 and 12."
  )
  expect_error(
    audit_year("2024-06-30", start_month = NA),
    "`start_month` must be a single integer between 1 and 12."
  )
  expect_error(
    audit_year("2024-06-30", start_month = 13),
    "`start_month` must be a single integer between 1 and 12."
  )
  expect_error(
    audit_year("2024-06-30", start_month = -1),
    "`start_month` must be a single integer between 1 and 12."
  )
  expect_error(
    audit_year("2024-06-30", start_month = c(1, 2, 3)),
    "`start_month` must be a single integer between 1 and 12."
  )

})

test_that("audit_year correctly identifies correct quarters based on custom Q1", {
  expect_equal(audit_year("2024-06-30", start_month = 1), "2024/25 Q2")
  expect_equal(audit_year("2024-09-30", start_month = 1), "2024/25 Q3")
  expect_equal(audit_year("2024-10-01", start_month = 8), "2024/25 Q1")
  expect_equal(audit_year("2025-01-01", start_month = 9), "2024/25 Q2")
  expect_equal(audit_year("2025-02-15", start_month = 10), "2024/25 Q2")
  expect_equal(audit_year("2025-03-31", start_month = 11), "2024/25 Q2")
})


# ========== Test audit_year_range() ==========

test_that("audit_year_range generates correct sequence of formatted years", {
  result <- audit_year_range(start_year = 2015, end_year = 2020)

  expected <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21")
  expect_equal(result, expected)
  expect_length(result, 6)
  expect_type(result, "character")
})

test_that("audit_year_range generates correct sequence of unformatted years", {
  result <- audit_year_range(start_year = 2018, end_year = 2020, format = FALSE)

  expected <- c(2018L, 2019L, 2020L)
  expect_equal(result, expected)
  expect_length(result, 3)
  expect_type(result, "integer")
})

test_that("audit_year_range handles single year correctly", {
  # Single year range
  result_formatted <- audit_year_range(start_year = 2020, end_year = 2020)
  expect_equal(result_formatted, "2020/21")
  expect_length(result_formatted, 1)

  result_int <- audit_year_range(start_year = 2020, end_year = 2020, format = FALSE)
  expect_equal(result_int, 2020L)
  expect_length(result_int, 1)
})

test_that("audit_year_range uses default values correctly", {
  # Default start year is 2010
  result <- audit_year_range(end_year = 2012)
  expect_equal(result[1], "2010/11")
  expect_equal(result[3], "2012/13")
  expect_length(result, 3)

  # Default end year is current audit year
  result_default_end <- audit_year_range(start_year = 2023)
  current_year <- audit_year(format = FALSE)
  expect_length(result_default_end, current_year - 2023 + 1)
})

test_that("audit_year_range handles long sequences correctly", {
  # NPDA full history (2010-2024)
  result <- audit_year_range(start_year = 2010, end_year = 2024)

  expect_equal(result[1], "2010/11")
  expect_equal(result[15], "2024/25")
  expect_length(result, 15)

  # Check all are properly formatted
  expect_true(all(grepl("^\\d{4}/\\d{2}$", result)))
})

test_that("audit_year_range validates input parameters", {
  # start_year > end_year should error
  expect_error(
    audit_year_range(start_year = 2020, end_year = 2015),
    "start_year.*must be less than or equal to.*end_year"
  )

  # Non-numeric inputs should error
  expect_error(
    audit_year_range(start_year = "2020", end_year = 2024),
    "must be numeric"
  )

  expect_error(
    audit_year_range(start_year = 2020, end_year = "2024"),
    "must be numeric"
  )
})

test_that("audit_year_range works across century boundaries", {
  result <- audit_year_range(start_year = 1998, end_year = 2002)

  expected <- c("1998/99", "1999/00", "2000/01", "2001/02", "2002/03")
  expect_equal(result, expected)
})


# ========== Integration Tests ==========

test_that("audit_year and audit_year_range work together consistently", {
  # Current audit year should be in the default list
  current_year <- audit_year(format = FALSE)
  default_list <- audit_year_range(format = FALSE)

  expect_true(current_year %in% default_list)

  # Last element of default list should be current year
  expect_equal(default_list[length(default_list)], current_year)
})

test_that("Functions work in data frame operations", {
  library(dplyr)

  df <- data.frame(
    admission_date = as.Date(c("2024-05-01", "2024-12-01", "2025-02-01", "2023-04-15"))
  )

  result <- df |>
    mutate(audit_year = audit_year(admission_date),
           audit_year_int = audit_year(admission_date, format = FALSE))

  expect_equal(result$audit_year, c("2024/25 Q1", "2024/25 Q3", "2024/25 Q4", "2023/24 Q1"))
  expect_equal(result$audit_year_int, c(2024L, 2024L, 2024L, 2023L))

  # Test filtering with audit_year_range
  recent_years <- audit_year_range(start_year = 2024, end_year = 2024, format = FALSE)
  filtered <- result |>
    filter(audit_year_int %in% recent_years)

  expect_equal(nrow(filtered), 3)
})

test_that("Functions handle vectorized operations", {
  dates <- as.Date(c("2024-04-01", "2024-07-01", "2024-10-01", "2025-01-01"))

  result <- sapply(dates, audit_year)
  expect_equal(result, c("2024/25 Q1", "2024/25 Q2", "2024/25 Q3", "2024/25 Q4"))

  result_int <- sapply(dates, audit_year, format = FALSE)
  expect_equal(result_int, c(2024L, 2024L, 2024L, 2024L))
})


# ========== Edge Cases ==========

test_that("Functions handle leap years correctly", {
  # Leap year boundary (Feb 29)
  expect_equal(audit_year(as.Date("2024-02-29")), "2023/24 Q4")

  # March 1 after leap year
  expect_equal(audit_year(as.Date("2024-03-01")), "2023/24 Q4")

  # April 1 after leap year
  expect_equal(audit_year(as.Date("2024-04-01")), "2024/25 Q1")
})

test_that("Functions handle year 2100 correctly (not a leap year)", {
  # 2100 is not a leap year (divisible by 100 but not 400)
  expect_equal(audit_year(as.Date("2100-03-31")), "2099/00 Q4")
  expect_equal(audit_year(as.Date("2100-04-01")), "2100/01 Q1")
})

