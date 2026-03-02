# tests/testthat/test-audit_year.R

library(testthat)

# ========== Test get_AuditYear() ==========

test_that("get_AuditYear correctly identifies audit years for dates in Q1-Q3", {
  # Q1: April-June (current calendar year)
  expect_equal(get_AuditYear(as.Date("2024-04-01")), "2024/25")
  expect_equal(get_AuditYear(as.Date("2024-05-15")), "2024/25")
  expect_equal(get_AuditYear(as.Date("2024-06-30")), "2024/25")

  # Q2: July-September (current calendar year)
  expect_equal(get_AuditYear(as.Date("2024-07-01")), "2024/25")
  expect_equal(get_AuditYear(as.Date("2024-08-15")), "2024/25")
  expect_equal(get_AuditYear(as.Date("2024-09-30")), "2024/25")

  # Q3: October-December (current calendar year)
  expect_equal(get_AuditYear(as.Date("2024-10-01")), "2024/25")
  expect_equal(get_AuditYear(as.Date("2024-11-15")), "2024/25")
  expect_equal(get_AuditYear(as.Date("2024-12-31")), "2024/25")
})

test_that("get_AuditYear correctly identifies audit years for dates in Q4", {
  # Q4: January-March (next calendar year, but previous audit year)
  expect_equal(get_AuditYear(as.Date("2025-01-01")), "2024/25")
  expect_equal(get_AuditYear(as.Date("2025-02-15")), "2024/25")
  expect_equal(get_AuditYear(as.Date("2025-03-31")), "2024/25")
})

test_that("get_AuditYear handles audit year boundaries correctly", {
  # Last day of audit year 2023/24
  expect_equal(get_AuditYear(as.Date("2024-03-31")), "2023/24")

  # First day of audit year 2024/25
  expect_equal(get_AuditYear(as.Date("2024-04-01")), "2024/25")

  # Test multiple years
  expect_equal(get_AuditYear(as.Date("2020-03-31")), "2019/20")
  expect_equal(get_AuditYear(as.Date("2020-04-01")), "2020/21")

  expect_equal(get_AuditYear(as.Date("2023-03-31")), "2022/23")
  expect_equal(get_AuditYear(as.Date("2023-04-01")), "2023/24")
})

test_that("get_AuditYear returns unformatted year correctly", {
  # Formatted (default)
  expect_equal(get_AuditYear(as.Date("2024-05-15"), format = TRUE), "2024/25")

  # Unformatted (integer)
  expect_equal(get_AuditYear(as.Date("2024-05-15"), format = FALSE), 2024)
  expect_equal(get_AuditYear(as.Date("2024-02-15"), format = FALSE), 2023)
  expect_equal(get_AuditYear(as.Date("2024-04-01"), format = FALSE), 2024)
  expect_equal(get_AuditYear(as.Date("2024-03-31"), format = FALSE), 2023)

  # Check type
  expect_type(get_AuditYear(as.Date("2024-05-15"), format = FALSE), "integer")
  expect_type(get_AuditYear(as.Date("2024-05-15"), format = TRUE), "character")
})

test_that("get_AuditYear uses current date by default", {
  # Test that default works (uses Sys.Date())
  result <- get_AuditYear()
  expect_type(result, "character")
  expect_match(result, "^\\d{4}/\\d{2}$")  # Matches format YYYY/YY

  # Test unformatted default
  result_int <- get_AuditYear(format = FALSE)
  expect_type(result_int, "integer")
  expect_true(result_int >= 2010)  # Reasonable minimum
  expect_true(result_int <= as.integer(format(Sys.Date(), "%Y")))
})

test_that("get_AuditYear handles historical dates correctly", {
  # NPDA started in 2010/11
  expect_equal(get_AuditYear(as.Date("2010-04-01")), "2010/11")
  expect_equal(get_AuditYear(as.Date("2011-03-31")), "2010/11")

  # Earlier dates
  expect_equal(get_AuditYear(as.Date("2000-05-15")), "2000/01")
  expect_equal(get_AuditYear(as.Date("2000-02-15")), "1999/00")
})

test_that("get_AuditYear formats years correctly across centuries", {
  # Test year 2000 boundary
  expect_equal(get_AuditYear(as.Date("1999-05-15")), "1999/00")
  expect_equal(get_AuditYear(as.Date("2000-05-15")), "2000/01")

  # Test year 2010 boundary
  expect_equal(get_AuditYear(as.Date("2009-05-15")), "2009/10")
  expect_equal(get_AuditYear(as.Date("2010-05-15")), "2010/11")
})

test_that("get_AuditYear works with character date inputs", {
  # Character dates should be coerced to Date
  expect_equal(get_AuditYear("2024-05-15"), "2024/25")
  expect_equal(get_AuditYear("2024-02-15"), "2023/24")
})


# ========== Test get_AuditYears() ==========

test_that("get_AuditYears generates correct sequence of formatted years", {
  result <- get_AuditYears(startYear = 2015, endYear = 2020)

  expected <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21")
  expect_equal(result, expected)
  expect_length(result, 6)
  expect_type(result, "character")
})

test_that("get_AuditYears generates correct sequence of unformatted years", {
  result <- get_AuditYears(startYear = 2018, endYear = 2020, format = FALSE)

  expected <- c(2018L, 2019L, 2020L)
  expect_equal(result, expected)
  expect_length(result, 3)
  expect_type(result, "integer")
})

test_that("get_AuditYears handles single year correctly", {
  # Single year range
  result_formatted <- get_AuditYears(startYear = 2020, endYear = 2020)
  expect_equal(result_formatted, "2020/21")
  expect_length(result_formatted, 1)

  result_int <- get_AuditYears(startYear = 2020, endYear = 2020, format = FALSE)
  expect_equal(result_int, 2020L)
  expect_length(result_int, 1)
})

test_that("get_AuditYears uses default values correctly", {
  # Default start year is 2010
  result <- get_AuditYears(endYear = 2012)
  expect_equal(result[1], "2010/11")
  expect_equal(result[3], "2012/13")
  expect_length(result, 3)

  # Default end year is current audit year
  result_default_end <- get_AuditYears(startYear = 2023)
  current_year <- get_AuditYear(format = FALSE)
  expect_length(result_default_end, current_year - 2023 + 1)
})

test_that("get_AuditYears handles long sequences correctly", {
  # NPDA full history (2010-2024)
  result <- get_AuditYears(startYear = 2010, endYear = 2024)

  expect_equal(result[1], "2010/11")
  expect_equal(result[15], "2024/25")
  expect_length(result, 15)

  # Check all are properly formatted
  expect_true(all(grepl("^\\d{4}/\\d{2}$", result)))
})

test_that("get_AuditYears validates input parameters", {
  # startYear > endYear should error
  expect_error(
    get_AuditYears(startYear = 2020, endYear = 2015),
    "startYear.*must be less than or equal to.*endYear"
  )

  # Non-numeric inputs should error
  expect_error(
    get_AuditYears(startYear = "2020", endYear = 2024),
    "must be numeric"
  )

  expect_error(
    get_AuditYears(startYear = 2020, endYear = "2024"),
    "must be numeric"
  )
})

test_that("get_AuditYears works across century boundaries", {
  result <- get_AuditYears(startYear = 1998, endYear = 2002)

  expected <- c("1998/99", "1999/00", "2000/01", "2001/02", "2002/03")
  expect_equal(result, expected)
})


# ========== Integration Tests ==========

test_that("get_AuditYear and get_AuditYears work together consistently", {
  # Current audit year should be in the default list
  current_year <- get_AuditYear()
  default_list <- get_AuditYears()

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
    mutate(audit_year = get_AuditYear(admission_date),
           audit_year_int = get_AuditYear(admission_date, format = FALSE))

  expect_equal(result$audit_year, c("2024/25", "2024/25", "2024/25", "2023/24"))
  expect_equal(result$audit_year_int, c(2024L, 2024L, 2024L, 2023L))

  # Test filtering with get_AuditYears
  recent_years <- get_AuditYears(startYear = 2024, endYear = 2024)
  filtered <- result |>
    filter(audit_year %in% recent_years)

  expect_equal(nrow(filtered), 3)
})

test_that("Functions handle vectorized operations", {
  dates <- as.Date(c("2024-04-01", "2024-07-01", "2024-10-01", "2025-01-01"))

  result <- sapply(dates, get_AuditYear)
  expect_equal(result, c("2024/25", "2024/25", "2024/25", "2024/25"))

  result_int <- sapply(dates, get_AuditYear, format = FALSE)
  expect_equal(result_int, c(2024L, 2024L, 2024L, 2024L))
})


# ========== Edge Cases ==========

test_that("Functions handle leap years correctly", {
  # Leap year boundary (Feb 29)
  expect_equal(get_AuditYear(as.Date("2024-02-29")), "2023/24")

  # March 1 after leap year
  expect_equal(get_AuditYear(as.Date("2024-03-01")), "2023/24")

  # April 1 after leap year
  expect_equal(get_AuditYear(as.Date("2024-04-01")), "2024/25")
})

test_that("Functions handle year 2100 correctly (not a leap year)", {
  # 2100 is not a leap year (divisible by 100 but not 400)
  expect_equal(get_AuditYear(as.Date("2100-03-31")), "2099/00")
  expect_equal(get_AuditYear(as.Date("2100-04-01")), "2100/01")
})

