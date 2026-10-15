library(dplyr)

# ========== Test Data Setup ==========

# Easy test data
df_easy <- data.frame(
  systolic_obs  = c(  118,  118,  134,  139),
  diastolic_obs = c(   75,   75,   91,   96),
  sex           = c(    1,    1,    2,    2),
  age           = c(   12,   13,   17,   17),
  height        = c(-1.64,-1.64,-1.64,-1.64)
)

# Hard test data (comprehensive edge cases)
df_hard <- data.frame(
  systolic_obs  = c(  118,  118,  134,  139,   NA,  139,      139,  115,  120,  139,  139,  159,  169, 189, 301, 139),
  diastolic_obs = c(   75,   75,   91,   96,   NA,   96,       96,   68,   73,   96,   96,   96,  103, 123, 151,  96),
  gender        = c(  "m",  "m",  "f",  "f",  "f",   NA,"unknown",  "f",  "f",  "f",  "f",  "f",  "f", "f", "f", "f"),
  age           = c(   12,   13,   17,   17,   17,   12,       12,  0.5,  0.5,   NA,   -1,   18,   22,  23,  24,  17),
  height        = c(-1.64,-1.64,-1.64,-1.64,-1.64, 1.64,     1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 160, 160, 160)
)



# ========== Test bp_expected() ==========

test_that("bp_expected calculates expected BP correctly for easy data", {
  result <- df_easy |>
    mutate(
      sbp_expected = bp_expected(
        bp_type = "sys",
        sex = sex, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "Fourth Report",
        .quiet = TRUE
      ),
      dbp_expected = bp_expected(
        bp_type = "diastolic",
        sex = sex, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "Fourth Report",
        .quiet = TRUE
      )
    )

  # Test SBP expected values
  expect_equal(result$sbp_expected[1], 101.6220, tolerance = 0.001)
  expect_equal(result$sbp_expected[2], 104.0445, tolerance = 0.001)
  expect_equal(result$sbp_expected[3], 108.6104, tolerance = 0.001)
  expect_equal(result$sbp_expected[4], 108.6104, tolerance = 0.001)

  # Test DBP expected values
  expect_equal(result$dbp_expected[1], 59.71597, tolerance = 0.001)
  expect_equal(result$dbp_expected[2], 60.26170, tolerance = 0.001)
  expect_equal(result$dbp_expected[3], 64.92422, tolerance = 0.001)
  expect_equal(result$dbp_expected[4], 64.92422, tolerance = 0.001)
})

test_that("bp_expected handles edge cases in hard data", {
  result <- df_hard |>
    mutate(
      sbp_expected = bp_expected(
        bp_type = "sys",
        sex = gender, male_code = "m", female_code = "f",
        height_z = height, height_limit = 5,
        age_years = age, ref = "Fourth Report",
        .quiet = TRUE
      )
    )

  # Valid children (rows 1-5, 8-9)
  expect_equal(result$sbp_expected[1], 101.62195, tolerance = 0.001)
  expect_equal(result$sbp_expected[5], 108.61040, tolerance = 0.001)  # NA BP value but valid demographics
  expect_equal(result$sbp_expected[8], 89.37298, tolerance = 0.001)   # Newborn
  expect_equal(result$sbp_expected[9], 89.37298, tolerance = 0.001)   # Newborn

  # Invalid sex (rows 6-7: NA and 3)
  expect_true(is.na(result$sbp_expected[6]))
  expect_true(is.na(result$sbp_expected[7]))

  # Invalid age (rows 10-11: NA and -1)
  expect_true(is.na(result$sbp_expected[10]))
  expect_true(is.na(result$sbp_expected[11]))

  # Adults >17 years (rows 12-15)
  expect_true(is.na(result$sbp_expected[12]))
  expect_true(is.na(result$sbp_expected[13]))

  # Invalid height (row 14-16: height=160, should be z-score)
  expect_true(is.na(result$sbp_expected[14]))
  expect_true(is.na(result$sbp_expected[15]))
  expect_true(is.na(result$sbp_expected[16]))
})

test_that("bp_expected returns NA for NICE/BHF reference", {
  result <- df_easy |>
    mutate(
      sbp_expected_NICE = bp_expected(
        bp_type = "sys",
        sex = sex, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "NICE",
        .quiet = TRUE
      )
    )

  # All should be NA for children with NICE reference
  expect_true(all(is.na(result$sbp_expected_NICE)))
})

test_that("bp_expected works with partial matching", {
  result1 <- bp_expected(
    bp_type = "sys",  # Partial match: "sys" -> "systolic"
    sex = 1, male_code = 1, female_code = 2,
    height_z = 0, age_years = 10, ref = "Fourth",  # Partial: "Fourth" -> "Fourth Report"
    .quiet = TRUE
  )

  result2 <- bp_expected(
    bp_type = "dia",  # Partial match: "dia" -> "diastolic"
    sex = 2, male_code = 1, female_code = 2,
    height_z = 0, age_years = 10, ref = "Fourth Report",
    .quiet = TRUE
  )

  expect_type(result1, "double")
  expect_type(result2, "double")
  expect_false(is.na(result1))
  expect_false(is.na(result2))
})


# ========== Test bp_relative() ==========

test_that("bp_relative calculates z-scores and percentiles correctly for easy data", {
  result <- df_easy |>
    mutate(bp_relative(
      bp_value = systolic_obs, bp_type = "sys",
      sex = sex, male_code = 1, female_code = 2,
      height_z = height, height_limit = 5,
      age_years = age, ref = "Fourth Report",
      .quiet = TRUE
    )) |>
    rename(sbp_z = zscore, sbp_centile = percentile) |>
    mutate(
      dbp_centile = bp_relative(
        bp_value = diastolic_obs, bp_type = "diastolic",
        sex = sex, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "Fourth Report",
        .quiet = TRUE
      )$percentile
    )

  # Test SBP z-scores
  expect_equal(result$sbp_z[1], 1.528830, tolerance = 0.001)
  expect_equal(result$sbp_z[2], 1.302697, tolerance = 0.001)
  expect_equal(result$sbp_z[3], 2.421401, tolerance = 0.001)
  expect_equal(result$sbp_z[4], 2.898250, tolerance = 0.001)

  # Test SBP percentiles
  expect_equal(result$sbp_centile[1], 93.68467, tolerance = 0.01)
  expect_equal(result$sbp_centile[2], 90.36608, tolerance = 0.01)
  expect_equal(result$sbp_centile[3], 99.22696, tolerance = 0.01)
  expect_equal(result$sbp_centile[4], 99.81237, tolerance = 0.01)

  # Test DBP percentiles
  expect_equal(result$dbp_centile[1], 90.61185, tolerance = 0.01)
  expect_equal(result$dbp_centile[2], 89.79920, tolerance = 0.01)
  expect_equal(result$dbp_centile[3], 99.13381, tolerance = 0.01)
  expect_equal(result$dbp_centile[4], 99.77165, tolerance = 0.01)
})

test_that("bp_relative handles edge cases in hard data", {
  result <- df_hard |>
    mutate(bp_relative(
      bp_value = systolic_obs, bp_type = "sys",
      sex = gender, male_code = "m", female_code = "f",
      height_z = height, height_limit = 5,
      age_years = age, ref = "Fourth Report",
      .quiet = TRUE
    )) |>
    rename(sbp_z = zscore, sbp_centile = percentile)

  # Valid children
  expect_equal(result$sbp_z[1], 1.528830, tolerance = 0.001)
  expect_equal(result$sbp_z[8], 2.444043, tolerance = 0.001)  # Newborn
  expect_equal(result$sbp_centile[8], 99.27382, tolerance = 0.01)
  expect_equal(result$sbp_centile[9], 99.82548, tolerance = 0.01)

  # NA BP value (row 5)
  expect_true(is.na(result$sbp_z[5]))
  expect_true(is.na(result$sbp_centile[5]))

  # Invalid demographics (rows 6-7, 10-11)
  expect_true(all(is.na(result$sbp_z[c(6, 7, 10, 11)])))

  # Adults (rows 12-16)
  expect_true(all(is.na(result$sbp_z[12:16])))
})

test_that("bp_relative returns NA for NICE/BHF reference", {
  result <- df_easy |>
    mutate(
      dbp_centile_NICE = bp_relative(
        bp_value = diastolic_obs, bp_type = "diastolic",
        sex = sex, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "NICE",
        .quiet = TRUE
      )$percentile
    )

  # All should be NA for children with NICE reference
  expect_true(all(is.na(result$dbp_centile_NICE)))
})

test_that("bp_relative returns tibble with correct structure", {
  result <- bp_relative(
    bp_value = c(120, 130),
    bp_type = "systolic",
    sex = c(1, 2), male_code = 1, female_code = 2,
    height_z = c(0, 0), age_years = c(10, 12),
    ref = "Fourth Report",
    .quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("zscore", "percentile"))
  expect_equal(nrow(result), 2)
})


# ========== Test bp_category() ==========

test_that("bp_category categorises children correctly with Fourth Report", {
  result <- df_easy |>
    mutate(
      sbp_cat_c = bp_category(bp_value = systolic_obs, bp_type = "sys",
                              sex = sex, male_code = 1, female_code = 2,
                              height_z = height, height_limit=5,
                              age_years = age, ref = "Fourth",
                              .quiet = TRUE),
      dbp_cat_c = bp_category(bp_value = diastolic_obs, bp_type = "dia",
                              sex = sex, male_code = 1, female_code = 2,
                              height_z = height, height_limit=5,
                              age_years = age, ref = "Fourth",
                              .quiet = TRUE),
    )

  # Test SBP categories
  expect_equal(result$sbp_cat_c[1], "Prehypertension")
  expect_equal(result$sbp_cat_c[2], "Normotension")
  expect_equal(result$sbp_cat_c[3], "Stage 1 hypertension")
  expect_equal(result$sbp_cat_c[4], "Stage 2 hypertension")

  # Test DBP categories
  expect_equal(result$dbp_cat_c[1], "Prehypertension")
  expect_equal(result$dbp_cat_c[2], "Normotension")
  expect_equal(result$dbp_cat_c[3], "Stage 1 hypertension")
  expect_equal(result$dbp_cat_c[4], "Stage 2 hypertension")
})

test_that("bp_category categorises adults correctly with NICE/BHF", {
  # Create adult-only data
  df_adults <- df_hard |>
    filter(age > 17)

  result <- df_adults |>
    mutate(
      sbp_cat_yp = bp_category(bp_value = systolic_obs, bp_type = "sys",
                               sex = gender, male_code = "m", female_code = "f",
                               height_z = height, height_limit=5,
                               age_years = age, ref = "NICE", .quiet = TRUE)
    )

  expect_equal(result$sbp_cat_yp[1], "Stage 1 hypertension") # SBP 159 mmHg
  expect_equal(result$sbp_cat_yp[2], "Stage 2 hypertension") # SBP 169 mmHg
  expect_equal(result$sbp_cat_yp[3], "Stage 3 hypertension") # SBP 189 mmHg
  expect_true(is.na(result$sbp_cat_yp[4])) # SBP 301 mmHg, outside NDA limit
})

test_that("bp_category handles newborns correctly", {
  result <- df_hard |>
    filter(age == 0.5) |>
    mutate(
      sbp_cat_c = bp_category(bp_value = systolic_obs, bp_type = "sys",
                              sex = gender, male_code = "m", female_code = "f",
                              height_z = height, height_limit=5,
                              age_years = age, ref = "Fourth",
                              .quiet = TRUE),
      dbp_cat_c = bp_category(bp_value = diastolic_obs, bp_type = "dia",
                              sex = gender, male_code = "m", female_code = "f",
                              height_z = height, height_limit=5,
                              age_years = age, ref = "Fourth",
                              .quiet = TRUE)
      )

  expect_equal(result$sbp_cat_c[1], "Stage 1 hypertension") # SBP 115 (newborn)
  expect_equal(result$sbp_cat_c[2], "Stage 2 hypertension") # SBP 120 (newborn)

  # DBP not categorised for newborns <1 year in some cases
  # Check that function doesn't crash
  expect_type(result$dbp_cat_c, "character")
})

test_that("bp_category respects custom bp_limit", {
  result <- df_hard |>
    mutate(
      dbp_cat_custom = bp_category(bp_value = diastolic_obs, bp_type = "dia",
                                   bp_limit = c(0, 151),  # Custom limit
                                   sex = gender, male_code = "m", female_code = "f",
                                   height_z = height, height_limit = 5,
                                   age_years = age, ref = "NICE",
                                   .quiet = TRUE)
    )

  expect_equal(result$dbp_cat_custom[15], "Stage 3 hypertension") # DBP 151
})

test_that("bp_category handles invalid inputs", {
  result <- df_hard |>
    mutate(
      sbp_cat_c = bp_category(bp_value = systolic_obs, bp_type = "sys",
                              sex = gender, male_code = "m", female_code = "f",
                              height_z = height, height_limit = 5,
                              age_years = age, ref = "Fourth",
                              .quiet = TRUE)
      )

  expect_true(is.na(result$sbp_cat_c[5])) # Invalid BP value (NA)

  expect_true(is.na(result$sbp_cat_c[6])) # Invalid sex (NA)
  expect_true(is.na(result$sbp_cat_c[7])) # Invalid sex ("unknown")

  expect_true(is.na(result$sbp_cat_c[10])) # Invalid age (NA)
  expect_true(is.na(result$sbp_cat_c[11])) # Invalid age (-1)

  expect_true(is.na(result$sbp_cat_c[16])) # Invalid height (160, not z-score)
})


# ========== Test bp_assess() Master Function ==========

# Shared arguments for the Fourth Report easy-data cases
result_assess_easy <- bp_assess(
  bp_value = df_easy$systolic_obs, bp_type = "sys",
  sex = df_easy$sex, male_code = 1, female_code = 2,
  height_z = df_easy$height, height_limit = 5,
  age_years = df_easy$age, ref = "Fourth",
  .quiet = TRUE
)

test_that("bp_assess returns a tibble with one row per input", {
  expect_s3_class(result_assess_easy, "tbl_df")
  expect_named(result_assess_easy, c("expected", "zscore", "percentile", "category"))
  expect_equal(nrow(result_assess_easy), nrow(df_easy))
})

test_that("bp_assess matches bp_expected, bp_relative and bp_category", {
  args <- list(
    bp_type = "sys",
    sex = df_easy$sex, male_code = 1, female_code = 2,
    height_z = df_easy$height, height_limit = 5,
    age_years = df_easy$age,
    ref = "Fourth", .quiet = TRUE
  )

  expect_equal(result_assess_easy$expected,
               do.call(bp_expected, args))
  expect_equal(result_assess_easy$zscore,
               do.call(bp_relative, c(list(bp_value = df_easy$systolic_obs), args))$zscore)
  expect_equal(result_assess_easy$percentile,
               do.call(bp_relative, c(list(bp_value = df_easy$systolic_obs), args))$percentile)
  expect_equal(result_assess_easy$category,
               do.call(bp_category, c(list(bp_value = df_easy$systolic_obs), args)))

  expect_equal(result_assess_easy$expected[1],
               101.6220, tolerance = 0.001)
  expect_equal(result_assess_easy$zscore[1],
               1.528830, tolerance = 0.001)
  expect_equal(result_assess_easy$percentile[2],
               90.36608, tolerance = 0.01)
  expect_equal(result_assess_easy$category,
               c("Prehypertension", "Normotension", "Stage 1 hypertension", "Stage 2 hypertension"))
})

test_that("bp_assess unpacks into new columns inside mutate()", {
  result <- df_easy |>
    mutate(bp_assess(
      bp_value = systolic_obs, bp_type = "sys",
      sex = sex, male_code = 1, female_code = 2,
      height_z = height, age_years = age, ref = "Fourth",
      .quiet = TRUE
    ))

  expect_true(all(c("expected", "zscore", "percentile", "category") %in% names(result)))
  expect_equal(result$expected[2], 104.0445, tolerance = 0.001)
})

test_that("bp_assess prints each message once", {
  args <- list(
    bp_value = df_easy$systolic_obs, bp_type = "sys",
    sex = df_easy$sex, male_code = 1, female_code = 2,
    height_z = df_easy$height, height_limit = 5,
    age_years = df_easy$age, ref = "Fourth"
  )

  msgs <- character()
  withCallingHandlers(
    do.call(bp_assess, args),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_gt(length(msgs), 0)
  expect_equal(anyDuplicated(msgs), 0)

  expect_silent(do.call(bp_assess, c(args, .quiet = TRUE)))
})

# ========== Test Validation and Messages ==========

test_that("Functions produce appropriate messages", {
  # Test age out of range messages
  expect_message(
    bp_expected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 20, ref = "Fourth Report"
    ),
    "Ignore.*outside 0-17 years"
  )

  expect_message(
    bp_expected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 10, ref = "NICE/BHF"
    ),
    "Ignore.*\u2264 17 years"
  )

  # Test height out of range messages
  expect_message(
    bp_expected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 10, age_years = 12, ref = "Fourth Report"
    ),
    "Ignore.*beyond.*based on 'height_limit'"
  )

  # Test NICE/BHF limitation messages
  expect_message(
    bp_expected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 20, ref = "NICE/BHF"
    ),
    "NICE/BHF only provides category guidelines"
  )
})

test_that(".quiet parameter suppresses messages", {
  # Should not produce messages when .quiet = TRUE
  expect_silent(
    bp_expected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 10, age_years = 20, ref = "Fourth Report",
      .quiet = TRUE
    )
  )

  expect_silent(
    bp_relative(
      bp_value = 120, bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 10, age_years = 20, ref = "Fourth Report",
      .quiet = TRUE
    )
  )
})


# ========== Test (Other) Parameter Validation ==========

test_that("Functions require all inputs to have the same length", {
  expect_error(
    bp_assess(
      bp_value = c(100, 110, 120), bp_type = "sys",
      sex = c(1, 2), male_code = 1, female_code = 2,
      height_z = 0, age_years = 10, ref = "Fourth", .quiet = TRUE
    ),
    "same length.*age_years: 1.*height_z: 1"
  )

  expect_error(
    bp_expected(
      bp_type = "sys", sex = c(1, 2), male_code = 1, female_code = 2,
      height_z = 0, age_years = 10, ref = "Fourth", .quiet = TRUE
    ),
    "same length"
  )
})

test_that("Functions require ref parameter", {
  expect_error(
    bp_expected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 10
      # Missing ref parameter
    ),
    "'ref' must be specified"
  )
})

test_that("Functions require bp_type parameter", {
  expect_error(
    bp_expected(
      # Missing bp_type parameter
      sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 10, ref = "Fourth Report"
    ),
    "'bp_type' must be specified"
  )
})

test_that("Functions handle grouped data correctly", {
  result <- df_easy |>
    group_by(sex) |>
    mutate(
      sbp_cat = bp_category(
        bp_value = systolic_obs, bp_type = "systolic",
        sex = sex, male_code = 1, female_code = 2,
        height_z = height, age_years = age, ref = "Fourth Report"
      )
    ) |>
    ungroup()

  expect_equal(nrow(result), 4)
  expect_true(all(!is.na(result$sbp_cat)))
})
