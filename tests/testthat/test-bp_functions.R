library(dplyr)

# ========== Test Data Setup ==========

# Easy test data
df_easy <- data.frame(systolic_obs  = c(  118,  118,  134,  139),
                      diastolic_obs = c(   75,   75,   91,   96),
                      gender        = c(    1,    1,    2,    2),
                      age           = c(   12,   13,   17,   17),
                      height        = c(-1.64,-1.64,-1.64,-1.64))

# Hard test data (comprehensive edge cases)
df_hard <- data.frame(systolic_obs  = c(  118,  118,  134,  139,   NA,  139,  139,  115,  120,  139,  139,  159,  169, 189, 301, 139),
                      diastolic_obs = c(   75,   75,   91,   96,   NA,   96,   96,   68,   73,   96,   96,   96,  103, 123, 151,  96),
                      gender        = c(    1,    1,    2,    2,    2,   NA,    3,    2,    2,    2,    2,    2,    2,   2,   2,   2),
                      age           = c(   12,   13,   17,   17,   17,   12,   12,  0.5,  0.5,   NA,   -1,   18,   22,  23,  24,  17),
                      height        = c(-1.64,-1.64,-1.64,-1.64,-1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 160, 160, 160))



# ========== Test get_BPExpected() ==========

test_that("get_BPExpected calculates expected BP correctly for easy data", {
  result <- df_easy |>
    mutate(
      sbp_expected = get_BPExpected(
        bp_type = "sys",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "Fourth Report",
        .quiet = TRUE
      ),
      dbp_expected = get_BPExpected(
        bp_type = "diastolic",
        sex = gender, male_code = 1, female_code = 2,
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

test_that("get_BPExpected handles edge cases in hard data", {
  result <- df_hard |>
    mutate(
      sbp_expected = get_BPExpected(
        bp_type = "sys",
        sex = gender, male_code = 1, female_code = 2,
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

test_that("get_BPExpected returns NA for NICE/BHF reference", {
  result <- df_easy |>
    mutate(
      sbp_expected_NICE = get_BPExpected(
        bp_type = "sys",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "NICE",
        .quiet = TRUE
      )
    )

  # All should be NA for children with NICE reference
  expect_true(all(is.na(result$sbp_expected_NICE)))
})

test_that("get_BPExpected works with partial matching", {
  result1 <- get_BPExpected(
    bp_type = "sys",  # Partial match: "sys" -> "systolic"
    sex = 1, male_code = 1, female_code = 2,
    height_z = 0, age_years = 10, ref = "Fourth",  # Partial: "Fourth" -> "Fourth Report"
    .quiet = TRUE
  )

  result2 <- get_BPExpected(
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


# ========== Test get_BPRelative() ==========

test_that("get_BPRelative calculates z-scores and percentiles correctly for easy data", {
  result <- df_easy |>
    mutate(get_BPRelative(
      bp_value = systolic_obs, bp_type = "sys",
      sex = gender, male_code = 1, female_code = 2,
      height_z = height, height_limit = 5,
      age_years = age, ref = "Fourth Report",
      .quiet = TRUE
    )) |>
    rename(sbp_z = zscore, sbp_centile = percentile) |>
    mutate(
      dbp_centile = get_BPRelative(
        bp_value = diastolic_obs, bp_type = "diastolic",
        sex = gender, male_code = 1, female_code = 2,
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

test_that("get_BPRelative handles edge cases in hard data", {
  result <- df_hard |>
    mutate(get_BPRelative(
      bp_value = systolic_obs, bp_type = "sys",
      sex = gender, male_code = 1, female_code = 2,
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

test_that("get_BPRelative returns NA for NICE/BHF reference", {
  result <- df_easy |>
    mutate(
      dbp_centile_NICE = get_BPRelative(
        bp_value = diastolic_obs, bp_type = "diastolic",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "NICE",
        .quiet = TRUE
      )$percentile
    )

  # All should be NA for children with NICE reference
  expect_true(all(is.na(result$dbp_centile_NICE)))
})

test_that("get_BPRelative returns tibble with correct structure", {
  result <- get_BPRelative(
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


# ========== Test get_BPCategory() ==========

test_that("get_BPCategory categorises children correctly with Fourth Report", {
  result <- df_easy |>
    mutate(sbp_cat_c = get_BPCategory(bp_value = systolic_obs, bp_type = "sys",
                                      sex = gender, male_code = 1, female_code = 2,
                                      height_z = height, height_limit=5,
                                      age_years = age, ref = "Fourth"),
           dbp_cat_c = get_BPCategory(bp_value = diastolic_obs, bp_type = "dia",
                                      sex = gender, male_code = 1, female_code = 2,
                                      height_z = height, height_limit=5,
                                      age_years = age, ref = "Fourth"),
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

test_that("get_BPCategory categorises adults correctly with NICE/BHF", {
  # Create adult-only data
  df_adults <- df_hard |>
    filter(age > 17)

  result <- df_adults |>
    mutate(sbp_cat_yp = get_BPCategory(bp_value = systolic_obs, bp_type = "sys",
                                  sex = gender, male_code = 1, female_code = 2,
                                  height_z = height, height_limit=5,
                                  age_years = age, ref = "NICE"))

  # Row 1 (159 mmHg) - Stage 1 hypertension
  expect_equal(result$sbp_cat_yp[1], "Stage 1 hypertension")

  # Row 2 (169 mmHg) - Stage 2 hypertension
  expect_equal(result$sbp_cat_yp[2], "Stage 2 hypertension")

  # Row 3 (189 mmHg) - Stage 3 hypertension
  expect_equal(result$sbp_cat_yp[3], "Stage 3 hypertension")

  # Row 4 (301 mmHg) - Outside NDA limit, should be NA
  expect_true(is.na(result$sbp_cat_yp[4]))
})

test_that("get_BPCategory handles newborns correctly", {
  result <- df_hard |>
    filter(age == 0.5) |>
    mutate(sbp_cat_c = get_BPCategory(bp_value = systolic_obs, bp_type = "sys",
                                      sex = gender, male_code = 1, female_code = 2,
                                      height_z = height, height_limit=5,
                                      age_years = age, ref = "Fourth"),
           dbp_cat_c = get_BPCategory(bp_value = diastolic_obs, bp_type = "dia",
                                      sex = gender, male_code = 1, female_code = 2,
                                      height_z = height, height_limit=5,
                                      age_years = age, ref = "Fourth"))

  # Row 1 (SBP 115) - Stage 1 for newborn
  expect_equal(result$sbp_cat_c[1], "Stage 1 hypertension")

  # Row 2 (SBP 120) - Stage 2 for newborn
  expect_equal(result$sbp_cat_c[2], "Stage 2 hypertension")

  # DBP not categorised for newborns <1 year in some cases
  # Check that function doesn't crash
  expect_type(result$dbp_cat_c, "character")
})

test_that("get_BPCategory respects custom bp_limit", {
  result <- df_hard |>
    mutate(dbp_cat_custom = get_BPCategory(bp_value = diastolic_obs, bp_type = "dia",
                                           bp_limit = c(0, 151),  # Custom limit
                                           sex = gender, male_code = 1, female_code = 2,
                                           height_z = height, height_limit = 5,
                                           age_years = age, ref = "NICE"))

  # Row with DBP 151 should be Stage 3 (within custom limit)
  expect_equal(result$dbp_cat_custom[15], "Stage 3 hypertension")
})

test_that("get_BPCategory handles invalid inputs", {
  result <- df_hard |>
    mutate(sbp_cat_c = get_BPCategory(bp_value = systolic_obs, bp_type = "sys",
                                      sex = gender, male_code = 1, female_code = 2,
                                      height_z = height, height_limit = 5,
                                      age_years = age, ref = "Fourth"))

  # NA BP value (row 5)
  expect_true(is.na(result$sbp_cat_c[5]))

  # Invalid sex (rows 6-7)
  expect_true(is.na(result$sbp_cat_c[6]))
  expect_true(is.na(result$sbp_cat_c[7]))

  # Invalid age (rows 10-11)
  expect_true(is.na(result$sbp_cat_c[10]))
  expect_true(is.na(result$sbp_cat_c[11]))

  # Invalid height (row 16)
  expect_true(is.na(result$sbp_cat_c[16]))
})


# ========== Test get_BP() Master Function ==========

test_that("get_BP wrapper calls correct functions with 'expected' option", {
  result <- df_easy |>
    mutate(
      sbp_expected = get_BP(
        option = "expected",
        bp_type = "sys",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "Fourth",
        .quiet = TRUE
      )
    )

  expect_equal(result$sbp_expected[1], 101.6220, tolerance = 0.001)
  expect_equal(result$sbp_expected[2], 104.0445, tolerance = 0.001)
})

test_that("get_BP wrapper calls correct functions with 'percentile' option", {
  result <- df_easy |>
    mutate(
      sbp_centile = get_BP(
        option = "perc",  # Partial match: "perc" -> "percentile"
        bp_value = systolic_obs, bp_type = "sys",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "Fourth",
        .quiet = TRUE
      )
    )

  expect_equal(result$sbp_centile[1], 93.68467, tolerance = 0.01)
  expect_equal(result$sbp_centile[2], 90.36608, tolerance = 0.01)
})

test_that("get_BP wrapper calls correct functions with 'zscore' option", {
  result <- get_BP(
    option = "zscore",
    bp_value = c(118, 134),
    bp_type = "systolic",
    sex = c(1, 2), male_code = 1, female_code = 2,
    height_z = c(-1.64, -1.64),
    age_years = c(12, 17),
    ref = "Fourth Report",
    .quiet = TRUE
  )

  expect_equal(result[1], 1.528830, tolerance = 0.001)
  expect_equal(result[2], 2.421401, tolerance = 0.001)
})

test_that("get_BP wrapper calls correct functions with 'category' option", {
  result <- df_easy |>
    mutate(
      sbp_cat_c = get_BP(option = "cat",  # Partial match: "cat" -> "category"
        bp_value = systolic_obs, bp_type = "sys",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, height_limit = 5,
        age_years = age, ref = "Fourth"))

  expect_equal(result$sbp_cat_c[1], "Prehypertension")
  expect_equal(result$sbp_cat_c[2], "Normotension")
  expect_equal(result$sbp_cat_c[3], "Stage 1 hypertension")
  expect_equal(result$sbp_cat_c[4], "Stage 2 hypertension")
})

test_that("get_BP works with both references in same pipeline", {
  result <- df_hard |>
    mutate(
      sbp_expected = get_BP(option = "expected",
                            bp_type = "sys",
                            sex = gender, male_code = 1, female_code = 2,
                            height_z = height, height_limit = 5,
                            age_years = age, ref = "Fourth"),
      sbp_centile = get_BP(option = "perc",
                           bp_value = systolic_obs, bp_type = "sys",
                           sex = gender, male_code = 1, female_code = 2,
                           height_z = height, height_limit = 5,
                           age_years = age, ref = "Fourth"),
      sbp_cat_c = get_BP(option = "cat",
                         bp_value = systolic_obs, bp_type = "sys",
                         sex = gender, male_code = 1, female_code = 2,
                         height_z = height, height_limit = 5,
                         age_years = age, ref = "Fourth"),
      sbp_cat_yp = get_BP(option = "cat",
                          bp_value = systolic_obs, bp_type = "sys",
                          bp_limit = c(0, 301),
                          sex = gender, male_code = 1, female_code = 2,
                          height_z = height, height_limit = 5,
                          age_years = age, ref = "NICE"))

  # Children: should have Fourth Report results, NA for NICE
  expect_false(is.na(result$sbp_expected[1]))
  expect_false(is.na(result$sbp_cat_c[1]))
  expect_true(is.na(result$sbp_cat_yp[1]))

  # Adults: should have NA for Fourth Report, results for NICE
  expect_true(is.na(result$sbp_expected[12]))
  expect_true(is.na(result$sbp_cat_c[12]))
  expect_equal(result$sbp_cat_yp[12], "Stage 1 hypertension")
})


# ========== Test Validation and Messages ==========

test_that("Functions produce appropriate messages", {
  # Test age out of range messages
  expect_message(
    get_BPExpected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 20, ref = "Fourth Report"
    ),
    "Ignore.*outside 0-17 years"
  )

  expect_message(
    get_BPExpected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 10, ref = "NICE/BHF"
    ),
    "Ignore.*≤ 17 years"
  )

  # Test height out of range messages
  expect_message(
    get_BPExpected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 10, age_years = 12, ref = "Fourth Report"
    ),
    "Ignore.*beyond.*based on 'height_limit'"
  )

  # Test NICE/BHF limitation messages
  expect_message(
    get_BPExpected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 20, ref = "NICE/BHF"
    ),
    "NICE/BHF only provides category guidelines"
  )
})

test_that(".quiet parameter suppresses messages", {
  # Should not produce messages when .quiet = TRUE
  expect_silent(
    get_BPExpected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 10, age_years = 20, ref = "Fourth Report",
      .quiet = TRUE
    )
  )

  expect_silent(
    get_BPRelative(
      bp_value = 120, bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 10, age_years = 20, ref = "Fourth Report",
      .quiet = TRUE
    )
  )
})


# ========== Test Parameter Validation ==========

test_that("Functions require ref parameter", {
  expect_error(
    get_BPExpected(
      bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 10
      # Missing ref parameter
    ),
    "'ref' must be specified"
  )
})

test_that("Functions require bp_type parameter", {
  expect_error(
    get_BPExpected(
      # Missing bp_type parameter
      sex = 1, male_code = 1, female_code = 2,
      height_z = 0, age_years = 10, ref = "Fourth Report"
    ),
    "'bp_type' must be specified"
  )
})

test_that("Functions accept partial matching for arguments", {
  # bp_type partial matching
  result1 <- get_BPExpected(
    bp_type = "sys", sex = 1, male_code = 1, female_code = 2,
    height_z = 0, age_years = 10, ref = "Fourth", .quiet = TRUE
  )

  result2 <- get_BPExpected(
    bp_type = "dia", sex = 1, male_code = 1, female_code = 2,
    height_z = 0, age_years = 10, ref = "Fourth", .quiet = TRUE
  )

  # ref partial matching
  result3 <- get_BPExpected(
    bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
    height_z = 0, age_years = 10, ref = "Fourth", .quiet = TRUE
  )

  result4 <- get_BPExpected(
    bp_type = "systolic", sex = 1, male_code = 1, female_code = 2,
    height_z = 0, age_years = 20, ref = "NICE", .quiet = TRUE
  )

  expect_type(result1, "double")
  expect_type(result2, "double")
  expect_type(result3, "double")
  expect_type(result4, "logical")
})


# ========== Integration Tests ==========

test_that("Functions work in realistic dplyr pipeline", {
  result <- df_easy |>
    mutate(
      # Calculate all BP metrics in one pipeline
      sbp_exp = get_BPExpected(
        bp_type = "systolic", sex = gender, male_code = 1, female_code = 2,
        height_z = height, age_years = age, ref = "Fourth Report", .quiet = TRUE
      ),
      dbp_exp = get_BPExpected(
        bp_type = "diastolic", sex = gender, male_code = 1, female_code = 2,
        height_z = height, age_years = age, ref = "Fourth Report", .quiet = TRUE
      )
    ) |>
    mutate(
      get_BPRelative(
        bp_value = systolic_obs, bp_type = "systolic",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, age_years = age, ref = "Fourth Report", .quiet = TRUE
      )
    ) |>
    rename(sbp_z = zscore, sbp_pct = percentile) |>
    mutate(
      sbp_cat = get_BPCategory(
        bp_value = systolic_obs, bp_type = "systolic",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, age_years = age, ref = "Fourth Report"
      )
    )

  # Check that all columns were created
  expect_true("sbp_exp" %in% names(result))
  expect_true("dbp_exp" %in% names(result))
  expect_true("sbp_z" %in% names(result))
  expect_true("sbp_pct" %in% names(result))
  expect_true("sbp_cat" %in% names(result))

  # Check that values are reasonable
  expect_true(all(!is.na(result$sbp_exp)))
  expect_true(all(!is.na(result$sbp_cat)))
  expect_true(all(result$sbp_pct >= 0 & result$sbp_pct <= 100))
})

test_that("Functions handle grouped data correctly", {
  result <- df_easy |>
    group_by(gender) |>
    mutate(
      sbp_cat = get_BPCategory(
        bp_value = systolic_obs, bp_type = "systolic",
        sex = gender, male_code = 1, female_code = 2,
        height_z = height, age_years = age, ref = "Fourth Report"
      )
    ) |>
    ungroup()

  expect_equal(nrow(result), 4)
  expect_true(all(!is.na(result$sbp_cat)))
})


# Original raw test by me:
# Test functions  -------------------------------------------------------
# # ========== easy data  ==========
# df_easy <- data.frame(systolic_obs  = c(  118,  118,  134,  139),
#                       diastolic_obs = c(   75,   75,   91,   96),
#                       gender        = c(    1,    1,    2,    2),
#                       age           = c(   12,   13,   17,   17),
#                       height        = c(-1.64,-1.64,-1.64,-1.64))
#
# # ========== hard data  ==========
# df_hard <- data.frame(systolic_obs  = c(  118,  118,  134,  139,   NA,  139,  139,  115,  120,  139,  139,  159,  169, 189, 301, 139),
#                       diastolic_obs = c(   75,   75,   91,   96,   NA,   96,   96,   68,   73,   96,   96,   96,  103, 123, 151,  96),
#                       gender        = c(    1,    1,    2,    2,    2,   NA,    3,    2,    2,    2,    2,    2,    2,   2,   2,   2),
#                       age           = c(   12,   13,   17,   17,   17,   12,   12,  0.5,  0.5,   NA,   -1,   18,   22,  23,  24,  17),
#                       height        = c(-1.64,-1.64,-1.64,-1.64,-1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 1.64, 160, 160, 160))
#
#
# # ========== test get_BPExpected() ==========
# df |>
#   dplyr::mutate(
#     # Get expected SBP
#     sbp_expected = get_BPExpected(bp_type = "sys",
#                                   sex = gender, male_code = 1, female_code = 2,
#                                   height_z = height, height_limit=5,
#                                   age_years = age, ref = "Fourth Report"),
#     # Get expected DBP
#     dbp_expected = get_BPExpected(bp_type = "diastolic",
#                                   sex = gender, male_code = 1, female_code = 2,
#                                   height_z = height, height_limit=5,
#                                   age_years = age, ref = "Fourth Report"),
#     # Return NA if reference doesn't provide guidelines
#     sbp_expected_NICE = get_BPExpected(bp_type = "sys",
#                                        sex = gender, male_code = 1, female_code = 2,
#                                        height_z = height, height_limit=5,
#                                        age_years = age, ref = "NICE")
#   )
#
# # Expected output (easy data):
# # systolic_obs diastolic_obs gender age height sbp_expected dbp_expected sbp_expected_NICE
# # 1          118            75      1  12  -1.64     101.6220     59.71597                NA
# # 2          118            75      1  13  -1.64     104.0445     60.26170                NA
# # 3          134            91      2  17  -1.64     108.6104     64.92422                NA
# # 4          139            96      2  17  -1.64     108.6104     64.92422                NA
#
#
# # Expected output (hard data):
# # systolic_obs diastolic_obs gender  age height sbp_expected dbp_expected sbp_expected_NICE
# # 1           118            75      1 12.0  -1.64    101.62195     59.71597                NA
# # 2           118            75      1 13.0  -1.64    104.04447     60.26170                NA
# # 3           134            91      2 17.0  -1.64    108.61040     64.92422                NA
# # 4           139            96      2 17.0  -1.64    108.61040     64.92422                NA
# # 5            NA            NA      2 17.0  -1.64    108.61040     64.92422                NA
# # 6           139            96     NA 12.0   1.64           NA           NA                NA
# # 7           139            96      3 12.0   1.64           NA           NA                NA
# # 8           115            68      2  0.5   1.64     89.37298     39.21236                NA
# # 9           120            73      2  0.5   1.64     89.37298     39.21236                NA
# # 10          139            96      2   NA   1.64           NA           NA                NA
# # 11          139            96      2 -1.0   1.64           NA           NA                NA
# # 12          159            96      2 18.0   1.64           NA           NA                NA
# # 13          169           103      2 22.0   1.64           NA           NA                NA
# # 14          189           123      2 23.0 160.00           NA           NA                NA
# # 15          301           151      2 24.0 160.00           NA           NA                NA
# # 16          139            96      2 17.0 160.00           NA           NA                NA
#
# # ========== test get_BPRelative() ==========
# df_easy |>
#   # Get full output (zscore and percentile)
#   dplyr::mutate(get_BPRelative(bp_value = systolic_obs, bp_type = "sys",
#                                sex = gender, male_code = 1, female_code = 2,
#                                height_z = height, height_limit=5,
#                                age_years = age, ref = "Fourth Report")) |>
#   dplyr::rename(sbp_z = zscore, sbp_centile = percentile) |>
#   dplyr::mutate(
#     # Get specific output (percentile)|
#     dbp_centile = get_BPRelative(bp_value = diastolic_obs, bp_type = "diastolic",
#                                  sex = gender, male_code = 1, female_code = 2,
#                                  height_z = height, height_limit=5,
#                                  age_years = age, ref = "Fourth Report")$percentile,
#     # Return NA if reference doesn't provide guidelines
#     dbp_centile_NICE = get_BPRelative(bp_value = diastolic_obs, bp_type = "diastolic",
#                                       sex = gender, male_code = 1, female_code = 2,
#                                       height_z = height, height_limit=5,
#                                       age_years = age, ref = "NICE")$percentile,
#   )
#
# # Expected output (easy data):
# # systolic_obs diastolic_obs gender age height    sbp_z sbp_centile dbp_centile dbp_centile_NICE
# # 1          118            75      1  12  -1.64 1.528830    93.68467    90.61185               NA
# # 2          118            75      1  13  -1.64 1.302697    90.36608    89.79920               NA
# # 3          134            91      2  17  -1.64 2.421401    99.22696    99.13381               NA
# # 4          139            96      2  17  -1.64 2.898250    99.81237    99.77165               NA
#
# # Expected output (hard data):
# # systolic_obs diastolic_obs gender  age height    sbp_z sbp_centile dbp_centile dbp_centile_NICE
# # 1           118            75      1 12.0  -1.64 1.528830    93.68467    90.61185               NA
# # 2           118            75      1 13.0  -1.64 1.302697    90.36608    89.79920               NA
# # 3           134            91      2 17.0  -1.64 2.421401    99.22696    99.13381               NA
# # 4           139            96      2 17.0  -1.64 2.898250    99.81237    99.77165               NA
# # 5            NA            NA      2 17.0  -1.64       NA          NA          NA               NA
# # 6           139            96     NA 12.0   1.64       NA          NA          NA               NA
# # 7           139            96      3 12.0   1.64       NA          NA          NA               NA
# # 8           115            68      2  0.5   1.64 2.444043    99.27382    99.56962               NA
# # 9           120            73      2  0.5   1.64 2.920892    99.82548    99.89773               NA
# # 10          139            96      2   NA   1.64       NA          NA          NA               NA
# # 11          139            96      2 -1.0   1.64       NA          NA          NA               NA
# # 12          159            96      2 18.0   1.64       NA          NA          NA               NA
# # 13          169           103      2 22.0   1.64       NA          NA          NA               NA
# # 14          189           123      2 23.0 160.00       NA          NA          NA               NA
# # 15          301           151      2 24.0 160.00       NA          NA          NA               NA
# # 16          139            96      2 17.0 160.00       NA          NA          NA               NA
#
# # ========== test get_BPCategory() ==========
# df |>
#   dplyr::mutate(
#     # Young adults SBP/DBP category (NICE/BHF)
#     sbp_cat_yp = get_BPCategory(bp_value = systolic_obs, bp_type = "sys",
#                                 sex = gender, male_code = 1, female_code = 2,
#                                 height_z = height, height_limit=5,
#                                 age_years = age, ref = "NICE"),
#     dbp_cat_yp = get_BPCategory(bp_value = diastolic_obs, bp_type = "dia", bp_limit = c(0, 151), # Just to demonstrate that you can use other limit than NDA/NPDA's; In reality, this range isn't sensible
#                                 sex = gender, male_code = 1, female_code = 2,
#                                 height_z = height, height_limit=5,
#                                 age_years = age, ref = "NICE"),
#     # Children SBP/DBP category (Fourth report)
#     sbp_cat_c = get_BPCategory(bp_value = systolic_obs, bp_type = "sys",
#                                sex = gender, male_code = 1, female_code = 2,
#                                height_z = height, height_limit=5,
#                                age_years = age, ref = "Fourth"),
#     dbp_cat_c = get_BPCategory(bp_value = diastolic_obs, bp_type = "dia",
#                                sex = gender, male_code = 1, female_code = 2,
#                                height_z = height, height_limit=5,
#                                age_years = age, ref = "Fourth"),
#   )
#
# # Expected output (easy data):
# # systolic_obs diastolic_obs gender age height sbp_cat_yp dbp_cat_yp            sbp_cat_c            dbp_cat_c
# # 1          118            75      1  12  -1.64       <NA>       <NA>      Prehypertension      Prehypertension
# # 2          118            75      1  13  -1.64       <NA>       <NA>         Normotension         Normotension
# # 3          134            91      2  17  -1.64       <NA>       <NA> Stage 1 hypertension Stage 1 hypertension
# # 4          139            96      2  17  -1.64       <NA>       <NA> Stage 2 hypertension Stage 2 hypertension
#
# # Expected output (hard data):
# # systolic_obs diastolic_obs gender  age height           sbp_cat_yp           dbp_cat_yp            sbp_cat_c            dbp_cat_c
# # 1           118            75      1 12.0  -1.64                 <NA>                 <NA>      Prehypertension      Prehypertension
# # 2           118            75      1 13.0  -1.64                 <NA>                 <NA>         Normotension         Normotension
# # 3           134            91      2 17.0  -1.64                 <NA>                 <NA> Stage 1 hypertension Stage 1 hypertension
# # 4           139            96      2 17.0  -1.64                 <NA>                 <NA> Stage 2 hypertension Stage 2 hypertension
# # 5            NA            NA      2 17.0  -1.64                 <NA>                 <NA>                 <NA>                 <NA>
# # 6           139            96     NA 12.0   1.64                 <NA>                 <NA>                 <NA>                 <NA>
# # 7           139            96      3 12.0   1.64                 <NA>                 <NA>                 <NA>                 <NA>
# # 8           115            68      2  0.5   1.64                 <NA>                 <NA> Stage 1 hypertension                 <NA>
# # 9           120            73      2  0.5   1.64                 <NA>                 <NA> Stage 2 hypertension                 <NA>
# # 10          139            96      2   NA   1.64                 <NA>                 <NA>                 <NA>                 <NA>
# # 11          139            96      2 -1.0   1.64                 <NA>                 <NA>                 <NA>                 <NA>
# # 12          159            96      2 18.0   1.64 Stage 1 hypertension Stage 1 hypertension                 <NA>                 <NA>
# # 13          169           103      2 22.0   1.64 Stage 2 hypertension Stage 2 hypertension                 <NA>                 <NA>
# # 14          189           123      2 23.0 160.00 Stage 3 hypertension Stage 3 hypertension                 <NA>                 <NA>
# # 15          301           151      2 24.0 160.00                 <NA> Stage 3 hypertension                 <NA>                 <NA>
# # 16          139            96      2 17.0 160.00                 <NA>                 <NA>                 <NA>                 <NA>
#
# # ========== test get_BP() ==========
# df |>
#   dplyr::mutate(sbp_expected = get_BP(option = "expected",
#                                       # bp_value = systolic_obs,
#                                       bp_type = "sys",
#                                       sex = gender, male_code = 1, female_code = 2,
#                                       height_z = height, height_limit=5,
#                                       age_years = age, ref = "Fourth"),
#                 sbp_centile = get_BP(option="perc",
#                                      bp_value = systolic_obs, bp_type = "sys",
#                                      sex = gender, male_code = 1, female_code = 2,
#                                      height_z = height, height_limit=5,
#                                      age_years = age, ref = "Fourth"),
#                 sbp_cat_c = get_BP(option="cat",
#                                    bp_value = systolic_obs, bp_type = "sys",
#                                    sex = gender, male_code = 1, female_code = 2,
#                                    height_z = height, height_limit=5,
#                                    age_years = age, ref = "Fourth"),
#                 sbp_cat_yp = get_BP(option="cat",
#                                     bp_value = systolic_obs, bp_type = "sys", bp_limit = c(0, 301), # Just to demonstrate that you can use other limit than NDA/NPDA's; In reality, this range isn't sensible
#                                     sex = gender, male_code = 1, female_code = 2,
#                                     height_z = height, height_limit=5,
#                                     age_years = age, ref = "NICE")
#   )
#
# # Expected output (easy data):
# #   systolic_obs diastolic_obs gender age height sbp_expected sbp_centile            sbp_cat_c sbp_cat_yp
# # 1          118            75      1  12  -1.64     101.6220    93.68467      Prehypertension       <NA>
# # 2          118            75      1  13  -1.64     104.0445    90.36608         Normotension       <NA>
# # 3          134            91      2  17  -1.64     108.6104    99.22696 Stage 1 hypertension       <NA>
# # 4          139            96      2  17  -1.64     108.6104    99.81237 Stage 2 hypertension       <NA>
#
# # Expected output (hard data):
# #    systolic_obs diastolic_obs gender  age height sbp_expected sbp_centile            sbp_cat_c           sbp_cat_yp
# # 1           118            75      1 12.0  -1.64    101.62195    93.68467      Prehypertension                 <NA>
# # 2           118            75      1 13.0  -1.64    104.04447    90.36608         Normotension                 <NA>
# # 3           134            91      2 17.0  -1.64    108.61040    99.22696 Stage 1 hypertension                 <NA>
# # 4           139            96      2 17.0  -1.64    108.61040    99.81237 Stage 2 hypertension                 <NA>
# # 5            NA            NA      2 17.0  -1.64    108.61040          NA                 <NA>                 <NA>
# # 6           139            96     NA 12.0   1.64           NA          NA                 <NA>                 <NA>
# # 7           139            96      3 12.0   1.64           NA          NA                 <NA>                 <NA>
# # 8           115            68      2  0.5   1.64     89.37298    99.27382 Stage 1 hypertension                 <NA>
# # 9           120            73      2  0.5   1.64     89.37298    99.82548 Stage 2 hypertension                 <NA>
# # 10          139            96      2   NA   1.64           NA          NA                 <NA>                 <NA>
# # 11          139            96      2 -1.0   1.64           NA          NA                 <NA>                 <NA>
# # 12          159            96      2 18.0   1.64           NA          NA                 <NA> Stage 1 hypertension
# # 13          169           103      2 22.0   1.64           NA          NA                 <NA> Stage 2 hypertension
# # 14          189           123      2 23.0 160.00           NA          NA                 <NA> Stage 3 hypertension
# # 15          301           151      2 24.0 160.00           NA          NA                 <NA> Stage 3 hypertension
# # 16          139            96      2 17.0 160.00           NA          NA                 <NA>                 <NA>


