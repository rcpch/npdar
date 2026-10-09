library(dplyr)

# ========== Test Data Setup ==========
set.seed(1999)
df <- data.frame(
  participant_id      = 1:60,
  country             = c(rep("England", 30), rep("Wales", 30)),
  region              = c(rep("East England", 10), rep("West England", 10), rep(NA, 10),
                          rep("North Wales", 10), rep("South Wales", 10), rep(NA, 10)),
  # Categorical Q1 (character A–C, with some NAs)
  q1_catq = sample(c("A", "B", "C", NA), 60, replace = TRUE),
  # Categorical Q4 (character A–E, with some NAs)
  q4_catq = sample(c("A", "B", "C", "D", "E", NA), 60, replace = TRUE),
  # MCQ Q3 (logical T/F)
  q3_mcq_optionA         = c(rep(TRUE, 30), rep(FALSE, 30)),
  q3_mcq_optionB         = FALSE, # no one selected this option
  q3_mcq_optionC         = TRUE,  # everyone selected this option
  # MCQ Q4 (logical T/F)
  q4_mcq_optionX         = sample(c(TRUE, FALSE), 60, replace = TRUE),
  q4_mcq_optionY         = sample(c(TRUE, FALSE), 60, prob = c(0.4, 0.6), replace = TRUE),
  q4_mcq_optionZ         = sample(c(TRUE, FALSE), 60, prob = c(0.8, 0.2), replace = TRUE)
)

df <- df |>
  mutate(q3_mcq_optionNoneAbove = !if_any(c(q3_mcq_optionA, q3_mcq_optionB, q3_mcq_optionC)), .before = q4_mcq_optionX) |>
  mutate(q4_mcq_optionNoneAbove = !if_any(c(q4_mcq_optionX, q4_mcq_optionY, q4_mcq_optionZ)), .before = q1_catq)

# A different structure for nested grouping
set.seed(1999)
df_nested <- data.frame(
  auditYear = c(rep(2021, 3), rep(2022, 3), rep(2023, 6)),
  sex = c("Female", "Female", "Male",
          "Female", "Male", "Male",
          rep("Female", 1), rep("Male", 3), rep(NA, 2)),
  type = c("A", "B", "B",
           "A", "B", NA,
           sample(c("A", "B", NA), 6, replace = TRUE))
)


# ========== Run get_frequency() ==========
group_cols <- c("overall", "country", "region")

measure_cols <- df |>
  select(matches("q[0-9]+_mcq_.*") |   # logical columns
           matches("q[0-9]+_catq")) |> # categorical columns
  names()

freq <- get_frequency(data = df,
                      measures = measure_cols,
                      groups = group_cols)
freq_nested <- get_frequency(data = df_nested,
                             measures = "type",
                             groups = c("auditYear", "sex"),
                             nested = TRUE)
freq_na <- get_frequency(data = df,
                         measures = c("q1_catq", "q3_mcq_optionA"),
                         groups = c("overall", "country"),
                         count_na = TRUE)

# ========== Tests ==========

##### Test NA/single value #####
test_that("NAs are excluded from numerator and denominator", {
  expect_false("NA" %in% unique(freq$category))
  expect_false(any(is.na(freq$category)))
})

test_that("Unobserved logical level should still be preserved", {
  optionB_distinct <- freq |>
    filter(measure == "q3_mcq_optionB") |>
    distinct(category) |>
    pull(category)

  optionC_distinct <- freq |>
    filter(measure == "q3_mcq_optionC") |>
    distinct(category) |>
    pull(category)

  expect_setequal(optionB_distinct, c("TRUE", "FALSE"))
  expect_setequal(optionC_distinct, c("TRUE", "FALSE"))
})

test_that("Unobserved logical level gets numerator 0", {
  optionB_true <- freq |>
    filter(measure == "q3_mcq_optionB" & category == "TRUE" & overall == "overall")

  expect_equal(nrow(optionB_true), 1)
  expect_equal(optionB_true$numerator, 0)
  expect_equal(optionB_true$denominator, 60)
  expect_equal(optionB_true$percent, 0)
})

test_that("get_frequency() handles a measure with all NA values", {
  df_allna <- df |> mutate(q1_catq = NA_character_)
  result <- get_frequency(data = df_allna,
                          measures = "q1_catq")
  # All NA means no rows survive the filter(!is.na(category)) step
  expect_equal(nrow(result |> filter(measure == "q1_catq")), 0)
})

test_that("get_frequency() handles a measure that is all one value", {
  df_const <- df |> mutate(q4_catq = "A")
  result <- get_frequency(data = df_const,
                          measures = "q4_catq",
                          groups   = "overall")
  expect_equal(nrow(result |> filter(measure == "q4_catq")), 1)
  expect_equal(result$percent[result$measure == "q4_catq"], 1)
})


##### Test count_na #####
test_that("count_na = FALSE (default) excludes NA categories", {
  expect_false(any(is.na(freq$category)))
})

test_that("count_na = TRUE adds an NA category that counts missing values", {
  q1_na <- freq_na |>
    filter(measure == "q1_catq" & is.na(category))

  expect_equal(q1_na |> filter(overall == "overall") |> pull(numerator),
               sum(is.na(df$q1_catq)))

  england_na <- q1_na |> filter(country == "England") |> pull(numerator)
  expect_equal(england_na, sum(is.na(df$q1_catq[df$country == "England"])))
})

test_that("count_na = TRUE includes NAs in the denominator and percent sums to 1", {
  q1_overall <- freq_na |>
    filter(measure == "q1_catq" & overall == "overall")

  expect_equal(unique(q1_overall$denominator), nrow(df))
  expect_equal(sum(q1_overall$percent), 1)
})

test_that("count_na = TRUE does not change non-NA numerators", {
  non_na <- \(x) {
    x |>
      filter(measure == "q1_catq" & overall == "overall" & !is.na(category)) |>
      pull(numerator)}
  expect_equal(non_na(freq_na), non_na(freq))
})

test_that("count_na = TRUE adds no NA category to a measure without missing values", {
  expect_false(any(is.na(freq_na$category[freq_na$measure == "q3_mcq_optionA"])))
})

test_that("count_na = TRUE keeps an all-NA measure as a single NA category", {
  df_allna <- df |> mutate(q1_catq = NA_character_)
  result <- get_frequency(data = df_allna, measures = "q1_catq", count_na = TRUE)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$category))
  expect_equal(result$numerator, nrow(df))
  expect_equal(result$percent, 1)
})

test_that("count_na = TRUE places the NA category last within each measure", {
  q1_overall <- freq_na |>
    filter(measure == "q1_catq" & overall == "overall")

  expect_true(is.na(q1_overall$category[nrow(q1_overall)]))
})


##### Test user-specified order #####
test_that("measures are ordered as specified by the user", {
  measures_rev <- c("q4_catq", "q1_catq")
  result <- get_frequency(data = df, measures = measures_rev, groups = "country")

  # Order follows the argument, not alphabetical order
  expect_equal(unique(result$measure), measures_rev)

  # Measure order holds within each group, not just overall
  for (ctry in unique(result$country)) {
    expect_equal(unique(result$measure[result$country == ctry]), measures_rev)
  }
})


##### Test denominator #####
test_that("Overall denominator equals total participants for a logical measure without NA", {
  overall_denominator <- freq |>
    filter(overall == "overall" & measure == "q3_mcq_optionA") |>
    distinct(denominator) |>
    pull()
  expect_equal(overall_denominator, nrow(df))
})

test_that("Region denominators equal all participants in the region for a logical measure without NA", {
  region_denominator <- freq |>
    filter(!is.na(region) & measure == "q3_mcq_optionA") |>
    distinct(region, denominator)

  region_denominator <- setNames(region_denominator$denominator,
                                 region_denominator$region)

  expect_equal(
    region_denominator,
    c(table(df$region))
  )
})


##### Test numerator/percent #####
test_that("q3_mcq_optionA is exactly 50/50 T/F overall", {
  expect_equal(freq |> filter(measure == "q3_mcq_optionA" & overall == "overall" & category == "TRUE") |> pull(percent), 0.5)
  expect_equal(freq |> filter(measure == "q3_mcq_optionA" & overall == "overall" & category == "FALSE") |> pull(percent), 0.5)
})

test_that("q3_mcq_optionA is exactly 100/0 T/F for England and 0/100 for Wales", {
  expect_equal(freq |> filter(measure == "q3_mcq_optionA" & country == "England" & category == "TRUE") |> pull(percent), 1)
  expect_equal(freq |> filter(measure == "q3_mcq_optionA" & country == "England" & category == "FALSE") |> pull(percent), 0)
  expect_equal(freq |> filter(measure == "q3_mcq_optionA" & country == "Wales" & category == "TRUE") |> pull(percent), 0)
  expect_equal(freq |> filter(measure == "q3_mcq_optionA" & country == "Wales" & category == "FALSE") |> pull(percent), 1)
})

test_that("percent sums to 1 within each group x measure", {
  freq |>
    group_by(overall, country, region, measure) |>
    summarise(total_perc = sum(percent), .groups = "drop") |>
    pull(total_perc) |>
    (\(x) expect_true(all(abs(x - 1) < 1e-10)))()
})

test_that("numerator sums to denominator within each group x measure", {
  freq |>
    group_by(overall, country, region, measure) |>
    summarise(sum_num = sum(numerator),
              denom   = unique(denominator),
              .groups = "drop") |>
    (\(x) expect_true(all(x$sum_num == x$denom)))()
})


##### Test (nested) group #####

test_that("get_frequency() works without specifying group (default overall)", {
  result <- get_frequency(data = df, measures = measure_cols)
  expect_true("overall" %in% names(result))
  expect_false("country" %in% names(result))
})

test_that("get_frequency() keeps separate grouping by default", {
  # Separate summaries should include rows where one grouping column is NA
  expect_true(any(!is.na(freq$overall) & is.na(freq$country) & is.na(freq$region)))
  expect_true(any(is.na(freq$overall) & !is.na(freq$country) & is.na(freq$region)))
  expect_true(any(is.na(freq$overall) & is.na(freq$country) & !is.na(freq$region)))
})

test_that("get_frequency() supports nested grouping", {
  # Nested output should not have NA grouping columns, because rows with missing group membership are excluded
  expect_false(any(is.na(freq_nested$auditYear)))
  expect_false(any(is.na(freq_nested$sex)))
})

test_that("get_frequency() nested grouping results are correct", {
  # Easy one
  expect_equal(freq_nested |> filter(auditYear == 2021 & sex == "Female") |> pull(percent), c(0.5, 0.5))
  # NA in measure is excluded
  expect_equal(freq_nested |> filter(auditYear == 2022 & sex == "Male") |> pull(percent), c(0, 1))
  # NA in group is excluded
  expect_equal(freq_nested |> filter(auditYear == 2023) |> pull(percent), c(1, 0, 0, 1))
})
