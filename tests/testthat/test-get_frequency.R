library(dplyr)
library(tidyr)

# ========== Test Data Setup ==========
set.seed(1999)
df <- data.frame(
  participant_id      = 1:60,
  country             = c(rep("England", 30), rep("Wales", 30)),
  region              = c(rep("East England", 10), rep("West England", 10), rep(NA, 10),
                          rep("North Wales", 10), rep("South Wales", 10), rep(NA, 10)),
  # Categorical Q1 (character A–E, with some NAs)
  q1_catq = sample(c("A", "B", "C", NA), 60, replace = TRUE),
  # Categorical Q2 (character A–E, with some NAs)
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

sum <- get_frequency(data = df, measures = measure_cols, groups = group_cols)
sum_nested <- get_frequency(data = df_nested, measures = "type", groups = c("auditYear", "sex"), nested = TRUE)

# ========== Tests ==========

##### Test NA/single value #####
test_that("NAs are excluded from numerator and denominator", {
  expect_equal(sum(is.na(sum$category)), 0)
  expect_false("NA" %in% unique(sum$category))
  expect_false(any(is.na(sum$category)))
})

test_that("Unobserved logical level should still be preserved", {
  q1B_distinct <- sum |>
    filter(measure == "q3_mcq_optionB") |>
    distinct(category) |>
    pull(category)

  q1C_distinct <- sum |>
    filter(measure == "q3_mcq_optionC") |>
    distinct(category) |>
    pull(category)

  expect_setequal(q1B_distinct, c("TRUE", "FALSE"))
  expect_setequal(q1C_distinct, c("TRUE", "FALSE"))
})

test_that("Unobserved logical level gets numerator 0", {
  q1B_true <- sum |>
    filter(measure == "q3_mcq_optionB" & category == "TRUE" & overall == "overall")

  expect_equal(nrow(q1B_true), 1)
  expect_equal(q1B_true$numerator, 0)
  expect_equal(q1B_true$denominator, 60)
  expect_equal(q1B_true$percent, 0)
})

test_that("get_frequency() handles a measure with all NA values", {
  df_allna <- df |> mutate(q1_catq = NA_character_)
  sum_test <- get_frequency(data = df_allna,
                            measures = "q1_catq")
  # All NA means no rows survive the filter(!is.na(category)) step
  expect_equal(nrow(sum_test |> filter(measure == "q1_catq")), 0)
})

test_that("get_frequency() handles a measure that is all one value", {
  df_const <- df |> mutate(q4_catq = "A")
  result <- get_frequency(data = df_const,
                          measures = "q4_catq",
                          groups   = "overall")
  expect_equal(nrow(result |> filter(measure == "q4_catq")), 1)
  expect_equal(result$percent[result$measure == "q4_catq"], 1)
})


##### Test denominator #####
test_that("Overall denominator equals total participants for a logical measure without NA", {
  overall_q1A_denominator <- sum |>
    filter(overall == "overall" & measure == "q3_mcq_optionA") |>
    distinct(denominator) |>
    pull()
  expect_equal(overall_q1A_denominator, nrow(df))
})

test_that("Region denominators equal all participants in the region for a logical measure without NA", {
  region_q1A_denominator <- sum |>
    filter(!is.na(region) & measure == "q3_mcq_optionA") |>
    distinct(region, denominator)

  region_q1A_denominator <- setNames(region_q1A_denominator$denominator,
                                     region_q1A_denominator$region)

  expect_equal(
    region_q1A_denominator,
    c(table(df$region))
  )
})


##### Test numerator/percent #####
test_that("q3_mcq_optionA is exactly 50/50 T/F overall", {
  expect_equal(sum |> filter(measure == "q3_mcq_optionA" & overall == "overall" & category == "TRUE") |> pull(percent), 0.5)
  expect_equal(sum |> filter(measure == "q3_mcq_optionA" & overall == "overall" & category == "FALSE") |> pull(percent), 0.5)
})

test_that("q3_mcq_optionA is exactly 100/0 T/F for England and 0/100 for Wales", {
  expect_equal(sum |> filter(measure == "q3_mcq_optionA" & country == "England" & category == "TRUE") |> pull(percent), 1)
  expect_equal(sum |> filter(measure == "q3_mcq_optionA" & country == "England" & category == "FALSE") |> pull(percent), 0)
  expect_equal(sum |> filter(measure == "q3_mcq_optionA" & country == "Wales" & category == "TRUE") |> pull(percent), 0)
  expect_equal(sum |> filter(measure == "q3_mcq_optionA" & country == "Wales" & category == "FALSE") |> pull(percent), 1)
})

test_that("percent sums to 1 within each group x measure", {
  sum |>
    group_by(overall, country, region, measure) |>
    summarise(total_perc = sum(percent), .groups = "drop") |>
    pull(total_perc) |>
    (\(x) expect_true(all(abs(x - 1) < 1e-10)))()
})

test_that("numerator sums to denominator within each group x measure", {
  sum |>
    group_by(overall, country, region, measure) |>
    summarise(sum_num = sum(numerator),
              denom   = unique(denominator),
              .groups = "drop") |>
    (\(x) expect_true(all(x$sum_num == x$denom)))()
})


##### Test (nested) group #####

test_that("get_frequency() works without specifying group (default overall)", {
  sum_test <- get_frequency(data = df, measures = measure_cols)
  expect_true("overall" %in% names(sum_test))
  expect_false("country" %in% names(sum_test))
})

test_that("get_frequency() keeps separate grouping by default", {
  # Separate summaries should include rows where one grouping column is NA
  expect_true(any(!is.na(sum$overall) & is.na(sum$country) & is.na(sum$region)))
  expect_true(any(is.na(sum$overall) & !is.na(sum$country) & is.na(sum$region)))
  expect_true(any(is.na(sum$overall) & is.na(sum$country) & !is.na(sum$region)))
})

test_that("get_frequency() supports nested grouping", {
  # Nested output should not have NA grouping columns, because rows with missing group membership are excluded
  expect_false(any(is.na(sum_nested$auditYear)))
  expect_false(any(is.na(sum_nested$sex)))
})

test_that("get_frequency() nested grouping results are correct", {
  # Easy one
  expect_equal(sum_nested |> filter(auditYear == 2021 & sex == "Female") |> pull(percent), c(0.5, 0.5))
  # NA in measure is excluded
  expect_equal(sum_nested |> filter(auditYear == 2022 & sex == "Male") |> pull(percent), c(0, 1))
  # NA in group is excluded
  expect_equal(sum_nested |> filter(auditYear == 2023) |> pull(percent), c(1, 0, 0, 1))
})
