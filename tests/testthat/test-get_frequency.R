library(dplyr)
library(tidyr)

# ========== Test Data Setup ==========
set.seed(1999)
df <- data.frame(
  participant_id      = 1:60,
  country             = c(rep("England", 30), rep("Wales", 30)),
  region              = c(rep("East England", 10), rep("West England", 10), rep(NA, 10),
                          rep("North Wales", 10), rep("South Wales", 10), rep(NA, 10)),
  # Q1 MCQ (logical T/F)
  q1_mcq_optionA         = c(rep(TRUE, 30), rep(FALSE, 30)),
  q1_mcq_optionB         = FALSE, # no one selected this option
  q1_mcq_optionC         = TRUE,  # everyone selected this option
  # Q2 MCQ (logical T/F)
  q2_mcq_optionX         = sample(c(TRUE, FALSE), 60, replace = TRUE),
  q2_mcq_optionY         = sample(c(TRUE, FALSE), 60, prob = c(0.4, 0.6), replace = TRUE),
  q2_mcq_optionZ         = sample(c(TRUE, FALSE), 60, prob = c(0.8, 0.2), replace = TRUE),
  # Q3 categorical (character A–E, with some NAs)
  q3_catq = sample(c("A", "B", "C", NA), 60, replace = TRUE),
  # Q4 categorical (character A–E, with some NAs)
  q4_catq = sample(c("A", "B", "C", "D", "E", NA), 60, replace = TRUE)
)

df <- df |>
  mutate(q1_mcq_optionNoneAbove = !if_any(c(q1_mcq_optionA, q1_mcq_optionB, q1_mcq_optionC)), .before = q2_mcq_optionX) |>
  mutate(q2_mcq_optionNoneAbove = !if_any(c(q2_mcq_optionX, q2_mcq_optionY, q2_mcq_optionZ)), .before = q3_catq)

# ========== Run get_frequency() ==========
group_cols <- c("overall", "country", "region")

measure_cols <- df |>
  select(matches("q[0-9]+_mcq_.*") |   # logical columns
           matches("q[0-9]+_catq")) |> # categorical columns
  names()

summary_long <- get_frequency(data = df, measures = measure_cols, groups = group_cols)

# ========== Tests ==========

##### Test NA/single value #####
test_that("NAs are excluded from numerator and denominator", {
  expect_equal(sum(is.na(summary_long$category)), 0)
  expect_false("NA" %in% unique(summary_long$category))
  expect_false(any(is.na(summary_long$category)))
})

test_that("Unobserved logical level should still be preserved", {
  q1B_distinct <- summary_long |>
    filter(measure == "q1_mcq_optionB") |>
    distinct(category) |>
    pull(category)

  q1C_distinct <- summary_long |>
    filter(measure == "q1_mcq_optionC") |>
    distinct(category) |>
    pull(category)

  expect_setequal(q1B_distinct, c("TRUE", "FALSE"))
  expect_setequal(q1C_distinct, c("TRUE", "FALSE"))
})

test_that("Unobserved logical level gets numerator 0", {
  q1B_true <- summary_long |>
    filter(measure == "q1_mcq_optionB" & category == "TRUE" & overall == "overall")

  expect_equal(nrow(q1B_true), 1)
  expect_equal(q1B_true$numerator, 0)
  expect_equal(q1B_true$denominator, 60)
  expect_equal(q1B_true$percent, 0)
})

test_that("get_frequency() handles a measure with all NA values", {
  df_allna <- df |> mutate(q3_catq = NA_character_)
  summary_test <- get_frequency(data = df_allna,
                                measures = "q3_catq")
  # All NA means no rows survive the filter(!is.na(category)) step
  expect_equal(nrow(summary_test |> filter(measure == "q3_catq")), 0)
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
  overall_q1A_denominator <- summary_long |>
    filter(overall == "overall" & measure == "q1_mcq_optionA") |>
    distinct(denominator) |>
    pull()
  expect_equal(overall_q1A_denominator, nrow(df))
})

test_that("Region denominators equal all participants in the region for a logical measure without NA", {
  region_q1A_denominator <- summary_long |>
    filter(!is.na(region) & measure == "q1_mcq_optionA") |>
    distinct(region, denominator)

  region_q1A_denominator <- setNames(region_q1A_denominator$denominator,
                                     region_q1A_denominator$region)

  expect_equal(
    region_q1A_denominator,
    c(table(df$region))
  )
})


##### Test numerator/percent #####
test_that("q1_mcq_optionA is exactly 50/50 T/F overall", {
  expect_equal(summary_long |> filter(measure == "q1_mcq_optionA" & overall == "overall" & category == "TRUE") |> pull(percent), 0.5)
  expect_equal(summary_long |> filter(measure == "q1_mcq_optionA" & overall == "overall" & category == "FALSE") |> pull(percent), 0.5)
})

test_that("q1_mcq_optionA is exactly 100/0 T/F for England and 0/100 for Wales", {
  expect_equal(summary_long |> filter(measure == "q1_mcq_optionA" & country == "England" & category == "TRUE") |> pull(percent), 1)
  expect_equal(summary_long |> filter(measure == "q1_mcq_optionA" & country == "England" & category == "FALSE") |> pull(percent), 0)
  expect_equal(summary_long |> filter(measure == "q1_mcq_optionA" & country == "Wales" & category == "TRUE") |> pull(percent), 0)
  expect_equal(summary_long |> filter(measure == "q1_mcq_optionA" & country == "Wales" & category == "FALSE") |> pull(percent), 1)
})

test_that("percent sums to 1 within each group x measure", {
  summary_long |>
    group_by(overall, country, region, measure) |>
    summarise(total_perc = sum(percent), .groups = "drop") |>
    pull(total_perc) |>
    (\(x) expect_true(all(abs(x - 1) < 1e-10)))()
})

test_that("numerator sums to denominator within each group x measure", {
  summary_long |>
    group_by(overall, country, region, measure) |>
    summarise(sum_num = sum(numerator),
              denom   = unique(denominator),
              .groups = "drop") |>
    (\(x) expect_true(all(x$sum_num == x$denom)))()
})

##### Test group #####
test_that("get_frequency() works without specifying group (default overall)", {
  summary_test <- get_frequency(data = df, measures = measure_cols)
  expect_true("overall" %in% names(summary_test))
  expect_false("country" %in% names(summary_test))
})

