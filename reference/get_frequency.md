# Summarise categorical measure(s) by group(s)

For each specified grouping variable, count the frequency of each unique
category of the given measure column(s), and compute the denominator
(non-missing categories by default) and percentage. Results for the
specified group(s) are combined into a single long tibble for easy use
in \`ggplot\` or \`plotly\`.

## Usage

``` r
get_frequency(
  data,
  measures,
  groups = "overall",
  nested = FALSE,
  count_na = FALSE
)
```

## Arguments

- data:

  A data frame containing measure columns (and grouping columns), with
  one row per participant.

- measures:

  A character vector of column names to summarise. All columns will be
  coerced to character, so this works for logical, factor, and character
  columns alike. Measures appear in the output in this order.

- groups:

  A character vector of grouping column names. Defaults to `"overall"`,
  which creates a single group containing all rows. Rows where the
  grouping variable is `NA` are excluded from that group's summary.

- nested:

  Logical; if `FALSE` (default), each grouping variable in `groups` is
  summarised separately. If `TRUE`, all variables in `groups` are
  treated as a nested grouping set.

- count_na:

  Logical; if `FALSE` (default), missing values in `measures` are
  excluded from the numerator and denominator. If `TRUE`, missing values
  are counted as their own category (`category = NA`) and included in
  the denominator, but only for measures that contain at least one
  missing value (like `table(useNA = "ifany")`). Missing values in
  `groups` are always excluded.

## Value

A tibble in long format with one row per group level × measure ×
category combination.

## Examples

``` r

library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

set.seed(1999)
df <- data.frame(
  participant_id = 1:60,
  country        = c(rep("England", 30), rep("Wales", 30)),
  region         = c(
    rep("East England", 10), rep("West England", 10), rep(NA, 10),
    rep("North Wales", 10), rep("South Wales", 10), rep(NA, 10)
  ),
  # Categorical Q1
  q1_catq        = sample(c("A", "B", "C", NA), 60, replace = TRUE),
  # Categorical Q2
  q2_catq        = sample(c("A", "B", "C", "D", "E", NA), 60, replace = TRUE)
)

group_cols <- c("overall", "country", "region")

measure_cols <- df |>
  select(tidyselect::matches("q[0-9]+_catq")) |>
  names()

freq <- get_frequency(
  data     = df,
  measures = measure_cols,
  groups   = group_cols
)

head(freq)
#> # A tibble: 6 × 8
#>   overall country region measure category numerator denominator percent
#>   <chr>   <chr>   <chr>  <chr>   <chr>        <dbl>       <dbl>   <dbl>
#> 1 overall NA      NA     q1_catq A               10          41   0.244
#> 2 overall NA      NA     q1_catq B               18          41   0.439
#> 3 overall NA      NA     q1_catq C               13          41   0.317
#> 4 overall NA      NA     q2_catq A                9          54   0.167
#> 5 overall NA      NA     q2_catq B               16          54   0.296
#> 6 overall NA      NA     q2_catq C               10          54   0.185

# Nested grouping example (region is nested within country):
sum_q1_nested <- get_frequency(
  data = df,
  measures = "q1_catq",
  groups = c("country", "region"),
  nested = TRUE
)

sum_q1_nested
#> # A tibble: 12 × 7
#>    country region       measure category numerator denominator percent
#>    <chr>   <chr>        <chr>   <chr>        <int>       <int>   <dbl>
#>  1 England East England q1_catq A                2           7   0.286
#>  2 England East England q1_catq B                4           7   0.571
#>  3 England East England q1_catq C                1           7   0.143
#>  4 England West England q1_catq A                1           7   0.143
#>  5 England West England q1_catq B                3           7   0.429
#>  6 England West England q1_catq C                3           7   0.429
#>  7 Wales   North Wales  q1_catq A                1           7   0.143
#>  8 Wales   North Wales  q1_catq B                2           7   0.286
#>  9 Wales   North Wales  q1_catq C                4           7   0.571
#> 10 Wales   South Wales  q1_catq A                3           9   0.333
#> 11 Wales   South Wales  q1_catq B                4           9   0.444
#> 12 Wales   South Wales  q1_catq C                2           9   0.222

# Count missing values as a category
get_frequency(
  data = df,
  measures = "q1_catq",
  groups = "country",
  count_na = TRUE
)
#> # A tibble: 8 × 6
#>   country measure category numerator denominator percent
#>   <chr>   <chr>   <chr>        <int>       <int>   <dbl>
#> 1 England q1_catq A                5          30   0.167
#> 2 England q1_catq B               10          30   0.333
#> 3 England q1_catq C                6          30   0.2  
#> 4 England q1_catq NA               9          30   0.3  
#> 5 Wales   q1_catq A                5          30   0.167
#> 6 Wales   q1_catq B                8          30   0.267
#> 7 Wales   q1_catq C                7          30   0.233
#> 8 Wales   q1_catq NA              10          30   0.333
```
