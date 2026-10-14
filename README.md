

# npdar

<!-- badges: start -->

[![R
Package](https://img.shields.io/badge/R-package-blue.png)](https://github.com/rcpch/npdar)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue.png)](https://rcpch.github.io/npdar/)
[![GPLv3
license](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/rcpch/npdar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rcpch/npdar/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

**npdar** provides R utilities for the [National Paediatric Diabetes
Audit (NPDA)](https://www.rcpch.ac.uk/work-we-do/clinical-audits/npda),
developed and maintained by the [Royal College of Paediatrics and Child
Health (RCPCH)](https://www.rcpch.ac.uk/).

The package supports:

- **Blood pressure assessment:** calculate expected values, z-scores and
  percentiles, and categorise blood pressure using paediatric or adult
  references.

- **Statistical utilities:**

  1.  mask small counts and apply secondary suppression to reduce the
      risk of back-calculation for data privacy;

  2.  identify first/last/most recent modal values or first/last entry
      in a longitudinal record with options to ignore specific values;

  3.  summarise categorical measure(s) by group(s) with counts,
      denominators and percentages in long format for easy use in
      `ggplot2` and `plotly`.

- **Audit period identification:** identify audit/fiscal years and
  quarters using a configurable first-quarter start month.

- **Correlation visualisation:** create interactive correlation matrix
  with `plotly` and `stats::cor()`.

See the [package documentation](https://rcpch.github.io/npdar/) for full
function references and examples.

## Disclaimer :warning:

**This package is designed for:**

- Assisting NPDA data analysis.
- Improving transparency and reproducibility of NPDA analysis.
- Helping other research and epidemiological analysis.

**This package is NOT:**

- A clinical decision support or diagnosis tool.
- Approved for use in clinical care pathways.

Healthcare professionals must use validated clinical tools and their
professional judgement when making patient care decisions. The blood
pressure categorisation functions implement published research
guidelines (NHBPEP Fourth Report, NICE/BHF guidances) **for audit
analysis purposes only, not for clinical use**.

## Installation

**npdar** is not currently available on CRAN. Install the development
version from GitHub using [pak](https://pak.r-lib.org/):

``` r
install.packages("pak")
pak::pak("rcpch/npdar")

# devtools::install_github() was deprecated 
# in favour of pak::pak() since devtools 2.5.0
# install.packages("devtools")
# devtools::install_github("rcpch/npdar")
```

Alternatively, use [remotes](https://remotes.r-lib.org/):

``` r
install.packages("remotes")
remotes::install_github("rcpch/npdar")
```

## Quick Start

``` r
library(npdar)
```

### Blood Pressure

It’s recommended to use the NHBPEP Fourth Report reference for children
and adolescents aged 0-17 years. Height must already be expressed as a
z-score, not in centimetres. The 25-year-old below is outside this
reference’s age range, so returns `NA`.

``` r
bp_assess(
  option = "category",
  bp_value = c(95, 125, 155),
  bp_type = "systolic",
  sex = c(1, 2, 1),
  male_code = 1, female_code = 2,
  height_z = c(0, 0.5, -0.5),
  age_years = c(10, 15, 25),
  ref = "Fourth Report"
)
#> Ignore 1 age(s) outside 0-17 years. NHBPEP Fourth Report is only designed for children <= 17 years.
#> NHBPEP Fourth Report categorise 'Normotension' to 'Stage 2 hypertension'; Special rules for newborns (<1), children (0-12), adolescents (12-17).
#> BP outside National Paediatric Diabetes Audit (NPDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.
#> [1] "Normotension"    "Prehypertension" NA
```

For adults older than 17 years, it’s recommended to select the NICE/BHF
reference explicitly. Ages outside the selected reference range return
`NA`.

``` r
bp_assess(
  option = "category",
  bp_value = c(155, 155),
  bp_type = "systolic",
  sex = c(1, 1),
  male_code = 1, female_code = 2,
  height_z = c(0, 0),
  age_years = c(15, 25),
  ref = "NICE/BHF"
)
#> Ignore 1 age(s) ≤ 17 years. NPDA only uses NICE/BHF reference for (young) adults >17 years.
#> NICE/BHF categorise 'Normotension' to 'Stage 3 hypertension'.
#> BP outside National Diabetes Audit (NDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.
#> [1] NA                     "Stage 1 hypertension"
```

### Privacy Masking

By default, `get_masked()` masks positive counts below 3 and leaves
zeros unchanged. If only one count is masked, all occurrences of the
next smallest positive value are also masked to prevent
back-calculation. The result is a character vector. Here only `2` is
below 3, so the `4`s are masked as well.

``` r
get_masked(c(4, 6, 2, 4, 0, 5, 5))
#> [1] "masked" "6"      "masked" "masked" "0"      "5"      "5"
```

Customise the threshold and the suppression label:

``` r
get_masked(c(4, 6, 2, 4, 0, 5, 5), threshold = 5, mask_label = "*")
#> [1] "*" "6" "*" "*" "0" "5" "5"
get_masked(c(4, 6, 2, 4, 0, 5, 5), threshold = 6, mask_label = "*")
#> [1] "*" "6" "*" "*" "0" "*" "*"
```

### Longitudinal Summaries

`get_ultimate()` uses vector order, so sort longitudinal records
chronologically before selecting a most recent value. These examples
exclude unknown and missing values and deliberately include tied modes;
warnings explain which modes were not selected.

``` r
dat_long <- c("A", "B", "C", "A", "A", "Unknown", "Unknown",
          NA, "C", "C", "B", "B", "Unknown")
```

Select the tied mode that occurs most recently:

``` r
get_ultimate(dat_long, find = "uptodate_mode", exclude = c("Unknown", NA))
#> Warning in get_ultimate(dat_long, find = "uptodate_mode", exclude = c("Unknown",
#> : There are multiple modes, the remaining modes are: A, C
#> [1] "B"
```

Use the package’s `last_mode` rule instead:

``` r
get_ultimate(dat_long, find = "last_mode", exclude = c("Unknown", NA))
#> Warning in get_ultimate(dat_long, find = "last_mode", exclude = c("Unknown", :
#> There are multiple modes, the remaining modes are: A, B
#> [1] "C"
```

### Categorical Frequencies

The following examples use synthetic data. `get_frequency()` returns a
long table with a numerator, denominator and proportion (`percent`, on a
0-1 scale) for each category. Missing measure values are excluded by
default; use `count_na = TRUE` to include them.

``` r
set.seed(1999)
dat_survey1 <- data.frame(
  participant_id = seq_len(60),
  country = rep(c("England", "Wales"), each = 30),
  region = rep(
    c("East England", "West England", NA, "North Wales", "South Wales", NA),
    each = 10
  ),
  q1_catq = sample(c("A", "B", "C", NA), 60, replace = TRUE),
  q2_catq = sample(c("A", "B", "C", "D", "E", NA), 60, replace = TRUE)
)

dat_survey2 <- data.frame(
  auditYear = c(rep(2021, 3), rep(2022, 3), rep(2023, 6)),
  sex = c(
    "Female", "Female", "Male", "Female", "Male", "Male",
    "Female", "Male", "Male", "Male", NA, NA
  ),
  type = c("A", "B", "B", "A", "B", NA,
           sample(c("A", "B", NA), 6, replace = TRUE))
)
```

#### Separate Group Summary

With `nested = FALSE` (the default), each grouping variable is
summarised separately. The special group name `"overall"` adds an
overall summary.

``` r
sum_freq_eg1 <- get_frequency(
  data = dat_survey1,
  measures = c("q1_catq", "q2_catq"),
  groups = c("overall", "country", "region")
)

sum_freq_eg1 |>
  dplyr::filter(category == "A")
#> # A tibble: 14 × 8
#>    overall country region       measure category numerator denominator percent
#>    <chr>   <chr>   <chr>        <chr>   <chr>        <dbl>       <dbl>   <dbl>
#>  1 overall <NA>    <NA>         q1_catq A               10          41  0.244 
#>  2 overall <NA>    <NA>         q2_catq A                9          54  0.167 
#>  3 <NA>    England <NA>         q1_catq A                5          21  0.238 
#>  4 <NA>    England <NA>         q2_catq A                1          28  0.0357
#>  5 <NA>    Wales   <NA>         q1_catq A                5          20  0.25  
#>  6 <NA>    Wales   <NA>         q2_catq A                8          26  0.308 
#>  7 <NA>    <NA>    East England q1_catq A                2           7  0.286 
#>  8 <NA>    <NA>    East England q2_catq A                0           9  0     
#>  9 <NA>    <NA>    North Wales  q1_catq A                1           7  0.143 
#> 10 <NA>    <NA>    North Wales  q2_catq A                4          10  0.4   
#> 11 <NA>    <NA>    South Wales  q1_catq A                3           9  0.333 
#> 12 <NA>    <NA>    South Wales  q2_catq A                2           9  0.222 
#> 13 <NA>    <NA>    West England q1_catq A                1           7  0.143 
#> 14 <NA>    <NA>    West England q2_catq A                1          10  0.1
```

#### Nested Group Summary

Use `nested = TRUE` to summarise combinations of grouping variables,
such as sex within each audit year:

``` r
get_frequency(
  data = dat_survey2,
  measures = "type",
  groups = c("auditYear", "sex"),
  nested = TRUE
)
#> # A tibble: 12 × 7
#>    auditYear sex    measure category numerator denominator percent
#>        <dbl> <chr>  <chr>   <chr>        <dbl>       <dbl>   <dbl>
#>  1      2021 Female type    A                1           2     0.5
#>  2      2021 Female type    B                1           2     0.5
#>  3      2021 Male   type    A                0           1     0  
#>  4      2021 Male   type    B                1           1     1  
#>  5      2022 Female type    A                1           1     1  
#>  6      2022 Female type    B                0           1     0  
#>  7      2022 Male   type    A                0           1     0  
#>  8      2022 Male   type    B                1           1     1  
#>  9      2023 Female type    A                0           1     0  
#> 10      2023 Female type    B                1           1     1  
#> 11      2023 Male   type    A                0           2     0  
#> 12      2023 Male   type    B                2           2     1
```

#### Count NAs

Use `count_na = TRUE` to include missing values in the denominator and
report them as a separate `NA` category (only for measures that contain
missing values).

``` r
sum_freq_eg3 <- get_frequency(
  data = dat_survey1,
  measures = "q1_catq",
  groups = c("country", "region"),
  nested = TRUE,
  count_na = TRUE
)

sum_freq_eg3 |>
  dplyr::filter(region %in% c("East England", "North Wales"))
#> # A tibble: 8 × 7
#>   country region       measure category numerator denominator percent
#>   <chr>   <chr>        <chr>   <chr>        <int>       <int>   <dbl>
#> 1 England East England q1_catq A                2          10     0.2
#> 2 England East England q1_catq B                4          10     0.4
#> 3 England East England q1_catq C                1          10     0.1
#> 4 England East England q1_catq <NA>             3          10     0.3
#> 5 Wales   North Wales  q1_catq A                1          10     0.1
#> 6 Wales   North Wales  q1_catq B                2          10     0.2
#> 7 Wales   North Wales  q1_catq C                4          10     0.4
#> 8 Wales   North Wales  q1_catq <NA>             3          10     0.3
```

#### Static Plot with `ggplot2`

``` r
library(ggplot2)

sum_freq_eg1 |>
  dplyr::filter(!is.na(country)) |>
  ggplot(aes(x = country, y = percent, fill = category)) +
  geom_col() +
  facet_wrap(~ measure, nrow = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Country", y = "Percent", fill = "Category")
```

#### Interactive Plot with `plotly`

``` r
library(plotly)

sum_freq_eg1 |>
  dplyr::filter(!is.na(country)) |>
  dplyr::group_by(measure) |>
  dplyr::group_map(\(df, ...) {
    plot_ly(df, x = ~country, y = ~percent, 
            color = ~category, type = "bar")
  }) |>
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
```

### Audit Periods

The default audit year starts in April. With no date supplied,
`audit_year()` uses today’s date:

``` r
audit_year()
#> [1] "2026/27 Q3"
```

Fixed dates keep the rendered examples reproducible:

``` r
audit_year("2025-03-31")
#> [1] "2024/25 Q4"
audit_year("2025-03-31", format = FALSE)
#> [1] 2024
```

Change the first-quarter start month for a different audit calendar:

``` r
audit_year("2025-03-31", start_month = 1)
#> [1] "2025/26 Q1"
```

Generate an inclusive sequence of audit years:

``` r
data.frame(
  audit_year = audit_year_range(2010, 2015),
  audit_year_numeric = audit_year_range(2010, 2015, format = FALSE)
)
#>   audit_year audit_year_numeric
#> 1    2010/11               2010
#> 2    2011/12               2011
#> 3    2012/13               2012
#> 4    2013/14               2013
#> 5    2014/15               2014
#> 6    2015/16               2015
```

### Correlation Matrix with `plotly`

`plot_corr_matrix()` creates an interactive lower-triangle correlation
heatmap. Supply a data frame containing only numeric variables:

``` r
plot_corr_matrix(mtcars)
```

Customise the plot, for example by selecting Spearman correlations,
hiding the values and customising the colour bar:

``` r
plot_corr_matrix(
  mtcars,
  method = "spearman",
  show_values = FALSE,
  show_stars = TRUE,
  colorbar = list(title = "Spearman Corr")
) |>
  plotly::layout(
    margin = list(b = 80),
    annotations = list(list(
      text = paste0("<i><b>Note:</b> *: p<0.05, **: p<0.01, ***: p<0.001.</i>"),
      x = 1, y = -0.15, xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "top",
      showarrow = FALSE
    ))
  )
```

## License

This package is licensed under the [GNU General Public License v3.0 or
later](LICENSE.md).
