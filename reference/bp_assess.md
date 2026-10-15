# Master Wrapper for Blood Pressure Utilities

Runs
[`bp_expected`](https://rcpch.github.io/npdar/reference/bp_expected.md),
[`bp_relative`](https://rcpch.github.io/npdar/reference/bp_relative.md)
and
[`bp_category`](https://rcpch.github.io/npdar/reference/bp_category.md)
in one call. Inputs are validated once and the expected BP is computed
once, so messages are printed once and the results are consistent with
each other.

## Usage

``` r
bp_assess(bp_value, bp_limit = c(-Inf, Inf), ..., .quiet = FALSE)
```

## Arguments

- bp_value:

  Numeric vector of observed BP values (mmHg).

- bp_limit:

  Length-2 numeric vector `c(min, max)` giving acceptable range used for
  categorisation. See
  [`bp_category`](https://rcpch.github.io/npdar/reference/bp_category.md).

- ...:

  Arguments passed on to
  [`.bp_validate_demo_input`](https://rcpch.github.io/npdar/reference/dot-bp_validate_demo_input.md)

  `bp_type`

  :   Character; `"systolic"` or `"diastolic"`.

  `sex`

  :   Vector of sex codes in your data. Will be mapped to `"male"` or
      `"female"` using `male_code` and `female_code`. Values not
      matching these codes become `NA`.

  `male_code,female_code`

  :   Scalar values that identify the male and female codes in `sex`
      (e.g., `1` and `2`, or `"M"` and `"F"`).

  `height_z`

  :   Numeric vector of height z-scores. Must already be z-transformed
      (e.g., US CDC, UK-WHO).

  `height_limit`

  :   Positive numeric; z-scores with `|z| > height_limit` are treated
      as invalid and set to `NA`. Default `5`.

  `age_years`

  :   Numeric vector of ages in years.

  `ref`

  :   Reference: `"Fourth Report"` (children/adolescents, 0-17) or
      `"NICE/BHF"` (adults, \>17).

- .quiet:

  Logical; suppress messages (default `FALSE`).

## Value

A tibble with one row per input and columns `expected` (\\\mu\\, mmHg),
`zscore`, `percentile` (0-100) and `category`. Values that cannot be
computed are `NA`; for `ref = "NICE/BHF"` only `category` is defined.

## See also

Other BP functions:
[`.bp_message()`](https://rcpch.github.io/npdar/reference/dot-bp_message.md),
[`.bp_resolve_limit()`](https://rcpch.github.io/npdar/reference/dot-bp_resolve_limit.md),
[`.bp_validate_demo_input()`](https://rcpch.github.io/npdar/reference/dot-bp_validate_demo_input.md),
[`bp_category()`](https://rcpch.github.io/npdar/reference/bp_category.md),
[`bp_expected()`](https://rcpch.github.io/npdar/reference/bp_expected.md),
[`bp_family`](https://rcpch.github.io/npdar/reference/bp_family.md),
[`bp_relative()`](https://rcpch.github.io/npdar/reference/bp_relative.md)

## Examples

``` r
# One-stop interface:
bp_assess(
  bp_value = c(115, 142),
  bp_type = "systolic",
  sex = c("M","F"), male_code = "M", female_code = "F",
  height_z = c(0, 0.2),
  age_years = c(13, 16),
  ref = "Fourth Report"
)
#> NHBPEP Fourth Report categorise 'Normotension' to 'Stage 2 hypertension'; Special rules for newborns (<1), children (0-12), adolescents (12-17).
#> BP outside National Paediatric Diabetes Audit (NPDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.
#> # A tibble: 2 × 4
#>   expected zscore percentile category            
#>      <dbl>  <dbl>      <dbl> <chr>               
#> 1     109.  0.581       71.9 Normotension        
#> 2     112.  2.88        99.8 Stage 2 hypertension
```
