# Compute Blood Pressure Z-score and Percentile

Computes z-scores and percentiles for observed BP values against
expected values (\\\mu, \sigma\\) from a specified reference (currently
only NHBPEP Fourth Report is available). For `ref = "NICE/BHF"`,
z-scores are not defined and `NA` is returned with a message.

## Usage

``` r
get_bpRelative(bp_value, ..., .quiet = FALSE)
```

## Arguments

- bp_value:

  Numeric vector of observed BP values (mmHg).

- ...:

  Arguments passed on to
  [`.valid_bpDemoInput`](https://rcpch.github.io/npdar/reference/dot-valid_bpDemoInput.md)

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

  Logical; suppress validation messages (default `FALSE`).

## Value

A tibble with two columns: `zscore` and `percentile` (0-100). `NA` where
expected BP could not be computed (e.g., invalid inputs or adult
reference).

## References

- NHBPEP Fourth Report: [Paediatric BP
  categories](https://www.nhlbi.nih.gov/files/docs/resources/heart/hbp_ped.pdf)

- NICE Guidelines: [Adult BP
  categories](https://cks.nice.org.uk/topics/hypertension/)

- British Heart Foundation: [Adult BP
  categories](https://www.bhf.org.uk/informationsupport/risk-factors/high-blood-pressure)

- NDA Methodology: [Adult BP
  Range](https://digital.nhs.uk/data-and-information/publications/statistical/national-diabetes-audit/core-report-1-2020-21/methodology)

- NPDA Methodology: [Paediatric BP
  Range](https://www.rcpch.ac.uk/work-we-do/clinical-audits/npda/transparency-open-data)

- RCPCH Growth API: [Python library for Paediatric SDS
  calculation](https://growth.rcpch.ac.uk/)

- childsds package: [R package for Paediatric SDS
  calculation](https://mvogel78.r-universe.dev/childsds)

## See also

Other BP functions:
[`.valid_bpDemoInput()`](https://rcpch.github.io/npdar/reference/dot-valid_bpDemoInput.md),
[`family_bp`](https://rcpch.github.io/npdar/reference/family_bp.md),
[`get_bp()`](https://rcpch.github.io/npdar/reference/get_bp.md),
[`get_bpCategory()`](https://rcpch.github.io/npdar/reference/get_bpCategory.md),
[`get_bpExpected()`](https://rcpch.github.io/npdar/reference/get_bpExpected.md)

## Examples

``` r
# Z-score and percentile for systolic BP in children using Fourth Report:
get_bpRelative(
  bp_value = c(95, 110, 130),
  bp_type = "systolic",
  sex = c("M","F","M"),
  male_code = "M", female_code = "F",
  height_z = c(0, 0.5, -1),
  age_years = c(8, 12, 16),
  ref = "Fourth Report"
)
#> # A tibble: 3 × 2
#>   zscore percentile
#>    <dbl>      <dbl>
#> 1 -0.375       35.4
#> 2  0.298       61.7
#> 3  1.53        93.6
```
