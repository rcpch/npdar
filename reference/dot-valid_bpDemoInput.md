# Validate Demographic Inputs for BP Functions (Internal)

Internal helper function to validate and standardize demographic inputs
for blood pressure calculations. Checks age ranges, sex coding, and
height z-scores against specified limits.

## Usage

``` r
.valid_bpDemoInput(
  bp_type = c("systolic", "diastolic"),
  sex,
  male_code,
  female_code,
  height_z,
  height_limit = 5,
  age_years,
  ref = c("Fourth Report", "NICE/BHF"),
  .quiet = FALSE
)
```

## Arguments

- bp_type:

  Character; `"systolic"` or `"diastolic"`.

- sex:

  Vector of sex codes in your data. Will be mapped to `"male"` or
  `"female"` using `male_code` and `female_code`. Values not matching
  these codes become `NA`.

- male_code, female_code:

  Scalar values that identify the male and female codes in `sex` (e.g.,
  `1` and `2`, or `"M"` and `"F"`).

- height_z:

  Numeric vector of height z-scores. Must already be z-transformed
  (e.g., US CDC, UK-WHO).

- height_limit:

  Positive numeric; z-scores with `|z| > height_limit` are treated as
  invalid and set to `NA`. Default `5`.

- age_years:

  Numeric vector of ages in years.

- ref:

  Reference: `"Fourth Report"` (children/adolescents, 0-17) or
  `"NICE/BHF"` (adults, \>17).

- .quiet:

  Logical; suppress validation messages (default `FALSE`).

## Value

A list with normalized elements: `bp_type`, `sex` (`"male"` or
`"female"` or `NA`), `age_years` (invalid ages set to `NA` given `ref`),
`height_z` (out-of-bound z set to `NA`), and `ref`.

## See also

Other BP functions:
[`family_bp`](https://rcpch.github.io/npdar/reference/family_bp.md),
[`get_bp()`](https://rcpch.github.io/npdar/reference/get_bp.md),
[`get_bpCategory()`](https://rcpch.github.io/npdar/reference/get_bpCategory.md),
[`get_bpExpected()`](https://rcpch.github.io/npdar/reference/get_bpExpected.md),
[`get_bpRelative()`](https://rcpch.github.io/npdar/reference/get_bpRelative.md)
