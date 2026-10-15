# Validate Demographic Inputs for BP Functions (Internal)

Internal helper function to validate and standardize demographic inputs
for blood pressure calculations. Checks age ranges, sex coding, and
height z-scores against specified limits.

## Usage

``` r
.bp_validate_demo_input(
  bp_type = c("systolic", "diastolic"),
  sex,
  male_code,
  female_code,
  height_z,
  height_limit = 5,
  age_years,
  ref = c("Fourth Report", "NICE/BHF"),
  bp_value = NULL,
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

- bp_value:

  Optional numeric vector of observed BP (mmHg); only used here to check
  that its length matches the other inputs.

- .quiet:

  Logical; suppress validation messages (default `FALSE`).

## Value

A list with normalized elements: `bp_type`, `sex` (`"male"` or
`"female"` or `NA`), `age_years` (invalid ages set to `NA` given `ref`),
`height_z` (out-of-bound z set to `NA`), `ref`, and `bp_value` (`NULL`
if not supplied). It is an error if `sex`, `age_years`, `height_z` and
`bp_value` (when supplied) differ in length.

## See also

Other BP functions:
[`.bp_message()`](https://rcpch.github.io/npdar/reference/dot-bp_message.md),
[`.bp_resolve_limit()`](https://rcpch.github.io/npdar/reference/dot-bp_resolve_limit.md),
[`bp_assess()`](https://rcpch.github.io/npdar/reference/bp_assess.md),
[`bp_category()`](https://rcpch.github.io/npdar/reference/bp_category.md),
[`bp_expected()`](https://rcpch.github.io/npdar/reference/bp_expected.md),
[`bp_family`](https://rcpch.github.io/npdar/reference/bp_family.md),
[`bp_relative()`](https://rcpch.github.io/npdar/reference/bp_relative.md)
