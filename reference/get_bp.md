# Master Wrapper for Blood Pressure Utilities

Convenience wrapper to access expected values, z-scores, percentiles, or
guideline categories with a single function call.

## Usage

``` r
get_bp(option = c("expected", "zscore", "percentile", "category"), ...)
```

## Arguments

- option:

  Character; one of `"expected"`, `"zscore"`, `"percentile"`,
  `"category"` (partial matching enabled).

- ...:

  Arguments passed on to
  [`.valid_bpDemoInput`](https://rcpch.github.io/npdar/reference/dot-valid_bpDemoInput.md),
  [`get_bpExpected`](https://rcpch.github.io/npdar/reference/get_bpExpected.md),
  [`get_bpRelative`](https://rcpch.github.io/npdar/reference/get_bpRelative.md),
  [`get_bpCategory`](https://rcpch.github.io/npdar/reference/get_bpCategory.md)

  `.quiet`

  :   Logical; suppress validation messages (default `FALSE`).

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

  `bp_value`

  :   Numeric vector of observed BP values (mmHg).

  `bp_limit`

  :   Length-2 numeric vector `c(min, max)` giving acceptable range.
      Defaults to `c(-Inf, Inf)`; if left as-is, NPDA/NDA defaults are
      applied based on `ref`.

## Value

Depending on `option`:

- `"expected"`: numeric vector of \\\mu\\

- `"zscore"`: numeric vector of z-scores

- `"percentile"`: numeric vector (0-100)

- `"category"`: character vector of categories

## See also

Other BP functions:
[`.valid_bpDemoInput()`](https://rcpch.github.io/npdar/reference/dot-valid_bpDemoInput.md),
[`family_bp`](https://rcpch.github.io/npdar/reference/family_bp.md),
[`get_bpCategory()`](https://rcpch.github.io/npdar/reference/get_bpCategory.md),
[`get_bpExpected()`](https://rcpch.github.io/npdar/reference/get_bpExpected.md),
[`get_bpRelative()`](https://rcpch.github.io/npdar/reference/get_bpRelative.md)

## Examples

``` r
# One-stop interface:
get_bp(
  option = "category",
  bp_value = c(115, 142),
  bp_type = "systolic",
  sex = c("M","F"), male_code = "M", female_code = "F",
  height_z = c(0, 0.2),
  age_years = c(13, 16),
  ref = "Fourth Report"
)
#> NHBPEP Fourth Report categorise 'Normotension' to 'Stage 2 hypertension'; Special rules for newborns (<1), children (0-12), adolescents (12-17).
#> BP outside National Paediatric Diabetes Audit (NPDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.
#> [1] "Normotension"         "Stage 2 hypertension"
```
