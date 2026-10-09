# Compute Expected Blood Pressure (\\\mu\\)

Returns the expected blood pressure (mean/50th percentile) based on CYP
sex, age, and height z-score. Currently, only NHBPEP Fourth Report
regression models are available for use for children and adolescents
(0-17 years). For `ref = "NICE/BHF"` or people \>17 years old, expected
BP is not defined and `NA` is returned.

## Usage

``` r
get_bpExpected(..., .quiet = FALSE)
```

## Arguments

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

A numeric vector of expected BP values in mmHg (\\\mu\\). Returns `NA`
for:

- NICE/BHF reference (adults)

- Ages outside reference range

- Missing or invalid sex

- Extreme height z-scores

## Height Reference Standards for the Fourth Report

**Fourth Report Regression Model:**

The Fourth Report uses regression models with age (centred at 10 years)
and height z-score polynomial terms (up to 4th degree) to calculate
expected BP:

\$\$\mu = \alpha + \sum\_{j=1}^{4} \beta_j (Age-10)^j + \sum\_{k=1}^{4}
\gamma_k (Z\_{ht})^k\$\$

**BMI References:**

The Fourth Report was developed and validated using US CDC 2000 growth
references for height percentiles. Your height z-scores should ideally
be based on US CDC reference. However, UK clinical practice and NPDA
audit analysis use UK-WHO growth charts as standard (historically the
NPDA used the British 1990), so using the UK-WHO is also considered
acceptable in UK contexts (based on the assumption that the two
populations are similar enough).

To our knowledge, the Fourth Report BP percentiles have not been
validated using UK-WHO height references. The clinical impact of
applying Fourth Report regression coefficients with UK-WHO (rather than
US CDC) height z-scores has not been formally evaluated. Users should be
aware of this methodological discrepancy when interpreting results or
comparing to US-based implementations.

**Calculating Height Z-Scores:**

This package requires height z-scores as input and does not calculate
them internally. Two approaches I recommended for obtaining height
z-scores are:

1.  **childsds R package** (<https://mvogel78.r-universe.dev/childsds>),
    which provides SDS/z-score calculations based on multiple growth
    standards (including UK–WHO, US CDC references).

2.  **RCPCHGrowth Python library**
    (<https://growth.rcpch.ac.uk/products/python-library/>), which is
    the official RCPCH implementation of UK-WHO growth charts, with API
    accessible from R.

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
[`get_bpRelative()`](https://rcpch.github.io/npdar/reference/get_bpRelative.md)

## Examples

``` r
# Expected systolic BP for a 10-year-old boy at median height, and other some other children:
get_bpExpected(
  bp_type = "systolic",
  sex = c(1, 2, 1),
  male_code = 1, female_code = 2,
  height_z = c(0, 0, 1.5),
  age_years = c(10, 12, 8),
  ref = "Fourth Report"
)
#> [1] 102.1977 105.8496 102.5655

# Adults: returns NA with an informative message
get_bpExpected(
  bp_type = "diastolic",
  sex = "M", male_code = "M", female_code = "F",
  height_z = 0, age_years = 25, ref = "NICE/BHF"
)
#> NICE/BHF only provides category guidelines, expected BP not calculated.
#> [1] NA
```
