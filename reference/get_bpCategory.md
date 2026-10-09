# Categorise Blood Pressure (Children/Adolescents and Young Adults)

Categorises observed BP values into guideline-based categories. For ages
0-17, currently NHBPEP Fourth Report thresholds are applied (with
newborn and adolescent rules). For \>17, NICE/BHF adult thresholds are
applied.

Unless `bp_limit` is explicitly provided, implausible values are
excluded using:

- NPDA limit (children/adolescents): SBP \[50-200\] mmHg, DBP \[15-150\]
  mmHg

- NDA limit (young adults): SBP \[70-300\] mmHg, DBP \[20-150\] mmHg

## Usage

``` r
get_bpCategory(bp_value, bp_limit = c(-Inf, Inf), ...)
```

## Arguments

- bp_value:

  Numeric vector of observed BP values (mmHg).

- bp_limit:

  Length-2 numeric vector `c(min, max)` giving acceptable range.
  Defaults to `c(-Inf, Inf)`; if left as-is, NPDA/NDA defaults are
  applied based on `ref`.

- ...:

  Arguments passed on to
  [`.valid_bpDemoInput`](https://rcpch.github.io/npdar/reference/dot-valid_bpDemoInput.md),
  [`get_bpExpected`](https://rcpch.github.io/npdar/reference/get_bpExpected.md)

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

## Value

Character vector of BP categories: *"Normotension"*,
*"Prehypertension"*, *"Stage 1 hypertension"*, *"Stage 2 hypertension"*,
or *"Stage 3 hypertension"* (adults only). `NA` where inputs are invalid
or beyond specified `bp_limit`.

## BP Categories by Age Group

|  |  |  |  |
|----|----|----|----|
| **Category** | **Age (years)** | **Reference** | **Criteria** |
| Normotension | \[0-12\] (children) | NHBPEP Fourth Report (NPDA min) | (50/15mmHg \\\leq\\) SBP/DBP \< 90th percentile |
| Normotension | (12-17\] (adolescents) | NHBPEP Fourth Report (NPDA min) | (50/15mmHg \\\leq\\) SBP/DBP \< 120/80 mmHg |
| Normotension | \>17 | NICE/BHF (NDA min) | (70/20mmHg \\\leq\\) SBP/DBP \< 120/80 mmHg |
| Prehypertension | \[0-12\] (children) | NHBPEP Fourth Report | 90th \\\leq\\ SBP/DBP \< 95th |
| Prehypertension | (12-17\] (adolescents) | NHBPEP Fourth Report | 120/80mmHg \\\leq\\ SBP/DBP \< 95th |
| Prehypertension | \>17 | NICE/BHF | 120/80mmHg \\\leq\\ SBP/DBP \< 140/90mmHg |
| Stage 1 hypertension | \[0-1) (newborns) | NHBPEP Fourth Report | 95th \\\leq\\ SBP \< 5mmg above 99th |
| Stage 1 hypertension | \[1-17\] | NHBPEP Fourth Report | 95th \\\leq\\ SBP/DBP \< 5mmg above 99th |
| Stage 1 hypertension | \>17 | NICE/BHF | 140/90mmHg \\\leq\\ SBP/DBP \< 160/100mmHg |
| Stage 2 hypertension | \[0-1) (newborns) | NHBPEP Fourth Report (NPDA max) | 5mmg above 99th \\\leq\\ SBP (\\\leq\\ 200/150mmHg) |
| Stage 2 hypertension | \[1-17\] | NHBPEP Fourth Report (NPDA max) | 5mmg above 99th \\\leq\\ SBP/DBP (\\\leq\\ 200/150mmHg) |
| Stage 2 hypertension | \>17 | NICE/BHF | 160/100mmHg \\\leq\\ SBP/DBP \< 180/120mmHg |
| Stage 3 hypertension | \>17 | NICE/BHF (NDA max) | 180/120mmHg \\\leq\\ SBP/DBP (\\\leq\\ 300/150mmHg) |

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
[`get_bpExpected()`](https://rcpch.github.io/npdar/reference/get_bpExpected.md),
[`get_bpRelative()`](https://rcpch.github.io/npdar/reference/get_bpRelative.md)

## Examples

``` r
# Child/adolescent categorisation (Fourth Report):
get_bpCategory(
  bp_value = c(95, 118, 130, 150),
  bp_type = "systolic",
  sex = c(1, 2, 1, 2),
  male_code = 1, female_code = 2,
  height_z = c(0, 0, 0, 0),
  age_years = c(8, 14, 16, 6),
  ref = "Fourth Report"
)
#> NHBPEP Fourth Report categorise 'Normotension' to 'Stage 2 hypertension'; Special rules for newborns (<1), children (0-12), adolescents (12-17).
#> BP outside National Paediatric Diabetes Audit (NPDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.
#> [1] "Normotension"         "Normotension"         "Prehypertension"     
#> [4] "Stage 2 hypertension"

# Adult categorisation (NICE/BHF; uses NDA limits if bp_limit not provided):
get_bpCategory(
  bp_value = c(118, 135, 162, 185),
  bp_type = "diastolic",
  sex = "M", male_code = "M", female_code = "F",
  height_z = 0,
  age_years = c(22, 30, 45, 67),
  ref = "NICE/BHF"
)
#> NICE/BHF categorise 'Normotension' to 'Stage 3 hypertension'.
#> BP outside National Diabetes Audit (NDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.
#> [1] "Stage 2 hypertension" "Stage 3 hypertension" NA                    
#> [4] NA                    
```
