# Blood Pressure Assessment Functions for Paediatric and Adult Populations

This module provides functions to assess blood pressure (BP) in
children, young people, and adults using age-appropriate clinical
guidelines, currently the package uses:

- Children/adolescents (0-17 years): NHBPEP Fourth Report

- Young adults (\>17 years): NICE/BHF guidelines

## Details

The functions implement clinical BP categorisation following:

- **NHBPEP Fourth Report**: Sex-, age-, and height-specific percentiles
  for paediatric BP

- **NICE/BHF Guidelines**: Fixed thresholds for adult hypertension
  stages

- **NPDA Limits**: National Paediatric Diabetes Audit validity ranges
  (50/15-200/150 mmHg)

- **NDA Limits**: National Diabetes Audit validity ranges (70/20-300/150
  mmHg)

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
[`get_bp()`](https://rcpch.github.io/npdar/reference/get_bp.md),
[`get_bpCategory()`](https://rcpch.github.io/npdar/reference/get_bpCategory.md),
[`get_bpExpected()`](https://rcpch.github.io/npdar/reference/get_bpExpected.md),
[`get_bpRelative()`](https://rcpch.github.io/npdar/reference/get_bpRelative.md)
