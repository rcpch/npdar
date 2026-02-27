# npdar <img src="https://www.rcpch.ac.uk/sites/default/files/RCPCH%20logo.png" align="right" height="60" alt="RCPCH logo"/>

<!-- badges: start -->
[![R Package](https://img.shields.io/badge/R-package-blue)](https://github.com/rcpch/npdar)
[![License: GPL (>= 3)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%203%29-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Version](https://img.shields.io/badge/version-0.2.0-green)](https://github.com/rcpch/npdar)
<!-- badges: end -->

## Overview

**npdar** is an R package that provides utility functions to support the [National Paediatric Diabetes Audit (NPDA)](https://www.rcpch.ac.uk/work-we-do/clinical-audits/npda) programme, developed and maintained by the [Royal College of Paediatrics and Child Health (RCPCH)](https://www.rcpch.ac.uk/).

The package currently includes tools for:

- **Blood pressure assessment** — age-appropriate BP categorisation for children/adolescents (NHBPEP Fourth Report) and adults (NICE/BHF guidelines), including expected values, z-scores, percentiles, and hypertension staging.
- **Data suppression** — masking small numerator counts in audit outputs to protect patient privacy.
- **Longitudinal data summarisation** — identifying the most up-to-date, first, or last valid value from a patient's record history.

---

## Installation

**npdar** is not currently available on CRAN. Install the development version from GitHub using the [`devtools`](https://devtools.r-lib.org/) or [`remotes`](https://remotes.r-lib.org/) package:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install npdar from GitHub
devtools::install_github("rcpch/npdar")
```

Or using `remotes`:

```r
install.packages("remotes")
remotes::install_github("rcpch/npdar")
```

Then load the package:

```r
library(npdar)
```

---

## Quick Start

```r
library(npdar)

# --- Blood Pressure ---

# Categorise BP for children (Fourth Report) and adults (NICE/BHF)
get_BP(
  option     = "category",
  bp_value   = c(95, 125, 155),
  bp_type    = "systolic",
  sex        = c(1, 2, 1),
  male_code  = 1, female_code = 2,
  height_z   = c(0, 0.5, -0.5),
  age_years  = c(10, 15, 25),
  ref        = "Fourth Report"
)

# --- Privacy masking ---

# Mask numerator counts below 3 in audit outputs
mask_numerators(c(4, 6, 2, 4, 0))
#> [1] "masked" "6"      "masked" "masked" "0"

# --- Longitudinal summarisation ---

# Find the most recently observed (up-to-date) mode, excluding invalid codes
x <- c("Type 1", "Type 1", "Unknown", "Type 2", "Type 2", "Type 2", "Unknown")
get_ultimate(x, find = "uptodate_mode", except = "Unknown")
#> [1] "Type 2"
```

---

## Functions

### Blood Pressure (`bp_functions.R`)

| Function | Description |
|---|---|
| `get_BP()` | Master wrapper — returns expected value, z-score, percentile, or category via the `option` argument. |
| `get_BPExpected()` | Computes expected BP (mean, μ) using sex-, age-, and height-specific regression models (NHBPEP Fourth Report). Returns `NA` for NICE/BHF reference. |
| `get_BPRelative()` | Computes BP z-score and percentile against the NHBPEP Fourth Report reference. Returns a tibble with columns `zscore` and `percentile`. |
| `get_BPCategory()` | Categorises BP into Normotension / Prehypertension / Stage 1–3 hypertension using age-appropriate guidelines. Applies NPDA (children) or NDA (adults) validity limits by default. |

#### BP Category Reference Summary

| Category | Age (years) | Reference | Criteria |
|---|---|---|---|
| Normotension | 0–12 | NHBPEP Fourth Report | SBP/DBP < 90th percentile (≥ NPDA min) |
| Normotension | 12–17 | NHBPEP Fourth Report | SBP < 120 / DBP < 80 mmHg (≥ NPDA min) |
| Normotension | > 17 | NICE/BHF | SBP < 120 / DBP < 80 mmHg (≥ NDA min) |
| Prehypertension | 0–12 | NHBPEP Fourth Report | 90th ≤ SBP/DBP < 95th percentile |
| Prehypertension | 12–17 | NHBPEP Fourth Report | 120/80 mmHg ≤ SBP/DBP < 95th percentile |
| Prehypertension | > 17 | NICE/BHF | 120/80 ≤ SBP/DBP < 140/90 mmHg |
| Stage 1 hypertension | 0–1 (newborns) | NHBPEP Fourth Report | 95th ≤ SBP < 99th + 5 mmHg |
| Stage 1 hypertension | 1–17 | NHBPEP Fourth Report | 95th ≤ SBP/DBP < 99th + 5 mmHg |
| Stage 1 hypertension | > 17 | NICE/BHF | 140/90 ≤ SBP/DBP < 160/100 mmHg |
| Stage 2 hypertension | 0–1 (newborns) | NHBPEP Fourth Report | SBP ≥ 99th + 5 mmHg (≤ NPDA max) |
| Stage 2 hypertension | 1–17 | NHBPEP Fourth Report | SBP/DBP ≥ 99th + 5 mmHg (≤ NPDA max) |
| Stage 2 hypertension | > 17 | NICE/BHF | 160/100 ≤ SBP/DBP < 180/120 mmHg |
| Stage 3 hypertension | > 17 | NICE/BHF | SBP/DBP ≥ 180/120 mmHg (≤ NDA max) |

**NPDA validity limits (children/adolescents):** SBP 50–200 mmHg, DBP 15–150 mmHg  
**NDA validity limits (adults):** SBP 70–300 mmHg, DBP 20–150 mmHg

### Data Suppression (`mask_numerators.R`)

| Function | Description |
|---|---|
| `mask_numerators()` | Masks small numerator counts in a numeric vector. Values in the range `(0, maxNum)` are replaced with a mask message (default `"masked"`). If only one value is masked, all occurrences of the second-smallest value are also masked to prevent back-calculation. Returns a character vector. |

### Longitudinal Summarisation (`get_ultimate.R`)

| Function | Description |
|---|---|
| `get_ultimate()` | Returns a single representative value from a vector, choosing the up-to-date mode (`"uptodate_mode"`), first/last mode (`"first_mode"` / `"last_mode"`), or first/last valid entry (`"first_entry"` / `"last_entry"`). `NA` values are always excluded; additional values can be excluded via the `except` argument. |

---

## Height Z-score Calculation

Several BP functions require height z-scores (`height_z`). Two validated approaches for computing UK–WHO height or BMI z-scores are:

1. **[childsds](https://mvogel78.r-universe.dev/childsds)** — R package providing SDS/z-score calculations based on multiple growth standards.
2. **[RCPCHGrowth](https://growth.rcpch.ac.uk/products/python-library/)** — RCPCH Python library implementing the official UK-WHO growth chart algorithms, with an API accessible from R.

---

## Planned Improvements

See [TODO.md](TODO.md) for a dynamically updated list of planned improvements, including:

- Support for `POSIXct` date-time vectors in `get_ultimate()`
- Advanced secondary masking strategies in `mask_numerators()`
- Additional BP reference sources
- A new `get_audit()` function

---

## References

- **NHBPEP Fourth Report**: [Paediatric BP categories](https://www.nhlbi.nih.gov/files/docs/resources/heart/hbp_ped.pdf)
- **NICE Guidelines**: [Adult BP categories](https://cks.nice.org.uk/topics/hypertension/)
- **British Heart Foundation**: [Adult BP categories](https://www.bhf.org.uk/informationsupport/risk-factors/high-blood-pressure)
- **NDA Methodology**: [Adult BP range](https://digital.nhs.uk/data-and-information/publications/statistical/national-diabetes-audit/core-report-1-2020-21/methodology)
- **NPDA Methodology**: [Paediatric BP range](https://www.rcpch.ac.uk/work-we-do/clinical-audits/npda/transparency-open-data)

---

## License

This package is licensed under the [GNU General Public License v3.0 or later](LICENSE.md).

---

## Authors

- **Zhaonan Fang** — [zhaonan.fang@rcpch.ac.uk](mailto:zhaonan.fang@rcpch.ac.uk) — Author & Maintainer  
  *Royal College of Paediatrics and Child Health (RCPCH)*
