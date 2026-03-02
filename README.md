<!-- badges: start -->
[![R Package](https://img.shields.io/badge/R-package-blue)](https://github.com/rcpch/npdar)
[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](http://perso.crans.org/besson/LICENSE.html)
[![Version](https://img.shields.io/badge/version-0.2.0-green)](https://github.com/rcpch/npdar)
<!-- badges: end -->

## Overview

**npdar** is an R package that provides utility functions to support the [National Paediatric Diabetes Audit (NPDA)](https://www.rcpch.ac.uk/work-we-do/clinical-audits/npda) programme, developed and maintained by the [Royal College of Paediatrics and Child Health (RCPCH)](https://www.rcpch.ac.uk/).

The package currently includes functions for:

- **Blood pressure assessment** — age-appropriate BP categorisation for children/adolescents (NHBPEP Fourth Report) and adults (NICE/BHF guidelines), including expected values, z-scores, percentiles, and hypertension staging.
- **Finding audit year(s)** - determining the NPDA audt period based on the provided date.
- **Data privacy suppression** — masking small numerators in a categorical variable to protect patient privacy.
- **Statistical Utilities** — identifying the most recent/first/last modal value or the first/last entry from a vector with options to ignore specific values.

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
  ref        = c("Fourth Report")
)
#> [1] "Normotension"    "Prehypertension" NA 

# --- Privacy masking ---

# Mask numerator counts below 3 in audit outputs
mask_numerators(c(4, 6, 2, 4, 0))
#> [1] "masked" "6"      "masked" "masked" "0"

# --- Longitudinal summarisation ---

# Find the desired mode
x <- c("A", "B", "C", "A", "A", "Unknown", "Unknown", NA, "C", "C", "B", "B", "Unknown")
# The most recent/up-to-date mode
get_ultimate(x, find = "uptodate_mode", except = c("Unknown", NA)) # Returns "B"
# The last mode
get_ultimate(x, find = "last_mode", except = c("Unknown", NA)) # Returns "C"

# --- Finding audit year(s) ---
get_AuditYear(as.Date("2025-03-31"))  # Returns "2024/25" (Q4 end)
data.frame(
  audit_year = get_AuditYears(2010, 2015),
  audit_year_numeric = get_AuditYears(2010, 2015, format = FALSE)
)
#   audit_year audit_year_numeric
# 1    2010/11               2010
# 2    2011/12               2011
# 3    2012/13               2012
# 4    2013/14               2013
# 5    2014/15               2014
# 6    2015/16               2015

```

---

## Planned Improvements

### get_ultimate()

- [ ] Allow vector with POSIXct Date Time values

### mask_numerators()

- [ ] for rows with (2, 3, 3, 3, 3) with threshold being <3, all values are currently masked.
- [ ] instead, new rules can be implemented in the future:
  1) randomly mask only one of the second smallest value if specify set.seed(),
  2) can give weights to different options, so a certain option may be more likely to be masked


### BP functions

- [ ] Review unit testing.
- [ ] Add new references apart from Fourth Report and NICE/BHF.
- [ ] Check non-ASCII characters.

### Audit Year functions

- [ ] Review unit testing.
- [ ] Option to determine current audit quarter (Q1-Q4)
- [ ] Return quarter period boundaries (start/end timestamps)

### AOB

- [ ] Better documentation and reference.


---

## License

This package is licensed under the [GNU General Public License v3.0](LICENSE.md).

---

## Authors

- **Zhaonan Fang** — [zhaonan.fang@rcpch.ac.uk](mailto:zhaonan.fang@rcpch.ac.uk) — Author & Maintainer  
  *Royal College of Paediatrics and Child Health (RCPCH)*
- **RCPCH Analysts** 
  *Royal College of Paediatrics and Child Health (RCPCH)*
