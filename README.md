<!-- badges: start -->
[![R Package](https://img.shields.io/badge/R-package-blue)](https://github.com/rcpch/npdar)
[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](http://perso.crans.org/besson/LICENSE.html)
<!-- badges: end -->

## Overview

**npdar** is an R package that provides utility functions to support the 
[National Paediatric Diabetes Audit (NPDA)](https://www.rcpch.ac.uk/work-we-do/clinical-audits/npda) programme,
developed and maintained by the [Royal College of Paediatrics and Child Health (RCPCH)](https://www.rcpch.ac.uk/).

The package currently includes functions for:

-   **Blood pressure assessment** — calculating expected BP values, z-scores, 
    percentiles, and categorising hypertension for children and adults based on
    appropriate references or guidelines.
-   **Finding audit year(s)** — determining the NPDA audit period based on the
    provided date.
-   **Data privacy suppression** — masking small numerators in a categorical
    variable to protect patient privacy.
-   **Statistical utilities** — identifying the most recent/first/last modal
    value or the first/last entry from a vector with options to ignore specific
    values.

## ⚠️Disclaimer

**This package is designed for:**

-   Assisting NPDA data analysis.
-   Improving transparency and reproducibility of NPDA analysis.
-   Helping other research and epidemiological analysis.

**This package is NOT:**

-   A clinical decision support or diagnosis tool.
-   Approved for use in clinical care pathways.

Healthcare professionals must use validated clinical tools and their
professional judgement when making patient care decisions. The blood pressure
categorisation functions implement published research guidelines (NHBPEP Fourth
Report, NICE/BHF guidances) 
**for audit analysis purposes only, not for clinical use**.

--------------------------------------------------------------------------------

## Installation

**npdar** is not currently available on CRAN. Install the development version
from GitHub with [`devtools`](https://devtools.r-lib.org/):

``` r
install.packages("devtools")
devtools::install_github("rcpch/npdar")
```

Or with [`remotes`](https://remotes.r-lib.org/):

``` r
install.packages("remotes")
remotes::install_github("rcpch/npdar")
```

Then load the package:

``` r
library(npdar)
```

--------------------------------------------------------------------------------

## Quick Start

``` r
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
#> Ignore 1 age(s) outside 0-17 years. NHBPEP Fourth Report is only designed for children ≤ 17 years.
#> NHBPEP Fourth Report categorise 'Normotension' to 'Stage 2 hypertension'; Special rules for newborns (<1), children (0-12), adolescents (12-17).
#> BP outside National Paediatric Diabetes Audit (NPDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.
#> [1] "Normotension"    "Prehypertension" NA


# --- Privacy masking ---

# Mask numerator counts below 3 in audit outputs
mask_numerators(c(4, 6, 2, 4, 0))
#> [1] "masked" "6"      "masked" "masked" "0"
mask_numerators(c(4, 6, 2, 4, 0), maxNum = 3, maskMessage = "*")
#> [1] "*" "6" "*" "*" "0"


# --- Longitudinal summarisation ---

# Find the desired mode
x <- c("A", "B", "C", "A", "A", "Unknown", "Unknown", NA, "C", "C", "B", "B", "Unknown")

# The most recent mode
get_ultimate(x, find = "uptodate_mode", except = c("Unknown", NA)) 
#> Warning in get_ultimate(x, find = "uptodate_mode", except = c("Unknown", NA)) :
#> There are multiple modes, the remaining modes are: A, C
#> [1] "B"

# The last mode
get_ultimate(x, find = "last_mode", except = c("Unknown"))
#> Warning in get_ultimate(x, find = "last_mode", except = except = c("Unknown")) :
#> There are multiple modes, the remaining modes are: A, B
#> [1] "C"


# --- Finding audit year(s) ---
get_AuditYear(as.Date("2025-03-31"))  
#> [1] "2024/25"

data.frame(
  audit_year = get_AuditYears(2010, 2015),
  audit_year_numeric = get_AuditYears(2010, 2015, format = FALSE)
)
#>   audit_year audit_year_numeric
#> 1    2010/11               2010
#> 2    2011/12               2011
#> 3    2012/13               2012
#> 4    2013/14               2013
#> 5    2014/15               2014
#> 6    2015/16               2015
```

--------------------------------------------------------------------------------

## Planned Improvements

### get_ultimate()

-   [ ] Allow vector with POSIXct Date Time values

### mask_numerators()

-   [ ] for rows with (2, 3, 3, 3, 3) with threshold being \<3, all values are
    currently masked.
-   [ ] instead, new rules can be implemented in the future:
    1)  randomly mask only one of the second smallest value if specify
        set.seed(),
    2)  can give weights to different options, so a certain option may be more
        likely to be masked.

### BP functions

-   [ ] Review unit testing.
-   [ ] Review non-ASCII characters.
-   [ ] Add new references apart from Fourth Report and NICE/BHF.

### Audit Year functions

-   [ ] Review unit testing.
-   [ ] Option to determine current audit quarter (Q1-Q4).
-   [ ] Return quarter period boundaries (start/end timestamps).

### AOB

-   [ ] Finalise and add Disclaimer to package documentation.

-   [ ] Better documentation and reference.

--------------------------------------------------------------------------------

## License

This package is licensed under the [GNU General Public License v3.0](LICENSE.md).

--------------------------------------------------------------------------------

## Authors

-   **Zhaonan Fang** —
    [Zhaonan.Fang@rcpch.ac.uk](mailto:Zhaonan.Fang@rcpch.ac.uk) — Author & Maintainer\
    *Royal College of Paediatrics and Child Health (RCPCH)*
-   **Amani Krayem** —
    [Amani.Krayem@rcpch.ac.uk}](mailto:Amani.Krayem@rcpch.ac.uk) — Contributor\
    *Royal College of Paediatrics and Child Health (RCPCH)*
-   **Humfrey Legge** —
    [Humfrey.Legge@rcpch.ac.uk}](mailto:Humfrey.Legge@rcpch.ac.uk) — Contributor\
    *Royal College of Paediatrics and Child Health (RCPCH)*
-   **Saira Pons Perez** —
    [Saira.PonsPerez@rcpch.ac.uk}](mailto:Saira.PonsPerez@rcpch.ac.uk) — Contributor\
    *Royal College of Paediatrics and Child Health (RCPCH)*

