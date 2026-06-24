<!-- badges: start -->
[![R Package](https://img.shields.io/badge/R-package-blue)](https://github.com/rcpch/npdar)
[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](http://perso.crans.org/besson/LICENSE.html)
[![R-CMD-check](https://github.com/rcpch/npdar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rcpch/npdar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

**npdar** is an R package that provides utility functions to support the 
[National Paediatric Diabetes Audit (NPDA)](https://www.rcpch.ac.uk/work-we-do/clinical-audits/npda) programme,
developed and maintained by the [Royal College of Paediatrics and Child Health (RCPCH)](https://www.rcpch.ac.uk/).

The package currently includes functions for:

-   **Blood pressure assessment** — calculating expected BP values, z-scores, 
    percentiles, and categorising hypertension for children and adults based on
    appropriate references or guidelines.
-   **Statistical utilities** — 1) identifying the most recent/first/last modal
    value or the first/last entry from a vector with options to ignore specific
    values. 2) summarising categorical measure(s) by group(s), returning count, 
    denominator, and percentage in long format for easy use in `ggplot2` or `plotly`.
-   **Data privacy suppression** — masking small numerators in a categorical
    variable to protect patient privacy.
-   **Finding audit year(s)** — determining current audit year & quarter based on 
    the provided date and Q1 start month.

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

library(dplyr)
library(ggplot2)
library(plotly)

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
#> NHBPEP Fourth Report categorise 'Normotension' to 'Stage 2 hypertension'; Special rules for 
#> newborns (<1), children (0-12), adolescents (12-17).
#> BP outside National Paediatric Diabetes Audit (NPDA) limit removed unless acceptable range is 
#> explicitly specified by 'bp_limit'.
#> [1] "Normotension"    "Prehypertension" NA


# --- Privacy masking ---

# Mask numerator counts below 3 in audit outputs
mask_numerators(c(4, 6, 2, 4, 0))
#> [1] "masked" "6"      "masked" "masked" "0"
mask_numerators(c(4, 6, 2, 4, 0), maxNum = 3, maskMessage = "*")
#> [1] "*" "6" "*" "*" "0"


# --- Longitudinal summarisation (first/last/most recent modal values) ---

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

# --- Categorical frequency summarisation ---
# 1. Unnested groups
set.seed(1999)
df <- data.frame(participant_id = 1:60,
                 country        = c(rep("England", 30), rep("Wales", 30)),
                 region         = c(
                   rep("East England", 10), rep("West England", 10), rep(NA, 10),
                   rep("North Wales", 10), rep("South Wales", 10), rep(NA, 10)
                 ),
                 # Categorical Q1
                 q1_catq        = sample(c("A", "B", "C", NA), 60, replace = TRUE),
                 # Categorical Q2 
                 q2_catq        = sample(c("A", "B", "C", "D", "E", NA), 60, replace = TRUE)
)

group_cols <- c("overall", "country", "region")

measure_cols <- df |>
  select(matches("q[0-9]+_catq")) |>
  names()

sum <- get_frequency(
  data     = df,
  measures = measure_cols,
  groups   = group_cols
)

head(sum)
#>   overall country region measure category numerator denominator percent
#> 1 overall NA      NA     q1_catq A               10          41   0.244
#> 2 overall NA      NA     q1_catq B               18          41   0.439
#> 3 overall NA      NA     q1_catq C               13          41   0.317
#> 4 overall NA      NA     q2_catq A                9          54   0.167
#> 5 overall NA      NA     q2_catq B               16          54   0.296
#> 6 overall NA      NA     q2_catq C               10          54   0.185

# plotly
sum |>
  filter(!is.na(country)) |>
  group_by(measure) |>
  do(p=plot_ly(., x = ~country, y = ~percent, color = ~category, type = "bar")) |>
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)

# ggplot
sum |>
  filter(!is.na(country)) |>
  ggplot(aes(x = country, y = percent, fill = category)) +
  geom_col() +
  facet_wrap(~ measure, nrow = 1) +
  labs(x = "Country", y = "Percent", fill = "Category")

# 2. Nested groups
set.seed(1999)
df_nested <- data.frame(
  auditYear = c(rep(2021, 3), rep(2022, 3), rep(2023, 6)),
  sex = c("Female", "Female", "Male",
          "Female", "Male", "Male",
          rep("Female", 1), rep("Male", 3), rep(NA, 2)),
  type = c("A", "B", "B",
           "A", "B", NA,
           sample(c("A", "B", NA), 6, replace = TRUE))
)

sum_nested <- get_frequency(data = df_nested, measures = "type", groups = c("auditYear", "sex"), nested = TRUE)

head(sum_nested)
#>   auditYear sex    measure category numerator denominator percent
#> 1      2021 Female type    A                1           2     0.5
#> 2      2021 Female type    B                1           2     0.5
#> 3      2021 Male   type    A                0           1     0  
#> 4      2021 Male   type    B                1           1     1  
#> 5      2022 Female type    A                1           1     1  
#> 6      2022 Female type    B                0           1     0  

# --- Finding audit period(s) ---
get_AuditYear() # Return current audit year & quarter (based on NPDA default Q1 start month)

get_AuditYear("2025-03-31")  
#> [1] "2024/25 Q4"

get_AuditYear("2025-03-31", format = F)  
#> [1] 2024

get_AuditYear("2025-03-31", start_month = 1)  
#> [1] "2025/26 Q1"

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

## License

This package is licensed under the [GNU General Public License v3.0](LICENSE.md).
