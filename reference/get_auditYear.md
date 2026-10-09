# Get Current Audit Year (& Quarters)

Determines the current audit year (& quarter) based on a given date. As
an example, the default is NPDA audit year, which runs from 1 April to
31 March, so dates from January-March fall into the audit year that
started in the previous calendar year.

## Usage

``` r
get_auditYear(date = Sys.Date(), start_month = 4, format = TRUE)
```

## Arguments

- date:

  Date object or string that can be coerced to Date. The date for which
  to determine the audit year. Default is
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) (today).

- start_month:

  Integer. The month (1-12) that the audit year starts. Default is 4
  (April, which is the NPDA default).

- format:

  Logical. If `TRUE` (default), returns audit year & quarter as
  formatted string "YYYY/YY Qx" (e.g., "2024/25 Q3"). If `FALSE`,
  returns the start year as an integer (e.g., 2024).

## Value

If `format = TRUE`, returns a character string in format "YYYY/YY". If
`format = FALSE`, returns an integer representing the audit year's start
year.

## Audit Year Structure

The NPDA uses a fiscal year structure running from 1 April to 31 March,
divided into four quarters:

|                   |                   |                   |                   |
|-------------------|-------------------|-------------------|-------------------|
| **Quarter**       | **Start date**    | **End date**      | Q1                |
| 01/04/CurrentYear | 30/06/CurrentYear | Q2                | 01/07/CurrentYear |
| 30/09/CurrentYear | Q3                | 01/10/CurrentYear | 31/12/CurrentYear |
| Q4                | 01/01/NextYear    | 31/03/NextYear    |                   |

For example:

- A date of 15 May 2025: 2025/26 (Q1)

- A date of 15 March 2026: 2025/26 (Q4)

- A date of 15 April 2026: 2025/26 (Q1)

## See also

Other auditYear functions:
[`family_auditYear`](https://rcpch.github.io/npdar/reference/family_auditYear.md),
[`get_auditYears()`](https://rcpch.github.io/npdar/reference/get_auditYears.md)

## Examples

``` r
# Get current audit year (based on today's date)
get_auditYear()
#> [1] "2026/27 Q3"

# Get current audit year as integer
get_auditYear(format = FALSE)
#> [1] 2026

# Customise Q1 start month
get_auditYear(start_month = 1)
#> [1] "2026/27 Q4"

# Customise Q1 start month
get_auditYear(start_month = 1)
#> [1] "2026/27 Q4"

# Determine audit year for specific dates
get_auditYear("2025-05-15")  # Returns "2025/26 Q1"
#> [1] "2025/26 Q1"
get_auditYear("2025-02-15")  # Returns "2024/25 Q4"
#> [1] "2024/25 Q4"
get_auditYear("2025-04-01")  # Returns "2025/26 Q1"
#> [1] "2025/26 Q1"
get_auditYear("2025-03-31")  # Returns "2024/26 Q4"
#> [1] "2024/25 Q4"

# Use in data processing
if (FALSE) { # \dontrun{
library(dplyr)
df <- data.frame(
  admission_date = as.Date(c("2024-05-01", "2024-12-01", "2025-02-01"))
)

df |> mutate(audit_year = get_auditYear(admission_date))
} # }
```
