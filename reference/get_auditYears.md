# Generate Sequential List of Audit Years

Creates a sequential list of NPDA audit years between specified start
and end years. Useful for generating axis labels, filtering data by
multiple audit years, or creating comprehensive reports across multiple
audit periods.

## Usage

``` r
get_auditYears(
  startYear = 2010,
  endYear = get_auditYear(format = FALSE),
  format = TRUE
)
```

## Arguments

- startYear:

  Integer. The first audit year to include in the sequence. Default is
  2010 (when NPDA began).

- endYear:

  Integer. The last audit year to include in the sequence. Default is
  the current audit year (determined by
  `get_auditYear(format = FALSE)`).

- format:

  Logical. If `TRUE` (default), returns audit years as formatted strings
  "YYYY/YY" (e.g., "2024/25"). If `FALSE`, returns integers representing
  each audit year's start year.

## Value

If `format = TRUE`, returns a character vector of audit years in format
"YYYY/YY". If `format = FALSE`, returns an integer vector of audit year
start years.

## Details

This function generates all audit years from `startYear` to `endYear`
inclusive. It's particularly useful for:

- Creating dropdown menus or filters in Shiny applications

- Generating x-axis labels for time-series plots

- Iterating over multiple audit years in analysis pipelines

- Ensuring consistent audit year labeling across reports

The NPDA began in the 2010/11 audit year, hence the default start year
of 2010.

## See also

[`get_auditYear`](https://rcpch.github.io/npdar/reference/get_auditYear.md)
for determining a single audit year from a date.

Other auditYear functions:
[`family_auditYear`](https://rcpch.github.io/npdar/reference/family_auditYear.md),
[`get_auditYear()`](https://rcpch.github.io/npdar/reference/get_auditYear.md)

## Examples

``` r
# Get all audit years from 2010 to current (formatted)
get_auditYears()
#>  [1] "2010/11" "2011/12" "2012/13" "2013/14" "2014/15" "2015/16" "2016/17"
#>  [8] "2017/18" "2018/19" "2019/20" "2020/21" "2021/22" "2022/23" "2023/24"
#> [15] "2024/25" "2025/26" "2026/27"

# Get specific range of audit years
get_auditYears(startYear = 2015, endYear = 2020)
#> [1] "2015/16" "2016/17" "2017/18" "2018/19" "2019/20" "2020/21"
# Returns: "2015/16" "2016/17" "2017/18" "2018/19" "2019/20" "2020/21"

# Get unformatted (integer) audit years
get_auditYears(startYear = 2018, endYear = 2020, format = FALSE)
#> [1] 2018 2019 2020
# Returns: 2018 2019 2020

# Use in plotting
if (FALSE) { # \dontrun{
library(ggplot2)
audit_years <- get_auditYears(2015, 2020)
ggplot(data.frame(year = audit_years, value = rnorm(6))) +
  aes(x = year, y = value) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Use in data filtering
library(dplyr)
recent_years <- get_auditYears(startYear = 2020)
df |> filter(audit_year %in% recent_years)

# Create lookup table
data.frame(
  audit_year = get_auditYears(2010, 2015),
  audit_year_numeric = get_auditYears(2010, 2015, format = FALSE)
)
#   audit_year audit_year_numeric
# 1    2010/11               2010
# 2    2011/12               2011
# 3    2012/13               2012
# 4    2013/14               2013
# 5    2014/15               2014
# 6    2015/16               2015
} # }
```
