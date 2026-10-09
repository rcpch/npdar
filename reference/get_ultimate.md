# Get the up-to-date/first/last mode or first/last entry from a vector

This function finds the most recent (i.e., up-to-date) mode, the
first/last mode, or the first/last entry from a vector, with options to
ignore specific values. Particularly useful for finding the most current
valid values in longitudinal data. `NA` values are always excluded by
default.

## Usage

``` r
get_ultimate(x, find, except, warning = TRUE)
```

## Arguments

- x:

  A vector of values to be evaluated. Can be numeric, character, but
  date (POSIXct) is currently not supported.

- find:

  Character string specifying what to find. Options are:

  - `"uptodate_mode"` - Among all modal values, returns the one that
    appears last in the vector

  - `"first_mode"` - Among all modal values, returns the first mode in
    order of unique values

  - `"last_mode"` - Among all modal values, returns the last mode in
    order of unique values

  - `"first_entry"` - The first valid entry in the vector (after
    exclusions)

  - `"last_entry"` - The last valid entry in the vector (after
    exclusions)

- except:

  A value or vector of values to ignore/exclude from consideration. `NA`
  values are always excluded automatically. Can be a single value (e.g.,
  99, "Unknown", NA) or multiple values (e.g., `c(99, "Unknown")`). If
  not specified, a warning is issued.

- warning:

  Logical. Whether to display warning messages. Warnings are shown when
  multiple modal values exist, showing the remaining modes apart from
  the one designated in `find`, or when `except` is not specified.
  Default is `TRUE`.

## Value

The result based on the `find` and `except` parameters. If no valid
values remain after exclusions, returns `NA`. When multiple modes exist
and `warning = TRUE`, a warning message lists the other modal values not
returned.

## Examples

``` r
# Example 1: Exclude invalid entries before finding the designated value
x <- c(1, 1, 2, 2, 2, 1, 99, 99, 99, 99, NA, NA, NA, NA)
get_ultimate(x, find="uptodate_mode")  # 99 (BAD)
#> Warning: NAs are excluded by default, but it's still good practice to specify the invalid values to exclude.
#> [1] 99
get_ultimate(x, find="uptodate_mode",
             except=99)                # 1 NAs excluded by default (GOOD)
#> Warning: There are multiple modes, the remaining modes are: 2
#> [1] 1
get_ultimate(x, find="uptodate_mode",
             except=c(99, NA))         # 1 (GOOD)
#> Warning: There are multiple modes, the remaining modes are: 2
#> [1] 1
# When all values are excluded
x <- c(99, 99, 99, 99, NA, NA, NA, NA)
get_ultimate(x, find='uptodate_mode')  # 99 (BAD PRACTICE)
#> Warning: NAs are excluded by default, but it's still good practice to specify the invalid values to exclude.
#> [1] 99
get_ultimate(x, find='uptodate_mode',
             except=99)                # NA (GOOD PRACTICE)
#> [1] NA
get_ultimate(c(9,9,9,9,9), find='uptodate_mode',
             except=9, warning=F)      # NA
#> [1] NA
get_ultimate(c(9,9,9,9,9), find='last_entry',
             except=9, warning=F)      # NA
#> [1] NA

# Example 2: Finding up-to-date mode with categorical data
x <- c("F", "F", "Other", "Other", "Unknown",
        "Other", "M", "M", "M", "M", "F", "F",
        "Other", "Unknown", "Unknown", "Unknown")
get_ultimate(x, find = "uptodate_mode",
             except = "Unknown")        # "Other" (last occurrence) (GOOD)
#> Warning: There are multiple modes, the remaining modes are: F, M
#> [1] "Other"
get_ultimate(x, find = "uptodate_mode") # "Unknown" (BAD)
#> Warning: NAs are excluded by default, but it's still good practice to specify the invalid values to exclude.
#> Warning: There are multiple modes, the remaining modes are: F, Other, M
#> [1] "Unknown"
get_ultimate(x, find = "first_mode",
             warning = FALSE)           # "F" (first among unique modes)
#> [1] "F"
get_ultimate(x, find = "last_mode",
             warning = FALSE)           # "Unknown" (last among unique modes)
#> [1] "M"

# Example 3: Finding first/last valid entry
get_ultimate(x, find = "last_entry",
             warning = FALSE)           # "Unknown" (BAD)
#> [1] "Unknown"
get_ultimate(x, find = "last_entry", except = "Unknown",
             warning = FALSE)           # "Other" (GOOD)
#> [1] "Other"
get_ultimate(x, find = "first_entry", except = "Unknown",
             warning = FALSE)           # "F"
#> [1] "F"
```
