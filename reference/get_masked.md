# Mask small numerators for sensitive data privacy

Masks small numerators (counts) to protect patient privacy when the
corresponding denominator (the total) is also shown. It applies two
rules: (1) mask any numerator in the range (0, threshold), optionally
together with zeros and `NA`s, and (2) make sure at least two values are
masked, at least one of them positive, by also masking the next smallest
positive value(s) if needed.

Rule 2 is the reason this function exists. If a denominator is
published, a single masked numerator can be recovered by subtracting the
visible numerators from the total. Masking at least two values, one of
them positive, leaves the individual values undetermined. If the
denominator is not shown, a simple
[`ifelse()`](https://rdrr.io/r/base/ifelse.html) on the threshold should
suffice and there's no need to use this function.

## Usage

``` r
get_masked(
  vec,
  threshold,
  mask_label = "masked",
  mask_zero = FALSE,
  mask_na = FALSE
)
```

## Arguments

- vec:

  A numeric vector of count data (numerators) to be masked. Only values
  `>= 0` and `NA` are accepted; negative values and non-numeric input
  throw an error. Note that a mixed vector such as
  `c(1, 2, TRUE, FALSE)` is coerced to numeric by R before reaching the
  function, so it will be silently allowed.

- threshold:

  Numeric. The threshold value for masking. Values greater than 0 and
  less than `threshold` will be masked. As an example, the NPDA usually
  uses 3.

- mask_label:

  Character string. The message to replace masked values with. Default
  is "masked".

- mask_zero:

  Logical. If `TRUE`, zeros are masked as well. Default is `FALSE`,
  because a zero conveys a meaningful and positive message (e.g. no
  patients at this hospital has the disease) and does not put privacy at
  risk.

- mask_na:

  Logical. If `TRUE`, `NA`s are masked as well. Default is `FALSE`,
  because an `NA` conveys a meaningful message (e.g. the hospital has
  unknown number of patients with the disease) and does not put privacy
  at risk.

## Value

A character vector with masked values replaced by `mask_label`. If no
values need masking, returns the original vector converted to character.
Unmasked `NA`s are returned as `NA_character_`.

## Details

The function implements a two-step masking process:

1.  **Initial masking**: Any values in the range (0, threshold) are
    masked, plus zeros if `mask_zero = TRUE` and `NA`s if
    `mask_na = TRUE`.

2.  **Secondary masking**: If anything was masked but fewer than two
    values were, or none of them is positive, the smallest unmasked
    positive value(s) are also masked, all occurrences at a time, until
    at least two values are masked and at least one is positive (or none
    are left).

**Masked zeros and `NA`s:** When `mask_zero` and `mask_na` are `TRUE`,
zeros and `NA`s count towards the minimum of two masked values but
cannot be the only ones. They add nothing to the total, so if only they
were masked, subtracting the visible numerators from the denominator
would reveal that every masked value is 0 or unknown, which is why the
smallest positive value(s) must also be masked.

## Note

The function always returns a character vector to facilitate merging
with other masked datasets that may contain non-numeric mask messages.

## Examples

``` r
# Positive values less than the threshold and the second smallest positive value are masked
vec1 <- c(5, 6, 4, 5, 1, 0, 0, NA)
get_masked(vec1, threshold = 3)
#> [1] "5"      "6"      "masked" "5"      "masked" "0"      "0"      NA      

# Custom mask message
get_masked(vec1, threshold = 3, mask_label = "*")
#> [1] "5" "6" "*" "5" "*" "0" "0" NA 

# No masking if all positive values are above the threshold
vec2 <- c(5, 6, 4, 5, 3, 0, 0, NA)
get_masked(vec2, mask_label = "*", threshold = 3)
#> [1] "5" "6" "4" "5" "3" "0" "0" NA 

# Custom threshold
get_masked(vec2, mask_label = "*", threshold = 4)
#> [1] "5" "6" "*" "5" "*" "0" "0" NA 

# Zeros are kept by default; mask_zero = TRUE masks them and the smallest positive values too
get_masked(vec1, threshold = 3, mask_label = "*", mask_zero = TRUE)
#> [1] "5" "6" "4" "5" "*" "*" "*" NA 

# NAs are kept by default; mask_na = TRUE masks them and the smallest positive values too
get_masked(vec1, threshold = 3, mask_label = "*", mask_na = TRUE)
#> [1] "5" "6" "4" "5" "*" "0" "0" "*"
get_masked(vec1, threshold = 3, mask_label = "*", mask_zero = TRUE, mask_na = TRUE)
#> [1] "5" "6" "4" "5" "*" "*" "*" "*"

# Realistic workflow example
if (FALSE) { # \dontrun{
library(dplyr)
library(tibble)

# Create example demographic data
set.seed(1234)
dat <- tibble(
  hospital_id = sample(8, size = 100, replace = TRUE),
  condition = sample(c("adhd", "asd", "dyslexia","prefer not to say"),
                     size = 100, replace = TRUE)
)
# Apply masking to grouped counts
results <- dat |>
  group_by(hospital_id, condition) |>
  summarise(numerator = n(), .groups = "drop") |>
  mutate(
    denomiantor = sum(numerator),
    percent = sprintf("%.1f%%", 100 * numerator / denomiantor),
    numerator_masked = get_masked(numerator, threshold = 3, mask_label = '*'),
    percent_masked = ifelse(numerator_masked == '*', '*', percent)
  )
} # }
```
