# Mask small numerators for sensitive data privacy

This function masks small numerator values in sensitive data to protect
privacy by preventing potential identification of individuals. It
applies two key rules: (1) mask any numerators in the range (0, maxNum),
and (2) if only one numerator is masked, also mask all occurrences of
the second smallest value to prevent back-calculation.

## Usage

``` r
get_masked(vec, maxNum = 3, maskMessage = "masked")
```

## Arguments

- vec:

  A numeric vector of count data (numerators) to be masked.

- maxNum:

  Numeric. The threshold value for masking. Values greater than 0 and
  less than `maxNum` will be masked. Default is 3.

- maskMessage:

  Character string. The message to replace masked values with. Default
  is "masked".

## Value

A character vector with small values replaced by the mask message. If no
values need masking, returns the original vector converted to character.

## Details

The function implements a two-step masking process:

1.  **Initial masking**: Any values in the range (0, maxNum) are masked

2.  **Secondary masking**: If only one value was masked in step 1, all
    occurrences of the second smallest value (excluding 0) are also
    masked to prevent back-calculation of the original masked value

This approach prevents scenarios where having only one masked value
would allow users to calculate the masked value by subtraction from
known totals.

## Note

Zero values are never masked as they typically represent legitimate
absence of cases. The function always returns a character vector to
facilitate merging with other masked datasets that may contain
non-numeric mask messages.

## Examples

``` r
# Basic usage - mask values less than 3
example_vec1 <- c(4, 6, 2, 4, 0)
get_masked(example_vec1)  # Returns: "4" "6" "masked" "4" "0"
#> [1] "masked" "6"      "masked" "masked" "0"     

# With different threshold
example_vec2 <- c(4, 6, 2, 4, 1)
get_masked(example_vec2, maxNum = 3)  # Masks values 1 and 2, plus second smallest (4)
#> [1] "4"      "6"      "masked" "4"      "masked"

# Custom mask message
example_vec3 <- c(4, 6, 2, 4, 3)
get_masked(example_vec3, maxNum = 3, maskMessage = "*")
#> [1] "4" "6" "*" "4" "*"

# Demonstrating secondary masking rule
example_vec4 <- c(4, 6, 3, 4, 3)
get_masked(example_vec4, maxNum = 3)  # Only value 2 would be masked initially
#> [1] "4" "6" "3" "4" "3"

# Higher threshold
example_vec5 <- c(4, 6, 3, 4, 3)
get_masked(example_vec5, maxNum = 5, maskMessage = "*")  # Masks 3s and 4s
#> [1] "*" "6" "*" "*" "*"

# No masking needed
example_vec6 <- c(5, 6, 7, 8, 0)
get_masked(example_vec6)  # Returns original as characters: "5" "6" "7" "8" "0"
#> [1] "5" "6" "7" "8" "0"

# Realistic workflow example
if (FALSE) { # \dontrun{
library(dplyr)
library(tibble)

# Create example demographic data
set.seed(1234)
dat <- tibble(
  unit_number = sample(8, size = 100, replace = TRUE),
  gender = sample(c("boy", "girl", "non-binary", "prefer not to say"),
                  size = 100, replace = TRUE)
)

# Apply masking to grouped counts
results <- dat |>
  group_by(unit_number, gender) |>
  summarise(numerator = n(), .groups = "drop") |>
  mutate(
    proportion = sprintf("%.1f%%", 100 * numerator / sum(numerator)),
    numerator_masked = get_masked(numerator, maxNum = 3),
    proportion_masked = ifelse(numerator_masked == 'masked', 'masked', proportion)
  )
} # }
```
