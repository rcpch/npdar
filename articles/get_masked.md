# Masking small numerators with get_masked()

``` r

library(npdar)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## The problem

Audit results are often published as counts (numerators) longside their
percentages and a total (denominator). Small counts can identify
individual patients, so they are masked. Suppose a region has 8
hospitals, and each reports the number of children who need
psychological support:

``` r

hospitals <- tibble(
  hospital = LETTERS[1:8],
  n = c(0, 0, 2, 2, 3, NA, NA, NA)
)
```

The regional total is 7 children. Hospitals A and B reported none, C and
D reported 2 each, E reported 3, and F, G, H did not report (`NA`).

## Why not just use `ifelse()`?

The obvious approach is to mask every count below a threshold, say 3.
But if the total is published, a **single** masked count can be
recovered by subtraction:

``` r

counts_real <- c(5, 1, 8, 10)
total <- sum(counts_real)

counts_naive_mask <- ifelse(counts_real > 0 & counts_real < 3, "*", counts_real)
counts_naive_mask
#> [1] "5"  "*"  "8"  "10"

# Anyone can recover the masked value from the published total
total - sum(as.numeric(counts_naive_mask[counts_naive_mask != "*"]))
#> [1] 1
```

[`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
prevents this. If fewer than two values would be masked, it also masks
the next smallest positive value (by default, all occurrences of it), so
the masked values can no longer be told apart:

``` r

get_masked(counts_real, threshold = 3, mask_label = "*")
#> [1] "*"  "*"  "8"  "10"
```

Obviously, masking with
[`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
only makes sense **when the denominator is shown**. If it is not, a
simple [`ifelse()`](https://rdrr.io/r/base/ifelse.html) on the threshold
is enough.

## Basic usage

[`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
masks positive counts below `threshold` which you must specify: the
right value depends on your reporting rules and the NPDA usually uses 3.
By default, zeros and `NA`s are exempt from masking as they are usally
meaningful and do not put privacy at risk, but you can choose to mask
them too. The result is always a character vector, so it can be combined
with a mask label. The default `mask_label` is “masked” for the masked
values.

``` r

get_masked(hospitals$n, threshold = 3, mask_label = "*")
#> [1] "0" "0" "*" "*" "3" NA  NA  NA
```

Both 2s are masked because they are below 3, and there are two of them.
Zeros and `NA`s are kept because they carry a meaningful message and do
not put privacy at risk. For instance, it could be a positive news that
no patient at hospital A and B need psychological support. Also, the
fact that hospital F, G, H result is unknown could carry many meanings,
it could be problem with workforce, or could mean there’s too many
patients requiring support that they lose count.

When only one value is below the threshold, the next smallest positive
value is masked as well. Here the 1 is the only count below 3, so the 5s
are also masked:

``` r

vec <- c(1, 5, 5, 6, 7, 0, NA)
get_masked(vec, threshold = 3, mask_label = "*")
#> [1] "*" "*" "*" "6" "7" "0" NA
```

The `threshold` sets the smallest count that can be shown, and
`mask_label` sets what replaces the masked values:

``` r

get_masked(vec, threshold = 3)
#> [1] "masked" "masked" "masked" "6"      "7"      "0"      NA
get_masked(vec, threshold = 7, mask_label = "*")
#> [1] "*" "*" "*" "*" "7" "0" NA
```

## Masking zeros and NAs

Zeros and `NA`s can be masked too, in case you believe that masking them
is better than masking the higher numbers.

``` r

get_masked(vec, threshold = 3, mask_label = "*", mask_zero = TRUE)
#> [1] "*" "5" "5" "6" "7" "*" NA
get_masked(vec, threshold = 3, mask_label = "*", mask_na = TRUE)
#> [1] "*" "5" "5" "6" "7" "0" "*"
get_masked(vec, threshold = 3, mask_label = "*", mask_zero = TRUE, mask_na = TRUE)
#> [1] "*" "5" "5" "6" "7" "*" "*"
```

Note that masked zeros and `NA`s count towards the minimum of two masked
values, but they cannot be the only ones. They add nothing to the total,
so if only they were masked, subtracting the visible counts from the
denominator would show that every masked value is 0 or unknown. At least
one positive value is therefore always masked, however many zeros or
`NA`s there are.

If there is nothing to protect, nothing is masked:

``` r

get_masked(c(0, 0, 0), threshold = 3)
#> [1] "0" "0" "0"
get_masked(c(0, 0, 0), threshold = 3, mask_label = "*", mask_zero = TRUE)
#> [1] "*" "*" "*"
```

A vector made only of bare `NA`s is logical in R, so use `NA_real_` or
`NA_integer_` if a whole group can be missing:

``` r

get_masked(c(NA_real_, NA_real_), threshold = 3, mask_label = "*", mask_na = TRUE)
#> [1] "*" "*"
```

## Masking a data frame

### Standard workflow

Derived values must be masked too. If a percentage is published for a
masked count, the masked count can be calculated back from it, so the
percentage should use the same mask:

``` r

hospitals |>
  mutate(
    total = sum(n, na.rm = TRUE),
    n_masked = get_masked(n, mask_label = "*", threshold = 3),
    pct_masked = case_when(
      is.na(n) ~ NA_character_,
      n_masked == "*" ~ "*",
      .default = sprintf("%.1f%%", 100 * n / total)
    )
  )
#> # A tibble: 8 × 5
#>   hospital     n total n_masked pct_masked
#>   <chr>    <dbl> <dbl> <chr>    <chr>     
#> 1 A            0     7 0        0.0%      
#> 2 B            0     7 0        0.0%      
#> 3 C            2     7 *        *         
#> 4 D            2     7 *        *         
#> 5 E            3     7 3        42.9%     
#> 6 F           NA     7 <NA>     <NA>      
#> 7 G           NA     7 <NA>     <NA>      
#> 8 H           NA     7 <NA>     <NA>
```

### Masking within groups

[`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
protects the vector it is given, so inside
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) each
group is masked separately. Do this when each group has its own
denominator:

``` r

regions <- tibble(
  region = rep(c("North", "South"), each = 4),
  hospital = LETTERS[1:8],
  n = c(1, 5, 6, 7, 10, 12, 2, 20)
)

regions |>
  group_by(region) |>
  mutate(total = sum(n, na.rm = TRUE),
         n_masked = get_masked(n, threshold = 3, mask_label = "*")) |>
  ungroup()
#> # A tibble: 8 × 5
#>   region hospital     n total n_masked
#>   <chr>  <chr>    <dbl> <dbl> <chr>   
#> 1 North  A            1    19 *       
#> 2 North  B            5    19 *       
#> 3 North  C            6    19 6       
#> 4 North  D            7    19 7       
#> 5 South  E           10    44 *       
#> 6 South  F           12    44 12      
#> 7 South  G            2    44 *       
#> 8 South  H           20    44 20
```

If only the overall total is published, mask the whole column instead,
without
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
