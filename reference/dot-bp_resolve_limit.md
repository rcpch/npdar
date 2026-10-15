# Resolve BP Limit (Internal)

Internal helper function to returns `bp_limit` unless it is the default
`c(-Inf, Inf)`, in which case the NPDA (Fourth Report) or NDA (NICE/BHF)
range for `valid$bp_type` is used.

## Usage

``` r
.bp_resolve_limit(valid, bp_limit)
```

## See also

Other BP functions:
[`.bp_message()`](https://rcpch.github.io/npdar/reference/dot-bp_message.md),
[`.bp_validate_demo_input()`](https://rcpch.github.io/npdar/reference/dot-bp_validate_demo_input.md),
[`bp_assess()`](https://rcpch.github.io/npdar/reference/bp_assess.md),
[`bp_category()`](https://rcpch.github.io/npdar/reference/bp_category.md),
[`bp_expected()`](https://rcpch.github.io/npdar/reference/bp_expected.md),
[`bp_family`](https://rcpch.github.io/npdar/reference/bp_family.md),
[`bp_relative()`](https://rcpch.github.io/npdar/reference/bp_relative.md)
