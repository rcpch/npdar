# Print BP Messages (Internal)

Internal helper function to print informative BP messages based on the
reference and inputs.

## Usage

``` r
.bp_message(valid, what, bp_limit = c(-Inf, Inf), .quiet = FALSE)
```

## Arguments

- valid:

  List returned by
  [`.bp_validate_demo_input()`](https://rcpch.github.io/npdar/reference/dot-bp_validate_demo_input.md).

- what:

  Any of `"expected"`, `"relative"`, `"category"`: the outputs
  requested.

- bp_limit:

  The `bp_limit` supplied by the user; the default value triggers the
  limit message.

- .quiet:

  Logical; suppress all messages.

## See also

Other BP functions:
[`.bp_resolve_limit()`](https://rcpch.github.io/npdar/reference/dot-bp_resolve_limit.md),
[`.bp_validate_demo_input()`](https://rcpch.github.io/npdar/reference/dot-bp_validate_demo_input.md),
[`bp_assess()`](https://rcpch.github.io/npdar/reference/bp_assess.md),
[`bp_category()`](https://rcpch.github.io/npdar/reference/bp_category.md),
[`bp_expected()`](https://rcpch.github.io/npdar/reference/bp_expected.md),
[`bp_family`](https://rcpch.github.io/npdar/reference/bp_family.md),
[`bp_relative()`](https://rcpch.github.io/npdar/reference/bp_relative.md)
