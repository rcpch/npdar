# Changelog

## npdar 0.5.0

- Added `count_na` argument to
  [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md)
  to allow user to count missing values in `measures` as their own
  category (`category = NA`) and include them in the denominator. It is
  backward-compatible as default is `FALSE` (exclude NAs)
  ([\#28](https://github.com/RCPCH/npdar/issues/28)).
- [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md)
  now returns measures in the order specified in `measures` instead of
  alphabetical/numerical order.
  ([\#29](https://github.com/RCPCH/npdar/issues/29)).
- Simplified
  [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md)
  and
  [`get_corrMat()`](https://rcpch.github.io/npdar/reference/get_corrMat.md)
  internals and tidied the unit tests.
- Added a pkgdown website, <https://rcpch.github.io/npdar/>, with a
  grouped function reference
  ([\#11](https://github.com/RCPCH/npdar/issues/11)).

## npdar 0.4.3

- Added
  [`get_corrMat()`](https://rcpch.github.io/npdar/reference/get_corrMat.md)
  function to create interactive correlation matrix with `plotly`
  ([\#23](https://github.com/RCPCH/npdar/issues/23)).
- Added new feature for
  [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md)
  to summarise measures by nested groups.
- Consistent get\_ + camelCase naming convention:
  - `mask_numerators()` →
    [`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
  - `get_BP()` →
    [`get_bp()`](https://rcpch.github.io/npdar/reference/get_bp.md)
  - `get_BPCategory()` →
    [`get_bpCategory()`](https://rcpch.github.io/npdar/reference/get_bpCategory.md)
  - `get_BPExpected()` →
    [`get_bpExpected()`](https://rcpch.github.io/npdar/reference/get_bpExpected.md)
  - `get_BPRelative()` →
    [`get_bpRelative()`](https://rcpch.github.io/npdar/reference/get_bpRelative.md)
  - `get_AuditYear()` →
    [`get_auditYear()`](https://rcpch.github.io/npdar/reference/get_auditYear.md)
  - `get_AuditYears()` →
    [`get_auditYears()`](https://rcpch.github.io/npdar/reference/get_auditYears.md)
- Updated to roxygen2 8.0.0 and improved documentation.

## npdar 0.4.2

- Renamed
  [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md)
  “response” as “category” (PR
  [\#20](https://github.com/RCPCH/npdar/issues/20)).
- Added new feature for `get_AuditYear()` to customise `start_month` and
  return quarter (PR [\#20](https://github.com/RCPCH/npdar/issues/20)).
- Revised `get_AuditYear()` unit tests (PR
  [\#20](https://github.com/RCPCH/npdar/issues/20)).

## npdar 0.4.1

- Fixed
  [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md)
  drop logical/factor measures’ unobserved levels (PR
  [\#16](https://github.com/RCPCH/npdar/issues/16)).

## npdar 0.4.0

- Added
  [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md)
  to summarise categorical measures by group, returning counts,
  denominators, and percentages in a long-format tibble
  ([\#7](https://github.com/RCPCH/npdar/issues/7)).
- Added GitHub Actions R-CMD-check CI workflow.
- Cleaned R CMD check notes in `family_BP` and `get_frequency`.

## npdar 0.3.1

- Added clinical use disclaimer clarifying the package is for audit
  analysis only, not clinical decision-making
  ([\#4](https://github.com/RCPCH/npdar/issues/4)).
- Added contributors (Amani Krayem, Humfrey Legge, Saira Pons Perez) to
  DESCRIPTION ([\#4](https://github.com/RCPCH/npdar/issues/4)).
- Documented UK-WHO and US CDC height references used by NHBPEP Fourth
  Report BP functions; added `childsds` to Suggests
  ([\#5](https://github.com/RCPCH/npdar/issues/5)).
- Fixed non-ASCII characters in source files.

## npdar 0.3.0

- Added `get_AuditYear()` and `get_AuditYears()` for audit year
  determination and sequence generation following NPDA conventions (PR
  [\#3](https://github.com/RCPCH/npdar/issues/3)).
- Added `npdar-package.R` with package-level documentation.
- Renamed `bp_functions.R` to `family_BP.R`.
- Added `man-roxygen` templates.

## npdar 0.2.0

- Added BP functions: `get_BP()`, `get_BPExpected()`,
  `get_BPRelative()`, and `get_BPCategory()` implementing NHBPEP Fourth
  Report (paediatric) and NICE/BHF (adult) guidelines.
- Added `mask_numerators()` for small-count suppression in audit
  outputs.
- Added
  [`get_ultimate()`](https://rcpch.github.io/npdar/reference/get_ultimate.md)
  for longitudinal data summarisation.
- Added unit tests for BP functions.
- Added README with overview, installation, and quick-start examples (PR
  [\#1](https://github.com/RCPCH/npdar/issues/1)).
- Fixed roxygen2 typos: “peadiatric” → “paediatric”, “explicity” →
  “explicitly” (PR [\#1](https://github.com/RCPCH/npdar/issues/1)).

## npdar 0.1.0

- Initial package skeleton with DESCRIPTION, NAMESPACE, and LICENSE.
