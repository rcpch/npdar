# Changelog

## npdar 0.7.0

- New
  [`plot_raincloud_ly()`](https://rcpch.github.io/npdar/reference/plot_raincloud_ly.md)
  draws an interactive plotly raincloud plot
  ([\#22](https://github.com/RCPCH/npdar/issues/22)).
- Breaking: `plot_corr_matrix()` is renamed to
  [`plot_corrmat_ly()`](https://rcpch.github.io/npdar/reference/plot_corrmat_ly.md),
  so interactive plotly functions share the `_ly` suffix.
- [`plot_corrmat_ly()`](https://rcpch.github.io/npdar/reference/plot_corrmat_ly.md)
  hover text now shows the sample size `n` used for each pair, according
  to `use` ([\#33](https://github.com/RCPCH/npdar/issues/33)).
- [`plot_corrmat_ly()`](https://rcpch.github.io/npdar/reference/plot_corrmat_ly.md)
  with `use = "na.or.complete"` no longer errors when there are no
  complete rows; it returns `NA` correlations as
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) does
  (`use = "complete.obs"` still errors).
- Breaking:
  [`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
  no longer has a default `threshold`; it must now be specified
  ([\#37](https://github.com/RCPCH/npdar/issues/37)).
- Initiated vignettes
  ([\#11](https://github.com/RCPCH/npdar/issues/11)).
- [`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
  now errors if `vec` is not numeric (including logical) or contains
  negative values and handles `NA`s; only values `>= 0` and `NA` are
  accepted ([\#34](https://github.com/RCPCH/npdar/issues/34)).
- [`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
  gains `mask_na` and `mask_zero` to allow user to choose whether
  whether to mask `NA`s or zeros (both default to `FALSE`, backward
  compatible). The function’s secondary masking now ensures at least two
  values are masked, at least one of them positive. Masked zeros and
  `NA`s count towards the two, but the smallest positive value(s) are
  always masked even if only zeros or `NA`s were masked
  ([\#34](https://github.com/RCPCH/npdar/issues/34)).
- Rewrite
  [`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
  documentation and add a vignette
  [`vignette("get_masked")`](https://rcpch.github.io/npdar/articles/get_masked.md)
  ([\#11](https://github.com/RCPCH/npdar/issues/11),
  [\#34](https://github.com/RCPCH/npdar/issues/34)).
- [`get_ultimate()`](https://rcpch.github.io/npdar/reference/get_ultimate.md)
  gains `na.rm` (default `TRUE`, backward compatible). Set
  `na.rm = FALSE` to treat `NA` as a meaningful value considered as a
  valid candidate. `exclude` must not contain `NA` (it now errors); use
  `na.rm` instead ([\#35](https://github.com/RCPCH/npdar/issues/35)).
- Fix typo in `npdar-package.R`.

## npdar 0.6.0

- Rewrite `README.md` with `README.qmd` and update `.Rbuildignore`.
- Add light switch and MathJax for math notations in pkgdown site.
- Breaking: renamed functions to snake_case; the domain families drop
  `get_`, the utilities keep it:
  - `get_bp()` →
    [`bp_assess()`](https://rcpch.github.io/npdar/reference/bp_assess.md)
  - `get_bpExpected()` →
    [`bp_expected()`](https://rcpch.github.io/npdar/reference/bp_expected.md)
  - `get_bpRelative()` →
    [`bp_relative()`](https://rcpch.github.io/npdar/reference/bp_relative.md)
  - `get_bpCategory()` →
    [`bp_category()`](https://rcpch.github.io/npdar/reference/bp_category.md)
  - `get_auditYear()` →
    [`audit_year()`](https://rcpch.github.io/npdar/reference/audit_year.md)
  - `get_auditYears()` →
    [`audit_year_range()`](https://rcpch.github.io/npdar/reference/audit_year_range.md)
  - `get_corrMat()` → `plot_corr_matrix()`
  - Help topics `family_bp` → `bp_family` and `family_auditYear` →
    `audit_year_family`.
  - [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md),
    [`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
    and
    [`get_ultimate()`](https://rcpch.github.io/npdar/reference/get_ultimate.md)
    are unchanged.
- Breaking: renamed arguments to snake_case with clearer names:
  - [`audit_year_range()`](https://rcpch.github.io/npdar/reference/audit_year_range.md):
    `startYear` → `start_year`, `endYear` → `end_year`.
  - [`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md):
    `maxNum` → `threshold`, `maskMessage` → `mask_label`.
  - [`get_ultimate()`](https://rcpch.github.io/npdar/reference/get_ultimate.md):
    `except` → `exclude`, `warning` → `warn`.
- Edit help aliases `bp` (for `bp_family`) and `fiscal_year` (for
  `audit_year_family`).
- Breaking: In `bp_family`,
  [`bp_assess()`](https://rcpch.github.io/npdar/reference/bp_assess.md)
  no longer takes `option`; it validates inputs once and returns a
  tibble with `expected`, `zscore`, `percentile` and `category`.
- In `bp_family`, functions now error if `sex`, `age_years`, `height_z`
  and `bp_value` differ in length (no recycling), and
  [`bp_relative()`](https://rcpch.github.io/npdar/reference/bp_relative.md)
  returns one row per input for NICE/BHF.
- In `bp_family`,
  [`bp_category()`](https://rcpch.github.io/npdar/reference/bp_category.md)
  gains `.quiet`, and `.quiet = TRUE` now also silences the NICE/BHF
  notices in
  [`bp_expected()`](https://rcpch.github.io/npdar/reference/bp_expected.md)
  and
  [`bp_relative()`](https://rcpch.github.io/npdar/reference/bp_relative.md).
- In `bp_family`, fixed
  [`bp_category()`](https://rcpch.github.io/npdar/reference/bp_category.md)
  and
  [`bp_assess()`](https://rcpch.github.io/npdar/reference/bp_assess.md)
  returning “Prehypertension” for BP below the lower `bp_limit`; these
  now return `NA`.

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
  and `get_corrMat()` internals and tidied the unit tests.
- Added a pkgdown website, <https://rcpch.github.io/npdar/>, with a
  grouped function reference
  ([\#11](https://github.com/RCPCH/npdar/issues/11)).

## npdar 0.4.3

- Added `get_corrMat()` function to create interactive correlation
  matrix with `plotly`
  ([\#23](https://github.com/RCPCH/npdar/issues/23)).
- Added new feature for
  [`get_frequency()`](https://rcpch.github.io/npdar/reference/get_frequency.md)
  to summarise measures by nested groups.
- Consistent get\_ + camelCase naming convention:
  - `mask_numerators()` →
    [`get_masked()`](https://rcpch.github.io/npdar/reference/get_masked.md)
  - `get_BP()` → `get_bp()`
  - `get_BPCategory()` → `get_bpCategory()`
  - `get_BPExpected()` → `get_bpExpected()`
  - `get_BPRelative()` → `get_bpRelative()`
  - `get_AuditYear()` → `get_auditYear()`
  - `get_AuditYears()` → `get_auditYears()`
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
