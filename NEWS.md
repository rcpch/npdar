# npdar 0.7.0

* New `plot_raincloud_ly()` draws an interactive plotly raincloud plot (#22).
* Breaking: `plot_corr_matrix()` is renamed to `plot_corrmat_ly()`, so interactive plotly functions share the `_ly` suffix.
* `plot_corrmat_ly()` hover text now shows the sample size `n` used for each pair, according to `use` (#33).
* `plot_corrmat_ly()` with `use = "na.or.complete"` no longer errors when there are no complete rows; it returns `NA` correlations as `stats::cor()` does (`use = "complete.obs"` still errors).
* Breaking: `get_masked()` no longer has a default `threshold`; it must now be specified (#37).
* Initiated vignettes (#11).
* `get_masked()` now errors if `vec` is not numeric (including logical) or contains negative values and handles `NA`s; only values `>= 0` and `NA` are accepted (#34).
* `get_masked()` gains `mask_na` and `mask_zero` to allow user to choose whether whether to mask `NA`s or zeros (both default to `FALSE`, backward compatible). The function's secondary masking now ensures at least two values are masked, at least one of them positive. Masked zeros and `NA`s count towards the two, but the smallest positive value(s) are always masked even if only zeros or `NA`s were masked (#34).
* Rewrite `get_masked()` documentation and add a vignette `vignette("get_masked")` (#11, #34).
* `get_ultimate()` gains `na.rm` (default `TRUE`, backward compatible). Set `na.rm = FALSE` to treat `NA` as a meaningful value considered as a valid candidate. `exclude` must not contain `NA` (it now errors); use `na.rm` instead (#35).
* Fix typo in `npdar-package.R`.

# npdar 0.6.0
* Rewrite `README.md` with `README.qmd` and update `.Rbuildignore`.
* Add light switch and MathJax for math notations in pkgdown site.
* Breaking: renamed functions to snake_case; the domain families drop `get_`, the utilities keep it:
    - `get_bp()` → `bp_assess()`
    - `get_bpExpected()` → `bp_expected()`
    - `get_bpRelative()` → `bp_relative()`
    - `get_bpCategory()` → `bp_category()`
    - `get_auditYear()` → `audit_year()`
    - `get_auditYears()` → `audit_year_range()`
    - `get_corrMat()` → `plot_corr_matrix()`
    - Help topics `family_bp` → `bp_family` and `family_auditYear` → `audit_year_family`.
    - `get_frequency()`, `get_masked()` and `get_ultimate()` are unchanged.
* Breaking: renamed arguments to snake_case with clearer names:
    - `audit_year_range()`: `startYear` → `start_year`, `endYear` → `end_year`.
    - `get_masked()`: `maxNum` → `threshold`, `maskMessage` → `mask_label`.
    - `get_ultimate()`: `except` → `exclude`, `warning` → `warn`.
* Edit help aliases `bp` (for `bp_family`) and `fiscal_year` (for `audit_year_family`).
* Breaking: In `bp_family`, `bp_assess()` no longer takes `option`; it validates inputs once and returns a tibble with `expected`, `zscore`, `percentile` and `category`.
* In `bp_family`, functions now error if `sex`, `age_years`, `height_z` and `bp_value` differ in length (no recycling), and `bp_relative()` returns one row per input for NICE/BHF.
* In `bp_family`, `bp_category()` gains `.quiet`, and `.quiet = TRUE` now also silences the NICE/BHF notices in `bp_expected()` and `bp_relative()`.
* In `bp_family`, fixed `bp_category()` and `bp_assess()` returning "Prehypertension" for BP below the lower `bp_limit`; these now return `NA`.

# npdar 0.5.0

* Added `count_na` argument to `get_frequency()` to allow user to count missing values in `measures` as their own category (`category = NA`) and include them in the denominator. It is backward-compatible as default is `FALSE` (exclude NAs) (#28).
* `get_frequency()` now returns measures in the order specified in `measures` instead of alphabetical/numerical order. (#29).
* Simplified `get_frequency()` and `get_corrMat()` internals and tidied the unit tests.
* Added a pkgdown website, <https://rcpch.github.io/npdar/>, with a grouped function reference (#11).

# npdar 0.4.3
* Added `get_corrMat()` function to create interactive correlation matrix with `plotly` (#23).
* Added new feature for `get_frequency()` to summarise measures by nested groups. 
* Consistent get_ + camelCase naming convention:
    - `mask_numerators()` → `get_masked()`
    - `get_BP()` → `get_bp()`
    - `get_BPCategory()` → `get_bpCategory()`
    - `get_BPExpected()` → `get_bpExpected()`
    - `get_BPRelative()` → `get_bpRelative()`
    - `get_AuditYear()` → `get_auditYear()`
    - `get_AuditYears()` → `get_auditYears()`
* Updated to roxygen2 8.0.0 and improved documentation.

# npdar 0.4.2

* Renamed `get_frequency()` "response" as "category" (PR #20).
* Added new feature for `get_AuditYear()` to customise `start_month` and return quarter (PR #20).
* Revised `get_AuditYear()` unit tests (PR #20).

# npdar 0.4.1

* Fixed `get_frequency()` drop logical/factor measures' unobserved levels (PR #16).

# npdar 0.4.0

* Added `get_frequency()` to summarise categorical measures by group, returning
  counts, denominators, and percentages in a long-format tibble (#7).
* Added GitHub Actions R-CMD-check CI workflow.
* Cleaned R CMD check notes in `family_BP` and `get_frequency`.

# npdar 0.3.1

* Added clinical use disclaimer clarifying the package is for audit analysis
  only, not clinical decision-making (#4).
* Added contributors (Amani Krayem, Humfrey Legge, Saira Pons Perez) to
  DESCRIPTION (#4).
* Documented UK-WHO and US CDC height references used by NHBPEP Fourth Report
  BP functions; added `childsds` to Suggests (#5).
* Fixed non-ASCII characters in source files.

# npdar 0.3.0

* Added `get_AuditYear()` and `get_AuditYears()` for audit year determination
  and sequence generation following NPDA conventions (PR #3).
* Added `npdar-package.R` with package-level documentation.
* Renamed `bp_functions.R` to `family_BP.R`.
* Added `man-roxygen` templates.

# npdar 0.2.0

* Added BP functions: `get_BP()`, `get_BPExpected()`, `get_BPRelative()`, and
  `get_BPCategory()` implementing NHBPEP Fourth Report (paediatric) and
  NICE/BHF (adult) guidelines.
* Added `mask_numerators()` for small-count suppression in audit outputs.
* Added `get_ultimate()` for longitudinal data summarisation.
* Added unit tests for BP functions.
* Added README with overview, installation, and quick-start examples (PR #1).
* Fixed roxygen2 typos: "peadiatric" → "paediatric", "explicity" →
  "explicitly" (PR #1).

# npdar 0.1.0

* Initial package skeleton with DESCRIPTION, NAMESPACE, and LICENSE.
