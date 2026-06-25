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
