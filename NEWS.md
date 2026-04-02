# npdar 0.4.0

* Added `get_frequency()` function (beta) to summarise categorical measures by
  group, returning counts, denominators, and percentages in a long-format tibble
  (#7).
* Added GitHub Actions R-CMD-check CI workflow.
* Cleaned R CMD check notes in `family_BP` and `get_frequency`.
* Updated README and package documentation.
* Created NEWS.md to track changes.

# npdar 0.3.1

* Added clinical use disclaimer to README and package documentation clarifying
  the package is for audit analysis only, not clinical decision-making (#4).
* Added contributors (Amani Krayem, Humfrey Legge, Saira Pons Perez) to
  DESCRIPTION and package documentation (#4).
* Improved documentation of UK-WHO and US CDC height references used by the
  NHBPEP Fourth Report BP functions (#5).
* Added `childsds` to Suggests for Fourth Report height z-score calculations
  (#5).
* Reviewed and fixed non-ASCII characters in source files.
* Improved function documentation formatting.

# npdar 0.3.0

* Added audit year functions: `get_AuditYear()` to determine audit year from
  date(s) and `get_AuditYears()` to generate sequences of audit years following
  NPDA conventions (PR #3).
* Improved documentation for BP functions, `get_ultimate()`, and overall package
  documentation.
* Added `npdar-package.R` with comprehensive package-level documentation.
* Renamed `bp_functions.R` to `family_BP.R`.
* Added `man-roxygen` templates.

# npdar 0.2.0

* Added blood pressure assessment functions: `get_BP()`, `get_BPExpected()`,
  `get_BPRelative()`, and `get_BPCategory()` implementing NHBPEP Fourth Report
  (paediatric) and NICE/BHF (adult) guidelines.
* Added `mask_numerators()` for data suppression of small counts in audit
  outputs.
* Added `get_ultimate()` for longitudinal data summarisation (up-to-date mode,
  first/last mode, first/last entry).
* Added unit tests for BP functions.
* Added README.md with overview, installation instructions, quick-start
  examples, and function reference (PR #1).
* Fixed typos in roxygen2 documentation: "peadiatric" → "paediatric",
  "explicity" → "explicitly" (PR #1).

# npdar 0.1.0

* Initial package skeleton with DESCRIPTION, NAMESPACE, and LICENSE.
