# Functions for Audit Year(s) Determination

Functions to determine current audit year and generate sequential lists
of audit years following NPDA fiscal year structure or your defined
conventions.

Audit years are formatted as "YYYY/YY" (e.g., "2024/25").

**Available Functions:**

- [`get_auditYear`](https://rcpch.github.io/npdar/reference/get_auditYear.md):
  Determine audit year & quarter from date(s)

- [`get_auditYears`](https://rcpch.github.io/npdar/reference/get_auditYears.md):
  Generate sequence of audit years

## Audit Year Structure

The NPDA uses a fiscal year structure running from 1 April to 31 March,
divided into four quarters:

|                   |                   |                   |                   |
|-------------------|-------------------|-------------------|-------------------|
| **Quarter**       | **Start date**    | **End date**      | Q1                |
| 01/04/CurrentYear | 30/06/CurrentYear | Q2                | 01/07/CurrentYear |
| 30/09/CurrentYear | Q3                | 01/10/CurrentYear | 31/12/CurrentYear |
| Q4                | 01/01/NextYear    | 31/03/NextYear    |                   |

For example:

- A date of 15 May 2025: 2025/26 (Q1)

- A date of 15 March 2026: 2025/26 (Q4)

- A date of 15 April 2026: 2025/26 (Q1)

## Key Features

- Fully vectorised - works with single dates or date vectors

- Integrates seamlessly with dplyr pipelines

- Handles edge cases (leap years, century boundaries)

- Optional integer or formatted string output

## See also

Other auditYear functions:
[`get_auditYear()`](https://rcpch.github.io/npdar/reference/get_auditYear.md),
[`get_auditYears()`](https://rcpch.github.io/npdar/reference/get_auditYears.md)
