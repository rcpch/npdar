# npdar: National Paediatric Diabetes Audit R Package

The npdar package provides tools for analysing National Paediatric
Diabetes Audit (NPDA) data.

## Main Features

**Blood Pressure Assessment:**

- [`bp_family`](https://rcpch.github.io/npdar/reference/bp_family.md):

  - Calculate expected blood pressure, z-scores and centiles based on
    given reference.

  - Categorise blood pressure into clinical stages (e.g., Hypertension).

**Statistical Utilities:**

- [`get_masked`](https://rcpch.github.io/npdar/reference/get_masked.md):

  - Mask small numerators to protect patient data privacy.

&nbsp;

- [`get_ultimate`](https://rcpch.github.io/npdar/reference/get_ultimate.md):

  - Find up-to-date/last/first modes and entries.

  - Handle missing and invalid data.

&nbsp;

- [`get_frequency`](https://rcpch.github.io/npdar/reference/get_frequency.md):

  - Summarise categorical measure(s) by group(s), returning count,
    denominator, and percentage in long format for easy use in
    \`ggplot2\` or \`plotly\`.

**Finding Audit Year(s):**

- [`audit_year_family`](https://rcpch.github.io/npdar/reference/audit_year_family.md):

  - Determine current audit year from (specified) dates, based on custom
    fiscal year structure.

  - Generate sequential lists of audit years.

**Creating interactive plots with plotly:**

- [`plot_corrmat_ly`](https://rcpch.github.io/npdar/reference/plot_corrmat_ly.md):

  - Draws an interactive correlation matrix heatmap plot using plotly.

  - Compute Pearson or Spearman correlation matrices for numeric
    variables.

  - Optionally show correlation values and statistical significance
    stars.

  - Supports missing-data handling via
    [`stats::cor()`](https://rdrr.io/r/stats/cor.html) options.

&nbsp;

- [`plot_raincloud_ly`](https://rcpch.github.io/npdar/reference/plot_raincloud_ly.md):

  - Draws an interactive raincloud plot using plotly.

  - Options to show or hide each of half-violin density plots, boxplots,
    and scatter plots of the raw values.

## See also

Useful links:

- Report bugs: <https://github.com/RCPCH/npdar/issues>

- RCPCH GitHub: <https://github.com/RCPCH>

## Author

- Zhaonan Fang (Author, Maintainer) <Zhaonan.Fang@rcpch.ac.uk>

- Amani Krayem (Contributor) <Amani.Krayem@rcpch.ac.uk>

- Humfrey Legge (Contributor) <Humfrey.Legge@rcpch.ac.uk>

- Saira Pons Perez (Contributor) <Saira.PonsPerez@rcpch.ac.uk>
