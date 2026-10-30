#' npdar: National Paediatric Diabetes Audit R Package
#'
#' @description
#' The npdar package provides tools for analysing
#' National Paediatric Diabetes Audit (NPDA) data.
#'
#' @section Main Features:
#'
#' \strong{Blood Pressure Assessment:}
#' \itemize{
#'   \item \code{\link{bp_family}}:
#'   \itemize{
#'     \item Calculate expected blood pressure, z-scores and centiles based on given reference.
#'     \item Categorise blood pressure into clinical stages (e.g., Hypertension).
#' }}
#'
#' \strong{Statistical Utilities:}
#' \itemize{
#'   \item \code{\link{get_masked}}:
#'   \itemize{
#'     \item Mask small numerators to protect patient data privacy.
#' }}
#'
#' \itemize{
#'   \item \code{\link{get_ultimate}}:
#'   \itemize{
#'     \item Find up-to-date/last/first modes and entries.
#'     \item Handle missing and invalid data.
#' }}
#'
#' \itemize{
#'   \item \code{\link{get_frequency}}:
#'   \itemize{
#'     \item Summarise categorical measure(s) by group(s), returning count,
#'     denominator, and percentage in long format for easy use in `ggplot2` or `plotly`.
#' }}
#'
#' \strong{Finding Audit Year(s):}
#' \itemize{
#'   \item \code{\link{audit_year_family}}:
#'   \itemize{
#'     \item Determine current audit year from (specified) dates,
#'     based on custom fiscal year structure.
#'     \item Generate sequential lists of audit years.
#' }}
#'
#' \strong{Creating interactive plots with plotly:}
#' \itemize{
#'   \item \code{\link{plot_corrmat_ly}}:
#'   \itemize{
#'     \item Draws an interactive correlation matrix heatmap plot using \pkg{plotly}.
#'     \item Compute Pearson or Spearman correlation matrices for numeric variables.
#'     \item Optionally show correlation values and statistical significance stars.
#'     \item Supports missing-data handling via \code{stats::cor()} options.
#' }}
#'
#' \itemize{
#'   \item \code{\link{plot_raincloud_ly}}:
#'   \itemize{
#'     \item Draws an interactive raincloud plot using \pkg{plotly}.
#'     \item Options to show or hide each of half-violin density plots,
#'     boxplots, and scatter plots of the raw values.
#' }}
#'
#' @author
#' \itemize{
#'   \item Zhaonan Fang (Author, Maintainer) \email{Zhaonan.Fang@rcpch.ac.uk}
#'   \item Amani Krayem (Contributor) \email{Amani.Krayem@rcpch.ac.uk}
#'   \item Humfrey Legge (Contributor) \email{Humfrey.Legge@rcpch.ac.uk}
#'   \item Saira Pons Perez (Contributor) \email{Saira.PonsPerez@rcpch.ac.uk}
#' }
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item Report bugs: \url{https://github.com/RCPCH/npdar/issues}
#'   \item RCPCH GitHub: \url{https://github.com/RCPCH}
#' }
#'
#' @keywords internal
"_PACKAGE"
