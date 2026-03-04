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
#'   \item Calculate expected blood pressure, z-scores and centiles based on given reference
#'   \item Categorise blood pressure into clinical stages (e.g., Hypertension)
#' }
#'
#' \strong{Finding audit year(s):}
#' \itemize{
#'   \item Determine current NPDA audit year from dates
#'   \item Generate sequential lists of audit years
#'   \item Handle NPDA fiscal year structure (April-March)
#' }
#' \strong{Data Privacy suppression:}
#' \itemize{
#'   \item Mask small numerators to protect patient privacy
#' }
#'
#' \strong{Statistical Utilities:}
#' \itemize{
#'   \item Find up-to-date/last/first modes and entries
#'   \item Handle missing and invalid data
#' }
#'
#' @section Key Functions:
#'
#' \strong{Blood Pressure Functions:}
#' \itemize{
#'   \item \code{\link{family_BP}}
#' }
#'
#' \strong{Audit Year Functions:}
#' \itemize{
#'   \item \code{\link{family_AuditYear}}
#' }
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
