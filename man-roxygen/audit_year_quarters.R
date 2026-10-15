#' @section Audit Year Structure:
#' The NPDA uses a fiscal year structure running from 1 April to 31 March,
#' divided into four quarters:
#' \tabular{lllc}{
#'   \strong{Quarter} \tab \strong{Start date} \tab \strong{End date} \cr
#'   Q1 \tab 01/04/CurrentYear \tab 30/06/CurrentYear \cr
#'   Q2 \tab 01/07/CurrentYear \tab 30/09/CurrentYear \cr
#'   Q3 \tab 01/10/CurrentYear \tab 31/12/CurrentYear \cr
#'   Q4 \tab 01/01/NextYear    \tab 31/03/NextYear    \cr
#' }
#'
#' For example:
#' \itemize{
#'   \item A date of 15 May 2025: 2025/26 (Q1)
#'   \item A date of 15 March 2026: 2025/26 (Q4)
#'   \item A date of 15 April 2026: 2025/26 (Q1)
#' }
