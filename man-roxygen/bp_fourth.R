#' @section Height Reference Standards for the Fourth Report:
#'
#' \strong{Fourth Report Regression Model:}
#'
#' The Fourth Report uses regression models with age (centred at 10 years) and
#' height z-score polynomial terms (up to 4th degree) to calculate expected BP:
#'
#' \deqn{\mu = \alpha + \sum_{j=1}^{4} \beta_j (Age-10)^j + \sum_{k=1}^{4} \gamma_k (Z_{ht})^k}
#'
#' \strong{BMI References:}
#'
#' The Fourth Report was developed and validated using US CDC 2000 growth references
#' for height percentiles. Your height z-scores should ideally be based on US CDC reference.
#' However, UK clinical practice and NPDA audit analysis use UK-WHO growth charts as standard
#' (historically the NPDA used the British 1990), so using the UK-WHO is also considered
#' acceptable in UK contexts (based on the assumption that the two populations are similar enough).
#'
#' To our knowledge, the Fourth Report BP percentiles have not been validated
#' using UK-WHO height references. The clinical impact of applying Fourth Report regression
#' coefficients with UK-WHO (rather than US CDC) height z-scores has not been
#' formally evaluated. Users should be aware of this methodological discrepancy
#' when interpreting results or comparing to US-based implementations.
#'
#' \strong{Calculating Height Z-Scores:}
#'
#' This package requires height z-scores as input and does not calculate them
#' internally. Two approaches I recommended for obtaining height z-scores are:
#'
#' \enumerate{
#'   \item \strong{childsds R package} (\url{https://mvogel78.r-universe.dev/childsds}),
#'   which provides SDS/z-score calculations based on multiple growth standards (including UK–WHO, US CDC references).
#'
#'   \item \strong{RCPCHGrowth Python library} (\url{https://growth.rcpch.ac.uk/products/python-library/}),
#'   which is the official RCPCH implementation of UK-WHO growth charts, with API accessible from R.
#' }
#'
