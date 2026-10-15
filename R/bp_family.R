#' Blood Pressure Assessment Functions for Paediatric and Adult Populations
#'
#' @description
#' This module provides functions to assess blood pressure (BP) in children,
#' young people, and adults using age-appropriate clinical guidelines, currently
#' the package uses:
#'
#' \itemize{
#'   \item Children/adolescents (0-17 years): NHBPEP Fourth Report
#'   \item Young adults (>17 years): NICE/BHF guidelines
#' }
#'
#' @details
#' The functions implement clinical BP categorisation following:
#' \itemize{
#'   \item \strong{NHBPEP Fourth Report}: Sex-, age-, and height-specific percentiles for paediatric BP
#'   \item \strong{NICE/BHF Guidelines}: Fixed thresholds for adult hypertension stages
#'   \item \strong{NPDA Limits}: National Paediatric Diabetes Audit validity ranges (50/15-200/150 mmHg)
#'   \item \strong{NDA Limits}: National Diabetes Audit validity ranges (70/20-300/150 mmHg)
#' }
#'
#' @template bp_categories
#'
#' @template bp_fourth
#'
#' @template bp_refs
#' @family BP functions
#' @name bp_family
#' @aliases BloodPressure
NULL

# ========== 0. Blood Pressure Regression Coefficients from the NHBPEP Fourth Report (Appendix Table B-1) ==========
#' Blood Pressure Regression Coefficients from the NHBPEP Fourth Report (Appendix Table B-1)
#'
#' @description Internal coefficient lists to compute expected systolic/diastolic BP (mean, \eqn{\mu})
#' and standard deviations (\eqn{\sigma}) for children and adolescents (0-17 years),
#' stratified by sex, as described in the NHBPEP Fourth Report.
#'
#' @details
#' These coefficients are used by \code{bp_expected()}, \code{bp_relative()},
#' and \code{bp_category()} when \code{ref = "Fourth Report"}.
#' The Fourth Report uses regression models with age (centred at 10 years) and
#' height z-score polynomial terms (up to 4th degree) to calculate expected BP:
#' \deqn{\mu = \alpha + \sum_{i=1}^{4} \beta_i (Age-10)^i + \sum_{i=1}^{4} \gamma_i (Zht)^i}
#'
#' @keywords internal
#' @noRd
#' @family BP functions
.coefs_BPFourth <- list(
  systolic = list(
    male = list(intercept = 102.19768,                                  # alpha
                age_coef = c(1.82416, 0.12776, 0.00249, -0.00135),      # beta1, beta2, beta3, beta4
                height_coef = c(2.73157, -0.19618, -0.04659, 0.00947),  # gamma1, gamma2, gamma3, gamma4
                std = 10.7128),                                         # sigma
    female = list(intercept = 102.01027,
                  age_coef = c(1.94397, 0.00598, -0.00789, -0.00059),
                  height_coef = c(2.03526, 0.02534, -0.01884, 0.00121),
                  std = 10.4855)
  ),
  diastolic = list(
    male = list(intercept = 61.01217,
                age_coef = c(0.68314, -0.09835, 0.01711, 0.00045),
                height_coef = c(1.46993, -0.07849, -0.03144, 0.00967),
                std = 11.6032),
    female = list(intercept = 60.50510,
                  age_coef = c(1.01301, 0.01157, 0.00424, -0.00137),
                  height_coef = c(1.16641, 0.12795, -0.03869, -0.00079),
                  std = 10.9573)
  )
)

# ========== 1a. (Internal Helper) Function to Validate Demographic Inputs for BP Functions ==========
#' Validate Demographic Inputs for BP Functions (Internal)
#'
#' @description Internal helper function to validate and standardize demographic inputs
#' for blood pressure calculations. Checks age ranges, sex coding, and height
#' z-scores against specified limits.
#'
#' @param .quiet Logical; suppress validation messages (default \code{FALSE}).
#' @param bp_type Character; \code{"systolic"} or \code{"diastolic"}.
#' @param sex Vector of sex codes in your data. Will be mapped to \code{"male"} or \code{"female"}
#'   using \code{male_code} and \code{female_code}. Values not matching these codes become \code{NA}.
#' @param male_code,female_code Scalar values that identify the male and female codes in \code{sex}
#'   (e.g., \code{1} and \code{2}, or \code{"M"} and \code{"F"}).
#' @param height_z Numeric vector of height z-scores. Must already be z-transformed (e.g., US CDC, UK-WHO).
#' @param height_limit Positive numeric; z-scores with \code{|z| > height_limit} are treated as invalid
#'   and set to \code{NA}. Default \code{5}.
#' @param age_years Numeric vector of ages in years.
#' @param ref Reference: \code{"Fourth Report"} (children/adolescents, 0-17) or \code{"NICE/BHF"} (adults, >17).
#' @param bp_value Optional numeric vector of observed BP (mmHg); only used here to check that its length matches the other inputs.
#'
#' @return A list with normalized elements: \code{bp_type}, \code{sex} (\code{"male"} or \code{"female"} or \code{NA}),
#'   \code{age_years} (invalid ages set to \code{NA} given \code{ref}), \code{height_z} (out-of-bound z set to \code{NA}),
#'   \code{ref}, and \code{bp_value} (\code{NULL} if not supplied). It is an error if \code{sex}, \code{age_years},
#'   \code{height_z} and \code{bp_value} (when supplied) differ in length.
#'
#' @keywords internal
#'
#' @family BP functions
.bp_validate_demo_input <- function(bp_type = c("systolic", "diastolic"),
                                    sex, male_code, female_code,
                                    height_z, height_limit=5,
                                    age_years, ref = c("Fourth Report", "NICE/BHF"),
                                    bp_value = NULL,
                                    .quiet = FALSE) {                           # Quiet validation message when being called multiple times (i.e. aim to print message only once)
  # 1. Input validation
  ## 1.1 Reference
  if (missing(ref)) {                                                           # Reminder to specify as match.arg() use first option as default
    stop("'ref' must be specified.")
  }
  ref <- match.arg(ref)                                                         # Partial match (e.g. "NICE" -> "NICE/BHF")

  ## 1.2 Blood Pressure
  if (missing(bp_type)) {
    stop("'bp_type' must be specified.")
  }
  bp_type <- match.arg(bp_type)                                                 # Partial match (e.g. "sys" -> "systolic")

  ## 1.3 Sex
  sex <- as.character(factor(sex,                                               # Vectorized sex mapping (e.g. 1/m/boy -> male, 2/m/girl -> female)
                             levels = c(male_code, female_code),
                             labels = c("male", "female")))

  ## 1.4 Age
  if (ref=="NICE/BHF") {                                                        # For NPDA, for those >17 years, do not calculate anything later unless we specify adult NICE/BHF guidelines.
    age_invalid <- purrr::map_lgl(age_years, function(n) n<=17)                 # Capture invalid age (outside >17) that will be excluded
    if (!.quiet && any(age_invalid, na.rm=TRUE)) {
      message("Ignore ", sum(age_invalid, na.rm = TRUE), " age(s) \u2264 17 years. NPDA only uses NICE/BHF reference for (young) adults >17 years.")
    }
  } else if (ref=="Fourth Report") {
    age_invalid <- purrr::map_lgl(age_years, function(n) n>17 || n<0)           # Capture invalid age (outside 0-17) that will be excluded
    if (!.quiet && any(age_invalid, na.rm=TRUE)) {
      message("Ignore ", sum(age_invalid, na.rm = TRUE), " age(s) outside 0-17 years. NHBPEP Fourth Report is only designed for children <= 17 years.")
    }
  } # (FUTURE ADD NEW REF IF AVAILABLE)

  age_years <- ifelse(!age_invalid, age_years, NA)                              # Keep age only if within age range for that reference (e.g., 0-17, >17)

  ## 1.5 Height
  height_invalid <- purrr::map_lgl(height_z, function(n) abs(n) > height_limit) # Capture invalid height beyond a certain SD (default is +-5) that we won't use to calculate expected BP later
  if (!.quiet && any(height_invalid, na.rm=TRUE)) {
    message("Ignore ", sum(height_invalid, na.rm = TRUE), " height(s) beyond \u00b1", height_limit, "SD based on 'height_limit'. Make sure heights have been z-transformed / rare cases (dwarfism/gigantism) exist in your dataset.")
  }

  height_z <- ifelse(!height_invalid, height_z, NA)                             # Keep height only if within a certain SD (default is +-5)

  ## 1.6 Lengths
  inputs <- list(sex = sex, age_years = age_years, height_z = height_z)
  if (!is.null(bp_value)) inputs$bp_value <- bp_value
  lens <- lengths(inputs)
  if (length(unique(lens)) > 1) {
    stop("Inputs must all have the same length. Length of ", paste0(names(inputs), ": ", lens, collapse = ", "), ".")
  }

  return(mget(c("bp_type", "sex", "age_years", "height_z", "ref", "bp_value")))
}

# ========== 1b. (Internal Helper) Messages ==========
#' Print BP Messages (Internal)
#'
#' @description Internal helper function to print informative BP messages
#' based on the reference and inputs.
#'
#' @param valid List returned by \code{.bp_validate_demo_input()}.
#' @param what Any of \code{"expected"}, \code{"relative"}, \code{"category"}: the outputs requested.
#' @param bp_limit The \code{bp_limit} supplied by the user; the default value triggers the limit message.
#' @param .quiet Logical; suppress all messages.
#'
#' @keywords internal
#'
#' @family BP functions
.bp_message <- function(valid, what, bp_limit = c(-Inf, Inf), .quiet = FALSE) {
  if (.quiet) return(invisible(NULL))
  ref_nice <- valid$ref == "NICE/BHF"

  skipped <- c(if ("expected" %in% what) "expected BP",
               if ("relative" %in% what) "zscore/percentile BP")
  if (ref_nice && length(skipped) > 0) {
    message("NICE/BHF only provides category guidelines, ", paste(skipped, collapse = " and "), " not calculated.")
  }

  if ("category" %in% what) {
    if (ref_nice) {
      message("NICE/BHF categorise 'Normotension' to 'Stage 3 hypertension'.")
    } else {
      message("NHBPEP Fourth Report categorise 'Normotension' to 'Stage 2 hypertension'; Special rules for newborns (<1), children (0-12), adolescents (12-17).")
    } # (FUTURE ADD NEW REF IF AVAILABLE)

    if (identical(bp_limit, c(-Inf, Inf))) {
      if (ref_nice) { # (FUTURE ADD NEW ADULT REF IF AVAILABLE)
        message("BP outside National Diabetes Audit (NDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.")
      } else {
        message("BP outside National Paediatric Diabetes Audit (NPDA) limit removed unless acceptable range is explicitly specified by 'bp_limit'.")
      }
    }
  }
  invisible(NULL)
}

# ========== 1c. (Internal Helper) Default BP Limits ==========
#' Resolve BP Limit (Internal)
#'
#' @description Internal helper function to returns \code{bp_limit} unless
#' it is the default \code{c(-Inf, Inf)}, in which case the NPDA (Fourth Report)
#' or NDA (NICE/BHF) range for \code{valid$bp_type} is used.
#'
#' @keywords internal
#'
#' @family BP functions
.bp_resolve_limit <- function(valid, bp_limit) {
  if (!identical(bp_limit, c(-Inf, Inf))) return(bp_limit)
  defaults <- list(
    "NICE/BHF"      = list(systolic = c(70, 300), diastolic = c(20, 150)),
    "Fourth Report" = list(systolic = c(50, 200), diastolic = c(15, 150))
  )
  defaults[[valid$ref]][[valid$bp_type]]
}

# ========== 2. Compute Expected Blood Pressure (mu) ==========
#' Compute Expected Blood Pressure (\eqn{\mu})
#'
#' @description Returns the expected blood pressure (mean/50th percentile) based on CYP sex, age, and height z-score.
#' Currently, only NHBPEP Fourth Report regression models are available for use for children and adolescents (0-17 years).
#' For \code{ref = "NICE/BHF"} or people >17 years old, expected BP is not defined and \code{NA} is returned.
#'
#' @param .quiet Logical; suppress validation messages (default \code{FALSE}).
#' @inheritDotParams .bp_validate_demo_input
#'
#' @template bp_fourth
#'
#' @return A numeric vector of expected BP values in mmHg (\eqn{\mu}). Returns \code{NA} for:
#'   \itemize{
#'     \item NICE/BHF reference (adults)
#'     \item Ages outside reference range
#'     \item Missing or invalid sex
#'     \item Extreme height z-scores
#'   }
#'
#' @template bp_refs
#'
#' @examples
#' # Expected systolic BP for a 10-year-old boy at median height, and other some other children:
#' bp_expected(
#'   bp_type = "systolic",
#'   sex = c(1, 2, 1),
#'   male_code = 1, female_code = 2,
#'   height_z = c(0, 0, 1.5),
#'   age_years = c(10, 12, 8),
#'   ref = "Fourth Report"
#' )
#'
#' # Adults: returns NA with an informative message
#' bp_expected(
#'   bp_type = "diastolic",
#'   sex = "M", male_code = "M", female_code = "F",
#'   height_z = 0, age_years = 25, ref = "NICE/BHF"
#' )
#'
#' @family BP functions
#'
#' @export
#' @importFrom purrr pmap_dbl map_lgl
bp_expected <- function(..., .quiet = FALSE){
  valid <- .bp_validate_demo_input(..., .quiet = .quiet)
  .bp_message(valid, "expected", .quiet = .quiet)
  return(.calc_expected(valid))
}

#' Expected BP from validated inputs (Internal)
#' @param valid List returned by \code{.bp_validate_demo_input()}.
#' @keywords internal
#' @noRd
#' @importFrom purrr pmap_dbl
.calc_expected <- function(valid) {
  # 2. Calculate expected BP (mu)
  if (valid$ref == "NICE/BHF") {
    mu <- rep(NA_real_, length(valid$age_years))

  } else if (valid$ref == "Fourth Report") {
    mu <- purrr::pmap_dbl(list(valid$sex, valid$age_years, valid$height_z), function(sexBinary, ageY, heightZ) {
      ageC <- ageY - 10                                                         # Center age to 10
      age_terms <- c(ageC, ageC^2, ageC^3, ageC^4)                              # Age terms: (Age-10), (Age-10)^2, (Age-10)^3, (Age-10)^4
      height_terms <- c(heightZ, heightZ^2, heightZ^3, heightZ^4)               # Height terms: Zht, Zht^2, Zht^3, Zht^4

      # Calculate mu
      if (is.na(sexBinary)) {                                                   # If binary sex not spec (i.e., not non-binary, other, NA, etc.)
        NA                                                                      # Don't calculate mu, return NA
      } else {
        coefs <- .coefs_BPFourth[[valid$bp_type]][[sexBinary]]                  # Get coefs from NHBPEP Fourth refs according to bp_type and sex

        coefs$intercept +                                                       # Calculate mu
          sum(coefs$age_coef * age_terms) +
          sum(coefs$height_coef * height_terms)
      }
    })
  } # (FUTURE ADD NEW REF IF AVAILABLE)

  return(mu)
}

# ========== 3. Compute Blood Pressure Z-score and Percentile ==========
#' Compute Blood Pressure Z-score and Percentile
#'
#' @description Computes z-scores and percentiles for observed BP values against expected values
#' (\eqn{\mu, \sigma}) from a specified reference (currently only NHBPEP Fourth Report is available). For \code{ref = "NICE/BHF"},
#' z-scores are not defined and \code{NA} is returned with a message.
#'
#' @param .quiet Logical; suppress validation messages (default \code{FALSE}).
#' @param bp_value Numeric vector of observed BP values (mmHg).
#' @inheritDotParams .bp_validate_demo_input
#'
#' @return A tibble with two columns: \code{zscore} and \code{percentile} (0-100).
#'   \code{NA} where expected BP could not be computed (e.g., invalid inputs or adult reference).
#'
#' @template bp_refs
#'
#' @examples
#' # Z-score and percentile for systolic BP in children using Fourth Report:
#' bp_relative(
#'   bp_value = c(95, 110, 130),
#'   bp_type = "systolic",
#'   sex = c("M","F","M"),
#'   male_code = "M", female_code = "F",
#'   height_z = c(0, 0.5, -1),
#'   age_years = c(8, 12, 16),
#'   ref = "Fourth Report"
#' )
#'
#' @family BP functions
#'
#' @export
#' @importFrom purrr pmap_dbl pmap_chr map_lgl
#' @importFrom tibble tibble
#' @importFrom stats pnorm
bp_relative <- function(bp_value, ..., .quiet=FALSE) {
  valid <- .bp_validate_demo_input(bp_value = bp_value, ..., .quiet = .quiet)
  .bp_message(valid, "relative", .quiet = .quiet)
  return(.calc_relative(valid, .calc_expected(valid)))
}

#' Z-score and percentile from validated inputs (Internal)
#' @param valid List returned by \code{.bp_validate_demo_input()} with \code{bp_value}.
#' @param mu Expected BP from \code{.calc_expected()}.
#' @keywords internal
#' @noRd
#' @importFrom purrr pmap_dbl
#' @importFrom tibble tibble
#' @importFrom stats pnorm
.calc_relative <- function(valid, mu) {
  # 3. Calculate z-score and percentile
  ## 3.1 Calculate z-score
  if (valid$ref == "NICE/BHF") {
    zscore <- rep(NA_real_, length(mu))
  } else if (valid$ref == "Fourth Report") {
    zscore <- purrr::pmap_dbl(list(valid$bp_value, mu, valid$sex), function(bpObserved, bpExpected, sexBinary) {
      if (is.na(bpExpected)) {                                                  # If expected BP wasn't calculated
        NA                                                                      # Don't calculate z-score, return NA
      } else {
        coefs <- .coefs_BPFourth[[valid$bp_type]][[sexBinary]]                  # Get coefs from NHBPEP Fourth refs according to bp_type and sex
        (bpObserved - bpExpected) / coefs$std                                   # Calculate z-score
      }
    })
  } # (FUTURE ADD NEW REF IF AVAILABLE)

  ## 3.2 Calculate percentile
  percentile <- stats::pnorm(zscore) * 100

  # return(mget(c("zscore", "percentile")))
  # return a tibble that allows mutate() to unpack automatically into new columns
  return(tibble::tibble(zscore, percentile))
}

# ========== 4. Categorise Blood Pressure (Children/Adolescents and Young Adults) ==========
#' Categorise Blood Pressure (Children/Adolescents and Young Adults)
#'
#' @description Categorises observed BP values into guideline-based categories. For ages 0-17,
#' currently NHBPEP Fourth Report thresholds are applied (with newborn and adolescent rules).
#' For >17, NICE/BHF adult thresholds are applied.
#'
#' Unless \code{bp_limit} is explicitly provided, implausible values are excluded using:
#' \itemize{
#'   \item NPDA limit (children/adolescents): SBP [50-200] mmHg, DBP [15-150] mmHg
#'   \item NDA limit (young adults): SBP [70-300] mmHg, DBP [20-150] mmHg
#' }
#'
#' @param bp_value Numeric vector of observed BP values (mmHg).
#' @param bp_limit Length-2 numeric vector \code{c(min, max)} giving acceptable range.
#'   Defaults to \code{c(-Inf, Inf)}; if left as-is, NPDA/NDA defaults are applied based on \code{ref}.
#' @param .quiet Logical; suppress messages (default \code{FALSE}).
#' @inheritDotParams .bp_validate_demo_input
#' @inheritDotParams bp_expected
#'
#' @return Character vector of BP categories:
#'   \emph{"Normotension"}, \emph{"Prehypertension"}, \emph{"Stage 1 hypertension"},
#'   \emph{"Stage 2 hypertension"}, or \emph{"Stage 3 hypertension"} (adults only).
#'   \code{NA} where inputs are invalid or beyond specified \code{bp_limit}.
#'
#' @template bp_categories
#'
#' @template bp_refs
#'
#' @examples
#' # Child/adolescent categorisation (Fourth Report):
#' bp_category(
#'   bp_value = c(95, 118, 130, 150),
#'   bp_type = "systolic",
#'   sex = c(1, 2, 1, 2),
#'   male_code = 1, female_code = 2,
#'   height_z = c(0, 0, 0, 0),
#'   age_years = c(8, 14, 16, 6),
#'   ref = "Fourth Report"
#' )
#'
#' # Adult categorisation (NICE/BHF; uses NDA limits if bp_limit not provided):
#' bp_category(
#'   bp_value = c(118, 135, 162, 185),
#'   bp_type = "diastolic",
#'   sex = rep("M", 4), male_code = "M", female_code = "F",
#'   height_z = rep(0, 4),
#'   age_years = c(22, 30, 45, 67),
#'   ref = "NICE/BHF"
#' )
#'
#' @family BP functions
#'
#' @export
#' @importFrom purrr pmap_chr pmap_dbl map_lgl
#' @importFrom stats qnorm
bp_category <- function(bp_value, bp_limit = c(-Inf, Inf), ..., .quiet = FALSE) {
  valid <- .bp_validate_demo_input(bp_value = bp_value, ..., .quiet = .quiet)
  .bp_message(valid, "category", bp_limit, .quiet = .quiet)
  return(.calc_category(valid, .calc_expected(valid), .bp_resolve_limit(valid, bp_limit)))
}

#' Categorise BP from validated inputs (Internal)
#' @param valid List returned by \code{.bp_validate_demo_input()} with \code{bp_value}.
#' @param mu Expected BP from \code{.calc_expected()}.
#' @param bp_limit Resolved acceptable range from \code{.bp_resolve_limit()}.
#' @keywords internal
#' @noRd
#' @importFrom purrr pmap_chr
#' @importFrom stats qnorm
.calc_category <- function(valid, mu, bp_limit) {
  bp_value <- valid$bp_value

  # 4. Categorise BP
  if (valid$ref == "NICE/BHF") {
    # 4.1 categorisation
    category <- purrr::pmap_chr(list(bp_value, valid$age_years), function(bpObserved, ageY) {
      if (is.na(ageY) || is.na(bpObserved)) return(NA)                            # This exclude anyone with invalid age//bp_value
      if (bpObserved < bp_limit[1]) return(NA)                                    # Below the plausible range

      if (valid$bp_type == "systolic") {
        if (bpObserved < 120) return("Normotension")
        else if (bpObserved < 140) return("Prehypertension")
        else if (bpObserved < 160) return("Stage 1 hypertension")
        else if (bpObserved < 180) return("Stage 2 hypertension")
        else if (bpObserved <= bp_limit[2]) return("Stage 3 hypertension")
        else {NA}
      } else if (valid$bp_type == "diastolic") {
        if (bpObserved < 80) return("Normotension")
        else if (bpObserved < 90) return("Prehypertension")
        else if (bpObserved < 100) return("Stage 1 hypertension")
        else if (bpObserved < 120) return("Stage 2 hypertension")
        else if (bpObserved <= bp_limit[2]) return("Stage 3 hypertension")
        else {NA}
      }
    })
  } else if (valid$ref == "Fourth Report") {
    # 4.2 categorisation
    category <- purrr::pmap_chr(list(bp_value, mu, valid$age_years, valid$sex), function(bpObserved, bpExpected, ageY, sexBinary) {
      if (is.na(bpExpected) || is.na(bpObserved)) return(NA)                   # This exclude anyone with invalid age/sex/height/bp_value
      if (bpObserved < bp_limit[1]) return(NA)                                 # Below the plausible range

      coefs <- .coefs_BPFourth[[valid$bp_type]][[sexBinary]]                  # Get coefs from NHBPEP Fourth refs according to bp_type and sex
      bp90Centile <- stats::qnorm(0.90, mean = bpExpected, sd = coefs$std)           # Calculate values based on percentiles
      bp95Centile <- stats::qnorm(0.95, mean = bpExpected, sd = coefs$std)
      bp99Centile_plus5 <- stats::qnorm(0.99, mean = bpExpected, sd = coefs$std) + 5

      if (ageY <= 12 && bpObserved < bp90Centile) return("Normotension")
      else if (ageY > 12 && bpObserved < 120 && valid$bp_type == "systolic") return("Normotension")
      else if (ageY > 12 && bpObserved < 80 && valid$bp_type == "diastolic") return("Normotension")
      else if (bpObserved < bp95Centile) return("Prehypertension")
      else if (ageY < 1 && bpObserved < bp99Centile_plus5 && valid$bp_type == "systolic") return("Stage 1 hypertension")
      else if (ageY >= 1 && bpObserved < bp99Centile_plus5) return("Stage 1 hypertension")
      else if (ageY < 1 && bpObserved <= bp_limit[2] && valid$bp_type == "systolic") return("Stage 2 hypertension")
      else if (ageY >= 1 && bpObserved <= bp_limit[2]) return("Stage 2 hypertension")
      else {NA}
    })
  } # (FUTURE ADD NEW REF IF AVAILABLE)
  return(category)
}

# ========== 5. Master Wrapper for Blood Pressure Utilities ==========
#' Master Wrapper for Blood Pressure Utilities
#'
#' @description Runs \code{\link{bp_expected}}, \code{\link{bp_relative}} and \code{\link{bp_category}}
#' in one call. Inputs are validated once and the expected BP is computed once, so messages
#' are printed once and the results are consistent with each other.
#'
#' @param bp_value Numeric vector of observed BP values (mmHg).
#' @param bp_limit Length-2 numeric vector \code{c(min, max)} giving acceptable range used for
#'   categorisation. See \code{\link{bp_category}}.
#' @param .quiet Logical; suppress messages (default \code{FALSE}).
#' @inheritDotParams .bp_validate_demo_input -bp_value
#'
#' @return A tibble with one row per input and columns \code{expected} (\eqn{\mu}, mmHg),
#'   \code{zscore}, \code{percentile} (0-100) and \code{category}. Values that cannot be
#'   computed are \code{NA}; for \code{ref = "NICE/BHF"} only \code{category} is defined.
#'
#' @examples
#' # One-stop interface:
#' bp_assess(
#'   bp_value = c(115, 142),
#'   bp_type = "systolic",
#'   sex = c("M","F"), male_code = "M", female_code = "F",
#'   height_z = c(0, 0.2),
#'   age_years = c(13, 16),
#'   ref = "Fourth Report"
#' )
#'
#' @family BP functions
#' @export
#' @importFrom tibble tibble
bp_assess <- function(bp_value, bp_limit = c(-Inf, Inf), ..., .quiet = FALSE) {
  valid <- .bp_validate_demo_input(bp_value = bp_value, ..., .quiet = .quiet)
  .bp_message(valid, c("expected", "relative", "category"), bp_limit, .quiet = .quiet)

  mu <- .calc_expected(valid)
  relative <- .calc_relative(valid, mu)
  category <- .calc_category(valid, mu, .bp_resolve_limit(valid, bp_limit))

  tibble::tibble(expected = mu, zscore = relative$zscore, percentile = relative$percentile, category = category)
}

