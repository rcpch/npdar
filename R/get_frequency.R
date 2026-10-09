#' Summarise categorical measure(s) by group(s)
#'
#' For each specified grouping variable, count the frequency of each
#' unique category of the given measure column(s), and compute the denominator
#' (non-missing categories by default) and percentage. Results for the specified group(s)
#' are combined into a single long tibble for easy use in `ggplot` or `plotly`.
#'
#' @param data A data frame containing measure columns (and grouping columns),
#' with one row per participant.
#' @param measures A character vector of column names to summarise. All columns
#'   will be coerced to character, so this works for logical, factor, and
#'   character columns alike. Measures appear in the output in this order.
#' @param groups A character vector of grouping column names. Defaults to
#'   \code{"overall"}, which creates a single group containing all rows.
#'   Rows where the grouping variable is \code{NA} are excluded
#'   from that group's summary.
#' @param nested Logical; if \code{FALSE} (default), each grouping variable in
#'   \code{groups} is summarised separately. If \code{TRUE}, all variables in
#'   \code{groups} are treated as a nested grouping set.
#' @param count_na Logical; if \code{FALSE} (default), missing values in
#'   \code{measures} are excluded from the numerator and denominator. If
#'   \code{TRUE}, missing values are counted as their own category
#'   (\code{category = NA}) and included in the denominator, but only for
#'   measures that contain at least one missing value (like
#'   \code{table(useNA = "ifany")}). Missing values in \code{groups} are
#'   always excluded.
#'
#' @return A tibble in long format with one row per group level ×
#' measure × category combination.
#'
#' @examples
#'
#' library(dplyr)
#'
#' set.seed(1999)
#' df <- data.frame(
#'   participant_id = 1:60,
#'   country        = c(rep("England", 30), rep("Wales", 30)),
#'   region         = c(
#'     rep("East England", 10), rep("West England", 10), rep(NA, 10),
#'     rep("North Wales", 10), rep("South Wales", 10), rep(NA, 10)
#'   ),
#'   # Categorical Q1
#'   q1_catq        = sample(c("A", "B", "C", NA), 60, replace = TRUE),
#'   # Categorical Q2
#'   q2_catq        = sample(c("A", "B", "C", "D", "E", NA), 60, replace = TRUE)
#' )
#'
#' group_cols <- c("overall", "country", "region")
#'
#' measure_cols <- df |>
#'   select(tidyselect::matches("q[0-9]+_catq")) |>
#'   names()
#'
#' freq <- get_frequency(
#'   data     = df,
#'   measures = measure_cols,
#'   groups   = group_cols
#' )
#'
#' head(freq)
#'
#' # Nested grouping example (region is nested within country):
#' sum_q1_nested <- get_frequency(
#'   data = df,
#'   measures = "q1_catq",
#'   groups = c("country", "region"),
#'   nested = TRUE
#' )
#'
#' sum_q1_nested
#'
#' # Count missing values as a category
#' get_frequency(
#'   data = df,
#'   measures = "q1_catq",
#'   groups = "country",
#'   count_na = TRUE
#' )
#'
#' @importFrom dplyr group_by select across filter mutate summarise ungroup bind_rows n distinct left_join if_all
#' @importFrom tidyr pivot_longer crossing
#' @importFrom rlang .data
#' @importFrom tidyselect all_of
#' @export
get_frequency <- function(data, measures, groups = "overall", nested = FALSE, count_na = FALSE) {

  # Step 0a: Internal helper to define the full category set for each measure (even if unobserved)
  .get_measure_levels <- function(x) {
    lvls <- if (is.logical(x)) {
      # Preserve both logical levels (even if unobserved)
      c("FALSE", "TRUE")
    } else if (is.factor(x)) {
      # Preserve all factor levels (even if unobserved)
      as.character(levels(x))
    } else {
      # Character / other categorical values: preserve observed non-missing values
      unique(as.character(stats::na.omit(x)))
    }

    if (isTRUE(count_na) && anyNA(x)) c(lvls, NA_character_) else lvls
  }

  # Step 0b: Define the unique measure levels (even if unobserved)
  measure_levels <- dplyr::bind_rows(
    lapply(measures, function(m) {
      lvls <- .get_measure_levels(data[[m]])

      data.frame(measure_order = rep(match(m, measures), length(lvls)),  # First column so crossing() sorts by user-specified order
                 measure = rep(m, length(lvls)),
                 category = lvls,
                 stringsAsFactors = FALSE)
    })
  )

  # Step 0c: Define whether grouping variables are separate or nested
  group_sets <- if (isTRUE(nested)) {
    list(groups)
  } else {
    as.list(groups)
  }

  results <- list()

  for (group_set in group_sets) {                      # Get summary by each group

    group_set <- unlist(group_set)

    # Step 1. Group data
    grp_data <- data
    if ("overall" %in% group_set) {                    # "overall" is a constant grouping column so downstream code is uniform
      grp_data <- dplyr::mutate(grp_data, overall = "overall")
    }

    grp_data <- grp_data |>                            # Exclude rows/participants with no group membership
      dplyr::filter(dplyr::if_all(
        dplyr::all_of(setdiff(group_set, "overall")),
        \(x) !is.na(x)
      ))

    group_levels <- grp_data |>
      dplyr::select(dplyr::all_of(group_set)) |>
      dplyr::distinct()

    # Step 2. Count observed categories
    counts <- grp_data |>
      dplyr::mutate(dplyr::across(              # Coerce to character so logical, factor, and character cols are handled uniformly by pivot_longer
        dplyr::all_of(measures), as.character)
      ) |>
      tidyr::pivot_longer(                      # Reshape: one row per respondent × measure
        cols = tidyselect::all_of(measures),
        names_to = "measure",
        values_to = "category"
      ) |>
      dplyr::filter(isTRUE(count_na) | !is.na(.data$category)) |>  # Unless count_na, exclude NAs from denominator (i.e., treat as skipped)
      dplyr::group_by(dplyr::across(            # Count occurrences of each category within group × measure
        dplyr::all_of(c(group_set, "measure", "category")))
      ) |>
      dplyr::summarise(numerator = dplyr::n(), .groups = "drop")

    # Step 3. Build full scaffold of group × measure × category
    scaffold <- tidyr::crossing(group_levels, measure_levels)

    # Step 4. Join counts onto scaffold and fill absent categorys with 0
    results[[paste(group_set, collapse = "__")]] <- scaffold |>
      dplyr::left_join(counts, by = c(group_set, "measure", "category")) |>
      dplyr::select(-"measure_order") |>
      dplyr::mutate(
        numerator = ifelse(is.na(.data$numerator), 0, .data$numerator)
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_set, "measure")))) |>
      dplyr::mutate(                            # Compute denominator and percentage within group × measure
        denominator = sum(.data$numerator),
        percent = ifelse(.data$denominator > 0, .data$numerator / .data$denominator, NA_real_)
      ) |>
      dplyr::ungroup()
  }

  # Step 5. Combine all groups
  dplyr::bind_rows(results) |>
    dplyr::select(
      dplyr::all_of(c(groups, "measure", "category", "numerator", "denominator", "percent"))
    )
}
