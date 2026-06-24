#' Summarise categorical measure(s) by group(s)
#'
#' For each specified grouping variable, count the frequency of each unique category
#' of the given measure column(s), and compute the denominator (non-missing
#' categories) and percentage. Results for the specified group(s) are combined into a single
#' long tibble for easy use in `ggplot` or `plotly`.
#'
#' @param data A data frame containing measure columns (and grouping columns), with one row per participant.
#' @param measures A character vector of column names to summarise. All columns
#'   will be coerced to character, so this works for logical, factor, and
#'   character columns alike.
#' @param groups A character vector of grouping column names. Defaults to
#'   \code{"overall"}, which creates a single group containing all rows.
#'   Rows where the grouping variable is \code{NA} are excluded
#'   from that group's summary.
#' @param nested Logical; if \code{FALSE} (default), each grouping variable in
#'   \code{groups} is summarised separately. If \code{TRUE}, all variables in
#'   \code{groups} are treated as a nested grouping set.
#'
#' @return A tibble in long format with one row per group level × measure × category combination.
#'
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' set.seed(1999)
#' df <- data.frame(participant_id = 1:60,
#'                  country        = c(rep("England", 30), rep("Wales", 30)),
#'                  region         = c(
#'                    rep("East England", 10), rep("West England", 10), rep(NA, 10),
#'                    rep("North Wales", 10), rep("South Wales", 10), rep(NA, 10)
#'                  ),
#'                  # Categorical Q1
#'                  q1_catq        = sample(c("A", "B", "C", NA), 60, replace = TRUE),
#'                  # Categorical Q2
#'                  q2_catq        = sample(c("A", "B", "C", "D", "E", NA), 60, replace = TRUE)
#' )
#'
#' group_cols <- c("overall", "country", "region")
#'
#' measure_cols <- df |>
#'   select(tidyselect::matches("q[0-9]+_catq")) |>
#'   names()
#'
#' sum <- get_frequency(
#'   data     = df,
#'   measures = measure_cols,
#'   groups   = group_cols
#' )
#'
#' head(sum)
#'
# sum |>
#   filter(!is.na(country)) |>
#   ggplot(aes(x = country, y = percent, fill = category)) +
#   geom_col() +
#   facet_wrap(~ measure, nrow = 1) +
#   labs(x = "Country", y = "Percent", fill = "Category")
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
#' @importFrom dplyr group_by select across filter mutate summarise ungroup bind_rows n distinct left_join if_all
#' @importFrom tidyr pivot_longer crossing
#' @importFrom rlang .data sym
#' @importFrom tidyselect all_of
#' @export
get_frequency <- function(data, measures, groups = "overall", nested = FALSE) {

  # Step 0a: Internal helper to define the full category set for each measure (even if unobserved)
  .get_measure_levels <- function(x) {
    if (is.logical(x)) {
      # Preserve both logical levels (even if unobserved)
      c("FALSE", "TRUE")
    } else if (is.factor(x)) {
      # Preserve all factor levels (even if unobserved)
      as.character(levels(x))
    } else {
      # Character / other categorical values: preserve observed non-missing values
      unique(as.character(stats::na.omit(x)))
    }
  }

  # Step 0b: Define the unique measure levels (even if unobserved)
  measure_levels <- dplyr::bind_rows(
    lapply(measures, function(m) {
      lvls <- .get_measure_levels(data[[m]])

      if (length(lvls) == 0) {
        data.frame(measure = character(0),
                   category = character(0),
                   stringsAsFactors = FALSE)
      } else {
        data.frame(measure = rep(m, length(lvls)),
                   category = lvls,
                   stringsAsFactors = FALSE)
      }
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
    if (identical(group_set, "overall")) {             # If no group specified (i.e., Overall),
      grp_data <- data |>                              # create a constant grouping column so downstream code is uniform
        dplyr::mutate(overall = "overall")

      group_levels <- data.frame(overall = "overall", stringsAsFactors = FALSE)

    } else {                                           # If group specified

      if ("overall" %in% group_set) {
        data_for_group <- data |>
          dplyr::mutate(overall = "overall")
      } else {
        data_for_group <- data
      }

      non_overall_groups <- setdiff(group_set, "overall")

      grp_data <- data_for_group                       # Exclude rows/participants with no group membership
      if (length(non_overall_groups) > 0) {
        grp_data <- grp_data |>
          dplyr::filter(dplyr::if_all(
            dplyr::all_of(non_overall_groups),
            \(x) !is.na(x)
          ))
      }

      group_levels <- grp_data |>
        dplyr::select(dplyr::all_of(group_set)) |>
        dplyr::distinct()
    }

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
      dplyr::filter(!is.na(.data$category)) |>  # Exclude NAs from denominator (i.e., treat as skipped)
      dplyr::group_by(dplyr::across(            # Count occurrences of each category within group × measure
        dplyr::all_of(c(group_set, "measure", "category")))
      ) |>
      dplyr::summarise(numerator = dplyr::n(), .groups = "drop")

    # Step 3. Build full scaffold of group × measure × category
    scaffold <- tidyr::crossing(group_levels, measure_levels)

    # Step 4. Join counts onto scaffold and fill absent categorys with 0
    results[[paste(group_set, collapse = "__")]] <- scaffold |>
      dplyr::left_join(counts, by = c(group_set, "measure", "category")) |>
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
