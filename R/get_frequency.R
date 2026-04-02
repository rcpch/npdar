#' Summarise categorical measure(s) by group(s)
#'
#' For each specified grouping variable, count the frequency of each response
#' option of the given measure column(s), and compute the denominator (non-missing
#' responses) and percentage. Results for the specified group(s) are combined into a single
#' long tibble.
#'
#' @param data A data frame containing measure columns (and grouping columns), with one row per participant.
#' @param measures A character vector of column names to summarise. All columns
#'   will be coerced to character, so this works for logical, factor, and
#'   character columns alike.
#' @param groups A character vector of grouping column names. Defaults to
#'   \code{"overall"}, which creates a single group containing all rows.
#'   Rows where the grouping variable is \code{NA} are excluded
#'   from that group's summary.
#'
#' @return A tibble in long format with one row per group level × measure × response combination.
#'
#' @examples
#' \dontrun{
#' group_cols <- c("overall", "country", "region")
#' measure_cols <- df |>
#'   dplyr::select(
#'     tidyselect::matches("q[0-9]+_mcq_.*") |
#'     tidyselect::matches("q[0-9]+_catq")
#'   ) |>
#'   names()
#'
#' sum_categorical_measures <- get_frequency(
#'   data = df,
#'   measures = measure_cols,
#'   groups = group_cols
#' )
#' }
#'
#' @importFrom dplyr group_by select across filter mutate summarise ungroup bind_rows n
#' @importFrom tidyr pivot_longer complete nesting
#' @importFrom rlang .data sym
#' @importFrom tidyselect all_of
#' @export
get_frequency <- function(data, measures, groups = "overall") {
  results <- list()

  for (grp in groups) {                 # Get summary by each group

    # Step 1. Group data
    if (grp == "overall") {             # If no group specified (i.e., Overall),
      grp_data <- data |>               # create a constant grouping column so downstream code is uniform
        dplyr::mutate(overall = "overall")
    } else {                            # If group specified
      grp_data <- data |>               # exclude rows/participants with no group membership
        dplyr::filter(!is.na(!!rlang::sym(grp))) |>   # and group data
        dplyr::group_by(!!rlang::sym(grp))
    }

    # Step 2. Summarise each measure within this group
    results[[grp]] <- grp_data |>
      dplyr::mutate(dplyr::across(               # Coerce to character so logical, factor, and character cols are handled uniformly by pivot_longer
        dplyr::all_of(measures),
        as.character)) |>
      tidyr::pivot_longer(                       # Reshape: one row per respondent × measure
        cols      = tidyselect::all_of(measures),
        names_to  = "measure",
        values_to = "response"
      ) |>
      dplyr::group_by(dplyr::across(
        dplyr::all_of(c(grp, "measure", "response"))
        )) |>                                    # Count occurrences of each response within group × measure
      dplyr::summarise(numerator = dplyr::n(), .groups = "drop") |>
      dplyr::filter(!is.na(.data$response)) |>   # Exclude NAs from denominator (i.e., treat as skipped)
      tidyr::complete(                           # Fill in 0 for missing combinations
        !!rlang::sym(grp),
        tidyr::nesting(
          !!rlang::sym("measure"),
          !!rlang::sym("response")
        ),
        fill = list(numerator = 0)
      ) |>
      dplyr::group_by(dplyr::across(             # Compute denominator and percentage within group × measure
        dplyr::all_of(c(grp, "measure"))
        )) |>
      dplyr::mutate(
        denominator = sum(.data$numerator),
        percent     = .data$numerator / .data$denominator
      ) |>
      dplyr::ungroup()
  }

  # Step 3. Combine all groups
  dplyr::bind_rows(results) |>                   # Compute denominator and percentage within group × measure
    dplyr::select(
      dplyr::all_of(c(groups, "measure", "response", "numerator", "denominator", "percent"))
    )
}
