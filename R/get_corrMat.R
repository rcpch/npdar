#' Correlation Matrix with Plotly
#'
#' @description
#' Computes a correlation matrix and corresponding p-values, and visualises the
#' lower triangle as an interactive heatmap using \pkg{plotly}. Correlation values
#' and significance stars can be displayed inside each tile.
#'
#' @param data A data frame or matrix containing only numeric variables.
#' @param method Correlation method: \code{"pearson"} or \code{"spearman"},
#'   passed directly to \code{stats::cor()} and \code{stats::cor.test()}.
#' @param use Missing-data handling passed directly to \code{stats::cor()}.
#'   One of \code{"pairwise.complete.obs"}, \code{"complete.obs"},
#'   \code{"everything"}, \code{"all.obs"}, or \code{"na.or.complete"}.
#' @param digits Number of decimal places for correlation labels.
#' @param show_stars Logical; display significance stars (*, **, ***) based on p-values.
#' @param show_values Logical; display correlation values inside tiles.
#' @param colors A vector of colours for the heatmap colour scale.
#'   Defaults to NPDA colours.
#' @param ... Additional arguments passed to \code{plotly::plot_ly()}.
#'   These can override default plotly arguments
#'   such as \code{showscale}, or \code{colorbar}.
#'
#' @return A \pkg{plotly} heatmap object.
#'
#' @examples
#' # Basic correlation matrix
#' get_corrMat(mtcars)
#'
#' # Spearman correlation without tile labels, with additional plotly arguments
#' get_corrMat(mtcars,
#'             method = "spearman",
#'             show_values = FALSE,
#'             show_stars = TRUE,
#'             colorbar = list(title = "Correlation")) |>
#' plotly::layout(annotations = list(
#'   text = paste0("<i><b>Note:</b>",
#'                 "*: p<0.05, **: p<0.01, ***: p<0.001.</i>"),
#'   x = 1, y = 1, xref = "paper", yref = "paper",
#'   xanchor = "right", yanchor = "top",
#'   showarrow = FALSE))
#'
#' @importFrom dplyr left_join mutate filter select arrange case_when if_else all_of
#' @importFrom tidyr pivot_longer expand_grid pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
#' @export
get_corrMat <- function(data,
                        method = c("pearson", "spearman"), # mimic stats::cor() options
                        use = c("pairwise.complete.obs",
                                "complete.obs",
                                "everything",
                                "all.obs",
                                "na.or.complete"), # mimic stats::cor() options
                        digits = 2,
                        show_stars = TRUE,
                        show_values = TRUE,
                        colors = c("#11A7F2", "#FFFFFF", "#E00087"),
                        ...                        # other plotly arguments
                        ) {

  method <- match.arg(method)
  use  <- match.arg(use)

  #------ 1) Check input ------
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  if (any(!vapply(data, is.numeric, logical(1)))) {
    stop("All columns in `data` must be numeric.")
  }

  if (ncol(data) < 2) {
    stop("`data` must contain at least two numeric columns.")
  }

  if (!is.numeric(digits) || length(digits) != 1 || digits < 0 || digits != floor(digits) || is.na(digits)) {
    stop("`digits` must be a single non-negative whole number.")
  }

  if (!is.logical(show_stars) || length(show_stars) != 1 || is.na(show_stars)) {
    stop("`show_stars` must be TRUE or FALSE.")
  }

  if (!is.logical(show_values) || length(show_values) != 1 || is.na(show_values)) {
    stop("`show_values` must be TRUE or FALSE.")
  }

  if (length(colors) < 2) {
    stop("`colors` must contain at least two colours.")
  }

  vars <- names(data)

  #------ 2) Prepare data according to `use` ------
  # stats::cor() can handle `use` directly, but stats::cor.test() cannot
  complete_idx <- stats::complete.cases(data)

  if (use == "all.obs" && any(!complete_idx)) {
    stop("Missing values present in `data`, but `use = 'all.obs'` does not allow NA values.")
  }

  data_complete <- data[complete_idx, , drop = FALSE]

  if (use %in% c("complete.obs", "na.or.complete") && nrow(data_complete) == 0) {
    stop("No complete observations available after applying `use = '", use, "`.")
  }

  #------ 3) Correlation matrix ------
  # Pass `method` and use` directly to stats::cor()
  cor_mat <- stats::cor(data,
                        method = method,
                        use = use)

  #------ 4) P-value matrix ------
  # Build p-values manually via cor.test()
  p_mat <- matrix(NA_real_,
                  nrow = length(vars),
                  ncol = length(vars),
                  dimnames = list(vars, vars)
                  )

  for (i in seq_along(vars)) {
    for (j in seq_len(i - 1)) {

      x <- data[[vars[i]]]
      y <- data[[vars[j]]]

      # Missing-data handling for p-values
      if (use == "pairwise.complete.obs") {
        ok <- stats::complete.cases(x, y)
        x2 <- x[ok]
        y2 <- y[ok]
      } else if (use %in% c("complete.obs", "all.obs", "na.or.complete")) {
        x2 <- data_complete[[vars[i]]]
        y2 <- data_complete[[vars[j]]]
      } else if (use == "everything") {
        if (anyNA(x) || anyNA(y)) {
          next
        }
        x2 <- x
        y2 <- y
      }

      # Need enough observations and variation
      if (
        length(x2) < 3 ||
        length(unique(x2)) < 2 ||
        length(unique(y2)) < 2
      ) {
        next
      }

      test_res <- tryCatch(
        suppressWarnings(
          stats::cor.test(
            x2,
            y2,
            method = method,
            exact = if (method == "spearman") FALSE else NULL
          )
        ),
        error = function(e) NULL
      )

      p_val <- if (is.null(test_res)) NA_real_ else test_res$p.value

      p_mat[i, j] <- p_val
      p_mat[j, i] <- p_val
    }
  }

  #------ 5) Convert matrices to long format ------
  cor_df <- cor_mat |>
    as.data.frame() |>
    tibble::rownames_to_column("Var1") |>
    tidyr::pivot_longer(-"Var1", names_to = "Var2", values_to = "Correlation")

  p_df <- p_mat |>
    as.data.frame() |>
    tibble::rownames_to_column("Var1") |>
    tidyr::pivot_longer(-"Var1", names_to = "Var2", values_to = "P_value")

  #------ 6) Keep lower triangle only (exclude diagonal) ------
  corr_long <- cor_df |>
    dplyr::left_join(p_df, by = c("Var1", "Var2")) |>
    dplyr::mutate(
      i = match(.data$Var1, vars),
      j = match(.data$Var2, vars)
    ) |>
    dplyr::filter(.data$i > .data$j) |>
    dplyr::mutate(
      Sig = dplyr::case_when(
        !is.na(.data$P_value) & .data$P_value < 0.001 ~ "***",
        !is.na(.data$P_value) & .data$P_value < 0.01  ~ "**",
        !is.na(.data$P_value) & .data$P_value < 0.05  ~ "*",
        TRUE ~ ""
      ),
      CorrLabel = dplyr::if_else(
        is.na(.data$Correlation),
        "",
        sprintf(paste0("%.", digits, "f"), .data$Correlation)
      ),
      TileLabel = dplyr::case_when(
        show_values & show_stars & .data$Sig != "" ~ paste0(.data$CorrLabel, .data$Sig),
        show_values & show_stars & .data$Sig == "" ~ .data$CorrLabel,
        show_values & !show_stars                  ~ .data$CorrLabel,
        !show_values & show_stars & .data$Sig != "" ~ .data$Sig,
        TRUE ~ ""
      ),
      HoverText = dplyr::case_when(
        is.na(.data$Correlation) ~ paste0(
          "<b>", .data$Var1, " vs ", .data$Var2, "</b>",
          "<br>", method, " correlation: NA",
          "<br>p-value: NA"
        ),
        TRUE ~ paste0(
          "<b>", .data$Var1, " vs ", .data$Var2, "</b>",
          "<br>", method, " correlation: ", .data$CorrLabel, .data$Sig,
          "<br>p-value: ", format.pval(.data$P_value, digits = 3, eps = 0.001)
        )
      )
    )

  #------ 7) Build rectangular layout to remove blank diagonal ticks ------
  x_vars <- vars[-length(vars)]
  y_vars <- vars[-1]

  plot_df <- corr_long |>
    dplyr::mutate(
      .corr_x = factor(.data$Var2, levels = x_vars),
      .corr_y = factor(.data$Var1, levels = y_vars)
    )

  grid_df <- tidyr::expand_grid(
    .corr_y = factor(y_vars, levels = y_vars),
    .corr_x = factor(x_vars, levels = x_vars)
  ) |>
    dplyr::left_join(plot_df, by = c(".corr_x", ".corr_y")) |>
    dplyr::arrange(.data$.corr_y, .data$.corr_x)

  #------ 8) Convert to matrices for plotly ------
  z_mat <- grid_df |>
    dplyr::select(dplyr::all_of(c(".corr_y", ".corr_x", "Correlation"))) |>
    tidyr::pivot_wider(
      names_from = ".corr_x",
      values_from = "Correlation"
    ) |>
    dplyr::select(-dplyr::all_of(".corr_y")) |>
    as.matrix()

  text_mat <- grid_df |>
    dplyr::select(dplyr::all_of(c(".corr_y", ".corr_x", "HoverText"))) |>
    tidyr::pivot_wider(
      names_from = ".corr_x",
      values_from = "HoverText"
    ) |>
    dplyr::select(-dplyr::all_of(".corr_y")) |>
    as.matrix()

  label_mat <- grid_df |>
    dplyr::select(dplyr::all_of(c(".corr_y", ".corr_x", "TileLabel"))) |>
    tidyr::pivot_wider(
      names_from = ".corr_x",
      values_from = "TileLabel"
    ) |>
    dplyr::select(-dplyr::all_of(".corr_y")) |>
    as.matrix()


  #------ 9) Plotly colorscale ------
  colors_pos <- seq(0, 1, length.out = length(colors))
  colors_scale <- lapply(seq_along(colors), function(i) {
    c(colors_pos[i], colors[i])
  })

  #------ 10) Build plotly heatmap ------
  default_plotly_args <- list(x = x_vars,
                              y = y_vars,
                              z = z_mat,
                              type = "heatmap",
                              zmin = -1,
                              zmax = 1,
                              colorscale = colors_scale,
                              text = text_mat,
                              hovertemplate = "%{text}<extra></extra>")

  user_plotly_args <- list(...)

  plotly_args <- utils::modifyList(
    default_plotly_args,
    user_plotly_args,
    keep.null = TRUE
  )

  fig <- do.call(plotly::plot_ly, plotly_args)

  #------ 11) Add annotations ------
  ann <- list()

  for (row in seq_len(nrow(label_mat))) {
    for (col in seq_len(ncol(label_mat))) {
      lab <- label_mat[row, col]

      if (!is.na(lab) && nzchar(lab)) {
        ann[[length(ann) + 1]] <- list(x = x_vars[col],
                                       y = y_vars[row],
                                       text = lab,
                                       showarrow = FALSE,
                                       xref = "x",
                                       yref = "y",
                                       font = list(size = 12, color = "black"),
                                       align = "center")
      }
    }
  }

  fig |>
    plotly::layout(xaxis = list(title = "", ticks = "", side = "bottom"),
                   yaxis = list(title = "", ticks = "", autorange = "reversed"),
                   annotations = ann)
}
