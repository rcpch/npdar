#' Correlation Matrix with Plotly
#'
#' @description
#' Computes a correlation matrix and corresponding p-values, and visualises the
#' lower triangle as an interactive heatmap using \pkg{plotly}. Correlation
#' values and significance stars can be displayed inside each tile.
#'
#' @param data A data frame or matrix containing only numeric variables.
#' @param method Correlation method: \code{"pearson"} or \code{"spearman"},
#'   passed directly to \code{stats::cor()} and \code{stats::cor.test()}.
#' @param use Missing-data handling passed directly to \code{stats::cor()}.
#'   One of \code{"pairwise.complete.obs"}, \code{"complete.obs"},
#'   \code{"everything"}, \code{"all.obs"}, or \code{"na.or.complete"}.
#' @param digits Number of decimal places for correlation labels.
#' @param show_stars Logical; display significance stars (*, **, ***)
#' based on p-values.
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
#' @importFrom dplyr left_join mutate case_when if_else
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble
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

  if (!all(vapply(data, is.numeric, logical(1)))) {
    stop("All columns in `data` must be numeric.")
  }

  if (ncol(data) < 2) {
    stop("`data` must contain at least two numeric columns.")
  }

  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0 || digits != floor(digits)) {
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
    stop("No complete observations available after applying `use = '", use, "'`.")
  }

  #------ 3) Correlation matrix ------
  # Pass `method` and `use` directly to stats::cor()
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
      } else if (use == "everything") {
        if (anyNA(x) || anyNA(y)) {
          next
        }
        x2 <- x
        y2 <- y
      } else {                                         # complete.obs, all.obs, na.or.complete
        x2 <- data_complete[[vars[i]]]
        y2 <- data_complete[[vars[j]]]
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

      p_mat[i, j] <- if (is.null(test_res)) NA_real_ else test_res$p.value
    }
  }

  #------ 5) Long format, lower triangle only (exclude diagonal) ------
  lower <- which(lower.tri(cor_mat), arr.ind = TRUE)

  corr_long <- tibble::tibble(
    Var1 = vars[lower[, "row"]],
    Var2 = vars[lower[, "col"]],
    Correlation = cor_mat[lower],
    P_value = p_mat[lower]
  ) |>
    dplyr::mutate(
      Sig = dplyr::case_when(
        .data$P_value < 0.001 ~ "***",
        .data$P_value < 0.01  ~ "**",
        .data$P_value < 0.05  ~ "*",
        TRUE ~ ""
      ),
      CorrLabel = dplyr::if_else(
        is.na(.data$Correlation),
        "",
        sprintf(paste0("%.", digits, "f"), .data$Correlation)
      ),
      TileLabel = paste0(
        if (show_values) .data$CorrLabel else "",
        if (show_stars) .data$Sig else ""
      ),
      HoverText = dplyr::if_else(
        is.na(.data$Correlation),
        paste0(
          "<b>", .data$Var1, " vs ", .data$Var2, "</b>",
          "<br>", method, " correlation: NA",
          "<br>p-value: NA"
        ),
        paste0(
          "<b>", .data$Var1, " vs ", .data$Var2, "</b>",
          "<br>", method, " correlation: ", .data$CorrLabel, .data$Sig,
          "<br>p-value: ", format.pval(.data$P_value, digits = 3, eps = 0.001)
        )
      )
    )

  #------ 6) Build rectangular layout to remove blank diagonal ticks ------
  x_vars <- vars[-length(vars)]
  y_vars <- vars[-1]

  # One row per y, one column per x (row-major), so matrices can be filled byrow
  grid_df <- tidyr::expand_grid(Var1 = y_vars, Var2 = x_vars) |>
    dplyr::left_join(corr_long, by = c("Var1", "Var2"))

  #------ 7) Convert to matrices for plotly ------
  to_matrix <- function(col) {
    matrix(grid_df[[col]], nrow = length(y_vars), byrow = TRUE)
  }

  z_mat <- to_matrix("Correlation")
  text_mat <- to_matrix("HoverText")
  label_mat <- to_matrix("TileLabel")

  #------ 8) Plotly colorscale ------
  colors_pos <- seq(0, 1, length.out = length(colors))
  colors_scale <- lapply(seq_along(colors), function(i) {
    c(colors_pos[i], colors[i])
  })

  #------ 9) Build plotly heatmap ------
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

  #------ 10) Add annotations ------
  labelled <- which(!is.na(label_mat) & nzchar(label_mat), arr.ind = TRUE)

  ann <- lapply(seq_len(nrow(labelled)), function(k) {
    list(x = x_vars[labelled[k, "col"]],
         y = y_vars[labelled[k, "row"]],
         text = label_mat[labelled[k, "row"], labelled[k, "col"]],
         showarrow = FALSE,
         xref = "x",
         yref = "y",
         font = list(size = 12, color = "black"),
         align = "center")
  })

  fig |>
    plotly::layout(xaxis = list(title = "", ticks = "", side = "bottom"),
                   yaxis = list(title = "", ticks = "", autorange = "reversed"),
                   annotations = ann)
}
