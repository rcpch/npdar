#' Raincloud Plot with Plotly
#'
#' @description
#' Draws an interactive raincloud plot using \pkg{plotly}. With option to show
#' or hide each of half-violin density plots, boxplots, and scatter plots
#' of the raw values.
#'
#' @details
#' Exactly one of `x` or `y` must be numeric and the other categorical:
#' * categorical `x` and numeric `y` give a vertical plot;
#' * numeric `x` and categorical `y` give a horizontal plot.
#'
#' Rows with missing values in either column are dropped. Group order follows
#' the factor levels (unused levels are dropped), otherwise the order of first
#' appearance. Each group uses one colour across all three elements.
#'
#' With `scatter_layout = "dots"`, values are binned along the numeric axis and
#' stacked along the categorical axis, which approximates stacked-dot displays
#' without overlapping points. With `"jitter"` the positions are random, so use
#' [set.seed()] for reproducible plots.
#'
#' Elements switched off with `show_density`, `show_scatter` or `show_box` are
#' still added to the plot, but start hidden, so that the buttons can reveal
#' them.
#'
#' @param data A data frame.
#' @param x,y Single strings naming columns of `data`. One must be numeric and
#'   the other categorical.
#' @param colors Optional character vector of colours. A named vector is matched
#'   to group names; an unnamed vector is assigned in group order. If `NULL`,
#'   colours come from [grDevices::hcl.colors()].
#' @param show_density,show_scatter,show_box Logical; whether each element is
#'   initially visible.
#' @param show_buttons Logical; add buttons that toggle each element.
#' @param show_legend Logical; show one legend entry per group.
#' @param density_side Side of the group position on which the half-violin is
#'   drawn: `"right"` or `"left"`. For horizontal plots these are the upper and
#'   lower sides.
#' @param density_width Maximum width of the half-violin, in units of the
#'   distance between groups.
#' @param density_opacity Fill opacity of the density, between 0 and 1.
#' @param scatter_layout Point placement: `"jitter"` (random), `"dots"`
#'   (stacked within bins) or `"center"` (on the group position).
#' @param scatter_side Side of the group position on which points are placed:
#'   `"auto"` (opposite to the density), `"left"`, `"right"` or `"center"`.
#' @param scatter_size Marker size.
#' @param scatter_opacity Marker opacity, between 0 and 1.
#' @param jitter_width Maximum jitter when `scatter_layout = "jitter"`.
#' @param dot_binwidth Bin width along the numeric axis when
#'   `scatter_layout = "dots"`. If `NULL`, one thirtieth of the data range.
#' @param dot_spacing Spacing between stacked dots when
#'   `scatter_layout = "dots"`.
#' @param box_width Width of the boxplot.
#' @param boxpoints Passed to [plotly::add_boxplot()]; `FALSE`, `"outliers"`
#'   or `"suspectedoutliers"`.
#' @param boxmean Passed to [plotly::add_boxplot()]; `TRUE`, `FALSE` or `"sd"`.
#' @param ... Additional arguments passed to [plotly::plot_ly()].
#'
#' @return A \pkg{plotly} object.
#'
#' @examples
#' set.seed(1)
#' plot_raincloud_ly(iris,
#'                   x = "Species",
#'                   y = "Sepal.Length",
#'                   scatter_layout = "jitter",
#'                   show_buttons = FALSE)
#'
#' plot_raincloud_ly(iris,
#'                   x = "Sepal.Length",
#'                   y = "Species",
#'                   colors = c("#11A7F2", "#E00087", "#00bdaa"),
#'                   scatter_layout = "dots",
#'                   show_box = FALSE,
#'                   show_legend = TRUE) |>
#'   plotly::layout(yaxis = list(showticklabels = FALSE),
#'                  legend = list(x = 1, y = -0.2, xanchor = "right",
#'                                orientation = "h"))
#'
#' @importFrom stats complete.cases setNames
#' @importFrom grDevices hcl.colors
#' @export
plot_raincloud_ly <- function(data, x, y,
                              colors = NULL,
                              # Display options for different plot components
                              show_density = TRUE,
                              show_scatter = TRUE,
                              show_box = TRUE,
                              show_buttons = TRUE,
                              show_legend = FALSE,
                              # Density
                              density_side = c("right", "left"),
                              density_width = 0.38,
                              density_opacity = 0.45,
                              # Scatter
                              scatter_layout = c("jitter", "dots", "center"),
                              scatter_side = c("auto", "left", "right", "center"),
                              scatter_size = 6,
                              scatter_opacity = 0.65,
                              jitter_width = 0.2,
                              dot_binwidth = NULL,
                              dot_spacing = 0.035,
                              # Box
                              box_width = 0.16,
                              boxpoints = FALSE,
                              boxmean = TRUE,
                              ...                        # other plotly arguments
) {

  density_side <- match.arg(density_side)
  scatter_layout <- match.arg(scatter_layout)
  scatter_side <- match.arg(scatter_side)

  #------ 1) Check input ------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  for (arg in c("x", "y")) {
    val <- get(arg)
    if (!is.character(val) || length(val) != 1 || is.na(val) || !val %in% names(data)) {
      stop("`", arg, "` must be a single column name present in `data`.")
    }
  }

  for (arg in c("show_density", "show_scatter", "show_box", "show_buttons", "show_legend")) {
    val <- get(arg)
    if (!is.logical(val) || length(val) != 1 || is.na(val)) {
      stop("`", arg, "` must be TRUE or FALSE.")
    }
  }

  x_numeric <- is.numeric(data[[x]])
  y_numeric <- is.numeric(data[[y]])

  if (x_numeric == y_numeric) {
    stop("Exactly one of `x` or `y` must be numeric, and the other categorical.")
  }

  horizontal <- x_numeric
  group_col <- if (horizontal) y else x
  value_col <- if (horizontal) x else y

  #------ 2) Prepare data ------
  plot_data <- data.frame(group = data[[group_col]], value = data[[value_col]])
  plot_data <- plot_data[stats::complete.cases(plot_data), , drop = FALSE]

  if (nrow(plot_data) == 0) {
    stop("No complete cases available after removing missing values.")
  }

  groups <- if (is.factor(plot_data$group)) {
    levels(droplevels(plot_data$group))
  } else {
    unique(as.character(plot_data$group))
  }
  plot_data$group <- factor(plot_data$group, levels = groups)
  positions <- stats::setNames(seq_along(groups), groups)

  #------ 3) Colours ------
  if (is.null(colors)) {
    colors <- stats::setNames(grDevices::hcl.colors(length(groups), palette = "Set2"), groups)
  } else {
    if (!is.character(colors)) {
      stop("`colors` must be a character vector of colours.")
    }

    if (is.null(names(colors))) {
      if (length(colors) < length(groups)) {
        stop("Unnamed `colors` must have at least one colour per group.")
      }
      colors <- stats::setNames(colors[seq_along(groups)], groups)
    } else {
      missing_cols <- setdiff(groups, names(colors))
      if (length(missing_cols) > 0) {
        stop("`colors` is missing values for: ", paste(missing_cols, collapse = ", "))
      }
      colors <- colors[groups]
    }
  }

  #------ 4) Defaults depending on the data ------
  if (is.null(dot_binwidth)) {
    dot_binwidth <- diff(range(plot_data$value)) / 30
    if (!is.finite(dot_binwidth) || dot_binwidth <= 0) {
      dot_binwidth <- 0.1
    }
  }

  if (scatter_side == "auto") {
    scatter_side <- if (density_side == "right") "left" else "right"
  }

  #------ 5) Build traces; remember which belong to which element ------
  fig <- plotly::plot_ly(...)
  n_traces <- 0L
  layer_idx <- list(density = integer(), scatter = integer(), box = integer())
  cat_coords <- num_coords <- numeric()

  # Swap coordinates for horizontal plots
  xy <- function(cat, num) {
    if (horizontal) list(x = num, y = cat) else list(x = cat, y = num)
  }

  for (g in groups) {
    vals <- plot_data$value[plot_data$group == g]
    pos <- positions[[g]]
    col <- colors[[g]]

    # Legend-only trace, so the entry stays visible when elements are hidden
    if (show_legend) {
      fig <- plotly::add_markers(
        fig,
        x = list(NA_real_),
        y = list(NA_real_),
        marker = list(size = 10, symbol = "square", color = col),
        name = g,
        legendgroup = g,
        showlegend = TRUE,
        hoverinfo = "skip"
      )
      n_traces <- n_traces + 1L
    }

    if (length(vals) > 1) {
      dens <- .raincloud_half_density(vals, pos, density_side, density_width)
      co <- xy(dens$cat, dens$num)

      fig <- plotly::add_trace(
        fig,
        x = co$x,
        y = co$y,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = .raincloud_rgba(col, density_opacity),
        line = list(color = col),
        name = g,
        legendgroup = g,
        showlegend = FALSE,
        hoverinfo = "skip",
        visible = show_density
      )
      n_traces <- n_traces + 1L
      layer_idx$density <- c(layer_idx$density, n_traces)
      cat_coords <- c(cat_coords, dens$cat)
      num_coords <- c(num_coords, dens$num)
    }

    cat_pos <- .raincloud_scatter_positions(
      vals, pos, scatter_layout, scatter_side,
      jitter_width, dot_binwidth, dot_spacing
    )
    co <- xy(cat_pos, vals)

    fig <- plotly::add_markers(
      fig,
      x = co$x,
      y = co$y,
      marker = list(
        size = scatter_size,
        color = .raincloud_rgba(col, scatter_opacity),
        line = list(color = col, width = 0.5)
      ),
      name = g,
      legendgroup = g,
      showlegend = FALSE,
      hovertemplate = paste0(if (horizontal) "%{x}" else "%{y}", "<extra></extra>"),
      visible = show_scatter
    )
    n_traces <- n_traces + 1L
    layer_idx$scatter <- c(layer_idx$scatter, n_traces)
    cat_coords <- c(cat_coords, cat_pos, pos - box_width / 2, pos + box_width / 2)
    num_coords <- c(num_coords, vals)

    co <- xy(rep(pos, length(vals)), vals)

    fig <- plotly::add_boxplot(
      fig,
      x = co$x,
      y = co$y,
      orientation = if (horizontal) "h" else "v",
      width = box_width,
      boxpoints = boxpoints,
      boxmean = boxmean,
      fillcolor = .raincloud_rgba(col, 0.2),
      line = list(color = col),
      marker = list(color = col),
      name = g,
      legendgroup = g,
      showlegend = FALSE,
      hoverinfo = if (horizontal) "x" else "y",
      visible = show_box
    )
    n_traces <- n_traces + 1L
    layer_idx$box <- c(layer_idx$box, n_traces)
  }

  #------ 6) Layout ------
  cat_axis <- list(
    title = group_col,
    tickvals = unname(positions),
    ticktext = names(positions),
    zeroline = FALSE
  )
  num_axis <- list(title = value_col, zeroline = FALSE)

  # Fix the axes so toggling elements does not rescale the plot
  if (show_buttons) {
    pad <- function(r) r + c(-1, 1) * 0.05 * diff(r)
    cat_axis$range <- pad(range(cat_coords))
    num_axis$range <- pad(range(num_coords))
    cat_axis$showgrid <- num_axis$showgrid <- TRUE
    num_axis$type <- "linear"
  }

  fig <- plotly::layout(
    fig,
    xaxis = if (horizontal) num_axis else cat_axis,
    yaxis = if (horizontal) cat_axis else num_axis,
    hovermode = "closest"
  )

  #------ 7) Toggle buttons ------
  if (show_buttons) {
    labels <- c(density = "Density", scatter = "Scatter", box = "Box")
    initial <- c(density = show_density, scatter = show_scatter, box = show_box)

    layers <- names(labels)[lengths(layer_idx) > 0]

    # One menu per button: plotly.js tracks args/args2 state per menu, not per button.
    # Indices are 0-based in plotly.js; `active = -1` makes the first click use `args`.
    # `pad` is in pixels, so the buttons keep their spacing and sit below the axis title
    # whatever the plot size.
    menus <- lapply(seq_along(layers), function(i) {
      layer <- layers[i]
      idx <- as.list(layer_idx[[layer]] - 1L)
      list(
        type = "buttons",
        active = -1,
        showactive = FALSE,
        x = 0,
        xanchor = "left",
        y = 0,
        yanchor = "top",
        pad = list(t = 70, l = (i - 1) * 80),
        buttons = list(list(
          label = labels[[layer]],
          method = "restyle",
          args = list("visible", !initial[[layer]], idx),
          args2 = list("visible", initial[[layer]], idx)
        ))
      )
    })

    fig <- plotly::layout(fig, margin = list(b = 120), updatemenus = menus)
  }

  fig
}


#' Convert a colour to a plotly rgba string
#' @noRd
.raincloud_rgba <- function(col, alpha = 1) {
  rgb_val <- grDevices::col2rgb(col)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb_val[1, 1], rgb_val[2, 1], rgb_val[3, 1], alpha)
}


#' Half-violin polygon: `cat` is the categorical-axis coordinate, `num` the numeric one
#' @noRd
.raincloud_half_density <- function(vals, pos, side, width) {
  dens <- stats::density(vals)
  offset <- dens$y / max(dens$y) * width
  if (side == "left") {
    offset <- -offset
  }

  data.frame(
    cat = c(pos, pos + offset, pos),
    num = c(dens$x[1], dens$x, dens$x[length(dens$x)])
  )
}


#' Categorical-axis coordinates for the scatter points of one group
#' @noRd
.raincloud_scatter_positions <- function(vals, pos, layout, side,
                                         jitter_width, dot_binwidth, dot_spacing) {
  n <- length(vals)

  if (layout == "center") {
    return(rep(pos, n))
  }

  if (layout == "jitter") {
    return(switch(
      side,
      center = pos + stats::runif(n, -jitter_width, jitter_width),
      right  = pos + stats::runif(n, 0.01, jitter_width),
      left   = pos - stats::runif(n, 0.01, jitter_width)
    ))
  }

  # dots: stack points sharing a bin along the categorical axis
  out <- numeric(n)
  bins <- split(seq_len(n), round(vals / dot_binwidth))

  for (idx in bins) {
    k <- length(idx)
    offsets <- switch(
      side,
      right = (seq_len(k) - 0.2) * dot_spacing,
      left  = -(seq_len(k) - 0.2) * dot_spacing,
      # center: 0, 1, -1, 2, -2, ...
      c(0, rbind(seq_len(k), -seq_len(k)))[seq_len(k)] * dot_spacing
    )
    out[idx] <- pos + offsets
  }

  out
}
