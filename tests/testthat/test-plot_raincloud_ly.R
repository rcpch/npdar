# ========== Test input validation ==========
test_that("plot_raincloud_ly validates data and column names", {
  expect_error(plot_raincloud_ly(1:5, "a", "b"), "`data` must be a data frame")
  expect_error(plot_raincloud_ly(iris, "nope", "Sepal.Length"), "`x` must be a single column name")
  expect_error(plot_raincloud_ly(iris, "Species", c("Sepal.Length", "Sepal.Width")),
               "`y` must be a single column name")
})

test_that("plot_raincloud_ly needs one numeric and one categorical column", {
  expect_error(plot_raincloud_ly(iris, "Sepal.Length", "Sepal.Width"),
               "Exactly one of `x` or `y`")
  expect_error(plot_raincloud_ly(data.frame(a = letters, b = LETTERS), "a", "b"),
               "Exactly one of `x` or `y`")
})

test_that("plot_raincloud_ly validates flags and colors", {
  expect_error(plot_raincloud_ly(iris, "Species", "Sepal.Length", show_box = NA),
               "`show_box` must be TRUE or FALSE")
  expect_error(plot_raincloud_ly(iris, "Species", "Sepal.Length", colors = c("red", "blue")),
               "at least one colour per group")
  expect_error(plot_raincloud_ly(iris, "Species", "Sepal.Length", colors = c(setosa = "red")),
               "`colors` is missing values for")
})

test_that("plot_raincloud_ly errors when no complete cases remain", {
  df <- data.frame(g = c("a", "b"), v = c(NA_real_, NA_real_))
  expect_error(plot_raincloud_ly(df, "g", "v"), "No complete cases")
})

# ========== Test traces and buttons ==========
test_that("plot_raincloud_ly returns a plotly object for both orientations", {
  expect_s3_class(plot_raincloud_ly(iris, "Species", "Sepal.Length"), "plotly")
  expect_s3_class(plot_raincloud_ly(iris, "Sepal.Length", "Species"), "plotly")
})

test_that("plot_raincloud_ly buttons toggle the right traces", {
  p <- plotly::plotly_build(plot_raincloud_ly(iris, "Species", "Sepal.Length", show_legend = TRUE))
  menus <- p$x$layout$updatemenus
  btn <- lapply(menus, function(m) m$buttons[[1]])
  labels <- vapply(btn, function(b) b$label, character(1))

  # Per group: legend, density, scatter, box
  expect_equal(labels, c("Density", "Scatter", "Box"))
  expect_equal(length(p$x$data), 12)
  expect_equal(unlist(btn[[1]]$args[[3]]), c(1, 5, 9))
  expect_equal(unlist(btn[[3]]$args[[3]]), c(3, 7, 11))
  expect_false(btn[[1]]$args[[2]])
  expect_true(btn[[1]]$args2[[2]])

  # No button starts active, so the first click always runs `args`
  expect_true(all(vapply(menus, function(m) m$active == -1, logical(1))))
})

test_that("plot_raincloud_ly places buttons bottom left by default", {
  p <- plotly::plotly_build(plot_raincloud_ly(iris, "Species", "Sepal.Length"))
  menus <- p$x$layout$updatemenus

  expect_equal(unique(vapply(menus, function(m) m$x, numeric(1))), 0)
  expect_equal(unique(vapply(menus, function(m) m$y, numeric(1))), 0)
  expect_equal(vapply(menus, function(m) m$pad$l, numeric(1)), c(0, 80, 160))
  expect_equal(unique(vapply(menus, function(m) m$pad$t, numeric(1))), 70)
  expect_equal(p$x$layout$margin$b, 120)
})

test_that("plot_raincloud_ly button_options customise every button", {
  p <- plotly::plotly_build(plot_raincloud_ly(
    iris, "Species", "Sepal.Length",
    button_options = list(x = 1, xanchor = "right", y = 1.1, yanchor = "bottom", bgcolor = "white")
  ))
  menus <- p$x$layout$updatemenus

  expect_length(menus, 3)
  for (m in menus) {
    expect_equal(m$x, 1)
    expect_equal(m$xanchor, "right")
    expect_equal(m$y, 1.1)
    expect_equal(m$yanchor, "bottom")
    expect_equal(m$bgcolor, "white")
    expect_equal(m$type, "buttons")
    expect_equal(m$active, -1)
  }

  # Custom `y` drops the top padding and the extra bottom margin; spacing stays
  expect_equal(vapply(menus, function(m) m$pad$t, numeric(1)), c(0, 0, 0))
  expect_equal(vapply(menus, function(m) m$pad$l, numeric(1)), c(0, 80, 160))
  expect_false(identical(p$x$layout$margin$b, 120))
})

test_that("plot_raincloud_ly button_options merges pad with the defaults", {
  p <- plotly::plotly_build(plot_raincloud_ly(
    iris, "Species", "Sepal.Length", button_options = list(pad = list(l = 10, r = 5))
  ))
  menus <- p$x$layout$updatemenus

  expect_equal(vapply(menus, function(m) m$pad$l, numeric(1)), c(10, 90, 170))
  expect_equal(unique(vapply(menus, function(m) m$pad$r, numeric(1))), 5)
  expect_equal(unique(vapply(menus, function(m) m$pad$t, numeric(1))), 70)
})

test_that("plot_raincloud_ly validates button_options", {
  args <- list(iris, "Species", "Sepal.Length")

  expect_error(do.call(plot_raincloud_ly, c(args, list(button_options = "a"))),
               "`button_options` must be a named list")
  expect_error(do.call(plot_raincloud_ly, c(args, list(button_options = list(1)))),
               "`button_options` must be a named list")
  expect_error(do.call(plot_raincloud_ly, c(args, list(button_options = list(type = "dropdown")))),
               "cannot set: `type`")
  expect_error(do.call(plot_raincloud_ly, c(args, list(button_options = list(buttons = list())))),
               "cannot set: `buttons`")
  expect_s3_class(do.call(plot_raincloud_ly, c(args, list(button_options = list()))), "plotly")
})

test_that("plot_raincloud_ly hides elements switched off but keeps their buttons", {
  p <- plotly::plotly_build(
    plot_raincloud_ly(iris, "Species", "Sepal.Length", show_box = FALSE, show_legend = TRUE)
  )
  btn <- p$x$layout$updatemenus[[3]]$buttons[[1]]

  expect_true(btn$args[[2]])
  expect_false(btn$args2[[2]])
  expect_false(p$x$data[[4]]$visible)
})

test_that("plot_raincloud_ly fixes axis ranges only when buttons are shown", {
  p <- plotly::plotly_build(plot_raincloud_ly(iris, "Sepal.Length", "Species"))
  expect_length(p$x$layout$xaxis$range, 2)
  expect_length(p$x$layout$yaxis$range, 2)
  expect_true(p$x$layout$xaxis$showgrid)
  expect_true(p$x$layout$yaxis$showgrid)

  p <- plotly::plotly_build(plot_raincloud_ly(iris, "Sepal.Length", "Species", show_buttons = FALSE))
  expect_null(p$x$layout$xaxis$range)
  expect_null(p$x$layout$yaxis$range)
})

test_that("plot_raincloud_ly hover text omits the group name", {
  p <- plotly::plotly_build(plot_raincloud_ly(iris, "Species", "Sepal.Length", show_legend = TRUE))

  expect_equal(unique(as.vector(p$x$data[[3]]$hovertemplate)), "%{y}<extra></extra>")
  expect_equal(unique(as.vector(p$x$data[[4]]$hoverinfo)), "y")

  p <- plotly::plotly_build(plot_raincloud_ly(iris, "Sepal.Length", "Species", show_legend = TRUE))
  expect_equal(unique(as.vector(p$x$data[[4]]$hoverinfo)), "x")
})

test_that("plot_raincloud_ly can omit buttons and legend entries", {
  p <- plotly::plotly_build(
    plot_raincloud_ly(iris, "Species", "Sepal.Length", show_buttons = FALSE, show_legend = FALSE)
  )

  expect_null(p$x$layout$updatemenus)
  expect_equal(length(p$x$data), 9)
})

test_that("plot_raincloud_ly respects factor order and drops unused levels", {
  df <- data.frame(g = factor(c("b", "a", "b", "a"), levels = c("b", "a", "c")),
                   v = c(1, 2, 3, 4))
  p <- plotly::plotly_build(plot_raincloud_ly(df, "g", "v"))

  expect_equal(p$x$layout$xaxis$ticktext, c("b", "a"))
})

test_that("plot_raincloud_ly stacked layout gives distinct positions within a bin", {
  pos <- npdar:::.raincloud_scatter_positions(
    rep(1, 5), pos = 1, layout = "stacked",
    width = 1, scatter_binwidth = 0.2, dot_spacing = 0.1, gap = 0.1
  )

  expect_equal(length(unique(pos)), 5)
})

test_that("plot_raincloud_ly stacked layout: tallest stack spans scatter_width exactly", {
  df <- data.frame(g = rep(c("a", "b"), c(6, 3)), v = 1)

  for (w in c(0.1, 0.3, 0.8)) {
    p <- plotly::plotly_build(plot_raincloud_ly(
      df, "g", "v", scatter_layout = "stacked", scatter_binwidth = 1,
      scatter_box_gap = 0.1, scatter_width = w
    ))
    scatter <- Filter(function(t) identical(t$mode, "markers"), p$x$data)
    a <- 1 - scatter[[1]]$x
    b <- 2 - scatter[[2]]$x

    expect_equal(range(a), c(0.1, 0.1 + w), info = w)
    # same spacing in every group, so a shorter stack is narrower
    expect_equal(max(b) - min(b), w * 2 / 5, info = w)
  }
})

test_that("plot_raincloud_ly scatter_box_gap sets the same gap for every layout", {
  for (gap in c(0.05, 0.1, 0.3)) {
    for (layout in c("stacked", "jitter")) {
      pos <- npdar:::.raincloud_scatter_positions(
        c(1, 1, 1, 2), pos = 1, layout = layout,
        width = 0.2, scatter_binwidth = 0.2, dot_spacing = 0.1, gap = gap
      )
      info <- paste(layout, gap)

      expect_true(all(1 - pos >= gap - 1e-12), info = info)
      if (layout == "stacked") {
        expect_equal(min(1 - pos), gap, info = info)
      }
    }
  }
})

test_that("plot_raincloud_ly scatter_box_gap defaults to just outside the box edge", {
  p <- plotly::plotly_build(plot_raincloud_ly(
    iris, "Species", "Sepal.Length",
    scatter_layout = "stacked", scatter_width = 0, box_width = 0.2
  ))
  scatter <- Filter(function(t) identical(t$mode, "markers") && !is.na(t$x[[1]]), p$x$data)

  expect_equal(1 - max(scatter[[1]]$x), 0.1 + 0.025)
})

test_that("plot_raincloud_ly scatter_width = 0 gives a single column in both layouts", {
  for (layout in c("stacked", "jitter")) {
    p <- plotly::plotly_build(plot_raincloud_ly(
      data.frame(g = "a", v = c(1, 1, 2)), "g", "v",
      scatter_layout = layout, scatter_box_gap = 0.1, scatter_width = 0
    ))
    scatter <- Filter(function(t) identical(t$mode, "markers"), p$x$data)

    expect_equal(as.numeric(scatter[[1]]$x), rep(0.9, 3), info = layout)
  }
})

test_that("plot_raincloud_ly only accepts jitter and stacked layouts", {
  expect_error(plot_raincloud_ly(iris, "Species", "Sepal.Length", scatter_layout = "aligned"))
})

test_that("plot_raincloud_ly validates numeric arguments", {
  bad <- list(
    scatter_box_gap = NA, scatter_box_gap = "a", scatter_box_gap = c(0, 1),
    box_width = 0, scatter_size = -1, density_width = 0,
    scatter_binwidth = 0, scatter_width = -0.1,
    density_opacity = 1.5, scatter_opacity = -0.1, scatter_opacity = Inf
  )

  for (i in seq_along(bad)) {
    arg <- names(bad)[i]
    args <- c(list(iris, "Species", "Sepal.Length"), bad[i])
    expect_error(do.call(plot_raincloud_ly, args), paste0("`", arg, "` must be a single number"),
                 info = paste(arg, format(bad[[i]])))
  }

  expect_s3_class(plot_raincloud_ly(iris, "Species", "Sepal.Length", scatter_box_gap = 0), "plotly")
  expect_s3_class(plot_raincloud_ly(iris, "Species", "Sepal.Length", scatter_box_gap = -0.1), "plotly")
  expect_s3_class(plot_raincloud_ly(iris, "Species", "Sepal.Length", scatter_binwidth = NULL), "plotly")
})
