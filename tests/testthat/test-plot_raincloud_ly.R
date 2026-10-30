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

test_that("plot_raincloud_ly dots layout gives distinct positions within a bin", {
  pos <- npdar:::.raincloud_scatter_positions(
    rep(1, 5), pos = 1, layout = "dots", side = "center",
    jitter_width = 0.2, dot_binwidth = 0.2, dot_spacing = 0.1
  )

  expect_equal(length(unique(pos)), 5)
})
