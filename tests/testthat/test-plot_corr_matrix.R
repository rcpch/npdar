# ========== Test df requirements ==========
test_that("plot_corr_matrix requires numeric columns only", {
  expect_error(
    plot_corr_matrix(data.frame(x = 1:5,
                                y = letters[1:5])),
    "All columns in `data` must be numeric"
  )
})

test_that("plot_corr_matrix requires at least two columns", {
  expect_error(
    plot_corr_matrix(mtcars["mpg"]),
    "`data` must contain at least two numeric columns"
  )
})

test_that("plot_corr_matrix accepts numeric matrix input", {
  mat <- as.matrix(mtcars[, 1:4])

  expect_s3_class(
    plot_corr_matrix(mat),
    "plotly"
  )
})

test_that("plot_corr_matrix handles constant columns without error", {
  df <- data.frame(
    x = 1:10,
    y = rep(1, 10),
    z = 10:1
  )

  expect_warning(
    p <- plot_corr_matrix(df),
    "standard deviation is zero"
  )

  expect_s3_class(p, "plotly")
})

# ========== Test heatmap content ==========
test_that("plot_corr_matrix shows lower-triangle correlations only", {
  p <- plot_corr_matrix(mtcars[, 1:3])   # x axis: mpg, cyl; y axis: cyl, disp
  z <- unname(p$x$attrs[[1]]$z)
  r <- cor(mtcars[, 1:3])

  expect_equal(p$x$attrs[[1]]$x, c("mpg", "cyl"))
  expect_equal(p$x$attrs[[1]]$y, c("cyl", "disp"))
  expect_equal(z, matrix(c(r["cyl", "mpg"],  NA,
                           r["disp", "mpg"], r["disp", "cyl"]),
                         nrow = 2, byrow = TRUE))
})

# ========== Test digits handling ==========
test_that("plot_corr_matrix validates digits argument", {
  expect_error(
    plot_corr_matrix(mtcars[, 1:4], digits = -1),
    "`digits` must be a single non-negative whole number"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], digits = 1.5),
    "`digits` must be a single non-negative whole number"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], digits = c(1, 2)),
    "`digits` must be a single non-negative whole number"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], digits = NA),
    "`digits` must be a single non-negative whole number"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], digits = NA_real_),
    "`digits` must be a single non-negative whole number"
  )
})

# ========== Test show_stars & show_values ==========
test_that("plot_corr_matrix works when values and stars are hidden", {
  expect_s3_class(
    plot_corr_matrix(
      mtcars[, 1:4],
      show_values = FALSE,
      show_stars = FALSE
    ),
    "plotly"
  )
})

test_that("plot_corr_matrix validates show_stars argument", {
  expect_error(
    plot_corr_matrix(mtcars[, 1:4], show_stars = "yes"),
    "`show_stars` must be TRUE or FALSE"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], show_stars = c(TRUE, FALSE)),
    "`show_stars` must be TRUE or FALSE"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], show_stars = NA),
    "`show_stars` must be TRUE or FALSE"
  )
})

test_that("plot_corr_matrix validates show_values argument", {
  expect_error(
    plot_corr_matrix(mtcars[, 1:4], show_values = "no"),
    "`show_values` must be TRUE or FALSE"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], show_values = c(TRUE, FALSE)),
    "`show_values` must be TRUE or FALSE"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], show_values = NA),
    "`show_values` must be TRUE or FALSE"
  )
})

# ========== Test colors validation ==========
test_that("plot_corr_matrix validates colors argument", {
  expect_error(
    plot_corr_matrix(mtcars[, 1:4], colors = "#FFFFFF"),
    "`colors` must contain at least two colours"
  )

  expect_error(
    plot_corr_matrix(mtcars[, 1:4], colors = NA),
    "`colors` must contain at least two colours"
  )
})

# ========== Test NAs handling ==========
test_that("plot_corr_matrix handles pairwise missing data", {
  df <- mtcars[, 1:4]
  df[1:3, "mpg"] <- NA

  expect_s3_class(
    plot_corr_matrix(df, use = "pairwise.complete.obs"),
    "plotly")
})

test_that("plot_corr_matrix errors with all.obs and missing values", {
  df <- mtcars[, 1:4]
  df[1, "mpg"] <- NA

  expect_error(
    plot_corr_matrix(df, use = "all.obs"),
    "does not allow NA values")
})

test_that("plot_corr_matrix errors when complete.obs leaves no complete observations", {
  df <- data.frame(x = c(1, NA, NA),
                   y = c(NA, 2, NA),
                   z = c(NA, NA, 3))

  expect_error(
    plot_corr_matrix(df, use = "complete.obs"),
    "No complete observations available"
  )
})

test_that("plot_corr_matrix returns NA correlations when na.or.complete has no complete rows", {
  df <- data.frame(x = c(1, NA, NA),
                   y = c(NA, 2, NA),
                   z = c(NA, NA, 3))

  p <- plot_corr_matrix(df, use = "na.or.complete")

  expect_true(all(is.na(p$x$attrs[[1]]$z)))
})

# ========== Test sample size in hover text ==========
test_that("plot_corr_matrix hover text shows n according to `use`", {
  df <- mtcars[, 1:3]
  df[1:3, "mpg"] <- NA
  hover <- function(use) {
    p <- plot_corr_matrix(df, use = use)
    unname(p$x$attrs[[1]]$text)
  }

  # rows: cyl, disp; cols: mpg, cyl
  pairwise <- hover("pairwise.complete.obs")
  expect_match(pairwise[1, 1], "n: 29", fixed = TRUE)  # cyl vs mpg
  expect_match(pairwise[2, 1], "n: 29", fixed = TRUE)  # disp vs mpg
  expect_match(pairwise[2, 2], "n: 32", fixed = TRUE)  # disp vs cyl

  complete <- hover("complete.obs")
  expect_match(complete[2, 2], "n: 29", fixed = TRUE)  # disp vs cyl
  expect_match(hover("na.or.complete")[2, 2], "n: 29", fixed = TRUE)

  # correlations involving mpg are NA, but n is still reported
  everything <- hover("everything")
  expect_match(everything[1, 1], "correlation: NA", fixed = TRUE)
  expect_match(everything[1, 1], "n: 32", fixed = TRUE)
  expect_match(everything[2, 2], "n: 32", fixed = TRUE)
})

# ========== Test tile labels and spearman ==========
test_that("plot_corr_matrix tile labels follow digits, show_values and show_stars", {
  labels <- function(...) {
    p <- plot_corr_matrix(mtcars[, 1:3], ...)
    vapply(plotly::plotly_build(p)$x$layout$annotations, `[[`, "", "text")
  }
  r <- cor(mtcars[, 1:3])["cyl", "mpg"]

  expect_equal(labels()[1], paste0(sprintf("%.2f", r), "***"))
  expect_equal(labels(digits = 3)[1], paste0(sprintf("%.3f", r), "***"))
  expect_equal(labels(show_stars = FALSE)[1], sprintf("%.2f", r))
  expect_equal(labels(show_values = FALSE)[1], "***")
  expect_length(labels(show_values = FALSE, show_stars = FALSE), 0)
})

test_that("plot_corr_matrix supports spearman correlation", {
  p <- plot_corr_matrix(mtcars[, 1:3], method = "spearman")
  z <- unname(p$x$attrs[[1]]$z)

  expect_equal(z[1, 1], cor(mtcars$cyl, mtcars$mpg, method = "spearman"))
  expect_match(unname(p$x$attrs[[1]]$text)[1, 1], "spearman correlation", fixed = TRUE)
})

# ========== Test argument mismatch ==========
test_that("plot_corr_matrix validates use argument", {
  expect_error(
    plot_corr_matrix(mtcars[, 1:4], use = "something.random"),
    "'arg' should be one of"
  )
})

test_that("plot_corr_matrix validates method argument", {
  expect_error(
    plot_corr_matrix(mtcars, method = "kendall"),
    "'arg' should be one of"
  )
})

# ========== Test additional plotly arguments ==========
test_that("plot_corr_matrix allows additional plotly arguments through dots", {
  expect_s3_class(
    plot_corr_matrix(mtcars,
                     showscale = FALSE,
                     zmin = -0.5,
                     zmax = 0.5),
    "plotly"
  )
})

test_that("plot_corr_matrix plotly dots can override heatmap arguments", {
  p <- plot_corr_matrix(mtcars[, 1:4],
                        showscale = FALSE,
                        zmin = -0.5,
                        zmax = 0.5)

  expect_s3_class(p, "plotly")
  expect_false(p$x$attrs[[1]]$showscale)
  expect_equal(p$x$attrs[[1]]$zmin, -0.5)
  expect_equal(p$x$attrs[[1]]$zmax, 0.5)
})
