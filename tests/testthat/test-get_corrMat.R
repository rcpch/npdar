
# ========== Test df requirements ==========
test_that("get_corrMat requires numeric columns only", {
  expect_error(
    get_corrMat(data.frame(x = 1:5,
                           y = letters[1:5])),
    "All columns in `data` must be numeric"
  )
})

test_that("get_corrMat requires at least two columns", {
  expect_error(
    get_corrMat(mtcars["mpg"]),
    "`data` must contain at least two numeric columns"
  )
})

test_that("get_corrMat accepts numeric matrix input", {
  mat <- as.matrix(mtcars[, 1:4])

  expect_s3_class(
    get_corrMat(mat),
    "plotly"
  )
})

test_that("get_corrMat handles constant columns without error", {
  df <- data.frame(
    x = 1:10,
    y = rep(1, 10),
    z = 10:1
  )

  expect_warning(
    p <- get_corrMat(df),
    "standard deviation is zero"
  )

  expect_s3_class(
    get_corrMat(df),
    "plotly"
  )
})

# ========== Test digits handling ==========
test_that("get_corrMat validates digits argument", {
  expect_error(
    get_corrMat(mtcars[, 1:4], digits = -1),
    "`digits` must be a single non-negative whole number"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], digits = 1.5),
    "`digits` must be a single non-negative whole number"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], digits = c(1, 2)),
    "`digits` must be a single non-negative whole number"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], digits = NA),
    "`digits` must be a single non-negative whole number"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], digits = NA_real_),
    "`digits` must be a single non-negative whole number"
  )
})

# ========== Test show_stars & show_labels validation ==========
test_that("get_corrMat works when values and stars are hidden", {
  expect_s3_class(
    get_corrMat(
      mtcars[, 1:4],
      show_values = FALSE,
      show_stars = FALSE
    ),
    "plotly"
  )
})

test_that("get_corrMat validates show_stars argument", {
  expect_error(
    get_corrMat(mtcars[, 1:4], show_stars = "yes"),
    "`show_stars` must be TRUE or FALSE"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], show_stars = c(TRUE, FALSE)),
    "`show_stars` must be TRUE or FALSE"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], show_stars = NA),
    "`show_stars` must be TRUE or FALSE"
  )
})

test_that("get_corrMat validates show_values argument", {
  expect_error(
    get_corrMat(mtcars[, 1:4], show_values = "no"),
    "`show_values` must be TRUE or FALSE"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], show_values = c(TRUE, FALSE)),
    "`show_values` must be TRUE or FALSE"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], show_values = NA),
    "`show_values` must be TRUE or FALSE"
  )
})

# ========== Test colors validation ==========
test_that("get_corrMat validates colors argument", {
  expect_error(
    get_corrMat(mtcars[, 1:4], colors = "#FFFFFF"),
    "`colors` must contain at least two colours"
  )

  expect_error(
    get_corrMat(mtcars[, 1:4], colors = NA),
    "`colors` must contain at least two colours"
  )
})

# ========== Test NAs handling ==========
test_that("get_corrMat handles pairwise missing data", {
  df <- mtcars[, 1:4]
  df[1:3, "mpg"] <- NA

  expect_s3_class(
    get_corrMat(df, use = "pairwise.complete.obs"),
    "plotly")
})

test_that("get_corrMat errors with all.obs and missing values", {
  df <- mtcars[, 1:4]
  df[1, "mpg"] <- NA

  expect_error(
    get_corrMat(df, use = "all.obs"),
    "does not allow NA values")
})

test_that("get_corrMat warns when na.or.complete or complete.obs leaves no complete observations", {
  df <- data.frame(x = c(1, NA, NA),
                   y = c(NA, 2, NA),
                   z = c(NA, NA, 3))

  expect_error(
    get_corrMat(df, use = "na.or.complete"),
    "No complete observations available"
  )

  expect_error(
    get_corrMat(df, use = "complete.obs"),
    "No complete observations available"
  )
})

# ========== Test argument mismatch ==========
test_that("get_corrMat validates use argument", {
  expect_error(
    get_corrMat(mtcars[, 1:4], use = "something.random"),
    "'arg' should be one of"
  )
})

test_that("get_corrMat validates method argument", {
  expect_error(
    get_corrMat(mtcars, method = "kendall"),
    "'arg' should be one of"
  )
})




# ========== Test additional plotly arguments ==========
test_that("get_corrMat allows additional plotly arguments through dots", {
  expect_s3_class(
    get_corrMat(mtcars,
                showscale = FALSE,
                zmin = -0.5,
                zmax = 0.5),
    "plotly"
    )
})

test_that("get_corrMat plotly dots can override heatmap arguments", {
  p <- get_corrMat(mtcars[, 1:4],
                   showscale = FALSE,
                   zmin = -0.5,
                   zmax = 0.5)

  expect_s3_class(p, "plotly")
  expect_false(p$x$attrs[[1]]$showscale)
  expect_equal(p$x$attrs[[1]]$zmin, -0.5)
  expect_equal(p$x$attrs[[1]]$zmax, 0.5)
})
