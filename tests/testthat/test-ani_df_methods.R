# Tests for ani_df methods

library(testthat)
library(dplyr)
library(cli)

# Helper function to create test data
create_test_ani_df <- function() {
  suppressMessages(
    ani_df(
      time = 1:20,
      x = rnorm(20),
      y = rnorm(20),
      individual = rep(1:2, 10),
      trial = rep(1:2, each = 10)
    )
  )
}

# ---- Test dplyr verb methods ----

test_that("mutate preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df |> mutate(z = x + y)

  expect_s3_class(result, "ani_df")
  expect_true("z" %in% names(result))
})

test_that("filter preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df |> filter(time > 10)

  expect_s3_class(result, "ani_df")
  expect_true(all(result$time > 10))
})

test_that("select preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df |> select(time, x, y)

  expect_s3_class(result, "ani_df")
  expect_equal(ncol(result), 6)
})

test_that("arrange preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df |> arrange(desc(time))

  expect_s3_class(result, "ani_df")
  expect_equal(result$time[1], 20)
})

test_that("rename preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df |> rename(position_x = x)

  expect_s3_class(result, "ani_df")
  expect_true("position_x" %in% names(result))
  expect_false("x" %in% names(result))
})

test_that("relocate preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df |> relocate(y, .before = x)

  expect_s3_class(result, "ani_df")
  expect_true(which(names(result) == "y") < which(names(result) == "x"))
})

test_that("slice preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df |> slice(1:2) # It slices per group

  expect_s3_class(result, "ani_df")
  expect_equal(nrow(result), 8)
})

test_that("group_by preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df |> group_by(trial)

  expect_s3_class(result, "ani_df")
  expect_true(dplyr::is_grouped_df(result))
})

test_that("ungroup preserves ani_df class", {
  df <- create_test_ani_df()

  expect_warning(
    result <- df |> ungroup(),
    "Ungrouping an ani_df"
  )

  expect_s3_class(result, "ani_df")
  expect_false(dplyr::is_grouped_df(result))
})

test_that("ungroup with quiet=TRUE suppresses warning", {
  df <- create_test_ani_df()

  expect_silent(
    result <- df |> ungroup(quiet = TRUE)
  )

  expect_s3_class(result, "ani_df")
})

# ---- Test method chaining ----

test_that("multiple dplyr operations preserve ani_df class", {
  df <- create_test_ani_df()

  result <- df |>
    filter(time > 5) |>
    mutate(z = x * 2) |>
    select(time, x, y, z) |>
    arrange(time)

  expect_s3_class(result, "ani_df")
  expect_true("z" %in% names(result))
  expect_true(all(result$time > 5))
})

# ---- Test base R extraction methods ----

test_that("[ extraction preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df[1:5, ]

  expect_s3_class(result, "ani_df")
  expect_equal(nrow(result), 5)
})

test_that("[ column extraction preserves ani_df class", {
  df <- create_test_ani_df()
  result <- df[, c("time", "x", "y")]

  expect_s3_class(result, "ani_df")
  expect_equal(ncol(result), 3)
})

test_that("[[ extraction returns vector", {
  df <- create_test_ani_df()
  result <- df[["time"]]

  expect_type(result, "integer")
  expect_false(inherits(result, "ani_df"))
})

test_that("$ extraction returns vector", {
  df <- create_test_ani_df()
  result <- df$time

  expect_type(result, "integer")
  expect_false(inherits(result, "ani_df"))
})

# ---- Test base R assignment methods ----

test_that("[<- preserves ani_df class", {
  df <- create_test_ani_df()
  df[1, "x"] <- 999

  expect_s3_class(df, "ani_df")
  expect_equal(df$x[1], 999)
})

test_that("[[<- preserves ani_df class", {
  df <- create_test_ani_df()
  df[["x"]] <- rnorm(20)

  expect_s3_class(df, "ani_df")
  expect_equal(length(df$x), 20)
})

test_that("$<- preserves ani_df class", {
  df <- create_test_ani_df()
  df$new_col <- 1:20

  expect_s3_class(df, "ani_df")
  expect_true("new_col" %in% names(df))
})

test_that("names<- preserves ani_df class", {
  df <- create_test_ani_df()
  original_names <- names(df)
  new_names <- paste0("col_", seq_along(original_names))
  names(df) <- new_names

  expect_s3_class(df, "ani_df")
  expect_equal(names(df), new_names)
})

# ---- Test conversion methods ----

test_that("as.data.frame removes ani_df class", {
  df <- create_test_ani_df()
  result <- as.data.frame(df)

  expect_false(inherits(result, "ani_df"))
  expect_s3_class(result, "data.frame")
})

test_that("as.data.frame preserves data", {
  df <- create_test_ani_df()
  result <- as.data.frame(df)

  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
  expect_equal(result$time, df$time)
})

# ---- Test edge cases ----

test_that("methods work with single row ani_df", {
  df <- suppressMessages(
    ani_df(time = 1, x = 0, y = 0)
  )

  result <- df |> mutate(z = 1)
  expect_s3_class(result, "ani_df")
  expect_equal(nrow(result), 1)
})

test_that("methods work with minimal columns", {
  df <- suppressMessages(
    ani_df(time = 1:10, x = 1:10, y = 1:10)
  )

  result <- df |> filter(x > 5)
  expect_s3_class(result, "ani_df")
  expect_true(all(result$x > 5))
})

test_that("select can remove required columns (loses validation)", {
  df <- create_test_ani_df()

  # This should work but results in invalid ani_df
  result <- df |> select(time, x)

  expect_s3_class(result, "ani_df")
  expect_false("y" %in% names(result))
})

test_that("mutate can modify factor columns", {
  df <- create_test_ani_df()

  result <- df |> mutate(individual = factor(as.character(individual)))

  expect_s3_class(result, "ani_df")
  expect_s3_class(result$individual, "factor")
})

test_that("filter with all FALSE removes all rows", {
  df <- create_test_ani_df()
  result <- df |> filter(time > 1000)

  expect_s3_class(result, "ani_df")
  expect_equal(nrow(result), 0)
})

test_that("arrange with .by_group works", {
  df <- create_test_ani_df()

  result <- df |>
    arrange(desc(time), .by_group = TRUE)

  expect_s3_class(result, "ani_df")
})

# ---- Test grouped operations ----

test_that("grouped mutate preserves ani_df class", {
  df <- create_test_ani_df()

  result <- df |>
    group_by(trial) |>
    mutate(mean_x = mean(x))

  expect_s3_class(result, "ani_df")
  expect_true("mean_x" %in% names(result))
})

test_that("grouped filter preserves ani_df class", {
  df <- create_test_ani_df()

  result <- df |>
    group_by(trial) |>
    filter(x > mean(x))

  expect_s3_class(result, "ani_df")
})

test_that("grouped summarise creates tibble (not ani_df)", {
  df <- create_test_ani_df()

  result <- df |>
    group_by(trial) |>
    summarise(mean_x = mean(x))

  # Summarise should not preserve ani_df since structure changes fundamentally
  expect_s3_class(result, "tbl_df")
})

# ---- Test complex scenarios ----

test_that("methods preserve grouping structure", {
  df <- create_test_ani_df()

  result <- df |>
    mutate(z = x + y) |>
    filter(time > 5)

  expect_true(dplyr::is_grouped_df(result))
  expect_equal(dplyr::group_vars(result), dplyr::group_vars(df))
})

test_that("multiple assignment operations preserve class", {
  df <- create_test_ani_df()

  df$new1 <- 1
  df[["new2"]] <- 2
  df[1, "x"] <- 0

  expect_s3_class(df, "ani_df")
  expect_true(all(c("new1", "new2") %in% names(df)))
})

test_that("methods work after ungroup", {
  df <- create_test_ani_df()

  result <- suppressWarnings(df |> ungroup()) |>
    mutate(z = 1) |>
    filter(time > 5)

  expect_s3_class(result, "ani_df")
  expect_false(dplyr::is_grouped_df(result))
})

test_that("slice variants preserve ani_df class", {
  df <- create_test_ani_df()

  result1 <- df |> slice_head(n = 5)
  result2 <- df |> slice_tail(n = 5)
  result3 <- df |> slice_sample(n = 5)

  expect_s3_class(result1, "ani_df")
  expect_s3_class(result2, "ani_df")
  expect_s3_class(result3, "ani_df")
})
