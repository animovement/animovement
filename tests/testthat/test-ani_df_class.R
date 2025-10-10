# Tests for ani_df constructor and creation functions

library(testthat)
library(dplyr)
library(cli)

# ---- Test ani_df() constructor ----

test_that("ani_df creates object with required columns", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  )

  expect_s3_class(df, "ani_df")
  expect_s3_class(df, "tbl_df")
  expect_true("time" %in% names(df))
  expect_true("x" %in% names(df))
  expect_true("y" %in% names(df))
})

test_that("ani_df requires positional data", {
  expect_error(
    ani_df(time = 1:10),
    "Missing positional data"
  )
})

test_that("ani_df requires time column", {
  expect_error(
    ani_df(x = 1:10, y = 1:10),
    "Missing a time column"
  )
})

test_that("ani_df accepts z coordinate", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    z = rnorm(10)
  )

  expect_true("z" %in% names(df))
})

test_that("ani_df creates default keypoint column", {
  expect_message(
    df <- ani_df(time = 1:10, x = rnorm(10), y = rnorm(10)),
    "No 'keypoint' column was found"
  )

  expect_true("keypoint" %in% names(df))
  expect_s3_class(df$keypoint, "factor")
  expect_equal(levels(df$keypoint), "centroid")
})

test_that("ani_df converts keypoint to factor", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    keypoint = rep(c("nose", "tail"), 5)
  )

  expect_s3_class(df$keypoint, "factor")
  expect_setequal(levels(df$keypoint), c("nose", "tail"))
})

test_that("ani_df creates default individual column", {
  expect_message(
    df <- ani_df(time = 1:10, x = rnorm(10), y = rnorm(10)),
    "No 'individual' column was found"
  )

  expect_true("individual" %in% names(df))
  expect_s3_class(df$individual, "factor")
})

test_that("ani_df converts individual to factor", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    individual = rep(1:2, 5)
  )

  expect_s3_class(df$individual, "factor")
})

test_that("ani_df converts numeric trial to integer", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    trial = 1
  )

  expect_type(df$trial, "integer")
})

test_that("ani_df converts character trial to factor", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    trial = "baseline"
  )

  expect_s3_class(df$trial, "factor")
  expect_equal(levels(df$trial), "baseline")
})

test_that("ani_df handles multiple character trial levels", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    trial = rep(c("baseline", "treatment"), 5)
  )

  expect_s3_class(df$trial, "factor")
  expect_setequal(levels(df$trial), c("baseline", "treatment"))
})

test_that("ani_df converts numeric session to integer", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    session = 2
  )

  expect_type(df$session, "integer")
})

test_that("ani_df converts character session to factor", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    session = "day1"
  )

  expect_s3_class(df$session, "factor")
  expect_equal(levels(df$session), "day1")
})

test_that("ani_df handles multiple character session levels", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    session = rep(c("morning", "afternoon"), 5)
  )

  expect_s3_class(df$session, "factor")
  expect_setequal(levels(df$session), c("morning", "afternoon"))
})

test_that("ani_df leaves non-numeric non-character trial unchanged", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    trial = factor(rep(c("a", "b"), 5))
  )

  expect_s3_class(df$trial, "factor")
})

test_that("ani_df leaves non-numeric non-character session unchanged", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    session = factor(rep(c("s1", "s2"), 5))
  )

  expect_s3_class(df$session, "factor")
})

test_that("ani_df relocates columns in correct order", {
  df <- ani_df(
    x = rnorm(10),
    time = 1:10,
    y = rnorm(10),
    trial = 1,
    session = 1
  )

  expected_order <- c("session", "trial", "individual", "keypoint", "time", "x", "y")
  expect_equal(names(df)[1:7], expected_order)
})

test_that("ani_df groups by appropriate columns", {
  df <- ani_df(
    time = 1:20,
    x = rnorm(20),
    y = rnorm(20),
    individual = rep(1:2, 10),
    trial = rep(1:2, each = 10)
  )

  expect_true(dplyr::is_grouped_df(df))
  expect_setequal(
    dplyr::group_vars(df),
    c("trial", "individual", "keypoint")
  )
})

test_that("ani_df arranges by groups", {
  df <- ani_df(
    time = 1:20,
    x = rnorm(20),
    y = rnorm(20),
    individual = rep(c(2, 1), 10),
    trial = rep(c(2, 1), each = 10)
  )

  # Should be arranged by trial, then individual
  expect_equal(df$trial[1], 1L)
  expect_equal(df$individual[1], factor(1, levels = c(1,2)))
})

# ---- Test is_ani_df() ----

test_that("is_ani_df identifies ani_df objects", {
  df <- ani_df(time = 1:10, x = rnorm(10), y = rnorm(10))
  expect_true(is_ani_df(df))
})

test_that("is_ani_df returns FALSE for non-ani_df", {
  df <- tibble(time = 1:10, x = rnorm(10))
  expect_false(is_ani_df(df))
  expect_false(is_ani_df(data.frame(x = 1)))
  expect_false(is_ani_df(list()))
})

# ---- Test example_tbl() ----

test_that("example_tbl creates valid ani_df", {
  df <- example_tbl()

  expect_s3_class(df, "ani_df")
  expect_equal(nrow(df), 50)
  expect_true(all(c("individual", "time", "x", "y", "trial") %in% names(df)))
})

test_that("example_tbl has expected structure", {
  df <- example_tbl()

  expect_equal(length(unique(df$individual)), 2)
  expect_equal(unique(df$trial), 1L)
})

# ---- Test tbl_sum.ani_df() ----

test_that("tbl_sum.ani_df returns custom summary", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    individual = rep(1:2, 5),
    trial = 1
  )

  summary <- pillar::tbl_sum(df)

  expect_true("Individuals" %in% names(summary))
  expect_true("Keypoints" %in% names(summary))
  expect_true("Trials" %in% names(summary))
})

test_that("tbl_sum.ani_df includes sessions when present", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    session = rep(1:2, 5)
  )

  summary <- pillar::tbl_sum(df)
  expect_true("Sessions" %in% names(summary))
})

test_that("tbl_sum.ani_df handles multiple individuals", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    individual = rep(1:3, length.out = 10)
  )

  summary <- pillar::tbl_sum(df)
  expect_match(summary["Individuals"], "1.*2.*3")
})

# ---- Test as_ani_df() ----

test_that("as_ani_df converts compatible data frames", {
  df <- tibble(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  )

  ani <- suppressMessages(as_ani_df(df))
  expect_s3_class(ani, "ani_df")
})

# ---- Test edge cases ----

test_that("ani_df handles single row", {
  df <- ani_df(
    time = 1,
    x = 0,
    y = 0
  )

  expect_equal(nrow(df), 1)
  expect_s3_class(df, "ani_df")
})

test_that("ani_df handles large datasets", {
  n <- 10000
  df <- ani_df(
    time = 1:n,
    x = rnorm(n),
    y = rnorm(n)
  )

  expect_equal(nrow(df), n)
  expect_s3_class(df, "ani_df")
})

test_that("ani_df preserves additional columns", {
  df <- ani_df(
    time = 1:10,
    x = rnorm(10),
    y = rnorm(10),
    custom_col = letters[1:10]
  )

  expect_true("custom_col" %in% names(df))
  expect_equal(df$custom_col, letters[1:10])
})
