# Tests for print.animovement_conflicts
# -------------------------------------
# 1. Returns its input invisibly, like every other print method (#159)
# 2. Prints the conflict message when there are conflicts
# 3. Prints nothing when there are none

test_that("print.animovement_conflicts returns its input invisibly", {
  x <- animovement_conflicts()

  result <- withVisible(print(x))

  expect_identical(result$value, x)
  expect_false(result$visible)
})

test_that("print.animovement_conflicts prints the conflict message", {
  x <- structure(
    list(filter = c("dplyr", "stats")),
    class = "animovement_conflicts"
  )

  expect_output(print(x), "filter")
})

test_that("print.animovement_conflicts prints nothing without conflicts", {
  x <- structure(list(), class = "animovement_conflicts")

  expect_output(print(x), NA)
})
