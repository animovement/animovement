# Tests for the colour helpers
# ----------------------------
# 1. Colour is suppressed when the destination cannot render it (#178)
# 2. animovement.styling overrides the detection in both directions
# 3. Every helper goes through the same gate

test_that("no colour when the destination cannot render it", {
  withr::local_options(animovement.styling = NULL, cli.num_colors = 1)

  expect_identical(kingsblue("animovement"), "animovement")
  expect_identical(bold("Attaching packages"), "Attaching packages")
})

test_that("colour when the destination can render it", {
  withr::local_options(animovement.styling = NULL, cli.num_colors = 256)

  expect_identical(kingsblue("x"), "\033[38;5;33mx\033[39m")
  expect_identical(bold("x"), "\033[1mx\033[22m")
})

test_that("animovement.styling = FALSE suppresses colour regardless", {
  withr::local_options(animovement.styling = FALSE, cli.num_colors = 256)

  expect_identical(kingsblue("x"), "x")
})

test_that("animovement.styling = TRUE forces colour regardless", {
  withr::local_options(animovement.styling = TRUE, cli.num_colors = 1)

  expect_identical(kingsblue("x"), "\033[38;5;33mx\033[39m")
})

test_that("every helper is gated on the same check", {
  helpers <- list(green, blue, magenta2, gold, kingsblue, grey70, red, bold)

  withr::local_options(animovement.styling = NULL, cli.num_colors = 1)
  expect_true(all(vapply(helpers, function(f) f("x") == "x", logical(1))))

  withr::local_options(animovement.styling = NULL, cli.num_colors = 256)
  expect_false(any(vapply(helpers, function(f) f("x") == "x", logical(1))))
})

test_that("the attach banner carries no escapes when colour is off", {
  withr::local_options(animovement.styling = NULL, cli.num_colors = 1)

  banner <- paste(
    capture.output(
      animovement_attach(.core_pkg, onattach = FALSE),
      type = "message"
    ),
    collapse = "\n"
  )

  expect_identical(banner, cli::ansi_strip(banner))
})
