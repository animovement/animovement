# Tests for .onAttach
# --------------------
# 1. Attaches the packages check_attached() reports as missing
#
# Tests for animovement_detach
# ----------------------------
# 2. Without arguments, it asks check_attached() for the attached packages
# 3. session = TRUE clears the session options

test_that(".onAttach() attaches the packages check_attached() reports as missing", {
  skip_if_not_installed("withr")
  withr::local_dir(withr::local_tempdir())
  withr::local_options(animovement.install = NULL, animovement.quiet = TRUE)
  attached <- NULL
  local_mocked_bindings(
    check_attached = function(needed = TRUE) "anicore",
    animovement_attach = function(to_load, ...) attached <<- to_load
  )

  .onAttach("", "animovement")

  expect_equal(attached, "anicore")
})

test_that("animovement_detach() asks check_attached() for the attached packages", {
  needed <- NULL
  local_mocked_bindings(
    check_attached = function(needed = TRUE) {
      needed <<- needed
      character()
    }
  )

  expect_null(animovement_detach(include.self = FALSE))
  expect_false(needed)
})

test_that("animovement_detach(session = TRUE) clears the session options", {
  skip_if_not_installed("withr")
  local_mocked_bindings(check_attached = function(needed = TRUE) character())
  withr::local_options(
    animovement.extend = "dplyr",
    animovement.quiet = TRUE
  )

  animovement_detach(include.self = FALSE, session = TRUE)

  expect_null(getOption("animovement.extend"))
  # The package's own options stay when include.self = FALSE
  expect_true(getOption("animovement.quiet"))
})
