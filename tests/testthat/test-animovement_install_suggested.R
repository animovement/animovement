# Tests for animovement_install_suggested
# ----------------------------------------
# 1. animovement_install_suggested returns invisible NULL
# 2. Shows success message when all packages are installed
# 3. Shows info message when no suggested packages found
# 4. Uses pak::pkg_install when pak is available
# 5. Falls back to utils::install.packages when pak is unavailable
# 6. WebR compatibility: r-wasm repo is included when pak unavailable
# 7. Only installs packages that are not yet installed
#
# Tests for animovement_show_suggested
# ------------------------------------
# 8. Returns all suggested packages invisibly
# 9. Excludes dev packages from results
#
# Tests for helper functions
# --------------------------
# 10. .get_animovement_packages returns expected packages
# 11. .exclude_dev_packages removes dev and animovement packages
# 12. .find_suggested parses Suggests field correctly
# 13. .get_all_suggested excludes animovement packages from results

test_that("animovement_install_suggested returns invisible NULL", {
  local_mocked_bindings(
    .get_all_suggested = function(pkg) character(0),
    .package = "animovement"
  )

  result <- animovement_install_suggested("animovement")
  expect_null(result)
  expect_invisible(animovement_install_suggested("animovement"))
})

test_that("shows success message when all packages installed", {
  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr", "ggplot2"),
    .installed_packages = function() c("dplyr", "ggplot2", "base"),
    .package = "animovement"
  )

  expect_message(
    animovement_install_suggested("animovement"),
    "All suggested packages are already installed"
  )
})

test_that("shows info message when no suggested packages found", {
  local_mocked_bindings(
    .get_all_suggested = function(pkg) character(0),
    .package = "animovement"
  )

  expect_message(
    animovement_install_suggested("animovement"),
    "No suggested packages found"
  )
})

test_that("uses pak::pkg_install when pak is available", {
  install_called <- FALSE
  captured_repos <- NULL

  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr", "ggplot2"),
    .installed_packages = function() "base",
    .check_if_installed = function(pkg) pkg == "pak",
    .package = "animovement"
  )

  local_mocked_bindings(
    pkg_install = function(pkgs, repos) {
      install_called <<- TRUE
      captured_repos <<- repos
    },
    .package = "pak"
  )

  suppressMessages(animovement_install_suggested("animovement"))

  expect_true(install_called)
  expect_true("https://animovement.r-universe.dev" %in% captured_repos)
})

test_that("falls back to install.packages when pak unavailable", {
  install_called <- FALSE
  captured_repos <- NULL

  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr", "ggplot2"),
    .installed_packages = function() "base",
    .check_if_installed = function(pkg) FALSE,
    .package = "animovement"
  )

  local_mocked_bindings(
    install.packages = function(pkgs, repos) {
      install_called <<- TRUE
      captured_repos <<- repos
    },
    .package = "utils"
  )

  suppressMessages(animovement_install_suggested("animovement"))

  expect_true(install_called)
})

test_that("r-wasm repo is included when pak unavailable (WebR compatibility)", {
  captured_repos <- NULL

  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr"),
    .installed_packages = function() "base",
    .check_if_installed = function(pkg) FALSE,
    .package = "animovement"
  )

  local_mocked_bindings(
    install.packages = function(pkgs, repos) {
      captured_repos <<- repos
    },
    .package = "utils"
  )

  suppressMessages(animovement_install_suggested("animovement"))

  expect_true(
    "https://repo.r-wasm.org" %in% captured_repos,
    info = "r-wasm repo must be included for WebR compatibility"
  )
})

test_that("only installs packages that are not yet installed", {
  captured_packages <- NULL

  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr", "ggplot2", "tidyr"),
    .installed_packages = function() c("dplyr", "base"),
    .check_if_installed = function(pkg) FALSE,
    .package = "animovement"
  )

  local_mocked_bindings(
    install.packages = function(pkgs, repos) {
      captured_packages <<- pkgs
    },
    .package = "utils"
  )

  suppressMessages(animovement_install_suggested("animovement"))

  expect_equal(sort(captured_packages), c("ggplot2", "tidyr"))
  expect_false("dplyr" %in% captured_packages)
})

test_that("animovement_show_suggested returns suggested packages invisibly", {
  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr", "ggplot2"),
    .package = "animovement"
  )

  expect_invisible(
    result <- suppressMessages(animovement_show_suggested("animovement"))
  )
  expect_equal(sort(result), c("dplyr", "ggplot2"))
})

test_that(".get_animovement_packages returns expected packages", {
  pkgs <- .get_animovement_packages()

  expect_true("animovement" %in% pkgs)
  expect_true("aniframe" %in% pkgs)
  expect_true("aniread" %in% pkgs)
  expect_true("aniprocess" %in% pkgs)
  expect_true("animetric" %in% pkgs)
  expect_true("anicheck" %in% pkgs)
  expect_true("anivis" %in% pkgs)
})

test_that(".exclude_dev_packages removes dev and animovement packages", {
  input <- c(
    "dplyr",
    "knitr",
    "testthat",
    "rmarkdown",
    "pak",
    "aniframe",
    "ggplot2"
  )
  result <- .exclude_dev_packages(input)

  expect_equal(sort(result), c("dplyr", "ggplot2"))
  expect_false("knitr" %in% result)
  expect_false("testthat" %in% result)
  expect_false("pak" %in% result)
  expect_false("aniframe" %in% result)
})

test_that(".find_suggested parses Suggests field correctly", {
  skip_if_not_installed("animovement")

  result <- .find_suggested("animovement")

  expect_type(result, "character")
  expect_true(length(result) >= 0)
})

test_that(".find_suggested returns NULL for non-existent package", {
  result <- .find_suggested("nonexistent_package_12345")
  expect_null(result)
})

test_that(".get_all_suggested excludes animovement ecosystem packages", {
  local_mocked_bindings(
    .find_suggested = function(pkg) {
      if (pkg == "animovement") {
        c("dplyr", "aniframe", "ggplot2")
      } else {
        NULL
      }
    },
    .package = "animovement"
  )

  result <- .get_all_suggested("animovement")

  expect_false("aniframe" %in% result)
  expect_false("animovement" %in% result)
  expect_true("dplyr" %in% result)
  expect_true("ggplot2" %in% result)
})
