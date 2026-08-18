# Tests for animovement_install_suggested
# ----------------------------------------
# 1. animovement_install_suggested returns invisible NULL
# 2. Shows success message when all packages are installed
# 3. Shows info message when no suggested packages found
# 4. Uses pak::pkg_install when pak is available, without a `repos` argument
# 5. Restores the repos option after the pak install
# 6. Includes the Bioconductor R-universe so rhdf5 resolves
# 7. Falls back to utils::install.packages when pak is unavailable
# 8. WebR installs go through webr::install(), with an r-wasm fallback
# 9. The r-wasm repo is not used outside WebR
# 10. Dev-only packages are excluded
# 11. Only installs packages that are not yet installed
#
# Tests for animovement_show_suggested
# ------------------------------------
# 12. Returns all suggested packages invisibly
# 13. Excludes dev packages from results
#
# Tests for helper functions
# --------------------------
# 14. .get_animovement_packages returns expected packages
# 15. .exclude_dev_packages removes dev and animovement packages
# 16. .find_suggested parses Suggests field correctly
# 17. .get_all_suggested excludes animovement packages from results

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
    # Deliberately mirrors the real pak::pkg_install() signature. It has no
    # `repos` argument, so passing one aborts with "unused argument"
    # (animovement#146) -- the previous mock invented one and so validated the
    # bug instead of catching it. pak reads getOption("repos") instead.
    pkg_install = function(
      pkg,
      lib = NULL,
      upgrade = FALSE,
      ask = interactive(),
      dependencies = NA
    ) {
      install_called <<- TRUE
      captured_repos <<- getOption("repos")
    },
    .package = "pak"
  )

  suppressMessages(animovement_install_suggested("animovement"))

  expect_true(install_called)
  expect_true("https://animovement.r-universe.dev" %in% captured_repos)
})

test_that("pak::pkg_install() really does not take a repos argument", {
  skip_if_not_installed("pak")

  # Guards the mock above against drifting from the real signature.
  expect_false("repos" %in% names(formals(pak::pkg_install)))
})

test_that("the repos option is restored after the pak install", {
  before <- getOption("repos")

  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr"),
    .installed_packages = function() "base",
    .check_if_installed = function(pkg) pkg == "pak",
    .package = "animovement"
  )
  local_mocked_bindings(
    pkg_install = function(
      pkg,
      lib = NULL,
      upgrade = FALSE,
      ask = interactive(),
      dependencies = NA
    ) {
      invisible(NULL)
    },
    .package = "pak"
  )

  suppressMessages(animovement_install_suggested("animovement"))

  expect_equal(getOption("repos"), before)
})

test_that("Bioconductor is included so rhdf5 can be installed", {
  captured_repos <- NULL

  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("rhdf5"),
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

  # rhdf5 is on neither CRAN nor the animovement R-universe.
  expect_true("https://bioc.r-universe.dev" %in% captured_repos)
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

test_that("WebR installs go through webr::install(), not install.packages()", {
  # animovement#139: install.packages() cannot build Emscripten packages in the
  # browser, so the r-wasm repo alone is not enough.
  webr_packages <- NULL

  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr"),
    .installed_packages = function() "base",
    .is_webr = function() TRUE,
    .install_webr = function(packages, repos) {
      webr_packages <<- packages
    },
    .package = "animovement"
  )
  local_mocked_bindings(
    install.packages = function(...) {
      stop("install.packages() must not be used under WebR")
    },
    .package = "utils"
  )

  suppressMessages(animovement_install_suggested("animovement"))

  expect_equal(webr_packages, "dplyr")
})

test_that("WebR without the webr package falls back to the r-wasm repo", {
  captured_repos <- NULL

  local_mocked_bindings(
    .check_if_installed = function(pkg) FALSE,
    .package = "animovement"
  )
  local_mocked_bindings(
    install.packages = function(pkgs, repos) {
      captured_repos <<- repos
    },
    .package = "utils"
  )

  .install_webr("dplyr", repos = c(CRAN = "https://cloud.r-project.org"))

  expect_true("https://repo.r-wasm.org" %in% captured_repos)
})

test_that("the r-wasm repo is not used outside WebR", {
  captured_repos <- NULL

  local_mocked_bindings(
    .get_all_suggested = function(pkg) c("dplyr"),
    .installed_packages = function() "base",
    .is_webr = function() FALSE,
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

  expect_false("https://repo.r-wasm.org" %in% captured_repos)
})

test_that("dev-only packages are excluded from installation", {
  # animovement#143
  expect_equal(
    .exclude_dev_packages(
      c("here", "covr", "pkgdown", "withr", "ragg", "circular", "sf")
    ),
    c("circular", "sf")
  )
})

test_that("animovement_update() and animovement_install() route through WebR too", {
  # The same browser constraint applies to every entry point that installs.
  for (body_fn in list(animovement_update, animovement_install)) {
    expect_true(
      any(grepl(".install_packages", deparse(body(body_fn)), fixed = TRUE))
    )
  }
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
