# Tests for animovement_deps() / animovement_update() repository handling
#
# The animovement packages live on R-universe, not CRAN, so a default `repos`
# finds none of them. These tests use a local file:// repository containing a
# single unrelated package, which reproduces that situation without network.

local_empty_repo <- function(envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  contrib <- file.path(dir, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  writeLines(
    c("Package: notanimovement", "Version: 1.0.0", ""),
    file.path(contrib, "PACKAGES")
  )
  paste0("file:///", gsub("^/", "", normalizePath(dir, winslash = "/")))
}

test_that("animovement_repos() adds the R-universe by default", {
  skip_if_not_installed("withr")

  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))

  expect_equal(
    animovement_repos(),
    c(
      animovement = "https://animovement.r-universe.dev",
      CRAN = "https://cloud.r-project.org"
    )
  )
})

test_that("animovement_repos() does not duplicate an R-universe already present", {
  skip_if_not_installed("withr")

  repos <- c(
    CRAN = "https://cloud.r-project.org",
    myuniverse = "https://animovement.r-universe.dev"
  )
  withr::local_options(repos = repos)

  expect_equal(animovement_repos(), repos)
})

test_that("animovement_repos() resolves the @CRAN@ placeholder", {
  skip_if_not_installed("withr")

  # A fresh R install leaves this in place, and available.packages() aborts on
  # it with "trying to use CRAN without setting a mirror".
  withr::local_options(repos = c(CRAN = "@CRAN@"))

  out <- animovement_repos()

  expect_false(any(out == "@CRAN@"))
  expect_true(any(out == "https://animovement.r-universe.dev"))
  expect_named(out, c("animovement", "CRAN"))
})

test_that("animovement_repos() keeps CRAN, since dependencies live there", {
  skip_if_not_installed("withr")

  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))

  expect_true("https://cloud.r-project.org" %in% animovement_repos())
})

test_that("the update entry points default to a repos containing R-universe", {
  skip_if_not_installed("withr")

  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))

  for (fn in list(animovement_deps, animovement_update, animovement_install)) {
    default <- eval(formals(fn)$repos)
    expect_true("https://animovement.r-universe.dev" %in% default)
  }
})

test_that("animovement_deps() aborts when no requested package is in repos", {
  skip_if_not_installed("withr")

  repo <- local_empty_repo()

  expect_error(
    animovement_deps(
      pkg = c("anicore", "aniread"),
      repos = repo,
      check.deps = FALSE
    ),
    "not available in the configured repositories"
  )
})

test_that("the abort names the repositories searched and points at R-universe", {
  skip_if_not_installed("withr")

  repo <- local_empty_repo()

  err <- tryCatch(
    animovement_deps(pkg = "anicore", repos = repo, check.deps = FALSE),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, repo, fixed = TRUE)
  expect_match(err, "animovement.r-universe.dev", fixed = TRUE)
})

test_that("animovement_update() surfaces that error rather than 'behind' not found", {
  skip_if_not_installed("withr")

  repo <- local_empty_repo()

  # Regression test for animovement#144: animovement_deps() used to return NULL
  # here, so subset(deps, behind) failed with "object 'behind' not found".
  expect_error(
    animovement_update(
      pkg = "anicore",
      repos = repo,
      check.deps = FALSE
    ),
    "not available in the configured repositories"
  )
})

test_that("a partial match warns but still returns a data frame", {
  skip_if_not_installed("withr")

  repo <- local_empty_repo()

  expect_warning(
    deps <- animovement_deps(
      pkg = c("notanimovement", "anicore"),
      repos = repo,
      check.deps = FALSE
    ),
    "not available in the configured repositories"
  )

  expect_s3_class(deps, "data.frame")
  expect_equal(deps$package, "notanimovement")
  expect_named(deps, c("package", "cran", "local", "behind"))
})

test_that("non-animovement packages get no R-universe hint", {
  skip_if_not_installed("withr")

  repo <- local_empty_repo()

  err <- tryCatch(
    animovement_deps(pkg = "somethingelse", repos = repo, check.deps = FALSE),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "not available in the configured repositories")
  expect_false(grepl("r-universe", err, fixed = TRUE))
})

test_that("the printed install command carries repos so it can be pasted", {
  repos <- c(animovement = "https://animovement.r-universe.dev")

  out <- capture_messages(
    animovement_install(
      "definitelynotarealpackage",
      install = FALSE,
      repos = repos
    )
  )

  expect_match(
    paste(out, collapse = ""),
    "repos = ",
    fixed = TRUE
  )
  expect_match(
    paste(out, collapse = ""),
    "animovement.r-universe.dev",
    fixed = TRUE
  )
})

test_that("the install command respects animovement.quiet", {
  skip_if_not_installed("withr")

  withr::local_options(animovement.quiet = TRUE)

  expect_silent(
    animovement_install(
      "definitelynotarealpackage",
      install = FALSE,
      repos = c(animovement = "https://animovement.r-universe.dev")
    )
  )
})

# A local repository listing the given packages at the given versions, so
# animovement_deps() and its callers run without network access.
local_repo <- function(versions, envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = envir)
  contrib <- file.path(dir, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  writeLines(
    paste0("Package: ", names(versions), "\nVersion: ", versions, "\n"),
    file.path(contrib, "PACKAGES")
  )
  paste0("file:///", gsub("^/", "", normalizePath(dir, winslash = "/")))
}

test_that("animovement_update() reports through messages, not stdout", {
  skip_if_not_installed("withr")

  repo <- local_repo(c(anicore = "0.0.1", aniread = "999.0.0"))

  expect_output(
    msgs <- capture_messages(
      animovement_update(
        pkg = c("anicore", "aniread"),
        repos = repo,
        check.deps = FALSE
      )
    ),
    NA
  )
  msgs <- paste(msgs, collapse = "")
  expect_match(msgs, "The following packages are out of date", fixed = TRUE)
  expect_match(msgs, "aniread", fixed = TRUE)
  expect_match(msgs, "Start a clean R session then run:", fixed = TRUE)

  withr::local_options(animovement.quiet = TRUE)
  expect_silent(
    animovement_update(
      pkg = c("anicore", "aniread"),
      repos = repo,
      check.deps = FALSE
    )
  )
})

test_that("animovement_update() says when everything is up to date", {
  skip_if_not_installed("withr")

  repo <- local_repo(c(anicore = "0.0.1"))

  expect_message(
    animovement_update(pkg = "anicore", repos = repo, check.deps = FALSE),
    "All animovement packages up-to-date"
  )
})

test_that("animovement_sitrep() returns an animovement_sitrep object", {
  skip_if_not_installed("withr")

  withr::local_options(animovement.extend = NULL)
  pkg <- animovement_packages(include.self = FALSE)
  versions <- stats::setNames(rep("0.0.1", length(pkg)), pkg)
  versions[[1]] <- "999.0.0"
  repo <- local_repo(versions)

  report <- animovement_sitrep(repos = repo, check.deps = FALSE)

  expect_s3_class(report, "animovement_sitrep")
  expect_named(
    report,
    c("animovement_version", "r_version", "project_config", "packages")
  )
  expect_identical(
    report$animovement_version,
    as.character(packageVersion("animovement"))
  )
  expect_identical(report$r_version, getRversion())
  expect_type(report$project_config, "logical")
  expect_named(
    report$packages,
    c("package", "cran", "local", "behind", "group")
  )
  expect_setequal(report$packages$package, pkg)
  expect_true(all(report$packages$group == "core"))
  expect_true(report$packages$behind[report$packages$package == pkg[[1]]])
})

test_that("printing an animovement_sitrep gives the situation report", {
  skip_if_not_installed("withr")

  withr::local_options(animovement.extend = NULL, animovement.styling = FALSE)
  pkg <- animovement_packages(include.self = FALSE)
  repo <- local_repo(stats::setNames(rep("0.0.1", length(pkg)), pkg))

  report <- animovement_sitrep(repos = repo, check.deps = FALSE)

  expect_output(print(report), "Situation Report", fixed = TRUE)
  expect_output(print(report), "Core packages", fixed = TRUE)
  expect_output(print(report), "Project config file", fixed = TRUE)
  expect_output(print(report), pkg[[1]], fixed = TRUE)
  capture.output(vis <- withVisible(print(report)))
  expect_false(vis$visible)
  expect_identical(vis$value, report)
})
