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
  repos <- c(
    CRAN = "https://cloud.r-project.org",
    myuniverse = "https://animovement.r-universe.dev"
  )
  withr::local_options(repos = repos)

  expect_equal(animovement_repos(), repos)
})

test_that("animovement_repos() resolves the @CRAN@ placeholder", {
  # A fresh R install leaves this in place, and available.packages() aborts on
  # it with "trying to use CRAN without setting a mirror".
  withr::local_options(repos = c(CRAN = "@CRAN@"))

  out <- animovement_repos()

  expect_false(any(out == "@CRAN@"))
  expect_true(any(out == "https://animovement.r-universe.dev"))
  expect_named(out, c("animovement", "CRAN"))
})

test_that("animovement_repos() keeps CRAN, since dependencies live there", {
  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))

  expect_true("https://cloud.r-project.org" %in% animovement_repos())
})

test_that("the update entry points default to a repos containing R-universe", {
  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))

  for (fn in list(animovement_deps, animovement_update, animovement_install)) {
    default <- eval(formals(fn)$repos)
    expect_true("https://animovement.r-universe.dev" %in% default)
  }
})

test_that("animovement_deps() aborts when no requested package is in repos", {
  repo <- local_empty_repo()

  expect_error(
    animovement_deps(
      pkg = c("aniframe", "aniread"),
      repos = repo,
      check.deps = FALSE
    ),
    "not available in the configured repositories"
  )
})

test_that("the abort names the repositories searched and points at R-universe", {
  repo <- local_empty_repo()

  err <- tryCatch(
    animovement_deps(pkg = "aniframe", repos = repo, check.deps = FALSE),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, repo, fixed = TRUE)
  expect_match(err, "animovement.r-universe.dev", fixed = TRUE)
})

test_that("animovement_update() surfaces that error rather than 'behind' not found", {
  repo <- local_empty_repo()

  # Regression test for animovement#144: animovement_deps() used to return NULL
  # here, so subset(deps, behind) failed with "object 'behind' not found".
  expect_error(
    animovement_update(
      pkg = "aniframe",
      repos = repo,
      check.deps = FALSE
    ),
    "not available in the configured repositories"
  )
})

test_that("a partial match warns but still returns a data frame", {
  repo <- local_empty_repo()

  expect_warning(
    deps <- animovement_deps(
      pkg = c("notanimovement", "aniframe"),
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

  out <- capture.output(
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
