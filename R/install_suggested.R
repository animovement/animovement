#' Download all suggested packages
#'
#' In `animovement`, we have a minimal dependency policy to keep the package
#' light and fast to install. However, we rely on several packages for testing
#' and specific features. These "soft dependencies" can be downloaded at once
#' using this function, allowing you to fully utilize all of animovement's
#' functionalities without errors.
#'
#' @param package Character string specifying the package name. Currently only
#'   `"animovement"` is supported.
#'
#' @details To reduce the dependency load, animovement by default will not
#'   download all internally needed packages. It will ask the user to download
#'   them only if they are needed. The current function can help install all
#'   packages that animovement and its ecosystem packages might need.
#'   `animovement_show_suggested()` is a convenient helper to show the current
#'   list of suggested packages.
#'
#' This function will check the `Suggests` field of animovement and all its
#' imported packages (aniframe, aniread, aniprocess, animetric, anicheck, anivis),
#' excluding packages only needed for development or documentation workflows
#' (knitr, rmarkdown, testthat, pak, here, covr, pkgdown).
#'
#' Under WebR, packages are installed with `webr::install()`, since
#' `utils::install.packages()` cannot build Emscripten packages in the browser.
#' Otherwise, if `{pak}` is installed then `pak::pkg_install()` is used, and
#' failing that `utils::install.packages()`. Repositories searched are CRAN, the
#' animovement R-universe and the Bioconductor R-universe -- the last of which
#' is where `rhdf5` comes from.
#'
#' @return Invisible `NULL`. Used for side-effect of installing packages.
#'
#' @examples
#' # download all suggested packages
#' if (FALSE) {
#'   animovement_install_suggested("animovement")
#' }
#'
#' # listing all soft/weak dependencies
#' animovement_show_suggested()
#'
#' @export
animovement_install_suggested <- function(package = "animovement") {
  suggested_packages <- .get_all_suggested(package)

  if (is.null(suggested_packages) || length(suggested_packages) == 0) {
    cli::cli_alert_info("No suggested packages found.")
    return(invisible(NULL))
  }

  # install only the packages not yet installed
  installed_packages <- suggested_packages %in% .installed_packages()

  if (all(installed_packages)) {
    cli::cli_alert_success("All suggested packages are already installed.")
  } else {
    to_install <- suggested_packages[!installed_packages]

    cli::cli_alert_info(
      "Installing {length(to_install)} package{?s}: {.pkg {to_install}}"
    )

    # pak is preferred here because it resolves Bioconductor (rhdf5) itself.
    .install_packages(to_install, repos = .suggested_repos(), use_pak = TRUE)
  }

  invisible(NULL)
}


#' @rdname animovement_install_suggested
#' @export
animovement_show_suggested <- function(package = "animovement") {
  all_packages <- .get_animovement_packages()

  cli::cli_h2("Suggested packages for animovement ecosystem")

  for (pkg in all_packages) {
    suggested <- .find_suggested(pkg)
    suggested <- .exclude_dev_packages(suggested)

    if (!is.null(suggested) && length(suggested) > 0) {
      cli::cli_text("{.field {pkg}}: {paste(suggested, collapse = ', ')}")
    }
  }

  all_suggested <- .get_all_suggested(package)
  invisible(all_suggested)
}


# Helper functions --------------------------------------------------------

# The suggested packages are spread across three places: CRAN, the animovement
# R-universe, and Bioconductor (rhdf5, which is on neither of the other two).
# `utils::install.packages()` needs all of them named explicitly; pak resolves
# Bioconductor by itself but still reads getOption("repos") for the rest.
bioc_universe <- "https://bioc.r-universe.dev"

#' @keywords internal
.suggested_repos <- function() {
  repos <- animovement_repos()
  if (!any(repos == bioc_universe)) {
    repos <- c(repos, bioc = bioc_universe)
  }
  repos
}


#' @keywords internal
.get_animovement_packages <- function() {
  c(
    "animovement",
    "aniframe",
    "anispace",
    "aniread",
    "aniprocess",
    "animetric",
    "anicheck",
    "anivis"
  )
}


#' @keywords internal
.get_all_suggested <- function(package) {
  all_packages <- .get_animovement_packages()

  # collect suggested packages from all animovement packages
  all_suggested <- character(0)

  for (pkg in all_packages) {
    suggested <- .find_suggested(pkg)
    suggested <- .exclude_dev_packages(suggested)
    if (!is.null(suggested)) {
      all_suggested <- c(all_suggested, suggested)
    }
  }

  # return unique packages, excluding animovement packages themselves
  unique(setdiff(all_suggested, all_packages))
}


#' @keywords internal
.exclude_dev_packages <- function(packages) {
  # Packages used only for development or documentation workflows, which
  # users of the ecosystem have no reason to install (animovement#143).
  dev_packages <- c(
    "knitr",
    "rmarkdown",
    "testthat",
    "pak",
    "here",
    "covr",
    "pkgdown"
  )
  animovement_packages <- .get_animovement_packages()
  setdiff(packages, c(dev_packages, animovement_packages))
}


#' @keywords internal
.find_suggested <- function(package) {
  # read suggests field from package description
  suggests <- tryCatch(
    suppressWarnings(utils::packageDescription(package)$Suggests),
    error = function(e) NULL
  )

  if (is.null(suggests)) {
    return(NULL)
  }

  # parse package names from Suggests field
  suggested_packages <- trimws(
    gsub("(\n|\\(.*\\))", "", unlist(strsplit(suggests, ",", fixed = TRUE)))
  )

  suggested_packages
}


#' @keywords internal
.installed_packages <- function() {
  rownames(utils::installed.packages())
}


#' @keywords internal
.check_if_installed <- function(package) {
  requireNamespace(package, quietly = TRUE)
}
