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
#' excluding development packages (knitr, rmarkdown, testthat).
#'
#' If package `{pak}` is installed, `pak::pkg_install()` will be used to install
#' packages. Otherwise, `utils::install.packages()` is used with the animovement
#' R-universe repository.
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

    if (.check_if_installed("pak")) {
      repos <- c(
        "https://animovement.r-universe.dev",
        "https://cloud.r-project.org"
      )
      pak::pkg_install(to_install, repos = repos)
    } else {
      repos <- c(
        "https://animovement.r-universe.dev",
        "https://cloud.r-project.org",
        "https://repo.r-wasm.org"
      )
      utils::install.packages(to_install, repos = repos)
    }
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
  dev_packages <- c("knitr", "rmarkdown", "testthat", "pak")
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
