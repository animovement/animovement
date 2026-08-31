.c <- function(...) as.character(substitute(c(...))[-1L])

is_attached <- function(x) paste0("package:", x) %in% search()
is_installed <- function(x) vapply(x, requireNamespace, TRUE, quietly = TRUE)

wasm_repo <- "https://repo.r-wasm.org"

# WebR runs R in the browser, where packages are Emscripten builds that cannot
# be compiled locally -- utils::install.packages() fails there, so installation
# has to go through webr::install(). This affects every entry point that
# installs, not just animovement_install_suggested().
#' @keywords internal
.is_webr <- function() {
  identical(R.version$os, "emscripten") ||
    grepl("emscripten", R.version$platform, fixed = TRUE)
}

# Single installation route for animovement_update(), animovement_install() and
# animovement_install_suggested(), so WebR and pak handling stay consistent.
#' @keywords internal
.install_packages <- function(packages, repos, use_pak = FALSE) {
  if (.is_webr()) {
    return(invisible(.install_webr(packages, repos)))
  }
  if (use_pak && .check_if_installed("pak")) {
    # pak::pkg_install() has no `repos` argument -- passing one aborts with
    # "unused argument". It reads getOption("repos") instead.
    old_repos <- options(repos = repos)
    on.exit(options(old_repos), add = TRUE)
    return(invisible(pak::pkg_install(packages)))
  }
  utils::install.packages(packages, repos = repos)
}

#' @keywords internal
.install_webr <- function(packages, repos) {
  if (.check_if_installed("webr")) {
    # webr only ever exists inside WebR, so it cannot go in Suggests. Reach it
    # without a `::` literal so R CMD check does not flag an undeclared package.
    webr_install <- get("install", envir = asNamespace("webr"))
    return(webr_install(packages))
  }
  # Without webr itself, the r-wasm repository is the next best thing.
  utils::install.packages(packages, repos = c(repos, wasm = wasm_repo))
}


msg <- function(..., startup = FALSE) {
  if (!isTRUE(getOption("animovement.quiet"))) {
    if (startup) packageStartupMessage(...) else message(...)
  }
}

project_packages <- function() {
  fileConn <- file(".animovement")
  pkg <- readLines(fileConn, warn = FALSE, skipNul = TRUE)
  close(fileConn)
  pkg <- trimws(pkg[nzchar(pkg)])
  pkg <- pkg[!startsWith(pkg, "_")]
  pkg <- trimws(unlist(strsplit(pkg, ", | ,|,| "), use.names = FALSE)) # This will always work!
  pkg <- pkg[nzchar(pkg)]
  if (!length(pkg)) {
    stop(
      "Empty config file. Please write package names into your .animovement config file, separated by commas, spaces or line breaks."
    )
  }
  pkg
}

project_options <- function() {
  fileConn <- file(".animovement")
  pkg <- readLines(fileConn, warn = FALSE, skipNul = TRUE)
  close(fileConn)
  pkg <- trimws(pkg[nzchar(pkg)])
  optl <- startsWith(pkg, "_")
  if (!any(optl)) {
    return(list(before = NULL, after = NULL))
  }
  if (all(optl)) {
    before <- pkg
    after <- NULL
  } else {
    ppos <- which.min(optl)
    before <- if (ppos > 1L) pkg[1:(ppos - 1L)] else NULL
    after <- if (ppos < length(pkg)) {
      pkg[ppos:length(pkg)][optl[ppos:length(pkg)]]
    } else {
      NULL
    }
  }
  lapply(list(before = before, after = after), function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    ol <- startsWith(x, "_opt_")
    x <- substr(x, 6L, 100000L)
    r <- "function() {"
    if (any(ol)) {
      r <- paste0(r, "options(", paste(x[ol], collapse = ", "), ")")
    }
    if (all(ol)) {
      r <- paste0(r, "}")
    } else {
      r <- if (any(ol)) {
        paste0(r, "; Sys.setenv(", paste(x[!ol], collapse = ", "), ")}")
      } else {
        paste0(r, "Sys.setenv(", paste(x[!ol], collapse = ", "), ")}")
      }
    }
    eval(str2lang(r), NULL, NULL)
  })
}

#' List all packages in the animovement
#'
#' Core packages are first fetched from a project-level configuration file (named \code{.animovement}, if found), otherwise the standard set of core packages is returned.
#' In addition, if \code{extensions = TRUE}, any packages used to extend the \emph{animovement} for the current
#' session are also returned.
#'
#' @param extensions logical. \code{TRUE} appends the set of core packages with all packages found in \code{options("animovement.extend")}.
#' @param include.self logical. Include the \emph{animovement} package in the list?
#'
#' @returns A character vector of package names.
#' @export
#' @seealso \code{\link{animovement_extend}}, \code{\link{animovement}}
#' @examples
#' animovement_packages()
animovement_packages <- function(extensions = TRUE, include.self = TRUE) {
  if (file.exists(".animovement")) {
    pkg <- project_packages()
  } else {
    pkg <- .core_pkg
  }
  if (extensions && length(ex <- getOption("animovement.extend"))) {
    pkg <- unique(c(pkg, ex))
  }
  if (include.self) {
    pkg <- c(pkg, "animovement")
  }
  pkg
}


package_version <- function(x) {
  paste(unclass(packageVersion(x))[[1L]], collapse = ".")
}

# Colour is only safe when whatever receives the message can render it. A file,
# a pipe, a knitr chunk or a terminal without ANSI support all get escape codes
# otherwise. cli::num_ansi_colors() settles that in one call -- it honours
# NO_COLOR, TERM, RStudio, knitr and non-interactive sessions -- while
# animovement.styling stays the manual override in both directions.
styled <- function() {
  styling <- getOption("animovement.styling")
  if (isFALSE(styling)) {
    return(FALSE)
  }
  if (isTRUE(styling)) {
    return(TRUE)
  }
  cli::num_ansi_colors() > 1
}

ansi <- function(x, open, close = "39") {
  if (!styled()) {
    return(x)
  }
  paste0("\033[", open, "m", x, "\033[", close, "m")
}

green <- function(x) ansi(x, "32")
blue <- function(x) ansi(x, "34")
magenta2 <- function(x) ansi(x, "38;5;198")
gold <- function(x) ansi(x, "38;5;214")
kingsblue <- function(x) ansi(x, "38;5;33")
grey70 <- function(x) ansi(x, "0;38;5;249")
red <- function(x) ansi(x, "31")
bold <- function(x) ansi(x, "1", "22")
# Crayons white is more gray-isch
# white <- function(x) if(isFALSE(getOption("animovement.styling"))) x else paste0("\033[37m", x, "\033[39m")
# Using bright white: https://i.stack.imgur.com/9UVnC.png
# white <- function(x) if(isFALSE(getOption("animovement.styling"))) x else paste0("\033[97m", x, "\033[39m")
# Problem: If console is white: cannot read..

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!identical(.Platform$GUI, "RStudio")) {
    return(x)
  }
  grey70(x)
}

rule <- function(
  left,
  right = NULL,
  style.left = identity,
  style.right = identity,
  style.rule = FALSE
) {
  n <- getOption("width")
  left <- as.character(left)
  if (length(right)) {
    right <- as.character(right)
    width <- n - nchar(left) - nchar(right) - 8L
    if (!is.finite(width) || width <= 2L) {
      width <- 2L
    }
    if (style.rule) {
      res <- paste(
        c(
          text_col("-- "),
          style.left(left),
          " ",
          text_col(paste(rep("-", width), collapse = "")),
          " ",
          style.right(right),
          text_col(" --")
        ),
        collapse = ""
      )
    } else {
      res <- paste(
        c(
          "-- ",
          style.left(left),
          " ",
          rep("-", width),
          " ",
          style.right(right),
          " --"
        ),
        collapse = ""
      )
    }
  } else {
    width <- n - nchar(left) - 4L
    if (!is.finite(width) || width <= 2L) {
      width <- 2L
    }
    if (style.rule) {
      res <- paste(
        c(
          text_col("-- "),
          style.left(left),
          " ",
          text_col(paste(rep("-", width), collapse = ""))
        ),
        collapse = ""
      )
    } else {
      res <- paste(
        c("-- ", style.left(left), " ", rep("-", width)),
        collapse = ""
      )
    }
  }
  class(res) <- "animovement_rule"
  res
}

# Not needed, but better than not..
#' @export
print.animovement_rule <- function(x, ..., sep = "\n") {
  cat(x, ..., sep = sep)
  invisible(x)
}
