packageVersion2 <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) packageVersion(pkg) else 0
}

#' List all animovement dependencies
#' 
#' Lists all \emph{animovement} dependencies and the local and CRAN versions of packages and dependencies.
#'
#' @param pkg character vector of packages to check dependencies and versions of. The default is all \emph{animovement} packages. 
#' @param recursive logical. \code{TRUE} recursively determines all packages required to operate these packages.
#' \code{FALSE} will only list the packages and their direct dependencies. 
#' @param repos the repositories to use to check for updates. Defaults to \code{getOptions("repos")}.
#' @param include.self logical. \code{TRUE} also includes the \emph{animovement} package and checks against the CRAN version.  
#' @param check.deps logical. \code{FALSE} will not determine dependencies but only display the update status of packages in \code{pkg}. 
#' 
#' @returns A data frame giving the package names, the CRAN and local version, and a logical variable stating whether the local version is behind the CRAN version. 
#' @seealso \code{\link{animovement_sitrep}}, \code{\link{animovement}}
#' @export
animovement_deps <- function(pkg = animovement_packages(), recursive = FALSE, 
                           repos = getOption("repos"), include.self = FALSE, check.deps = TRUE) {  
  
  pkgs <- available.packages(repos = repos)
  if(!length(pkgs)) stop("Please connect to the internet to execute this function")
  fv <- "animovement"
  pkg <- pkg[pkg != fv] # Code should work regardless of whether pkg includes "animovement" or not
  if(check.deps) {
    if(!include.self) fv <- NULL
    deps <- package_dependencies(pkg, pkgs, recursive = recursive)
    pkg_deps <- unique(c(pkg, fv, sort(unlist(deps, use.names = FALSE)))) 
    base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
      "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk", "utils")
    pkg_deps <- setdiff(pkg_deps, base_pkgs)
  } else {
    pkg_deps <- if(include.self) c(pkg, fv) else pkg
  }
  
  if(!all(pnmiss <- pkg_deps %in% rownames(pkgs))) {
    warning(paste("Ignoring package(s)",  paste(pkg_deps[!pnmiss], collapse = ", "), "not available on CRAN"))
    pkg_deps <- pkg_deps[pnmiss]
    if(!length(pkg_deps)) return()
  }
  
  cran_version <- lapply(pkgs[pkg_deps, "Version"], base::package_version)
  local_version <- lapply(pkg_deps, packageVersion2)
  
  behind <- mapply(`>`, cran_version, local_version)
  
  data.frame(
    package = pkg_deps,
    cran = sapply(cran_version, as.character),
    local = sapply(local_version, as.character),
    behind = behind,
    row.names = seq_along(pkg_deps)
  )
}


#' Update animovement packages
#'
#' This will check all \emph{animovement} packages (and their
#' dependencies) for updates and (optionally) install those updates. 
#'
#' @param \dots arguments passed to \code{\link{animovement_deps}}.
#' @param install logical. \code{TRUE} will proceed to install outdated packages, whereas \code{FALSE} (recommended) will print the installation command asking you to run it in a clean R session.
#' 
#' @returns \code{animovement_update} returns \code{NULL} invisibly. 
#' @seealso \code{\link{animovement_deps}}, \code{\link{animovement}}
#' @export
animovement_update <- function(..., install = FALSE) {
  
  deps <- animovement_deps(...) 
  behind <- subset(deps, behind)
  
  if (nrow(behind) == 0L) {
    if(!isTRUE(getOption("animovement.quiet"))) cat("All animovement packages up-to-date\n")
    return(invisible())
  }
  
  if(!isTRUE(getOption("animovement.quiet"))) {
    cat("The following packages are out of date:\n")
    cat("\n", paste0("* ", gold(format(behind$package)), " (", behind$local, " -> ", behind$cran, ")\n"))
  }
  
  if(install) {
    install.packages(behind$package)
  } else {
    cat("\nStart a clean R session then run:\n")
    pkg_str <- paste0(deparse(behind$package), collapse = "\n")
    cat("install.packages(", pkg_str, ")\n", sep = "")
  }
  
  invisible()
}

#' Install (missing) animovement packages
#'
#' This function (by default) checks if any \emph{animovement} package is missing and installs the missing package(s).
#'
#' @param \dots comma-separated package names, quoted or unquoted, or vectors of package names. If left empty, all packages returned by \code{\link{animovement_packages}} are checked. 
#' @param only.missing logical. \code{TRUE} only installs packages that are unavailable. \code{FALSE} installs all packages, even if they are available. 
#' @param install logical. \code{TRUE} will proceed to install packages, whereas \code{FALSE} (recommended) will print the installation command asking you to run it in a clean R session.
#' 
#' @note 
#' There is also the possibility to set \code{options(animovement.install = TRUE)} before \code{library(animovement)}, which will call \code{animovement_install()} before loading any packages to make sure all packages are available.
#' If you are using a \code{.animovement} configuration file inside a project (see vignette), you can also place \code{_opt_animovement.install = TRUE} before the list of packages in that file.
#' 
#' 
#' @returns \code{animovement_install} returns \code{NULL} invisibly. 
#' @seealso \code{\link{animovement_update}}, \code{\link{animovement}}
#' @export
animovement_install <- function(..., only.missing = TRUE, install = TRUE) {
  
  if(missing(...)) {
    pkg <- animovement_packages(include.self = FALSE)
  } else {
    pkg <- tryCatch(c(...), error = function(e) .c(...))
    if(!is.character(pkg) || length(pkg) > 200L) pkg <- .c(...)
  }
  
  needed <- if(only.missing) pkg[!is_installed(pkg)] else pkg
  
  if(length(needed)) {
    if(install) {
      install.packages(needed)
    } else {
      cat("\nStart a clean R session then run:\n")
      pkg_str <- paste0(deparse(needed), collapse = "\n")
      cat("install.packages(", pkg_str, ")\n", sep = "")
    }
  } else if(!isTRUE(getOption("animovement.quiet"))) {
    cat("All animovement packages installed\n")
  }
  
  return(invisible())
}


#' Get a situation report on the animovement
#'
#' This function gives a quick overview of the version of R and all 
#' \emph{animovement} packages (including availability updates for packages) and indicates 
#' whether any project-level configuration files are used.
#' 
#' @param \dots arguments other than \code{pkg} passed to \code{\link{animovement_deps}}.
#' 
#' @returns \code{animovement_sitrep} returns \code{NULL} invisibly. 
#' @seealso \code{\link{animovement_deps}}, \code{\link{animovement}}
#' @export
animovement_sitrep <- function(...) {

  cat(rule(paste0("animovement ", package_version("animovement"), ": Situation Report"), 
       paste("R", getRversion()), 
       style.left = function(x) sub("Situation Report", bold("Situation Report"), 
                                    sub("animovement", kingsblue("animovement"), x, fixed = TRUE), fixed = TRUE)))

  pkg <- animovement_packages(include.self = FALSE) 
  deps <- animovement_deps(pkg, ...)  

  package_pad <- format(deps$package)
  packages <- ifelse(
    deps$behind,
    paste0("* ", gold(package_pad), " (", deps$local, " < ", deps$cran, ")\n"), 
    paste0("* ", magenta2(package_pad), " (", deps$cran, ")\n")
  )
  
  deps <- deps$package
  
  ex <- getOption("animovement.extend")
  if(length(ex)) pkg <- setdiff(pkg, ex)
  if(any(deps == "animovement")) pkg <- c(pkg, "animovement") # include.self = FALSE above makes sure we don't add it twice here
  
  pcol <- file.exists(".animovement")
  cat("\n", paste0("* Project config file: ", pcol, "\n"))
  cat(rule("Core packages"), "\n", packages[deps %in% pkg])
  if(length(ex)) {
    cat(rule("Extension packages"), "\n", packages[deps %in% ex])
    pkg <- c(pkg, ex)
  }
  if(missing(...) || !any(cdl <- ...names() == "check.deps") || ...elt(which(cdl))) 
    cat(rule("Dependencies"), "\n", packages[!deps %in% pkg])
  
  invisible()
}


