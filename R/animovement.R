#' The animovement
#'
#' @description
#' The \emph{animovement} is an extensible suite of R packages. It is a descendant of the \href{https://fastverse.github.io/fastverse/}{fastverse}.
#'
#' @details NULL
#' @section Functions in the \emph{animovement} Package:
#'
#' Functions to extend or reduce the number of packages in the \emph{animovement}
#'
#' \code{\link[=animovement_extend]{animovement_extend()}}\cr
#' \code{\link[=animovement_detach]{animovement_detach()}}
#'
#' Function to display conflicts for \emph{animovement} packages (or any other attached packages)
#'
#' \code{\link[=animovement_conflicts]{animovement_conflicts()}}
#'
#' Function to update \emph{animovement} packages (and dependencies) and install (missing) packages
#'
#' \code{\link[=animovement_update]{animovement_update()}}\cr
#' \code{\link[=animovement_install]{animovement_install()}}
#'
#' Utilities to retrieve the names of \emph{animovement} packages (and dependencies), their update status and produce a situation report.
#'
#' \code{\link[=animovement_packages]{animovement_packages()}}\cr
#' \code{\link[=animovement_deps]{animovement_deps()}}\cr
#' \code{\link[=animovement_sitrep]{animovement_sitrep()}}
#'
#' @section \emph{animovement} Options:
#' \itemize{
#' \item \code{options(animovement.quiet = TRUE)} will disable all automatic messages (including conflict reporting) when calling \code{library(animovement)}, \code{\link{animovement_extend}}, \code{\link[=animovement_update]{animovement_update(install = TRUE)}} and \code{\link{animovement_install}}.
#' \item \code{options(animovement.styling = FALSE)} will disable all styling applied to text printed to the console.
#' \item \code{options(animovement.extend = c(...))} can be set before calling \code{library(animovement)} to extend the animovement with some packages for the session. The same can be done with the
#' \code{\link{animovement_extend}} function after \code{library(animovement)}, which will also populate \code{options("animovement.extend")}.
#' \item \code{options(animovement.install = TRUE)} can be set before \code{library(animovement)} to install any missing packages beforehand. See also \code{\link{animovement_install}}.
#' }
#'
#' @name animovement
#'
#' @importFrom utils stack packageVersion install.packages available.packages
#' @importFrom tools package_dependencies
#' @import aniframe
#' @import aniread
#' @import aniprocess
#' @import anicheck
#' @import animetric
#' @import anivis
#'
"_PACKAGE"
