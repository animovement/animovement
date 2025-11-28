ls_env <- function(env) ls(pos = env)

invert_simplify <- function(x) {
  if (length(x) == 0L) {
    return()
  }
  stacked <- unclass(stack(x))
  dup <- stacked$values[duplicated(stacked$values)]
  if (!length(dup)) {
    return()
  }
  dup <- which(stacked$values %in% dup)
  stacked <- lapply(stacked, `[`, dup)
  split(as.character(stacked$ind), stacked$values)
}

confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- lapply(packages, get, x = name)
  objs <- objs[vapply(objs, is.function, TRUE)]
  if (length(objs) <= 1L) {
    return()
  }
  # Remove identical functions
  if (sum(!duplicated(objs)) == 1L) {
    return()
  }
  packages[!duplicated(packages)]
}


#' Conflicts between the animovement and other packages
#'
#' This function lists all the conflicts among \emph{animovement} packages and between \emph{animovement} packages and other attached packages.
#' It can also be used to check conflicts for any other attached packages.
#'
#' @param pkg character. A vector of packages to check conflicts for. The default is all \emph{animovement} packages.
#'
#' @returns An object of class 'animovement_conflicts': A named list of character vectors where the names are the conflicted objects, and the
#' content are the names of the package namespaces containing the object, in the order they appear on the \code{\link{search}} path.
#' @seealso \code{\link{animovement}}
#' @export
#' @examples
#' # Check conflicts between animovement packages and all attached packages
#' animovement_conflicts()
#'
#' # Check conflicts among all attached packages
#' animovement_conflicts(sub("package:", "", search()[-1]))
animovement_conflicts <- function(pkg = animovement_packages()) {
  envs <- grep("^package:", search(), value = TRUE)
  names(envs) <- envs
  conflicts <- invert_simplify(lapply(envs, ls_env))
  tidy_names <- paste0("package:", pkg)
  conflicts <- conflicts[vapply(
    conflicts,
    function(x) any(x %in% tidy_names),
    TRUE
  )]

  conflict_funs <- Map(confirm_conflict, conflicts, names(conflicts))
  conflict_funs <- conflict_funs[lengths(conflict_funs) > 0L]

  structure(conflict_funs, class = "animovement_conflicts")
}

animovement_conflict_message <- function(x) {
  if (length(x) == 0L) {
    return("")
  }

  header <- text_col(rule(
    left = "Conflicts",
    style.left = bold,
    right = "animovement_conflicts()"
  ))

  pkgs <- lapply(x, gsub, pattern = "^package:", replacement = "")
  others <- lapply(pkgs, `[`, -1L)
  other_calls <- mapply(
    function(x, y) {
      paste0(blue(x), text_col(paste0("::", y, "()")), collapse = ", ")
    },
    others,
    names(others)
  )

  winner <- vapply(pkgs, `[`, character(1L), 1L)
  funs <- format(paste0(
    blue(winner),
    text_col("::"),
    green(paste0(names(x), "()"))
  ))
  bullets <- paste0(
    red("x"),
    " ",
    funs,
    text_col(" masks "),
    other_calls,
    collapse = "\n"
  )

  paste0(header, "\n", bullets)
}

cat_line <- function(x) cat(x, "\n", sep = "", file = stdout(), append = TRUE)

#' @export
print.animovement_conflicts <- function(x, ..., startup = FALSE) {
  if (length(x)) cat_line(animovement_conflict_message(x))
}
