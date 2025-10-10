# Methods for ani_df class to preserve class through dplyr operations

# ---- dplyr verb methods ----

#' Ungroup an ani_df
#'
#' @param x An ani_df object
#' @param quiet If FALSE, shows a warning about ungrouping
#' @param ... Additional arguments passed to dplyr::ungroup
#' @return An ungrouped ani_df
#' @importFrom dplyr ungroup
#' @export
ungroup.ani_df <- function(x, quiet = FALSE, ...) {
  if (!quiet) {
    cli::cli_warn(
      "Ungrouping an ani_df data frame makes errors more likely. Proceed with care."
    )
  }

  class(x) <- setdiff(class(x), "ani_df")
  x <- dplyr::ungroup(x, ...)
  class(x) <- c("ani_df", class(x))
  x
}

#' Group an ani_df
#'
#' @param .data An ani_df object
#' @param ... Variables to group by
#' @param .add If TRUE, add to existing groups
#' @param .drop Drop unused factor levels
#' @return A grouped ani_df
#' @importFrom dplyr group_by
#' @export
group_by.ani_df <- function(.data, ..., .add = FALSE, .drop = TRUE) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Mutate columns in an ani_df
#'
#' @param .data An ani_df object
#' @param ... Name-value pairs of expressions
#' @return An ani_df with modified columns
#' @importFrom dplyr mutate
#' @export
mutate.ani_df <- function(.data, ...) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Select columns from an ani_df
#'
#' @param .data An ani_df object
#' @param ... Columns to select
#' @return An ani_df with selected columns
#' @importFrom dplyr select
#' @export
select.ani_df <- function(.data, ...) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Filter rows of an ani_df
#'
#' @param .data An ani_df object
#' @param ... Logical predicates
#' @param .preserve Keep group structure
#' @return A filtered ani_df
#' @importFrom dplyr filter
#' @export
filter.ani_df <- function(.data, ..., .preserve = FALSE) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Arrange rows of an ani_df
#'
#' @param .data An ani_df object
#' @param ... Variables to order by
#' @param .by_group If TRUE, arrange within groups
#' @return An arranged ani_df
#' @importFrom dplyr arrange
#' @export
arrange.ani_df <- function(.data, ..., .by_group = FALSE) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Rename columns in an ani_df
#'
#' @param .data An ani_df object
#' @param ... Name-value pairs for renaming
#' @return An ani_df with renamed columns
#' @importFrom dplyr rename
#' @export
rename.ani_df <- function(.data, ...) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Relocate columns in an ani_df
#'
#' @param .data An ani_df object
#' @param ... Columns to relocate
#' @param .before Column to place before
#' @param .after Column to place after
#' @return An ani_df with relocated columns
#' @importFrom dplyr relocate
#' @export
relocate.ani_df <- function(.data, ..., .before = NULL, .after = NULL) {
  class(.data) <- setdiff(class(.data), "ani_df")
  x <- dplyr::relocate(.data, ..., .before = {{ .before }}, .after = {{ .after }})
  class(x) <- c("ani_df", class(x))
  x
}

#' Slice rows from an ani_df
#'
#' @param .data An ani_df object
#' @param ... Integer row positions
#' @param .preserve Keep group structure
#' @return A sliced ani_df
#' @importFrom dplyr slice
#' @export
slice.ani_df <- function(.data, ..., .preserve = FALSE) {
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

# ---- Base R extraction methods ----

#' Subset ani_df with [
#'
#' @param x An ani_df object
#' @param i Row indices
#' @param j Column indices
#' @param ... Additional arguments
#' @param drop If TRUE, simplify to vector when possible
#' @return A subset ani_df
#' @export
`[.ani_df` <- function(x, i, j, ..., drop = FALSE) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Extract single column from ani_df with [[
#'
#' @param x An ani_df object
#' @param i Column index or name
#' @param ... Additional arguments
#' @return A vector or data frame
#' @export
`[[.ani_df` <- function(x, i, ...) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  if (is.data.frame(x)) {
    class(x) <- c("ani_df", class(x))
  }
  x
}

#' Extract column from ani_df with $
#'
#' @param x An ani_df object
#' @param name Column name
#' @return A vector
#' @export
`$.ani_df` <- function(x, name) {
  class(x) <- setdiff(class(x), "ani_df")
  NextMethod()
}

# ---- Assignment methods ----

#' Subset assignment for ani_df with [<-
#'
#' @param x An ani_df object
#' @param i Row indices
#' @param j Column indices
#' @param ... Additional arguments
#' @param value Replacement values
#' @return Modified ani_df
#' @export
`[<-.ani_df` <- function(x, i, j, ..., value) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Column assignment for ani_df with [[<-
#'
#' @param x An ani_df object
#' @param i Column index or name
#' @param ... Additional arguments
#' @param value Replacement value
#' @return Modified ani_df
#' @export
`[[<-.ani_df` <- function(x, i, ..., value) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Column assignment for ani_df with $<-
#'
#' @param x An ani_df object
#' @param name Column name
#' @param value Replacement value
#' @return Modified ani_df
#' @export
`$<-.ani_df` <- function(x, name, value) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

#' Rename columns with names<-
#'
#' @param x An ani_df object
#' @param value New column names
#' @return Modified ani_df
#' @export
`names<-.ani_df` <- function(x, value) {
  class(x) <- setdiff(class(x), "ani_df")
  x <- NextMethod()
  class(x) <- c("ani_df", class(x))
  x
}

# ---- Conversion methods ----

#' Convert ani_df to regular data frame
#'
#' @param x An ani_df object
#' @param ... Additional arguments
#' @return A regular data frame
#' @export
as.data.frame.ani_df <- function(x, ...) {
  class(x) <- setdiff(class(x), "ani_df")
  NextMethod()
}
