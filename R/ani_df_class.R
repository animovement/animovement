# Constructor and main creation functions for ani_df class

#' Create a new ani_df object (internal constructor)
#'
#' @param x A data frame to convert to ani_df
#' @param metadata Optional metadata list
#' @return An ani_df object
#' @keywords internal
new_ani_df <- function(x, metadata = list()) {
  class(x) <- c("ani_df", class(x))
  x <- init_metadata(x, metadata)
  x
}

#' Create an ani_df data frame
#'
#' Creates a specialized data frame for animal tracking data with required
#' columns for positional data (x/y/z) and time, plus optional columns for
#' individual, keypoint, trial, and session identifiers.
#'
#' @param ... Name-value pairs to create columns in the data frame
#' @param metadata Optional list of metadata
#' @param .rows Number of rows (passed to tibble)
#' @param .name_repair How to repair column names (passed to tibble)
#'
#' @return An ani_df object (tibble with ani_df class)
#' @export
#'
#' @examples
#' ani_df(
#'   individual = rep(1:2, each = 25),
#'   time = rep(1:10, 5),
#'   x = rnorm(50),
#'   y = rnorm(50),
#'   trial = 1
#' )
ani_df <- function(
    ...,
    metadata = list(),
    .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal")
) {

  # Create tibble
  df <- dplyr::tibble(..., .rows = .rows, .name_repair = .name_repair)

  # Validate required columns
  has_position <- any(c("x", "y", "z") %in% names(df))
  if (!has_position) {
    cli::cli_abort("Missing positional data. Make sure to include x/y/z at least.")
  }

  if (!"time" %in% names(df)) {
    cli::cli_abort("Missing a time column.")
  }

  # Handle keypoint column
  if (!"keypoint" %in% names(df)) {
    cli::cli_alert_info(
      "No 'keypoint' column was found. The column was created and filled with 'centroid'."
    )
    df <- df |>
      dplyr::mutate(keypoint = factor("centroid"))
  } else {
    df <- df |>
      dplyr::mutate(keypoint = factor(.data$keypoint))
  }

  # Handle individual column
  if (!"individual" %in% names(df)) {
    cli::cli_alert_info(
      "No 'individual' column was found. The column was created and filled with 'NA'."
    )
    df <- df |>
      dplyr::mutate(individual = factor(NA))
  } else {
    df <- df |>
      dplyr::mutate(individual = factor(.data$individual))
  }

  # Convert trial to integer if present
  if ("trial" %in% names(df) && is.numeric(df$trial)) {
    df <- df |>
      dplyr::mutate(trial = as.integer(.data$trial))
  } else if ("trial" %in% names(df) && is.character(df$trial)){
    df <- df |>
      dplyr::mutate(trial = factor(.data$trial))
  }

  # Convert session to integer if present
  if ("session" %in% names(df) && is.numeric(df$session)) {
    df <- df |>
      dplyr::mutate(session = as.integer(.data$session))
  } else if ("session" %in% names(df) && is.character(df$session)){
    df <- df |>
      dplyr::mutate(session = factor(.data$session))
  }

  # Relocate columns to standard order
  df <- df |>
    dplyr::relocate(dplyr::any_of(
      c("session", "trial", "individual", "keypoint", "time", "x", "y", "z", "confidence")
    ))

  # Group by relevant columns
  potential_groups <- c("session", "trial", "individual", "keypoint")
  groupings <- potential_groups[potential_groups %in% names(df)]
  group_syms <- rlang::syms(groupings)

  df <- df |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::arrange(.by_group = TRUE)

  # Convert to ani_df
  new_ani_df(df, metadata)
}

#' Convert a data frame to ani_df
#'
#' @param data A data frame with appropriate columns
#' @return An ani_df object
#' @export
as_ani_df <- function(data) {
  ani_df(data)
}

#' Check if object is an ani_df
#'
#' @param x An object to test
#' @return Logical: TRUE if x inherits from ani_df
#' @export
is_ani_df <- function(x) {
  inherits(x, "ani_df")
}

#' Create example ani_df data
#'
#' @param class Unused parameter (for future extension)
#' @return An example ani_df object
#' @export
#'
#' @examples
#' example_tbl()
example_tbl <- function(class = NULL) {
  ani_df(
    individual = rep(1:2, each = 25),
    time = rep(1:10, 5),
    x = stats::rnorm(50),
    y = stats::rnorm(50),
    trial = 1
  )
}

#' Custom tibble summary for ani_df
#'
#' @param x An ani_df object
#' @param ... Additional arguments (unused)
#' @return Named character vector with summary information
#' @export
tbl_sum.ani_df <- function(x, ...) {
  default_header <- NextMethod()

  # Initialize new header
  new_header <- c(
    "Individuals" = paste(unique(x$individual), collapse = ", "),
    "Keypoints" = paste(unique(x$keypoint), collapse = ", ")
  )

  # Add sessions if column exists
  if ("session" %in% names(x)) {
    n_sessions <- length(unique(x$session))
    new_header <- c(new_header, "Sessions" = as.character(n_sessions))
  }

  # Add trials if column exists
  if ("trial" %in% names(x)) {
    new_header <- c(new_header, "Trials" = paste(unique(x$trial), collapse = ", "))
  }

  # Add sampling rate if available in metadata
  sampling_rate <- get_metadata(x)$sampling_rate
  if (!is.null(sampling_rate) && !is.na(sampling_rate)) {
    new_header <- c(new_header, "Sampling rate" = paste(sampling_rate, "Hz"))
  }

  new_header
}
