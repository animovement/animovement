# Main function that can handle multiple unit changes
set_unit <- function(data, ...) {
  args <- list(...)

  # Validate input types
  valid_types <- c("time", "distance", "angle")
  if (length(args) == 0) {
    cli::cli_abort("No units specified. Use arguments {.arg time}, {.arg distance}, or {.arg angle}.")
  }

  invalid_args <- setdiff(names(args), valid_types)
  if (length(invalid_args) > 0) {
    cli::cli_abort("Invalid unit type{?s}: {.val {invalid_args}}. Must be one of: {.val {valid_types}}")
  }

  # Apply each unit conversion sequentially
  for (type in names(args)) {
    unit <- args[[type]]
    data <- switch(type,
                   "time" = set_unit_time(data, unit),
                   "distance" = set_unit_distance(data, unit),
                   "angle" = set_unit_angle(data, unit)
    )
  }

  data
}

# Individual unit setters
set_unit_time <- function(data, unit) {
  # Implementation from previous example
  # but specific to time units
}

set_unit_distance <- function(data, unit) {
  # Similar implementation for distance
}

set_unit_angle <- function(data, unit) {
  # Similar implementation for angles
}

set_unit_time <- function(data, unit, conversion_factor = NULL){
  unit <- switch(
    tolower(unit),
    "us" = "us",
    "microsecond" = "us",
    "microseconds" = "us",
    "ms" = "ms",
    "millisecond" = "ms",
    "milliseconds" = "ms",
    "s" = "s",
    "second" = "s",
    "seconds" = "s",
    "min" = "min",
    "minute" = "min",
    "minutes" = "min",
    "h" = "h",
    "hour" = "h",
    "hours" = "h",
    cli::cli_abort(
      "Invalid unit: {.val {unit}}. Must be one of:
      'us', 'microsecond', 'microseconds',
      'ms', 'millisecond', 'milliseconds',
      's', 'second', 'seconds',
      'min', 'minute', 'minutes',
      'h', hour', 'hours")
  )

  # Does it have metadata already?

  # What is the current unit?


}
