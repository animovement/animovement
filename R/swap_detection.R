#' Detect and Correct Keypoint Swaps in Motion Tracking Data
#'
#' @param data A tibble containing columns: individual, time, keypoint, x, y
#' @param detection_threshold Numeric threshold for detecting swaps (default: 0.4)
#' @return A tibble with corrected keypoint positions
#' @details
#' This function detects and corrects swapped keypoints in motion tracking data by:
#' 1. Analyzing movement patterns for each individual
#' 2. Detecting potential swaps using high-pass filtered position differences
#' 3. Applying corrections where two keypoints appear to have swapped positions
#'
detect_keypoint_swaps <- function(data, detection_threshold = 0.4) {
  # Pre-compute unique values
  keypoints <- unique(data$keypoint)
  individuals <- unique(data$individual)
  t <- unique(data$time)

  # Initialize results tibble
  data_filtered <- tibble(.rows = length(data))

  # Process each individual
  for (i in seq_along(individuals)) {
    # Extract data for current individual
    data_ind <- data |>
      filter(individual == individuals[[i]]) |>
      select(-individual)

    # Calculate position differences and apply high-pass filter
    df_ind <- data_ind |>
      pivot_wider(
        id_cols = c(time),
        values_from = c(x, y),
        names_from = c(keypoint)
      ) |>
      mutate(
        across(!time, ~ .x - lag(.x)),
        across(!time, ~ filter_highpass(.x, cutoff_freq = 0.5, sampling_rate = 30))
      )

    # Reshape data and calculate normalized movement energy
    d2 <- df_ind |>
      pivot_longer(
        cols = -time,
        values_to = "value",
        names_to = c("coord", "keypoint"),
        names_sep = "_"
      ) |>
      pivot_wider(
        id_cols = c(time, keypoint),
        values_from = "value",
        names_from = coord
      ) |>
      group_by(keypoint) |>
      mutate(
        x1 = abs(x),
        y1 = abs(y)
      ) |>
      ungroup() |>
      mutate(
        x1 = x1 / max(x1),
        y1 = y1 / max(y1),
        e = (abs(x1) + abs(y1)) / 2,
        peaks = e > detection_threshold
      )

    # Identify swap intervals
    d3 <- filter(d2, peaks == TRUE) |>
      group_by(time) |>
      filter(n() == 2) |>
      ungroup() |>
      group_by(keypoint) |>
      mutate(
        row = row_number(),
        onset = if_else(row %% 2 == 0, FALSE, TRUE),
        offset = if_else(row %% 2 == 0, TRUE, FALSE)
      ) |>
      select(c(time, keypoint, onset, offset)) |>
      mutate(
        onset = if_else(onset == TRUE, time, NA),
        offset = if_else(offset == TRUE, time, NA)
      ) |>
      mutate(
        onset = if_else(is.na(onset), lag(onset), onset),
        offset = if_else(is.na(offset), lead(offset), offset)
      ) |>
      na.omit() |>
      select(-time) |>
      distinct() |>
      ungroup() |>
      mutate(onset = t[findInterval(onset, t) - 1])

    # Apply swaps and combine results
    swapped_result <- swap_keypoints(data_ind, d3) |>
      mutate(individual = individuals[i]) |>
      filter(time %in% data$time)

    data_filtered <- bind_rows(data_filtered, swapped_result)
  }

  return(data_filtered)
}

#' Swap Keypoint Positions Within Specified Time Intervals
#'
#' @param data A tibble containing columns: time, keypoint, x, y, confidence
#' @param swap_intervals A tibble containing columns: keypoint, onset, offset
#' @return A tibble with swapped keypoint positions
#' @details
#' This function applies the actual swapping of keypoint positions based on
#' the intervals identified in detect_keypoint_swaps()
#'
swap_keypoints <- function(data, swap_intervals) {
  # Validate required columns
  required_cols <- c("time", "keypoint", "x", "y", "confidence")
  if (!all(required_cols %in% names(data))) {
    cli_abort("Data must contain columns: {paste(required_cols, collapse = ', ')}")
  }

  # Get unique pairs of keypoints to swap
  swap_pairs <- swap_intervals |>
    select(keypoint, onset) |>
    group_by(onset) |>
    summarise(keypoints = list(unique(keypoint)), .groups = "drop")

  # Initialize output data
  swapped_data <- data

  # Apply swaps for each interval
  for (i in seq_len(nrow(swap_pairs))) {
    current_pair <- unlist(swap_pairs$keypoints[i])
    current_onset <- swap_pairs$onset[i]
    current_offset <- swap_intervals |>
      filter(onset == current_onset) |>
      pull(offset) |>
      unique()

    # Validate pair of keypoints
    if (length(current_pair) != 2) {
      cli_warn("Skipping interval at {current_onset} - exactly 2 keypoints required")
      next
    }

    # Perform swap
    swapped_data <- swapped_data |>
      mutate(
        keypoint = case_when(
          time > current_onset &
            time < current_offset &
            keypoint == current_pair[1] ~ current_pair[2],
          time > current_onset &
            time < current_offset &
            keypoint == current_pair[2] ~ current_pair[1],
          TRUE ~ keypoint
        )
      )
  }

  return(swapped_data)
}
