#' Improved adaptive Modified Sinc Filter for tracking data
#' @param x Numeric vector to filter
#' @param auto_params Whether to automatically select parameters
#' @param deg Optional degree override
#' @param m Optional half-width override
#' @param return_params Logical, whether to return parameters instead of filtered data
#' @return Numeric vector of filtered values (or list of parameters if return_params=TRUE)
filter_ms <- function(x, auto_params = TRUE, deg = NULL, m = NULL,
                      return_params = FALSE) {
  # Early return for all NA input
  if(all(is.na(x))) {
    return(x)
  }

  # Store original length and NA positions
  n <- length(x)
  na_positions <- is.na(x)

  # Get clean data for parameter estimation
  x_clean <- x[!na_positions]

  # Handle case where there's insufficient clean data
  if(length(x_clean) < 4) {
    return(x)  # Return original if not enough data to filter
  }

  if(auto_params) {
    params <- select_ms_parameters(x_clean)
    deg <- params$degree
    m <- params$half_width
  } else {
    if(is.null(deg) || is.null(m)) {
      cli::cli_abort("Must provide both deg and m if auto_params = FALSE")
    }
    params <- list(degree = deg, half_width = m)
  }

  # If user just wants parameters, return them here
  if(return_params) {
    return(params)
  }

  # Find contiguous blocks of non-NA values
  rle_result <- rle(!na_positions)
  cumsum_lengths <- cumsum(rle_result$lengths)
  block_starts <- c(1, cumsum_lengths[-length(cumsum_lengths)] + 1)

  # Create output vector
  x_filtered <- x

  # Process each non-NA block separately
  for(i in seq_along(rle_result$lengths)) {
    if(rle_result$values[i]) {  # If this is a non-NA block
      start_idx <- block_starts[i]
      end_idx <- cumsum_lengths[i]
      block_data <- x[start_idx:end_idx]

      if(length(block_data) >= 4) {  # Only filter if enough points
        filtered_block <- safe_smoothMS(block_data, deg = params$degree, m = params$half_width)
        x_filtered[start_idx:end_idx] <- filtered_block
      }
    }
  }

  return(x_filtered)
}

#' Improved parameter selection for tracking data
#' @param x Numeric vector of signal
#' @return List of optimal parameters
select_ms_parameters <- function(x) {
  # Remove NAs for estimation
  x_clean <- x[!is.na(x)]

  if(length(x_clean) < 4) {
    cli::cli_abort("Insufficient non-NA data points")
  }

  # Compute movement statistics
  diffs <- diff(x_clean)
  second_diffs <- diff(diffs)

  # Estimate noise using high-frequency components
  noise_est <- mad(second_diffs, na.rm = TRUE) / sqrt(2)

  # Estimate typical movement scale
  movement_scale <- mad(diffs, na.rm = TRUE)

  # Calculate effective SNR using movement vs noise
  effective_snr <- 20 * log10(movement_scale / noise_est)

  # Estimate characteristic timescale using autocorrelation
  acf_result <- acf(x_clean, plot = FALSE, na.action = na.pass)
  first_min <- which(diff(acf_result$acf) >= 0)[1]
  if(is.na(first_min)) first_min <- length(acf_result$acf)/4

  # Select degree based on SNR and movement characteristics
  deg <- if(effective_snr > 30) {
    8  # Very clean signal
  } else if(effective_snr > 20) {
    6  # Clean signal
  } else if(effective_snr > 10) {
    4  # Moderate noise
  } else {
    2  # Very noisy
  }

  # Select kernel width based on characteristic timescale and noise
  base_width <- ceiling(first_min/2)
  m <- if(effective_snr > 30) {
    max(base_width, deg * 2)  # Clean signal - preserve details
  } else if(effective_snr > 20) {
    max(base_width * 1.5, deg * 3)  # Moderate noise
  } else if(effective_snr > 10) {
    max(base_width * 2, deg * 4)  # Significant noise
  } else {
    max(base_width * 3, deg * 5)  # Very noisy
  }

  # Ensure minimum width requirements
  m <- max(m, deg + 2)

  # For tracking data, we want to ensure enough smoothing
  m <- max(m, 15)  # Minimum kernel width for tracking data

  return(list(
    degree = deg,
    half_width = m,
    estimated_snr = effective_snr,
    noise_sd = noise_est,
    movement_scale = movement_scale,
    characteristic_width = first_min
  ))
}

#' Safe version of smoothMS that handles edge cases
#' @param x Numeric vector to filter
#' @param deg Degree of filter
#' @param m Half width of kernel
#' @return Filtered numeric vector
safe_smoothMS <- function(x, deg, m) {
  if(length(x) < 2*m + 1) {
    return(x)  # Return original if too short
  }

  # Create kernel
  kernel <- kernelMS(deg, m)

  # Extend data safely
  n <- length(x)
  left_ext <- rep(x[1], m)
  right_ext <- rep(x[n], m)
  x_ext <- c(left_ext, x, right_ext)

  # Apply filter
  smoothed <- stats::filter(x_ext, kernel, sides = 2)

  # Remove NA values that filter may have introduced
  smoothed[is.na(smoothed)] <- x_ext[is.na(smoothed)]

  # Return central portion
  return(smoothed[(m+1):(length(smoothed)-m)])
}


# Helper function to get parameters for debugging
get_ms_params <- function(x) {
  filter_ms(x, return_params = TRUE)
}
