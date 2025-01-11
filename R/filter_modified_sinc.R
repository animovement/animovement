# R implementation of smoothing by a modified sinc kernel (MS)
# Ported from the Matlab/Octave implementation by Michael Schmid
# Original paper: M. Schmid, D. Rath and U. Diebold,
# 'Why and how Savitzky-Golay filters should be replaced',
# ACS Measurement Science Au, 2022

#' Modified Sinc Smoother
#'
#' @param data Numeric vector to smooth
#' @param deg Degree, determines cutoff sharpness (2,4,6,8,10)
#' @param m Half width of kernel
#' @return Smoothed numeric vector
smoothMS <- function(data, deg, m) {
  if(length(data) < 2) stop("Less than two data points")

  kernel <- kernelMS(deg, m)
  fit_weights <- edgeWeights(deg, m)
  ext_data <- extendData(data, m, fit_weights)
  smoothed_ext_data <- stats::filter(ext_data, kernel, sides = 2)
  smoothed_data <- smoothed_ext_data[(m+1):(length(smoothed_ext_data)-m)]

  return(smoothed_data)
}

#' Modified Sinc Smoother (shorter kernel)
#'
#' @param data Numeric vector to smooth
#' @param deg Degree, determines cutoff sharpness (2,4,6,8,10)
#' @param m Half width of kernel
#' @return Smoothed numeric vector
smoothMS1 <- function(data, deg, m) {
  if(length(data) < 2) stop("Less than two data points")

  kernel <- kernelMS1(deg, m)
  fit_weights <- edgeWeights1(deg, m)
  ext_data <- extendData(data, m, fit_weights)
  smoothed_ext_data <- stats::filter(ext_data, kernel, sides = 2)
  smoothed_data <- smoothed_ext_data[(m+1):(length(smoothed_ext_data)-m)]

  return(smoothed_data)
}

#' Get correction coefficients for MS kernel
#'
#' @param deg Degree of filter (2,4,6,8,10)
#' @return Matrix of coefficients
corrCoeffsMS <- function(deg) {
  coeffs <- switch(as.character(deg),
                   "2" = NULL,
                   "4" = NULL,
                   "6" = matrix(c(0.001717576, 0.02437382, 1.64375), nrow=1),
                   "8" = matrix(c(0.0043993373, 0.088211164, 2.359375,
                                  0.006146815, 0.024715371, 3.6359375),
                                nrow=2, byrow=TRUE),
                   "10" = matrix(c(0.0011840032, 0.04219344, 2.746875,
                                   0.0036718843, 0.12780383, 2.7703125),
                                 nrow=2, byrow=TRUE),
                   stop("Invalid degree")
  )
  return(coeffs)
}

#' Get correction coefficients for MS1 kernel
#'
#' @param deg Degree of filter (2,4,6,8,10)
#' @return Matrix of coefficients
corrCoeffsMS1 <- function(deg) {
  coeffs <- switch(as.character(deg),
                   "2" = NULL,
                   "4" = matrix(c(0.021944195, 0.050284006, 0.765625), nrow=1),
                   "6" = matrix(c(0.001897730, 0.00847681, 1.2625,
                                  0.023064667, 0.13047926, 1.2265625),
                                nrow=2, byrow=TRUE),
                   "8" = matrix(c(0.006590300, 0.05792946, 1.915625,
                                  0.002323448, 0.01029885, 2.2726562,
                                  0.021046653, 0.16646601, 1.98125),
                                nrow=3, byrow=TRUE),
                   "10" = matrix(c(9.749618E-4, 0.00207429, 3.74375,
                                   0.008975366, 0.09902466, 2.707812,
                                   0.002419541, 0.01006486, 3.296875,
                                   0.019185117, 0.18953617, 2.784961),
                                 nrow=4, byrow=TRUE),
                   stop("Invalid degree")
  )
  return(coeffs)
}

#' Calculate MS convolution kernel
#'
#' @param deg Degree of filter
#' @param m Half width of kernel
#' @return Kernel coefficients
kernelMS <- function(deg, m) {
  coeffs <- corrCoeffsMS(deg)
  kappa <- numeric()

  if(!is.null(coeffs)) {
    kappa <- apply(coeffs, 1, function(abcd) {
      abcd[1] + abcd[2]/(abcd[3] - m)^3
    })
  }

  nu_minus2 <- if(deg %% 4 == 2) -1 else 0  # degree 6, 10

  kernel <- numeric(2*m + 1)
  kernel[m+1] <- windowMS(0, 4)  # center element

  for(i in 1:m) {
    x <- i/(m+1)
    w <- windowMS(x, 4)
    a <- sin((0.5*deg+2)*pi*x)/((0.5*deg+2)*pi*x)

    if(length(kappa) > 0) {
      for(j in seq_along(kappa)) {
        a <- a + kappa[j]*x*sin((2*j+nu_minus2)*pi*x)
      }
    }

    a <- a*w
    kernel[m+1-i] <- a
    kernel[m+1+i] <- a
  }

  norm <- sum(kernel)
  kernel <- kernel/norm

  return(kernel)
}

#' Calculate MS1 convolution kernel
#'
#' @param deg Degree of filter
#' @param m Half width of kernel
#' @return Kernel coefficients
kernelMS1 <- function(deg, m) {
  coeffs <- corrCoeffsMS1(deg)
  kappa <- numeric()

  if(!is.null(coeffs)) {
    kappa <- apply(coeffs, 1, function(abcd) {
      abcd[1] + abcd[2]/(abcd[3] - m)^3
    })
  }

  kernel <- numeric(2*m + 1)
  kernel[m+1] <- windowMS(0, 2)  # center element

  for(i in 1:m) {
    x <- i/(m+1)
    w <- windowMS(x, 2)
    a <- sin((0.5*deg+1)*pi*x)/((0.5*deg+1)*pi*x)

    if(length(kappa) > 0) {
      for(j in seq_along(kappa)) {
        a <- a + kappa[j]*x*sin(j*pi*x)
      }
    }

    a <- a*w
    kernel[m+1-i] <- a
    kernel[m+1+i] <- a
  }

  norm <- sum(kernel)
  kernel <- kernel/norm

  return(kernel)
}

#' Gaussian-like window function for MS and MS1 kernels
#'
#' @param x Input value(s)
#' @param alpha Window parameter
#' @return Window function values
windowMS <- function(x, alpha) {
  w <- exp(-alpha*x^2) +
    exp(-alpha*(x+2)^2) +
    exp(-alpha*(x-2)^2) -
    (2*exp(-alpha) + exp(-9*alpha))
  return(w)
}

#' Edge weights for MS smoothing
#'
#' @param deg Degree of filter
#' @param m Half width of kernel
#' @return Weight vector
edgeWeights <- function(deg, m) {
  beta <- 0.70 + 0.14*exp(-0.6*(deg-4))
  fit_length_d <- ((m+1)*beta)/(1.5+0.5*deg)
  fit_length <- floor(fit_length_d)

  i <- 0:fit_length
  cosine <- cos(pi/2*i/fit_length_d)
  w <- cosine^2

  return(w)
}

#' Edge weights for MS1 smoothing
#'
#' @param deg Degree of filter
#' @param m Half width of kernel
#' @return Weight vector
edgeWeights1 <- function(deg, m) {
  beta <- 0.65 + 0.35*exp(-0.55*(deg-4))
  fit_length_d <- ((m+1)*beta)/(1+0.5*deg)
  fit_length <- floor(fit_length_d)

  i <- 0:fit_length
  cosine <- cos(pi/2*i/fit_length_d)
  w <- cosine^2

  return(w)
}

#' Extend data by weighted linear extrapolation
#'
#' @param data Input data vector
#' @param m Half width of kernel
#' @param fit_weights Weights for linear fit
#' @return Extended data vector
extendData <- function(data, m, fit_weights) {
  dat_length <- length(data)
  fit_length <- length(fit_weights)
  fit_x <- 1:fit_length

  # Left boundary
  fit_y <- data[1:fit_length]
  # Handle case where all points are NA
  if(all(is.na(fit_y))) {
    left_ext <- rep(data[min(which(!is.na(data)))], m)
  } else {
    fit_result <- fitWeighted(fit_x[!is.na(fit_y)],
                              fit_y[!is.na(fit_y)],
                              fit_weights[!is.na(fit_y)])
    offset <- fit_result[1]
    slope <- fit_result[2]
    left_ext <- offset + seq(-m+1, 0) * slope
  }

  # Right boundary
  fit_y <- rev(data[(dat_length-fit_length+1):dat_length])
  # Handle case where all points are NA
  if(all(is.na(fit_y))) {
    right_ext <- rep(data[max(which(!is.na(data)))], m)
  } else {
    fit_result <- fitWeighted(fit_x[!is.na(fit_y)],
                              fit_y[!is.na(fit_y)],
                              fit_weights[!is.na(fit_y)])
    offset <- fit_result[1]
    slope <- fit_result[2]
    right_ext <- offset + seq(0, -m+1, by=-1) * slope
  }

  ext_data <- c(left_ext, data, right_ext)
  return(ext_data)
}

#' Weighted linear fit
#'
#' @param x_data X values
#' @param y_data Y values
#' @param weights Weights for fit
#' @return List with offset and slope
fitWeighted <- function(x_data, y_data, weights) {
  sum_weights <- sum(weights)
  sum_x <- sum(x_data * weights)
  sum_y <- sum(y_data * weights)
  sum_x2 <- sum(x_data^2 * weights)
  sum_xy <- sum(x_data * y_data * weights)

  var_x2 <- sum_x2*sum_weights - sum_x^2

  if(var_x2 == 0) {
    slope <- 0
  } else {
    slope <- (sum_xy*sum_weights - sum_x*sum_y)/var_x2
  }

  offset <- (sum_y - slope*sum_x)/sum_weights

  return(c(offset, slope))
}

# Test code similar to original
test_ms_filter <- function() {
  deg <- 6  # degree
  m <- 7    # kernel halfwidth
  data <- c(0, 1, -2, 3, -4, 5, -6, 7, -8, 9, 10, 6, 3, 1, 0)

  cat("smoothMS of test data:\n")
  out <- smoothMS(data, deg, m)
  cat(sprintf("%.8f ", out), "\n")
}
