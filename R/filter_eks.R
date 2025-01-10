#' Ensemble Kalman Smoother Core Functions
#' @description Core implementation of the Ensemble Kalman Smoother for trajectory smoothing
#' @name eks

#' Compute ensemble statistics from multiple model predictions
#'
#' @param predictions List of matrices, each containing predictions from one model
#' @param mode Character string specifying the ensemble mode ("mean" or "median")
#' @return List containing ensemble means, variances and optionally likelihoods
#' @export
ensemble_stats <- function(predictions, mode = c("mean", "median")) {
  mode <- match.arg(mode)

  # Stack predictions into array
  n_models <- length(predictions)
  n_times <- nrow(predictions[[1]])
  n_points <- ncol(predictions[[1]])

  pred_array <- array(0, dim = c(n_models, n_times, n_points))
  for(i in seq_len(n_models)) {
    pred_array[i,,] <- predictions[[i]]
  }

  # Compute means/medians
  if(mode == "mean") {
    means <- apply(pred_array, c(2,3), mean, na.rm = TRUE)
  } else {
    means <- apply(pred_array, c(2,3), median, na.rm = TRUE)
  }

  # Compute variances
  vars <- apply(pred_array, c(2,3), var, na.rm = TRUE)

  # Return statistics
  list(
    means = means,
    variances = vars,
    raw_ensemble = pred_array
  )
}

#' Forward pass of Kalman filter
#'
#' @param y Matrix of observations (time x dimension)
#' @param m0 Initial state mean vector
#' @param S0 Initial state covariance matrix
#' @param A State transition matrix
#' @param Q Process noise covariance matrix
#' @param C Measurement matrix
#' @param R Measurement noise covariance matrix
#' @param ensemble_vars Time-varying observation noise variance (time x dimension)
#' @return List containing filtered means and covariances
#' @export
forward_pass <- function(y, m0, S0, A, Q, C, R, ensemble_vars) {
  T <- nrow(y)
  state_dim <- length(m0)
  obs_dim <- nrow(C)

  # Storage
  mf <- matrix(0, T, state_dim)
  Vf <- array(0, dim = c(T, state_dim, state_dim))
  S <- array(0, dim = c(T, state_dim, state_dim))
  innovations <- matrix(0, T, obs_dim)
  innovation_covs <- array(0, dim = c(T, obs_dim, obs_dim))

  # Initial time step
  R_t <- R
  diag(R_t) <- ensemble_vars[1,]
  K <- kalman_gain(S0, C, R_t)
  mf[1,] <- m0 + K %*% (y[1,] - C %*% m0)
  Vf[1,,] <- S0 - K %*% C %*% S0
  S[1,,] <- S0
  innovations[1,] <- y[1,] - C %*% mf[1,]
  innovation_covs[1,,] <- C %*% S0 %*% t(C) + R_t

  # Remaining time steps
  for(t in 2:T) {
    # Predict
    m_pred <- A %*% mf[t-1,]
    S[t-1,,] <- A %*% Vf[t-1,,] %*% t(A) + Q

    # Update R with time-varying variance
    R_t <- R
    diag(R_t) <- ensemble_vars[t,]

    if(!any(is.na(y[t,]))) {
      # Update
      innovations[t,] <- y[t,] - C %*% m_pred
      K <- kalman_gain(S[t-1,,], C, R_t)
      mf[t,] <- m_pred + K %*% innovations[t,]
      Vf[t,,] <- S[t-1,,] - K %*% C %*% S[t-1,,]
      innovation_covs[t,,] <- C %*% S[t-1,,] %*% t(C) + R_t
    } else {
      Vf[t,,] <- S[t-1,,]
    }
  }

  list(
    filtered_means = mf,
    filtered_covs = Vf,
    pred_covs = S,
    innovations = innovations,
    innovation_covs = innovation_covs
  )
}

#' Backward pass of Kalman smoother
#'
#' @param y Matrix of observations
#' @param mf Filtered means from forward pass
#' @param Vf Filtered covariances from forward pass
#' @param S Predicted covariances from forward pass
#' @param A State transition matrix
#' @return List containing smoothed means and covariances
#' @export
backward_pass <- function(y, mf, Vf, S, A) {
  T <- nrow(y)
  state_dim <- ncol(mf)

  # Storage
  ms <- matrix(0, T, state_dim)
  Vs <- array(0, dim = c(T, state_dim, state_dim))
  CV <- array(0, dim = c(T-1, state_dim, state_dim))

  # Last time step
  ms[T,] <- mf[T,]
  Vs[T,,] <- Vf[T,,]

  # Remaining time steps
  for(t in (T-1):1) {
    if(!all(is.na(y[t,]))) {
      # Compute smoother gain
      J <- try(solve(S[t,,], A %*% Vf[t,,]))
      if(!inherits(J, "try-error")) {
        J <- t(J)
        Vs[t,,] <- Vf[t,,] + J %*% (Vs[t+1,,] - S[t,,]) %*% t(J)
        ms[t,] <- mf[t,] + J %*% (ms[t+1,] - A %*% mf[t,])
        CV[t,,] <- Vs[t+1,,] %*% t(J)
      }
    }
  }

  list(
    smoothed_means = ms,
    smoothed_covs = Vs,
    cross_covs = CV
  )
}

#' Helper function to compute Kalman gain
#' @keywords internal
kalman_gain <- function(V, C, R) {
  S <- C %*% V %*% t(C) + R
  K <- V %*% t(C) %*% solve(S)
  return(K)
}

#' Compute negative log likelihood from Kalman filter innovations
#' @keywords internal
compute_nll <- function(innovations, innovation_covs, epsilon = 1e-6) {
  T <- nrow(innovations)
  n_coords <- ncol(innovations)
  nll <- 0
  nll_values <- numeric(T)
  c <- log(2 * pi) * n_coords

  for(t in seq_len(T)) {
    if(!any(is.na(innovations[t,]))) {
      # Regularize covariance
      reg_cov <- innovation_covs[t,,] + diag(epsilon, n_coords)

      # Compute likelihood components
      log_det <- determinant(reg_cov, logarithm = TRUE)$modulus
      quad_term <- t(innovations[t,]) %*% solve(reg_cov, innovations[t,])

      # Increment
      nll_inc <- 0.5 * abs(log_det + quad_term + c)
      nll_values[t] <- nll_inc
      nll <- nll + nll_inc
    }
  }

  list(nll = nll, nll_values = nll_values)
}

#' Main Ensemble Kalman Smoother function
#'
#' @param ensemble_preds List of matrices containing predictions from each model
#' @param smooth_param Smoothing parameter (scalar or vector)
#' @param ensemble_mode Method for combining ensemble predictions ("mean" or "median")
#' @return List containing smoothed trajectories and associated statistics
#' @export
filter_eks <- function(ensemble_preds,
                       smooth_param = NULL,
                       ensemble_mode = "median") {

  # Get ensemble statistics
  ens_stats <- ensemble_stats(ensemble_preds, mode = ensemble_mode)
  y <- ens_stats$means
  ensemble_vars <- ens_stats$variances

  # Initialize state space model parameters
  n_times <- nrow(y)
  n_states <- ncol(y)

  # Default parameters if not provided
  if(is.null(smooth_param)) {
    smooth_param <- rep(0.1, n_states)
  } else if(length(smooth_param) == 1) {
    smooth_param <- rep(smooth_param, n_states)
  }

  # Build state space matrices
  A <- diag(n_states)  # State transition matrix
  Q <- diag(smooth_param)  # Process noise covariance
  C <- diag(n_states)  # Measurement matrix
  R <- diag(n_states)  # Base measurement noise

  # Initial state
  m0 <- colMeans(y, na.rm = TRUE)
  S0 <- diag(apply(y, 2, var, na.rm = TRUE))

  # Forward pass
  forward <- forward_pass(y, m0, S0, A, Q, C, R, ensemble_vars)

  # Backward pass
  backward <- backward_pass(y,
                            forward$filtered_means,
                            forward$filtered_covs,
                            forward$pred_covs,
                            A)

  # Compute NLL
  nll <- compute_nll(forward$innovations, forward$innovation_covs)

  # Return results
  list(
    smoothed_means = backward$smoothed_means,
    smoothed_covs = backward$smoothed_covs,
    filtered_means = forward$filtered_means,
    filtered_covs = forward$filtered_covs,
    nll = nll$nll,
    nll_values = nll$nll_values,
    ensemble_stats = ens_stats
  )
}
