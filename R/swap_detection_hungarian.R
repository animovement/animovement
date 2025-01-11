#' #' Fix identity swaps in pose tracking data using hierarchical Hungarian algorithm
#' #'
#' #' @param data Data frame with columns: time, individual, keypoint, x, y, confidence
#' #' @param max_individual_cost Maximum cost for individual assignment
#' #' @param max_keypoint_cost Maximum cost for keypoint assignment
#' #' @return Data frame with corrected identities
#' #' @export
#' fix_hierarchical_swaps <- function(data,
#'                                    max_individual_cost = 100,
#'                                    max_keypoint_cost = 50) {
#'
#'   # Load required package
#'   if (!requireNamespace("clue", quietly = TRUE)) {
#'     stop("Package 'clue' needed. Please install it.", call. = FALSE)
#'   }
#'   require(dplyr)
#'
#'   # Order data
#'   data <- data %>%
#'     arrange(time, individual, keypoint)
#'
#'   # Get unique values
#'   times <- unique(data$time)
#'   individuals <- unique(data$individual)
#'   keypoints <- unique(data$keypoint)
#'
#'   n_times <- length(times)
#'   n_individuals <- length(individuals)
#'   n_keypoints <- length(keypoints)
#'
#'   # Function to compute costs between point sets
#'   compute_costs <- function(prev_points, curr_points, prev_velocities = NULL) {
#'     n_rows <- nrow(prev_points)
#'     n_cols <- nrow(curr_points)
#'     costs <- matrix(0, n_rows, n_cols)
#'
#'     for(i in 1:n_rows) {
#'       for(j in 1:n_cols) {
#'         # Distance cost
#'         dist_cost <- sqrt((curr_points$x[j] - prev_points$x[i])^2 +
#'                             (curr_points$y[j] - prev_points$y[i])^2)
#'
#'         # Velocity cost if available
#'         vel_cost <- 0
#'         if(!is.null(prev_velocities)) {
#'           vel_x <- curr_points$x[j] - prev_points$x[i]
#'           vel_y <- curr_points$y[j] - prev_points$y[i]
#'           prev_vel_x <- prev_velocities$x[i]
#'           prev_vel_y <- prev_velocities$y[i]
#'           vel_cost <- sqrt((vel_x - prev_vel_x)^2 + (vel_y - prev_vel_y)^2)
#'         }
#'
#'         # Include confidence in cost
#'         confidence_factor <- 2 - (prev_points$confidence[i] + curr_points$confidence[j])/2
#'
#'         # Combine costs
#'         costs[i,j] <- (dist_cost + 0.5 * vel_cost) * confidence_factor
#'       }
#'     }
#'     return(costs)
#'   }
#'
#'   # Process each frame
#'   corrected_data <- data
#'   prev_velocities <- NULL
#'
#'   for(t in 2:n_times) {
#'     curr_time <- times[t]
#'     prev_time <- times[t-1]
#'
#'     # Get current and previous frame data
#'     prev_frame <- corrected_data %>% filter(time == prev_time)
#'     curr_frame <- data %>% filter(time == curr_time)
#'
#'     # Step 1: Fix individual assignments
#'     # ----------------------------------
#'     # Compute individual centroids
#'     prev_individuals <- prev_frame %>%
#'       group_by(individual) %>%
#'       summarize(x = mean(x),
#'                 y = mean(y),
#'                 confidence = mean(confidence))
#'
#'     curr_individuals <- curr_frame %>%
#'       group_by(individual) %>%
#'       summarize(x = mean(x),
#'                 y = mean(y),
#'                 confidence = mean(confidence))
#'
#'     # Compute costs and assignments between individuals
#'     ind_costs <- compute_costs(prev_individuals, curr_individuals, NULL)
#'     ind_assignment <- clue::solve_LSAP(ind_costs)
#'
#'     # Apply individual assignments if cost is below threshold
#'     valid_ind_assignments <- ind_costs[cbind(1:n_individuals, ind_assignment)] < max_individual_cost
#'
#'     if(any(!valid_ind_assignments)) {
#'       # Reorder individuals based on assignment
#'       new_individuals <- individuals[ind_assignment]
#'       curr_frame <- curr_frame %>%
#'         mutate(individual = new_individuals[match(individual, individuals)])
#'     }
#'
#'     # Step 2: Fix keypoint assignments within each individual
#'     # -----------------------------------------------------
#'     for(ind in individuals) {
#'       # Get keypoints for this individual
#'       prev_kpts <- prev_frame %>% filter(individual == ind)
#'       curr_kpts <- curr_frame %>% filter(individual == ind)
#'
#'       # Compute costs between keypoints
#'       kpt_costs <- compute_costs(prev_kpts, curr_kpts,
#'                                  if(t > 2) prev_velocities[[ind]] else NULL)
#'       kpt_assignment <- clue::solve_LSAP(kpt_costs)
#'
#'       # Apply keypoint assignments if cost is below threshold
#'       valid_kpt_assignments <- kpt_costs[cbind(1:n_keypoints, kpt_assignment)] < max_keypoint_cost
#'
#'       if(any(!valid_kpt_assignments)) {
#'         # Reorder keypoints based on assignment
#'         curr_kpts <- curr_kpts %>%
#'           arrange(keypoint) %>%
#'           mutate(keypoint = keypoints[kpt_assignment])
#'
#'         # Update current frame
#'         curr_frame <- curr_frame %>%
#'           rows_update(curr_kpts, by = c("time", "individual"))
#'       }
#'
#'       # Store velocities for next frame
#'       prev_velocities[[ind]] <- data.frame(
#'         x = curr_kpts$x - prev_kpts$x,
#'         y = curr_kpts$y - prev_kpts$y
#'       )
#'     }
#'
#'     # Update corrected data
#'     corrected_data <- corrected_data %>%
#'       rows_update(curr_frame, by = "time")
#'   }
#'
#'   return(corrected_data)
#' }
#'
#' # Example usage with tidy data
#' if(TRUE) {
#'   # Generate example data
#'   t <- seq(0, 5, length.out=500)
#'
#'   # Individual 1, keypoint 1
#'   x11 <- 100 + 50 * sin(2*pi*t)
#'   y11 <- 80 + 20 * cos(4*pi*t)
#'
#'   # Individual 1, keypoint 2
#'   x12 <- x11 + 20
#'   y12 <- y11 + 20
#'
#'   # Individual 2, keypoint 1
#'   x21 <- 200 + 50 * sin(2*pi*t + pi)
#'   y21 <- 80 + 20 * cos(4*pi*t + pi)
#'
#'   # Individual 2, keypoint 2
#'   x22 <- x21 + 20
#'   y22 <- y21 + 20
#'
#'   # Create tidy data frame
#'   data <- data.frame(
#'     time = rep(t, 4),
#'     individual = rep(rep(c("individual1", "individual2"), each=length(t)), 2),
#'     keypoint = rep(c(1, 2), each=2*length(t)),
#'     x = c(x11, x21, x12, x22),
#'     y = c(y11, y21, y12, y22),
#'     confidence = 0.9  # Example confidence
#'   )
#'
#'   # Add noise
#'   set.seed(42)
#'   noise <- 3
#'   data$x <- data$x + rnorm(nrow(data), 0, noise)
#'   data$y <- data$y + rnorm(nrow(data), 0, noise)
#'
#'   # Introduce swaps at specific times
#'   swap_times_start <- c(150, 300, 400)
#'   swap_times_end <- c(170, 320, 420)
#'
#'   # Swap individuals
#'   for(i in 1:length(swap_times_start)) {
#'     data <- data |>
#'       mutate(swap = if_else(time > t[swap_times_start[i]] & time < t[swap_times_end[i]], TRUE, FALSE),
#'              individual = case_when(
#'                swap == TRUE & individual == "individual1" ~ "individual2",
#'                swap == TRUE & individual == "individual2" ~ "individual1",
#'                .default = individual))
#'   }
#'
#'   # Fix swaps
#'   fixed <- fix_hierarchical_swaps(data)
#'
#'   # Plot results
#'   library(ggplot2)
#'
#'   # Create plot data
#'   plot_data <- bind_rows(
#'     data %>% mutate(type = "raw"),
#'     # fixed %>% mutate(type = "fixed")
#'   )
#'
#'   # Add shading for swap regions
#'   swap_regions_df <- data.frame(
#'     start = c(150, 300, 400),
#'     end = c(170, 320, 420)
#'   ) %>%
#'     mutate(
#'       start = t[swap_times_start],
#'       end = t[swap_times_end]
#'     )
#'
#'   # Plot
#'   plot_data |>
#'   ggplot() +
#'     geom_line(aes(x=time, y=x, color=interaction(individual, keypoint, type),
#'                   linetype=type)) +
#'     geom_rect(data=swap_regions_df,
#'               aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
#'               fill="pink", alpha=0.3) +
#'     # facet_wrap(~individual) +
#'     theme_bw() +
#'     labs(title="Raw vs Corrected Trajectories",
#'          subtitle="Pink regions show introduced swaps")
#' }
#'
#'
#'
#' detect_keypoint_swaps <- function(data){
#'   d <- data |>
#'     pivot_wider(id_cols = c(time),
#'                 values_from = c(x,y),
#'                 names_from = c(individual, keypoint)) |>
#'     mutate(across(!time, ~ .x - lag(.x))) |>
#'     mutate(across(!time, ~ filter_kalman(.x, sampling_rate = 1)))
#' }
