t <- seq(0, 5, length.out=500)

# Individual 1, keypoint 1
x11 <- 100 + 50 * sin(2*pi*t)
y11 <- 80 + 20 * cos(4*pi*t)

# Individual 1, keypoint 2
x12 <- x11 + 20
y12 <- y11 + 20

# Individual 2, keypoint 1
x21 <- 200 + 50 * sin(2*pi*t + pi)
y21 <- 80 + 20 * cos(4*pi*t + pi)

# Individual 2, keypoint 2
x22 <- x21 + 20
y22 <- y21 + 20

# Create tidy data frame
data <- data.frame(
  time = rep(t, 4),
  individual = rep(rep(c("individual1", "individual2"), each=length(t)), 2),
  keypoint = rep(c("a", "b"), each=2*length(t)),
  x = c(x11, x21, x12, x22),
  y = c(y11, y21, y12, y22),
  confidence = 0.9  # Example confidence
)

# Add noise
set.seed(42)
noise <- 3
data$x <- data$x + rnorm(nrow(data), 0, noise)
data$y <- data$y + rnorm(nrow(data), 0, noise)

# Introduce swaps at specific times
swap_times_start <- c(150, 300, 400)
swap_times_end <- c(170, 320, 420)

# Swap individuals
for(i in 1:length(swap_times_start)) {
  data <- data |>
    mutate(swap = if_else(time > t[swap_times_start[i]] & time < t[swap_times_end[i]], TRUE, FALSE),
           individual = case_when(
             swap == TRUE & individual == "individual1" ~ "individual2",
             swap == TRUE & individual == "individual2" ~ "individual1",
             .default = individual))
}


# Plot results
library(ggplot2)

# Create plot data
plot_data <- bind_rows(
  data %>% mutate(type = "raw"),
  # fixed %>% mutate(type = "fixed")
)

# Add shading for swap regions
swap_regions_df <- data.frame(
  start = c(150, 300, 400),
  end = c(170, 320, 420)
) %>%
  mutate(
    start = t[swap_times_start],
    end = t[swap_times_end]
  )

# Plot
plot_data |>
  ggplot() +
  geom_line(aes(x=time, y=x, color=interaction(individual, keypoint, type),
                linetype=type)) +
  geom_rect(data=swap_regions_df,
            aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
            fill="pink", alpha=0.3) +
  # facet_wrap(~individual) +
  theme_bw() +
  labs(title="Raw vs Corrected Trajectories",
       subtitle="Pink regions show introduced swaps")
}



detect_keypoint_swaps <- function(data){
  d <- data |>
    pivot_wider(id_cols = c(time),
                values_from = c(x,y),
                names_from = c(individual, keypoint)) |>
    mutate(across(!time, ~ .x - lag(.x))) |>
    mutate(across(!time, ~ filter_highpass(.x, cutoff_freq = 0.5, sampling_rate = 30)))



  d2 <- d |>
    pivot_longer(cols = -time,
                 values_to = "value",
                 names_to = c("coord", "individual", "keypoint"),
                 names_sep = "_") |>
    pivot_wider(id_cols = c(time, individual, keypoint),
                values_from = "value",
                names_from = coord)
  d2 <- d2 |>
    group_by(individual, keypoint) |>
    mutate(x1 = abs(x),
           x1 = x1 / max(x1),
           y1 = abs(y),
           y1 = y1 / max(y1),
           ) |>
    group_by(individual, keypoint, time) |>
    mutate(e = mean(c(max(.data$x1), abs(.data$y1))),
           e = filter_rollmean(e, window_width = 3),
           # e = e - lag(e),
           peaks = if_else(e > 0.25, TRUE, FALSE))

  d3 <- filter(d2, peaks == TRUE) |>
    ungroup() |>
    group_by(individual, keypoint) |>
    mutate(row = row_number(),
           onset = if_else(row %% 2 == 0, FALSE, TRUE),
           offset = if_else(row %% 2 == 0, TRUE, FALSE))
  d2 |>
    ggplot(aes(time, e, colour = interaction(individual, keypoint))) +
    geom_line() +
    geom_point(data = filter(d3, peaks == TRUE), aes(colour = onset)) +
    facet_grid(rows = vars(individual),
               cols = vars(keypoint))

  # Find best match after detecting peaks

  # Swap data around
}
