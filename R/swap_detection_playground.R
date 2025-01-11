library(dplyr)
library(ggplot2)

t <- seq(0, 10, length.out = 1000)  # Extended timeframe

# Individual 1, keypoints a, b, c
x11 <- 100 + 50 * sin(2 * pi * t)
y11 <- 80 + 20 * cos(4 * pi * t)
x12 <- x11 + 20
y12 <- y11 + 20
x13 <- x11 - 15
y13 <- y11 - 25

# Individual 2, keypoints a, b, c
x21 <- 200 + 50 * sin(2 * pi * t + pi)
y21 <- 80 + 20 * cos(4 * pi * t + pi)
x22 <- x21 + 20
y22 <- y21 + 20
x23 <- x21 - 15
y23 <- y21 - 25

# Individual 3, keypoints a, b, c
x31 <- 300 + 50 * sin(2 * pi * t + pi / 2)
y31 <- 100 + 20 * cos(4 * pi * t + pi / 2)
x32 <- x31 + 20
y32 <- y31 + 20
x33 <- x31 - 15
y33 <- y31 - 25

# Create tidy data frame
data <- data.frame(
  time = rep(t, 9),
  individual = rep(rep(c("individual1", "individual2", "individual3"), each = length(t)), 3),
  keypoint = rep(c("a", "b", "c"), each = 3 * length(t)),
  x = c(x11, x21, x31, x12, x22, x32, x13, x23, x33),
  y = c(y11, y21, y31, y12, y22, y32, y13, y23, y33),
  confidence = 0.9  # Example confidence
)

# Add noise
set.seed(42)
noise <- 3
data$x <- data$x + rnorm(nrow(data), 0, noise)
data$y <- data$y + rnorm(nrow(data), 0, noise)

# Introduce swaps at specific times
swap_times_start <- c(150, 300, 400, 700)
swap_times_end <- c(170, 320, 420, 720)

# Swap individuals
for (i in 1:length(swap_times_start)) {
  data <- data |>
    mutate(swap = if_else(time > t[swap_times_start[i]] & time < t[swap_times_end[i]], TRUE, FALSE),
           individual = case_when(
             swap == TRUE & individual == "individual1" ~ "individual2",
             swap == TRUE & individual == "individual2" ~ "individual3",
             swap == TRUE & individual == "individual3" ~ "individual1",
             .default = individual),
           confidence = if_else(swap == TRUE, 0.4, confidence))
}


# Plot results
library(ggplot2)

# Create plot data
plot_data <- bind_rows(
  data %>% mutate(type = "raw"),
  # fixed %>% mutate(type = "fixed")
)

# Add shading for swap regions
swap_regions_df <- tibble(.rows = length(swap_times_start)) %>%
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
  facet_grid(cols = vars(individual),
             rows = vars(keypoint)) +
  theme_bw() +
  labs(title="Raw vs Corrected Trajectories",
       subtitle="Pink regions show introduced swaps")


filtered_pose_data <- filter_hungarian(plot_data)
filtered_pose_data <- greedy_matching(plot_data)
filtered_pose_data |>
  ggplot() +
  geom_line(aes(x=time, y=y, color=interaction(individual, keypoint, type),
                linetype=type)) +
  geom_rect(data=swap_regions_df,
            aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
            fill="pink", alpha=0.3) +
  facet_grid(cols = vars(individual),
             rows = vars(keypoint)) +
  theme_bw() +
  labs(title="Raw vs Corrected Trajectories",
       subtitle="Pink regions show introduced swaps")
