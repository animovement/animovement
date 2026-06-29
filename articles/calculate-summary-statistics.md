# Calculate movement statistics

Voilá! We’ve arrived at the final step, and all that’s left is to
calculate summary statistics! We can decide whether which measures of
central tendency and dispersion (e.g. mean and SD). As most movement
data is highly skewed, we recommend to use median and MAD (default
setting).

``` r

library(tinytable)
df_kinematics_clean |>
  summarise_aniframe(measures = "median_mad") |>
  mutate(across(where(is.numeric), as.numeric)) |>
  tidyr::pivot_longer(where(is.numeric),
    names_to = "Measure",
    values_to = "Value"
  ) |> 
  select(-any_of(c("individual", "keypoint"))) |>
  tt() |>
  format_tt(digits = 2)
```

| Measure                     | Value |
|-----------------------------|-------|
| median_speed                | 1.08  |
| mad_speed                   | 0.32  |
| median_acceleration         | 0     |
| mad_acceleration            | 5.99  |
| median_angular_speed        | 0     |
| mad_angular_speed           | 0     |
| median_angular_velocity     | 0     |
| mad_angular_velocity        | 0     |
| median_angular_acceleration | 0     |
| mad_angular_acceleration    | 0     |
| median_heading              | 0.79  |
| mad_heading                 | 0     |
| total_path_length           | 0.25  |
| total_angular_path_length   | NA    |
| net_displacement            | 0.25  |
| straightness                | 1     |
| sinuosity                   | 0     |
| emax                        | Inf   |
