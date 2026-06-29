# Calculate kinematics

``` r

library(animovement)
#> -- Attaching packages ------------------------------------- animovement 0.7.3 --
#> v aniframe   0.6.0     v anicheck   0.2.0
#> v aniread    0.5.0     v animetric  0.3.2
#> v anispace   0.1.3     v anivis     0.2.0
#> v aniprocess 0.2.0
library(tibble)
library(ggplot2)
library(tidyr)
#> 
#> Attaching package: 'tidyr'
#> The following object is masked from 'package:aniprocess':
#> 
#>     replace_na
library(dplyr, warn.conflicts = FALSE)
library(here)
#> here() starts at /home/runner/work/animovement/animovement
here::i_am("vignettes/articles/calculate-kinematics.Rmd")
#> here() starts at /home/runner/work/animovement/animovement
```

## Calculate kinematics

When we work with movement data, we are often interested in more than
just where an animal *is*; we’re interested in how fast it moves, where
it is heading etc. `calculate_kinematics` calculates a range of
kinematic variables:

- `distance`: The distance the animal moved since the last observation
  (simply calculated using Pythagoras’ theorem)
- `v_translation`: The translational velocity, like what you see on a
  speedometer in a car.
- `direction`: The direction (in radians) the animal is heading - where
  the arrow on the compass is heading.
- `rotation`: Difference from direction of the last observation.
- `v_rotation`: The rotational velocity (in rad/s).

``` r

# Augment all data in list
df_kinematics <- df_smooth |>
  calculate_kinematics()
glimpse(df_kinematics)
#> Rows: 19
#> Columns: 17
#> Sampling rate: 60 Hz
#> Time: 2023-09-14 14:37:55.977 to 2023-09-14 14:37:56.277
#> $ keypoint             <fct> centroid, centroid, centroid, centroid, centroid,…
#> $ time                 <dbl> 0.00000000, 0.01666667, 0.03333333, 0.05000000, 0…
#> $ x                    <dbl> NA, 0.005076142, 0.011421320, 0.016497462, 0.0203…
#> $ y                    <dbl> NA, 0.005076142, 0.011421320, 0.016497462, 0.0203…
#> $ speed                <dbl> NA, NA, 0.4845656, 0.3768843, 0.2692031, 0.215362…
#> $ acceleration         <dbl> NA, NA, NA, -6.460874e+00, -4.845656e+00, -1.6152…
#> $ path_length          <dbl> 0.000000000, 0.000000000, 0.008973436, 0.01615218…
#> $ v_x                  <dbl> NA, NA, 0.3426396, 0.2664975, 0.1903553, 0.152284…
#> $ v_y                  <dbl> NA, NA, 0.3426396, 0.2664975, 0.1903553, 0.152284…
#> $ a_x                  <dbl> NA, NA, NA, -4.568528e+00, -3.426396e+00, -1.1421…
#> $ a_y                  <dbl> NA, NA, NA, -4.568528e+00, -3.426396e+00, -1.1421…
#> $ heading              <dbl> NA, NA, 0.7853982, 0.7853982, 0.7853982, 0.785398…
#> $ heading_unwrapped    <dbl> NA, NA, 0.7853982, 0.7853982, 0.7853982, 0.785398…
#> $ angular_speed        <dbl> NA, NA, NA, 2.927346e-15, 0.000000e+00, 0.000000e…
#> $ angular_velocity     <dbl> NA, NA, NA, 2.927346e-15, 0.000000e+00, 0.000000e…
#> $ angular_acceleration <dbl> NA, NA, NA, NA, -8.782038e-14, 0.000000e+00, 0.00…
#> $ angular_path_length  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
```

### Clean kinematics

``` r

df_kinematics_clean <- df_kinematics |>
  filter_na_speed(threshold = 100)
```
