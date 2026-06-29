# Read trackball data

``` r

library(animovement)
#> -- Attaching packages ------------------------------------- animovement 0.7.3 --
#> v aniframe   0.6.0     v anicheck   0.2.0
#> v aniread    0.5.0     v animetric  0.3.2
#> v anispace   0.1.3     v anivis     0.2.0
#> v aniprocess 0.2.0
```

Trackball experiments, in which animals are tethered/restrained atop a
(most commonly) styrofoam ball are common experiments within animal
behaviour and neuroscience.

There are different ways to obtain data from trackball experiments, and
although we only currently support limited ways, we are looking forward
to supporting all your trackball experiments!

The main interface from your data to a standardised trackball data frame
is the `read_trackball()` function. The most common experimental setup
uses two optical computer mics (or other optical flow sensors), which
detects movements of the ball at different axes and is the primary
experimental setup we support (although see our
[roadmap](https://roald-arboel.com/trackballr#roadmap)!). For this
experiment, `read_trackball()` requires **two file paths, one for each
sensor**.

You can find pairs with variations of
[`list.files()`](https://rdrr.io/r/base/list.files.html) and for-loops.
We’ll go through how to use `read_trackball()` for a single experimental
trial, as well as a few ways of managing multiple experimental trials at
the same time.

## The `read_trackball()` function

We recommend keeping your analysis scripts and data in the same project
(which doesn’t mean folder), and to use the great
[here](https://here.r-lib.org/index.html) package package to manage your
files (see their documentation to learn more about it).

First, we’ll let R know which is our project root folder.

``` r

library(here, quietly = TRUE)
#> here() starts at /home/runner/work/animovement/animovement
here::i_am("vignettes/articles/read-trackball.Rmd")
#> here() starts at /home/runner/work/animovement/animovement
```

Our data lives in a funky place (because of the constraints of being in
an R package), but giving file paths is easy.

``` r

filepaths <- get_sample_data("trackball", quiet = TRUE)

# `get_sample_data()` downloads a pair of example sensor files. In your own
# analysis these would be paths to your two sensor files, e.g.
# c(here("data", "trackball", "sensor_1.csv"), here("data", "trackball", "sensor_2.csv"))
```

Great, now we have our file paths. But the function needs to know a few
more things.

- **Experimental configuration**
  - Takes either `"free"` or `"fixed"`.
- **Time column**
  - Which column contains time stamps in either seconds or datetime
    format (takes either column number or name in quotes, e.g. `4` or
    `"time"`)
- **Sampling rate**
  - This *can* be your actual sampling rate, but we use this information
    to bin observations, so you can easily choose a lower sampling rate.
- **Mouse Dots-per-cm (DPCM)**
  - This value is optional, but if provided is used to convert your
    spatial estimates from unit-less to centimeters. Can be found for
    most computer mice.

Let’s try to read our provided files:

``` r

df <- read_trackball(
  filepaths,
  setup = "of_free",
  col_time = "datetime",
  sampling_rate = 60
) |>
  # Trackball coordinates come out in sensor dots; calibrate to cm
  # (394 dots per cm for this mouse sensor).
  set_unit_space(to_unit = "cm", calibration_factor = 1 / 394)
```

## Working with multiple experiments

When working with multiple experimental trials, there are multiple ways
of reading all the data whilst keeping track of all the important
information. Here we’ll cover a few ways that are supported by
*animovement*.

- [**Read from from metadata file**](#from-metadata).
  - *When to use*: File names contain little or no information.
- [**Read from file names**](#from-filenames).
  - *When to use*: File names contain enough information that can be
    used to distinguish between experiments.

### Read from file names

***When to use***: File names contain enough information that can be
used to distinguish between experiments.

**WORK IN PROGRESS!**
