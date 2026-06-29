# animovement

*An R toolbox for analysing movement across space and time*

The primary aim of the *animovement* package is to provide a unified,
standardised workflow for analysing movement data in a
*tidyverse*-friendly syntax.

*We work actively with the developers of the Python
[`movement`](https://movement.neuroinformatics.dev/) package, to reach a
similar data standards, workflow and use cases; if you prefer analysing
your data in Python, we highly recommend using
[`movement`](https://movement.neuroinformatics.dev/).*

## Installation

You can install the development version of *animovement* with:

``` r

install.packages('animovement', repos = c('https://animovement.r-universe.dev', 'https://cloud.r-project.org'))
```

Once you have installed the package, you can load it with:

``` r

library("animovement")
```

## Documentation

Analysis of animal movement follows a similar workflow irrespective of
the type of data (e.g. pose estimation, centroid tracking, trackball,
treadmill). See our docs to go through the steps, one-by-one:

- [Introduction to
  `animovement`](https://animovement.dev/animovement/articles/animovement.html)
- [Read trackball
  data](https://animovement.dev/animovement/articles/read-trackball.html)
- [Clean
  tracks](https://animovement.dev/animovement/articles/clean-tracks.html)
- [Calculate
  kinematics](https://animovement.dev/animovement/articles/calculate-kinematics.html)
- [Calculate summary
  statistics](https://animovement.dev/animovement/articles/calculate-summary-statistics.html)

## Status

> **Warning**
>
> 🏗️ The package is currently in early development and the interface is
> subject to change. Feel free to play around and provide feedback.

## Contribute

**If your favourite type of movement data is not currently supported, we
would love to get a sample of your data to support it!**

If you enjoy the package, please make sure to [cite it](#citation). If
you find a bug, feel free to open an issue!

## Citation

To cite *animovement* in publications use:

``` r

citation("animovement")
#> To cite package 'animovement' in publications use:
#> 
#>   https://animovement.dev/animovement/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{roaldarbol:2025,
#>     title = {animovement: An R toolbox for analysing movement across space and time.},
#>     author = {Mikkel Roald-Arbøl},
#>     year = {2025},
#>     url = {http://animovement.dev/},
#>     abstract = {An R toolbox for analysing movement across space and time.},
#>     version = {0.7.0},
#>   }
```
