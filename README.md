
<!-- README.md is generated from README.Rmd. Please edit that file -->

# animovement

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/773406370.svg)](https://zenodo.org/doi/10.5281/zenodo.13235277)
[![R-CMD-check](https://github.com/animovement/animovement/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/animovement/animovement/actions/workflows/R-CMD-check.yaml)
[![animovement status
badge](https://animovement.r-universe.dev/badges/animovement)](https://animovement.r-universe.dev)
<!-- badges: end -->

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
  `animovement`](https://www.roald-arboel.com/animovement/articles/animovement.html)
- [Read trackball
  data](https://www.roald-arboel.com/animovement/articles/read-trackball.html)
- [Clean
  tracks](https://www.roald-arboel.com/animovement/articles/clean-tracks.html)
- [Calculate
  kinematics](https://www.roald-arboel.com/animovement/articles/calculate-kinematics.html)
- [Calculate summary
  statistics](https://www.roald-arboel.com/animovement/articles/calculate-summary-statistics.html)

## Status

> **Warning**
>
> 🏗️ The package is currently in early development and the interface is
> subject to change. Feel free to play around and provide feedback.

## Contribute

**If your favourite type of movement data is not currently supported, we
would love to get a sample of your data to support it!**

If you enjoy the package, please make sure to [cite it](#citation). If
you find a bug, feel free to open an issue.

<!-- ## Acknowledgements -->

<!-- *animovement* is all about the data, and we are deeply grateful for all those who have shared data with us to implement and test our code. Thank you! -->

<!-- - [Stan Edwards](): Trackball with optical flow, free. -->

<!-- - [Estelle Moubarak](): Trackball with optical flow, fixed. -->

<!-- - [Maria Cozan](): Treadmill with rotary encoder. -->

<!-- - [Violette Chiara](): AnimalTA -->

## Citation

To cite *animovement* in publications use:

``` r
citation("animovement")
#> To cite package 'animovement' in publications use:
#> 
#>   Roald-Arbøl M (2025). "animovement: An R toolbox for analysing animal
#>   movement across space and time."
#>   <http://www.roald-arboel.com/animovement/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{roaldarbol:2025,
#>     title = {animovement: An R toolbox for analysing animal movement across space and time.},
#>     author = {Mikkel Roald-Arbøl},
#>     year = {2025},
#>     url = {http://www.roald-arboel.com/animovement/},
#>     abstract = {An R toolbox for analysing animal movement across space and time.},
#>     version = {0.6.0},
#>   }
```
