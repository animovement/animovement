# Get a situation report on the animovement

This function gives a quick overview of the version of R and all
*animovement* packages (including availability updates for packages) and
indicates whether any project-level configuration files are used.

## Usage

``` r
animovement_sitrep(...)
```

## Arguments

- ...:

  arguments other than `pkg` passed to
  [`animovement_deps`](https://animovement.dev/animovement/reference/animovement_deps.md).

## Value

`animovement_sitrep` returns `NULL` invisibly.

## See also

[`animovement_deps`](https://animovement.dev/animovement/reference/animovement_deps.md),
[`animovement`](https://animovement.dev/animovement/reference/animovement.md)

## Examples

``` r
# Versions of every animovement package, and whether any are behind
animovement_sitrep()
#> -- animovement 0.8.0.9002: Situation Report ------------------------- R 4.6.1 --
#>  * Project config file: FALSE
#> -- Core packages --------------------------------------------------------------- 
#>  * anicore    (0.8.0.9002)
#>  * aniread    (0.7.0.9001)
#>  * anispace   (0.3.0.9001)
#>  * aniprocess (0.5.0.9001)
#>  * anicheck   (0.3.0.9001)
#>  * animetric  (0.5.0.9001)
#>  * anivis     (0.2.1.9000)
#> -- Dependencies ---------------------------------------------------------------- 
#>  * anytime    (0.3.13)
#>  * cli        (3.6.6)
#>  * data.table (1.18.6.1)
#>  * dplyr      (1.2.1)
#>  * ggplot2    (4.0.3)
#>  * hms        (1.1.4)
#>  * janitor    (2.2.1)
#>  * lifecycle  (1.0.5)
#>  * marquee    (1.2.1)
#>  * patchwork  (1.3.2)
#>  * pillar     (1.11.1)
#>  * purrr      (1.2.2)
#>  * rlang      (1.3.0)
#>  * stringr    (1.6.0)
#>  * tidyr      (1.3.2)
#>  * tidyselect (1.2.1)
#>  * vroom      (1.7.1)
```
