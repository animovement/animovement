# List all packages in the animovement

Core packages are first fetched from a project-level configuration file
(named `.animovement`, if found), otherwise the standard set of core
packages is returned. In addition, if `extensions = TRUE`, any packages
used to extend the *animovement* for the current session are also
returned.

## Usage

``` r
animovement_packages(extensions = TRUE, include.self = TRUE)
```

## Arguments

- extensions:

  logical. `TRUE` appends the set of core packages with all packages
  found in `options("animovement.extend")`.

- include.self:

  logical. Include the *animovement* package in the list?

## Value

A character vector of package names.

## See also

[`animovement_extend`](http://animovement.dev/animovement/reference/animovement_extend.md),
[`animovement`](http://animovement.dev/animovement/reference/animovement.md)

## Examples

``` r
animovement_packages()
#> [1] "aniframe"    "aniread"     "anispace"    "aniprocess"  "anicheck"   
#> [6] "animetric"   "anivis"      "animovement"
```
