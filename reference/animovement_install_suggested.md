# Download all suggested packages

In `animovement`, we have a minimal dependency policy to keep the
package light and fast to install. However, we rely on several packages
for testing and specific features. These "soft dependencies" can be
downloaded at once using this function, allowing you to fully utilize
all of animovement's functionalities without errors.

## Usage

``` r
animovement_install_suggested(package = "animovement")

animovement_show_suggested(package = "animovement")
```

## Arguments

- package:

  Character string specifying the package name. Currently only
  `"animovement"` is supported.

## Value

Invisible `NULL`. Used for side-effect of installing packages.

## Details

To reduce the dependency load, animovement by default will not download
all internally needed packages. It will ask the user to download them
only if they are needed. The current function can help install all
packages that animovement and its ecosystem packages might need.
`animovement_show_suggested()` is a convenient helper to show the
current list of suggested packages.

This function will check the `Suggests` field of animovement and all its
imported packages (aniframe, aniread, aniprocess, animetric, anicheck,
anivis), excluding development packages (knitr, rmarkdown, testthat).

If package `{pak}` is installed,
[`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.html)
will be used to install packages. Otherwise,
[`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)
is used with the animovement R-universe repository.

## Examples

``` r
# download all suggested packages
if (FALSE) {
  animovement_install_suggested("animovement")
}

# listing all soft/weak dependencies
animovement_show_suggested()
#> 
#> ── Suggested packages for animovement ecosystem ──
#> 
#> animovement: here, circular, ggplot2, patchwork, readxl, rhdf5, signal, tibble,
#> tidyr, tinytable
#> aniframe: covr, pkgdown
#> aniread: arrow, covr, curl, rhdf5, xml2, c3dr, withr
#> aniprocess: covr, signal, stinepack
#> animetric: circular, data.table, sf
#> anicheck: withr
#> anivis: ragg
```
