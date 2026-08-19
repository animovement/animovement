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
anivis), excluding packages only needed for development or documentation
workflows (knitr, rmarkdown, testthat, pak, here, covr, pkgdown, withr,
ragg, curl, readxl, tibble, tinytable), and packages already required by
an ecosystem package, which are installed regardless.

Under WebR, packages are installed with `webr::install()`, since
[`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)
cannot build Emscripten packages in the browser. Otherwise, if `{pak}`
is installed then
[`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.html)
is used, and failing that
[`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).
Repositories searched are CRAN, the animovement R-universe and the
Bioconductor R-universe – the last of which is where `rhdf5` comes from.

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
#> animovement: circular, signal
#> aniread: arrow, rhdf5, xml2, c3dr
#> aniprocess: signal, stinepack
#> animetric: circular, sf
```
