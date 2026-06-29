# Install (missing) animovement packages

This function (by default) checks if any *animovement* package is
missing and installs the missing package(s).

## Usage

``` r
animovement_install(..., only.missing = TRUE, install = TRUE)
```

## Arguments

- ...:

  comma-separated package names, quoted or unquoted, or vectors of
  package names. If left empty, all packages returned by
  [`animovement_packages`](http://animovement.dev/animovement/reference/animovement_packages.md)
  are checked.

- only.missing:

  logical. `TRUE` only installs packages that are unavailable. `FALSE`
  installs all packages, even if they are available.

- install:

  logical. `TRUE` will proceed to install packages, whereas `FALSE`
  (recommended) will print the installation command asking you to run it
  in a clean R session.

## Value

`animovement_install` returns `NULL` invisibly.

## Note

There is also the possibility to set
`options(animovement.install = TRUE)` before
[`library(animovement)`](https://animovement.dev), which will call
`animovement_install()` before loading any packages to make sure all
packages are available. If you are using a `.animovement` configuration
file inside a project (see vignette), you can also place
`_opt_animovement.install = TRUE` before the list of packages in that
file.

## See also

[`animovement_update`](http://animovement.dev/animovement/reference/animovement_update.md),
[`animovement`](http://animovement.dev/animovement/reference/animovement.md)
