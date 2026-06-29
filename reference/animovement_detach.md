# Detach animovement packages

Detaches *animovement* packages, removing them from the
[`search`](https://rdrr.io/r/base/search.html) path.

## Usage

``` r
animovement_detach(
  ...,
  unload = FALSE,
  force = FALSE,
  include.self = TRUE,
  session = FALSE
)
```

## Arguments

- ...:

  comma-separated package names, quoted or unquoted, or vectors of
  package names. If left empty, all packages returned by
  [`animovement_packages`](http://animovement.dev/animovement/reference/animovement_packages.md)
  are detached.

- unload:

  logical. `TRUE` also unloads the packages using
  [`detach(name, unload = TRUE)`](https://rdrr.io/r/base/detach.html).

- force:

  logical. should a *animovement* package be detached / unloaded even
  though other attached packages depend on it?

- include.self:

  logical. `TRUE` also includes the `animovement` package - only
  applicable if `...` is left empty.

- session:

  logical. `TRUE` also removes the packages from
  `options("animovement.extend")`, so they will not be attached again
  with [`library(animovement)`](https://animovement.dev) in the current
  session. If `...` is left empty and `include.self = TRUE`, this will
  clear **all** *animovement* options set for the session.

## Value

`animovement_detach` returns `NULL` invisibly.

## See also

[`animovement_extend`](http://animovement.dev/animovement/reference/animovement_extend.md),
[`animovement`](http://animovement.dev/animovement/reference/animovement.md)
