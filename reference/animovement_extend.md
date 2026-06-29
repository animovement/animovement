# Extend the animovement

Loads additional packages as part of the *animovement*.

## Usage

``` r
animovement_extend(
  ...,
  install = FALSE,
  check.conflicts = !isTRUE(getOption("animovement.quiet"))
)
```

## Arguments

- ...:

  comma-separated package names, quoted or unquoted, or vectors of
  package names.

- install:

  logical. Install packages not available?

- check.conflicts:

  logical. Should conflicts between extension packages and attached
  packages be checked?

## Value

`animovement_extend` returns `NULL` invisibly.

## Details

When the *animovement* is extended calling `animovement_extend(...)`,
the packages that are not attached are attached, but conflicts are
checked for all specified packages. An `options("animovement.extend")`
is set which stores these extension packages, regardless of whether they
were already attached or not. When calling
[`animovement_packages`](http://animovement.dev/animovement/reference/animovement_packages.md),
[`animovement_deps`](http://animovement.dev/animovement/reference/animovement_deps.md),
[`animovement_conflicts`](http://animovement.dev/animovement/reference/animovement_conflicts.md),
[`animovement_update`](http://animovement.dev/animovement/reference/animovement_update.md),
[`animovement_sitrep`](http://animovement.dev/animovement/reference/animovement_sitrep.md)
or
[`animovement_detach`](http://animovement.dev/animovement/reference/animovement_detach.md),
these packages are included as part of the *animovement*. To extend the
*animovement* for the current session when it is not yet loaded, users
can also set `options(animovement.extend = c(...))`, where `c(...)` is a
character vector of package names, before calling
[`library(animovement)`](https://animovement.dev).

## See also

[`animovement_detach`](http://animovement.dev/animovement/reference/animovement_detach.md),
[`animovement`](http://animovement.dev/animovement/reference/animovement.md)
