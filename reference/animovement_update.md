# Update animovement packages

This will check all *animovement* packages (and their dependencies) for
updates and (optionally) install those updates.

## Usage

``` r
animovement_update(..., install = FALSE)
```

## Arguments

- ...:

  arguments passed to
  [`animovement_deps`](http://animovement.dev/animovement/reference/animovement_deps.md).

- install:

  logical. `TRUE` will proceed to install outdated packages, whereas
  `FALSE` (recommended) will print the installation command asking you
  to run it in a clean R session.

## Value

`animovement_update` returns `NULL` invisibly.

## See also

[`animovement_deps`](http://animovement.dev/animovement/reference/animovement_deps.md),
[`animovement`](http://animovement.dev/animovement/reference/animovement.md)
