# Update animovement packages

This will check all *animovement* packages (and their dependencies) for
updates and (optionally) install those updates.

## Usage

``` r
animovement_update(..., install = FALSE, repos = animovement_repos())
```

## Arguments

- ...:

  arguments passed to
  [`animovement_deps`](http://animovement.dev/animovement/reference/animovement_deps.md).

- install:

  logical. `TRUE` will proceed to install outdated packages, whereas
  `FALSE` (recommended) will print the installation command asking you
  to run it in a clean R session.

- repos:

  the repositories to check against and install from. Defaults to
  [`animovement_repos()`](http://animovement.dev/animovement/reference/animovement_repos.md),
  which adds the *animovement* R-universe to `getOption("repos")`.

## Value

`animovement_update` returns `NULL` invisibly.

## See also

[`animovement_deps`](http://animovement.dev/animovement/reference/animovement_deps.md),
[`animovement`](http://animovement.dev/animovement/reference/animovement.md)
