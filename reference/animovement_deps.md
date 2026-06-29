# List all animovement dependencies

Lists all *animovement* dependencies and the local and CRAN versions of
packages and dependencies.

## Usage

``` r
animovement_deps(
  pkg = animovement_packages(),
  recursive = FALSE,
  repos = getOption("repos"),
  include.self = FALSE,
  check.deps = TRUE
)
```

## Arguments

- pkg:

  character vector of packages to check dependencies and versions of.
  The default is all *animovement* packages.

- recursive:

  logical. `TRUE` recursively determines all packages required to
  operate these packages. `FALSE` will only list the packages and their
  direct dependencies.

- repos:

  the repositories to use to check for updates. Defaults to
  `getOptions("repos")`.

- include.self:

  logical. `TRUE` also includes the *animovement* package and checks
  against the CRAN version.

- check.deps:

  logical. `FALSE` will not determine dependencies but only display the
  update status of packages in `pkg`.

## Value

A data frame giving the package names, the CRAN and local version, and a
logical variable stating whether the local version is behind the CRAN
version.

## See also

[`animovement_sitrep`](http://animovement.dev/animovement/reference/animovement_sitrep.md),
[`animovement`](http://animovement.dev/animovement/reference/animovement.md)
