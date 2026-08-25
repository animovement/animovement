# List all animovement dependencies

Lists all *animovement* dependencies and the local and CRAN versions of
packages and dependencies.

## Usage

``` r
animovement_deps(
  pkg = animovement_packages(),
  recursive = FALSE,
  repos = animovement_repos(),
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
  [`animovement_repos()`](https://animovement.dev/animovement/reference/animovement_repos.md),
  which is `getOption("repos")` plus the R-universe the *animovement*
  packages are published on.

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

[`animovement_sitrep`](https://animovement.dev/animovement/reference/animovement_sitrep.md),
[`animovement`](https://animovement.dev/animovement/reference/animovement.md)

## Examples

``` r
# The packages the suite depends on, and their versions
head(animovement_deps())
#>      package       cran      local behind
#> 1   aniframe 0.7.0.9000 0.7.0.9000  FALSE
#> 2    aniread 0.6.0.9000 0.6.0.9000  FALSE
#> 3   anispace 0.2.0.9000 0.2.0.9000  FALSE
#> 4 aniprocess      0.4.0      0.4.0  FALSE
#> 5   anicheck      0.2.0      0.2.0  FALSE
#> 6  animetric      0.4.0      0.4.0  FALSE

# Including their dependencies in turn
nrow(animovement_deps(recursive = TRUE))
#> [1] 60
```
