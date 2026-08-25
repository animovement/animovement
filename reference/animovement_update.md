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
  [`animovement_deps`](https://animovement.dev/animovement/reference/animovement_deps.md).

- install:

  logical. `TRUE` will proceed to install outdated packages, whereas
  `FALSE` (recommended) will print the installation command asking you
  to run it in a clean R session.

- repos:

  the repositories to check against and install from. Defaults to
  [`animovement_repos()`](https://animovement.dev/animovement/reference/animovement_repos.md),
  which adds the *animovement* R-universe to `getOption("repos")`.

## Value

`animovement_update` returns `NULL` invisibly.

## See also

[`animovement_deps`](https://animovement.dev/animovement/reference/animovement_deps.md),
[`animovement`](https://animovement.dev/animovement/reference/animovement.md)

## Examples

``` r
# Report which packages are behind, without changing anything
animovement_update()
#> All animovement packages up-to-date

# Pass install = TRUE to actually install what is out of date
if (FALSE) {
animovement_update(install = TRUE)
}
```
