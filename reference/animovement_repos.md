# Repositories to find animovement packages in

The *animovement* packages are published on
[R-universe](https://animovement.r-universe.dev) rather than CRAN, so
`getOption("repos")` alone will not find them. This adds the R-universe
to a repository vector unless it is already present, and is the default
for
[`animovement_deps`](http://animovement.dev/animovement/reference/animovement_deps.md),
[`animovement_update`](http://animovement.dev/animovement/reference/animovement_update.md)
and
[`animovement_install`](http://animovement.dev/animovement/reference/animovement_install.md).

## Usage

``` r
animovement_repos(repos = getOption("repos"))
```

## Arguments

- repos:

  a repository vector to extend. Defaults to `getOption("repos")`.

## Value

A named character vector of repository URLs.

## Details

A fresh R installation leaves CRAN set to the `"@CRAN@"` placeholder,
which
[`available.packages`](https://rdrr.io/r/utils/available.packages.html)
refuses to resolve. Any such entry is replaced with the CRAN cloud
mirror. The CRAN entry is kept rather than dropped because the
*animovement* dependencies live there.

## See also

[`animovement_update`](http://animovement.dev/animovement/reference/animovement_update.md),
[`animovement`](http://animovement.dev/animovement/reference/animovement.md)

## Examples

``` r
animovement_repos()
#>                                                          RSPM 
#> "https://packagemanager.posit.co/cran/__linux__/noble/latest" 
#>                                                          CRAN 
#>                                    "https://cran.rstudio.com" 
#>                                                               
#>                          "https://animovement.r-universe.dev" 

# Make it apply to install.packages() and friends for the whole session
if (FALSE) { # \dontrun{
options(repos = animovement_repos())
} # }
```
