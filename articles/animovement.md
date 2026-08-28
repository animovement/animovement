# Introduction to animovement

*animovement* is a meta-package. It does not analyse movement data
itself — it installs, attaches and keeps in sync the suite of packages
that do, so that a single
[`library(animovement)`](https://animovement.dev) gives you a complete
and coherent workflow instead of seven packages to track separately. In
that respect it is a descendant of the
[*fastverse*](https://fastverse.github.io/fastverse/), whose approach to
package management it borrows.

Splitting the ecosystem into focused packages keeps each one small,
testable and useful on its own; bundling them into a meta-package means
you rarely have to think about that split. You get the whole workflow,
at versions that are known to work together, and a handful of functions
for the housekeeping — which is what this vignette is about. For
tutorials on actually *analysing* movement data with these packages, see
[animovement.dev](https://animovement.dev).

## Attaching the animovement

``` r

library(animovement)
#> -- Attaching packages ------------------------------------- animovement 0.8.0 --
#> v anicore    0.8.0.9000     v anicheck   0.3.0     
#> v aniread    0.7.0          v animetric  0.4.0.9000
#> v anispace   0.3.0          v anivis     0.2.1     
#> v aniprocess 0.5.0
```

Attaching prints a startup message listing the core packages and their
versions, followed by any conflicts between them and packages you
already have loaded. Both can be silenced with
`options(animovement.quiet = TRUE)`.

The core packages are:

| Package | Role |
|----|----|
| [*anicore*](https://animovement.dev/anicore) | Core data structures for movement data |
| [*aniread*](https://animovement.dev/aniread) | Reading and writing movement data |
| [*anispace*](https://animovement.dev/anispace) | Spatial transformation methods |
| [*aniprocess*](https://animovement.dev/aniprocess) | Signal processing and filtering |
| [*anicheck*](https://animovement.dev/anicheck) | Diagnosing movement data quality |
| [*animetric*](https://animovement.dev/animetric) | Calculating movement-based metrics |
| [*anivis*](https://animovement.dev/anivis) | Visualising movement data and diagnostics |

[`animovement_packages()`](https://animovement.dev/animovement/reference/animovement_packages.md)
reports the set that is actually in force — the core packages, or
whatever a project configuration file specifies, plus any session
extensions:

``` r

animovement_packages()
#> [1] "anicore"     "aniread"     "anispace"    "aniprocess"  "anicheck"   
#> [6] "animetric"   "anivis"      "animovement"
```

## Conflicts

When two attached packages export a function of the same name, the one
attached later masks the other.
[`animovement_conflicts()`](https://animovement.dev/animovement/reference/animovement_conflicts.md)
lists every such clash involving an animovement package, in search-path
order:

``` r

animovement_conflicts()
```

It is also a general-purpose tool — pass it any set of attached packages
to check those instead:

``` r

animovement_conflicts(sub("package:", "", search()[-1]))
```

## Extending the animovement

[`animovement_extend()`](https://animovement.dev/animovement/reference/animovement_extend.md)
adds packages for the current session. They are attached alongside the
core packages, their conflicts are reported, and they are remembered in
`options("animovement.extend")` so
[`animovement_packages()`](https://animovement.dev/animovement/reference/animovement_packages.md),
[`animovement_update()`](https://animovement.dev/animovement/reference/animovement_update.md)
and friends include them:

``` r

animovement_extend(ggplot2, tidyr)
```

Use `install = TRUE` to install anything missing first. To extend
*before* attaching, set the option directly:

``` r

options(animovement.extend = c("ggplot2", "tidyr"))
library(animovement)
```

[`animovement_detach()`](https://animovement.dev/animovement/reference/animovement_detach.md)
is the inverse. With no arguments it detaches all animovement packages;
`session = TRUE` also clears them from the session options, and
`unload = TRUE` unloads the namespaces as well:

``` r

animovement_detach(ggplot2)                     # one package
animovement_detach(session = TRUE)              # everything, for good
```

## Project configuration

For a set of packages that belongs to a project rather than a session,
put a file named `.animovement` in the project root. List the packages
one per line, or separated by commas or spaces:

    anicore, aniread, aniprocess
    ggplot2

When this file is present, it *replaces* the standard set of core
packages — so list everything the project needs.
[`animovement_packages()`](https://animovement.dev/animovement/reference/animovement_packages.md)
reads it, and [`library(animovement)`](https://animovement.dev) attaches
exactly that set.

Options and environment variables can be set from the same file by
prefixing them with `_opt_` (options) or giving them bare (environment
variables). Entries placed before the package names are applied before
the packages are attached, those after them afterwards:

    _opt_animovement.install = TRUE
    anicore, aniread, aniprocess

The options the meta-package recognises are:

- `animovement.quiet` — `TRUE` disables startup, conflict and
  installation messages
- `animovement.styling` — `FALSE` disables styling of console output
- `animovement.extend` — packages to attach in addition to the core set
- `animovement.install` — `TRUE` installs missing packages on attach

## Installing and updating

The animovement packages are published on
[R-universe](https://animovement.r-universe.dev) rather than CRAN, so a
plain `getOption("repos")` will not find them.
[`animovement_repos()`](https://animovement.dev/animovement/reference/animovement_repos.md)
adds the R-universe to a repository vector, and is the default for the
functions below:

``` r

animovement_repos()
#>                                                          RSPM 
#> "https://packagemanager.posit.co/cran/__linux__/noble/latest" 
#>                                                          CRAN 
#>                                    "https://cran.rstudio.com" 
#>                                                               
#>                          "https://animovement.r-universe.dev"
```

``` r

options(repos = animovement_repos()) # ...or apply it to the whole session
```

[`animovement_update()`](https://animovement.dev/animovement/reference/animovement_update.md)
checks the installed versions against the repositories and installs what
is behind;
[`animovement_install()`](https://animovement.dev/animovement/reference/animovement_install.md)
installs packages that are missing altogether:

``` r

animovement_update()
animovement_install()
```

[`animovement_sitrep()`](https://animovement.dev/animovement/reference/animovement_sitrep.md)
gives the situation report the two are based on — every package in your
animovement, its installed version, and whether a newer one is available
— and
[`animovement_deps()`](https://animovement.dev/animovement/reference/animovement_deps.md)
lists the dependencies underneath them:

``` r

animovement_sitrep()
animovement_deps()
```

## Suggested packages

To keep installation light, functionality that only some users need is a
soft dependency of the ecosystem packages rather than a hard one. Rather
than installing them one error message at a time, you can take them all
at once:

``` r

animovement_show_suggested()    # list them
animovement_install_suggested() # install them
```

Packages needed only for development or documentation (knitr, testthat,
and the like) are excluded, as are packages an ecosystem package already
requires.
