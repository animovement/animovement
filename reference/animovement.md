# The animovement

The *animovement* is an extensible suite of R packages. It is a
descendant of the [fastverse](https://fastverse.github.io/fastverse/).

## Functions in the *animovement* Package

Functions to extend or reduce the number of packages in the
*animovement*

[`animovement_extend()`](http://animovement.dev/animovement/reference/animovement_extend.md)  
[`animovement_detach()`](http://animovement.dev/animovement/reference/animovement_detach.md)

Function to display conflicts for *animovement* packages (or any other
attached packages)

[`animovement_conflicts()`](http://animovement.dev/animovement/reference/animovement_conflicts.md)

Function to update *animovement* packages (and dependencies) and install
(missing) packages

[`animovement_update()`](http://animovement.dev/animovement/reference/animovement_update.md)  
[`animovement_install()`](http://animovement.dev/animovement/reference/animovement_install.md)

Utilities to retrieve the names of *animovement* packages (and
dependencies), their update status and produce a situation report.

[`animovement_packages()`](http://animovement.dev/animovement/reference/animovement_packages.md)  
[`animovement_deps()`](http://animovement.dev/animovement/reference/animovement_deps.md)  
[`animovement_sitrep()`](http://animovement.dev/animovement/reference/animovement_sitrep.md)

## *animovement* Options

- `options(animovement.quiet = TRUE)` will disable all automatic
  messages (including conflict reporting) when calling
  [`library(animovement)`](https://animovement.dev),
  [`animovement_extend`](http://animovement.dev/animovement/reference/animovement_extend.md),
  [`animovement_update(install = TRUE)`](http://animovement.dev/animovement/reference/animovement_update.md)
  and
  [`animovement_install`](http://animovement.dev/animovement/reference/animovement_install.md).

- `options(animovement.styling = FALSE)` will disable all styling
  applied to text printed to the console.

- `options(animovement.extend = c(...))` can be set before calling
  [`library(animovement)`](https://animovement.dev) to extend the
  animovement with some packages for the session. The same can be done
  with the
  [`animovement_extend`](http://animovement.dev/animovement/reference/animovement_extend.md)
  function after [`library(animovement)`](https://animovement.dev),
  which will also populate `options("animovement.extend")`.

- `options(animovement.install = TRUE)` can be set before
  [`library(animovement)`](https://animovement.dev) to install any
  missing packages beforehand. See also
  [`animovement_install`](http://animovement.dev/animovement/reference/animovement_install.md).

## See also

Useful links:

- <https://animovement.dev>

- <https://animovement.dev/animovement>

- <https://github.com/animovement/animovement>

- Report bugs at <https://github.com/animovement/animovement/issues>

## Author

**Maintainer**: Mikkel Roald-Arbøl <animovement.84w1m@passmail.com>
([ORCID](https://orcid.org/0000-0002-9998-0058))
