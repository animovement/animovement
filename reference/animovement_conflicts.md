# Conflicts between the animovement and other packages

This function lists all the conflicts among *animovement* packages and
between *animovement* packages and other attached packages. It can also
be used to check conflicts for any other attached packages.

## Usage

``` r
animovement_conflicts(pkg = animovement_packages())
```

## Arguments

- pkg:

  character. A vector of packages to check conflicts for. The default is
  all *animovement* packages.

## Value

An object of class 'animovement_conflicts': A named list of character
vectors where the names are the conflicted objects, and the content are
the names of the package namespaces containing the object, in the order
they appear on the [`search`](https://rdrr.io/r/base/search.html) path.

## See also

[`animovement`](http://animovement.dev/animovement/reference/animovement.md)

## Examples

``` r
# Check conflicts between animovement packages and all attached packages
animovement_conflicts()

# Check conflicts among all attached packages
animovement_conflicts(sub("package:", "", search()[-1]))
#> -- Conflicts ---------------------------------------- animovement_conflicts() --
#> x methods::body<-()    masks base::body<-()
#> x methods::kronecker() masks base::kronecker()
```
