# Infix operator to apply functions to matrix-like objects

The infix operator `%of%` is a generic function which applies functions
to objects. This page describes the function and the methods defined in
package mixAR.

## Usage

``` r
e1 %of% e2
e1 %of% e2
```

## Arguments

- e1:

  usually a function, the name of a function, a character vector, or a
  list of functions, see Details.

- e2:

  an object, usually matrix-like.

## Details

`%of%` is a generic function with dispatch on both arguments. It is
intended to be used mainly in infix form.

`%of%` transforms each “column” of a matrix-like object by a function.
If `e1` specifies a single function, that is applied to all columns.
Otherwise `length(e1)` should equal the number of “columns” of `e2` and
`e1[[i]]` is applied to the `i`-th “column” of `e2`.

The mental model is that the first argument, `e1`, is (converted to) a
list of functions containing one function for each column of `e2`. The
i-th function is applied to each element of the i-th column.

The methods for `"MixComp"` objects allow for very transparent and
convenient computing with `"MixAR"` objects.

## Value

for the default method, a matrix;

for methods with `e2` from class `MixComp`, a `MixComp` object with its
slot `m` replaced by the result of applying `e1` to its elements, see
the descriptions of the individual methods for details;

## Methods

Below are the descriptions of the methods for `%of%` defined by package
mixAR.

- `signature(e1 = "ANY", e2 = "ANY")`:

  This is the default method. It uses
  [`apply()`](https://rdrr.io/r/base/apply.html) to evaluate `e1` for
  each element of the matrix `e2`, without checking the arguments. If
  the arguments are not suitable for
  [`apply()`](https://rdrr.io/r/base/apply.html), any error messages
  will come from it. So, for this method `e1` is a function (or the name
  of a function) and `e2` is a matrix or array.

- `signature(e1 = "function", e2 = "MixComp")`:

  Create (and return) a `MixComp` object with its slot `m` replaced by
  the result of applying the function `e1` to each element of the
  `MixComp` object `e2`, see class
  `"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`.

- `signature(e1 = "character", e2 = "MixComp")`:

  Here `e1` contains the names of one or more functions. If
  `length(e1) = 1`, this is equivalent to the method for `e1` of class
  `"function"`.

  If `length(e1) > 1`, then for each `i` the function specified by
  `e1[i]` is applied to the `i`th column of `e2@m`. In this case there
  is no recycling: `e1` must have `ncol(e2@m)` elements.

- `signature(e1 = "list", e2 = "MixComp")`:

  Here each element of `e1` is a function or the name of a function. It
  works analogously to the method with `e1` from class `"character"`. If
  `length(e1) = 1`, then `e1[[1]]` is applied to each element of `e1@m`.
  Otherwise, if `length(e1) > 1`, then `e1[[i]]` is applied to the `i`th
  column of `e2@m`.

## Author

Georgi N. Boshnakov

## Note

The code is rather inefficient for some of the methods.

Maybe should require that the functions in the first argument are
vectorised. (Some methods effectively assume it.)

## See also

class
`"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`

## Examples

``` r
m <- matrix(rnorm(18), ncol = 3)
## defult method
pm1 <- pnorm %of% m
f3 <- list(pnorm, function(x, ...) pnorm(x, mean = 0.1),
                  function(x, ...) pnorm(x, mean = -0.1) )
## no method for f from "list" yet:
## pm2 <- f3 %of% m

mc <- new("MixComp", m = m)
pnorm %of% mc
#> An object of class "MixComp"
#> Slot "m":
#>           [,1]      [,2]       [,3]
#> [1,] 0.1160244 0.6740943 0.03769883
#> [2,] 0.1910889 0.3883076 0.27336004
#> [3,] 0.5757670 0.9257244 0.95629633
#> [4,] 0.2870982 0.2739692 0.47767764
#> [5,] 0.3089111 0.9584489 0.66386241
#> [6,] 0.1328869 0.9665520 0.85391210
#> 
pmc3 <- f3 %of% mc
## result is equivalent to applying f3[[i] to m[ , i]:
all.equal(pmc3@m, cbind(f3[[1]](m[ , 1]), f3[[2]](m[ , 2]), f3[[3]](m[ , 3])))
#> [1] TRUE
```
