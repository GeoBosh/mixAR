# Class `"MixComp"` — manipulation of MixAR time series

Class `"MixComp"` represents components of mixture autoregressive time
series and their transformations obtained by arithmetic and related
operations. Methods are provided to allow convenient computation with
such time series.

## Objects from the Class

Objects can be created by calls of the form `new("MixComp", ...)`. It is
more usual however to obtain such objects initially from functions such
as `mix_ek`. Methods are defined to allow for convenient and intuitive
further manipulation of such objects.

Internally, an object of class `MixComp` is a matrix with one column for
each component. However, methods for arithmetic operations involving
`MixComp` objects are defined to perform natural operations for mixture
objects. For example, multiplication by vectors is commutative and “does
the right thing”.

## Slots

- `m`::

  Object of class `"matrix"` with one column correponding to each
  component of the mixture AR model.

## Methods

Arithmetic operations involving `MixComp` objects are defined to allow
for convenient execution of computations for mixture autoregressive
models, see class `"MixComp"`.

- -:

  `signature(e1 = "MixComp", e2 = "missing")`: unary minus for "MixComp"
  objects.

- -:

  `signature(e1 = "numeric", e2 = "MixComp")`:

  If `e2` is thought of as a matrix, \\m\\, then the number of elements
  of `e1` must be the same as the number of rows of \\m\\ and each
  column of \\m\\ is subtracted from `e1`, see also `"mix_ek"`,
  `"mix_hatk"`.

  As a special case, if \\m\\ has only one row, then it is subtracted
  from each element of `e1`, i.e. that row is replicated to obtain a
  matrix with as many rows as the length of `e1` and then subtracted
  from `e1` as above.

  The result is a `MixComp` object.

- -:

  `signature(e1 = "MixComp", e2 = "numeric")`: This is analogous to the
  above method. (**FIXME:** the code of this function does not deal with
  the special case as in the above method. Is this an omission or I have
  done it on purpose?)

- %of%:

  `signature(e1 = "function", e2 = "MixComp")`: This applies the
  function `e1` to each element of `e2`. Together with the arithmetic
  operations this allows for easy computation with MixComp objects (e.g.
  pdfs, likelihoods).

- %of%:

  `signature(e1 = "character", e2 = "MixComp")`:

- %of%:

  `signature(e1 = "list", e2 = "MixComp")`: If `e1` is of length one it
  specifies a function to be applied to each element of `e2`, otherwise
  it is a list of functions, such that the \\i\\th function is applied
  to the \\i\\th column of `e2@m`.

- \*:

  `signature(e1 = "MixComp", e2 = "MixComp")`: ...

- \*:

  `signature(e1 = "MixComp", e2 = "numeric")`: see the following.

- \*:

  `signature(e1 = "numeric", e2 = "MixComp")`: “Column” \\i\\ of the
  `MixComp` object is multiplied by the \\i\\th element of the numeric
  vector, i.e. each “row” of the `MixComp` object is multiplied by the
  vector (or, the vector is replicated to a matrix to be multiplied by
  the `MixComp` object).

- \*:

  `signature(e1 = "function", e2 = "MixComp")`: Multiplying a function
  by a `MixComp` object actually applies the function to each element of
  the object. This is a misuse of methods, prefer operator `%of%` which
  does the same.

- \*:

  `signature(e1 = "character", e2 = "MixComp")`: The first argument is a
  name of a function which is applied to each element of the `MixComp`
  object. This is a misuse of methods, see operator
  [`%of%`](https://geobosh.github.io/mixAR/reference/percent_of.md)
  which does the same.

- /:

  `signature(e1 = "MixComp", e2 = "numeric")`:

- /:

  `signature(e1 = "numeric", e2 = "MixComp")`: Division works
  analogously to `"*"`.

- ^:

  `signature(e1 = "MixComp", e2 = "numeric")`: If `k` is a scalar, raise
  each element of `e1@m` to power `k`.

  (For consistency this operation should have the semantics of "\*" and
  "/" but this operator probably makes sense only for scalar 'e2', where
  the semantics doesn't matter. So, don't bother for now.)

- +:

  `signature(e1 = "numeric", e2 = "MixComp")`:

- +:

  `signature(e1 = "MixComp", e2 = "numeric")`: Addition involving
  `MixComp` objects works analogously to subtraction.

- inner:

  `signature(x = "MixComp", y = "missing", star = "missing", plus = "missing")`:
  With one argument `inner` computes the sum of the columns of the
  argument. This is conceptually equivalent to `y` being a vector of
  ones.

- inner:

  `signature(x = "MixComp", y = "numeric", star = "missing", plus = "missing")`:

- inner:

  `signature(x = "numeric", y = "MixComp", star = "missing", plus = "missing")`:
  The number of elements of the numeric argument should be equal to the
  number of rows of the `MixComp` object. Effectively, computes the
  inner product of the two arguments. The order of the arguments does
  not matter.

  Returns a numeric vector.

- inner:

  `signature(x = "MixComp", y = "numeric", star = "ANY", plus = "ANY")`:
  Computes a generalised inner product of `x` with `y` using the
  specified functions in place of the usual "\*" and "+" operations. The
  defaults for `star` and `+` are equivalent to multiplication and
  addition, respectively.

  Note that "+" is a binary operation (not \\n\\-ary) in R. So
  technically the correct way to specify the default operation here is
  "sum" or `sum`. Since it is easy to make this mistake, if
  `plus == "+"`, it is replaced by "sum". (In fact, `plus` is given a
  single argument, the vector of values to work on. Since "+" works as a
  unary operator on one argument, it would give surprising results if
  left as is.)

- inner:

  `signature(x = "MixComp", y = "numeric", star = "ANY", plus = "missing")`:
  This is a more efficient implementation for the case when
  `plus = sum`.

- mix_ncomp:

  `signature(x = "MixComp")`: Number of components.

- `signature(x = "MixComp")`:

  A `"MixComp"` object is essentially a matrix. This method gives the
  dimension of the underlying matrix. This method indirectly ensures
  that [`nrow()`](https://rdrr.io/r/base/nrow.html) and
  [`ncol()`](https://rdrr.io/r/base/nrow.html) work naturally for
  `"MixComp"` objects.

## Author

Georgi N. Boshnakov

## Examples

``` r
## dim, nrow, ncol
a <- new("MixComp", m = matrix(c(1:7, 11:17, 21:27), ncol = 3))
a
#> An object of class "MixComp"
#> Slot "m":
#>      [,1] [,2] [,3]
#> [1,]    1   11   21
#> [2,]    2   12   22
#> [3,]    3   13   23
#> [4,]    4   14   24
#> [5,]    5   15   25
#> [6,]    6   16   26
#> [7,]    7   17   27
#> 
dim(a)
#> [1] 7 3
nrow(a)
#> [1] 7
ncol(a)
#> [1] 3
mix_ncomp(a)
#> [1] 3

-a
#> An object of class "MixComp"
#> Slot "m":
#>      [,1] [,2] [,3]
#> [1,]   -1  -11  -21
#> [2,]   -2  -12  -22
#> [3,]   -3  -13  -23
#> [4,]   -4  -14  -24
#> [5,]   -5  -15  -25
#> [6,]   -6  -16  -26
#> [7,]   -7  -17  -27
#> 
a - 1:7 
#> An object of class "MixComp"
#> Slot "m":
#>      [,1] [,2] [,3]
#> [1,]    0   10   20
#> [2,]    0   10   20
#> [3,]    0   10   20
#> [4,]    0   10   20
#> [5,]    0   10   20
#> [6,]    0   10   20
#> [7,]    0   10   20
#> 
1:7 + a
#> An object of class "MixComp"
#> Slot "m":
#>      [,1] [,2] [,3]
#> [1,]    2   12   22
#> [2,]    4   14   24
#> [3,]    6   16   26
#> [4,]    8   18   28
#> [5,]   10   20   30
#> [6,]   12   22   32
#> [7,]   14   24   34
#> 
2*a
#> An object of class "MixComp"
#> Slot "m":
#>      [,1] [,2] [,3]
#> [1,]    2   22   42
#> [2,]    4   24   44
#> [3,]    6   26   46
#> [4,]    8   28   48
#> [5,]   10   30   50
#> [6,]   12   32   52
#> [7,]   14   34   54
#> 


b <- new("MixComp", m = matrix(rnorm(18), ncol = 3))

## apply a function to the columns of a MixComp object
pnorm %of% b
#> An object of class "MixComp"
#> Slot "m":
#>            [,1]       [,2]      [,3]
#> [1,] 0.23030925 0.27440789 0.9298521
#> [2,] 0.64915726 0.88887962 0.9559969
#> [3,] 0.65379738 0.07595603 0.3556432
#> [4,] 0.33456253 0.92311527 0.8152295
#> [5,] 0.01232201 0.57175454 0.4986453
#> [6,] 0.53848139 0.65221316 0.9103216
#> 

## apply a separate function to to each column
flist <- list(function(x) pnorm(x),
              function(x) pt(x, df = 5),
              function(x) pt(x, df = 4) )
flist %of% b
#> An object of class "MixComp"
#> Slot "m":
#>            [,1]      [,2]      [,3]
#> [1,] 0.23030925 0.2874733 0.8928424
#> [2,] 0.64915726 0.8616708 0.9183992
#> [3,] 0.65379738 0.1056748 0.3650257
#> [4,] 0.33456253 0.8934470 0.7898658
#> [5,] 0.01232201 0.5682036 0.4987266
#> [6,] 0.53848139 0.6441535 0.8747547
#> 
```
