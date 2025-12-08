# Generalised inner product and methods for class `"MixComp"`

Generalised inner product and methods for class MixComp. The methods for
MixComp provide for very convenient computing with MixAR models.

## Usage

``` r
inner(x, y, star = "*", plus = .mplus)
```

## Arguments

- x:

  the first argument.

- y:

  the second argument.

- star:

  function to apply to pairs of elements from `x` and `y`, default is
  multiplication, as for the usual inner product.

- plus:

  function to apply to combine the results from the pairs, default is
  addition, as for the usual inner product.

## Details

`inner` computes a generalised inner product `x . y`, where
multiplication and summation can be replaced by other functions.

The default method of `inner` applies `star` to the corresponding pairs
of elements and combines them with `plus`. There is no recycling, if `x`
and `y` have different lengths, an error is raised. The elements of `x`
and `y` are accessed with "\[\[". `plus` should be an `n`-ary operation.

## Value

the inner product, the type of the result depends on the arguments

## Methods

Methods for inner product between a `"MixComp"` object and a vector are
similar to a product between a matrix and a vector but comply with the
conventions of class `"MixComp"`. For this reason they are described in
the help page for class
`"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`,
along with methods for other functions and operators applied to
`"MixComp"` objects.

- `signature(x = "ANY", y = "ANY", star = "ANY", plus = "ANY")`:

  This is the default method, see section Details.

- `signature(x = "MixComp", y = "missing", star = "missing", plus = "missing")`:

  see
  `"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`.

- `signature(x = "MixComp", y = "numeric", star = "missing", plus = "missing")`:

  see
  `"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`.

- `signature(x = "numeric", y = "MixComp", star = "missing", plus = "missing")`:

  see
  `"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`.

- `signature(x = "MixComp", y = "numeric", star = "ANY", plus = "ANY")`:

  see
  `"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`.

- `signature(x = "MixComp", y = "numeric", star = "ANY", plus = "missing")`:

  see
  `"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`.

## See also

`"`[`MixComp`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)`"`

## Examples

``` r
inner(1:3, 2:4) # [1] 20
#> [1] 20
class(inner(1:3, 2:4)) # [1] "integer"
#> [1] "integer"
## compare to:
1:3 %*% 2:4        # 20, but (1,1)-matrix
#>      [,1]
#> [1,]   20
class(1:3 %*% 2:4) # matrix
#> [1] "matrix" "array" 
```
