# Class `"raggedCoefS"` — ragged list

Ragged list used to hold coefficients of MixAR models with seasonal AR
parameters.

## Objects from the Class

Objects are created by calls of the form

`new("raggedCoefS", a = list(v1, v2 , ...), as = list(vs1, vs2, ...), s)`.

If orders `p` and `ps` are specified, a consistency check is made.

## Slots

- a:

  Object of class "`list`" containing AR values. Each element of the
  list must be "`numeric`"

- p:

  Object of class "`numeric`" containing the lengths of components in
  `a`. If missing, it is generated based on lengths of elements of `a`.

- as:

  Object of class "`list`" containing seasonal AR values. Each element
  of the list must be "`numeric`"

- ps:

  Object of class "`numeric`" containing the lengths of elements of
  `as`. If missing, it is generated based on lengths of elements of
  `as`.

- s:

  A single element "`numeric`" vector determining the seasonality in the
  model(monthly, quarterly, etc..).

## Methods

Indexing with "\[" treats a `raggedCoef` object as a matrix (one row for
each component), while "\[\[" treats the object as list (it works on
slot `a`). Specifically, "\[\[1\]\]" picks the systematic AR parameters,
"\[\[2\]\]" picks seasonal AR parameters.

The replacement variants of "\[" and "\[\[" do not change the structure
of the object.

Replacement methods only work for subsets `x[[i]]`, `x[[i]][[j]]`,
`x[[i]][[j]][k]` for suitable `i`, `j` and `k`.

`i` must be equal to 1 for `x@a` and 2 for `x@as`.

- \[:

  `signature(x = "raggedCoefS", i = "missing", j = "missing")`:

- \[:

  `signature(x = "raggedCoefS", i = "missing", j = "missing")`:

- \[:

  `signature(x = "raggedCoefS", i = "numeric", j = "missing")`:

- \[:

  `signature(x = "raggedCoefS", i = "numeric", j = "numeric")`: Indexing
  with "\[" treats a `raggedCoef` object as a matrix with one row for
  each component and number of columns equal to `max(p) + max(ps)` in
  increasing lag. However, `x[2]` is equivalent to `x[2,]` which is
  different from the treatment of `matrix` objects in base R.

- \[\[:

  `signature(x = "raggedCoefS"), i = "numeric"`:

- \[\[:

  `signature(x = "raggedCoefS"), i = "numeric", j = "numeric"`:

- \[\[:

  `signature(x = "raggedCoefS"), i = "numeric", j = "numeric", k = "numeric"`:

## Author

Davide Ravagli

## See also

class
`"`[`raggedCoef`](https://geobosh.github.io/mixAR/reference/raggedCoef-class.md)`"`

## Examples

``` r
showClass("raggedCoefS")
#> Class "raggedCoefS" [package "mixAR"]
#> 
#> Slots:
#>                                               
#> Name:       as      ps       s       a       p
#> Class:    list numeric numeric    list numeric
#> 
#> Extends: "raggedCoef"

ragA <- new("raggedCoefS", a = list( c(0.5, -0.5), 1),
            as = list(0, c(0.3, -0.1) ), s = 12)
ragB <- new("raggedCoefS", a = list( c(0.5, -0.5), 1), p = c(2, 1),
            as = list(0, c(0.3, -0.1) ), ps = c(1, 2), s = 12)  # same

## Elements selection examples

ragA[]             ## matrix of coefficients
#>      [,1] [,2] [,3] [,4]
#> [1,]  0.5 -0.5  0.0  0.0
#> [2,]  1.0  0.0  0.3 -0.1
ragA[1]; ragA[1, ] ## vector of coefficients from first component
#> [1]  0.5 -0.5  0.0  0.0
#> [1]  0.5 -0.5  0.0  0.0
ragA[[2]]          ## list of seasonal AR parameters
#> [[1]]
#> [1] 0
#> 
#> [[2]]
#> [1]  0.3 -0.1
#> 
ragA[[2]][[1]]      ## vector of seasonal AR parameters from first component
#> [1] 0

## Replacement of values in 'raggedCoefS' objects


ragB[[2]] <- list(1, c(-0.5,0.5))
ragB[[2]][[2]] <- c(20, 22)
ragB[[1]][[1]][1] <- 0

```
