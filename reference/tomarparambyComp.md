# Translations of my old MixAR Mathematica functions

Translations of some of my MixAR Mathematica functions. Not sure if
these are still used.

## Usage

``` r
tomarparambyComp(params)
tomarparambyType(params)
permuteArpar(params)
```

## Arguments

- params:

  the parameters of the MixAR model, a list, see Details.

## Details

`tomarparambyComp` is for completeness, my Mathematica programs do not
have this currently.

The arrangement of the parameters of MixAR models in package `"MixAR"`
is “by type”: slot `prob` contains the mixture probabilities (weights),
`shift` contains intercepts, and so on.

An alternative representation is “by component”: a list whose k-th
elements contains all parameters associated with the k-th mixture
component. The functions described here use the following order for the
parameter of the k-th component: prob_k, shift_k, arcoeff_k, sigma2_k.

`tomarparambyType` takes an argument, `params`, arranged “by component”
and converts it to “by type”. `tomarparambyComp` does the inverse
operation, from “by type” to “by component”.

`permuteArpar` creates all permutaions of the components of a MixAR
model. It takes a “by component” argument. The autoregressive orders are
not permuted, in that if the input model has AR orders `c(2, 1, 3)`, all
permuted models are also `c(2, 1, 3)`. The AR coefficients of shorter or
longer components are padded with zeroes or truncated, respectively, see
the unexported
[`adjustLengths()`](https://geobosh.github.io/mixAR/reference/adjustLengths.md).

## Value

For `tomarparambyComp`, a list containing the parameters of the model
arranged “by component”, see Details.

For `tomarparambyType`, a list containing the parameters of the model
arranged “by type”. It contains the following elements.

- prob:

  mixture probabilities, a numeric vector,

- shift:

  shifts, a numeric vector,

- arcoef:

  autoregressive coefficients,

- s2:

  noise variances, a numeric vector.

For `permuteArpar`, a list with one element (arranged “by type”) for
each possible permutation of the AR parameters.

## Author

Georgi N. Boshnakov

## See also

[`randomArCoefficients`](https://geobosh.github.io/mixAR/reference/randomArCoefficients.md)

## Examples

``` r
bycomp <- list(list(0.1, 10,  0.11,                1),
               list(0.2,  20, c(0.11, 0.22),       2),
               list(0.3,  30, c(0.11, 0.22, 0.33), 3) )
bytype <- tomarparambyType(bycomp)
identical(bycomp, tomarparambyComp(bytype)) # TRUE
#> [1] TRUE
               
permuteArpar(bycomp)
#> [[1]]
#> [[1]][[1]]
#> [[1]][[1]][[1]]
#> [1] 0.1
#> 
#> [[1]][[1]][[2]]
#> [1] 10
#> 
#> [[1]][[1]][[3]]
#> [1] 0.11
#> 
#> [[1]][[1]][[4]]
#> [1] 1
#> 
#> 
#> [[1]][[2]]
#> [[1]][[2]][[1]]
#> [1] 0.2
#> 
#> [[1]][[2]][[2]]
#> [1] 20
#> 
#> [[1]][[2]][[3]]
#> [1] 0.11 0.22
#> 
#> [[1]][[2]][[4]]
#> [1] 2
#> 
#> 
#> [[1]][[3]]
#> [[1]][[3]][[1]]
#> [1] 0.3
#> 
#> [[1]][[3]][[2]]
#> [1] 30
#> 
#> [[1]][[3]][[3]]
#> [1] 0.11 0.22 0.33
#> 
#> [[1]][[3]][[4]]
#> [1] 3
#> 
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [[2]][[1]][[1]]
#> [1] 0.1
#> 
#> [[2]][[1]][[2]]
#> [1] 10
#> 
#> [[2]][[1]][[3]]
#> [1] 0.11
#> 
#> [[2]][[1]][[4]]
#> [1] 1
#> 
#> 
#> [[2]][[2]]
#> [[2]][[2]][[1]]
#> [1] 0.3
#> 
#> [[2]][[2]][[2]]
#> [1] 30
#> 
#> [[2]][[2]][[3]]
#> [1] 0.11 0.22
#> 
#> [[2]][[2]][[4]]
#> [1] 3
#> 
#> 
#> [[2]][[3]]
#> [[2]][[3]][[1]]
#> [1] 0.2
#> 
#> [[2]][[3]][[2]]
#> [1] 20
#> 
#> [[2]][[3]][[3]]
#> [1] 0.11 0.22 0.00
#> 
#> [[2]][[3]][[4]]
#> [1] 2
#> 
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#> [[3]][[1]][[1]]
#> [1] 0.3
#> 
#> [[3]][[1]][[2]]
#> [1] 30
#> 
#> [[3]][[1]][[3]]
#> [1] 0.11
#> 
#> [[3]][[1]][[4]]
#> [1] 3
#> 
#> 
#> [[3]][[2]]
#> [[3]][[2]][[1]]
#> [1] 0.1
#> 
#> [[3]][[2]][[2]]
#> [1] 10
#> 
#> [[3]][[2]][[3]]
#> [1] 0.11 0.00
#> 
#> [[3]][[2]][[4]]
#> [1] 1
#> 
#> 
#> [[3]][[3]]
#> [[3]][[3]][[1]]
#> [1] 0.2
#> 
#> [[3]][[3]][[2]]
#> [1] 20
#> 
#> [[3]][[3]][[3]]
#> [1] 0.11 0.22 0.00
#> 
#> [[3]][[3]][[4]]
#> [1] 2
#> 
#> 
#> 
#> [[4]]
#> [[4]][[1]]
#> [[4]][[1]][[1]]
#> [1] 0.3
#> 
#> [[4]][[1]][[2]]
#> [1] 30
#> 
#> [[4]][[1]][[3]]
#> [1] 0.11
#> 
#> [[4]][[1]][[4]]
#> [1] 3
#> 
#> 
#> [[4]][[2]]
#> [[4]][[2]][[1]]
#> [1] 0.2
#> 
#> [[4]][[2]][[2]]
#> [1] 20
#> 
#> [[4]][[2]][[3]]
#> [1] 0.11 0.22
#> 
#> [[4]][[2]][[4]]
#> [1] 2
#> 
#> 
#> [[4]][[3]]
#> [[4]][[3]][[1]]
#> [1] 0.1
#> 
#> [[4]][[3]][[2]]
#> [1] 10
#> 
#> [[4]][[3]][[3]]
#> [1] 0.11 0.00 0.00
#> 
#> [[4]][[3]][[4]]
#> [1] 1
#> 
#> 
#> 
#> [[5]]
#> [[5]][[1]]
#> [[5]][[1]][[1]]
#> [1] 0.2
#> 
#> [[5]][[1]][[2]]
#> [1] 20
#> 
#> [[5]][[1]][[3]]
#> [1] 0.11
#> 
#> [[5]][[1]][[4]]
#> [1] 2
#> 
#> 
#> [[5]][[2]]
#> [[5]][[2]][[1]]
#> [1] 0.3
#> 
#> [[5]][[2]][[2]]
#> [1] 30
#> 
#> [[5]][[2]][[3]]
#> [1] 0.11 0.22
#> 
#> [[5]][[2]][[4]]
#> [1] 3
#> 
#> 
#> [[5]][[3]]
#> [[5]][[3]][[1]]
#> [1] 0.1
#> 
#> [[5]][[3]][[2]]
#> [1] 10
#> 
#> [[5]][[3]][[3]]
#> [1] 0.11 0.00 0.00
#> 
#> [[5]][[3]][[4]]
#> [1] 1
#> 
#> 
#> 
#> [[6]]
#> [[6]][[1]]
#> [[6]][[1]][[1]]
#> [1] 0.2
#> 
#> [[6]][[1]][[2]]
#> [1] 20
#> 
#> [[6]][[1]][[3]]
#> [1] 0.11
#> 
#> [[6]][[1]][[4]]
#> [1] 2
#> 
#> 
#> [[6]][[2]]
#> [[6]][[2]][[1]]
#> [1] 0.1
#> 
#> [[6]][[2]][[2]]
#> [1] 10
#> 
#> [[6]][[2]][[3]]
#> [1] 0.11 0.00
#> 
#> [[6]][[2]][[4]]
#> [1] 1
#> 
#> 
#> [[6]][[3]]
#> [[6]][[3]][[1]]
#> [1] 0.3
#> 
#> [[6]][[3]][[2]]
#> [1] 30
#> 
#> [[6]][[3]][[3]]
#> [1] 0.11 0.22 0.33
#> 
#> [[6]][[3]][[4]]
#> [1] 3
#> 
#> 
#> 
```
