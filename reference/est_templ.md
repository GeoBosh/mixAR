# Create estimation templates from MixAR model objects

Create estimation templates from MixAR model objects.

## Usage

``` r
est_templ(model, shift = TRUE, ...)
```

## Arguments

- model:

  a `"MixAR"` object.

- shift:

  logical, see Details.

- ...:

  currently not used.

## Details

Argument `model` is used as a template to specify values of parameters
and/or which parameters to estimate or fix. In general, If a value of a
parameter in `model` is `NA`, then it is to be estimated. Otherwise the
parameter is taken as is.

The current implementation is incomplete. In particular, the AR
parameters are always designated for estimation.

Argument `shift` can be used to overwrite some or values component
`shift` in `model`. If `shift` has length one, it is replicated to the
number of MixAR components. If `shift[k]` is `TRUE`, then the shift
coefficient for the k-th component is set to `NA` to request its
estimation. Otherwise, the value of the shift for the k-th component in
`model` is taken.

Argument `shift` has a default of `TRUE` which causes the shift
coefficients to be estimated irrespectively of their values in model.

`est_templ` returns a list with as many components as there are MixAR
components in the model. The k-th component of the list is itself a list
specifing which parameters of the i-th MixAR component to estimate or
fix.

## Value

a list, as described in Details.

## Examples

``` r
exampleModels$WL_A
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1 0.5    0     5     1   0.5 
#> Comp_2 0.5    0     1     1   1.1 
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
est_templ(exampleModels$WL_A)
#> [[1]]
#> [[1]][[1]]
#> [1] NA
#> 
#> [[1]][[2]]
#> [1] NA
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] NA
#> 
#> [[2]][[2]]
#> [1] NA
#> 
#> 
est_templ(exampleModels$WL_A, shift = FALSE)
#> [[1]]
#> [[1]][[1]]
#> [1] 0
#> 
#> [[1]][[2]]
#> [1] NA
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] 0
#> 
#> [[2]][[2]]
#> [1] NA
#> 
#> 

exampleModels$WL_I
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.4    0     1     2    0.9 -0.60     
#> Comp_2 0.3    0     1     1   -0.5           
#> Comp_3 0.3   -5     5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
est_templ(exampleModels$WL_I)
#> [[1]]
#> [[1]][[1]]
#> [1] NA
#> 
#> [[1]][[2]]
#> [1] NA NA
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] NA
#> 
#> [[2]][[2]]
#> [1] NA
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#> [1] NA
#> 
#> [[3]][[2]]
#> [1] NA NA NA
#> 
#> 
```
