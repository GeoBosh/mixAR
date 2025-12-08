# Fit mixture autoregressive models with seasonal AR parameters

Provides estimation via EM-Algorithm for mixture autoregressive models
including seasonal AR parameters.

## Usage

``` r
mixSARfit(y, model, est_shift = FALSE, tol = 10^-14)
```

## Arguments

- y:

  a time series (currently a numeric vector).

- model:

  an object of class `"MixAR"` including seasonal components.

- est_shift:

  if missing or `FALSE`, fix the intercepts to zero, otherwise estimate
  them.

- tol:

  threshold for stopping criterion.

## Details

This function only works for `"MixAR"` objects in which slot `arcoef` is
of class `"raggedCoefS"`.

## Value

A list of 2:

- model:

  an object of class `"MixAR"`. The estimated model.

- vallogf:

  the value of the loglikelihood function for the returned model.

## Author

Davide Ravagli and Georgi N. Boshnakov

## Examples

``` r
ar1 <- list(c(0.5, -0.5), c(1.1, 0, -0.5))
ar12 <- list(0, c(-0.3, 0.1))
s = 12

rag <- new("raggedCoefS", a = ar1, as = ar12, s = s)

model <- new("MixARGaussian", prob = exampleModels$WL_A@prob, # c(0.5, 0.5)
             scale = exampleModels$WL_A@scale,                # c(5, 1)
             arcoef = rag)

set.seed(1234)
y <- mixAR_sim(model, n = 100, init = rep(0, 24))

mixSARfit(y, model)
#> $model
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob      shift     scale    order ar_1      ar_2        ar_3      
#> Comp_1 0.5690198 0.4332104 4.315505   2   0.4074179 -0.43966159           
#> Comp_2 0.4309802 0.1342139 0.897868   3   1.0695694  0.02743196 -0.5119741
#>        s_order ar_12        ar_24    
#> Comp_1 1        0.006045561          
#> Comp_2 2       -0.308274546 0.1101399
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $vallogf
#> [1] -204.461
#> 
## fix the intercepts to zero
mixSARfit(y, model, est_shift = FALSE, tol = 10e-4)
#> $model
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob      shift     scale     order ar_1      ar_2        ar_3      
#> Comp_1 0.5601305 0.4223943 4.3230787   2   0.4063056 -0.43836730           
#> Comp_2 0.4398695 0.1047950 0.9290184   3   1.0706203  0.02662952 -0.5131505
#>        s_order ar_12        ar_24    
#> Comp_1 1        0.006308565          
#> Comp_2 2       -0.310873840 0.1057719
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $vallogf
#> [1] -204.5035
#> 
```
