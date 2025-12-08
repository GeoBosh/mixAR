# Log-likelihood of MixAR models

Compute the log-likelihood of a MixAR model for a univariate time
series.

## Usage

``` r
cond_loglik(model, x, index)
cond_loglikS(model, x, index)
```

## Arguments

- model:

  a MixAR model.

- x:

  a time series or numeric vector.

- index:

  a vector of integers giving the indices in `x` over which to compute
  the sum for the log-likelihood, default is `(p+1):length(x)`, where
  `p` is the maximum AR order of the components of the model.

## Details

`cond_loglik` computes the conditional log-likelihood of a MixAR model.
Conditional here means conditional on the first `p` values being fixed,
where `p` is the maximum AR order of the components of the model.

Argument `index` can be used to compute the sum over a subset of time
points.

`cond_loglikS` is a variant of `cond_loglik` for the case when the input
model contains seasonal AR coefficients.

## Value

the log-likelihood, a numeric value

## Author

Georgi N. Boshnakov and Davide Ravagli

## Examples

``` r
## data(ibmclose, package = "fma") # doesn't work with fma v2.4, using '::'
cond_loglik(exampleModels$WL_ibm,     as.numeric(fma::ibmclose))
#> [1] -1212.188
cond_loglik(exampleModels$WL_ibm_gen, as.numeric(fma::ibmclose))
#> [1] -1212.188

data(lynx)  # for 'lynx' data
sar <- new("raggedCoefS", a = list(c(1.1022, -0.2835), c(1.5279, -0.8871)),
                          as = list(c(0, 0), 0), s = 10)

## SMAR(2; 2, 2)(2, 1)_10
model_s10 <- new("MixARGaussian", prob = c(.3, .7), scale = c(.08, .202),
                                  arcoef = sar, shift = c(.7,1)) 
cond_loglikS(model_s10, log(lynx))
#> [1] -2036.966
cond_loglikS(model_s10, log(lynx), index = 45:114) # on reduced dataset
#> [1] -1593.132
```
