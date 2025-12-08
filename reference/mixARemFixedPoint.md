# EM estimation for mixture autoregressive models

Fit a mixture autoregressive model to a univariate time series using the
EM algorithm.

## Usage

``` r
mixARemFixedPoint(y, model, est_shift = TRUE, crit = 1e-14, 
                  maxniter = 200, minniter = 10, verbose = FALSE)

mixARgenemFixedPoint(y, model, crit = 1e-14, maxniter = 200, 
                     minniter = 10, verbose = FALSE, ...)
```

## Arguments

- y:

  a univariate time series.

- model:

  an object of class MixAR, a mixture autoregressive model providing the
  model specifications and initial values for the parameters.

- est_shift:

  if TRUE optimise also w.r.t. the shift (constant) terms of the AR
  components, if FALSE keep the shift terms fixed.

- crit:

  stop iterations when the relative change in the log-likelihood becomes
  smaller than this value.

- maxniter:

  maximum number of iterations.

- minniter:

  minimum number of iterations, do at leat that many iterations.

- ...:

  further arguments to be passed on to the M-step optimiser.

- verbose:

  print more details during optimisation.

## Details

`mixARemFixedPoint` and `mixARgenemFixedPoint` estimate MixAR models
with the EM algorithm. For `mixARemFixedPoint`, the distribution of the
components are fixed to be Gaussian. For `mixARgenemFixedPoint`, the
distributions can, in principle be arbitrary (well, to a point).

Starting with `model`, the expectation and maximisation steps of the EM
algorithm are repeated until convergence is detected or the maximum
number of iterations, `maxniter` is exceeded.

Currently the convergence check is very basic—the iterations stop when
the relative change in the log-likelihood in the last two iterations is
smaller than the threshold value specified by `crit` and at least
`minniter` iterations have been done.

The EM algorithm may converge very slowly. To do additional iterations
use the returned value in another call of this function.

## Value

the fitted model as an object inheriting from "MixAR".

## Author

Georgi N. Boshnakov

## Note

This function was not intended to be called directly by the user (hence
the inconvenient name).

## See also

[`fit_mixAR`](https://geobosh.github.io/mixAR/reference/fit_mixAR-methods.md)
which uses these functions for estimation, classes
`"`[`MixARGaussian`](https://geobosh.github.io/mixAR/reference/MixARGaussian-class.md)`"`,
`"`[`MixARgen`](https://geobosh.github.io/mixAR/reference/MixARgen-class.md)`"`

## Examples

``` r
## data(ibmclose, package = "fma") # ibm data from BJ

m0 <- exampleModels$WL_ibm
m1 <- mixARemFixedPoint(fma::ibmclose, m0)
m1a <- mixARemFixedPoint(fma::ibmclose, m1$model)
show_diff(m1$model, m1a$model)
#>        prob          shift         scale         order ar_1         
#> Comp_1  3.837969e-07 -4.495242e-07  2.519267e-06   2    3.163561e-07
#> Comp_2 -3.149383e-07 -1.540680e-05 -5.260464e-07   2    4.774857e-08
#> Comp_3 -6.885862e-08  9.676052e-06  1.097474e-05   1   -3.288296e-08
#>        ar_2         
#> Comp_1 -3.151398e-07
#> Comp_2 -2.032724e-08
#> Comp_3              

mixARemFixedPoint(fma::ibmclose, m0, est_shift = FALSE)
#> $model
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift scale     order ar_1      ar_2      
#> Comp_1 0.54399212   0    4.801666   2   0.6827977  0.3182769
#> Comp_2 0.42196471   0    5.971484   2   1.6737008 -0.6761710
#> Comp_3 0.03404316   0   18.038309   1   0.9885533           
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $vallogf
#> [1] -1209.927
#> 

# \donttest{
## simulate a continuation of ibmclose, assuming m0
ts1 <- mixAR_sim(m0, n = 50, init = c(346, 352, 357), nskip = 0)
m2a <- mixARemFixedPoint(ts1,       m0, est_shift = FALSE)$model
m2b <- mixARemFixedPoint(diff(ts1), m0, est_shift = FALSE)$model
# }
```
